(in-package :rest-interface)

;in the midterm write a reader thread
;(make-thread (lambda () (write-line "Hello, world"))) in sbcl
;http://www.sbcl.org/manual/Threading-basics.html#Threading-basics

(defparameter *read-frequency* 3600) ;read frequency in seconds


(setf drakma:*drakma-default-external-format* :utf-8)

(setf drakma:*text-content-types*
      (append drakma:*text-content-types* 
              (list (cons "application" "x-tm+xml"))
              (list (cons "application" "xml"))
              (list (cons "application" "atom+xml"))))


(defun read-url (url)
  "takes a url and returns a string with the url's contents if
successful. Throws an error otherwise"
  (multiple-value-bind (reply status)
      (drakma:http-request url)
    (if (= status 200)
        reply
        (error "no successful connection in read-url"))))


(defun read-fragments-feed (fragment-feed-url)
  "read a feed of TM fragments and build a fragments-feed object
  containing fragment-entries from it" 
  ;from the feed we need only the source locator
  (let
      ((fragment-feed 
        (read-url fragment-feed-url)))
    (parse-fragments-feed fragment-feed)))

(defun read-snapshots-feed (snapshot-feed-url)
  "read a feed of TM snapshots and build a snapshot-feed object
  containing fragment-entries from it" 
  ;from the feed we need only the source locator
  (let
      ((snapshot-feed 
        (read-url snapshot-feed-url)))
    (parse-snapshots-feed snapshot-feed)))

(defun import-fragments-feed (fragment-feed-url)
  ;a bit of a borderline case if that should be here or in the
  ;importer. Since it deals with the network interface, I think it
  ;makes sense to have it here, though
  (let
      ((feed (read-fragments-feed fragment-feed-url)) 
       (revision (d:get-revision)))
    (loop for entry in (atom:entries feed) do
         (let
             ((top  (d:get-item-by-psi (psi entry) :revision revision)) 
              (xtm-id (atom:id entry))
              (source-locator  (source-locator-prefix feed)))
           ;check if xtm-id has already been imported. If so, don't do it again
           (unless (xtm-id-p xtm-id)
             (when top
               (mark-as-deleted top :source-locator source-locator :revision revision))
	     (format t "Fragment feed: ~a~&" (link entry))
             (importer-xtm1.0 
              (dom:document-element
               (cxml:parse-rod (read-url (link entry)) (cxml-dom:make-dom-builder)))
              :xtm-id xtm-id :revision revision)
             ;the consequence of the algorithm is to add the source
             ;locator + a suitable internal id as an identifier to all
             ;characteristics and associations that don't already have
             ;one and then reuse it next time
             (add-source-locator 
              (d:get-item-by-psi (psi entry) :revision revision) ;works even if the topic is only created during import
              :source-locator source-locator :revision revision))))))

(defun import-snapshots-feed (snapshot-feed-url)
  ;this would have to find the oldest (?) snapshot and import that. It
  ;subsequently applies all fragments against it

  ;sets source locator
 (let
      ((feed (read-snapshots-feed snapshot-feed-url))
       (revision (get-revision)))
   ;TODO: we lie for now and claim that the first entry always
   ;represents the oldest snapshot we lie in addition in that we
   ;import the snapshotfeed outright, not looking at the source
   ;locator
   (let*
       ((entry (first (entries feed)))
        (xtm-id (id entry)))
     ;;that *should* be the algorithm...
     ;;    If a client has a local topic map that contains topic map
     ;;    data from more than one server and wants to fetch and update
     ;;    the latest full topic map from ONE source then it MUST do the
     ;;    following. Apply the delete topic algorithm from below, but
     ;;    apply it to the entire topic map. Then proceed in terms of 'A
     ;;    Clean Start', by fetching the topic map and merging it in
     ;;    (1b, 1.4.3.2)
     (unless (xtm-id-p xtm-id)
       (importer-xtm1.0
        (dom:document-element
         (cxml:parse-rod (read-url (link entry)) (cxml-dom:make-dom-builder)))
        :xtm-id xtm-id :revision revision)))))

(defun import-tm-feed (feed-url)
  "takes the feed url, imports the first snapshot if necessary and
then applies all fragments to it"
  ;the implementation may be a bit brutal, but relies only on
  ;guaranteed rel-attributes on the links
  (let*
      ((feed-string (read-url feed-url))
       (feed-dom (dom:document-element 
                  (cxml:parse-rod feed-string (cxml-dom:make-dom-builder))))
       (link-elems 
        (xpath-select-location-path feed-dom
                                    '((*atom-ns* "entry")
                                      (*atom-ns* "link"))))
       (snapshot-feed-link-elem 
        (find-if (lambda(elem)
                   (string= (get-attribute elem "rel")
                            "http://www.egovpt.org/sdshare/snapshotsfeed")) link-elems))
       (fragment-feed-link-elem
        (find-if (lambda(elem)
                   (string= (get-attribute elem "rel")
                            "http://www.egovpt.org/sdshare/fragmentsfeed")) link-elems)))
    (import-snapshots-feed 
     (get-attribute snapshot-feed-link-elem "href"))
    (import-fragments-feed 
     (get-attribute fragment-feed-link-elem "href"))))
    

    