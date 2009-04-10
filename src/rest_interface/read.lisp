;;+-----------------------------------------------------------------------------
;;+  Isidorus
;;+  (c) 2008-2009 Marc Kuester, Christoph Ludwig, Lukas Giessmann
;;+
;;+  Isidorus is freely distributable under the LGPL license.
;;+  You can find a detailed description in trunk/docs/LGPL-LICENSE.txt.
;;+-----------------------------------------------------------------------------


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

(defun import-fragments-feed (fragment-feed-url imported-snapshot-entry &key tm-id)
  ;a bit of a borderline case if that should be here or in the
  ;importer. Since it deals with the network interface, I think it
  ;makes sense to have it here, though
  (let
      ((feed (read-fragments-feed fragment-feed-url)) 
       (revision (d:get-revision)))
    (loop for entry in (slot-value feed 'atom:entries) do
         (let
             ((top  (d:get-item-by-psi (psi entry) :revision revision)) 
              (xtm-id (atom:id entry))
              (source-locator  (source-locator-prefix feed)))
           ;check if xtm-id has already been imported or if the entry is older
           ;than the snapshot feed. If so, don't do it again
           (unless (or (xtm-id-p xtm-id) (string> (atom:updated entry) (atom:updated imported-snapshot-entry)))
             (when top
               (mark-as-deleted top :source-locator source-locator :revision revision))
	     ;(format t "Fragment feed: ~a~&" (link entry))
             (importer-xtm1.0 
              (dom:document-element
               (cxml:parse-rod (read-url (link entry)) (cxml-dom:make-dom-builder)))
              :tm-id tm-id :xtm-id xtm-id :revision revision)
             ;the consequence of the algorithm is to add the source
             ;locator + a suitable internal id as an identifier to all
             ;characteristics and associations that don't already have
             ;one and then reuse it next time
             (add-source-locator 
              (d:get-item-by-psi (psi entry) :revision revision) ;works even if the topic is only created during import
              :source-locator source-locator :revision revision))))))

(defun string-max (string-list &optional (max nil))
  (cond
    ((null string-list)
     max)
    ((string> (first string-list) max)
     (string-max (rest string-list) (first string-list)))
    (t 
     (string-max (rest string-list) max))))

(defun most-recent-entry (entry-list)
  (let
      ((most-recent-update (string-max (mapcar #'atom:updated entry-list))))
    (find most-recent-update entry-list :key #'updated :test #'string=)))

(defun most-recent-imported-snapshot (all-snapshot-entries)
  (let
      ((all-imported-entries
	(remove-if-not #'xtm-id-p all-snapshot-entries :key #'atom:id)))
    (most-recent-entry all-imported-entries)))

(defun import-snapshots-feed (snapshot-feed-url &key tm-id)
  "checks if we already imported any of this feed's snapshots. If not,
finds the most recent snapshot and imports that. It returns the entry
corresponding to the snapshot imported (now or previously)."
 (let*
      ((feed (read-snapshots-feed snapshot-feed-url))
       (all-entries (slot-value feed 'atom:entries))
       (most-recent-imported-entry (most-recent-entry all-entries)))
   (if most-recent-imported-entry
       most-recent-imported-entry
       (let*
	   ((entry (most-recent-entry all-entries))
	    (snapshot-dom 
	     (dom:document-element
	      (cxml:parse-rod (read-url (link entry)) (cxml-dom:make-dom-builder))))
	    (xtm-id (id entry))
	    (revision (get-revision)))
	 ;;that *should* be the algorithm...
	 ;;    If a client has a local topic map that contains topic map
	 ;;    data from more than one server and wants to fetch and update
	 ;;    the latest full topic map from ONE source then it MUST do the
	 ;;    following. Apply the delete topic algorithm from below, but
	 ;;    apply it to the entire topic map. Then proceed in terms of 'A
	 ;;    Clean Start', by fetching the topic map and merging it in
	 ;;    (1b, 1.4.3.2)
	 (importer-xtm1.0 snapshot-dom :tm-id tm-id :xtm-id xtm-id :revision revision)
	 entry))))

(defun import-tm-feed (feed-url &optional (processed-feed-urls nil))
  "takes the feed url of a collection feed, processes the dependencies,
imports the first snapshot if necessary and then applies all fragments to it"
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

    ;;Process dependencies
    (dolist (dependency-elem
	      (xpath-select-location-path feed-dom
                                    '((*egovpt-ns* "dependency"))))
      (let  ;;prevent circular dependencies
	  ((dependent-feed-url 
	    (xpath-fn-string dependency-elem)))
	(unless (find dependent-feed-url processed-feed-urls)
          (format t "Recursively processing feed ~a~&" dependent-feed-url)
	  (import-tm-feed dependent-feed-url (append processed-feed-urls feed-url)))))
	      
    ;; import a snapshot (if necessary) and the process all fragments more 
    ;; recent than the snapshot
    (let
	((imported-snapshot-entry
	  (import-snapshots-feed 
	   (get-attribute snapshot-feed-link-elem "href")
           :tm-id feed-url)))
      (assert imported-snapshot-entry)
      (import-fragments-feed 
       (get-attribute fragment-feed-link-elem "href")
       imported-snapshot-entry :tm-id feed-url))))
    

    