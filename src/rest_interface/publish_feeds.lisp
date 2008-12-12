(in-package :rest-interface)

(defgeneric publish-feed (feed)
  (:documentation "Register feed urls with hunchentoot"))

(defmacro as-feed (feed)
  `(setf (content-type) "application/atom+xml; charset=UTF-8")
  `(cxml:with-xml-output (cxml:make-string-sink :canonical t)
     (atom:feed-to-elem ,feed)))

(defun overview-feed ()
  "Interface function to the corresponding Atom method"
  (setf (content-type) "application/atom+xml; charset=UTF-8")
  (as-feed atom:*tm-feed*))


(defmethod publish-feed ((feed atom:feed))
  (push 
   (create-regex-dispatcher 
    (format nil "~a~a" (path feed) "/?$") #'overview-feed)
   hunchentoot:*dispatch-table*)
  (mapc #'publish-feed (atom:subfeeds feed)))

(defmethod publish-feed ((feed atom:collection-feed))
  (push 
   (create-regex-dispatcher 
    (format nil "~a~a" (path feed) "/?$") 
    (lambda () 
 (setf (content-type) "application/atom+xml; charset=UTF-8")
      (as-feed feed)))
   hunchentoot:*dispatch-table*)
  (mapc #'publish-feed (atom:subfeeds feed)))

(defmethod publish-feed ((feed atom:fragments-feed))
  (push 
   (create-regex-dispatcher 
    (format nil "~a~a" (path feed) "/?$") 
    (lambda () 
 (setf (content-type) "application/atom+xml; charset=UTF-8")
      (as-feed feed)))
   hunchentoot:*dispatch-table*)
  ;and now register the general fragments method
  (push 
   (create-regex-dispatcher 
    (format nil "~a~a" (path feed) "/([0-9]+)$") 
    (lambda (&optional unique-id) 
      (setf (content-type) "application/x-tm+xml;version=1.0; charset=utf-8")
      (let 
          ((fragment 
            (d:get-fragment (parse-integer unique-id))))
        (if fragment
            (exporter:export-xtm-fragment fragment :xtm-format '1.0)
            (format nil "<t:topicMap xmlns:t=\"http://www.topicmaps.org/xtm/1.0/\" xmlns:xlink=\"http://www.w3.org/1999/xlink\"/>")))))
   hunchentoot:*dispatch-table*))

(defmethod publish-feed ((feed snapshots-feed))
  (push 
   (create-regex-dispatcher 
    (format nil "~a~a" (path feed) "/?$") 
    (lambda () 
 (setf (content-type) "application/atom+xml; charset=UTF-8")
      (as-feed feed)))
   hunchentoot:*dispatch-table*)
  ;and now register the general snapshots method
  (push 
   (create-regex-dispatcher 
    (format nil "~a~a" (path feed) "/([0-9]+)$") 
    (lambda (&optional revision) 
      (setf (content-type) "application/x-tm+xml;version=1.0; charset=utf-8")
      (exporter:export-xtm-to-string 
       :revision (parse-integer revision) 
       :tm-id (atom:tm-id feed)
       :xtm-format '1.0)))
   hunchentoot:*dispatch-table*))




