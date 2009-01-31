;; (defmacro site (&body lines)
;;            `(dolist (line (quote ,lines))
;;               (format t "~a~&" line)))

(in-package :atom)

(defmacro get-conflist (sym conflines)
  `(rest (assoc ,sym ,conflines)))

(defmacro get-confvalue (sym conflines)
  `(first (rest (assoc ,sym ,conflines))))

(defmacro build-updatefeed (collection-feed feed feedtype source-locator-prefix)
  "Helper macro to build an update feed (feedtype: snapshotsfeed or fragmentfeed)"
  `(let*
       ((entry 
         (get-conflist ,feedtype  (rest ,feed)))
        (entry-obj 
         (make-instance
          'collection-entry
          :id  (get-confvalue 'id entry)
          :link-type ,feedtype
          :tm-id (id ,collection-feed)
          :path (format nil "~a/~a" (path ,collection-feed) (get-confvalue 'relative-path entry))
          :title (get-confvalue 'title entry))))
     ;(format t "feed: ~a" ,feed)
     ;(format t "entry: ~a" entry)
     (register-entry ,collection-feed entry-obj)
     (register-subfeed 
      ,collection-feed 
      (make-instance
       ,feedtype
       :id (get-confvalue 'id entry)
       :author (author ,collection-feed)
       :path (format nil "~a/~a" (path ,collection-feed) (get-confvalue 'relative-path entry))
       :tm-id (id ,collection-feed)
       :source-locator-prefix ,source-locator-prefix
       :title (get-confvalue 'title entry)))))

(defmacro defsite (sitename &body conflines)
  "Macro to encapsulate the definition of feeds for the TMs the engine hosts"
 
  (setf *tm-feed*
        (make-instance 
         'feed
         :id (string-downcase sitename)
         :title (get-confvalue 'title conflines)
         :path (get-confvalue 'relative-path conflines)
         :author (get-confvalue 'author conflines)))
  
  (dolist (feed 
            (remove-if-not (lambda (elem) (eq elem 'collection-feed)) conflines :key #'first))
    
    (let*
        ((collection-url
          (format nil "~a/~a" (get-confvalue 'relative-path conflines) (get-confvalue 'relative-path (rest feed))))
         (source-locator-prefix (get-confvalue 'source-locator-prefix (rest feed)))
         (overview-entry 
          (make-instance 
           'overview-entry
           :id (get-confvalue 'id (rest feed))
           :title (get-confvalue 'title (rest feed))
           :author (get-confvalue 'author (rest feed))
           :path collection-url))
         (cf
          (make-instance 
           'collection-feed
           :id (get-confvalue 'id (rest feed))
           :title (get-confvalue 'title (rest feed))
           :source-locator-prefix source-locator-prefix
           :dependency (get-conflist 'dependency (rest feed))
           :author (get-confvalue 'author (rest feed))
           :path collection-url)))
      
      (register-entry *tm-feed* overview-entry)
      (register-subfeed *tm-feed* cf)
      
      (build-updatefeed cf feed 'fragments-feed source-locator-prefix)
      (build-updatefeed cf feed 'snapshots-feed source-locator-prefix))))



           

