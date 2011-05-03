;;+-----------------------------------------------------------------------------
;;+  Isidorus
;;+  (c) 2008-2010 Marc Kuester, Christoph Ludwig, Lukas Georgieff
;;+
;;+  Isidorus is freely distributable under the LLGPL license.
;;+  You can find a detailed description in trunk/docs/LLGPL-LICENSE.txt and
;;+  trunk/docs/LGPL-LICENSE.txt.
;;+-----------------------------------------------------------------------------

(in-package :xtm-exporter)


(defun list-extern-associations (&key (revision *TM-REVISION*))
  "gets all instances of AssociationC - which does not realize an
   instanceOf relationship in the db"
  (let ((instance-topic 
	 (identified-construct
	  (elephant:get-instance-by-value 'PersistentIdC 'uri *instance-psi*)))
	(type-topic 
	 (identified-construct 
	  (elephant:get-instance-by-value 'PersistentIdC 'uri *type-psi*))))
    (loop for item in (d:get-all-associations revision) 
       when (and (= (length (roles item :revision revision)) 2)
		 (not (and (or (eq instance-topic
				   (instance-of (first (roles item
							      :revision revision))
						:revision revision))
			       (eq instance-topic
				   (instance-of (second (roles item
							       :revision revision))
						:revision revision)))
			   (or (eq type-topic
				   (instance-of (first (roles item
							      :revision revision))
						:revision revision))
			       (eq type-topic 
				   (instance-of (second (roles item
							       :revision revision))
						:revision revision))))))
       collect item)))


(defmacro with-xtm2.0 ((tm revision) &body body)
  "helper macro to build the Topic Map element"
    `(cxml:with-namespace ("t" *xtm2.0-ns*)
      (cxml:with-element 
          "t:topicMap" :empty
          (cxml:attribute "version" "2.0")
	  (when ,tm
	    (to-reifier-elem ,tm ,revision)
	    (map 'list #'(lambda(x)
			   (to-elem x ,revision))
		 (item-identifiers ,tm :revision ,revision)))
          ,@body)))


(defmacro with-xtm1.0 ((tm revision) &body body)
  "helper macro to build the Topic Map element"
    `(cxml:with-namespace ("t" *xtm1.0-ns*)
       (cxml:with-namespace ("xlink" *xtm1.0-xlink*)
	 (cxml:with-element 
             "t:topicMap" :empty
	     (when ,tm
	       (to-reifier-elem-xtm1.0 ,tm ,revision))
	     ,@body))))


(defmacro export-to-elem (tm to-elem)
  `(setf *export-tm* ,tm)
  `(format t "*export-tm*: ~a" *export-tm*)
  `(map 'list 
        ,to-elem
        (remove-if 
         #'null 
         (map 'list 
              #'(lambda(top)
                  (d:find-item-by-revision top revision))
	      (if ,tm
		  (union
		   (filter-type-instance-topics (d:topics ,tm) tm :revision revision)
		   (d:associations ,tm))
		  (union
		   (elephant:get-instances-by-class 'd:TopicC)
		   (list-extern-associations :revision revision)))))))


(defun export-as-xtm (xtm-path &key 
		      tm-id
		      (revision (get-revision)) 
		      (xtm-format :2.0))
  (declare (type (or Null String) tm-id)
	   (Integer revision)
	   (Keyword xtm-format))
  (with-reader-lock
    (let ((tm 
	   (when tm-id
	     (get-item-by-item-identifier tm-id :revision revision))))
      (setf *export-tm* tm)
      (with-revision revision
	(setf SB-IMPL::*DEFAULT-EXTERNAL-FORMAT* :utf-8)
	(with-open-file (stream xtm-path :direction :output)
	  (cxml:with-xml-output (cxml:make-character-stream-sink stream :canonical nil)
	    (if (eq xtm-format :2.0)
		(with-xtm2.0 (tm revision)
		  (export-to-elem tm #'(lambda(elem)
					 (to-elem elem revision))))
		(with-xtm1.0 (tm revision)
		  (export-to-elem tm #'(lambda(elem)
					 (to-elem-xtm1.0 elem revision)))))))))))


(defun export-as-xtm-string (&key 
                             tm-id
                             (revision (get-revision))
			     (xtm-format :2.0))
  (declare (type (or Null String) tm-id)
	   (Integer revision)
	   (Keyword xtm-format))
  (with-reader-lock
    (let ((tm 
	   (when tm-id
	     (get-item-by-item-identifier tm-id :revision revision))))
      (with-revision revision
	(cxml:with-xml-output (cxml:make-string-sink :canonical nil)
	  (if (eq xtm-format :2.0)
	      (with-xtm2.0 (tm revision)
		(export-to-elem tm #'(lambda(elem)
				       (to-elem elem revision))))
	      (with-xtm1.0 (tm revision)
		(export-to-elem tm #'(lambda(elem)
				       (to-elem-xtm1.0 elem revision))))))))))


(defgeneric export-construct-as-xtm-string (construct &key xtm-format)
  (:documentation "Exports a FragmentC object as xtm string.")
  (:method ((construct FragmentC) &key (xtm-format :2.0))
    (declare (Keyword xtm-format))
    (with-reader-lock
      (with-revision (revision construct)
	(cxml:with-xml-output  (cxml:make-string-sink :canonical nil)
	  (if (eq xtm-format :2.0)
	      (with-xtm2.0 (nil nil)
		(to-elem construct (revision construct)))
	      (with-xtm1.0 (nil nil)
		(to-elem-xtm1.0 construct (revision construct)))))))))