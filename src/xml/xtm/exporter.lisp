;;+-----------------------------------------------------------------------------
;;+  Isidorus
;;+  (c) 2008-2009 Marc Kuester, Christoph Ludwig, Lukas Giessmann
;;+
;;+  Isidorus is freely distributable under the LGPL license.
;;+  You can find a detailed description in trunk/docs/LGPL-LICENSE.txt.
;;+-----------------------------------------------------------------------------


(in-package :exporter)


;; (defun instanceofs-to-elem (ios)
;;   (when ios
;;       (map 'list (lambda (io) (cxml:with-element "t:instanceOf" (ref-to-elem io))) ios)))


(defun list-extern-associations ()
  "gets all instances of AssociationC - which does not realize an instanceOf relationship in the db"
  (let ((instance-topic 
	 (identified-construct
	  (elephant:get-instance-by-value 'PersistentIdC 'uri "http://psi.topicmaps.org/iso13250/model/instance")))
	(type-topic 
	 (identified-construct 
	  (elephant:get-instance-by-value 'PersistentIdC 'uri "http://psi.topicmaps.org/iso13250/model/type"))))
    (loop for item in (elephant:get-instances-by-class 'AssociationC)
       when (not (and (or (eq instance-topic (instance-of (first (roles item))))
			  (eq instance-topic (instance-of (second (roles item)))))
		      (or (eq type-topic (instance-of (first (roles item))))
			  (eq type-topic (instance-of (second (roles item)))))))
       collect item)))

(defmacro with-xtm2.0 (&body body)
  "helper macro to build the Topic Map element"
    `(cxml:with-namespace ("t" *xtm2.0-ns*)
      (cxml:with-element 
          "t:topicMap" :empty
          (cxml:attribute "version" "2.0")
          ,@body)))


(defmacro with-xtm1.0 (&body body)
  "helper macro to build the Topic Map element"
    `(cxml:with-namespace ("t" *xtm1.0-ns*)
       (cxml:with-namespace ("xlink" *xtm1.0-xlink*)
	 (cxml:with-element 
             "t:topicMap" :empty
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
                    (d:topics ,tm) (d:associations ,tm))
                  (union
                   (elephant:get-instances-by-class 'd:TopicC)
                   (list-extern-associations)))))))

(defun export-xtm (xtm-path &key 
                   tm-id
                   (revision (get-revision)) 
                   (xtm-format '2.0))
  (with-reader-lock
    (let
	((tm 
	  (when tm-id
	    (get-item-by-item-identifier tm-id :revision revision))))
      (setf *export-tm* tm)
      (with-revision revision
	(setf SB-IMPL::*DEFAULT-EXTERNAL-FORMAT* :utf-8)
	(with-open-file (stream xtm-path :direction :output)
	  (cxml:with-xml-output (cxml:make-character-stream-sink stream :canonical nil)
	    (if (eq xtm-format '2.0)
		(with-xtm2.0
                  (export-to-elem tm #'to-elem))
		(with-xtm1.0
                  (export-to-elem tm #'to-elem-xtm1.0)))))))))


(defun export-xtm-to-string (&key 
                             tm-id
                             (revision (get-revision)) (xtm-format '2.0))
  (with-reader-lock
    (let
	((tm 
	  (when tm-id
	    (get-item-by-item-identifier tm-id :revision revision))))
      (with-revision revision
	(cxml:with-xml-output (cxml:make-string-sink :canonical nil)
	  (if (eq xtm-format '2.0)
	      (with-xtm2.0
		(export-to-elem tm #'to-elem))
	      (with-xtm1.0
		(export-to-elem tm #'to-elem-xtm1.0))))))))


(defun export-xtm-fragment (fragment &key (xtm-format '2.0))
  (declare (FragmentC fragment))
  (with-reader-lock
    (with-revision (revision fragment)
      (cxml:with-xml-output  (cxml:make-string-sink :canonical nil)
	(if (eq xtm-format '2.0)
	    (with-xtm2.0
              (to-elem fragment))
	    (with-xtm1.0
              (to-elem-xtm1.0 fragment)))))))
	  