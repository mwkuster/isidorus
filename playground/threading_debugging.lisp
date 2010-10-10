(require :asdf)
(asdf:operate 'asdf:load-op :isidorus)
(xml-importer:setup-repository "textgrid.xtm" "data_base"
			       :tm-id "http://ztt.fh-worms.de/textgrid.xtm"
			       :xtm-id "textgrid.xtm")


(defun return-all-tmcl-types-test-handler (&optional param)
  "similar to hunchentoot's corresponding handler - but without hunchentoot's
   variables, e.g. hunchentoot:content-type, ..."
  (declare (ignorable param))
  (handler-case (let ((topic-types
		       (isidorus-threading:with-reader-lock
			   (json-tmcl::return-all-tmcl-types :revision 0))))
		  (json:encode-json-to-string
		   (map 'list #'(lambda(y)
				  (map 'list #'d:uri y))
			(map 'list #'d:psis topic-types))))
    (condition (err) (error (format nil "~a" err)))))


(defun return-all-tmcl-instances-test-handler(&optional param)
  "similar to hunchentoot's corresponding handler - but without hunchentoot's
   variables, e.g. hunchentoot:content-type, ..."
  (declare (ignorable param))
  (handler-case (let ((topic-instances 
		       (isidorus-threading:with-reader-lock
			   (json-tmcl::return-all-tmcl-instances :revision 0))))
		  (json:encode-json-to-string
		   (map 'list #'(lambda(y)
				  (map 'list #'d:uri y))
			(map 'list #'d:psis topic-instances))))
    (condition (err) (error (format nil "~a" err)))))



(defun return-all-topic-psis-test-handler (&optional param)
  "similar to hunchentoot's corresponding handler - but without hunchentoot's
   variables, e.g. hunchentoot:content-type, ..."
  (declare (ignorable param))
  (handler-case (isidorus-threading:with-reader-lock
		    (json-exporter::get-all-topic-psis :revision 0))
    (condition (err) (error (format nil "~a" err)))))


(defun my-thread-function-1 ()
  (dotimes (i 100)
    (return-all-tmcl-types-test-handler)))


(defun programm-1 (thread-fun)
  "bordeaux-threads"
  (defvar *thread-1* (bordeaux-threads:make-thread thread-fun))
  (defvar *thread-2* (bordeaux-threads:make-thread thread-fun)))


(defun programm-2 (thread-fun)
  "bordeaux-threads"
  (let ((thread-1 nil)
	(thread-2 nil)
	(max-iterations 150))
    (do ((c1 0 (+ c1 0))
	 (c2 0 (+ c2 0)))
	((and (>= c1 max-iterations) (>= c2 max-iterations)))
      (when (or (not thread-1) (not (bordeaux-threads:thread-alive-p thread-1)))
	(setf thread-1 (bordeaux-threads:make-thread thread-fun))
	(incf c1)
	(format t "c1: ~a  c2: ~a~%" c1 c2))
      (when (or (not thread-2) (not (bordeaux-threads:thread-alive-p thread-2)))
	(setf thread-2 (bordeaux-threads:make-thread thread-fun))
	(incf c2)
	(format t "c1: ~a  c2: ~a~%" c1 c2)))))


(defun programm-3 (thread-fun)
  "sb-thread"
  (defvar *thread-3* (sb-thread:make-thread thread-fun))
  (defvar *thread-4* (sb-thread:make-thread thread-fun)))


(defun programm-4 (thread-fun)
  "sb-thread"
  (let ((thread-1 nil)
	(thread-2 nil)
	(max-iterations 150))
    (do ((c1 0 (+ c1 0))
	 (c2 0 (+ c2 0)))
	((and (>= c1 max-iterations) (>= c2 max-iterations)))
      (when (or (not thread-1) (not (sb-thread:thread-alive-p thread-1)))
	(setf thread-1 (sb-thread:make-thread thread-fun))
	(incf c1)
	(format t "c1: ~a  c2: ~a~%" c1 c2))
      (when (or (not thread-2) (not (sb-thread:thread-alive-p thread-2)))
	(setf thread-2 (sb-thread:make-thread thread-fun))
	(incf c2)
	(format t "c1: ~a  c2: ~a~%" c1 c2)))))





(defun main()
  (programm-4 #'return-all-tmcl-types-test-handler))


(main)