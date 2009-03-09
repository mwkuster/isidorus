(in-package :rest-interface)

(defparameter *json-rest-prefix* "/json/psi")
(defparameter *json-user-interface-url* "/isidorus")
(defparameter *json-user-interface-file-path* "json/json_interface.html")

(defun set-up-json-interface (&key (rest-prefix *json-rest-prefix*) (ui-url *json-user-interface-url*) (ui-file-path *json-user-interface-file-path*))
  "registers the json im/exporter to the passed base-url in hunchentoot's dispatch-table
   and also registers a file-hanlder to the html-user-interface"
  (declare (string rest-prefix ui-url ui-file-path))
  (let ((rest-regex (concatenate 'string rest-prefix "/(.+)$"))
	(ui-regex (concatenate 'string ui-url "/?$")))
    ;(format t "rest-interface: ~a~%user-interface: ~a~%user-interface-file-path: ~a~%" rest-regex ui-regex ui-file-path)
    (push
     (create-regex-dispatcher ui-regex #'(lambda()
					 (hunchentoot:handle-static-file ui-file-path)))
     hunchentoot:*dispatch-table*)
    (push
     (create-regex-dispatcher rest-regex
			      #'(lambda (&optional uri)
				  (assert uri)
					;decodes the url-encoding "%23" to "#" character (only the first which will be found)
				  (let ((identifier (let ((pos (search "%23" uri)))
						      (if pos
							  (let ((str-1 (subseq uri 0 pos))
								(str-2 (if (> (length uri) (+ pos 3))
									   (subseq uri (+ pos 3))
									   "")))
							    (concatenate 'string str-1 "#" str-2))
							  uri)))
					(http-method (request-method))
					(external-format (flexi-streams:make-external-format :UTF-8 :eol-style :LF))) ;is needed to get a string of the put-request
				    (with-open-file (stream "/home/lukas/Desktop/tmp2.txt" :direction :output :if-exists :supersede)
				      (format stream "http-method: ~a~%" http-method))
				    (cond
				      ((eq http-method :GET)
				       (progn
					 (setf (hunchentoot:content-type) "application/json") ;RFC 4627
					 (let ((fragment
						(get-latest-fragment-of-topic identifier)))
					   (if fragment
					       (handler-case (to-json-string fragment)
						 (condition (err) (progn
								    (setf (hunchentoot:return-code) hunchentoot:+http-internal-server-error+)
								    (format nil "<p style=\"color:red\">Condition: \"~a\"</p>" err))))
					       "{}"))))
				      ((eq http-method :PUT)
				       (let ((put-data (hunchentoot:raw-post-data :external-format external-format :force-text t)))	      
					 (handler-case (progn
							 (json-importer:json-to-elem put-data)
							 (setf (hunchentoot:return-code) hunchentoot:+http-ok+)
							 (setf (hunchentoot:content-type) "text")
							 (format nil "~a" hunchentoot:+http-ok+))
					   (condition (err) (progn
							      (setf (hunchentoot:return-code) hunchentoot:+http-internal-server-error+)
							      (format nil "<p style=\"color:red\">Condition: \"~a\"</p>" err))))))
				      ((eq http-method :POST)
				       (let ((post-data (hunchentoot:raw-post-data :external-format external-format :force-text t)))
					(with-open-file (stream "/home/lukas/Desktop/tmp.txt" :direction :output :if-exists :supersede)
					  (format stream "post-data: ~a~%" post-data))
					 (handler-case (progn
							 (json-importer:json-to-elem post-data)
							 (setf (hunchentoot:return-code) hunchentoot:+http-ok+)
							 (setf (hunchentoot:content-type) "text")
							 (format nil "~a" hunchentoot:+http-ok+))
					   (condition (err) (progn
							      (setf (hunchentoot:return-code) hunchentoot:+http-internal-server-error+)
							      (format nil "<p style=\"color:red\">Condition: \"~a\"</p>" err))))))
				      (t
				       (progn ;for all htt-methods except for get and post
					 (setf (hunchentoot:return-code) hunchentoot:+http-internal-server-error+)
					 (format nil "<p style=\"color:red\">You have to use either the HTTP-Method \"GET\" or \"PUT\", but you used \"~a\"</p>" http-method)))))))
     hunchentoot:*dispatch-table*)))



;
;				    (if (eq http-method :GET)
;					(progn
;					  (setf (hunchentoot:content-type) "application/json") ;RFC 4627
;					  (let ((fragment
;						 (get-latest-fragment-of-topic identifier)))
;					    (if fragment
;						(handler-case (to-json-string fragment)
;						  (condition (err) (progn
;								     (setf (hunchentoot:return-code) hunchentoot:+http-internal-server-error+)
;								     (format nil "<p style=\"color:red\">Condition: \"~a\"</p>" err))))
;						"{}")))
;					(if (eq http-method :PUT)
;					    (let ((put-data (hunchentoot:raw-post-data :external-format external-format :force-text t)))	      
;					      (handler-case (progn
;							      (json-importer:json-to-elem put-data)
;							      (setf (hunchentoot:return-code) hunchentoot:+http-ok+)
;							      (setf (hunchentoot:content-type) "text")
;							      (format nil "~a" hunchentoot:+http-ok+))
;						(condition (err) (progn
;								   (setf (hunchentoot:return-code) hunchentoot:+http-internal-server-error+)
;								   (format nil "<p style=\"color:red\">Condition: \"~a\"</p>" err)))))
;					    (if (eq http-method :POST)
;						(let ((post-data (hunchentoot:post-parameter "json-data")))
;						  (handler-case (progn
;							      (json-importer:json-to-elem post-data)
;							      (setf (hunchentoot:return-code) hunchentoot:+http-ok+)
;							      (setf (hunchentoot:content-type) "text")
;							      (format nil "~a" hunchentoot:+http-ok+))
;						    (condition (err) (progn
;								       (setf (hunchentoot:return-code) hunchentoot:+http-internal-server-error+)
;								       (format nil "<p style=\"color:red\">Condition: \"~a\"</p>" err)))))
;						(progn ;for all htt-methods except for get and post
;						  (setf (hunchentoot:return-code) hunchentoot:+http-internal-server-error+)
;						  (format nil "<p style=\"color:red\">You have to use either the HTTP-Method \"GET\" or \"PUT\", but you used \"~a\"</p>" http-method))))))))
;     hunchentoot:*dispatch-table*)))