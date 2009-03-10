(in-package :rest-interface)

(defparameter *json-rest-prefix* "/json/psi") ;the prefix to get a fragment by the psis -> localhost:8000/json/psi/<fragment-psi>
(defparameter *json-rest-all-psis* "/json/psis") ;the url to get all topic psis of isidorus -> localhost:8000/json/psis
(defparameter *json-user-interface-url* "/isidorus") ;the url to the user interface -> localhost:8000/isidorus
(defparameter *json-user-interface-file-path* "json/json_interface.html") ;the file path to the HTML file implements the user interface


(defun set-up-json-interface (&key (rest-prefix *json-rest-prefix*) (rest-all-psis *json-rest-all-psis*)
			      (ui-url *json-user-interface-url*) (ui-file-path *json-user-interface-file-path*))
  "registers the json im/exporter to the passed base-url in hunchentoot's dispatch-table
   and also registers a file-hanlder to the html-user-interface"
  (declare (string rest-prefix ui-url ui-file-path))
  (let ((rest-regex (concatenate 'string rest-prefix "/(.+)$"))
	(ui-regex (concatenate 'string ui-url "/?$"))
	(all-psis-regex (concatenate 'string rest-all-psis "/?$")))
    ;(format t "rest-interface: ~a~%user-interface: ~a~%user-interface-file-path: ~a~%" rest-regex ui-regex ui-file-path)
    (push
     (create-regex-dispatcher ui-regex #'(lambda()
					 (hunchentoot:handle-static-file ui-file-path)))
     hunchentoot:*dispatch-table*)
    (push
     (create-regex-dispatcher all-psis-regex #'(lambda()
						 (setf (hunchentoot:content-type) "application/json") ;RFC 4627
						 (get-all-topic-psis)))
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
				      ))))
;;				      ((eq http-method :POST)
;;				       (let ((post-data (hunchentoot:raw-post-data :external-format external-format :force-text t)))
;;					 (handler-case (progn
;;							 (json-importer:json-to-elem post-data)
;;							 (setf (hunchentoot:return-code) hunchentoot:+http-ok+)
;;							 (setf (hunchentoot:content-type) "text")
;;							 (format nil "~a" hunchentoot:+http-ok+))
;;					   (condition (err) (progn
;;							      (setf (hunchentoot:return-code) hunchentoot:+http-internal-server-error+)
;;							      (format nil "<p style=\"color:red\">Condition: \"~a\"</p>" err))))))
;;				      (t
;;				       (progn ;for all htt-methods except for get and post
;;					 (setf (hunchentoot:return-code) hunchentoot:+http-internal-server-error+)
;;					 (format nil "<p style=\"color:red\">You have to use either the HTTP-Method \"GET\" or \"PUT\", but you used \"~a\"</p>" http-method)))))))
     hunchentoot:*dispatch-table*)))