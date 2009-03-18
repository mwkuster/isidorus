(in-package :rest-interface)

(defparameter *json-get-prefix* "/json/get/(.+)$") ;the prefix to get a fragment by the psis -> localhost:8000/json/get/<fragment-psi>
(defparameter *json-commit-url* "/json/commit/?$") ;the url to commit a json fragment by "put" or "post"
(defparameter *json-get-all-psis* "/json/psis/?$") ;the url to get all topic psis of isidorus -> localhost:8000/json/psis
(defparameter *json-user-interface-url* "/isidorus/?$") ;the url to the user interface -> localhost:8000/isidorus
(defparameter *json-user-interface-file-path* "json/json_interface.html") ;the file path to the HTML file implements the user interface


(defun set-up-json-interface (&key (json-get-prefix *json-get-prefix*) (json-get-all-psis *json-get-all-psis*)
			      (json-commit-url *json-commit-url*) (json-user-interface-url *json-user-interface-url*)
			      (json-user-interface-file-path *json-user-interface-file-path*))
  "registers the json im/exporter to the passed base-url in hunchentoot's dispatch-table
   and also registers a file-hanlder to the html-user-interface"
  (declare (string json-get-prefix json-get-all-psis json-commit-url json-user-interface-url json-user-interface-file-path))
  (push ;TODO create a static-file-and-folder-handler for all static files
   (create-regex-dispatcher json-user-interface-url
			    #'(lambda()
				(hunchentoot:handle-static-file json-user-interface-file-path "text/html")))
   hunchentoot:*dispatch-table*)
  ;; === ajax frameworks =======================================================
  ;; TODO: *define a new parameter which represents the folder of the javascript
  ;;        files and load all files, better would be a "folder-dispatcher-and-handler"
  ;;        of hunchentoot, but it seems to have some problems???
  ;;       *a second parameter should represent the prefix of the javascript files
  ;;       *the own javascript code should be extracted of the .html-file into a
  ;;        javascript file in the javascripts directory
  (dolist (script '("builder.js" "controls.js" "dragdrop.js" "effects.js" "prototype-1.6.0.3.js" "scriptaculous.js" "slider.js" "sound.js" "unittest.js"))
    (let ((script-path (concatenate 'string "json/javascripts/" script))
	  (script-url (concatenate 'string "/javascripts/" script)))
      (push
       (create-regex-dispatcher script-url
				#'(lambda()
				    (hunchentoot:handle-static-file script-path "text/javascript")))
       hunchentoot:*dispatch-table*)))
  ;; === ajax frameworks end ===================================================
  (push
   (create-regex-dispatcher json-get-all-psis #'return-all-topic-psis)
   hunchentoot:*dispatch-table*)
  (push
   (create-regex-dispatcher json-get-prefix #'return-json-fragment)
   hunchentoot:*dispatch-table*)
  (push
   (create-regex-dispatcher json-commit-url #'json-commit)
   hunchentoot:*dispatch-table*))


;; =============================================================================
;; --- some handlers for the json-rest-interface -------------------------------
;; =============================================================================
(defun return-all-topic-psis (&optional param)
  "return all psis currently existing in isidorus as a list of list. every topic is a list
   of psis and the entire list contains a list of topics"
  (declare (ignorable param))
  (let ((http-method (hunchentoot:request-method*)))
    (if (eq http-method :GET)
	(progn
	  (setf (hunchentoot:content-type*) "application/json") ;RFC 4627
	  (get-all-topic-psis))
	(setf (hunchentoot:return-code*) hunchentoot:+http-bad-request+))))


(defun return-json-fragment(&optional psi)
  "returns the json-fragmen belonging to the psi passed by the parameter psi"
  (assert psi)
  (let ((http-method (hunchentoot:request-method*)))
    (if (eq http-method :GET)
	(let ((identifier (let ((pos (search "%23" psi)))
			    (if pos
				(let ((str-1 (subseq psi 0 pos))
				      (str-2 (if (> (length psi) (+ pos 3))
						 (subseq psi (+ pos 3))
						 "")))
				  (concatenate 'string str-1 "#" str-2))
				psi))))
	  (setf (hunchentoot:content-type*) "application/json") ;RFC 4627
	  (let ((fragment
		 (get-latest-fragment-of-topic identifier)))
	    (if fragment
		(handler-case (to-json-string fragment)
		  (condition (err)
		    (progn
		      (setf (hunchentoot:return-code*) hunchentoot:+http-internal-server-error+)
		      (format nil "<p>Condition: \"~a\"</p>" err))))
		"{}")))
	(setf (hunchentoot:return-code*) hunchentoot:+http-bad-request+))))


(defun json-commit(&optional param)
  "calls the json-to-elem method for a json-fragment and imports it to elephant"
  (declare (ignorable param)) ;param is currently not used
  (let ((http-method (hunchentoot:request-method*)))
    (if (or (eq http-method :PUT)
	    (eq http-method :POST))
	(let ((external-format (flexi-streams:make-external-format :UTF-8 :eol-style :LF)))
	  (let ((json-data (hunchentoot:raw-post-data :external-format external-format :force-text t)))
	    (handler-case (json-importer:json-to-elem json-data)
	      (condition (err)
		(progn
		  (setf (hunchentoot:return-code*) hunchentoot:+http-internal-server-error+)
		  (format nil "<p>Condition: \"~a\"</p>" err))))))
	(setf (hunchentoot:return-code*) hunchentoot:+http-bad-request+))))