;;+-----------------------------------------------------------------------------
;;+  Isidorus
;;+  (c) 2008-2009 Marc Kuester, Christoph Ludwig, Lukas Giessmann
;;+
;;+  Isidorus is freely distributable under the LGPL license.
;;+  You can find a detailed description in trunk/docs/LGPL-LICENSE.txt.
;;+-----------------------------------------------------------------------------


(in-package :rest-interface)

(defparameter *json-get-prefix* "/json/get/(.+)$") ;the prefix to get a fragment by the psis -> localhost:8000/json/get/<fragment-psi>
(defparameter *json-commit-url* "/json/commit/?$") ;the url to commit a json fragment by "put" or "post"
(defparameter *json-get-all-psis* "/json/psis/?$") ;the url to get all topic psis of isidorus -> localhost:8000/json/psis
(defparameter *json-get-summary-url* "/json/summary/?$") ;the url to get a summary od all topic stored in isidorus; you have to set the GET-parameter "start" for the start index of all topics within elephant and the GET-paramter "end" for the last index of the topic sequence -> http://localhost:8000/json/summary/?start=12&end=13
(defparameter *ajax-user-interface-url* "/isidorus/?$") ;the url to the user interface; if you want to get all topics set start=0&end=nil -> localhost:8000/isidorus
(defparameter *ajax-user-interface-css-prefix* "/css") ;the url to the css files of the user interface
(defparameter *ajax-user-interface-css-directory-path* "ajax/css") ;the directory contains the css files
(defparameter *ajax-user-interface-file-path* "ajax/isidorus.html") ;the file path to the HTML file implements the user interface
(defparameter *ajax-javascript-directory-path* "ajax/javascripts") ;the directory which contains all necessary javascript files
(defparameter *ajax-javascript-url-prefix* "/javascripts") ; the url prefix of all javascript files


(defun set-up-json-interface (&key (json-get-prefix *json-get-prefix*)
			      (json-get-all-psis *json-get-all-psis*)
			      (json-commit-url *json-commit-url*)
			      (json-get-summary-url *json-get-summary-url*)
			      (ajax-user-interface-url *ajax-user-interface-url*)
			      (ajax-user-interface-file-path *ajax-user-interface-file-path*)
			      (ajax-user-interface-css-prefix *ajax-user-interface-css-prefix*)
			      (ajax-user-interface-css-directory-path *ajax-user-interface-css-directory-path*)
			      (ajax-javascripts-directory-path *ajax-javascript-directory-path*)
			      (ajax-javascripts-url-prefix *ajax-javascript-url-prefix*))
  "registers the json im/exporter to the passed base-url in hunchentoot's dispatch-table
   and also registers a file-hanlder to the html-user-interface"
  ;; === html and css files ====================================================
  (push
   (create-regex-dispatcher ajax-user-interface-url
			    #'(lambda()
				(hunchentoot:handle-static-file ajax-user-interface-file-path "text/html")))
   hunchentoot:*dispatch-table*)

  (dolist (script-path-and-url (make-file-path-and-url ajax-user-interface-css-directory-path ajax-user-interface-css-prefix))
    (let ((script-path (getf script-path-and-url :path))
	  (script-url (getf script-path-and-url :url)))
      (push (create-regex-dispatcher script-url
				     #'(lambda()
					 (hunchentoot:handle-static-file script-path))); "text/javascript")))
	    hunchentoot:*dispatch-table*)))

  ;; === ajax frameworks and javascript files ==================================
  (dolist (script-path-and-url (make-file-path-and-url ajax-javascripts-directory-path ajax-javascripts-url-prefix))
    (let ((script-path (getf script-path-and-url :path))
	  (script-url (getf script-path-and-url :url)))
      (push (create-regex-dispatcher script-url
				     #'(lambda()
					 (hunchentoot:handle-static-file script-path))); "text/javascript")))
	    hunchentoot:*dispatch-table*)))

  ;; === rest interface ========================================================
  (push
   (create-regex-dispatcher json-get-all-psis #'return-all-topic-psis)
   hunchentoot:*dispatch-table*)
  (push
   (create-regex-dispatcher json-get-prefix #'return-json-fragment)
   hunchentoot:*dispatch-table*)
  (push
   (create-regex-dispatcher json-commit-url #'json-commit)
   hunchentoot:*dispatch-table*)
  (push
   (create-regex-dispatcher json-get-summary-url #'return-topic-summaries)
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


(defun return-topic-summaries(&optional param)
  "returns a summary of the requested topics"
  (declare (ignorable param))
  (let ((start-idx 
	 (handler-case (parse-integer (hunchentoot:get-parameter "start"))
	   (condition () 0)))
	(end-idx
	 (handler-case (parse-integer (hunchentoot:get-parameter "end"))
	   (condition () nil))))

    (let ((topics (elephant:get-instances-by-class 'd:TopicC)))
      (let ((end
	     (cond
	       ((not end-idx)
		(length topics))
	       ((> end-idx (length topics))
		(length topics))
	       ((< end-idx 0)
		0)
	       (t
		end-idx))))
	(let ((start
	       (cond
		 ((> start-idx (length topics))
		  end)
		 ((< start-idx 0)
		  0)
		 (t
		  start-idx))))
	  (let ((topics-in-range
		 (if (<= start end)
		     (subseq topics start end)
		     (reverse (subseq topics end start)))))
	    (setf (hunchentoot:content-type*) "application/json") ;RFC 4627
	    (json-exporter:make-topic-summary topics-in-range)))))))


;; =============================================================================
;; --- some helper functions ---------------------------------------------------
;; =============================================================================
(defun make-file-path-and-url (path-to-files-directory url-prefix)
  "returns a list of lists which contains an absolute file path and a file-url
   concatenated of the url-prefix and the relative path of all all files in the
   passed directory and its subdirectories"
  (let ((start-position-of-relative-path
	 (- (length (write-to-string (com.gigamonkeys.pathnames:file-exists-p path-to-files-directory))) 2)))
    (let ((files-and-urls nil))
      (com.gigamonkeys.pathnames:walk-directory path-to-files-directory
						#'(lambda(current-path)
						    (let ((current-path-string
							   (write-to-string current-path)))
						      (let ((last-position-of-current-path
							     (- (length current-path-string) 1)))
							(let ((current-url
							       (concatenate 'string url-prefix
									    (subseq current-path-string start-position-of-relative-path last-position-of-current-path))))
							  (push (list :path current-path :url current-url) files-and-urls))))))
      files-and-urls)))