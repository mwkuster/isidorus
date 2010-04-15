;;+-----------------------------------------------------------------------------
;;+  Isidorus
;;+  (c) 2008-2009 Marc Kuester, Christoph Ludwig, Lukas Giessmann
;;+
;;+  Isidorus is freely distributable under the LGPL license.
;;+  You can find a detailed description in trunk/docs/LGPL-LICENSE.txt.
;;+-----------------------------------------------------------------------------


(in-package :rest-interface)

(defparameter *json-get-prefix* "/json/get/(.+)$") ;the prefix to get a fragment by the psi -> localhost:8000/json/get/<fragment-psi>
(defparameter *get-rdf-prefix* "/json/get/rdf/(.+)$") ;the prefix to get a fragment by the psi -> localhost:8000/json/rdf/get/<fragment-psi>
(defparameter *json-commit-url* "/json/commit/?$") ;the url to commit a json fragment by "put" or "post"
(defparameter *json-get-all-psis* "/json/psis/?$") ;the url to get all topic psis of isidorus -> localhost:8000/json/psis
(defparameter *json-get-summary-url* "/json/summary/?$") ;the url to get a summary of all topic stored in isidorus; you have to set the GET-parameter "start" for the start index of all topics within elephant and the GET-paramter "end" for the last index of the topic sequence -> http://localhost:8000/json/summary/?start=12&end=13
(defparameter *json-get-all-type-psis* "/json/tmcl/types/?$") ;returns a list of all psis that can be a type
(defparameter *json-get-all-instance-psis* "/json/tmcl/instances/?$") ;returns a list of all psis that belongs to a valid topic-instance
(defparameter *json-get-topic-stub-prefix* "/json/topicstubs/(.+)$") ;the json prefix for getting some topic stub information of a topic
(defparameter *json-get-type-tmcl-url* "/json/tmcl/type/?$") ;the json url for getting some tmcl information of a topic treated as a type
(defparameter *json-get-instance-tmcl-url* "/json/tmcl/instance/?$") ;the json url for getting some tmcl information of a topic treated as an instance
(defparameter *json-get-overview* "/json/tmcl/overview/?$") ; returns a json-object representing a tree view
(defparameter *ajax-user-interface-url* "/isidorus") ;the url to the user interface;
(defparameter *ajax-user-interface-css-prefix* "/css") ;the url to the css files of the user interface
(defparameter *ajax-user-interface-css-directory-path* "ajax/css") ;the directory contains the css files
(defparameter *ajax-user-interface-file-path* "ajax/isidorus.html") ;the file path to the HTML file implements the user interface
(defparameter *ajax-javascript-directory-path* "ajax/javascripts") ;the directory which contains all necessary javascript files
(defparameter *ajax-javascript-url-prefix* "/javascripts") ; the url prefix of all javascript files
(defparameter *mark-as-deleted-url* "/mark-as-deleted") ; the url suffix that calls the mark-as-deleted handler

(defun set-up-json-interface (&key (json-get-prefix *json-get-prefix*)
			      (get-rdf-prefix *get-rdf-prefix*)
			      (json-get-all-psis *json-get-all-psis*)
			      (json-commit-url *json-commit-url*)
			      (json-get-summary-url *json-get-summary-url*)
			      (json-get-all-type-psis *json-get-all-type-psis*)
			      (json-get-all-instance-psis *json-get-all-instance-psis*)
			      (json-get-topic-stub-prefix *json-get-topic-stub-prefix*)
			      (json-get-type-tmcl-url *json-get-type-tmcl-url*)
			      (json-get-instance-tmcl-url *json-get-instance-tmcl-url*)
			      (json-get-overview *json-get-overview*)
			      (ajax-user-interface-url *ajax-user-interface-url*)
			      (ajax-user-interface-file-path *ajax-user-interface-file-path*)
			      (ajax-user-interface-css-prefix *ajax-user-interface-css-prefix*)
			      (ajax-user-interface-css-directory-path *ajax-user-interface-css-directory-path*)
			      (ajax-javascripts-directory-path *ajax-javascript-directory-path*)
			      (ajax-javascripts-url-prefix *ajax-javascript-url-prefix*)
			      (mark-as-deleted-url *mark-as-deleted-url*))
  "registers the json im/exporter to the passed base-url in hunchentoot's dispatch-table
   and also registers a file-hanlder to the html-user-interface"

  ;; registers the http-code 500 for an internal server error to the standard
  ;; return codes. so there won't be attached a hunchentoot default message,
  ;; this is necessary to be able to send error messages in an individual way/syntax
  ;; e.g. a json error-message.
  (push hunchentoot:+http-internal-server-error+ hunchentoot:*approved-return-codes*)
  ;; === html and css files ====================================================
  (push
   (create-static-file-dispatcher-and-handler ajax-user-interface-url ajax-user-interface-file-path "text/html")
   hunchentoot:*dispatch-table*)

  (dolist (script-path-and-url (make-file-path-and-url ajax-user-interface-css-directory-path ajax-user-interface-css-prefix))
    (let ((script-path (getf script-path-and-url :path))
	  (script-url (getf script-path-and-url :url)))
      (push 
       (create-static-file-dispatcher-and-handler script-url script-path)
       hunchentoot:*dispatch-table*)))


  ;; === ajax frameworks and javascript files ==================================
  (dolist (script-path-and-url (make-file-path-and-url ajax-javascripts-directory-path ajax-javascripts-url-prefix))
    (let ((script-path (getf script-path-and-url :path))
	  (script-url (getf script-path-and-url :url)))
      (push 
       (create-static-file-dispatcher-and-handler script-url script-path)
       hunchentoot:*dispatch-table*)))
  

  ;; === rest interface ========================================================
  (push
   (create-regex-dispatcher json-get-all-psis #'return-all-topic-psis)
   hunchentoot:*dispatch-table*)
  (push
   (create-regex-dispatcher json-get-prefix #'return-json-fragment)
   hunchentoot:*dispatch-table*)
  (push
   (create-regex-dispatcher get-rdf-prefix #'return-json-rdf-fragment)
   hunchentoot:*dispatch-table*)
  (push
   (create-regex-dispatcher json-get-topic-stub-prefix #'return-topic-stub-of-psi)
   hunchentoot:*dispatch-table*)
  (push
   (create-regex-dispatcher json-get-all-type-psis #'return-all-tmcl-types)
   hunchentoot:*dispatch-table*)
  (push
   (create-regex-dispatcher json-get-all-instance-psis #'return-all-tmcl-instances)
   hunchentoot:*dispatch-table*)
  (push
   (create-regex-dispatcher json-get-type-tmcl-url #'(lambda(&optional param)
						       (declare (ignorable param))
						       (return-tmcl-info-of-psis 'json-tmcl::type)))
   hunchentoot:*dispatch-table*)
  (push
   (create-regex-dispatcher json-get-instance-tmcl-url #'(lambda(&optional param)
							   (declare (ignorable param))
							   (return-tmcl-info-of-psis 'json-tmcl::instance)))
   hunchentoot:*dispatch-table*)
  (push
   (create-regex-dispatcher json-get-overview #'return-overview)
   hunchentoot:*dispatch-table*)
  (push
   (create-regex-dispatcher json-commit-url #'json-commit)
   hunchentoot:*dispatch-table*)
  (push
   (create-regex-dispatcher json-get-summary-url #'return-topic-summaries)
   hunchentoot:*dispatch-table*)
  (push
   (create-regex-dispatcher mark-as-deleted-url #'mark-as-deleted-handler)
   hunchentoot:*dispatch-table*))

;; =============================================================================
;; --- some handlers for the json-rest-interface -------------------------------
;; =============================================================================
(defun return-all-tmcl-types(&optional param)
  "Returns all topic-psi that are valid types -> so they have to be valid to the
   topictype-constraint (if it exists) and the can't be abstract."
  (declare (ignorable param))
  (handler-case (let ((topic-types 
		         (with-reader-lock
			   (json-tmcl::return-all-tmcl-types))))
		  (setf (hunchentoot:content-type*) "application/json") ;RFC 4627
		  (json:encode-json-to-string
		   (map 'list #'(lambda(y)
				  (map 'list #'uri y))
			(map 'list #'psis topic-types))))
    (condition (err) (progn
		       (setf (hunchentoot:return-code*) hunchentoot:+http-internal-server-error+)
		       (setf (hunchentoot:content-type*) "text")
		       (format nil "Condition: \"~a\"" err)))))

(defun return-all-tmcl-instances(&optional param)
  "Returns all topic-psis that are valid instances of any topic type.
   The validity is only oriented on the typing of topics, e.g.
   type-instance or supertype-subtype."
  (declare (ignorable param))
  (handler-case (let ((topic-instances 
		         (with-reader-lock
			   (json-tmcl::return-all-tmcl-instances))))
		  (setf (hunchentoot:content-type*) "application/json") ;RFC 4627
		  (json:encode-json-to-string
		   (map 'list #'(lambda(y)
				  (map 'list #'uri y))
			(map 'list #'psis topic-instances))))
    (condition (err) (progn
		       (setf (hunchentoot:return-code*) hunchentoot:+http-internal-server-error+)
		       (setf (hunchentoot:content-type*) "text")
		       (format nil "Condition: \"~a\"" err)))))


(defun return-topic-stub-of-psi(&optional psi)
  "Returns a json string of a topic depending on the
   passed psi as a topic-stub-construct."
  (assert psi)
  (let ((topic (d:get-item-by-psi psi)))
    (if topic
	(let ((topic-json
	       (handler-case (with-reader-lock
			       (json-exporter::to-json-topicStub-string topic))
		 (condition (err) (progn
				    (setf (hunchentoot:return-code*) hunchentoot:+http-internal-server-error+)
				    (setf (hunchentoot:content-type*) "text")
				    (format nil "Condition: \"~a\"" err))))))
	  (setf (hunchentoot:content-type*) "application/json") ;RFC 4627
	  topic-json)
	(progn
	  (setf (hunchentoot:return-code*) hunchentoot:+http-not-found+)
	  (setf (hunchentoot:content-type*) "text")
	  (format nil "Condition: Topic \"~a\" not found" psi)))))


(defun return-tmcl-info-of-psis(treat-as)
  "Returns a json string which represents the defined tmcl-constraints of the
   topic and the associations where this topic can be a player."
  (let ((http-method (hunchentoot:request-method*)))
    (if (or (eq http-method :POST)
	    (eq http-method :PUT))
	(let ((external-format (flexi-streams:make-external-format :UTF-8 :eol-style :LF)))
	  (let ((json-data (hunchentoot:raw-post-data :external-format external-format :force-text t)))
	    (handler-case (let ((psis
				 (json:decode-json-from-string json-data)))			    
			    (let ((tmcl
				   (with-reader-lock
				     (json-tmcl:get-constraints-of-fragment psis :treat-as treat-as))))
			      (if tmcl
				  (progn
				    (setf (hunchentoot:content-type*) "application/json") ;RFC 4627
				    tmcl)
				  (progn
				    (setf (hunchentoot:return-code*) hunchentoot:+http-not-found+)
				    (setf (hunchentoot:content-type*) "text")
				    (format nil "Topic \"~a\" not found." psis)))))
	      (condition (err) (progn
				 (setf (hunchentoot:return-code*) hunchentoot:+http-internal-server-error+)
				 (setf (hunchentoot:content-type*) "text")
				 (format nil "Condition: \"~a\"" err))))))
	(setf (hunchentoot:return-code*) hunchentoot:+http-bad-request+))))


(defun return-all-topic-psis (&optional param)
  "return all psis currently existing in isidorus as a list of list. every topic is a list
   of psis and the entire list contains a list of topics"
  (declare (ignorable param))
  (let ((http-method (hunchentoot:request-method*)))
    (if (eq http-method :GET)
	(progn
	  (setf (hunchentoot:content-type*) "application/json") ;RFC 4627
	  (handler-case (with-reader-lock
			  (get-all-topic-psis))
	    (condition (err) (progn
			       (setf (hunchentoot:return-code*) hunchentoot:+http-internal-server-error+)
			       (setf (hunchentoot:content-type*) "text")
			       (format nil "Condition: \"~a\"" err)))))
	(setf (hunchentoot:return-code*) hunchentoot:+http-bad-request+))))


(defun return-json-fragment(&optional psi)
  "returns the json-fragmen belonging to the psi passed by the parameter psi"
  (assert psi)
  (let ((http-method (hunchentoot:request-method*)))
    (if (eq http-method :GET)
	(let ((identifier (string-replace psi "%23" "#")))
	  (setf (hunchentoot:content-type*) "application/json") ;RFC 4627
	  (let ((fragment
		 (with-reader-lock
		   (get-latest-fragment-of-topic identifier))))
	    (if fragment
		(handler-case (with-reader-lock
				(to-json-string fragment))
		  (condition (err)
		    (progn
		      (setf (hunchentoot:return-code*) hunchentoot:+http-internal-server-error+)
		      (setf (hunchentoot:content-type*) "text")
		      (format nil "Condition: \"~a\"" err))))
		(progn
		  (setf (hunchentoot:return-code*) hunchentoot:+http-not-found+)
		  (setf (hunchentoot:content-type*) "text")
		  (format nil "Topic \"~a\" not found" psi)))))
	(setf (hunchentoot:return-code*) hunchentoot:+http-bad-request+))))


(defun return-json-rdf-fragment(&optional psi)
  "returns the json-fragmen belonging to the psi passed by the parameter psi"
  (assert psi)
  (let ((http-method (hunchentoot:request-method*)))
    (if (eq http-method :GET)
	(let ((identifier (string-replace psi "%23" "#")))
	  (setf (hunchentoot:content-type*) "application/json") ;RFC 4627
	  (let ((fragment
		 (with-reader-lock
		   (get-latest-fragment-of-topic identifier))))
	    (if fragment
		(handler-case (with-reader-lock
				(rdf-exporter:to-rdf-string fragment))
		  (condition (err)
		    (progn
		      (setf (hunchentoot:return-code*) hunchentoot:+http-internal-server-error+)
		      (setf (hunchentoot:content-type*) "text")
		      (format nil "Condition: \"~a\"" err))))
		(progn
		  (setf (hunchentoot:return-code*) hunchentoot:+http-not-found+)
		  (setf (hunchentoot:content-type*) "text")
		  (format nil "Topic \"~a\" not found" psi)))))
	(setf (hunchentoot:return-code*) hunchentoot:+http-bad-request+))))


(defun json-commit(&optional param)
  "calls the json-to-elem method for a json-fragment and imports it to elephant"
  (declare (ignorable param)) ;param is currently not used
  (let ((http-method (hunchentoot:request-method*)))
    (if (or (eq http-method :PUT)
	    (eq http-method :POST))
	(let ((external-format (flexi-streams:make-external-format :UTF-8 :eol-style :LF)))
	  (let ((json-data (hunchentoot:raw-post-data :external-format external-format :force-text t)))
	    (handler-case (with-writer-lock 
			    (json-importer:json-to-elem json-data))
	      (condition (err)
		(progn
		  (setf (hunchentoot:return-code*) hunchentoot:+http-internal-server-error+)
		  (setf (hunchentoot:content-type*) "text")
		  (format nil "Condition: \"~a\"" err))))))
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
    (handler-case (with-reader-lock
		    (let ((topics 
			   (remove-if
			    #'null
			    (map 'list #'(lambda(top)
					   (when (d:find-item-by-revision top 0)
					     top))
				 (elephant:get-instances-by-class 'd:TopicC)))))
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
			    (json-exporter:make-topic-summary topics-in-range))))))
      (condition (err) (progn
			 (setf (hunchentoot:return-code*) hunchentoot:+http-internal-server-error+)
			 (setf (hunchentoot:content-type*) "text")
			 (format nil "Condition: \"~a\"" err))))))


(defun return-overview (&optional param)
  "Returns a json-object representing a topic map overview as a tree(s)"
  (declare (ignorable param))
  (with-reader-lock
    (handler-case (let ((json-string
			 (json-tmcl::tree-view-to-json-string (json-tmcl::make-tree-view))))
		    (setf (hunchentoot:content-type*) "application/json") ;RFC 4627
		    json-string)
      (Condition (err) (progn
			 (setf (hunchentoot:return-code*) hunchentoot:+http-internal-server-error+)
			 (setf (hunchentoot:content-type*) "text")
			 (format nil "Condition: \"~a\"" err))))))


(defun mark-as-deleted-handler (&optional param)
  "Marks the corresponding elem as deleted.
   {\"type\":<\"'TopicC\" | \"'OccurrenceC\" | \"'NameC\"
              \"'AssociationC\" | \"'RoleC\" | \"VariantC\" >,
    \"object\":<specified json-object: name or occurrence,
                if the deleted object is a topic this field
                has to be set to null>,
    \"parent-topic\":<psis or null>,
    \"parent-name\": <specified json-object: name>}."
  (declare (ignorable param)) ;param is currently not used
  (let ((http-method (hunchentoot:request-method*)))
    (if (or (eq http-method :PUT)
	    (eq http-method :POST))
	(let ((external-format (flexi-streams:make-external-format :UTF-8 :eol-style :LF)))
	  (let ((json-data (hunchentoot:raw-post-data :external-format external-format :force-text t)))
	    (handler-case
		(with-writer-lock
		  (json-tmcl::mark-as-deleted-from-json json-data))
	      (condition (err)
		(progn
		  (setf (hunchentoot:return-code*) hunchentoot:+http-internal-server-error+)
		  (setf (hunchentoot:content-type*) "text")
		  (format nil "Condition: \"~a\"" err))))))
	(setf (hunchentoot:return-code*) hunchentoot:+http-bad-request+))))


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


(defun string-replace (str search-str replace-str)
  "replaces all sub-strings in str of the form search-str with
   the string replace-str and returns the new generated string"
  (if (= (length search-str) 0)
      str
      (progn
	(let ((ret-str "")
	      (idx 0))
	  (loop
	     (if (string= str search-str
			  :start1 idx
			  :end1 (min (length str)
				     (+ idx (length search-str))))
		 (progn
		   (setf ret-str (concatenate 'string ret-str replace-str))
		   (incf idx (length search-str)))
		 (progn
		   (setf ret-str (concatenate 'string ret-str (subseq str idx (1+ idx))))
		   (incf idx)))
	     (unless (< idx (length str))
	       (return ret-str)))))))