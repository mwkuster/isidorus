;;+-----------------------------------------------------------------------------
;;+  Isidorus
;;+  (c) 2008-2010 Marc Kuester, Christoph Ludwig, Lukas Georgieff
;;+
;;+  Isidorus is freely distributable under the LLGPL license.
;;+  You can find a detailed description in trunk/docs/LLGPL-LICENSE.txt and
;;+  trunk/docs/LGPL-LICENSE.txt.
;;+-----------------------------------------------------------------------------

(in-package :rest-interface)

;caching tables
(defparameter *type-table* nil "Cointains integer==OIDs that represent a topic
                                instance of a vylid type-topic")

(defparameter *instance-table* nil "Contains integer==OIDs that represent a topic
                                    instance of a valid instance-topic")

(defparameter *overview-table* nil "Is of the following structure
                                    ((:topic <oid> :psis (<oid> <oid> <...>)) (...))
                                    that represents a list of topics and their
                                    valid psi object id's")

(defparameter *use-overview-cache* t "if this boolean vaue is set to t, the rest
                                      interface uses the *verview-table*-list to
                                      cache topics and their psis.")

(defparameter *use-http-authentication* 0 "if this variable is set to > 0, the
                                           host page will require basic
                                           authentication. If it's value is set
                                           to > 1, all json-commit handlers will require
                                           basic-authentication. If this value is set to
                                           > 2 all json-handlers require authentication.
                                           If this value is set to 0, no authentication
                                           is required.")


(defparameter *cache-initialised* nil "determines wheter the cache has been
                                       already set or not")

(defparameter *fragments-initialised* nil "determines wheter the fragments has
                                           been already initialised or not.")

;the prefix to get a fragment by the psi -> localhost:8000/json/get/<fragment-psi>
(defparameter *json-get-prefix* "/json/get/(.+)$")
;the prefix to get a fragment by the psi -> localhost:8000/json/rdf/get/<fragment-psi>
(defparameter *get-rdf-prefix* "/json/get/rdf/(.+)$")
;the url to commit a json fragment by "put" or "post"
(defparameter *json-commit-url* "/json/commit/?$")
;the url to commit a TM-fragment in XTM 2.0 format, the regular
;expression represents the topic map id
(defparameter *xtm-commit-prefix* "/import/xtm/2.0/(.+)$")
;the url to get all topic psis of isidorus -> localhost:8000/json/psis
(defparameter *json-get-all-psis* "/json/psis/?$")
;the url to get a summary of all topic stored in isidorus; you have to set the
;GET-parameter "start" for the start index of all topics within elephant and the
;GET-paramter "end" for the last index of the topic sequence
; -> http://localhost:8000/json/summary/?start=12&end=13
(defparameter *json-get-summary-url* "/json/summary/?$")
;returns a list of all psis that can be a type
(defparameter *json-get-all-type-psis* "/json/tmcl/types/?$")
;returns a list of all psis that belongs to a valid topic-instance
(defparameter *json-get-all-instance-psis* "/json/tmcl/instances/?$")
;the json prefix for getting some topic stub information of a topic
(defparameter *json-get-topic-stub-prefix* "/json/topicstubs/(.+)$")
;the json url for getting some tmcl information of a topic treated as a type
(defparameter *json-get-type-tmcl-url* "/json/tmcl/type/?$")
;the json url for getting some tmcl information of a topic treated as an instance
(defparameter *json-get-instance-tmcl-url* "/json/tmcl/instance/?$")
;returns a json-object representing a tree view
(defparameter *json-get-overview* "/json/tmcl/overview/?$")
;the url to the user interface
(defparameter *ajax-user-interface-url* "/isidorus")
;the url to the css files of the user interface
(defparameter *ajax-user-interface-css-prefix* "/css")
;the directory contains the css files
(defparameter *ajax-user-interface-css-directory-path* "ajax/css")
;the file path to the HTML file implements the user interface
(defparameter *ajax-user-interface-file-path* "ajax/isidorus.html")
;the directory which contains all necessary javascript files
(defparameter *ajax-javascript-directory-path* "ajax/javascripts")
;the url prefix of all javascript files
(defparameter *ajax-javascript-url-prefix* "/javascripts")
;the url suffix that calls the mark-as-deleted handler
(defparameter *mark-as-deleted-url* "/mark-as-deleted")
;the get url to request the latest revision of the storage
(defparameter *latest-revision-url* "/json/latest-revision/?$")
;the ulr to invoke a SPARQL query
(defparameter *sparql-url* "/json/tm-sparql/?$")


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
			      (mark-as-deleted-url *mark-as-deleted-url*)
			      (latest-revision-url *latest-revision-url*)
			      (xtm-commit-prefix *xtm-commit-prefix*)
			      (sparql-url *sparql-url*))
  "registers the json im/exporter to the passed base-url in hunchentoot's dispatch-table
   and also registers a file-hanlder to the html-user-interface"

  ;initializes cache and fragments
  (init-cache nil)
  (format t "~%")
  (init-fragments nil)

  ;; registers the http-code 500 for an internal server error to the standard
  ;; return codes. so there won't be attached a hunchentoot default message,
  ;; this is necessary to be able to send error messages in an individual way/syntax
  ;; e.g. a json error-message.
  (push hunchentoot:+http-internal-server-error+ hunchentoot:*approved-return-codes*)
  ;; === html and css files ====================================================
  (if (> *use-http-authentication* 0)
      (define-easy-handler (isidorus-ui :uri ajax-user-interface-url
					:default-request-type :get)
	  ()
	(with-http-authentication
	    (serve-file ajax-user-interface-file-path "text/html")))
      (push
       (create-static-file-dispatcher-and-handler
	ajax-user-interface-url ajax-user-interface-file-path "text/html")
       hunchentoot:*dispatch-table*))

  (let ((files-and-urls
	 (make-file-path-and-url ajax-user-interface-css-directory-path
				 ajax-user-interface-css-prefix)))
    (dotimes (idx (length files-and-urls))
      (let ((script-path (getf (elt files-and-urls idx) :path))
	    (script-url (getf (elt files-and-urls idx) :url)))
	(push
	 (create-static-file-dispatcher-and-handler script-url script-path)
	 hunchentoot:*dispatch-table*))))


  ;; === ajax frameworks and javascript files ==================================
  (let ((files-and-urls (make-file-path-and-url ajax-javascripts-directory-path
						ajax-javascripts-url-prefix)))
    (dotimes (idx (length files-and-urls))
      (let ((script-path (getf (elt files-and-urls idx) :path))
	    (script-url (getf (elt files-and-urls idx) :url)))
	(push
	 (create-static-file-dispatcher-and-handler script-url script-path)
	 hunchentoot:*dispatch-table*))))  

  ;; === rest interface ========================================================
  (push
   (if *use-overview-cache*
       (create-regex-dispatcher json-get-all-psis
				(if (> *use-http-authentication* 1)
				    (lambda(&optional param)
				      (with-http-authentication
					  (cached-return-all-topic-psis param)))
				    #'cached-return-all-topic-psis))
       (create-regex-dispatcher json-get-all-psis
				(if (> *use-http-authentication* 1)
				    (lambda(&optional param)
				      (with-http-authentication
					  (return-all-topic-psis param)))
				    #'return-all-topic-psis)))
   hunchentoot:*dispatch-table*)
  (push
   (create-regex-dispatcher json-get-prefix
			    (if (> *use-http-authentication* 2)
				(lambda(&optional psi)
				  (with-http-authentication
				      (return-json-fragment psi)))
				#'return-json-fragment))
   hunchentoot:*dispatch-table*)
  (push
   (create-regex-dispatcher get-rdf-prefix
			    (if (> *use-http-authentication* 2)
				(lambda(&optional psi)
				  (with-http-authentication
				      (return-json-rdf-fragment psi)))
				#'return-json-rdf-fragment))
   hunchentoot:*dispatch-table*)
  (push
   (create-regex-dispatcher json-get-topic-stub-prefix
			    (if (> *use-http-authentication* 2)
				(lambda(&optional psi)
				  (with-http-authentication
				      (return-topic-stub-of-psi psi)))
				#'return-topic-stub-of-psi))
   hunchentoot:*dispatch-table*)
  (push
   (create-regex-dispatcher json-get-all-type-psis
			    (if (> *use-http-authentication* 2)
				(lambda(&optional param)
				  (with-http-authentication
				      (return-all-tmcl-types param)))
				#'return-all-tmcl-types))
   hunchentoot:*dispatch-table*)
  (push
   (create-regex-dispatcher json-get-all-instance-psis
			    (if (> *use-http-authentication* 2)
				(lambda(&optional param)
				  (with-http-authentication
				      (return-all-tmcl-instances param)))
				#'return-all-tmcl-instances))
   hunchentoot:*dispatch-table*)
  (push
   (create-regex-dispatcher json-get-type-tmcl-url
			    (if (> *use-http-authentication* 2)
				(lambda(&optional param)
				  (declare (ignorable param))
				  (with-http-authentication
				      (return-tmcl-info-of-psis 'json-tmcl::type)))
				(lambda(&optional param)
				  (declare (ignorable param))
				  (return-tmcl-info-of-psis 'json-tmcl::type))))
   hunchentoot:*dispatch-table*)
  (push
   (create-regex-dispatcher json-get-instance-tmcl-url
			    (if (> *use-http-authentication* 2)
				(lambda(&optional param)
				  (declare (ignorable param))
				  (with-http-authentication
				      (return-tmcl-info-of-psis 'json-tmcl::instance)))
				(lambda(&optional param)
				  (declare (ignorable param))
				  (return-tmcl-info-of-psis 'json-tmcl::instance))))
   hunchentoot:*dispatch-table*)
  (push
   (create-regex-dispatcher json-get-overview
			    (if (> *use-http-authentication* 2)
				(lambda(&optional param)
				  (with-http-authentication
				      (return-overview param)))
				#'return-overview))
   hunchentoot:*dispatch-table*)
  (push
   (create-regex-dispatcher json-commit-url
			    (if (> *use-http-authentication* 1)
				(lambda(&optional param)
				  (with-http-authentication
				      (json-commit param)))
				#'json-commit))
   hunchentoot:*dispatch-table*)
  (push
   (create-regex-dispatcher json-get-summary-url
			    (if (> *use-http-authentication* 2)
				(lambda(&optional param)
				  (with-http-authentication
				      (return-topic-summaries param)))
				#'return-topic-summaries))
   hunchentoot:*dispatch-table*)
  (push
   (create-regex-dispatcher mark-as-deleted-url
			    (if (> *use-http-authentication* 1)
				(lambda(&optional param)
				  (with-http-authentication
				      (mark-as-deleted-handler param)))
				#'mark-as-deleted-handler))
   hunchentoot:*dispatch-table*)
  (push
   (create-regex-dispatcher xtm-commit-prefix
			    (if (> *use-http-authentication* 1)
				(lambda(&optional tm-id)
				  (with-http-authentication
				      (xtm-import-handler tm-id)))
				#'xtm-import-handler))
   hunchentoot:*dispatch-table*)
  (push
   (create-regex-dispatcher latest-revision-url
			    (if (> *use-http-authentication* 2)
				(lambda(&optional param)
				  (declare (ignorable param))
				  (with-http-authentication
				      (return-latest-revision)))
				#'return-latest-revision))
   hunchentoot:*dispatch-table*)
  (push
   (create-regex-dispatcher sparql-url
			    (if (> *use-http-authentication* 1)
				(lambda(&optional param)
				  (with-http-authentication
				      (return-tm-sparql param)))
				#'return-tm-sparql))
   hunchentoot:*dispatch-table*))

;; =============================================================================
;; --- some handlers for the json-rest-interface -------------------------------
;; =============================================================================
(defun return-all-tmcl-types(&optional param)
  "Returns all topic-psi that are valid types -> so they have to be valid to the
   topictype-constraint (if it exists) and the can't be abstract."
  (declare (ignorable param))
  (handler-case (with-reader-lock
		  (let ((topic-types
			 (map 'list #'(lambda (oid)
					(elephant::controller-recreate-instance
					 elephant::*store-controller* oid))
			      *type-table*)))
		    (setf (hunchentoot:content-type*) "application/json") ;RFC 4627
		    (json:encode-json-to-string
		     (map 'list #'(lambda(y)
				    (map 'list #'uri y))
			  (map 'list #'psis topic-types)))))
    (condition (err) (progn
		       (setf (hunchentoot:return-code*) hunchentoot:+http-internal-server-error+)
		       (setf (hunchentoot:content-type*) "text")
		       (format nil "Condition: \"~a\"" err)))))


(defun return-all-tmcl-instances(&optional param)
  "Returns all topic-psis that are valid instances of any topic type.
   The validity is only oriented on the typing of topics, e.g.
   type-instance or supertype-subtype."
  (declare (ignorable param))
  (handler-case (with-reader-lock
		  (let ((topic-instances 
			 (map 'list #'(lambda (oid)
					(elephant::controller-recreate-instance
					 elephant::*store-controller* oid))
			      *instance-table*)))
		    (setf (hunchentoot:content-type*) "application/json") ;RFC 4627
		    (json:encode-json-to-string
		     (map 'list #'(lambda(y)
				    (map 'list #'uri y))
			  (map 'list #'psis topic-instances)))))
    (condition (err) (progn
		       (setf (hunchentoot:return-code*) hunchentoot:+http-internal-server-error+)
		       (setf (hunchentoot:content-type*) "text")
		       (format nil "Condition: \"~a\"" err)))))


(defun return-topic-stub-of-psi(&optional psi)
  "Returns a json string of a topic depending on the
   passed psi as a topic-stub-construct."
  (assert psi)
  (with-reader-lock
    (let ((topic (d:get-item-by-psi psi)))
      (if topic
	  (handler-case
	      (progn (setf (hunchentoot:content-type*) "application/json") ;RFC 4627
		     (json-exporter::to-json-topicStub-string topic :revision 0))
	    (condition (err)
	      (progn
		(setf (hunchentoot:return-code*)
		      hunchentoot:+http-internal-server-error+)
		(setf (hunchentoot:content-type*) "text")
		(format nil "Condition: \"~a\"" err))))
	  (progn
	    (setf (hunchentoot:return-code*) hunchentoot:+http-not-found+)
	    (setf (hunchentoot:content-type*) "text")
	    (format nil "Condition: Topic \"~a\" not found" psi))))))


(defun return-tmcl-info-of-psis(treat-as)
  "Returns a json string which represents the defined tmcl-constraints of the
   topic and the associations where this topic can be a player."
  (let ((http-method (hunchentoot:request-method*)))
    (if (or (eq http-method :POST)
	    (eq http-method :PUT))
	(let ((external-format
	       (flexi-streams:make-external-format :UTF-8 :eol-style :LF)))
	  (let ((json-data
		 (hunchentoot:raw-post-data :external-format external-format
					    :force-text t)))
	    (handler-case
		(with-reader-lock
		  (let ((psis (json:decode-json-from-string json-data)))
		    (let ((tmcl (json-tmcl:get-constraints-of-fragment
				 psis :treat-as treat-as :revision 0)))
		      (if tmcl
			  (progn
			    (setf (hunchentoot:content-type*)
				  "application/json") ;RFC 4627
			    tmcl)
			  (progn
			    (setf (hunchentoot:return-code*)
				  hunchentoot:+http-not-found+)
			    (setf (hunchentoot:content-type*) "text")
			    (format nil "Topic \"~a\" not found." psis))))))
	      (condition ()
		(setf (hunchentoot:content-type*) "application/json") ;RFC 4627
		"{\"topicConstraints\":{\"exclusiveInstances\":null,\"subjectIdentifierConstraints\":null,\"subjectLocatorConstraints\":null,\"topicNameConstraints\":null,\"topicOccurrenceConstraints\":null,\"abstractConstraint\":false},\"associationsConstraints\":null}"))))
	(setf (hunchentoot:return-code*) hunchentoot:+http-bad-request+))))


(defun return-all-topic-psis (&optional param)
  "return all psis currently existing in isidorus as a list of list. every topic is a list
   of psis and the entire list contains a list of topics"
  (declare (ignorable param))
  (let ((http-method (hunchentoot:request-method*)))
    (if (eq http-method :GET)
	(progn
	  (setf (hunchentoot:content-type*) "application/json") ;RFC 4627
	  (handler-case
	      (with-reader-lock
		(get-all-topic-psis :revision 0))
	    (condition (err) (progn
			       (setf (hunchentoot:return-code*)
				     hunchentoot:+http-internal-server-error+)
			       (setf (hunchentoot:content-type*) "text")
			       (format nil "Condition: \"~a\"" err)))))
	(setf (hunchentoot:return-code*) hunchentoot:+http-bad-request+))))


(defun cached-return-all-topic-psis (&optional param)
  "return all psis currently existing in isidorus as a list of list. every topic is a list
   of psis and the entire list contains a list of topics"
  (declare (ignorable param))
  (let ((http-method (hunchentoot:request-method*)))
    (if (eq http-method :GET)
        (progn
          (setf (hunchentoot:content-type*) "application/json") ;RFC 4627
          ;(handler-case
          (with-reader-lock
	    (let* ((psi-instances
		    (map 'list
			 (lambda(item)
			   (let ((psi-strs (getf item :psis)))
			     (map 'list
				  (lambda(psi-oid)
				    (d:uri (elephant::controller-recreate-instance
					    elephant:*store-controller* psi-oid)))
				  psi-strs)))
			 *overview-table*))
		   (result (json:encode-json-to-string psi-instances)))
	      result))
          ;(condition (err) (progn
          ;(setf (hunchentoot:return-code*)
          ;hunchentoot:+http-internal-server-error+)
          ;(setf (hunchentoot:content-type*) "text")
          ;(format nil "Condition: \"~a\"" err)))))
          )
        (setf (hunchentoot:return-code*) hunchentoot:+http-bad-request+))))



(defun return-json-fragment(&optional psi)
  "returns the json-fragmen belonging to the psi passed by the parameter psi.
   If the topic is marked as deleted the corresponding fragment is treated
   as non-existent and an HTTP 404 is set."
  (assert psi)
  (let ((http-method (hunchentoot:request-method*)))
    (if (eq http-method :GET)
        (let ((identifier (string-replace psi "%23" "#")))
          (setf (hunchentoot:content-type*) "application/json") ;RFC 4627
          (with-reader-lock
	      ;(handler-case
	      (let* ((fragment (get-latest-fragment-of-topic identifier))
		     (top (when fragment (topic fragment)))
		     (serializer (fragment-serializer))
		     (result (when top (d:serialize-fragment fragment serializer))))
		(if result
		    result
		    (progn
		      (setf (hunchentoot:return-code*) hunchentoot:+http-not-found+)
		      (setf (hunchentoot:content-type*) "text")
		      (format nil "Topic \"~a\" not found" psi))))
	    ;(condition (err)
	    ;(progn
	    ;(setf (hunchentoot:return-code*)
	    ;hunchentoot:+http-internal-server-error+)
	    ;(setf (hunchentoot:content-type*) "text")
	    ;(format nil "Condition: \"~a\"" err))))))
	    ))
        (setf (hunchentoot:return-code*) hunchentoot:+http-bad-request+))))


(defun return-json-rdf-fragment(&optional psi)
  "returns the json-fragmen belonging to the psi passed by the parameter psi"
  (assert psi)
  (let ((http-method (hunchentoot:request-method*)))
    (if (eq http-method :GET)
	(let ((identifier (string-replace psi "%23" "#")))
	  (setf (hunchentoot:content-type*) "application/json") ;RFC 4627
	  (with-reader-lock
	    (let ((fragment (get-latest-fragment-of-topic identifier)))
	      (if (and fragment (find-item-by-revision (topic fragment) 0))
		  (handler-case
		      (rdf-exporter:to-rdf-string fragment)
		    (condition (err)
		      (progn
			(setf (hunchentoot:return-code*)
			      hunchentoot:+http-internal-server-error+)
			(setf (hunchentoot:content-type*) "text")
			(format nil "Condition: \"~a\"" err))))
		  (progn
		    (setf (hunchentoot:return-code*) hunchentoot:+http-not-found+)
		    (setf (hunchentoot:content-type*) "text")
		    (format nil "Topic \"~a\" not found" psi))))))
	(setf (hunchentoot:return-code*) hunchentoot:+http-bad-request+))))


(defun json-commit(&optional param)
  "calls the import-from-isidorus-json method for a json-fragment and
   imports it to elephant"
  (declare (ignorable param)) ;param is currently not used
  (let ((http-method (hunchentoot:request-method*)))
    (if (or (eq http-method :PUT)
	    (eq http-method :POST))
	(let ((external-format
	       (flexi-streams:make-external-format :UTF-8 :eol-style :LF)))
	  (let ((json-data
		 (hunchentoot:raw-post-data :external-format external-format
					    :force-text t)))
	    (with-writer-lock
	      (handler-case
		  (let ((result (json-importer:import-from-isidorus-json json-data)))
		    (when (getf result :fragment)
		      (update-fragments-after-commit
		       (getf result :fragment)
		       (getf result :foreign-associations))
		      (push-to-cache (d:topic (getf result :fragment)))
		      (update-list (d:topic (getf result :fragment))
				   (d:psis (d:topic (getf result :fragment))
					   :revision 0))))
		(condition (err)
		  (progn
		    (setf (hunchentoot:return-code*)
			  hunchentoot:+http-internal-server-error+)
		    (setf (hunchentoot:content-type*) "text")
		    (format nil "Condition: \"~a\"" err)))))))
	(setf (hunchentoot:return-code*) hunchentoot:+http-bad-request+))))


(defun update-fragments-after-commit (new-fragment foreign-associations)
  "Deleted all old fragment that belongs to the topic that is bound to
   the passed new-fragment. Deletes and creates a new fragment of all
   players of any association of the passed list foreign-associations."
  (declare (FragmentC new-fragment)
	   (List foreign-associations))
  (map 'list #'elephant:drop-instance
       (delete new-fragment
	       (elephant:get-instances-by-value
		'd:FragmentC 'd::topic (d:topic new-fragment))))
  (let* ((rev (d:revision new-fragment))
	 (frg-top (d:topic new-fragment))
	 (frg-assocs
	  (delete-if #'null (map 'list (lambda(role)
					 (d:parent role :revision rev))
				 (d:player-in-roles frg-top :revision rev))))
	 (tops
	  (append 
	   (loop for assoc in foreign-associations
	      append (loop for role in (d:roles assoc :revision rev)
			collect (d:player role :revision rev)))
	   (delete frg-top
		   (loop for assoc in frg-assocs
		      append (loop for role in (d:roles assoc :revision rev)
				collect (d:player role :revision rev)))))))
    (map 'list (lambda(top)
		 (map 'list #'elephant:drop-instance
		      (elephant:get-instances-by-value
		       'd:FragmentC 'd::topic top))
		 (serialize-fragment (d:create-latest-fragment-of-topic top)
				     (fragment-serializer)))
	 (delete-duplicates (delete-if #'null tops)))))


(defun return-topic-summaries(&optional param)
  "returns a summary of the requested topics"
  (declare (ignorable param))
  (let ((start-idx 
	 (handler-case (parse-integer (hunchentoot:get-parameter "start"))
	   (condition () 0)))
	(end-idx
	 (handler-case (parse-integer (hunchentoot:get-parameter "end"))
	   (condition () nil))))
    (with-reader-lock
      (handler-case
	  (let ((topics
		 (remove-null
		  (map 'list
		       #'(lambda(top)
			   (when (find-item-by-revision top 0)
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
		  (json-exporter:make-topic-summary topics-in-range)))))
	(condition (err) (progn
			   (setf (hunchentoot:return-code*)
				 hunchentoot:+http-internal-server-error+)
			   (setf (hunchentoot:content-type*) "text")
			   (format nil "Condition: \"~a\"" err)))))))


(defun return-overview (&optional param)
  "Returns a json-object representing a topic map overview as a tree(s)"
  (declare (ignorable param))
  (with-reader-lock
    (handler-case
	(let ((json-string
	       (json-tmcl::tree-view-to-json-string
		(json-tmcl::make-tree-view :revision 0))))
	  (setf (hunchentoot:content-type*) "application/json") ;RFC 4627
	  json-string)
      (Condition (err)
	(progn
	  (setf (hunchentoot:return-code*) hunchentoot:+http-internal-server-error+)
	  (setf (hunchentoot:content-type*) "text")
	  (format nil "Condition: \"~a\"" err))))))


(defun mark-as-deleted-handler (&optional param)
  "Marks the corresponding elem as deleted."
  (declare (ignorable param)) ;param is currently not used
  (let ((http-method (hunchentoot:request-method*)))
    (if (or (eq http-method :DELETE)
	    (eq http-method :POST)) ;not nice - but the current ui-library can't send http-delete messages
	(let ((external-format
	       (flexi-streams:make-external-format :UTF-8 :eol-style :LF)))
	  (let ((json-data
		 (hunchentoot:raw-post-data :external-format external-format
					    :force-text t)))
	    (with-writer-lock
	      (handler-case
		  (let* ((rev (d:get-revision))
			 (result (json-delete-interface:mark-as-deleted-from-json
				  json-data :revision rev)))
		    (if result
			(progn
			  (cond ((typep result 'd:TopicC)
				 (setf *type-table*
				       (delete (elephant::oid result) *type-table*))
				 (setf *instance-table*
				       (delete (elephant::oid result) *instance-table*))
				 (remove-topic-from-list result)
				 (map nil (lambda(fragment)
					    (when (eql (d:topic fragment) result)
					      (elephant:drop-instance fragment)))
				      (elephant:get-instances-by-value
				       'd:FragmentC 'd:topic result))
				 (update-fragments-after-delete result rev))
				((typep result 'd:AssociationC)
				 (let ((players
					(delete-if
					 #'null
					 (map 'list
					      (lambda(role)
						(let ((top (player role
								   :revision (1- rev))))
						  (when (psis top :revision 0)
						    top)))
					      (roles result :revision (1- rev))))))
				   (map nil
					(lambda(plr)
					  (map nil #'elephant:drop-instance 
					       (elephant:get-instances-by-value
						'd:FragmentC 'd:topic plr))
					  (d:serialize-fragment
					   (create-latest-fragment-of-topic plr)
					   (fragment-serializer)))
					players)))
				((or (typep result 'd:NameC)
				     (typep result 'd:OccurrenceC))
				 (let ((top (parent result :revision (1- rev))))
				   (when (and top (psis top :revision 0))
				     (map nil (lambda(frg)
						(setf (slot-value frg 'd::serializer-cache) nil)
						(d:serialize-fragment
						 (get-latest-fragment-of-topic top)
						 (fragment-serializer)))
					  (elephant:get-instances-by-value
					   'd:FragmentC 'd:topic top))))))
			  (format nil "")) ;operation succeeded
			(progn
			  (setf (hunchentoot:return-code*) hunchentoot:+http-not-found+)
			  (format nil "object not found"))))
		(condition (err)
		  (progn
		    (setf (hunchentoot:return-code*)
			  hunchentoot:+http-internal-server-error+)
		    (setf (hunchentoot:content-type*) "text")
		    (format nil "Condition: \"~a\"" err)))))))
	(setf (hunchentoot:return-code*) hunchentoot:+http-bad-request+))))


(defun update-fragments-after-delete(deleted-topic delete-revision)
  "Updates all fragments of topics that directly and indireclty
   related to the delete-topic."
  (declare (TopicC deleted-topic)
	   (Integer delete-revision))
  (let* ((rev (1- delete-revision))
	 (all-tops
	  (append
	   (let ((assocs
		  (map 'list (lambda(role)
			       (d:parent role :revision rev))
		       (d:player-in-roles deleted-topic :revision rev))))
	     (loop for assoc in assocs
		append (loop for role in (roles assoc :revision rev)
			  collect (d:player role :revision rev))))
	   (let ((items
		  (append (used-as-theme deleted-topic :revision rev)
			  (used-as-type deleted-topic :revision rev))))
	     (loop for item in items
		when (or (typep item 'NameC) (typep item 'OccurrenceC))
		collect (parent item :revision rev)
		when (or (typep item 'RoleC) (typep item 'AssociationC))
		append (let ((inst (if (typep item 'AssociationC)
				       item
				       (d:parent item :revision rev))))
			 (loop for role in (roles inst :revision rev)
			    collect (d:player role :revision rev)))))))
	 (fragments
	  (delete-if
	   #'null
	   (map 'list (lambda(top)
			(let ((all-frgs
			       (sort 
				(elephant:get-instances-by-value
				 'd:FragmentC 'd::topic top)
				#'> :key 'revision)))
			  (let ((frg (first all-frgs)))
			    (map nil 'elephant:drop-instance (rest all-frgs))
			    frg)))
		(delete-duplicates
		 (delete deleted-topic
			 (delete-if #'null all-tops)))))))
    (map nil (lambda(frg)
	       (setf (slot-value frg 'd::serializer-cache) nil)
	       (d:serialize-fragment frg (fragment-serializer)))
	 fragments)))
			       


(defun return-latest-revision ()
  "Returns an integer that represents the latest revision that
   is used in the storage."
  (handler-case
      (if (eql (hunchentoot:request-method*) :GET)
	    (let ((sorted-revisions
		   (with-reader-lock (sort (d:get-all-revisions) #'>))))
	      (when sorted-revisions
		(setf (hunchentoot:content-type*) "application/json") ;RFC 4627
		(format nil "~a" (first sorted-revisions))))
	    (setf (hunchentoot:return-code*) hunchentoot:+http-bad-request+))
    (condition (err)
      (progn
	(setf (hunchentoot:return-code*) hunchentoot:+http-internal-server-error+)
	(setf (hunchentoot:content-type*) "text")
	(format nil "Condition: \"~a\"" err)))))


(defun xtm-import-handler (&optional tm-id)
  "Imports the received data as XTM 2.0 topic map."
  (assert tm-id)
  (handler-case
      (if (eql (hunchentoot:request-method*) :POST)
	  (let ((external-format (flexi-streams:make-external-format
				  :UTF-8 :eol-style :LF)))
	    (let ((xml-data (hunchentoot:raw-post-data
			     :external-format external-format
			     :force-text t)))
	      (let ((xml-dom
		     (dom:document-element
		      (cxml:parse xml-data (cxml-dom:make-dom-builder)))))
		(xtm-importer:importer xml-dom :tm-id tm-id
				       :xtm-id (xtm-importer::get-uuid))
		(with-writer-lock
		  (init-cache)
		  (init-fragments))
		(format nil ""))))
	  (setf (hunchentoot:return-code*) hunchentoot:+http-bad-request+))
    (condition (err)
      (progn
	(setf (hunchentoot:return-code*) hunchentoot:+http-internal-server-error+)
	(setf (hunchentoot:content-type*) "text")
	(format nil "Condition: \"~a\"" err)))))


(defun return-tm-sparql (&optional param)
  "Returns a JSON object representing a SPARQL response."
  (declare (Ignorable param))
  (handler-case
      (if (eql (hunchentoot:request-method*) :POST)
	  (let ((external-format (flexi-streams:make-external-format
				  :UTF-8 :eol-style :LF)))
	    (let ((sparql-request (hunchentoot:raw-post-data
				   :external-format external-format
				   :force-text t)))
	      (export-construct-as-isidorus-json-string
	       (make-instance 'SPARQL-Query :query sparql-request
			      :revision 0))))
	  (setf (hunchentoot:return-code*) hunchentoot:+http-bad-request+))
    (condition (err)
      (progn
	(setf (hunchentoot:return-code*) hunchentoot:+http-internal-server-error+)
	(setf (hunchentoot:content-type*) "text")
	(if (typep err 'SPARQL-Parser-Error)
	    (format nil "SPARQL-Parser-Error: \"~a\"" (exceptions::message err))
	    (format nil "Condition: \"~a\"" err))))))

;; =============================================================================
;; --- some helper functions ---------------------------------------------------
;; =============================================================================
(defun make-file-path-and-url (path-to-files-directory url-prefix)
  "returns a list of lists which contains an absolute file path and a file-url
   concatenated of the url-prefix and the relative path of all all files in the
   passed directory and its subdirectories"
  (let ((start-position-of-relative-path
	 (- (length (write-to-string (com.gigamonkeys.pathnames:file-exists-p
				      path-to-files-directory))) 2)))
    (let ((files-and-urls nil))
      (com.gigamonkeys.pathnames:walk-directory
       path-to-files-directory
       #'(lambda(current-path)
	   (let ((current-path-string
		  (write-to-string current-path)))
	     (let ((last-position-of-current-path
		    (- (length current-path-string) 1)))
	       (let ((current-url
		      (concat
		       url-prefix
		       (subseq current-path-string start-position-of-relative-path
			       last-position-of-current-path))))
		 (push (list :path current-path :url current-url) files-and-urls))))))
      files-and-urls)))


(defun init-cache(force-init)
  "Initializes the type and instance cache-tables with all valid types/instances"
  (with-writer-lock
    (setf *type-table* nil)
    (setf *instance-table* nil)
    (setf *overview-table* nil)
    (let ((topictype (get-item-by-psi json-tmcl-constants::*topictype-psi*
				      :revision 0))
	  (topictype-constraint (json-tmcl::is-type-constrained :revision 0)))
      (format t "~%initializing cache: ")
      (map 'list #'(lambda(top)
		     (format t ".")
		     (push-to-cache top topictype topictype-constraint))
	   (elephant:get-instances-by-class 'TopicC)))
    (when *use-overview-cache*
      (setf *overview-table*
	    (remove-null
	     (map 'list (lambda(top)
			  (when (find-item-by-revision top 0)
			    (list :topic (elephant::oid top)
				  :psis (map 'list #'elephant::oid
					     (psis top :revision 0)))))
		  (elephant:get-instances-by-class 'TopicC)))))))


(defun push-to-cache (topic-instance &optional
		      (topictype
		       (get-item-by-psi
			json-tmcl::*topictype-psi* :revision 0))
		      (topictype-constraint
		       (json-tmcl::is-type-constrained :revision 0)))
  "Pushes the given topic-instance into the correspondng cache-tables"
  (when (not (json-tmcl::abstract-p topic-instance :revision 0))
    (handler-case (progn
		    (json-tmcl::topictype-p
		     topic-instance topictype topictype-constraint nil 0)
		    (pushnew (elephant::oid topic-instance) *type-table*))
      (condition () nil)))
  (handler-case (progn
		  (json-tmcl::valid-instance-p topic-instance nil nil 0)
		  (pushnew (elephant::oid topic-instance) *instance-table*))
    (condition () nil)))


(defun init-fragments (force-init)
  "Creates fragments of all topics that have a PSI."
  (format t "creating fragments: ")
  (map
   nil
   (lambda(top)
     (let ((psis-of-top (psis top)))
       (when psis-of-top
	 (format t ".")
	 (let ((fragment
		(create-latest-fragment-of-topic
		 (uri (first psis-of-top)))))
	   (d:serialize-fragment fragment (fragment-serializer))
	   fragment))))
	 (elephant:get-instances-by-class 'd:TopicC)))


(defun fragment-serializer ()
  (lambda(frg)
    (json-exporter:export-construct-as-isidorus-json-string
     frg :revision 0)))


(defun update-list (top psis)
  "Sets the psi list that is bound to the topic top to the passed
   psi list."
  (declare (TopicC top)
	   (List psis))
  (let ((top-oid (elephant::oid top)))
    (let ((node
	   (find-if (lambda(item)
		      (= (getf item :topic) top-oid))
		    *overview-table*))
	  (psi-oids (map 'list #'elephant::oid psis)))
      (if node
	  (setf (getf node :psis) psi-oids)
	  (push (list :topic top-oid :psis psi-oids)
		*overview-table*)))))


(defun remove-psis-from-list (top psis)
  "Removes the passed psis from the psi list that is bound
   to the passed topic."
  (declare (TopicC top)
	   (List psis))
  (let ((top-oid (elephant::oid top)))
    (let ((node
	   (find-if (lambda(item)
		      (= (getf item :topic) top-oid))
		    *overview-table*))
	  (psi-oids (map 'list #'elephant::oid psis)))
      (when node
	(dolist (psi psi-oids)
	  (setf (getf node :psis) (delete psi (getf node :psis) :test #'=)))))))


(defun remove-topic-from-list (top)
  "Removes the node that represents the passed topic item."
  (declare (TopicC top))
  (let ((top-oid (elephant::oid top)))
    (setf *overview-table*
	  (delete-if (lambda(item) (= (getf item :topic) top-oid))
		     *overview-table*))))


(defun add-to-list (top psis)
  "Adds the psis contained in the list psis to the psi list that is
   bound to the psi list of the topic top."
  (declare (TopicC top)
	   (List psis))
  (let ((top-oid (elephant::oid top)))
    (let ((node
	   (find-if (lambda(item) (= (getf item :topic) top-oid))
		    *overview-table*))
	(psi-oids (map 'list #'elephant::oid psis)))
      (if node
	  (dolist (psi psi-oids)1
	    (pushnew psi (getf node :psis) :test #'=))
	  (push (list :topic top-oid :psis psi-oids) *overview-table*)))))


(defun serve-file (file-path &optional mime-type)
  "Returns a stream of the corresponding file."
  (with-open-file (in file-path :direction :input
		      :element-type 'flex:octet)
    (when mime-type
      (setf (hunchentoot:content-type*) mime-type))
    (let ((data (make-array (file-length in)
			    :element-type 'flex:octet)))
      (read-sequence data in)
      data)))