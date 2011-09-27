;;+-----------------------------------------------------------------------------
;;+  Isidorus
;;+  (c) 2008-2010 Marc Kuester, Christoph Ludwig, Lukas Georgieff
;;+
;;+  Isidorus is freely distributable under the LLGPL license.
;;+  You can find a detailed description in trunk/docs/LLGPL-LICENSE.txt and
;;+  trunk/docs/LGPL-LICENSE.txt.
;;+-----------------------------------------------------------------------------

(in-package :rest-interface)

(defparameter *gdl-get-fragment* "/gdl/fragment/(.+)$")
(defparameter *gdl-get-schema* "/gdl/schema/?$")
(defparameter *gdl-commit-fragment* "/gdl/commit/?")
(defparameter *gdl-delete-fragment* "/gdl/delete/?")
(defparameter *gdl-host-address-hash-object* "/hash-object")
(defparameter *gdl-host-address-environment* "/environment")
(defparameter *gdl-base-path* "anaToMia/hosted_files/")
(defparameter *gdl-host-file* (concat *gdl-base-path* "GDL_Widgets.html"))
(defparameter *gdl-tm-id* "http://textgrid.org/serviceregistry/gdl-frontend/gdl-tm")
(defparameter *gdl-sparql* "/gdl/tm-sparql/?$")


(defun set-up-gdl-interface (&key (get-fragment *gdl-get-fragment*)
			     (get-schema *gdl-get-schema*)
			     (commit-fragment *gdl-commit-fragment*)
			     (delete-fragment *gdl-delete-fragment*)
			     (gdl-sparql *gdl-sparql*)
			     (base-path *gdl-base-path*)
			     (host-address-hash-object *gdl-host-address-hash-object*)
			     (host-address-environment *gdl-host-address-environment*)
			     (host-file *gdl-host-file*))
  (declare (String get-fragment get-schema commit-fragment
		   delete-fragment host-address-hash-object
		   host-address-environment host-file))

  ;(init-cache nil)
  ;(format t "~%")
  (init-fragments nil)

  ;; registers the http-code 500 for an internal server error to the standard
  ;; return codes. so there won't be attached a hunchentoot default message,
  ;; this is necessary to be able to send error messages in an individual way/syntax
  ;; e.g. a json error-message.
  (push hunchentoot:+http-internal-server-error+ hunchentoot:*approved-return-codes*)
  
  (init-hosted-files :host-address-hash-object host-address-hash-object
		     :host-address-environment host-address-environment
		     :host-file host-file :base-path base-path)
  
  (push
   (create-regex-dispatcher get-fragment
			    (if (> *use-http-authentication* 2)
				(lambda(&optional param)
				  (with-http-authentication
				      (return-json-fragment-handler param)))
				#'return-json-fragment-handler))
   hunchentoot:*dispatch-table*)

  (push
   (create-regex-dispatcher get-schema
			    (if (> *use-http-authentication* 2)
				(lambda()
				  (with-http-authentication
				      (return-gdl-schema-handler)))
				#'return-gdl-schema-handler))
   hunchentoot:*dispatch-table*)

  (push
   (create-regex-dispatcher commit-fragment
			    (if (> *use-http-authentication* 1)
				(lambda()
				  (with-http-authentication
				      (commit-fragment-handler)))
				#'commit-fragment-handler))
   hunchentoot:*dispatch-table*)

  (push
   (create-regex-dispatcher delete-fragment
			    (if (> *use-http-authentication* 1)
				(lambda()
				  (with-http-authentication
				      (delete-handler)))
				#'delete-handler))
   hunchentoot:*dispatch-table*)

  (push
   (create-regex-dispatcher gdl-sparql
			    (if (> *use-http-authentication* 2)
				(lambda(&optional param)
				  (with-http-authentication
				      (gdl-sparql-handler param)))
				#'gdl-sparql-handler))
   hunchentoot:*dispatch-table*))


(defun init-hosted-files (&key (host-address-hash-object *gdl-host-address-hash-object*)
			  (host-address-environment *gdl-host-address-environment*)
			  (host-file *gdl-host-file*)
			  (base-path *gdl-base-path*))
  "Adds handlers for the css, html and js files needed by the frontend."
  (declare (String host-address-hash-object host-address-environment
		   host-file base-path))
  ;; add the actual html file
  (let ((full-host-path
	 (concat (namestring
		  (asdf:component-pathname constants:*isidorus-system*))
		 host-file))
	(absolute-base-path
	 (concat
	  (namestring
	   (asdf:component-pathname constants:*isidorus-system*))
	  base-path)))
    (if (> *use-http-authentication* 0)
	(progn
	  (define-easy-handler (isidorus-ui :uri host-address-hash-object
					    :default-request-type :get)
	      ()
	    (with-http-authentication
		(serve-file full-host-path "text/html")))
	  (define-easy-handler (isidorus-ui :uri host-address-environment
					    :default-request-type :get)
	      ()
	    (with-http-authentication
		(serve-file full-host-path "text/html"))))
	(progn
	  (push
	   (create-static-file-dispatcher-and-handler
	    host-address-hash-object full-host-path "text/html")
	   hunchentoot:*dispatch-table*)
	  (push
	   (create-static-file-dispatcher-and-handler
	    host-address-environment full-host-path "text/html")
	   hunchentoot:*dispatch-table*)))
    ; add all additional files
    (let ((absolute-base-path-len (length absolute-base-path)))
      (com.gigamonkeys.pathnames:walk-directory
       "anaToMia/hosted_files"
       (lambda(item)
	 (unless (or (search "/.svn/" (namestring item) :test #'string=)
		     (string= full-host-path (namestring item)))
	   (let* ((rel-addr (subseq (namestring item) absolute-base-path-len))
		  (content-type (generate-content-type (file-namestring item)))
		  (rel-uri (concat "/" rel-addr)))
	     (push
	      (create-static-file-dispatcher-and-handler
	       rel-uri item content-type)
	      hunchentoot:*dispatch-table*))))))))


(defun generate-content-type(file-name)
  "Returns a mime-type that corresponds to the passed
   file-ending, note currently onle a fey types are supported!"
  (declare (String file-name))
  (cond ((string-ends-with file-name "png" :ignore-case t)
	 "image/png")
	((string-ends-with file-name "html" :ignore-case t)
	 "text/html")
	((string-ends-with file-name "js" :ignore-case t)
	 "application/json")
	((string-ends-with file-name "css" :ignore-case t)
	 "text/css")
	((string-ends-with file-name "gif" :ignore-case t)
	 "image/gif")
	(t
	 "text/plain")))


(defun delete-handler()
  "marks the corresponding construct(s) as deleted"
  (let ((http-method (hunchentoot:request-method*)))
    (if (or (eq http-method :DELETE)
	    (eq http-method :POST))
	(let ((external-format
	       (flexi-streams:make-external-format :UTF-8 :eol-style :LF)))
	  (let ((json-data
		 (hunchentoot:raw-post-data :external-format external-format
					    :force-text t)))
	    (with-writer-lock
	      (let* ((rev (d:get-revision)) 
		     (result (jtm-delete-interface:mark-as-deleted-from-jtm
			      json-data :revision rev)))
		(let ((tops
		       (remove-null
			(cond ((or (typep result 'OccurrenceC)
				   (typep result 'NameC))
			       (let ((top (parent result :revision (1- rev))))
				 (when top (list top))))
			      ((typep result 'VariantC)
			       (let ((name (parent result :revision (1- rev))))
				 (when name
				   (let ((top (parent name :revision (1- rev))))
				     (when top (list top))))))
			      ((typep result 'AssociationC)
			       (map 'list (lambda(role)
					    (player role :revision (1- rev)))
				    (roles result :revision (1- rev))))
			      ((typep result 'TopicC)
			       (let ((assocs
				      (remove-null
				       (map 'list (lambda(role)
						    (parent role :revision (1- rev)))
					    (player-in-roles result :revision (1- rev)))))
				     (frags
				      (elephant:get-instances-by-value
				       'd:FragmentC 'd:topic result)))
				 (map nil #'elephant:drop-instance frags)
				 (loop for assoc in assocs
				    append (map 'list (lambda(role)
							(player role :revision (1- rev)))
						(roles assoc :revision (1- rev))))))))))
		  (map nil (lambda(top)
			     (let ((frags
				    (elephant:get-instances-by-value 'd:FragmentC 'd:topic top)))
			       (map nil #'elephant:drop-instance frags))
			     (create-latest-fragment-of-topic top))
		       (if (typep result 'd:TopicC)
			   (delete result tops)
			   tops)))
		(unless result
		  (setf (hunchentoot:return-code*) hunchentoot:+http-not-found+)
		  (format nil "object not found"))))))
	(setf (hunchentoot:return-code*) hunchentoot:+http-bad-request+))))


(defun commit-fragment-handler ()
  "handles commits in the JTM 1.1 format."
  (let ((http-method (hunchentoot:request-method*)))
    (if (or (eq http-method :PUT)
	    (eq http-method :POST))
	(let ((external-format
	       (flexi-streams:make-external-format :UTF-8 :eol-style :LF)))
	  (let ((json-data
		 (hunchentoot:raw-post-data :external-format external-format
					    :force-text t)))
	    (with-writer-lock 
	      (jtm-importer:import-construct-from-jtm-string
	       json-data :revision (get-revision) :tm-id *gdl-tm-id*
	       :create-fragments t))))
	(setf (hunchentoot:return-code*) hunchentoot:+http-bad-request+))))


(defun return-gdl-schema-handler()
  "Currently the entore topic map is returned.
   To emerge the efficiency it will be necessary
   to structure the data as GDL-Fragments, so each view or schema
   can be served separately."
  (let ((http-method (hunchentoot:request-method*)))
    (if (eq http-method :GET)
	(progn (setf (hunchentoot:content-type*) "application/json")
	       (jtm-exporter:export-as-jtm-string :revision 0))
	(setf (hunchentoot:return-code*) hunchentoot:+http-bad-request+))))


(defun return-json-fragment-handler(&optional psi)
  "returns the json-fragmen belonging to the psi passed by the parameter psi"
  (assert psi)
  (let ((http-method (hunchentoot:request-method*)))
    (if (eq http-method :GET)
	(let ((identifier (hunchentoot:url-decode psi)))
	  (setf (hunchentoot:content-type*) "application/json") ;RFC 4627
	  (let ((fragment
		 (with-reader-lock
		   (get-latest-fragment-of-topic identifier))))
	    (if fragment
		(with-reader-lock
		  (jtm-exporter:export-construct-as-jtm-string
		   fragment :revision 0))
		(progn
		  (setf (hunchentoot:return-code*) hunchentoot:+http-not-found+)
		  (setf (hunchentoot:content-type*) "text")
		  (format nil "Topic \"~a\" not found" psi)))))
	(setf (hunchentoot:return-code*) hunchentoot:+http-bad-request+))))


(defun gdl-sparql-handler(&optional param)
  "Returns a JSON object representing a SPARQL response."
  (declare (Ignorable param))
  (if (eql (hunchentoot:request-method*) :POST)
      (let ((external-format (flexi-streams:make-external-format
			      :UTF-8 :eol-style :LF)))
	(let ((sparql-request (hunchentoot:raw-post-data
			       :external-format external-format
			       :force-text t)))
	  (export-construct-as-isidorus-json-string
	   (make-instance 'SPARQL-Query :query sparql-request
			  :revision 0))))
      (setf (hunchentoot:return-code*) hunchentoot:+http-bad-request+)))