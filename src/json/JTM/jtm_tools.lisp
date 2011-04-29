;;+-----------------------------------------------------------------------------
;;+  Isidorus
;;+  (c) 2008-2010 Marc Kuester, Christoph Ludwig, Lukas Georgieff
;;+
;;+  Isidorus is freely distributable under the LLGPL license.
;;+  You can find a detailed description in trunk/docs/LLGPL-LICENSE.txt and
;;+  trunk/docs/LGPL-LICENSE.txt.
;;+-----------------------------------------------------------------------------

(defpackage :jtm
  (:use :cl :json :datamodel :base-tools :isidorus-threading
	:constants :exceptions)
  (:export :import-from-jtm
	   :export-as-jtm
	   :export-as-jtm-string
	   :export-construct-as-jtm-string))

(in-package :jtm)

(defvar *jtm-xtm* "jtm-xtm"); Represents the currently active TM of the JTM-Importer

(defvar item_type-topicmap "topicmap")

(defvar item_type-topic "topic")

(defvar item_type-name "name")

(defvar item_type-variant "variant")

(defvar item_type-occurrence "occurrence")

(defvar item_type-association "association")

(defvar item_type-role "role")


(defgeneric create-prefix-list-for-construct (construct &key revision)
  (:documentation "Returns a list of the following structure:
                   ((:pref 'pref_1' :value 'uri-pref') (...))."))


(defun export-construct-as-jtm-string (construct &key (revision (get-revision))
				       (jtm-format :1.1) (parent-p t))
  "Exports a name variant as JTM string.
   jtm-format must be set either to :1.0 or :1.1."
  (declare (Symbol jtm-format)
	   (Integer revision)
	   (Boolean parent-p)
	   (ReifiableConstructC construct))
  (with-reader-lock
    (let* ((prefixes
	    (when (eql jtm-format :1.1)
	      (create-prefix-list-for-construct construct :revision revision)))
	   (prefixes-p (cond ((eql jtm-format :1.1) t)
			     ((eql jtm-format :1.0) nil)
			     (t (error (make-condition 'JTM-error :message (format nil "From export-construct-as-jtm-string(): jtm-format must be set to :1.1 or :1.0, but is ~a" jtm-format))))))
	   (version (concat "\"" (symbol-name jtm-format) "\""))
	   (json-str (export-to-jtm construct :parent-p parent-p :prefixes prefixes
				    :prefixes-p prefixes-p :revision revision)))
      (concat "{\"version\":" version "," (subseq json-str 1)))))


(defun export-as-jtm-string (&key tm-id (revision (get-revision))
			     (jtm-format :1.1))
  "Exports a topic map or all stored constructs as JTM string.
   jtm-format must be set either to :1.0 or :1.1."
  (declare (type (or Null String) tm-id)
	   (Symbol jtm-format)
	   (Integer revision))
  (with-reader-lock
    (let ((tm 
	   (when tm-id
	     (get-item-by-item-identifier tm-id :revision revision)))
	  (version-1.1-p (eq jtm-format :1.1)))
      (let* ((tm-tops
	      (if tm
		  (delete-if #'(lambda(top)
				 (not (find-item-by-revision top revision)))
			     (topics tm))
		  (get-all-topics revision)))
	     (tm-assocs
	      (if tm
		  (delete-if #'(lambda(assoc)
				 (not (find-item-by-revision assoc revision)))
			     (associations tm))
		  (get-all-associations revision)))
	     (prefixes
	      (when version-1.1-p
		(create-prefix-list-for-tm tm-tops tm-assocs tm :revision revision)))
	     (version (if version-1.1-p
			  "\"version\":\"1.1\","
			  "\"version\":\"1.0\","))
	     (prefix-value (when version-1.1-p
			      (concat "\"prefixes\":"
				      (export-prefix-list-to-jtm prefixes) ",")))
	     (iis (concat "\"item_identifiers\":"
			  (export-identifiers-to-jtm
			   tm :identifier-type 'ItemIdentifierC :prefixes prefixes
			   :revision revision) ","))
	     (topics (concat "\"topics\":"
			     (export-topics-to-jtm tm-tops :prefixes prefixes
						   :revision revision)))
	     (assocs (concat "\"associations\":"
			     (export-associations-to-jtm tm-assocs :prefixes prefixes
							 :revision revision)))
	     (item-type (concat "\"item_type\":" item_type-topicmap)))
	(concat "{" version prefix-value iis topics assocs item-type "}")))))
	     
	     

(defun export-as-jtm (jtm-path &key tm-id (revision (get-revision))
		      (jtm-format :1.1))
  "Exports a topic map or all stored constructs as JTM file by calling
   export-as-jtm-string."
  (declare (type (or Null String) jtm-path tm-id)
	   (Symbol jtm-format)
	   (Integer revision))
  (with-open-file (stream jtm-path :direction :output)
    (format stream (export-as-jtm-string :tm-id tm-id :revision revision
					 :jtm-format jtm-format))))


(defun export-prefix-list-to-jtm (prefix-list)
  "Returns a json object that represent an object with namespaces and their
   prefix qualifiers."
  (declare (List prefix-list))
  (if prefix-list
      (let ((result "{"))
	(loop for item in prefix-list
	   do (push-string
	       (concat "\"" (getf item :pref) "\":"
		       (json:encode-json-to-string (getf item :value)) ",")
	       result))
	(concat (subseq result 0 (1- (length result))) "}"))
      "null"))


(defun create-prefix-list-for-tm (topics associations topic-map &key
			   (revision *TM-REVISION*))
  "Returns a list of the following structure: ((:pref 'pref_1'
   :value 'uri-pref') (...))."
  (declare (List topics associations)
	   (type (or Null TopicMapC) topic-map)
	   (Integer revision))
  (let ((identifiers
	 (append (loop for topic in topics
		    append
		      (append
		       (get-all-identifiers-of-construct topic :revision revision)
		       (loop for name in (names topic :revision revision)
			  append (append
				  (item-identifiers name :revision revision)
				  (loop for variant in
				       (variants name :revision revision)
				     append (append
					     (item-identifiers
					      variant :revision revision)))))
		       (loop for occ in (occurrences topic :revision revision)
			  append (append
				  (item-identifiers occ :revision revision)))))
		 (loop for assoc in associations
		    append (append
			    (item-identifiers assoc :revision revision)
			    (loop for role in (roles assoc :revision revision)
			       append (item-identifiers role :revision revision))))
		 (when topic-map
		   (item-identifiers topic-map :revision revision)))))
    (create-prefix-list-of-identifiers identifiers)))


(defun create-prefix-list-of-identifiers (identifiers)
  "Returns a list of the following structure: ((:pref 'pref_1'
   :value 'uri-pref') (...)) the list identifiers can own items of
   the type IdentifierC and of the form (list :pref 'pref' :value 'value')."
  (declare (List identifiers))
  (let ((prefixes
	 (remove-duplicates
	  (remove-null (map 'list  #'(lambda(id)
				       (if (typep id 'IdentifierC)
					   (prefix-of-uri (uri id))
					   (getf id :value)))
			    identifiers)) :test #'string=)))
    (let ((result
	   (append
	    (loop for idx to (1- (length prefixes))
	       collect (list :pref (concat "pref_" (write-to-string (1+ idx)))
			     :value (elt prefixes idx)))
	    (list (list :pref "xsd" :value *xsd-ns*)))))
      (sort result #'(lambda(x y)
		       (> (length (getf x :value)) (length (getf y :value))))))))


(defmethod create-prefix-list-for-construct ((construct VariantC) &key
					     (revision *TM-REVISION*))
  (declare (Integer revision))
  (let ((identifiers
	 (append
	  (loop for scope in (themes construct :revision revision)
	     append (get-all-identifiers-of-construct scope :revision revision))
	  (when (parent construct :revision revision)
	    (get-all-identifiers-of-construct
	     (parent construct :revision revision) :revision revision))
	  (when (reifier construct :revision revision)
	    (get-all-identifiers-of-construct
	     (reifier construct :revision revision))))))
    (create-prefix-list-of-identifiers identifiers)))


(defmethod create-prefix-list-for-construct ((construct NameC) &key
					     (revision *TM-REVISION*))
  (declare (Integer revision))
  (let ((identifiers
	 (append
	  (loop for scope in (themes construct :revision revision)
	     append (get-all-identifiers-of-construct scope :revision revision))
	  (when (parent construct :revision revision)
	    (get-all-identifiers-of-construct
	     (parent construct :revision revision) :revision revision))
	  (loop for var in (variants construct :revision revision)
	     append (create-prefix-list-for-construct var :revision revision))
	  (when (reifier construct :revision revision)
	    (get-all-identifiers-of-construct
	     (reifier construct :revision revision)))
	  (when (instance-of construct :revision revision)
	    (get-all-identifiers-of-construct
	     (instance-of construct :revision revision))))))
    (create-prefix-list-of-identifiers identifiers)))


(defmethod create-prefix-list-for-construct ((construct OccurrenceC) &key
					     (revision *TM-REVISION*))
  (declare (Integer revision))
  (let ((identifiers
	 (append
	  (loop for scope in (themes construct :revision revision)
	     append (get-all-identifiers-of-construct scope :revision revision))
	  (when (parent construct :revision revision)
	    (get-all-identifiers-of-construct
	     (parent construct :revision revision) :revision revision))
	  (when (reifier construct :revision revision)
	    (get-all-identifiers-of-construct
	     (reifier construct :revision revision)))
	  (when (instance-of construct :revision revision)
	    (get-all-identifiers-of-construct
	     (instance-of construct :revision revision))))))
    (create-prefix-list-of-identifiers identifiers)))


(defmethod create-prefix-list-for-construct ((construct RoleC) &key
					     (revision *TM-REVISION*))
  (declare (Integer revision))
  (let ((identifiers
	 (append
	  (when (parent construct :revision revision)
	    (get-all-identifiers-of-construct
	     (parent construct :revision revision) :revision revision))
	  (when (reifier construct :revision revision)
	    (get-all-identifiers-of-construct
	     (reifier construct :revision revision)))
	  (when (player construct :revision revision)
	    (get-all-identifiers-of-construct
	     (player construct :revision revision)))
	  (when (instance-of construct :revision revision)
	    (get-all-identifiers-of-construct
	     (instance-of construct :revision revision))))))
    (create-prefix-list-of-identifiers identifiers)))


(defmethod create-prefix-list-for-construct ((construct AssociationC) &key
					     (revision *TM-REVISION*))
  (declare (Integer revision))
  (let ((identifiers
	 (append
	  (loop for tm in (in-topicmaps construct :revision revision)
	     append (get-all-identifiers-of-construct tm :revision revision))
	  (when (reifier construct :revision revision)
	    (get-all-identifiers-of-construct
	     (reifier construct :revision revision)))
	  (when (instance-of construct :revision revision)
	    (get-all-identifiers-of-construct
	     (instance-of construct :revision revision)))
	  (loop for scope in (themes construct :revision revision)
	     append (get-all-identifiers-of-construct construct
						      :revision revision))
	  (loop for role in (roles construct :revision revision)
	     append (create-prefix-list-for-construct role :revision revision)))))
    (create-prefix-list-of-identifiers identifiers)))


(defmethod create-prefix-list-for-construct ((construct TopicC) &key
					     (revision *TM-REVISION*))
  (declare (Integer revision))
  (let ((identifiers
	 (append
	  (get-all-identifiers-of-construct construct :revision revision)
	  (loop for occ in (occurrences construct :revision revision)
	     append (create-prefix-list-for-construct occ :revision revision))
	  (loop for name in (names construct :revision revision)
	     append (create-prefix-list-for-construct name :revision revision))
	  (loop for top in (list-instanceof construct :revision revision)
	     append (get-all-identifiers-of-construct top :revision revision)))))
    (create-prefix-list-of-identifiers identifiers)))