;;+-----------------------------------------------------------------------------
;;+  Isidorus
;;+  (c) 2008-2010 Marc Kuester, Christoph Ludwig, Lukas Georgieff
;;+
;;+  Isidorus is freely distributable under the LLGPL license.
;;+  You can find a detailed description in trunk/docs/LLGPL-LICENSE.txt and
;;+  trunk/docs/LGPL-LICENSE.txt.
;;+-----------------------------------------------------------------------------


(in-package :jtm)


(defgeneric export-to-jtm (construct &key item-type-p parent-p prefixes
				     prefixes-p revision)
  (:documentation "Exports the given construct in JTM notation.
                   If item-type-p is t the corresponding item-type
                   will be also set. If parent-p is t the corresponding
                   parent of the given construct is also set.
                   prefixes is a plist of the form ((:pref pref :value <value>) ...),
                   whereas pref is used as prefix identifier and value is
                   used as actual value. If prefix-p is set to t the member
                   prefixes will be set to the corresponding values in prefixes.
                   If prefixes is set these prefixes are used for the given
                   construct and all its sub-constructs."))


(defmethod export-to-jtm ((construct TopicC) &key (item-type-p t)
			  (parent-p nil) prefixes prefixes-p (revision 0))
  "Exports a topic as JTM string."
  (declare (Boolean item-type-p parent-p prefixes-p)
	   (List prefixes)
	   (type (or Null Integer) revision))
  (let ((prefix-value (when prefixes-p
			(concat "\"prefixes\":"
				(export-prefix-list-to-jtm prefixes))))
	(top-psis
	 (concat "\"subject_identifiers\":"
		 (export-identifiers-to-jtm
		  construct :identifier-type 'PersistentIdC :prefixes prefixes
		  :revision revision) ","))
	(top-sls
	 (concat "\"subject_locators\":"
		 (export-identifiers-to-jtm
		  construct :identifier-type 'SubjectLocatorC :prefixes prefixes
		  :revision revision) ","))
	(top-iis
	 (concat "\"item_identifiers\":"
		 (export-identifiers-to-jtm
		  construct :identifier-type 'ItemIdentifierC :prefixes prefixes
		  :revision revision) ","))
	(instance-ofs
	 (concat "\"instance_of\":"
		 (export-instance-ofs-to-jtm construct :prefixes prefixes
					     :revision revision) ","))
	(item-type (when item-type-p
		     (concat "\"item_type\":" item_type-topic ",")))
	(top-parent
	 (when parent-p
	   (concat "\"parent\":"
		   (export-parent-reference-to-jtm construct :prefixes prefixes
						   :revision revision) ",")))
	(top-names
	 (concat "\"names\":"
		 (export-names-to-jtm
		  construct :item-type-p nil :prefixes prefixes
		  :prefixes-p prefixes-p :revision revision) ","))
	(top-occs
	 (concat "\"occurrences\":"
		 (export-occurrences-to-jtm
		  construct :item-type-p nil :prefixes prefixes
		  :prefixes-p prefixes-p :revision revision))))
    (concat "{" prefix-value top-psis top-sls top-iis instance-ofs item-type
	    top-parent top-names top-occs "}")))


(defmethod export-to-jtm ((construct IdentifierC) &key item-type-p parent-p
		       prefixes prefixes-p (revision *TM-REVISION*))
  "Exports any given object of the type IdentifierC"
  (declare (Ignorable item-type-p parent-p revision prefixes-p)
	   (List prefixes))
  (let ((possible-prefix
	 (when prefixes
	   (loop for item in prefixes
	      when (string-starts-with (uri construct) (getf item :value))
	      return item))))
    (if possible-prefix
	(json:encode-json-to-string
	 (concat "[" (getf possible-prefix :pref) ":"
		 (subseq (uri construct) (length (getf possible-prefix :value)))))
	(json:encode-json-to-string (uri construct)))))


(defgeneric export-identifiers-to-jtm (construct &key identifier-type prefixes
						 revision)
  (:documentation "Exports all identifiers of the given construct and type
                   given by identifier-type as JTM-array.")
  (:method ((construct ReifiableConstructC) &key (identifier-type 'ItemIdentifierC)
	    prefixes  (revision *TM-REVISION*))
    (declare (Symbol identifier-type)
	     (List prefixes)
	     (type (or Null Integer) revision))
    (let ((ids
	   (funcall (cond ((eql identifier-type 'PersistentIdC)
			   #'psis)
			  ((eql identifier-type 'SubjectLocatorC)
			   #'locators)
			  ((eql identifier-type 'ItemIdentifierC)
			   #'item-identifiers)
			  (t
			   (make-condition 'JTM-error
					   :message (format nil "From export-identifiers-to-jtm(): identifier type must be one of 'PersistentIdC, 'ItemIdentifierC, or 'SubjectLocatorC, but is: ~a" identifier-type))))
		    construct :revision revision)))
      (if ids
	  (let ((values "["))
	    (loop for id in ids
	       do (push-string
		   (concat (export-to-jtm id :prefixes prefixes)
			   ",") values))
	    (concat (subseq values 0 (1- (length values))) "]"))
	  "null"))))


(defmethod export-to-jtm ((construct NameC) &key item-type-p parent-p
		       prefixes prefixes-p (revision *TM-REVISION*))
  "Exports any given object bof the type NameC"
  (declare (Boolean item-type-p parent-p prefixes-p)
	   (List prefixes)
	   (type (or Null Integer) revision))       
  (let ((prefix-value (when prefixes-p
		    (concat "\"prefixes\":"
			    (export-prefix-list-to-jtm prefixes) "," )))
	(iis (concat "\"item_identifiers\":"
		     (export-identifiers-to-jtm 
		      construct :prefixes prefixes :revision revision) ","))
	(value (concat "\"value\":"
		       (json:encode-json-to-string (charvalue construct)) ","))
	(type (concat "\"type\":"
		      (export-type-to-jtm construct :prefixes prefixes
					  :error-if-nil nil :revision revision)
		      ","))
	(item-type (when item-type-p
		     (concat "\"item_type\":" item_type-name ",")))
	(name-parent
	 (when parent-p
	   (concat "\"parent\":"
		   (export-parent-reference-to-jtm construct :prefixes prefixes
						   :revision revision) ",")))
	(scopes (concat "\"scope\":"
			(export-scopes-to-jtm
			 construct :prefixes prefixes :revision revision) ","))
	(vars (concat "\"variants\":"
		      (export-variants-to-jtm
		       construct :item-type-p nil :prefixes prefixes
		       :prefixes-p prefixes-p :revision revision) ","))
	(name-reifier (concat "\"reifier\":"
			      (export-reifier-to-jtm construct :prefixes prefixes
						     :revision revision))))
    (concat "{" prefix-value iis value type item-type name-parent scopes vars
	    name-reifier "}")))


(defgeneric export-reifier-to-jtm (construct &key prefixes revision)
  (:documentation "Returns a topic reference that represents the construct's
                   reifier-topic.")
  (:method ((construct ReifiableConstructC) &key prefixes
	    (revision *TM-REVISION*))
    (declare (List prefixes)
	     (type (or Null Integer) revision))
    (if (reifier construct :revision revision)
	(export-topic-reference-to-jtm
	 (reifier construct :revision revision) :prefixes prefixes
	 :revision revision)
	"null")))


(defgeneric export-scopes-to-jtm (construct &key prefixes revision)
  (:documentation "Exports all topics within the scope of the passed construct.
                   The result value is a JSON array of topic references.")
  (:method ((construct ScopableC) &key prefixes (revision *TM-REVISION*))
    (declare (List prefixes)
	     (Integer revision))
    (let ((scope-tops (themes construct :revision revision)))
      (if scope-tops
	  (let ((result "["))
	    (loop for top in scope-tops
	       do (push-string
		   (concat (export-topic-reference-to-jtm top :prefixes prefixes
							  :revision revision) ",")
		   result))
	    (concat (subseq result 0 (1- (length result))) "]"))
	  "null"))))


(defgeneric export-type-to-jtm (construct &key prefixes error-if-nil revision)
  (:documentation "Returns a string of the type \"type\":<type-uri>. If
                   error-if-nil is set to t and the given construct has no
                   name, a JTM-error is thrown.")
  (:method ((construct TypableC) &key prefixes (error-if-nil t)
	    (revision *TM-REVISION*))
    (declare (List prefixes)
	     (Boolean error-if-nil)
	     (type (or Null Integer) revision))
    (let ((type (instance-of construct :revision revision)))
      (when (and error-if-nil (not type))
	(make-condition 'JTM-error :message (format nil "From export-type-to-jtm(): the construct ~a is not bound to a type" construct)))
      (if type
	  (export-topic-reference-to-jtm construct :prefixes prefixes
					 :revision revision)
	  "null"))))


(defgeneric export-topic-reference-to-jtm (construct &key prefixes revision)
  (:documentation "Returns an identifier that is the reference of the given
                   topic. If the topic owns at least one psi the return value
                   is si:psi-value. If the topic owns no psi but at least one
                   subject-locator the return value is sl:sl-value. If the
                   topic owns no psi and no subject-locator but at least one
                   item-identifier the return value is ii:ii-value. If the
                   topic does not have any identifiers a JTM-error is thrown.")
  (:method ((construct TopicC) &key prefixes(revision *TM-REVISION*))
    (declare (List prefixes)
	     (type (or Null Integer) revision))
    (cond ((psis construct :revision revision)
	   (concat "\"si:"
		   (export-to-jtm (first (psis construct :revision revision))
				  :prefixes prefixes)
		   "\""))
	  ((locators construct :revision revision)
	   (concat "\"sl:"
		   (export-to-jtm (first (locators construct :revision revision))
				  :prefixes prefixes)
		   "\""))
	  ((item-identifiers construct :revision revision)
	   (concat "\"ii:"
		   (export-to-jtm (first (item-identifiers construct :revision revision))
				  :prefixes prefixes)
		   "\""))
	  (t
	   (make-condition 'JTM-error :message (format nil "From export-topic-reference-to-jtm(): the topic ~a has no identifiers" construct))))))


(defgeneric export-parent-reference-to-jtm (construct &key prefixes revision)
  (:documentation "Returns an identifier that is the reference of the given
                   construct's parent. If the parent is a topic
                   export-topic-reference-to-jtm is called otherwise an
                   item-identifier of the parent is returned.")
  (:method ((construct ReifiableConstructC) &key prefixes (revision *TM-REVISION*))
    (declare (List prefixes)
	     (type (or Null Integer) revision))
    (let ((parent
	   (cond ((and (or (typep construct 'TopicC)
			   (typep construct 'AssociationC))
		       (in-topicmaps construct :revision revision))
		  (first (in-topicmaps construct :revision revision)))
		 ((or (typep construct 'CharacteristicC)
		      (typep construct 'RoleC))
		  (parent construct :revision revision)))))
      (unless parent
	(make-condition 'JTM-error :message (format nil "From export-parent-reference-to-jtm(): the passed construct ~a is not bound to parent" construct)))
      (if (typep parent 'TopicC)
	  (export-topic-reference-to-jtm parent :prefixes prefixes
					 :revision revision)
	  (progn
	    (unless (item-identifiers parent :revision revision)
	      (make-condition 'JTM-error :message (format nil "From export-parent-reference-to-jtm(): the parent [~a] of the passed construct [~a] is not bound to an item-identifier" parent construct)))
	    (concat "\"ii:"
		    (export-to-jtm (first (item-identifiers parent :revision revision))
				   :prefixes prefixes)
		    "\""))))))


(defmethod export-to-jtm ((construct VariantC) &key (item-type-p t)
			  parent-p prefixes prefixes-p (revision 0))
  "Exports any object of the type VariantC as JTM-object."
  (declare (Boolean item-type-p parent-p prefixes-p)
	   (List prefixes)
	   (type (or Null Integer) revision))       
  (let ((prefix-value (when prefixes-p
			(concat "\"prefixes\":"
				(export-prefix-list-to-jtm prefixes) ",")))
	(iis (concat "\"item_identifiers\":"
		     (export-identifiers-to-jtm 
		      construct :prefixes prefixes :revision revision) ","))
	(value (concat "\"value\":"
		       (json:encode-json-to-string (charvalue construct)) ","))
	(datatype (concat "\"datatype\":"
			  (json:encode-json-to-string (datatype construct)) ","))
	(item-type (when item-type-p
		     (concat "\"item_type\":" item_type-variant ",")))
	(var-parent
	 (when parent-p
	   (concat "\"parent\":"
		   (export-parent-reference-to-jtm construct :prefixes prefixes
						   :revision revision) ",")))
	(scopes (concat "\"scope\":"
			(export-scopes-to-jtm
			 construct :prefixes prefixes :revision revision) ","))
	(var-reifier (concat "\"reifier\":"
			      (export-reifier-to-jtm construct :prefixes prefixes
						     :revision revision))))
    (concat "{" prefix-value iis datatype value item-type var-parent scopes
	    var-reifier "}")))


(defgeneric export-variants-to-jtm (construct &key item-type-p parent-p
					      prefixes prefixes-p revision)
  (:documentation "Returns a json array of JTM variant-objects.")
  (:method ((construct NameC) &key (item-type-p t) parent-p
	    prefixes prefixes-p (revision *TM-REVISION*))
    (declare (Boolean item-type-p parent-p prefixes-p)
	     (List prefixes)
	     (type (or Null Integer) revision))
    (if (variants construct :revision revision)
	(let ((result "["))
	  (loop for var in (variants construct :revision revision)
	     do (push-string
		 (concat (export-to-jtm
			  var :item-type-p item-type-p :parent-p parent-p
			  :prefixes prefixes :prefixes-p prefixes-p
			  :revision revision) ",")
		 result))
	  (concat (subseq result 0 (1- (length result))) "]"))
	"null")))


(defgeneric export-names-to-jtm (construct &key item-type-p parent-p
					   prefixes prefixes-p revision)
  (:documentation "Returns a json array of JTM name-objects.")
  (:method ((construct TopicC) &key (item-type-p t) parent-p
	    prefixes prefixes-p (revision *TM-REVISION*))
    (declare (Boolean item-type-p parent-p prefixes-p)
	     (List prefixes)
	     (type (or Null Integer) revision))
    (if (variants construct :revision revision)
	(let ((result "["))
	  (loop for name in (names construct :revision revision)
	     do (push-string
		 (concat (export-to-jtm
			  name :item-type-p item-type-p :parent-p parent-p
			  :prefixes prefixes :prefixes-p prefixes-p
			  :revision revision) ",")
		 result))
	  (concat (subseq result 0 (1- (length result))) "]"))
	"null")))


(defmethod export-to-jtm ((construct OccurrenceC) &key (item-type-p t)
			  parent-p prefixes prefixes-p (revision 0))
  "Exports any object of the type OccurrenceC as JTM-object."
  (declare (Boolean item-type-p parent-p prefixes-p)
	   (List prefixes)
	   (type (or Null Integer) revision))       
  (let ((prefix-value (when prefixes-p
			(concat "\"prefixes\":"
				(export-prefix-list-to-jtm prefixes) ",")))
	(iis (concat "\"item_identifiers\":"
		     (export-identifiers-to-jtm 
		      construct :prefixes prefixes :revision revision) ","))
	(value (concat "\"value\":"
		       (json:encode-json-to-string (charvalue construct)) ","))
	(datatype (concat "\"datatype\":"
			  (json:encode-json-to-string (datatype construct)) ","))
	(type (concat "\"type\":"
		      (export-type-to-jtm construct :prefixes prefixes
					  :revision revision)
		      ","))
	(item-type (when item-type-p
		     (concat "\"item_type\":" item_type-occurrence ",")))
	(occ-parent
	 (when parent-p
	   (concat "\"parent\":"
		   (export-parent-reference-to-jtm construct :prefixes prefixes
						   :revision revision) ",")))
	(scopes (concat "\"scope\":"
			(export-scopes-to-jtm
			 construct :prefixes prefixes :revision revision) ","))
	(occ-reifier (concat "\"reifier\":"
			      (export-reifier-to-jtm construct :prefixes prefixes
						     :revision revision))))
    (concat "{" prefix-value iis datatype type value item-type occ-parent
	    scopes occ-reifier "}")))


(defgeneric export-occurrences-to-jtm (construct &key item-type-p parent-p
						 prefixes prefixes-p revision)
  (:documentation "Returns a json array of JTM occurrence-objects.")
  (:method ((construct TopicC) &key (item-type-p t) parent-p
	    prefixes prefixes-p (revision *TM-REVISION*))
    (declare (Boolean item-type-p parent-p prefixes-p)
	     (List prefixes)
	     (type (or Null Integer) revision))
    (if (occurrences construct :revision revision)
	(let ((result "["))
	  (loop for occ in (occurrences construct :revision revision)
	     do (push-string
		 (concat (export-to-jtm
			  occ :item-type-p item-type-p :parent-p parent-p
			  :prefixes prefixes :prefixes-p prefixes-p
			  :revision revision) ",")
		 result))
	  (concat (subseq result 0 (1- (length result))) "]"))
	"null")))


(defgeneric export-instance-ofs-to-jtm (construct &key prefixes revision)
  (:documentation "Exports a list of topic references, whereas every topic
                   reference represents a topic type that the given topic
                   is an instance of.")
  (:method ((construct TopicC) &key prefixes (revision *TM-REVISION*))
    (let ((instance-ofs (list-instanceof construct :revision revision)))
      (if instance-ofs
	  (let ((result "["))
	    (loop for top in instance-ofs
	       do (push-string
		   (concat (export-topic-reference-to-jtm
			    top :prefixes prefixes :revision revision) ",")
		   result))
	    (concat (subseq result 0 (1- (length result))) "]"))
	  "null"))))


(defmethod export-to-jtm ((construct RoleC) &key (item-type-p t)
			  (parent-p nil) prefixes prefixes-p (revision 0))
  "Exports any object of type RoleC as JTM-role-object."
  (declare (Boolean item-type-p parent-p prefixes-p)
	   (List prefixes)
	   (type (or Null Integer) revision))
  (let ((prefix-value (when prefixes-p
			(concat "\"prefixes\":"
				(export-prefix-list-to-jtm prefixes) ",")))
	(iis (concat "\"item_identifiers\":"
		     (export-identifiers-to-jtm 
		      construct :prefixes prefixes :revision revision) ","))
	(type (concat "\"type\":"
		      (export-type-to-jtm construct :prefixes prefixes
					  :revision revision)
		      ","))
	(item-type (when item-type-p
		     (concat "\"item_type\":" item_type-role ",")))
	(role-parent 
	 (when parent-p
	   (concat "\"parent\":"
		   (export-parent-reference-to-jtm construct :prefixes prefixes
						   :revision revision) ",")))
	(role-reifier (concat "\"reifier\":"
			      (export-reifier-to-jtm construct :prefixes prefixes
						     :revision revision)))
	(role-player
	 (progn
	   (unless (player construct :revision revision)
	     (make-condition 'JTM-error :message "From export-to-jtm(): the role [~a] is not bound to a player" construct))
	   (concat "\"player\":"
		   (export-topic-reference-to-jtm
		    (player construct :revision revision) :prefixes prefixes
		    :revision revision)))))
    (concat "{" prefix-value iis type item-type role-parent role-reifier
	    role-player "}")))


(defgeneric export-roles-to-jtm (construct &key prefixes prefixes-p
					   parent-p revision)
  (:documentation "Exports a json array of roles serialised
                   as JTM-role-objects.")
  (:method ((construct AssociationC) &key parent-p prefixes prefixes-p
	    (revision *TM-REVISION*))
    (declare (List prefixes)
	     (Boolean prefixes-p parent-p)
	     (Integer revision))
    (let ((assoc-roles (roles construct :revision revision)))
      (if assoc-roles
	  (let ((result "["))
	    (loop for role in assoc-roles
	       do (push-string
		   (concat (export-to-jtm
			    role :prefixes prefixes :prefixes-p prefixes-p
			    :parent-p parent-p :revision revision) ",")
		   result))
	    (concat (subseq result 0 (1- (length result))) "]"))
	  "null"))))


(defmethod export-to-jtm ((construct AssociationC) &key (item-type-p t)
			  (parent-p nil) prefixes prefixes-p (revision 0))
  "Exports any object of type AssociationC as JTM-association-object."
  (declare (Boolean item-type-p parent-p prefixes-p)
	   (List prefixes)
	   (type (or Null Integer) revision))
  (let ((prefix-value (when prefixes-p
			(concat "\"prefixes\":"
				(export-prefix-list-to-jtm prefixes) ",")))
	(iis (concat "\"item_identifiers\":"
		     (export-identifiers-to-jtm 
		      construct :prefixes prefixes :revision revision) ","))
	(type (concat "\"type\":"
		      (export-type-to-jtm construct :prefixes prefixes
					  :revision revision)
		      ","))
	(item-type (when item-type-p
		     (concat "\"item_type\":" item_type-association ",")))
	(assoc-parent
	 (when parent-p
	   (concat "\"parent\":"
		   (export-parent-reference-to-jtm construct :prefixes prefixes
						   :revision revision) ",")))
	(assoc-reifier (concat "\"reifier\":"
			       (export-reifier-to-jtm construct :prefixes prefixes
						      :revision revision)))
	(scopes (concat "\"scope\":"
			(export-scopes-to-jtm
			 construct :prefixes prefixes :revision revision) ","))
	(assoc-roles
	 (concat "\"roles\":"
		 (export-roles-to-jtm construct :prefixes prefixes
				      :prefixes-p prefixes-p :revision revision))))
    (concat "{" prefix-value iis type item-type assoc-parent assoc-reifier
	    scopes assoc-roles "}")))


(defmethod export-to-jtm ((construct FragmentC) &key (item-type-p t)
			  (parent-p nil) prefixes prefixes-p (revision 0))
  (declare (Boolean prefixes-p)
	   (Ignorable parent-p item-type-p prefixes)
	   (type (or Null Integer) revision))
  (let* ((prefixes-list
	  (create-prefix-list-of-fragment construct :revision revision))
	 (prefixes-value (export-prefix-list-to-jtm prefixes-list))
	 (frag-tops (concat "\"topics\":"
			    (export-topics-to-jtm
			     (topics construct) :prefixes prefixes-list
			     :revision revision)))
	 (frag-assocs (concat "\"associations\":"
			      (export-associations-to-jtm
			       (associations construct) :prefixes prefixes-list
			       :revision revision)))
	 (item-type (concat "\"item_type\":" item_type-topicmap ","))
	 (version (concat "\"version\":" (if prefixes-p "\"1.1\"" "\"1.0\"") ","))
	 (iis "\"item_identifiers\":null,")
	 (frag-reifier "\"reifier\":null"))
    (concat "{" version prefixes-value frag-tops frag-assocs item-type
	    iis frag-reifier "}")))


(defgeneric create-prefix-list-of-fragment (construct &key revision)
  (:documentation "Returns a list of the following structure:
                   ((:pref 'pref_1' :value 'uri-pref') (...)).")
  (:method ((construct FragmentC) &key (revision *TM-REVISION*))
    (declare (type (or Null Integer) revision))
    (create-prefix-list (append (list (topic construct))
				(referenced-topics construct))
			(associations construct) nil :revision revision)))


(defgeneric export-topics-to-jtm (topics &key prefixes parent-p revision)
  (:documentation "Exports a json array of topics serialised as JTM-role-objects.")
  (:method ((topics List) &key prefixes parent-p (revision *TM-REVISION*))
    (declare (List prefixes)
	     (Boolean parent-p)
	     (type (or Null Integer) revision))
      (if topics
	  (let ((result "["))
	    (loop for top in topics
	       do (push-string
		   (concat
		    (export-to-jtm top :item-type-p nil :prefixes prefixes
				   :parent-p parent-p :revision revision) ",")
		   result))
	    (concat (subseq result 0 (1- (length result))) "]"))
	  "null")))


(defgeneric export-associations-to-jtm (associations &key prefixes parent-p
						     revision)
  (:documentation "Exports a json array of topics serialised as JTM-role-objects.")
  (:method ((associations List) &key prefixes parent-p (revision *TM-REVISION*))
    (declare (List prefixes)
	     (Boolean parent-p)
	     (type (or Null Integer) revision))
      (if associations
	  (let ((result "["))
	    (loop for assoc in associations
	       do (push-string
		   (concat
		    (export-to-jtm assoc :item-type-p nil :prefixes prefixes
				   :parent-p parent-p :revision revision) ",")
		   result))
	    (concat (subseq result 0 (1- (length result))) "]"))
	  "null")))