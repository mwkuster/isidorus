;;+-----------------------------------------------------------------------------
;;+  Isidorus
;;+  (c) 2008-2010 Marc Kuester, Christoph Ludwig, Lukas Georgieff
;;+
;;+  Isidorus is freely distributable under the LLGPL license.
;;+  You can find a detailed description in trunk/docs/LLGPL-LICENSE.txt and
;;+  trunk/docs/LGPL-LICENSE.txt.
;;+-----------------------------------------------------------------------------


(in-package :jtm)

(defun get-item (item-keyword jtm-list)
  (declare (Keyword item-keyword)
	   (List jtm-list))
  (rest (find item-keyword jtm-list :key #'first)))


(defun make-prefix-list-from-jtm-list (jtm-list)
  "Creates a plist of the form ((:pref 'pref_1' :value 'value-1')
   (:pref 'pref_2' :value 'value-2')) if the passed jtm-list is
   of the form ((:PREF--1 . 'value-1')(:PREF--2 . 'value-2'))."
  (declare (List jtm-list))
  (loop for item in jtm-list
     collect (list :pref (json:lisp-to-camel-case
			  (subseq (write-to-string (first item)) 1))
		   :value (rest item))))


(defun import-construct-from-jtm-string (jtm-string &key
					 (revision *TM-REVISION*)
					 (jtm-format :1.1) tm-id)
  "Imports the passed jtm-string.
   Note tm-id needs not to be declared, but if the imported construct
   is a topicmap and it has no item-identifiers defined, a JTM-error
   is thrown."
  (declare (String jtm-string)
	   (type (or Null String) tm-id)
	   (Integer revision)
	   (Keyword jtm-format))
  (let* ((jtm-list (json:decode-json-from-string jtm-string))
	 (version (get-item :VERSION jtm-list))
	 (item_type (get-item :ITEM--TYPE jtm-list))
	 (prefixes (make-prefix-list-from-jtm-list (get-item :PREFIXES jtm-list)))
	 (format-1.1-p (eql jtm-format :1.1)))
    (cond ((eql jtm-format :1.0)
	   (unless (string= version "1.0")
	     (error (make-condition 'exceptions:JTM-error :message (format nil "From import-construct-from-jtm-string(): the member version must be set to \"1.0\" in JTM version 1.0, but is ~a" version))))
	   (when prefixes
	     (error (make-condition 'exceptions:JTM-error :message (format nil "From import-construct-from-jtm-string(): the member prefixes must not be set when using JTM version 1.0, but found: ~a" prefixes)))))
	  ((eql jtm-format :1.1)
	   (unless (string= version "1.1")
	     (error (make-condition 'exceptions:JTM-error :message (format nil "From import-construct-from-jtm-string(): the member version must be set to \"1.1\" in JTM version 1.1, but is ~a" version)))))
	  (t
	   (error (make-condition 'exceptions:JTM-error :message (format nil "From import-construct-from-jtm-string(): only JTM format \"1.0\" and \"1.1\" is supported, but found: \"~a\"" jtm-format)))))
    (cond ((or (not item_type)
	       (string= item_type item_type-topicmap))
	   (import-topic-map-from-jtm-list
	    jtm-list tm-id :revision revision :prefixes prefixes
	    :instance-of-p format-1.1-p))					   
	  ((string= item_type item_type-topic)
	   (import-topic-stub-from-jtm-list jtm-list nil :revision revision
					    :prefixes prefixes)
	   (merge-topic-from-jtm-list jtm-list nil :instance-of-p format-1.1-p
				      :revision revision :prefixes prefixes))
	  ((string= item_type item_type-name)
	   (import-name-from-jtm-list jtm-list nil :revision revision
				      :prefixes prefixes))
	  ((string= item_type item_type-variant)
	   (import-variant-from-jtm-list jtm-list nil :revision revision
					 :prefixes prefixes))
       	  ((string= item_type item_type-occurrence)
	   (import-occurrence-from-jtm-list jtm-list nil :revision revision
					    :prefixes prefixes))
	  ((string= item_type item_type-role)
	   (import-role-from-jtm-list jtm-list nil :revision revision
				      :prefixes prefixes))
	  ((string= item_type item_type-association)
	  (import-association-from-jtm-list jtm-list nil :revision revision
					    :prefixes prefixes))
	  (t
	   (error (make-condition 'exceptions:JTM-error :message (format nil "From import-construct-from-jtm-string(): the member \"item_type\" must be set to one of ~a or nil, but found \"~a\". If \"item_type\" is not specified or nil the JTM-data is treated as a topicmap." item_type (list item_type-topicmap item_type-topic item_type-name item_type-variant item_type-occurrence item_type-role item_type-association))))))))


(defun import-from-jtm (jtm-path repository-path &key (tm-id (error "you must provide a stable identifier (PSI-style) for this TM")) (revision *TM-REVISION*) (jtm-format :1.1))
  "Imports the given jtm-file by calling import-construct-from-jtm-string."
  (declare (type (or Pathname String) jtm-path repository-path)
	   (String tm-id)
	   (Keyword jtm-format)
	   (Integer revision))
  (open-tm-store repository-path)
  (import-construct-from-jtm-string (read-file-to-string jtm-path)
				    :tm-id tm-id :revision revision
				    :jtm-format jtm-format)
  (close-tm-store))


(defun import-topic-map-from-jtm-list (jtm-list tm-id &key (revision *TM-REVISION*)
				       prefixes (instance-of-p t))
  "Creates and returns a topic map corresponding to the tm-id or a given
   item-identifier in the jtm-list and returns the tm construct after all
   topics and associations contained in the jtm-list has been created."
  (declare (List jtm-list prefixes)
	   (Integer revision)
	   (Boolean instance-of-p))
  (let* ((iis (let ((value (append (import-identifiers-from-jtm-strings
				    (get-item :ITEM--IDENTIFIERS jtm-list)
				    :prefixes prefixes)
				   (when tm-id
				     (list
				      (make-construct 'ItemIdentifierC
						      :uri tm-id))))))
		(unless value
		  (error (make-condition 'JTM-error :message (format nil "From import-topic-map-from-jtm-list(): no topic-map item-identifier is set for ~a" jtm-list))))
		value))
	 (j-tops (get-item :TOPICS jtm-list))
	 (j-assocs (get-item :ASSOCIATIONS jtm-list))
	 (tm (make-construct 'TopicMapC :start-revision revision
			     :item-identifiers iis)))
    (import-topic-stubs-from-jtm-lists j-tops (list tm) :revision revision
				       :prefixes prefixes)
    (merge-topics-from-jtm-lists j-tops (list tm) :instance-of-p instance-of-p
				 :revision revision :prefixes prefixes)
    (import-associations-from-jtm-lists j-assocs (list tm) :revision revision
					:prefixes prefixes)
    tm))


(defun import-associations-from-jtm-lists (jtm-lists parents &key
					   (revision *TM-REVISION*) prefixes)
  "Create a listof AssociationC objects corresponding to the passed jtm-lists
    and returns it."
  (declare (List jtm-lists parents prefixes)
	   (Integer revision))
  (map 'list #'(lambda(jtm-list)
		 (import-association-from-jtm-list
		  jtm-list parents :revision revision :prefixes prefixes))
       jtm-lists))


(defun import-role-from-jtm-list (jtm-list parent &key (revision *TM-REVISION*)
				  prefixes)
  "Creates and returns a role object form the given jtm-list."
    (let* ((iis (import-identifiers-from-jtm-strings
	       (get-item :ITEM--IDENTIFIERS jtm-list)
	       :prefixes prefixes))
	   (type (get-item :TYPE jtm-list))
	   (reifier (get-item :REIFIER jtm-list))
	   (player (get-item :PLAYER jtm-list))
	   (parent-references (get-item :PARENT jtm-list))
	   (local-parent
	    (if parent
		(list parent)
		(when parent-references
		  (get-items-from-jtm-references
		   parent-references :revision revision :prefixes prefixes)))))
      (unless local-parent
	(error (make-condition 'JTM-error :message (format nil "From import-role-from-jtm-list(): the JTM role ~a must have exactly one parent set in its members." jtm-list))))
      (unless type
	(error (make-condition 'JTM-error :message (format nil "From import-role-from-jtm-list(): the role ~a must have exactly one type set as member." jtm-list))))
      (unless player
	(error (make-condition 'JTM-error :message (format nil "From import-role-from-jtm-list(): the role ~a must have exactly one player set as member." jtm-list))))
      (make-construct 'RoleC :start-revision revision
		      :item-identifiers iis
		      :reifier (when reifier
				 (get-item-from-jtm-reference
				  reifier :revision revision :prefixes prefixes))
		      :instance-of (get-item-from-jtm-reference
				    type :revision revision :prefixes prefixes)
		      :player (get-item-from-jtm-reference
			       player :revision revision :prefixes prefixes)
		      :parent (first local-parent))))


(defun make-plist-of-jtm-role(jtm-list &key (revision *TM-REVISION*) prefixes)
  "Returns a plist of the form (:start-revision <rev> :player <top>
   :instance-of <top> :reifier <top> :item-identifiers <ii>)."
  (unless (and (get-item :PLAYER jtm-list)
	       (get-item :TYPE jtm-list))
    (error (make-condition 'JTM-error :message (format nil "From make-plist-of-jtm-role(): the role ~a must have a type and player member set." jtm-list))))
  (list :start-revision revision
	:player (get-item-from-jtm-reference
		 (get-item :PLAYER jtm-list)
		 :revision revision :prefixes prefixes)
	:instance-of (get-item-from-jtm-reference
		      (get-item :TYPE jtm-list)
		      :revision revision :prefixes prefixes)
	:item-identifiers (import-identifiers-from-jtm-strings
			   (get-item :ITEM--IDENTIFIERS jtm-list)
			   :prefixes prefixes)
	:reifier (when (get-item :REIFIER jtm-list)
		   (get-item-from-jtm-reference
		    (get-item :REIFIER jtm-list)
		    :revision revision :prefixes prefixes))))


(defun import-association-from-jtm-list (jtm-list parents &key
					 (revision *TM-REVISION*) prefixes)
  "Create an AssociationC object corresponding to the passed jtm-list and
   returns it."
  (declare (List jtm-list parents prefixes)
	   (Integer revision))
  (let* ((iis (import-identifiers-from-jtm-strings
	       (get-item :ITEM--IDENTIFIERS jtm-list)
	       :prefixes prefixes))
	 (scope (get-item :SCOPE jtm-list))
	 (type (get-item :TYPE jtm-list))
	 (reifier (get-item :REIFIER jtm-list))
	 (parent-references (get-item :PARENT jtm-list))
	 (role-lists
	  (map 'list #'(lambda(role)
			 (make-plist-of-jtm-role role :revision revision
						 :prefixes prefixes))
	       (get-item :ROLES jtm-list)))
	 (local-parent
	  (if parents
	      parents
	      (when parent-references
		(get-items-from-jtm-references
		 parent-references :revision revision :prefixes prefixes)))))
    (unless local-parent
      (error (make-condition 'JTM-error :message (format nil "From import-association-from-jtm-list(): the JTM association ~a must have at least one parent set in its members." jtm-list))))
    (unless role-lists
      (error (make-condition 'JTM-error :message (format nil "From import-association-from-jtm-list(): the JTM association ~a must have at least one role set in its members." jtm-list))))
    (unless type
      (error (make-condition 'JTM-error :message (format nil "From import-association-from-jtm-list(): the association ~a must have exactly one type set as member." jtm-list))))
    (let ((assoc
	   (make-construct 'AssociationC :start-revision revision
			   :item-identifiers iis
			   :themes (get-items-from-jtm-references
				    scope :revision revision :prefixes prefixes)
			   :reifier (when reifier
				      (get-item-from-jtm-reference
				       reifier :revision revision :prefixes prefixes))
			   :instance-of (get-item-from-jtm-reference
					 type :revision revision :prefixes prefixes)
			   :roles role-lists)))
      (dolist (tm local-parent)
	(add-to-tm tm assoc))
      (format t "a")
      assoc)))


(defun import-topic-stubs-from-jtm-lists (jtm-lists parents &key
					  (revision *TM-REVISION*) prefixes)
  "Creates and returns a list of topics.
   Note only the topic identifiers are imported and set in this function,
   entire topics are imported in merge-topics-from-jtm-lists."
  (declare (List jtm-lists parents prefixes)
	   (Integer revision))
  (map 'list #'(lambda(jtm-list)
		 (import-topic-stub-from-jtm-list
		  jtm-list parents :revision revision :prefixes prefixes))
       jtm-lists))


(defun import-topic-stub-from-jtm-list(jtm-list parents &key
				       (revision *TM-REVISION*) prefixes)
  "Creates and returns a topic object from the passed jtm
   list generated by json:decode-json-from-string.
   Note this function only sets the topic's identifiers."
  (declare (List jtm-list parents prefixes)
	   (Integer revision))
  (let* ((t-iis (import-identifiers-from-jtm-strings
		 (get-item :ITEM--IDENTIFIERS jtm-list)
		 :prefixes prefixes))
	 (t-psis (import-identifiers-from-jtm-strings
		  (get-item :SUBJECT--IDENTIFIERS jtm-list)
		  :prefixes prefixes :identifier-type-symbol 'd:PersistentIdC))
	 (t-sls (import-identifiers-from-jtm-strings
		 (get-item :SUBJECT--LOCATORS jtm-list)
		 :prefixes prefixes :identifier-type-symbol 'd:SubjectLocatorC))
	 (parent-references (get-item :PARENT jtm-list))
	 (local-parents
	  (if parents
	      parents
	      (when parent-references
		(get-items-from-jtm-references
		 parent-references :revision revision :prefixes prefixes)))))
    (unless local-parents
      (error (make-condition 'JTM-error :message (format nil "From import-topic-from-jtm-string(): the JTM topic ~a must have at least one parent set in its members." jtm-list))))
    (unless (append t-iis t-sls t-psis)
      (error (make-condition 'JTM-error :message (format nil "From import-topic-from-jtm-string(): the JTM topic ~a must have at least one identifier set in its members." jtm-list))))
    (let* ((top (make-construct 'TopicC :start-revision revision
				:psis t-psis
				:item-identifiers t-iis
				:locators t-sls)))
      (dolist (tm local-parents)
	(add-to-tm tm top))
      top)))


(defun make-instance-of-association (instance-top type-top parents &key
				     (revision *TM-REVISION*))
  "Creates and returns a type-instance-association for the passed
   instance and type topics."
  (declare (TopicC instance-top type-top)
	   (List parents)
	   (Integer revision))
  (unless parents
    (error (make-condition 'JTM-error :message (format nil "From make-instance-of-association(): parents must contain at least one TopicMapC object, but is nil"))))
  (let ((t-top (get-item-by-psi *type-psi* :revision revision))
	(i-top (get-item-by-psi *instance-psi* :revision revision))
	(ti-top (get-item-by-psi *type-instance-psi* :revision revision)))
    (unless (and i-top t-top ti-top)
      (let ((missing-topic (cond ((not t-top) *type-psi*)
				 ((not i-top) *instance-psi*)
				 (t *type-instance-psi*))))
	(error (make-condition 'missing-reference-error :message (format nil "From make-instance-of-association(): the core topics ~a, ~a, and ~a are necessary, but ~a cannot be found" *type-psi* *instance-psi* *type-instance-psi* missing-topic) :reference missing-topic))))
    (let ((assoc
	   (make-construct 'AssociationC :start-revision revision
			   :instance-of ti-top
			   :roles (list (list :start-revision revision
					      :player instance-top
					      :instance-of i-top)
					(list :start-revision revision
					      :player type-top
					      :instance-of t-top)))))
      (dolist (tm parents)
	(add-to-tm tm i-top)
	(add-to-tm tm t-top)
	(add-to-tm tm ti-top)
	(add-to-tm tm assoc))
      assoc)))


(defun merge-topics-from-jtm-lists (jtm-lists parents &key (instance-of-p t)
				    (revision *TM-REVISION*) prefixes)
  "Creates and returns a list of topics."
  (declare (List jtm-lists parents prefixes)
	   (Boolean instance-of-p)
	   (Integer revision))
  (map 'list #'(lambda(jtm-list)
		 (merge-topic-from-jtm-list
		  jtm-list parents :revision revision :prefixes prefixes
		  :instance-of-p instance-of-p))
       jtm-lists))


(defun merge-topic-from-jtm-list(jtm-list parents &key (instance-of-p t)
				  (revision *TM-REVISION*) prefixes)
  "Creates and returns a topic object from the passed jtm
   list generated by json:decode-json-from-string.
   Note that the merged topics are not added explicitly to the parent
   topic maps, it is only needed for the instance-of-associations -
   topics are added in the function import-topic-stubs-from-jtm-lists
   to their topic map elements."
  (declare (List jtm-list prefixes parents)
	   (Boolean instance-of-p)
	   (Integer revision))
  (let* ((ids (append (get-item :ITEM--IDENTIFIERS jtm-list)
		      (get-item :SUBJECT--IDENTIFIERS jtm-list)
		      (get-item :SUBJECT--LOCATORS jtm-list)))
	 (top (when ids
		(get-item-by-any-id
		 (compute-uri-from-jtm-identifier (first ids) prefixes)
		 :revision revision)))
	 (instanceof (get-items-from-jtm-references
		      (get-item :INSTANCE--OF jtm-list) :revision revision
		      :prefixes prefixes))
	 (top-names (import-characteristics-from-jtm-lists
		     (get-item :NAMES jtm-list) top
		     #'import-name-from-jtm-list :revision revision
		     :prefixes prefixes))
	 (top-occs (import-characteristics-from-jtm-lists
		    (get-item :OCCURRENCES jtm-list) top
		    #'import-occurrence-from-jtm-list :revision revision
		    :prefixes prefixes)))
    (unless ids
      (error (make-condition 'JTM-error :message (format nil "From merge-topic-from-jtm-list(): the passed topic has to own at least one identifier: ~a" jtm-list))))
    (unless top
      (error (make-condition 'JTM-error :message (format nil "From merge-topic-from-jtm-list(): cannot find a topic that matches the corresponding JTM-list: ~a" jtm-list))))
    (when (and (not instance-of-p) instanceof)
      (error (make-condition 'JTM-error :message (format nil "From merge-topic-from-jtm-list(): the JTM-topic has an instance_of member set, but JTM version 1.0 does not allow an intance_of member within a topic object: ~a" jtm-list))))
    (dolist (type-top instanceof)
      (make-instance-of-association top type-top parents :revision revision))
    (dolist (name top-names)
      (add-name top name :revision revision))
    (dolist (occ top-occs)
      (add-occurrence top occ :revision revision))
    (format t "t")
    top))


(defun import-name-from-jtm-list (jtm-list parent &key
				  (revision *TM-REVISION*) prefixes)
  "Creates and returns a name object from the passed jtm
   list generated by json:decode-json-from-string."
  (declare (List jtm-list prefixes)
	   (Integer revision)
	   (type (or Null TopicC) parent))
  (let* ((iis (import-identifiers-from-jtm-strings
	       (get-item :ITEM--IDENTIFIERS jtm-list)
	       :prefixes prefixes))
	 (scope (get-item :SCOPE jtm-list))
	 (type (get-item :TYPE jtm-list))
	 (value (get-item :VALUE jtm-list))
	 (name-variants (get-item :VARIANTS jtm-list))
	 (reifier (get-item :REIFIER jtm-list))
	 (parent-references (get-item :PARENT jtm-list))
	 (local-parent
	  (if parent
	      (list parent)
	      (when parent-references
		(get-items-from-jtm-references
		 parent-references :revision revision :prefixes prefixes)))))
    (when (/= (length local-parent) 1)
      (error (make-condition 'JTM-error :message (format nil "From import-name-from-jtm-list(): the JTM name ~a must have exactly one parent set in its members." jtm-list))))
    (let ((name
	   (make-construct
	    'NameC :start-revision revision
	    :item-identifiers iis
	    :charvalue value
	    :themes (get-items-from-jtm-references
		     scope :revision revision :prefixes prefixes)
	    :instance-of (if type
			     (get-item-from-jtm-reference
			      type :revision revision :prefixes prefixes)
			     (get-item-by-psi *topic-name-psi*
					      :revision revision :error-if-nil t))
	    :parent (first local-parent)
	    :reifier (when reifier
		       (get-item-from-jtm-reference
			reifier :revision revision :prefixes prefixes)))))
      (import-characteristics-from-jtm-lists name-variants name
					     #'import-variant-from-jtm-list
					     :revision revision :prefixes prefixes)
      name)))


(defun import-occurrence-from-jtm-list (jtm-list parent &key
					(revision *TM-REVISION*) prefixes)
  "Creates and returns an occurrence object from the passed jtm
   list generated by json:decode-json-from-string."
  (declare (List jtm-list prefixes)
	   (Integer revision)
	   (type (or Null TopicC) parent))
  (let* ((iis (import-identifiers-from-jtm-strings
	       (get-item :ITEM--IDENTIFIERS jtm-list)
	       :prefixes prefixes))
	 (datatype (get-item :DATATYPE jtm-list))
	 (scope (get-item :SCOPE jtm-list))
	 (type (get-item :TYPE jtm-list))
	 (value (get-item :VALUE jtm-list))
	 (reifier (get-item :REIFIER jtm-list))
	 (parent-references (get-item :PARENT jtm-list))
	 (local-parent
	  (if parent
	      (list parent)
	      (when parent-references
		(get-items-from-jtm-references
		 parent-references :revision revision :prefixes prefixes)))))
    (when (/= (length local-parent) 1)
      (error (make-condition 'JTM-error :message (format nil "From import-occurrence-from-jtm-list(): the JTM occurrence ~a must have a parent set in its members." jtm-list))))
    (unless type
      (error (make-condition 'JTM-error :message (format nil "From import-occurrence-from-jtm-list(): the JTM occurrence ~a must have a type set in its members." jtm-list))))
    (make-construct 'OccurrenceC :start-revision revision
		    :item-identifiers iis
		    :datatype (if datatype datatype *xml-string*)
		    :charvalue value
		    :themes (get-items-from-jtm-references
			     scope :revision revision :prefixes prefixes)
		    :instance-of (get-item-from-jtm-reference
				  type :revision revision :prefixes prefixes)
		    :parent (first local-parent)
		    :reifier (when reifier
			       (get-item-from-jtm-reference
				reifier :revision revision :prefixes prefixes)))))


(defun import-characteristics-from-jtm-lists(jtm-lists parent next-fun &key
					     (revision *TM-REVISION*) prefixes)
  "Creates and returns a list of TM-Constructs returned by next-fun."
  (declare (List jtm-lists prefixes)
	   (Integer revision)
	   (type (or Null ReifiableConstructC) parent)
	   (Function next-fun))
  (map 'list #'(lambda(jtm-list)
		 (apply next-fun (list jtm-list parent :revision revision
				       :prefixes prefixes)))
       jtm-lists))


(defun import-variant-from-jtm-list(jtm-list parent &key
				      (revision *TM-REVISION*) prefixes)
  "Creates a variant object from the passed jtm list generated by
   json:decode-json-from-string."
  (declare (List jtm-list prefixes)
	   (type (or Null NameC) parent)
	   (Integer revision))
  (let* ((iis (import-identifiers-from-jtm-strings
	       (get-item :ITEM--IDENTIFIERS jtm-list)
	       :prefixes prefixes))
	 (datatype (get-item :DATATYPE jtm-list))
	 (value (get-item :VALUE jtm-list))
	 (reifier (get-item :REIFIER jtm-list))
	 (parent-references (get-item :PARENT jtm-list))
	 (local-parent
	  (if parent
	      (list parent)
	      (when parent-references
		(get-items-from-jtm-references
		 parent-references :revision revision :prefixes prefixes))))
	 (scopes (when local-parent
		   (remove-duplicates
		    (append
		     (get-items-from-jtm-references
		      (get-item :SCOPE jtm-list)
		      :revision revision :prefixes prefixes)
		     (themes (first local-parent) :revision revision))))))
    (when (/= (length local-parent) 1)
      (error (make-condition 'JTM-error :message (format nil "From import-variant-from-jtm-list(): the JTM variant ~a must have exactly one parent set in its members." jtm-list))))
    (make-construct 'VariantC :start-revision revision
		    :item-identifiers iis
		    :datatype (if datatype datatype *xml-string*)
		    :charvalue value
		    :themes scopes
		    :parent (first local-parent)
		    :reifier (when reifier
			       (get-item-from-jtm-reference
				reifier :revision revision :prefixes prefixes)))))


(defun import-identifiers-from-jtm-strings
    (jtm-strings  &key (identifier-type-symbol 'ItemIdentifierC) prefixes)
  "Creates and returns a list of identifiers specified by jtm-strings and
   identifier-type-symbol."
  (declare (List jtm-strings)
	   (Symbol identifier-type-symbol)
	   (List prefixes))
  (map 'list #'(lambda(jtm-string)
		 (import-identifier-from-jtm-string
		  jtm-string :prefixes prefixes
		  :identifier-type-symbol identifier-type-symbol))
       jtm-strings))


(defun import-identifier-from-jtm-string
    (jtm-string &key (identifier-type-symbol 'ItemIdentifierC) prefixes)
  "Creates and returns an identifier of the type specified by
   identifier-type-symbol."
  (declare (String jtm-string)
	   (Symbol identifier-type-symbol)
	   (List prefixes))
  (let ((uri-value (compute-uri-from-jtm-identifier jtm-string prefixes)))
    (make-construct identifier-type-symbol
		    :uri uri-value)))
  


(defun get-item-from-jtm-reference (reference-string &key (revision *TM-REVISION*)
				    prefixes)
  "Returns a ReifiableConstructC that is bound to the reference that is
   passed to this function. If the construct cannot be found the error
   tm-reference-error is thrown."
  (declare (Integer revision)
	   (List prefixes)
	   (String reference-string))
  (let* ((identifier-type
	  (get-identifier-type-from-jtm-reference reference-string))
	 (identifier-value (subseq reference-string 3))
	 (identifier-uri
	  (compute-uri-from-jtm-identifier identifier-value prefixes))
	 (construct
	  (d::get-item-by-identifier identifier-uri :revision revision
				     :identifier-type-symbol identifier-type)))
    (if construct
	construct
	(error (make-condition 'missing-reference-error :message (format nil "From get-item-from-jtm-reference(): cannot find the item identified by \"~a\"(~a)" identifier-uri reference-string)
			       :reference identifier-uri)))))	


(defun get-items-from-jtm-references (reference-strings &key (revision *TM-REVISion*)
				      prefixes)
  "Returns a list of ReifiableConstructCs that are referenced via the
   string-values in reference-strings."
  (declare (List reference-strings prefixes)
	   (Integer revision))
  (map 'list #'(lambda(reference-string)
		 (get-item-from-jtm-reference reference-string :revision revision
					      :prefixes prefixes))
       reference-strings))


(defun compute-uri-from-jtm-identifier (identifier-value prefixes)
  "Returns the full uri of an identifier string, i.e.
   * if the value is of the form '[pref:value]' the return value is
     the concatenation of 'value-of-pref' and 'value'.
   * if the value is of the form 'full-uri' the return value is
     'full-uri'"
  (declare (String identifier-value)
	   (List prefixes))
  (cond ((and (string-starts-with identifier-value "[")
	      (string-ends-with identifier-value "]"))
	 (let* ((pref-name
		 (let ((value (string-until identifier-value ":")))
		   (when value
		     (subseq value 1))))
		(suffix
		 (when pref-name
		   (let ((value
			  (subseq identifier-value (1+ (length pref-name)))))
		     (when value
		       (subseq value (min 1 (length value))
			       (max 0 (1- (length value)))))))))
	   (when (or (not pref-name) (not suffix))
	     (error (make-condition 'JTM-error :message (format nil "From compute-uri-from-jtm-identifier: the section within the range of \"[\" and \"]\" must be of the form prefix:suffix, but is: \"~a\"" identifier-value))))
	   (compute-full-uri prefixes pref-name suffix)))
	((> (length identifier-value) 0)
	 identifier-value)
	(t
	 (error (make-condition 'JTM-error :message (format nil "From compute-uri-from-jtm-identifier(): the identifier-value must be of the form \"[pref:value]\" or \"full-uri\", but is: \"~a\"" identifier-value))))))


(defun get-identifier-type-from-jtm-reference (identifier-string)
  "Returns the symbol 'PersistentIdC if identifier-string starts
   with si:, 'SubjectLocatorC if identifier-string starts with
   sl:, or 'ItemIdentifierC if identifier-string starts with ii:.
   If identifier-string do not start with one of these strings
   the error JTM-error is thrown."
  (cond ((string-starts-with identifier-string "ii:")
	 'ItemIdentifierC)
	((string-starts-with identifier-string "si:")
	 'PersistentIdC)
	((string-starts-with identifier-string "sl:")
	 'SubjectLocatorC)
	(t
	 (error (make-condition 'JTM-error :message (format nil "From get-identifier-type(): the identifier value must start with one of \"ii:\", \"si:\", or \"sl:\", but is: \"~a\"" identifier-string))))))