;;+-----------------------------------------------------------------------------
;;+  Isidorus
;;+  (c) 2008-2010 Marc Kuester, Christoph Ludwig, Lukas Georgieff
;;+
;;+  Isidorus is freely distributable under the LLGPL license.
;;+  You can find a detailed description in trunk/docs/LLGPL-LICENSE.txt and
;;+  trunk/docs/LGPL-LICENSE.txt.
;;+-----------------------------------------------------------------------------

(defpackage :datamodel
  (:use :cl :elephant :constants :base-tools)
  (:nicknames :d)
  (:import-from :exceptions
		duplicate-identifier-error
		object-not-found-error
		missing-argument-error
		not-mergable-error
		tm-reference-error
		bad-type-error)
  (:import-from :constants
		*xml-string*
		*instance-psi*)
  (:export ;;classes
           :TopicMapConstructC
	   :VersionedConstructC
	   :ReifiableConstructC
	   :ScopableC
	   :TypableC
           :TopicMapC
           :AssociationC
           :RoleC
	   :CharacteristicC
           :OccurrenceC
	   :NameC
	   :VariantC
	   :PointerC
	   :IdentifierC
           :PersistentIdC
	   :ItemIdentifierC
	   :SubjectLocatorC
	   :TopicIdentificationC
	   :TopicC
	   :FragmentC

	   ;;methods, functions and macros
	   :xtm-id
	   :uri
	   :identified-construct
	   :item-identifiers
	   :add-item-identifier
	   :delete-item-identifier
	   :reifier
	   :add-reifier
	   :delete-reifier
	   :find-item-by-revision
	   :find-most-recent-revision
	   :themes
	   :add-theme
	   :delete-theme
	   :instance-of
	   :add-type
	   :delete-type
	   :parent
	   :add-parent
	   :delete-parent
	   :variants
	   :add-variant
	   :delete-variant
	   :player
	   :add-player
	   :delete-player
	   :roles
	   :add-role
	   :delete-role
	   :associations
	   :topics
	   :add-to-tm
	   :delete-from-tm
	   :psis
	   :add-psi
	   :delete-psi
	   :topic-identifiers
	   :add-topic-identifier
	   :delete-topic-identifier
	   :topic-id
	   :locators
	   :add-locator
	   :delete-locator
	   :names
	   :add-name
	   :delete-name
	   :occurrences
	   :add-occurrence
	   :delete-occurrence
	   :player-in-roles
	   :used-as-type
	   :used-as-theme
	   :datatype
	   :charvalue
	   :reified-construct
	   :mark-as-deleted
	   :marked-as-deleted-p
	   :in-topicmaps
	   :delete-construct
	   :get-revision
	   :get-item-by-id
	   :get-item-by-psi
	   :get-item-by-item-identifier
	   :get-item-by-locator
	   :get-item-by-content
	   :string-integer-p
	   :with-revision
	   :get-latest-fragment-of-topic
	   :create-latest-fragment-of-topic
	   :PointerC-p
	   :IdentifierC-p
	   :SubjectLocatorC-p
	   :PersistentIdC-p
	   :ItemIdentifierC-p
	   :TopicIdentificationC-p
	   :CharacteristicC-p
	   :OccurrenceC-p
	   :NameC-p
	   :VariantC-p
	   :ScopableC-p
	   :TypableC-p
	   :TopicC-p
	   :AssociationC-p
	   :RoleC-p
	   :TopicMapC-p
	   :ReifiableConstructC-p
	   :TopicMapConstructC-p
	   :VersionedConstructC-p
	   :make-construct
	   :list-instanceOf
	   :list-super-types
	   :in-topicmap
	   :get-fragments
	   :get-fragment
	   :get-all-revisions
	   :unique-id
	   :topic
	   :referenced-topics
	   :revision
	   :get-all-revisions-for-tm
	   :add-source-locator
	   :changed-p
	   :check-for-duplicate-identifiers
	   :find-item-by-content
	   :rec-remf
	   :get-all-topics
	   :get-all-associations
	   :get-all-tms

	   ;;globals
	   :*TM-REVISION*
	   :*CURRENT-XTM*
	   
	   ;;trivial-queries
	   :roles-by-type
	   :roles-by-player
	   :filter-associations-by-type
	   :filter-associations-by-role
	   :associations-of
	   :instance-of-associations
	   :supertype-associations
	   :direct-supertypes
	   :supertypes
	   :direct-instance-of
	   :invoke-on
	   :names-by-type
	   :occurrences-by-type
	   :characteristics-by-type
	   :occurrences-by-value
	   :names-by-value
	   :characteristics-by-value
	   :isa
	   :aka))

(in-package :datamodel)


;;TODO: implement a macro with-merge-constructs, that merges constructs
;;      after all operations in the body were called



;;; globals ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *TM-REVISION* 0)


(defparameter *CURRENT-XTM* nil "Represents the currently active TM.")


;;; classes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; versioning
(defpclass VersionInfoC()
  ((start-revision :initarg :start-revision
		   :accessor start-revision
		   :type integer
		   :initform 0
		   :documentation "The start-revision of the version's
                                   interval of a versioned object.")
   (end-revision :initarg :end-revision
		 :accessor end-revision
		 :type integer
		 :initform 0
		 :documentation "The end-revision of the version's interval
                                 of a versioned object.")
   (versioned-construct :initarg :versioned-construct
			:accessor versioned-construct
			:associate VersionedConstructC
			:documentation "The reference of the versioned
                                        object that is described by this
                                        VersionInfoC-object."))
  (:documentation "A VersionInfoC-object describes the revision information
                   of a versioned object in intervals starting by the value
                   start-revision and ending by the value end-revision - 1.
                   end-revision=0 means always the latest version."))


(defpclass VersionedConstructC()
  ((versions :initarg :versions
	     :accessor versions
	     :inherit t
	     :associate (VersionInfoC versioned-construct)
	     :documentation "Version infos for former versions of this base
                             class.")))


;;; base classes ...
(defpclass TopicMapConstructC()
  ()
  (:documentation "An abstract base class for all classes that describes
                   Topic Maps data."))


(defpclass ScopableC()
  ((themes :associate (ScopeAssociationC scopable-construct)
	   :inherit t
	   :documentation "Contains all association-objects that contain the
                           actual scope-topics."))
  (:documentation "An abstract base class for all constructs that are scoped."))


(defpclass TypableC()
  ((instance-of :associate (TypeAssociationC typable-construct)
		:inherit t
		:documentation "Contains all association-objects that contain
                                the actual type-topic."))
  (:documentation "An abstract base class for all typed constructcs."))


(defpclass DatatypableC()
  ((datatype :accessor datatype
             :initarg :datatype
             :initform constants:*xml-string*
	     :type string
	     :index t
             :documentation "The XML Schema datatype of the occurrencevalue
                             (optional, always IRI for resourceRef)."))
  (:documentation "An abstract base class for characteristics that own
                   an xml-datatype."))


;;; pointers ...
(defpclass PointerC(TopicMapConstructC)
  ((uri :initarg :uri
	:accessor uri
	:inherit t
	:type string
	:initform (error (make-missing-argument-condition "From PointerC(): uri must be set for a pointer" 'uri ':uri))
	:index t
	:documentation "The actual value of a pointer, i.e. uri or ID.")
   (identified-construct :associate (PointerAssociationC identifier)
			 :inherit t
			 :documentation "Associates a association-object that
                                         additionally stores some
                                         version-infos."))
  (:documentation "An abstract base class for all pointers."))


(defpclass IdentifierC(PointerC)
  ()
  (:documentation "An abstract base class for all TM-Identifiers."))


(defpclass TopicIdentificationC(PointerC)
  ((xtm-id :initarg :xtm-id
	   :accessor xtm-id
	   :type string
	   :initform (error (make-missing-argument-condition "From TopicIdentificationC(): xtm-id must be seet for a topic-identifier" 'xtm-id ':xtm-id))
	   :index t
	   :documentation "ID of the TM this identification came from."))
  (:index t)
  (:documentation "Identify topic items through generalized topic-ids.
                   A topic may have many original topicids, the class
                   representing one of them."))


(defpclass SubjectLocatorC(IdentifierC)
  ()
  (:index t)
  (:documentation "A subject-locator that contains an uri-value and an
                   association to SubjectLocatorAssociationC's which are in
                   turn associated with TopicC's."))


(defpclass PersistentIdC(IdentifierC)
  ()
  (:index t)
  (:documentation "A subject-identifier that contains an uri-value and an
                   association to PersistentIdAssociationC's which are in
                   turn associated with TopicC's."))


(defpclass ItemIdentifierC(IdentifierC)
  ()
  (:index t)
  (:documentation "An item-identifier that contains an uri-value and an
                   association to ItemIdAssociationC's which are in turn
                   associated with RiefiableConstructC's."))


;;; reifiables ...
(defpclass ReifiableConstructC(TopicMapConstructC)
  ((item-identifiers :associate (ItemIdAssociationC parent-construct)
		     :inherit t
		     :documentation "A relation to all item-identifiers of
                                     this construct.")
   (reifier :associate (ReifierAssociationC reifiable-construct)
	    :inherit t
	    :documentation "A relation to a reifier-topic."))
  (:documentation "Reifiable constructs as per TMDM."))


(defpclass AssociationC(ReifiableConstructC ScopableC TypableC
					    VersionedConstructC)
  ((roles :associate (RoleAssociationC parent-construct)
	  :documentation "Contains all association-objects of all roles this
                          association contains.")
   (in-topicmaps :associate (TopicMapC associations)
		 :many-to-many t
		 :documentation "List of all topic maps this association is
                                 part of"))
  (:index t)
  (:documentation "Association in a Topic Map"))


(defpclass RoleC(ReifiableConstructC TypableC)
  ((parent :associate (RoleAssociationC role)
	   :documentation "Associates this object with a role-association.")
   (player :associate (PlayerAssociationC parent-construct)
	   :documentation "Associates this object with a player-association.")))


(elephant:defpclass TopicMapC (ReifiableConstructC VersionedConstructC)
  ((topics :associate (TopicC in-topicmaps)
	   :many-to-many t
	   :accessor topics
	   :documentation "List of topics that explicitly belong to this TM.")
   (associations :associate (AssociationC in-topicmaps)
		 :many-to-many t
		 :accessor associations
                 :documentation "List of associations that belong to this TM."))
  (:documentation "Represnets a topic map."))


(defpclass TopicC (ReifiableConstructC VersionedConstructC)
  ((topic-identifiers :associate (TopicIdAssociationC parent-construct)
		      :documentation "Contains all association objects that
                                      relate a topic with its actual
                                      topic-identifiers.")
   (psis :associate (PersistentIdAssociationC parent-construct)
	 :documentation "Contains all association objects that relate a topic
                         with its actual psis.")
   (locators :associate (SubjectLocatorAssociationC parent-construct)
	     :documentation "Contains all association objects that relate a
                             topic with its actual subject-lcoators.")
   (names :associate (NameAssociationC parent-construct)
	  :documentation "Contains all association objects that relate a topic
                          with its actual names.")
   (occurrences :associate (OccurrenceAssociationC parent-construct)
		:documentation "Contains all association objects that relate a
                                topic with its actual occurrences.")
   (player-in-roles :associate (PlayerAssociationC player-topic)
		    :documentation "Contains all association objects that relate
                                    a topic that is a player with its role.")
   (used-as-type :associate (TypeAssociationC type-topic)
		 :documentation "Contains all association objects that relate a
                                 topic that is a type with its typable obejct.")
   (used-as-theme :associate (ScopeAssociationC theme-topic)
		  :documentation "Contains all association objects that relate a
                                  topic that is a theme with its scoppable
                                  object.")
   (reified-construct :associate (ReifierAssociationC reifier-topic)
		      :documentation "Contains all association objects that
                                      relate a topic that is a reifier with
                                      its reified object.")
   (in-topicmaps :associate (TopicMapC topics)
		 :many-to-many t
		 :documentation "List of all topic maps this topic is part of."))
  (:index t)
  (:documentation "Represents a TM topic."))



;;; characteristics ...
(defpclass CharacteristicC(ReifiableConstructC ScopableC TypableC)
  ((parent :associate (CharacteristicAssociationC characteristic)
	   :inherit t
	   :documentation "Assocates the characterist obejct with the
                           parent-association.")
   (charvalue :initarg :charvalue
	      :accessor charvalue
	      :type string
	      :inherit t
	      :initform ""
	      :index t
	      :documentation "Contains the actual data of this object."))
  (:documentation "Scoped characteristic of a topic (meant to be used
                   as an abstract class)."))


(defpclass OccurrenceC(CharacteristicC DatatypableC)
  ()
  (:documentation "Represents a TM occurrence."))


(defpclass NameC(CharacteristicC)
  ((variants :associate (VariantAssociationC parent-construct)
	     :documentation "Associates this obejct with varian-associations."))
  (:documentation "Scoped name of a topic."))


(defpclass VariantC(CharacteristicC DatatypableC)
  ()
  (:documentation "Represents a TM variant."))


;;; versioned associations ...
(defpclass VersionedAssociationC(VersionedConstructC)
  ()
  (:documentation "An abstract base class for all versioned associations."))


(defpclass TypeAssociationC(VersionedAssociationC)
  ((type-topic :initarg :type-topic
	       :accessor type-topic
	       :initform (error (make-missing-argument-condition "From TypeAssociationC(): type-topic must be set" 'type-topic ':type-topic))
	       :associate TopicC
	       :documentation "Associates this object with a topic that is used
                               as type.")
   (typable-construct :initarg :typable-construct
		      :accessor typable-construct
		      :initform (error (make-missing-argument-condition	"From TypeAssociationC(): typable-construct must be set" 'typable-construct ':typable-construct))
		      :associate TypableC
		      :documentation "Associates this object with the typable
                                      construct that is typed by the
                                      type-topic."))
  (:documentation "This class associates topics that are used as type for
                   typable constructcs. Additionally there are stored some
                   version-infos."))


(defpclass ScopeAssociationC(VersionedAssociationC)
  ((theme-topic :initarg :theme-topic
		:accessor theme-topic
		:initform (error (make-missing-argument-condition "From ScopeAssociationC(): theme-topic must be set" 'theme-topic ':theme-topic))
		:associate TopicC
		:documentation "Associates this opbject with a topic that is a
                                scopable construct.")
   (scopable-construct :initarg :scopable-construct
		       :accessor scopable-construct
		       :initform (error (make-missing-argument-condition "From ScopeAssociationC(): scopable-construct must be set" 'scopable-construct ':scopable-construct))
		       :associate ScopableC
		       :documentation "Associates this object with the socpable
                                       construct that is scoped by the
                                       scope-topic."))
  (:documentation "This class associates topics that are used as scope with
                   scopable construtcs. Additionally there are stored some
                   version-infos"))


(defpclass ReifierAssociationC(VersionedAssociationC)
  ((reifiable-construct :initarg :reifiable-construct
			:accessor reifiable-construct
			:initform (error (make-missing-argument-condition "From ReifierAssociation(): reifiable-construct must be set" 'reifiable-construct ':reifiable-construct))
			:associate ReifiableConstructC
			:documentation "The actual construct which is reified
                                        by a topic.")
   (reifier-topic :initarg :reifier-topic
		  :accessor reifier-topic
		  :initform (error (make-missing-argument-condition "From ReifierAssociationC(): reifier-topic must be set" 'reifier-topic ':reifier-topic))
		  :associate TopicC
		  :documentation "The reifier-topic that reifies the
                                  reifiable-construct."))
  (:documentation "A versioned-association that relates a reifiable-construct
                   with a topic."))


;;; pointer associations ...
(defpclass PointerAssociationC (VersionedAssociationC)
  ((identifier :initarg :identifier
	       :accessor identifier
	       :inherit t
	       :initform (error (make-missing-argument-condition "From PointerAssociationC(): identifier must be set" 'identifier ':identifier))
	       :associate PointerC
	       :documentation "The actual data that is associated with
                               the pointer-association's parent."))
  (:documentation "An abstract base class for all versioned
                   pointer-associations."))


(defpclass SubjectLocatorAssociationC(PointerAssociationC)
  ((parent-construct :initarg :parent-construct
		     :accessor parent-construct
		     :initform (error (make-missing-argument-condition "From SubjectLocatorAssociationC(): parent-construct must be set" 'parent-construct ':parent-symbol))
		     :associate TopicC
		     :documentation "The actual topic which is associated
                                     with the subject-locator."))
  (:documentation "A pointer that associates subject-locators, versions
                   and topics."))


(defpclass PersistentIdAssociationC(PointerAssociationC)
  ((parent-construct :initarg :parent-construct
		     :accessor parent-construct
		     :initform (error (make-missing-argument-condition "From PersistentIdAssociationC(): parent-construct must be set" 'parent-construct ':parent-construct))
		     :associate TopicC
		     :documentation "The actual topic which is associated
                                     with the subject-identifier/psi."))
  (:documentation "A pointer that associates subject-identifiers, versions
                   and topics."))


(defpclass TopicIdAssociationC(PointerAssociationC)
  ((parent-construct :initarg :parent-construct
		     :accessor parent-construct
		     :initform (error (make-missing-argument-condition "From TopicIdAssociationC(): parent-construct must be set" 'parent-construct ':parent-construct))
		     :associate TopicC
		     :documentation "The actual topic which is associated
                                     with the topic-identifier."))
  (:documentation "A pointer that associates topic-identifiers, versions
                   and topics."))


(defpclass ItemIdAssociationC(PointerAssociationC)
  ((parent-construct :initarg :parent-construct
		     :accessor parent-construct
		     :initform (error (make-missing-argument-condition "From ItemIdAssociationC(): parent-construct must be set" 'parent-construct ':parent-construct))
		     :associate ReifiableConstructC
		     :documentation "The actual parent which is associated
                                     with the item-identifier."))
  (:documentation "A pointer that associates item-identifiers, versions
                   and reifiable-constructs."))


;;; characteristic associations ...
(defpclass CharacteristicAssociationC(VersionedAssociationC)
  ((characteristic :initarg :characteristic
		   :accessor characteristic
		   :inherit t
		   :initform (error (make-missing-argument-condition  "From CharacteristicCAssociation(): characteristic must be set" 'characteristic ':characteristic))
		   :associate CharacteristicC
		   :documentation "Associates this object with the actual
                                   characteristic object."))
  (:documentation "An abstract base class for all association-objects that
                   associates characteristics with topics."))


(defpclass VariantAssociationC(CharacteristicAssociationC)
  ((parent-construct :initarg :parent-construct
		     :accessor parent-construct
		     :initform (error (make-missing-argument-condition "From VariantAssociationC(): parent-construct must be set" 'parent-construct ':parent-construct))
		     :associate NameC
		     :documentation "Associates this object with a name."))
  (:documentation "Associates variant objects with name obejcts.
                   Additionally version-infos are stored."))


(defpclass NameAssociationC(CharacteristicAssociationC)
  ((parent-construct :initarg :parent-construct
		     :accessor parent-construct
		     :initform (error (make-missing-argument-condition "From NameAssociationC(): parent-construct must be set" 'parent-construct ':parent-construct))
		     :associate TopicC
		     :documentation "Associates this object with a topic."))
  (:documentation "Associates name objects with their parent topics.
                   Additionally version-infos are stored."))


(defpclass OccurrenceAssociationC(CharacteristicAssociationC)
  ((parent-construct :initarg :parent-construct
		     :accessor parent-construct
		     :initform (error (make-missing-argument-condition "From OccurrenceAssociationC(): parent-construct must be set" 'parent-construct ':parent-construct))
		     :associate TopicC
		     :documentation "Associates this object with a topic."))
  (:documentation "Associates occurrence objects with their parent topics.
                   Additionally version-infos are stored."))


;;; roles/association associations ...
(defpclass PlayerAssociationC(VersionedAssociationC)
  ((player-topic :initarg :player-topic
		 :accessor player-topic
		 :associate TopicC
		 :initform (error (make-missing-argument-condition "From PlayerAssociationC(): player-topic must be set" 'player-topic ':player-topic))
		 :documentation "Associates this object with a topic that is
                                 a player.")
   (parent-construct :initarg :parent-construct
		     :accessor parent-construct
		     :associate RoleC
		     :initform (error (make-missing-argument-condition "From PlayerAssociationC(): parent-construct must be set" 'parent-construct ':parent-construct))
		     :documentation "Associates this object with the parent-association."))
  (:documentation "This class associates roles and their player in given
                   revisions."))


(defpclass RoleAssociationC(VersionedAssociationC)
  ((role :initarg :role
	 :accessor role
	 :associate RoleC
	 :initform (error (make-missing-argument-condition "From RoleAssociationC(): role must be set" 'role ':role))
	 :documentation "Associates this objetc with a role-object.")
   (parent-construct :initarg :parent-construct
		     :accessor parent-construct
		     :associate AssociationC
		     :initform (error (make-missing-argument-condition "From RoleAssociationC(): parent-construct  must be set" 'parent-construct ':parent-construct))
		     :documentation "Assocates thius object with an
                                     association-object."))
  (:documentation "Associates roles with assoications and adds some
                   version-infos between these realtions."))


;;; some helpers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun make-duplicate-identifier-condition (message uri)
  "Returns an duplicate-identifier-condition with the passed arguments."
  (make-condition 'duplicate-identifier-error
		  :message message
		  :uri uri))


(defun make-object-not-found-condition (message)
  "Returns an object-not-found-condition with the passed arguments."
  (make-condition 'object-not-found-error
		  :message message))


(defun make-tm-reference-condition (message referenced-construct
				    existing-reference new-reference)
  "Returns a tm-reference-condition with the passed arguments."
  (make-condition 'tm-reference-error
		  :message message
		  :referenced-construct referenced-construct
		  :existing-reference existing-reference
		  :new-reference new-reference))


(defun make-bad-type-condition (message expected-type result-object)
  (make-condition
   'bad-type-error
   :message message
   :expected-type expected-type
   :result-object result-object))


(defun make-not-mergable-condition (message construct-1 construct-2)
  "Returns a not-mergable-condition with the passed arguments."
  (make-condition 'not-mergable-error
		  :message message
		  :construct-1 construct-1
		  :construct-2 construct-2))


(defun make-missing-argument-condition (message argument-symbol function-symbol)
  "Returns a missing-argument-condition with the passed arguments."
  (make-condition 'missing-argument-error
		  :message message
		  :argument-symbol argument-symbol
		  :function-symbol function-symbol))


(defgeneric get-most-recent-versioned-assoc (construct slot-symbol)
  (:documentation "Returns the most recent VersionedAssociationC
                   object.")
  (:method ((construct TopicMapConstructC) (slot-symbol Symbol))
    (let ((all-assocs (slot-p construct slot-symbol)))
      (let ((zero-assoc
	     (find-if #'(lambda(assoc)
			  (= (end-revision
			      (get-most-recent-version-info assoc)) 0))
		      all-assocs)))
	(if zero-assoc
	    zero-assoc
	    (let ((ordered-assocs
		   (sort all-assocs
			 #'(lambda(x y)
			     (> (end-revision
				 (get-most-recent-version-info x))
				(end-revision
				 (get-most-recent-version-info y)))))))
	      (when ordered-assocs
		(first ordered-assocs))))))))


(defun get-latest-topic-by-psi (topic-psi)
  "Returns the latest topic bound to the PersistentIdC
   object corresponding to the given uri."
  (declare (String topic-psi))
  (let ((psi-inst
	 (elephant:get-instance-by-value
	  'PersistentIdC 'uri topic-psi)))
    (when psi-inst
      (let ((latest-va
	     (get-most-recent-versioned-assoc
	      psi-inst 'identified-construct)))
	(when (and latest-va (versions latest-va))
	  (identified-construct
	   psi-inst :revision (start-revision (first (versions latest-va)))))))))


(defun get-db-instances-by-class (class-symbol &key (revision *TM-REVISION*))
  "Returns all instances of the given type and the given revision that are
   stored in the db."
  (declare (symbol class-symbol) (type (or null integer) revision))
  (let ((db-instances (elephant:get-instances-by-class class-symbol)))
    (let ((filtered-instances (remove-if-not #'(lambda(inst)
						 (typep inst class-symbol))
					     db-instances)))
      (if revision
	  (remove-if #'null
		     (map 'list #'(lambda(inst)
				    (find-item-by-revision inst revision))
			  filtered-instances))
	  filtered-instances))))


(defun get-all-topics (&optional (revision *TM-REVISION*))
  (get-db-instances-by-class 'TopicC :revision revision))


(defun get-all-associations (&optional (revision *TM-REVISION*))
  (get-db-instances-by-class 'AssociationC :revision revision))


(defun get-all-tms (&optional (revision *TM-REVISION*))
  (get-db-instances-by-class 'TopicMapC :revision revision))


(defun find-version-info (versioned-constructs
			 &key (sort-function #'<) (sort-key 'start-revision))
  "Returns all version-infos sorted by the function sort-function which is
   applied on the slot sort-key."
  (declare (list versioned-constructs))
  (let ((vis
	 (sort
	  (loop for vc in versioned-constructs
	     append (versions vc))
	  sort-function :key sort-key)))
    (when vis
      (first vis))))


(defun rec-remf (plist keyword)
  "Calls remf for the past plist with the given keyword until
   all key-value-pairs corresponding to the passed keyword were removed."
  (declare (list plist) (keyword keyword))
  (loop while (getf plist keyword)
     do (remf plist keyword))
  plist)


(defun get-item-by-content (content &key (revision *TM-REVISION*))
  "Finds characteristics by their (atomic) content."
  (flet
      ((get-existing-instances (class-symbol)
         (delete-if-not
	  #'(lambda (constr)
	      (find-item-by-revision constr revision))
	  (elephant:get-instances-by-value class-symbol 'charvalue content))))
    (nconc (get-existing-instances 'OccurenceC)
           (get-existing-instances 'NameC)
	   (get-existing-instances 'VariantC))))


(defmacro with-revision (revision &rest body)
  `(let
       ((*TM-REVISION* ,revision))
     ,@body))


(defun slot-p (instance slot-symbol)
  "Returns t if the slot depending on slot-symbol is bound and not nil."
  (if (slot-boundp instance slot-symbol)
      (let ((value (slot-value instance slot-symbol)))
	(when value
	  value))
      ;elephant-relations are handled separately, since slot-boundp does not
      ;work here
      (handler-case (let ((value (slot-value instance slot-symbol)))
		      (when value
			value))
	(error () nil))))


(defun delete-1-n-association(instance slot-symbol)
  (when (slot-p instance slot-symbol)
    (remove-association
     instance slot-symbol (slot-value instance slot-symbol))))


(defgeneric delete-construct (construct)
  (:documentation "Drops recursively construct and all its dependent objects
                   from the elephant store."))


(defmethod delete-construct ((construct elephant:persistent))
  nil)


(defmethod delete-construct :after ((construct elephant:persistent))
  (drop-instance construct))


(defun filter-slot-value-by-revision (construct slot-symbol
				      &key (start-revision
					    0 start-revision-provided-p))
  (declare (symbol slot-symbol) (integer start-revision))
  (let ((revision
	 (cond (start-revision-provided-p
		start-revision)
	       ((boundp '*TM-REVISION*)
		*TM-REVISION*)
	       (t 0)))
	(properties (slot-p construct slot-symbol)))
    (cond ((not properties)
	   nil) ;no properties were found -> nil
	  ((= 0 revision)
	   (remove-if #'null
		      (map 'list #'find-most-recent-revision properties)))
	  (t
	   (remove-if #'null
		      (map 'list #'(lambda(prop)
				     (find-item-by-revision prop revision))
			   properties))))))


(defun get-revision ()
  "TODO: replace by something that does not suffer from a 1 second resolution."
  (get-universal-time))


(defun string-integer-p (integer-as-string)
  "Returns t if the passed string can be parsed to an integer."
  (handler-case (when (parse-integer integer-as-string)
		  t)
    (condition () nil)))


(defun merge-all-constructs(constructs-to-be-merged &key (revision *TM-REVISION*))
  "Merges all constructs contained in the given list."
  (declare (list constructs-to-be-merged))
  (cond ((null constructs-to-be-merged)
	 nil)
	((= (length constructs-to-be-merged) 1)
	 (first constructs-to-be-merged))
	(t
	 (let ((constr-1 (first constructs-to-be-merged))
	       (constr-2 (second constructs-to-be-merged))
	       (tail (subseq constructs-to-be-merged 2)))
	   (let ((merged-constr
		  (merge-constructs constr-1 constr-2 :revision revision)))
	     (merge-all-constructs (append (list merged-constr)
					   tail)))))))


(defgeneric internal-id (construct)
  (:documentation "Returns the internal id that uniquely identifies a
                   construct (currently simply its OID)."))


(defmethod internal-id ((construct TopicMapConstructC))
  (slot-value construct (find-symbol "OID" 'elephant)))


;;; generic definitions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgeneric mark-as-deleted (construct &key source-locator revision)
  (:documentation "Mark a construct as deleted if it comes from the source
                   indicated by source-locator"))


(defgeneric marked-as-deleted-p (construct)
  (:documentation "Returns t if the construct was marked-as-deleted."))


(defgeneric find-self-or-equal (construct parent-construct &key revision)
  (:documentation "Returns the construct 'construct' if is owned by the
                   parent-construct or an equal construct or nil if there
                   is no equal one."))


(defgeneric merge-if-equivalent (new-characteristic parent-construct
						    &key revision)
  (:documentation "Merges the new characteristic/role with one equivalent of the
                   parent's charateristics/roles instead of adding the entire new
                   characteristic/role to the parent."))


(defgeneric parent (construct &key revision)
  (:documentation "Returns the parent construct of the passed object that
                   corresponds with the given revision. The returned construct
                   can be a TopicC or a NameC."))


(defgeneric delete-if-not-referenced (construct)
  (:documentation "Calls delete-construct for the given object if it is
                   not referenced by any other construct."))


(defgeneric add-characteristic (construct characteristic &key revision)
  (:documentation "Adds the passed characterisitc to the given topic by calling
                   add-name or add-occurrences.
                   Variants are added to names by calling add-name."))


(defgeneric private-delete-characteristic (construct characteristic &key revision)
  (:documentation "Deletes the passed characteristic of the given topic by
                   calling delete-name or delete-occurrence.
                   Variants are deleted from names by calling delete-variant."))


(defgeneric delete-characteristic (construct characteristic &key revision)
  (:documentation "See private-delete-characteristic but adds the parent
                   (if it is a variant also the parent's parent) to the
                   version history of this call's revision"))


(defgeneric find-oldest-construct (construct-1 construct-2)
  (:documentation "Returns the construct which owns the oldes version info.
                   If a construct is not a versioned construct the oldest
                   association determines the construct's version info."))


(defgeneric merge-constructs (construct-1 construct-2 &key revision)
  (:documentation "Merges two constructs of the same type if they are
                   mergable. The latest construct will be marked as deleted
                   The older one gets all characteristics of the marked as
                   deleted one. All referenced constructs are also updated
                   with the changeds that are caused by this operation."))


(defgeneric parent-delete-parent (construct parent-construct &key revision)
  (:documentation "Sets the assoication-object between the passed
                   constructs as marded-as-deleted."))


(defgeneric delete-parent (construct parent-construct &key revision)
  (:documentation "See private-delete-parent but adds the parent to
                   the given version."))


(defgeneric add-parent (construct parent-construct &key revision)
  (:documentation "Adds the parent-construct (TopicC or NameC) in form of
                   a corresponding association to the given object."))


(defgeneric find-item-by-revision (construct revision
					     &optional parent-construct)
  (:documentation "Returns the given object if it exists in the passed
                   version otherwise nil.
		   Constructs that exist to be owned by parent-constructs
                   must provide their parent-construct to get the corresponding
                   revision of the relationship between the construct itself and
                   its parent-construct."))


(defgeneric check-for-duplicate-identifiers (construct &key revision)
  (:documentation "Check for possibly duplicate identifiers and signal an
  duplicate-identifier-error is such duplicates are found"))


(defgeneric get-all-identifiers-of-construct (construct &key revision)
  (:documentation "Get all identifiers that a given construct has"))


(defgeneric get-all-characteristics (parent-construct characteristic-symbol)
  (:documentation "Returns all characterisitcs of the passed type the parent
                   construct was ever associated with."))


(defgeneric equivalent-construct (construct &key start-revision
					    &allow-other-keys)
  (:documentation "Returns t if the passed construct is equivalent to the passed
                   key arguments (TMDM equality rules). Parent-equality is not
                   checked in this methods, so the user has to pass children of
                   the same parent."))


(defgeneric equivalent-constructs (construct-1 construct-2 &key revision)
  (:documentation "Returns t if the passed constructs are equivalent to each
                   other (TMDM equality rules). Parent-equality is not
                   checked in this methods, so the user has to pass children of
                   the same parent."))


(defgeneric get-most-recent-version-info (construct)
  (:documentation "Returns the latest VersionInfoC object of the passed
                   versioned construct.
                   The latest construct is either the one with
                   end-revision=0 or with the highest end-revision value."))

(defgeneric owned-p (construct)
  (:documentation "Returns t if the passed construct is referenced by a parent
                   TM construct."))


(defgeneric in-topicmaps (construct &key revision)
  (:documentation "Returns all TopicMaps-obejcts where the construct is
                   contained in."))


(defgeneric add-to-tm (construct construct-to-add)
  (:documentation "Adds a TM construct (TopicC or AssociationC) to the TM."))


(defgeneric delete-from-tm (construct construct-to-delete)
  (:documentation "Deletes a TM construct (TopicC or AssociationC) from
                   the TM."))



;;; generic functions/accessors ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; VersionInfocC
(defmethod delete-construct :before ((version-info VersionInfoC))
  (delete-1-n-association version-info 'versioned-construct))


;;; VersionedConstructC
(defgeneric exist-in-version-history-p (versioned-construct)
  (:documentation "Returns t if the passed construct does not exist in any
                   revision, i.e. the construct has no version-infos or exactly
                   one whose start-revision is equal to its end-revision.")
  (:method ((versioned-construct VersionedConstructC))
    (or (not (versions versioned-construct))
	(and (= (length (versions versioned-construct)) 1)
	     (= (start-revision (first (versions versioned-construct)))
		(end-revision (first (versions versioned-construct))))))))


(defmethod find-oldest-construct ((construct-1 VersionedConstructC)
				 (construct-2 VersionedConstructC))
  (let ((vi-1 (find-version-info (list construct-1)))
	(vi-2 (find-version-info (list construct-2))))
    (cond ((not (or vi-1 vi-2))
	   construct-1)
	  ((not vi-1)
	   construct-2)
	  ((not vi-2)
	   construct-1)
	  ((<= (start-revision vi-1) (start-revision vi-2))
	   construct-1)
	  (t
	   construct-2))))


(defgeneric VersionedConstructC-p (class-symbol)
  (:documentation "Returns t if the passed class is equal to VersionedConstructC
                   or one of its subtypes.")
  (:method ((class-symbol symbol))
    (or (eql class-symbol 'VersionedconstructC)
	(TopicC-p class-symbol)
	(TopicMapC-p class-symbol)
	(AssociationC-p class-symbol))))


(defmethod delete-construct :before ((construct VersionedConstructC))
  (dolist (version-info (versions construct))
    (delete-construct version-info)))


(defmethod find-item-by-revision ((construct VersionedConstructC)
				  (revision integer) &optional parent-construct)
  (declare (ignorable parent-construct))
  (cond ((= revision 0)
	 (find-most-recent-revision construct))
	(t
	 (when (find-if
		#'(lambda(vi)
		    (and (>= revision (start-revision vi))
			 (or (< revision (end-revision vi))
			     (= 0 (end-revision vi)))))
		(versions construct))
	   construct))))


(defmethod get-most-recent-version-info ((construct VersionedConstructC))
  (let ((result (find 0 (versions construct) :key #'end-revision)))
    (if result
	result ;current version-info -> end-revision = 0
	(let ((sorted-list (sort (versions construct)
				 #'(lambda(x y)
				     (> (end-revision x) (end-revision y))))))
	  (when sorted-list
	    (first sorted-list)))))) ;latest version-info of marked-as-deleted constructs -> highest integer


(defgeneric find-most-recent-revision (construct)
  (:documentation "Returns the latest version-info-object of the passed
                   construct.")
  (:method ((construct VersionedConstructC))
    (when (find 0 (versions construct) :key #'end-revision)
      construct)))


(defun add-version-info(construct start-revision)
  "Adds 'construct' to the given version.
   If the construct is a VersionedConstructC add-to-version-history
   is called directly. Otherwise there is called a corresponding
   add-<whatever> method that adds recursively 'construct' to its
   parent and so on."
  (declare (type (or TopicMapConstructC VersionedConstructC) construct)
	   (integer start-revision))
  (cond ((typep construct 'VersionedConstructC)
	 (add-to-version-history construct :start-revision start-revision))
	((typep construct 'VariantC)
	 (let ((name (parent construct :revision start-revision)))
	   (when name
	     (add-variant name construct :revision start-revision)
	     (let ((top (parent name :revision start-revision)))
	       (when top
		 (add-name top name :revision start-revision))))))
	((typep construct 'CharacteristicC)
	 (let ((top (parent construct :revision start-revision)))
	   (when top
	     (add-characteristic top construct :revision start-revision))))
	((typep construct 'RoleC)
	 (let ((assoc (parent construct :revision start-revision)))
	   (when assoc
	     (add-role assoc construct :revision start-revision))))))


(defgeneric add-to-version-history (construct &key start-revision end-revision)
  (:documentation "Adds version history to a versioned construct")
  (:method ((construct VersionedConstructC)
	    &key (start-revision (error (make-missing-argument-condition "From add-to-version-history(): start revision must be present" 'start-revision 'add-to-version-history)))
	    (end-revision 0))
    (let ((eql-version-info
	   (find-if #'(lambda(vi)
			(and (= (start-revision vi) start-revision)
			     (= (end-revision vi) end-revision)))
		    (versions construct))))
      (if eql-version-info
	  eql-version-info
	  (let ((current-version-info
		 (get-most-recent-version-info construct)))
	    (cond
	      ((and current-version-info
		    (= (end-revision current-version-info) start-revision))
	       (setf (end-revision current-version-info) end-revision)
	       current-version-info)
	      ((and current-version-info
		    (= (end-revision current-version-info) 0))
	       (setf (end-revision current-version-info) start-revision)
	       (let ((vi (make-instance 'VersionInfoC 
					:start-revision start-revision
					:end-revision end-revision)))
		 (elephant:add-association vi 'versioned-construct construct)))
	      (t
	       (let ((vi (make-instance 'VersionInfoC 
					:start-revision start-revision
					:end-revision end-revision)))
		 (elephant:add-association vi 'versioned-construct construct)))))))))
		 


(defmethod marked-as-deleted-p ((construct VersionedConstructC))
  (unless (find-if #'(lambda(vi)
		     (= (end-revision vi) 0))
		 (versions construct))
    t))


(defmethod mark-as-deleted ((construct VersionedConstructC)
			    &key source-locator revision)
  (declare (ignorable source-locator))
  (let
      ((last-version ;the last active version
	(find 0 (versions construct) :key #'end-revision)))
    (if (and last-version
	     (= (start-revision last-version) revision))
	(progn
	  (delete-construct last-version)
	  (let ((sorted-versions
		 (sort (versions construct) #'> :key #'end-revision)))
	    (when sorted-versions
	      (setf (end-revision (first sorted-versions)) revision))))
	(when last-version
	  (setf (end-revision last-version) revision)))))


;;; TopicMapConstructC
(defgeneric strictly-equivalent-constructs (construct-1 construct-2
							&key revision)
  (:documentation "Checks if two topic map constructs are not identical but
                   equal according to the TMDM equality rules.")
  (:method ((construct-1 TopicMapConstructC) (construct-2 TopicMapConstructC)
	    &key (revision *TM-REVISION*))
    (declare (integer revision))
    (and (equivalent-constructs construct-1 construct-2 :revision revision)
	 (not (eql construct-1 construct-2)))))


(defmethod check-for-duplicate-identifiers ((construct TopicMapConstructC)
					    &key revision)
  (declare (ignorable revision construct))
  ;do nothing
  )


(defmethod get-all-characteristics ((parent-construct TopicC)
				    (characteristic-symbol symbol))
  (cond ((OccurrenceC-p characteristic-symbol)
	 (map 'list #'characteristic (slot-p parent-construct 'occurrences)))
	((NameC-p characteristic-symbol)
	 (map 'list #'characteristic (slot-p parent-construct 'names)))))


(defgeneric TopicMapConstructC-p (class-symbol)
  (:documentation "Returns t if the passed class is equal to TopicMapConstructC
                   or one of its subtypes.")
  (:method ((class-symbol symbol))
    (or (eql class-symbol 'TopicMapConstructC)
	(ReifiableConstructC-p class-symbol)
	(PointerC-p class-symbol))))


;;; PointerC
(defmethod versions ((construct PointerC))
  "Returns all versions that are indirectly through all PointerAssocitiations
   bound to the passed pointer object."
  (loop for p-assoc in (slot-p construct 'identified-construct)
     append (versions p-assoc)))


(defmethod mark-as-deleted ((construct PointerC) &key source-locator revision)
  "Marks the last active relation between a pointer and its parent construct
   as deleted."
  (declare (ignorable source-locator))
  (let ((owner (identified-construct construct :revision 0)))
    (when owner
      (cond ((typep construct 'PersistentIdC)
	     (private-delete-psi owner construct :revision revision))
	    ((typep construct 'SubjectLocatorC)
	     (private-delete-locator owner construct :revision revision))
	    ((typep construct 'ItemIdentifierC)
	     (private-delete-item-identifier owner construct :revision revision))
	    ((typep construct 'TopicIdentificationC)
	     (private-delete-topic-identifier owner construct :revision revision))))))


(defmethod marked-as-deleted-p ((construct PointerC))
  (unless (identified-construct construct :revision 0)
    t))


(defmethod find-oldest-construct ((construct-1 PointerC) (construct-2 PointerC))
  (let ((vi-1 (find-version-info (slot-p construct-1 'identified-construct)))
	(vi-2 (find-version-info (slot-p construct-2 'identified-construct))))
    (cond ((not (or vi-1 vi-2))
	   construct-1)
	  ((not vi-1)
	   construct-2)
	  ((not vi-2)
	   construct-1)
	  ((<= (start-revision vi-1) (start-revision vi-2))
	   construct-1)
	  (t
	   construct-2))))


(defmethod equivalent-constructs ((construct-1 PointerC) (construct-2 PointerC)
				  &key (revision nil))
  (declare (ignorable revision))
  (string= (uri construct-1) (uri construct-2)))


(defgeneric PointerC-p (class-symbol)
  (:documentation "Returns t if the passed symbol corresponds to the class
                   PointerC or one of its subclasses.")
  (:method ((class-symbol symbol))
    (or (eql class-symbol 'PointerC)
	(IdentifierC-p class-symbol)
	(TopicIdentificationC-p class-symbol)
	(PersistentIdC-p class-symbol)
	(ItemIdentifierC-p class-symbol)
	(SubjectLocatorC-p class-symbol))))


(defmethod equivalent-construct ((construct PointerC)
				 &key start-revision (uri ""))
  "All Pointers are equal if they have the same URI value."
  (declare (string uri) (ignorable start-revision))
  (string= (uri construct) uri))


(defmethod find-item-by-revision ((construct PointerC)
				  (revision integer) &optional parent-construct)
  (if parent-construct
      (let ((parent-assoc
	     (let ((assocs
		    (remove-if
		     #'null
		     (map 'list #'(lambda(assoc)
				    (when (eql (parent-construct assoc)
					       parent-construct)
				      assoc))
			  (slot-p construct 'identified-construct)))))
	       (when assocs
		 (first assocs)))))
	(when parent-assoc
	  (cond ((= revision 0)
		 (find-most-recent-revision parent-assoc))
		(t
		 (when (find-if
			#'(lambda(vi)
			    (and (>= revision (start-revision vi))
				 (or (< revision (end-revision vi))
				     (= 0 (end-revision vi)))))
			(versions parent-assoc))
		   construct)))))
      nil))


(defmethod delete-construct :before ((construct PointerC))
  (dolist (p-assoc (slot-p construct 'identified-construct))
    (delete-construct p-assoc)))


(defmethod owned-p ((construct PointerC))
  (when (slot-p construct 'identified-construct)
    t))


(defgeneric identified-construct (construct &key revision)
  (:documentation "Returns the identified-construct -> ReifiableConstructC or
                   TopicC that corresponds with the passed revision.")
  (:method ((construct PointerC) &key (revision *TM-REVISION*))
    (let ((assocs
	   (map 'list #'parent-construct
		(filter-slot-value-by-revision construct 'identified-construct
					       :start-revision revision))))
      (when assocs ;result must be nil or a list with one item
	(first assocs)))))


;;; TopicIdentificationC
(defmethod equivalent-constructs ((construct-1 TopicIdentificationC)
				  (construct-2 TopicIdentificationC)
				  &key (revision nil))
  (declare (ignorable revision))
  (and (call-next-method)
       (string= (xtm-id construct-1) (xtm-id construct-2))))
       


(defgeneric TopicIdentificationC-p (class-symbol)
  (:documentation "Returns t if the passed class symbol is equal
                   to TopicIdentificationC.")
  (:method ((class-symbol symbol))
    (eql class-symbol 'TopicIdentificationC)))


(defmethod equivalent-construct ((construct TopicIdentificationC)
				 &key start-revision (uri "") (xtm-id ""))
  "TopicIdentifiers are equal if teh URI and XTM-ID values are equal."
  (declare (string uri xtm-id))
  (let ((equivalent-pointer (call-next-method
			     construct :start-revision start-revision
			     :uri uri)))
    (and equivalent-pointer
	 (string= (xtm-id construct) xtm-id))))


;;; IdentifierC
(defgeneric IdentifierC-p (class-symbol)
  (:documentation "Returns t if the passed symbol is equal to IdentifierC
                   or one of its sybtypes.")
  (:method ((class-symbol symbol))
    (or (eql class-symbol 'IdentifierC)
	(PersistentIdC-p class-symbol)
	(SubjectLocatorC-p class-symbol)
	(ItemIdentifierC-p class-symbol))))


;;; PersistentIdC
(defgeneric PersistentIdC-p (class-symbol)
  (:documentation "Returns t if the passed symbol is equal to PersistentIdC.")
  (:method ((class-symbol symbol))
    (eql class-symbol 'PersistentIdC)))


;;; ItemIdentifierC
(defgeneric ItemIdentifierC-p (class-symbol)
  (:documentation "Returns t if the passed symbol is equal to ItemIdentifierC.")
  (:method ((class-symbol symbol))
    (eql class-symbol 'ItemIdentifierC)))

;;; SubjectLocatorC
(defgeneric SubjectLocatorC-p (class-symbol)
  (:documentation "Returns t if the passed symbol is equal to SubjectLocatorC.")
  (:method ((class-symbol symbol))
    (eql class-symbol 'SubjectLocatorC)))


;;; PointerAssociationC
(defmethod delete-construct :before ((construct PointerAssociationC))
  (delete-1-n-association construct 'identifier))


;;; ItemIdAssociationC
(defmethod delete-construct :before ((construct ItemIdAssociationC))
  (delete-1-n-association construct 'parent-construct))


;;; TopicIdAssociationC
(defmethod delete-construct :before ((construct TopicIdAssociationC))
  (delete-1-n-association construct 'parent-construct))


;;; PersistentIdAssociationC
(defmethod delete-construct :before ((construct PersistentIdAssociationC))
  (delete-1-n-association construct 'parent-construct))


;;; SubjectLocatorAssociationC
(defmethod delete-construct :before ((construct SubjectLocatorAssociationC))
  (delete-1-n-association construct 'parent-construct))


;;; ReifierAssociationC
(defmethod delete-construct :before ((construct ReifierAssociationC))
  (delete-1-n-association construct 'reifiable-construct)
  (delete-1-n-association construct 'reifier-topic))


;;; TypeAssociationC
(defmethod delete-construct :before ((construct TypeAssociationC))
  (delete-1-n-association construct 'type-topic)
  (delete-1-n-association construct 'typable-construct))


;;; ScopeAssociationC
(defmethod delete-construct :before ((construct ScopeAssociationC))
  (delete-1-n-association construct 'theme-topic)
  (delete-1-n-association construct 'scopable-construct))


;;; CharacteristicAssociationC
(defmethod delete-construct :before ((construct CharacteristicAssociationC))
  (delete-1-n-association construct 'characteristic))


;;; OccurrenceAssociationC
(defmethod delete-construct :before ((construct OccurrenceAssociationC))
  (delete-1-n-association construct 'parent-construct))


;;; NameAssociationC
(defmethod delete-construct :before ((construct NameAssociationC))
  (delete-1-n-association construct 'parent-construct))


;;; VariantAssociationC
(defmethod delete-construct :before ((construct VariantAssociationC))
  (delete-1-n-association construct 'parent-construct))


;;; RoleAssociationC
(defmethod delete-construct :before ((construct RoleAssociationC))
  (delete-1-n-association construct 'role)
  (delete-1-n-association construct 'parent-construct))


;;; PlayerAssociationC
(defmethod delete-construct :before ((construct PlayerAssociationC))
  (delete-1-n-association construct 'player-topic)
  (delete-1-n-association construct 'parent-construct))


;;; TopicC
(defmethod mark-as-deleted :around ((top TopicC)
				    &key (source-locator nil sl-provided-p)
				    revision)
  "Mark a topic as deleted if it comes from the source indicated by
   source-locator"
  ;;Part 1b, 1.4.3.3.1:
  ;; Let SP be the value of the ServerSourceLocatorPrefix element in the ATOM feed F
  ;; * Let SI be the value of TopicSI element in ATOM entry E
  ;; * feed F contains E)
  ;; * entry E references topic fragment TF
  ;; * Let LTM be the local topic map
  ;; * Let T be the topic in LTM that has a subjectidentifier that matches SI
  ;; * For all names, occurrences and associations in which T plays a role, TMC
  ;;   * Delete all SrcLocators of TMC that begin with SP. If the count of srclocators on TMC = 0 then delete TMC 
  ;;   * Merge in the fragment TF using SP as the base all generated source locators.
  (when (or (and (not source-locator) sl-provided-p)
	    (and sl-provided-p
		 (some (lambda (psi) (string-starts-with (uri psi) source-locator))
		       (psis top :revision 0))))
    (unless sl-provided-p
      (mapc (lambda(psi)(mark-as-deleted psi :revision revision
					 :source-locator source-locator))
	    (psis top :revision 0)))
    (mapc (lambda(sl)(mark-as-deleted sl :revision revision
				      :source-locator source-locator))
	  (locators top :revision 0))
    (mapc (lambda (name) (mark-as-deleted name :revision revision
					  :source-locator source-locator))
          (names top :revision 0))
    (mapc (lambda (occ) (mark-as-deleted occ :revision revision
					 :source-locator source-locator))
          (occurrences top :revision 0))
    (mapc (lambda (ass) (mark-as-deleted ass :revision revision
					 :source-locator source-locator))
	  (find-all-associations top :revision 0))
    (call-next-method)))


(defmethod equivalent-constructs ((construct-1 TopicC) (construct-2 TopicC)
				  &key (revision *TM-REVISION*))
  (declare (integer revision))
  (let ((ids-1 (union (union (item-identifiers construct-1 :revision revision)
			     (locators construct-1 :revision revision))
		      (psis construct-1 :revision revision)))
	(ids-2 (union (union (item-identifiers construct-2 :revision revision)
			     (locators construct-2 :revision revision))
		      (psis construct-2 :revision revision))))
    (when (intersection ids-1 ids-2)
      t)))


(defgeneric TopicC-p (class-symbol)
  (:documentation "Returns t if the passed symbol is equal to TopicC.")
  (:method ((class-symbol symbol))
    (eql class-symbol 'TopicC)))


(defmethod equivalent-construct ((construct TopicC)
				 &key (start-revision *TM-REVISION*) (psis nil)
				 (locators nil) (item-identifiers nil)
				 (topic-identifiers nil))
  "Isidorus handles Topic-equality only by the topic's identifiers
   'psis', 'subject locators' and 'item identifiers'. Names and occurences
   are not checked becuase we don't know when a topic is finalized and owns
   all its charactersitics. T is returned if the topic owns one of the given
   identifier-URIs."
  (declare (integer start-revision) (list psis locators item-identifiers
					  topic-identifiers))
  (when
      (intersection
       (union (union (psis construct :revision start-revision)
		     (locators construct :revision start-revision))
	      (union (item-identifiers construct :revision start-revision)
		     (topic-identifiers construct :revision start-revision)))
       (union (union psis locators) (union item-identifiers topic-identifiers)))
    t))


(defmethod delete-construct :before ((construct TopicC))
  (let ((psi-assocs-to-delete (slot-p construct 'psis))
	(sl-assocs-to-delete (slot-p construct 'locators))
	(name-assocs-to-delete (slot-p construct 'names))
	(occ-assocs-to-delete (slot-p construct 'occurrences))
	(role-assocs-to-delete (slot-p construct 'player-in-roles))
	(type-assocs-to-delete (slot-p construct 'used-as-type))
	(scope-assocs-to-delete (slot-p construct 'used-as-theme))
	(reifier-assocs-to-delete (slot-p construct 'reified-construct)))
    (let ((all-psis (map 'list #'identifier psi-assocs-to-delete))
	  (all-sls (map 'list #'identifier sl-assocs-to-delete))
	  (all-names (map 'list #'characteristic name-assocs-to-delete))
	  (all-occs (map 'list #'characteristic occ-assocs-to-delete))
	  (all-roles (map 'list #'parent-construct role-assocs-to-delete))
	  (all-types (map 'list #'typable-construct type-assocs-to-delete)))
      (dolist (construct-to-delete (append psi-assocs-to-delete
					   sl-assocs-to-delete
					   name-assocs-to-delete
					   occ-assocs-to-delete
					   role-assocs-to-delete
					   type-assocs-to-delete
					   scope-assocs-to-delete
					   reifier-assocs-to-delete))
	(delete-construct construct-to-delete))
      (dolist (candidate-to-delete (append all-psis all-sls all-names all-occs))
	(unless (owned-p candidate-to-delete)
	  (delete-construct candidate-to-delete)))
      (dolist (candidate-to-delete all-roles)
	(unless (player-p candidate-to-delete)
	  (delete-construct candidate-to-delete)))
      (dolist (candidate-to-delete all-types)
	(unless (instance-of-p candidate-to-delete)
	  (delete-construct candidate-to-delete)))
      (dolist (tm (slot-p construct 'in-topicmaps))
	(remove-association construct 'in-topicmaps tm)))))


(defmethod owned-p ((construct TopicC))
  (when (slot-p construct 'in-topicmaps)
    t))


(defgeneric topic-id (construct &optional revision xtm-id)
  (:documentation "Returns the primary id of this item
                   (= essentially the OID). If xtm-id is explicitly given,
                   returns one of the topic-ids in that TM
                   (which must then exist).")
  (:method ((construct TopicC) &optional (revision *TM-REVISION*) (xtm-id nil))
    (declare (type (or string null) xtm-id)
	     (type (or integer null) revision))
    (if xtm-id
	(let ((possible-identifiers
	       (remove-if-not
		#'(lambda(top-id)
		    (string= (xtm-id top-id) xtm-id))
		(topic-identifiers construct :revision revision))))
	  (unless possible-identifiers
	    (error (make-object-not-found-condition (format nil "Could not find an object ~a in xtm-id ~a" construct xtm-id))))
	  (uri (first possible-identifiers)))
	(concatenate 'string "t" (write-to-string (internal-id construct))))))


(defgeneric topic-identifiers (construct &key revision)
  (:documentation "Returns the TopicIdentificationC-objects that correspond
                   with the passed construct and the passed version.")
  (:method ((construct TopicC) &key (revision *TM-REVISION*))
    (let ((assocs (filter-slot-value-by-revision
		   construct 'topic-identifiers :start-revision revision)))
      (map 'list #'identifier assocs))))


(defgeneric add-topic-identifier (construct topic-identifier &key revision)
  (:documentation "Adds the passed topic-identifier to the passed topic.
                   If the topic-identifier is already related with the passed
                   topic a new revision is added.
                   If the passed identifer already identifies another object
                   the identified-constructs are merged.")
  (:method ((construct TopicC) (topic-identifier TopicIdentificationC)
	    &key (revision *TM-REVISION*))
    (let ((all-ids
	   (map 'list #'identifier (slot-p construct 'topic-identifiers)))
	  (construct-to-be-merged
	   (let ((id-owner (identified-construct topic-identifier
						 :revision revision)))
	     (when (not (eql id-owner construct))
	       id-owner))))
      (let ((merged-construct construct))
	(cond (construct-to-be-merged
	       (setf merged-construct
		     (merge-constructs construct construct-to-be-merged
				       :revision revision)))
	      ((find topic-identifier all-ids)
	       (let ((ti-assoc (loop for ti-assoc in (slot-p construct
							     'topic-identifiers)
				  when (eql (identifier ti-assoc)
					    topic-identifier)
				  return ti-assoc)))
		 (add-to-version-history ti-assoc :start-revision revision)))
	      (t
	       (make-construct 'TopicIdAssociationC
			       :parent-construct construct
			       :identifier topic-identifier
			       :start-revision revision)))
	(add-to-version-history merged-construct :start-revision revision)
	merged-construct))))


(defgeneric private-delete-topic-identifier
    (construct topic-identifier &key revision)
  (:documentation "Sets the association object between the passed constructs
                   as mark-as-deleted.")
  (:method ((construct TopicC) (topic-identifier TopicIdentificationC)
	    &key (revision (error (make-missing-argument-condition "From private-delete-topic-identifier(): revision must be set" 'revision 'private-delete-topic-identifier))))
    (let ((assoc-to-delete (loop for ti-assoc in (slot-p construct 'topic-identifiers)
			      when (eql (identifier ti-assoc) topic-identifier)
			      return ti-assoc)))
      (when assoc-to-delete
	(mark-as-deleted assoc-to-delete :revision revision)
	construct))))


(defgeneric delete-topic-identifier
    (construct topic-identifier &key revision)
  (:documentation "See private-delete-topic-identifier but adds the parent
                   construct to the given version")
  (:method ((construct TopicC) (topic-identifier TopicIdentificationC)
	    &key (revision (error (make-missing-argument-condition "From delete-topic-identifier(): revision must be set" 'revision 'delete-topic-identifier))))
    (when (private-delete-topic-identifier construct topic-identifier
					   :revision revision)
      (add-to-version-history construct :start-revision revision)
      construct)))


(defgeneric psis (construct &key revision)
  (:documentation "Returns the PersistentIdC-objects that correspond
                   with the passed construct and the passed version.")
  (:method ((construct TopicC) &key (revision *TM-REVISION*))
    (let ((assocs (filter-slot-value-by-revision
		   construct 'psis :start-revision revision)))
      (map 'list #'identifier assocs))))


(defgeneric add-psi (construct psi &key revision)
  (:documentation "Adds the passed psi to the passed topic.
                   If the psi is already related with the passed
                   topic a new revision is added.
                   If the passed identifer already identifies another object
                   the identified-constructs are merged.")
  (:method ((construct TopicC) (psi PersistentIdC)
	    &key (revision *TM-REVISION*))
    (let ((all-ids
	   (map 'list #'identifier (slot-p construct 'psis)))
	  (construct-to-be-merged
	   (let ((id-owner (identified-construct psi :revision revision)))
	     (when (not (eql id-owner construct))
	       id-owner))))
      (let ((merged-construct construct))
	(cond (construct-to-be-merged
	       (setf merged-construct
		     (merge-constructs construct construct-to-be-merged
				       :revision revision)))
	      ((find psi all-ids)
	       (let ((psi-assoc (loop for psi-assoc in (slot-p construct 'psis)
				   when (eql (identifier psi-assoc) psi)
				   return psi-assoc)))
		 (add-to-version-history psi-assoc :start-revision revision)))
	      (t
	       (make-construct 'PersistentIdAssociationC
			       :parent-construct construct
			       :identifier psi
			       :start-revision revision)))
	(add-to-version-history merged-construct :start-revision revision)
	merged-construct))))


(defgeneric private-delete-psi (construct psi &key revision)
  (:documentation "Sets the association object between the passed constructs
                   as mark-as-deleted.")
  (:method ((construct TopicC) (psi PersistentIdC)
	    &key (revision (error (make-missing-argument-condition "From private-delete-psi(): revision must be set" 'revision 'private-delete-psi))))
    (let ((assoc-to-delete (loop for psi-assoc in (slot-p construct 'psis)
			      when (eql (identifier psi-assoc) psi)
			      return psi-assoc)))
      (when assoc-to-delete
	(mark-as-deleted assoc-to-delete :revision revision)
	construct))))


(defgeneric delete-psi (construct psi &key revision)
  (:documentation "See private-delete-psis but adds the parent to the given
                   version.")
  (:method ((construct TopicC) (psi PersistentIdC)
	    &key (revision (error (make-missing-argument-condition "From delete-psi(): revision must be set" 'revision 'delete-psi))))
    (when (private-delete-psi construct psi :revision revision)
      (add-to-version-history construct :start-revision revision)
      construct)))


(defgeneric locators (construct &key revision)
  (:documentation "Returns the SubjectLocatorC-objects that correspond
                   with the passed construct and the passed version.")
  (:method ((construct TopicC) &key (revision *TM-REVISION*))
    (let ((assocs (filter-slot-value-by-revision
		   construct 'locators :start-revision revision)))
      (map 'list #'identifier assocs))))


(defgeneric add-locator (construct locator &key revision)
  (:documentation "Adds the passed locator to the passed topic.
                   If the locator is already related with the passed
                   topic a new revision is added.
                   If the passed identifer already identifies another object
                   the identified-constructs are merged.")
  (:method ((construct TopicC) (locator SubjectLocatorC)
	    &key (revision *TM-REVISION*))
    (let ((all-ids
	   (map 'list #'identifier (slot-p construct 'locators)))
	  (construct-to-be-merged
	   (let ((id-owner (identified-construct locator :revision revision)))
	     (when (not (eql id-owner construct))
	       id-owner))))
      (let ((merged-construct construct))
	(cond (construct-to-be-merged
	       (setf merged-construct
		     (merge-constructs construct construct-to-be-merged
				       :revision revision)))
	      ((find locator all-ids)
	       (let ((loc-assoc
		      (loop for loc-assoc in (slot-p construct 'locators)
			 when (eql (identifier loc-assoc) locator)
			 return loc-assoc)))
		 (add-to-version-history loc-assoc :start-revision revision)))
	      (t
	       (make-construct 'SubjectLocatorAssociationC
			       :parent-construct construct
			       :identifier locator
			       :start-revision revision)))
	(add-to-version-history merged-construct :start-revision revision)
	merged-construct))))


(defgeneric private-delete-locator (construct locator &key revision)
  (:documentation "Sets the association object between the passed constructs
                   as mark-as-deleted.")
  (:method ((construct TopicC) (locator SubjectLocatorC)
	    &key (revision (error (make-missing-argument-condition "From private-delete-locator(): revision must be set" 'revision  'private-delete-locator))))
    (let ((assoc-to-delete (loop for loc-assoc in (slot-p construct 'locators)
			      when (eql (identifier loc-assoc) locator)
			      return loc-assoc)))
      (when assoc-to-delete
	(mark-as-deleted assoc-to-delete :revision revision)
	construct))))


(defgeneric delete-locator (construct locator &key revision)
  (:documentation "See private-delete-locator but add the parent construct
                   to the given version.")
  (:method ((construct TopicC) (locator SubjectLocatorC)
	    &key (revision (error (make-missing-argument-condition "From delete-locator(): revision must be set" 'revision  'delete-locator))))
    (when (private-delete-locator construct locator :revision revision)
      (add-to-version-history construct :start-revision revision)
      construct)))


(defmethod get-all-identifiers-of-construct ((construct TopicC)
					     &key (revision *TM-REVISION*))
  (declare (integer revision))
  (append (psis construct :revision revision)
          (locators construct :revision revision)
          (item-identifiers construct :revision revision)))


(defgeneric names (construct &key revision)
  (:documentation "Returns the NameC-objects that correspond
                   with the passed construct and the passed version.")
  (:method ((construct TopicC) &key (revision *TM-REVISION*))
    (let ((assocs (filter-slot-value-by-revision
		   construct 'names :start-revision revision)))
      (map 'list #'characteristic assocs))))


(defgeneric add-name (construct name &key revision)
  (:documentation "Adds the passed name to the passed topic.
                   If the name is already related with the passed
                   topic a new revision is added.
                   If the passed name already owns another object
                   an error is thrown.")
  (:method ((construct TopicC) (name NameC)
	    &key (revision *TM-REVISION*))
    (when (and (parent name :revision revision)
	       (not (eql (parent name :revision revision) construct)))
      (error (make-tm-reference-condition (format nil "From add-name(): ~a can't be owned by ~a since it is already owned by the topic ~a"
						  name construct (parent name :revision revision))
					  name (parent name :revision revision) construct)))
    (if (merge-if-equivalent name construct :revision revision)
	construct
	(let ((all-names
	       (map 'list #'characteristic (slot-p construct 'names))))
	  (if (find name all-names)
	      (let ((name-assoc 
		     (loop for name-assoc in (slot-p construct 'names)
			when (eql (parent-construct name-assoc)
				  construct)
			return name-assoc)))
		(add-to-version-history name-assoc :start-revision revision))
	      (make-construct 'NameAssociationC
			      :parent-construct construct
			      :characteristic name
			      :start-revision revision))
	  (add-to-version-history construct :start-revision revision)
	  construct))))


(defgeneric private-delete-name (construct name &key revision)
  (:documentation "Sets the association object between the passed constructs
                   as mark-as-deleted.")
  (:method ((construct TopicC) (name NameC)
	    &key (revision (error (make-missing-argument-condition "From private-delete-name(): revision must be set" 'revision 'private-delete-name))))
    (let ((assoc-to-delete (loop for name-assoc in (slot-p construct 'names)
			      when (eql (characteristic name-assoc) name)
			      return name-assoc)))
      (when assoc-to-delete
	(mark-as-deleted assoc-to-delete :revision revision)
	construct))))


(defgeneric delete-name (construct name &key revision)
  (:documentation "See private-delete-name but adds the parent to
                   the given version.")
  (:method ((construct TopicC) (name NameC)
	    &key (revision (error (make-missing-argument-condition "From delete-name(): revision must be set" 'revision 'delete-name))))
    (when (private-delete-name construct name :revision revision)
      (add-to-version-history construct :start-revision revision)
      construct)))


(defgeneric occurrences (construct &key revision)
  (:documentation "Returns the OccurrenceC-objects that correspond
                   with the passed construct and the passed version.")
  (:method ((construct TopicC) &key (revision *TM-REVISION*))
    (let ((assocs (filter-slot-value-by-revision
		   construct 'occurrences :start-revision revision)))
      (map 'list #'characteristic assocs))))


(defgeneric add-occurrence (construct occurrence &key revision)
  (:documentation "Adds the passed occurrence to the passed topic.
                   If the occurrence is already related with the passed
                   topic a new revision is added.
                   If the passed occurrence already owns another object
                   an error is thrown.")
  (:method ((construct TopicC) (occurrence OccurrenceC)
	    &key (revision *TM-REVISION*))
    (when (and (parent occurrence :revision revision)
	       (not (eql (parent occurrence :revision revision) construct)))
      (error (make-tm-reference-condition (format nil "From add-occurrence(): ~a can't be owned by ~a since it is already owned by the topic ~a"
						  occurrence construct (parent occurrence :revision revision))
					  occurrence (parent occurrence :revision revision) construct)))
    (if (merge-if-equivalent occurrence construct :revision revision)
	construct
	(let ((all-occurrences
	       (map 'list #'characteristic (slot-p construct 'occurrences))))
	  (if (find occurrence all-occurrences)
	      (let ((occ-assoc
		     (loop for occ-assoc in (slot-p construct 'occurrences)
			when (eql (parent-construct occ-assoc) construct)
			return occ-assoc)))
		(add-to-version-history occ-assoc :start-revision revision))
	      (make-construct 'OccurrenceAssociationC
			      :parent-construct construct
			      :characteristic occurrence
			      :start-revision revision))
	  (add-to-version-history construct :start-revision revision)
	  construct))))


(defgeneric private-delete-occurrence (construct occurrence &key revision)
  (:documentation "Sets the association object between the passed constructs
                   as mark-as-deleted.")
  (:method ((construct TopicC) (occurrence OccurrenceC)
	    &key (revision (error (make-missing-argument-condition "From private-delete-occurrence(): revision must be set" 'revision 'private-delete-occurrence))))
    (let ((assoc-to-delete (loop for occ-assoc in (slot-p construct 'occurrences)
			      when (eql (characteristic occ-assoc) occurrence)
			      return occ-assoc)))
      (when assoc-to-delete
	(mark-as-deleted assoc-to-delete :revision revision)
	construct))))


(defgeneric delete-occurrence (construct occurrence &key revision)
  (:documentation "See private-delete-occurrence but adds the parent
                   to the given version history.")
  (:method ((construct TopicC) (occurrence OccurrenceC)
	    &key (revision (error (make-missing-argument-condition "From delete-occurrence(): revision must be set" 'revision 'delete-occurrence))))
    (when (private-delete-occurrence construct occurrence :revision revision)
      (add-to-version-history construct :start-revision revision)
      construct)))


(defmethod add-characteristic ((construct TopicC)
			       (characteristic CharacteristicC)
			       &key (revision *TM-REVISION*))
  (declare (integer revision) (type (or NameC OccurrenceC) characteristic))
  (if (typep characteristic 'NameC)
      (add-name construct characteristic :revision revision)
      (add-occurrence construct characteristic :revision revision)))


(defmethod private-delete-characteristic ((construct TopicC)
					  (characteristic CharacteristicC)
					  &key (revision (error (make-missing-argument-condition "From private-delete-characteristic(): revision must be set" 'revision 'private-delete-characteristic))))
  (declare (integer revision) (type (or NameC OccurrenceC) characteristic))
  (if (typep characteristic 'NameC)
      (private-delete-name construct characteristic :revision revision)
      (private-delete-occurrence construct characteristic
				 :revision revision)))


(defmethod delete-characteristic ((construct TopicC)
				  (characteristic CharacteristicC)
				  &key (revision (error (make-missing-argument-condition "From delete-characteristic(): revision must be set" 'revision 'delete-characteristic))))
  (declare (integer revision) (type (or NameC OccurrenceC) characteristic))
  (if (typep characteristic 'NameC)
      (delete-name construct characteristic :revision revision)
      (delete-occurrence construct characteristic :revision revision)))


(defgeneric player-in-roles (construct &key revision)
  (:documentation "Returns the RoleC-objects that correspond
                   with the passed construct and the passed version.")
  (:method ((construct TopicC) &key (revision *TM-REVISION*))
    (let ((assocs (filter-slot-value-by-revision
		   construct 'player-in-roles :start-revision revision)))
      (map 'list #'parent-construct assocs))))


(defgeneric used-as-type (construct &key revision)
  (:documentation "Returns the TypableC-objects that correspond
                   with the passed construct and the passed version.")
  (:method ((construct TopicC) &key (revision *TM-REVISION*))
    (let ((assocs (filter-slot-value-by-revision
		   construct 'used-as-type :start-revision revision)))
      (map 'list #'typable-construct assocs))))


(defgeneric used-as-theme (construct &key revision)
  (:documentation "Returns the ScopableC-objects that correspond
                   with the passed construct and the passed version.")
  (:method ((construct TopicC) &key (revision *TM-REVISION*))
    (let ((assocs (filter-slot-value-by-revision
		   construct 'used-as-theme :start-revision revision)))
      (map 'list #'scopable-construct assocs))))


(defgeneric reified-construct (construct &key revision)
  (:documentation "Returns the ReifiableConstructC-objects that correspond
                   with the passed construct and the passed version.")
  (:method ((construct TopicC) &key (revision *TM-REVISION*))
    (let ((assocs (filter-slot-value-by-revision
		   construct 'reified-construct :start-revision revision)))
      (when assocs
	(reifiable-construct (first assocs))))))


(defgeneric add-reified-construct (construct reified-construct &key revision)
  (:documentation "Sets the passed construct as reified-consturct of the given
                   topic.")
  (:method ((construct TopicC) (reified-construct ReifiableConstructC)
	    &key (revision *TM-REVISION*))
    (declare (integer revision))
    (add-reifier reified-construct construct :revision revision)))


(defgeneric private-delete-reified-construct
    (construct reified-construct &key revision)
  (:documentation "Unsets the passed construct as reified-construct of the
                   given topic.")
  (:method ((construct TopicC) (reified-construct ReifiableConstructC)
	    &key (revision (error (make-missing-argument-condition "From private-delete-reified-construct(): revision must be set" 'revision 'private-delete-reified-construct))))
    (declare (integer revision))
    (private-delete-reifier reified-construct construct
			    :revision revision)))


(defgeneric delete-reified-construct (construct reified-construct &key revision)
  (:documentation "See private-delete-reified-construct but adds the
                   reifier to the given version.")
  (:method ((construct TopicC) (reified-construct ReifiableConstructC)
	    &key (revision (error (make-missing-argument-condition "From -delete-reified-construct(): revision must be set" 'revision '-delete-reified-construct))))
    (declare (integer revision))
    (delete-reifier reified-construct construct :revision revision)))


(defmethod in-topicmaps ((topic TopicC) &key (revision *TM-REVISION*))
  (filter-slot-value-by-revision topic 'in-topicmaps :start-revision revision))


(defun get-item-by-id (topic-id &key (xtm-id *CURRENT-XTM*)
		       (revision *TM-REVISION*) (error-if-nil nil))
  "Gets a topic by its id, assuming an xtm-id. If xtm-id is empty, the current TM
   is chosen. If xtm-id is nil, choose the global TM with its internal ID, if
   applicable in the correct revision. If revison is provided, then the code checks
   if the topic already existed in this revision and returns nil otherwise.
   If no item meeting the constraints was found, then the return value is either
   NIL or an error is thrown, depending on error-if-nil."
  (declare (string topic-id) (integer revision))
  (let ((result
	 (if xtm-id
	     (let ((possible-top-ids
		    (delete-if-not
		     #'(lambda(top-id)
			 (and (typep top-id 'd:TopicIdentificationC)
			      ;fixes a bug in elephant -> all PointerCs are returned
			      (string= (xtm-id top-id) xtm-id)
			      (string= (uri top-id) topic-id)))
		     ;fixes a bug in get-instances-by-value that does a
		     ;case-insensitive comparision
		     (elephant:get-instances-by-value
		      'TopicIdentificationC
		      'uri topic-id))))
	       (when (and possible-top-ids
			  (identified-construct (first possible-top-ids)
						:revision revision))
		 (unless (= (length possible-top-ids) 1)
		   (error (make-duplicate-identifier-condition
			   (format nil "(length possible-items ~a) for id ~a and xtm-id ~a > 1"
				   possible-top-ids topic-id xtm-id)
			   topic-id)))
		 (identified-construct (first possible-top-ids)
				       :revision revision)
 	 	 ;no revision need not to be checked, since the revision
                 ;is implicitely checked by the function identified-construct
		 ))
	     (when (and (> (length topic-id) 0)
			(eql (elt topic-id 0) #\t)
			(string-integer-p (subseq topic-id 1)))
	       (let ((top-from-oid
		      (elephant::controller-recreate-instance
		       elephant::*store-controller*
		       (parse-integer (subseq topic-id 1)))))
		 (when (find-item-by-revision top-from-oid revision)
		   top-from-oid))))))
    (if (and error-if-nil (not result))
        (error (make-object-not-found-condition (format nil "No such item (id: ~a, tm: ~a, rev: ~a)" topic-id xtm-id revision)))
        result)))


(defun get-item-by-identifier (uri &key (revision *TM-REVISION*)
			       (identifier-type-symbol 'PersistentIdC)
			       (error-if-nil nil))
  "Returns the construct that is bound to the given identifier-uri."
  (declare (string uri) (integer revision) (symbol identifier-type-symbol))
  (let ((result
	 (let ((possible-ids
		(delete-if-not
		 #'(lambda(id)
		     (and (typep id identifier-type-symbol)
			  (string= (uri id) uri)))
		 (get-instances-by-value identifier-type-symbol 'uri uri))))
	   (when (and possible-ids
		      (identified-construct (first possible-ids)
					    :revision revision))
	     (unless (= (length possible-ids) 1)
	       (error (make-duplicate-identifier-condition (format nil "(length possible-items ~a) for id ~a" possible-ids uri) uri)))
	     (identified-construct (first possible-ids)
				   :revision revision)))))
	     ;no revision need to be checked, since the revision
             ;is implicitely checked by the function identified-construct
    (if (and result
	     (let ((parent-elem
		    (when (or (typep result 'CharacteristicC)
			      (typep result 'RoleC))
		      (parent result :revision revision))))
	       (find-item-by-revision result revision parent-elem)))
	result
	(when error-if-nil
	  (error (make-object-not-found-condition "No such item is bound to the given identifier uri."))))))


(defun get-item-by-item-identifier (uri &key (revision *TM-REVISION*)
				    (error-if-nil nil))
  "Returns a ReifiableConstructC that is bound to the identifier-uri."
  (get-item-by-identifier uri :revision revision
			  :identifier-type-symbol 'ItemIdentifierC
			  :error-if-nil error-if-nil))


(defun get-item-by-psi (uri &key (revision *TM-REVISION*) (error-if-nil nil))
  "Returns a TopicC that is bound to the identifier-uri."
  (get-item-by-identifier uri :revision revision
			  :identifier-type-symbol 'PersistentIdC
			  :error-if-nil error-if-nil))


(defun get-item-by-locator (uri &key (revision *TM-REVISION*) (error-if-nil nil))
  "Returns a TopicC that is bound to the identifier-uri."
  (get-item-by-identifier uri :revision revision
			  :identifier-type-symbol 'SubjectLocatorC
			  :error-if-nil error-if-nil))


(defgeneric list-instanceOf (topic &key tm revision)
 (:documentation "Generates a list of all topics that this topic is an
                  instance of, optionally filtered by a topic map")
 (:method ((topic TopicC) &key (tm nil) (revision *TM-REVISION*))
   (declare (type (or null TopicMapC) tm)
	    (integer revision))
   (remove-if 
    #'null
    (map 'list
	 #'(lambda(x)
	     (when (and (parent x :revision revision)
			(instance-of x :revision revision)
			(loop for psi in (psis (instance-of x :revision revision)
					       :revision revision)
			   when (string= (uri psi) constants:*instance-psi*)
			   return t))
	       (loop for role in (roles (parent x :revision revision)
					:revision revision)
		  when (not (eq role x))
		  return (player role :revision revision))))
	 (if tm
	     (remove-if-not 
	      (lambda (role)
		(in-topicmap tm (parent role :revision revision)
			     :revision revision))
	      (player-in-roles topic :revision revision))
	     (player-in-roles topic :revision revision))))))
 

(defgeneric list-super-types (topic &key tm revision)
 (:documentation "Generate a list of all topics that this topic is an
  subclass of, optionally filtered by a topic map")
 (:method ((topic TopicC)  &key (tm nil) (revision *TM-REVISION*))
   (declare (type (or null TopicMapC) tm)
	    (integer revision))
   (remove-if 
    #'null
    (map 'list
	 #'(lambda(x)
	     (when (loop for psi in (psis (instance-of x :revision revision)
					  :revision revision)
		      when (string= (uri psi) *subtype-psi*)
		      return t)
	       (loop for role in (roles (parent x :revision revision)
					:revision revision)
		  when (not (eq role x))
		  return (player role :revision revision))))
	 (if tm
	     (remove-if-not 
	      (lambda (role)
		(in-topicmap tm (parent role :revision revision)
			     :revision revision))
	      (player-in-roles topic :revision revision))
	     (player-in-roles topic :revision revision))))))


;;; CharacteristicC
(defmethod versions ((construct CharacteristicC))
  "Returns all versions that are indirectly through all
   CharacteristicAssocitiations bound to the passed characteristic object."
  (loop for p-assoc in (slot-p construct 'parent)
     append (versions p-assoc)))


(defmethod mark-as-deleted ((construct CharacteristicC) &key source-locator revision)
  "Marks the last active relation between a characteristic and its parent topic
   as deleted."
  (declare (ignorable source-locator))
  (let ((owner (parent construct :revision 0)))
    (when owner
      (private-delete-characteristic owner construct :revision revision))))


(defmethod marked-as-deleted-p ((construct CharacteristicC))
  (unless (parent construct :revision 0)
    t))


(defmethod find-self-or-equal ((construct CharacteristicC)
			       (parent-construct TopicC)
			       &key (revision *TM-REVISION*))
  (declare (integer revision) (type (or OccurrenceC NameC) construct))
  (let ((chars (if (typep construct 'OccurrenceC)
		   (occurrences parent-construct :revision revision)
		   (names parent-construct :revision revision))))
    (let ((self (find construct chars)))
      (if self
	  self
	  (let ((equal-char
		 (remove-if #'null
			    (map 'list
				 #'(lambda(char)
				     (strictly-equivalent-constructs
				      char construct :revision revision))
				 chars))))
	    (when equal-char
	      (first equal-char)))))))


(defmethod delete-if-not-referenced ((construct CharacteristicC))
  (let ((references (slot-p construct 'parent)))
    (when (or (not references)
	      (and (= (length references) 1)
		   (marked-as-deleted-p (first references))))
      (delete-construct construct))))


(defmethod find-oldest-construct ((construct-1 CharacteristicC)
				  (construct-2 CharacteristicC))
  (let ((vi-1 (find-version-info (slot-p construct-1 'parent)))
	(vi-2 (find-version-info (slot-p construct-2 'parent))))
    (cond ((not (or vi-1 vi-2))
	   construct-1)
	  ((not vi-1)
	   construct-2)
	  ((not vi-2)
	   construct-1)
	  ((<= (start-revision vi-1) (start-revision vi-2))
	   construct-1)
	  (t
	   construct-2))))


(defmethod equivalent-constructs ((construct-1 CharacteristicC)
				  (construct-2 CharacteristicC)
				  &key (revision *TM-REVISION*))
  (declare (integer revision))
  (and (string= (charvalue construct-1) (charvalue construct-2))
       (eql (instance-of construct-1 :revision revision)
	    (instance-of construct-2 :revision revision))
       (not (set-exclusive-or (themes construct-1 :revision revision)
			      (themes construct-2 :revision revision)))))


(defgeneric CharacteristicC-p (class-symbol)
  (:documentation "Returns t if the passed symbol is equal to CharacteristicC
                   or one of its subtypes.")
  (:method ((class-symbol symbol))
    (or (eql class-symbol 'CharacteristicC)
	(OccurrenceC-p class-symbol)
	(NameC-p class-symbol)
	(VariantC-p class-symbol))))


(defmethod equivalent-construct ((construct CharacteristicC)
				 &key (start-revision *TM-REVISION*)
				 (charvalue "") (instance-of nil) (themes nil))
  "Equality rule: Characteristics are equal if charvalue, themes and
    instance-of are equal."
  (declare (string charvalue) (list themes)
	   (integer start-revision)
	   (type (or null TopicC) instance-of))
  ;; item-identifiers and reifers are not checked because the equality have to
  ;; be variafied without them
  (and (string= (charvalue construct) charvalue)
       (equivalent-scopable-construct construct themes
				      :start-revision start-revision)
       (equivalent-typable-construct construct instance-of
				     :start-revision start-revision)))


(defmethod find-item-by-revision ((construct CharacteristicC)
				  (revision integer) &optional parent-construct)
  (if parent-construct
      (let ((parent-assoc
	     (let ((assocs
		    (remove-if
		     #'null
		     (map 'list #'(lambda(assoc)
				    (when (eql (parent-construct assoc)
					       parent-construct)
				      assoc))
			  (slot-p construct 'parent)))))
	       (when assocs
		 (first assocs)))))
	(when parent-assoc
	  (cond ((= revision 0)
		 (when
		     (find-most-recent-revision parent-assoc)
		   construct))
		(t
		 (when (find-if
			#'(lambda(vi)
			    (and (>= revision (start-revision vi))
				 (or (< revision (end-revision vi))
				     (= 0 (end-revision vi)))))
			(versions parent-assoc))
		   construct)))))
      nil))


(defmethod delete-construct :before ((construct CharacteristicC))
  (dolist (characteristic-assoc-to-delete (slot-p construct 'parent))
    (delete-construct characteristic-assoc-to-delete)))


(defmethod owned-p ((construct CharacteristicC))
  (when (slot-p construct 'parent)
    t))


(defmethod parent ((construct CharacteristicC) &key (revision *TM-REVISION*))
  (let ((valid-associations
	 (filter-slot-value-by-revision construct 'parent
					:start-revision revision)))
    (when valid-associations
      (parent-construct (first valid-associations)))))


(defmethod add-parent ((construct CharacteristicC)
		       (parent-construct ReifiableConstructC)
		       &key (revision *TM-REVISION*))
  (declare (integer revision))
  (let ((already-set-parent (parent construct :revision revision))
	(same-parent-assoc ;should contain an object that was marked as deleted
	 (loop for parent-assoc in (slot-p construct 'parent)
	    when (eql parent-construct (parent-construct parent-assoc))
	    return parent-assoc)))
    (when (and already-set-parent
	       (not (eql already-set-parent parent-construct)))
      (error (make-tm-reference-condition (format nil "From add-parent(): ~a can't be owned by ~a since it is already owned by ~a"
					      construct parent-construct already-set-parent)
					  construct (parent construct :revision revision) parent-construct)))
    (let ((merged-char
	   (merge-if-equivalent construct parent-construct :revision revision)))
      (if merged-char
	  merged-char
	  (progn
	    (cond (already-set-parent
		   (let ((parent-assoc
			  (loop for parent-assoc in (slot-p construct 'parent)
			     when (eql parent-construct
				       (parent-construct parent-assoc))
			     return parent-assoc)))
		     (add-to-version-history parent-assoc
					     :start-revision revision)))
		  (same-parent-assoc
		   (add-to-version-history same-parent-assoc
					   :start-revision revision))
		  (t
		   (let ((association-type (cond ((typep construct 'OccurrenceC)
						  'OccurrenceAssociationC)
						 ((typep construct 'NameC)
						  'NameAssociationC)
						 (t
						  'VariantAssociationC))))
		     (make-construct association-type
				     :characteristic construct
				     :parent-construct parent-construct
				     :start-revision revision))))
	    (when (typep parent-construct 'VersionedConstructC)
	      (add-to-version-history parent-construct :start-revision revision))
	    construct)))))


(defmethod private-delete-parent ((construct CharacteristicC)
				  (parent-construct ReifiableConstructC)
				  &key (revision (error (make-missing-argument-condition "From private-delete-parent(): revision must be set" 'revision 'private-delete-parent))))
  (let ((assoc-to-delete
	 (loop for parent-assoc in (slot-p construct 'parent)
	    when (eql (parent-construct parent-assoc) parent-construct)
	    return parent-assoc)))
    (when assoc-to-delete
      (mark-as-deleted assoc-to-delete :revision revision)
      construct)))


(defmethod delete-parent ((construct CharacteristicC)
			  (parent-construct ReifiableConstructC)
			  &key (revision (error (make-missing-argument-condition "From delete-parent(): revision must be set" 'revision 'delete-parent))))
  (let ((parent (parent construct :revision revision)))
    (when (private-delete-parent construct parent-construct :revision revision)
      (when parent
	(add-version-info parent revision))
      construct)))


;;; OccurrenceC
(defmethod equivalent-constructs ((construct-1 OccurrenceC) (construct-2 OccurrenceC)
				  &key (revision *TM-REVISION*))
  (declare (ignorable revision))
  (and (call-next-method)
       (string= (datatype construct-1) (datatype construct-2))))


(defgeneric OccurrenceC-p (class-symbol)
  (:documentation "Returns t if the passed symbol is equal to OccurrenceC.")
  (:method ((class-symbol symbol))
    (eql class-symbol 'OccurrenceC)))


(defmethod equivalent-construct ((construct OccurrenceC)
				 &key (start-revision *TM-REVISION*)
				 (charvalue "") (themes nil) (instance-of nil)
				 (datatype ""))
  "Occurrences are equal if their charvalue, datatype, themes and
    instance-of properties are equal."
  (declare (type (or null TopicC) instance-of) (string datatype)
	   (ignorable start-revision charvalue themes instance-of))
  (let ((equivalent-characteristic (call-next-method)))
    ;; item-identifiers and reifers are not checked because the equaity have to
    ;; be variafied without them
    (and equivalent-characteristic
	 (string= (datatype construct) datatype))))


;;; VariantC
(defmethod find-self-or-equal ((construct VariantC) (parent-construct NameC)
			       &key (revision *TM-REVISION*))
  (declare (integer revision))
  (let ((vars (variants parent-construct :revision revision)))
    (let ((self (find construct vars)))
      (if self
	  self
	  (let ((equal-var
		 (remove-if #'null
			    (map 'list
				 #'(lambda(var)
				     (strictly-equivalent-constructs
				      var construct :revision revision))
				 vars))))
	    (when equal-var
	      (first equal-var)))))))


(defmethod equivalent-constructs ((construct-1 VariantC) (construct-2 VariantC)
				  &key (revision *TM-REVISION*))
  (declare (ignorable revision))
  (and (call-next-method)
       (string= (datatype construct-1) (datatype construct-2))))


(defgeneric VariantC-p (class-symbol)
  (:documentation "Returns t if the passed symbol is equal to VariantC.")
  (:method ((class-symbol symbol))
    (eql class-symbol 'VariantC)))


(defmethod equivalent-construct ((construct VariantC)
				 &key (start-revision *TM-REVISION*)
				 (charvalue "") (themes nil) (datatype ""))
  "Variants are equal if their charvalue, datatype and themes
   properties are equal."
  (declare (string datatype) (ignorable start-revision charvalue themes))
  ;; item-identifiers and reifers are not checked because the equality have to
  ;; be variafied without them
  (let ((equivalent-characteristic (call-next-method)))
    (and equivalent-characteristic 
	 (string= (datatype construct) datatype))))


;;; NameC
(defmethod get-all-characteristics ((parent-construct NameC)
				    (characteristic-symbol symbol))
  (when (VariantC-p characteristic-symbol)
    (map 'list #'characteristic (slot-p parent-construct 'variants))))


(defgeneric NameC-p (class-symbol)
  (:documentation "Returns t if the passed symbol is equal to Name.")
  (:method ((class-symbol symbol))
    (eql class-symbol 'NameC)))


(defgeneric complete-name (construct variants &key start-revision)
  (:documentation "Adds all given variants to the passed construct.")
  (:method ((construct NameC) (variants list)
	    &key (start-revision *TM-REVISION*))
    (dolist (variant variants)
      (add-variant construct variant :revision start-revision))
    construct))


(defmethod equivalent-construct ((construct NameC)
				 &key (start-revision *TM-REVISION*)
				 (charvalue "") (themes nil) (instance-of nil))
  "Names are equal if their charvalue, instance-of and themes properties
   are equal."
  (declare (type (or null TopicC) instance-of)
	   (ignorable start-revision charvalue instance-of themes))
  (call-next-method))
  

(defmethod delete-construct :before ((construct NameC))
  (let ((variant-assocs-to-delete (slot-p construct 'variants)))
    (let ((all-variants (map 'list #'characteristic variant-assocs-to-delete)))
      (dolist (variant-assoc-to-delete variant-assocs-to-delete)
	(delete-construct variant-assoc-to-delete))
      (dolist (candidate-to-delete all-variants)
	(unless (owned-p candidate-to-delete)
	  (delete-construct candidate-to-delete))))))


(defgeneric variants (construct &key revision)
  (:documentation "Returns all variants that correspond with the given revision
                   and that are associated with the passed construct.")
  (:method ((construct NameC) &key (revision *TM-REVISION*))
    (let ((valid-associations
	   (filter-slot-value-by-revision construct 'variants
					  :start-revision revision)))
      (map 'list #'characteristic valid-associations))))


(defgeneric add-variant (construct variant &key revision)
  (:documentation "Adds the given theme-topic to the passed
                   scopable-construct.")
  (:method ((construct NameC) (variant VariantC)
	    &key (revision *TM-REVISION*))
    (when (and (parent variant :revision revision)
	       (not (eql (parent variant :revision revision) construct)))
      (error (make-tm-reference-condition (format nil "From add-variant(): ~a can't be owned by ~a since it is already owned by the name ~a"
						  variant construct (parent variant :revision revision))
					  variant (parent variant :revision revision) construct)))
    (if (merge-if-equivalent variant construct :revision revision)
	construct
	(let ((all-variants 
	       (map 'list #'characteristic (slot-p construct 'variants))))
	  (if (find variant all-variants)
	      (let ((variant-assoc
		     (loop for variant-assoc in (slot-p construct 'variants)
			when (eql (characteristic variant-assoc) variant)
			return variant-assoc)))
		(add-to-version-history variant-assoc :start-revision revision))
	      (make-construct 'VariantAssociationC
			      :characteristic variant
			      :parent-construct construct
			      :start-revision revision))
	  (when (parent construct :revision revision)
	    (add-name (parent construct :revision revision)  construct
		      :revision revision))
	  construct))))


(defgeneric private-delete-variant (construct variant &key revision)
  (:documentation "Deletes the passed variant by marking it's association as
                   deleted in the passed revision.")
  (:method ((construct NameC) (variant VariantC)
	    &key (revision (error (make-missing-argument-condition "From private-delete-variant(): revision must be set" 'revision 'private-delete-variant))))
    (let ((assoc-to-delete (loop for variant-assoc in (slot-p construct
							      'variants)
			      when (eql (characteristic variant-assoc) variant)
			      return variant-assoc)))
      (when assoc-to-delete
	(mark-as-deleted assoc-to-delete :revision revision)
	construct))))


(defgeneric delete-variant (construct variant &key revision)
  (:documentation "See private-delete-variant but adds a the parent
                   and the parent's parent to the given version history.")
  (:method ((construct NameC) (variant VariantC)
	    &key (revision (error (make-missing-argument-condition "From delete-variant(): revision must be set" 'revision 'delete-variant))))
    (when (private-delete-variant construct variant :revision revision)
      (when (parent construct :revision revision)
	(add-name (parent construct :revision revision) construct
		  :revision revision)
	construct))))


(defmethod add-characteristic ((construct NameC) (characteristic VariantC)
			       &key (revision *TM-REVISION*))
  (declare (integer revision))
  (add-variant construct characteristic :revision revision))


(defmethod private-delete-characteristic  ((construct NameC) (characteristic VariantC)
					   &key (revision (error (make-missing-argument-condition "From private-delete-characteristic(): revision must be set" 'revision 'private-delete-characteristic))))
  (declare (integer revision))
  (private-delete-variant construct characteristic :revision revision))


(defmethod delete-characteristic  ((construct NameC) (characteristic VariantC)
					   &key (revision (error (make-missing-argument-condition "From delete-characteristic(): revision must be set" 'revision 'delete-characteristic))))
  (declare (integer revision))
  (delete-variant construct characteristic :revision revision))


;;; AssociationC
(defmethod mark-as-deleted :around ((ass AssociationC) &key source-locator revision)
  "Marks an association and its roles as deleted"
  (mapc (lambda (role)
	  (mark-as-deleted role :revision revision :source-locator source-locator))
        (roles ass :revision 0))
  (call-next-method))


(defmethod equivalent-constructs ((construct-1 AssociationC)
				  (construct-2 AssociationC)
				  &key (revision *TM-REVISION*))
  (declare (ignorable revision))
  (and (eql (instance-of construct-1 :revision revision)
	    (instance-of construct-2 :revision revision))
       (not (set-exclusive-or (themes construct-1 :revision revision)
			      (themes construct-2 :revision revision)))

       (not (set-exclusive-or
	     (roles construct-1 :revision revision)
	     (roles construct-2 :revision revision)
	     :test #'(lambda(role-1 role-2)
		       (strictly-equivalent-constructs role-1 role-2
						       :revision revision))))))


(defgeneric AssociationC-p (class-symbol)
  (:documentation "Returns t if the passed symbol is equal to AssociationC.")
  (:method ((class-symbol symbol))
    (eql class-symbol 'AssociationC)))


(defmethod equivalent-construct ((construct AssociationC)
				 &key (start-revision *TM-REVISION*)
				 (roles nil) (instance-of nil) (themes nil))
  "Associations are equal if their themes, instance-of and roles
   properties are equal.
   To avoid ceation of duplicate roles the parameter roles is a list of plists
   of the form: ((:player <TopicC> :instance-of <TopicC>
   :item-identifiers <(ItemIdentifierC)> :reifier <TopicC>))."
  (declare (integer start-revision) (list roles themes)
	   (type (or null TopicC) instance-of))
  ;; item-identifiers and reifers are not checked because the equality have to
  ;; be variafied without them
  (let ((checked-roles nil))
    (loop for plist in roles
       do (let ((found-role
		 (find-if #'(lambda(assoc-role)
			      (equivalent-construct
			       assoc-role :player (getf plist :player)
			       :start-revision (or (getf plist :start-revision)
						   start-revision)
			       :instance-of (getf plist :instance-of)))
			  (roles construct :revision start-revision))))
	    (when found-role
	      (push found-role checked-roles))))
    (and
     (not (set-exclusive-or (roles construct :revision start-revision)
			    checked-roles))
     (= (length checked-roles) (length roles))
     (equivalent-typable-construct construct instance-of
				   :start-revision start-revision)
     (equivalent-scopable-construct construct themes
				    :start-revision start-revision))))


(defmethod delete-construct :before ((construct AssociationC))
  (let ((roles-assocs-to-delete (slot-p construct 'roles)))
    (let ((all-roles (map 'list #'role roles-assocs-to-delete)))
      (dolist (role-assoc-to-delete roles-assocs-to-delete)
	(delete-construct role-assoc-to-delete))
      (dolist (candidate-to-delete all-roles)
	(unless (owned-p candidate-to-delete)
	  (delete-construct candidate-to-delete)))
      (dolist (tm (slot-p construct 'in-topicmaps))
	(remove-association construct 'in-topicmaps tm)))))


(defmethod owned-p ((construct AssociationC))
  (when (slot-p construct 'in-topicmaps)
    t))


(defgeneric roles (construct &key revision)
  (:documentation "Returns all topics that correspond with the given revision
                   as a scope for the given topic.")
  (:method ((construct AssociationC) &key (revision *TM-REVISION*))
    (let ((valid-associations
	   (filter-slot-value-by-revision construct 'roles
					  :start-revision revision)))
      (map 'list #'role valid-associations))))


(defgeneric add-role (construct role &key revision)
  (:documentation "Adds the given role to the passed association-construct.")
  (:method ((construct AssociationC) (role RoleC)
	    &key (revision *TM-REVISION*))
    (if (merge-if-equivalent role construct :revision revision)
	construct
	(let ((all-roles
	       (map 'list #'role  (slot-p construct 'roles))))
	  (if (find role all-roles)
	      (let ((role-assoc
		     (loop for role-assoc in (slot-p construct 'roles)
			when (eql (role role-assoc) role)
			return role-assoc)))
		(add-to-version-history role-assoc  :start-revision revision))
	      (make-construct 'RoleAssociationC
			      :role role
			      :parent-construct construct
			      :start-revision revision))
	  (add-to-version-history construct :start-revision revision)
	  construct))))


(defgeneric private-delete-role (construct role &key revision)
  (:documentation "Deletes the passed role by marking it's association as
                   deleted in the passed revision.")
  (:method ((construct AssociationC) (role RoleC)
	    &key (revision (error (make-missing-argument-condition "From private-delete-role(): revision must be set" 'revision 'private-delete-role))))
    (let ((assoc-to-delete (loop for role-assoc in (slot-p construct 'roles)
			      when (eql (role role-assoc) role)
			      return role-assoc)))
      (when assoc-to-delete
	(mark-as-deleted assoc-to-delete :revision revision)
	construct))))


(defgeneric delete-role (construct role &key revision)
  (:documentation "See private-delete-role but adds the parent association
                   to the given version.")
  (:method ((construct AssociationC) (role RoleC)
	    &key (revision (error (make-missing-argument-condition "From delete-role(): revision must be set" 'revision 'delete-role))))
    (when (private-delete-role construct role :revision revision)
      (add-to-version-history construct :start-revision revision)
      construct)))


(defmethod in-topicmaps ((association AssociationC) &key (revision *TM-REVISION*))
  (filter-slot-value-by-revision association 'in-topicmaps :start-revision revision))


;;; RoleC
(defmethod mark-as-deleted ((construct RoleC) &key source-locator revision)
  "Marks the last active relation between a role and its parent association
   as deleted."
  (declare (ignorable source-locator))
  (let ((owner (parent construct :revision 0)))
    (when owner
      ;(private-delete-player construct (player construct :revision revision)
      ;:revision revision)
      (private-delete-role owner construct :revision revision))))


(defmethod marked-as-deleted-p ((construct RoleC))
  (unless (parent construct :revision 0)
    t))


(defmethod find-self-or-equal ((construct RoleC) (parent-construct AssociationC)
			       &key (revision *TM-REVISION*))
  (declare (integer revision))
  (let ((p-roles (roles parent-construct :revision revision)))
    (let ((self (find construct p-roles)))
      (if self
	  self
	  (let ((equal-role
		 (remove-if #'null
			    (map 'list
				 #'(lambda(role)
				     (strictly-equivalent-constructs
				      role construct :revision revision))
				 p-roles))))
	    (when equal-role
	      (first equal-role)))))))


(defmethod delete-if-not-referenced ((construct RoleC))
  (let ((references (slot-p construct 'parent)))
    (when (or (not references)
	      (and (= (length references) 1)
		   (marked-as-deleted-p (first references))))
      (delete-construct construct))))


(defmethod find-oldest-construct ((construct-1 RoleC) (construct-2 RoleC))
  (let ((vi-1 (find-version-info (slot-p construct-1 'parent)))
	(vi-2 (find-version-info (slot-p construct-2 'parent))))
    (cond ((not (or vi-1 vi-2))
	   construct-1)
	  ((not vi-1)
	   construct-2)
	  ((not vi-2)
	   construct-1)
	  ((<= (start-revision vi-1) (start-revision vi-2))
	   construct-1)
	  (t
	   construct-2))))


(defmethod equivalent-constructs ((construct-1 RoleC) (construct-2 RoleC)
				  &key (revision *TM-REVISION*))
  (declare (integer revision))
  (and (eql (instance-of construct-1 :revision revision)
	    (instance-of construct-2 :revision revision))
       (eql (player construct-1 :revision revision)
	    (player construct-2 :revision revision))))


(defgeneric RoleC-p (class-symbol)
  (:documentation "Returns t if the passed symbol is equal to RoleC.")
  (:method ((class-symbol symbol))
    (eql class-symbol 'RoleC)))


(defmethod equivalent-construct ((construct RoleC)
				&key (start-revision *TM-REVISION*)
				 (player nil) (instance-of nil))
  "Roles are equal if their instance-of and player properties are equal."
  (declare (integer start-revision) (type (or null TopicC) player instance-of))
  ;; item-identifiers and reifers are not checked because the equality have to
  ;; be variafied without them
  (and (equivalent-typable-construct construct instance-of
				     :start-revision start-revision)
       (eql player (player construct :revision start-revision))))


(defmethod find-item-by-revision ((construct RoleC)
				  (revision integer) &optional parent-construct)
  (if parent-construct
      (let ((parent-assoc
	     (let ((assocs
		    (remove-if
		     #'null
		     (map 'list #'(lambda(assoc)
				    (when (eql (parent-construct assoc)
					       parent-construct)
				      assoc))
			  (slot-p construct 'parent)))))
	       (when assocs
		 (first assocs)))))
	(when parent-assoc
	  (cond ((= revision 0)
		 (when
		     (find-most-recent-revision parent-assoc)
		   construct))
		(t
		 (when (find-if
			#'(lambda(vi)
			    (and (>= revision (start-revision vi))
				 (or (< revision (end-revision vi))
				     (= 0 (end-revision vi)))))
			(versions parent-assoc))
		   construct)))))
      nil))


(defmethod delete-construct :before ((construct RoleC))
  (dolist (role-assoc-to-delete (slot-p construct 'parent))
    (delete-construct role-assoc-to-delete))
  (dolist (player-assoc-to-delete (slot-p construct 'player))
    (delete-construct player-assoc-to-delete)))


(defgeneric player-p (construct)
  (:documentation "Returns t if a player is set in this role.
		   t is also returned if the player is markes-as-deleted.")
  (:method ((construct RoleC))
    (when (slot-p construct 'player)
      t)))


(defmethod owned-p ((construct RoleC))
  (when (slot-p construct 'parent)
    t))


(defmethod parent ((construct RoleC) &key (revision *TM-REVISION*))
  "Returns the construct's parent corresponding to the given revision."
  (let ((valid-associations
	 (filter-slot-value-by-revision construct 'parent
					:start-revision revision)))
    (when valid-associations
      (parent-construct (first valid-associations)))))
  

(defmethod add-parent ((construct RoleC) (parent-construct AssociationC)
			    &key (revision *TM-REVISION*))
  (declare (integer revision))
  (let ((already-set-parent (parent construct :revision revision))
	(same-parent-assoc (loop for parent-assoc in (slot-p construct 'parent)
			      when (eql parent-construct (parent-construct parent-assoc))
			      return parent-assoc)))
    (when (and already-set-parent
	       (not (eql already-set-parent parent-construct)))
      (error (make-tm-reference-condition (format nil "From add-parent(): ~a can't be owned by ~a since it is already owned by ~a"
						  construct parent-construct already-set-parent)
					  construct (parent construct :revision revision) parent-construct)))
    (let ((merged-role
	   (merge-if-equivalent construct parent-construct :revision revision)))
      (if merged-role
	  merged-role
	  (progn
	    (cond (already-set-parent
		   (let ((parent-assoc
			  (loop for parent-assoc in (slot-p construct 'parent)
			     when (eql parent-construct
				       (parent-construct parent-assoc))
			     return parent-assoc)))
		     (add-to-version-history parent-assoc
					     :start-revision revision)))
		  (same-parent-assoc
		   (add-to-version-history same-parent-assoc
					   :start-revision revision))
		  (t
		   (make-construct 'RoleAssociationC
				   :role construct
				   :parent-construct parent-construct
				   :start-revision revision)))
	    (add-to-version-history parent-construct :start-revision revision)
	    construct)))))


(defmethod private-delete-parent ((construct RoleC) (parent-construct AssociationC)
				  &key (revision (error (make-missing-argument-condition "From private-delete-parent(): revision must be set" 'revision 'private-delete-parent))))
  (let ((assoc-to-delete
	 (loop for parent-assoc in (slot-p construct 'parent)
	    when (eql (parent-construct parent-assoc) parent-construct)
	    return parent-assoc)))
    (when assoc-to-delete
      (mark-as-deleted assoc-to-delete :revision revision)
      construct)))


(defmethod delete-parent ((construct RoleC) (parent-construct AssociationC)
				  &key (revision (error (make-missing-argument-condition "From delete-parent(): revision must be set" 'revision 'delete-parent))))
  (when (private-delete-parent construct parent-construct :revision revision)
    (add-to-version-history parent-construct :start-revision revision)
    construct))


(defgeneric player (construct &key revision)
  (:documentation "Returns the construct's player corresponding to
                   the given revision.")
  (:method ((construct RoleC) &key (revision *TM-REVISION*))
    (let ((valid-associations
	   (filter-slot-value-by-revision construct 'player
					  :start-revision revision)))
      (when valid-associations
	(player-topic (first valid-associations))))))


(defgeneric add-player (construct player-topic &key revision)
  (:documentation "Adds a topic as a player to a role in the given revision.")
  (:method ((construct RoleC) (player-topic TopicC)
	    &key (revision *TM-REVISION*))
    (let ((already-set-player (player construct :revision revision))
	  (same-player-assoc
	   (loop for player-assoc in (slot-p construct 'player)
	      when (eql (player-topic player-assoc) player-topic)
	      return player-assoc)))
      (when (and already-set-player
		 (not (eql already-set-player player-topic)))
	(error (make-tm-reference-condition (format nil "From add-player(): ~a can't be played by ~a since it is played by ~a" construct player-topic already-set-player)
					    construct (player construct :revision revision) player-topic)))
      (cond (already-set-player
	     (let ((player-assoc
		    (loop for player-assoc in (slot-p construct 'player)
		       when (eql player-topic (player-topic player-assoc))
		       return player-assoc)))
	       (add-to-version-history player-assoc :start-revision revision)))
	    (same-player-assoc
	     (add-to-version-history same-player-assoc :start-revision revision))
	    (t
	     (make-construct 'PlayerAssociationC
			     :parent-construct construct
			     :player-topic player-topic
			     :start-revision revision))))
    construct))


(defgeneric private-delete-player (construct player-topic &key revision)
  (:documentation "Deletes the passed topic as a player of the passed role 
                   object by marking its association-object as deleted.")
  (:method ((construct RoleC) (player-topic TopicC)
	    &key (revision (error (make-missing-argument-condition "From private-delete-player(): revision must be set" 'revision 'private-delete-player))))
    (let ((assoc-to-delete
	   (loop for player-assoc in (slot-p construct 'player)
	      when (eql (parent-construct player-assoc) construct)
	      return player-assoc)))
      (when assoc-to-delete
	(mark-as-deleted assoc-to-delete :revision revision)
	construct))))


(defgeneric delete-player (construct player-topic &key revision)
  (:documentation "See delete-player but adds the parent role to
                   the given version.")
  (:method ((construct RoleC) (player-topic TopicC)
	    &key (revision (error (make-missing-argument-condition "From delete-player(): revision must be set" 'revision 'delete-player))))
   (when (private-delete-player construct player-topic :revision revision)
     (let ((assoc (parent construct :revision revision)))
       (when assoc
	 (add-role assoc construct :revision revision)
	 construct)))))


;;; ReifiableConstructC
(defmethod mark-as-deleted :around ((construct ReifiableConstructC)
				    &key source-locator revision)
  "Marks all item-identifiers of a given reifiable-construct as deleted."
  (declare (ignorable source-locator))
  (call-next-method)
  (dolist (ii (item-identifiers construct :revision 0))
    (private-delete-item-identifier construct ii :revision revision)))


(defmethod check-for-duplicate-identifiers ((construct ReifiableConstructC)
					    &key (revision *TM-REVISION*))
  (declare (integer revision))
  (dolist (id (get-all-identifiers-of-construct construct :revision revision))
    (when (>
	   (length
	    (delete-if-not #'(lambda(identifier)
			       (or (typep identifier 'PersistentIdC)
				   (typep identifier 'SubjectLocatorC)
				   (typep identifier 'ItemIdentifierC)))
			   (union 
			    (elephant:get-instances-by-value
			     'ItemIdentifierC 'uri (uri id))
			    (union 
			     (elephant:get-instances-by-value
			      'PersistentIdC 'uri (uri id))
			     (elephant:get-instances-by-value
			      'SubjectLocatorC 'uri (uri id))))))
	   1)
      (error (make-duplicate-identifier-condition (format nil "Duplicate Identifier ~a has been found" (uri id)) (uri id))))))


(defgeneric ReifiableConstructC-p (class-symbol)
  (:documentation "Returns t if the passed symbol is equal to ReifiableConstructC
                   or one of its subtypes.")
  (:method ((class-symbol symbol))
    (or (eql class-symbol 'ReifiableconstructC)
	(TopicMapC-p class-symbol)
	(TopicC-p class-symbol)
	(AssociationC-p class-symbol)
	(RoleC-p class-symbol)
	(CharacteristicC-p class-symbol))))


(defgeneric complete-reifiable (construct item-identifiers reifier
					    &key start-revision)
  (:documentation "Adds all item-identifiers and the reifier to the passed
                   construct.")
  (:method ((construct ReifiableConstructC) item-identifiers reifier
	    &key (start-revision *TM-REVISION*))
    (declare (integer start-revision) (list item-identifiers)
	     (type (or null TopicC) reifier))
    (let ((merged-construct construct))
      (dolist (ii item-identifiers)
	(setf merged-construct
	      (add-item-identifier merged-construct ii
				   :revision start-revision)))
      (when reifier
	(setf merged-construct (add-reifier merged-construct reifier
					    :revision start-revision)))
      merged-construct)))


(defgeneric equivalent-reifiable-construct (construct reifier item-identifiers
						      &key start-revision)
  (:documentation "Returns t if the passed constructs are TMDM equal, i.e
                   the reifiable construct have to share an item identifier
                   or reifier.")
  (:method ((construct ReifiableConstructC) reifier item-identifiers
	    &key (start-revision *TM-REVISION*))
    (declare (integer start-revision) (list item-identifiers)
	     (type (or null TopicC) reifier))
    (or (and (reifier construct :revision start-revision)
	     (eql reifier (reifier construct :revision start-revision)))
	(and (item-identifiers construct :revision start-revision)
	     (intersection (item-identifiers construct :revision start-revision)
			   item-identifiers)))))


(defmethod delete-construct :before ((construct ReifiableConstructC))
  (let ((ii-assocs-to-delete (slot-p construct 'item-identifiers))
	(reifier-assocs-to-delete (slot-p construct 'reifier)))
    (let ((all-iis (map 'list #'identifier ii-assocs-to-delete)))
      (dolist (construct-to-delete (append ii-assocs-to-delete
					   reifier-assocs-to-delete))
	(delete-construct construct-to-delete))
      (dolist (ii all-iis)
	(unless (owned-p ii)
	  (delete-construct ii))))))


(defgeneric item-identifiers (construct &key revision)
  (:documentation "Returns the ItemIdentifierC-objects that correspond
                   with the passed construct and the passed version.")
  (:method ((construct ReifiableConstructC) &key (revision *TM-REVISION*))
    (let ((assocs (filter-slot-value-by-revision
		   construct 'item-identifiers :start-revision revision)))
      (map 'list #'identifier assocs))))


(defgeneric reifier (construct &key revision)
  (:documentation "Returns the reifier-topic that corresponds
                   with the passed construct and the passed version.")
  (:method ((construct ReifiableConstructC) &key (revision *TM-REVISION*))
    (let ((assocs (filter-slot-value-by-revision
		   construct 'reifier :start-revision revision)))
      (when assocs ;assocs must be nil or a list with exactly one item
	(reifier-topic (first assocs))))))


(defgeneric add-item-identifier (construct item-identifier &key revision)
  (:documentation "Adds the passed item-identifier to the passed construct.
                   If the item-identifier is already related with the passed
                   construct a new revision is added.
                   If the passed identifer already identifies another object
                   the identified-constructs are merged.")
  (:method ((construct ReifiableConstructC) (item-identifier ItemIdentifierC)
	    &key (revision *TM-REVISION*))
    (let ((all-ids
	   (map 'list #'identifier (slot-p construct 'item-identifiers)))
	  (construct-to-be-merged
	   (let ((id-owner (identified-construct item-identifier
						 :revision revision)))
	     (when (not (eql id-owner construct))
	       id-owner))))
      (when (and construct-to-be-merged
		 (not (eql (type-of construct-to-be-merged)
			   (type-of construct))))
	(error (make-not-mergable-condition (format nil "From add-item-identifier(): ~a and ~a can't be merged since the identified-constructs are not of the same type"
						    construct construct-to-be-merged)
					    construct construct-to-be-merged)))
      (let ((merged-construct construct))
	(cond (construct-to-be-merged
	       (setf merged-construct
		     (merge-constructs construct construct-to-be-merged
				       :revision revision)))
	      ((find item-identifier all-ids)
	       (let ((ii-assoc
		      (loop for ii-assoc in (slot-p construct 'item-identifiers)
			 when (eql (identifier ii-assoc) item-identifier)
			 return ii-assoc)))
		 (add-to-version-history ii-assoc :start-revision revision)))
	      (t
	       (make-construct 'ItemIdAssociationC
			       :parent-construct construct
			       :identifier item-identifier
			       :start-revision revision)))
	(add-version-info construct revision)
	merged-construct))))


(defgeneric private-delete-item-identifier (construct item-identifier
						      &key revision)
  (:documentation "Sets the association object between the passed constructs
                   as mark-as-deleted.")
  (:method ((construct ReifiableConstructC) (item-identifier ItemIdentifierC)
	    &key (revision (error (make-missing-argument-condition "From private-delete-item-identifier(): revision must be set" 'revision 'private-delete-item-identifier))))
    (let ((assoc-to-delete (loop for ii-assoc in (slot-p construct 'item-identifiers)
			      when (eql (identifier ii-assoc) item-identifier)
			      return ii-assoc)))
      (when assoc-to-delete
	(mark-as-deleted assoc-to-delete :revision revision)
	construct))))


(defgeneric delete-item-identifier (construct item-identifier
						      &key revision)
  (:documentation "See private-delete-item-identifier but adds the parent
                   construct to the given version.")
  (:method ((construct ReifiableConstructC) (item-identifier ItemIdentifierC)
	    &key (revision (error (make-missing-argument-condition "From delete-item-identifier(): revision must be set" 'revision 'delete-item-identifier))))
    (when (private-delete-item-identifier construct item-identifier
					  :revision revision)
      (add-version-info construct revision)
      construct)))


(defgeneric add-reifier (construct reifier-topic &key revision)
  (:documentation "Adds the passed reifier-topic as reifier of the construct.
                   If the construct is already reified by the given topic
                   there only is added a new version-info.
                   If the reifier-topic reifies already another construct
                   the reified-constructs are merged.")
  (:method ((construct ReifiableConstructC) (reifier-topic TopicC)
	    &key (revision *TM-REVISION*))
    (when (and (reified-construct reifier-topic :revision revision)
	       (not (equivalent-constructs construct
					   (reified-construct
					    reifier-topic :revision revision))))
      (error (make-not-mergable-condition (format nil "From add-reifier(): ~a and ~a can't be merged since the reified-constructs (~a ~a) are not mergable"
						  reifier-topic (reifier construct :revision revision) (reified-construct reifier-topic :revision revision) construct)
					  construct (reified-construct reifier-topic :revision revision))))
    (let ((merged-reifier-topic
	   (if (reifier construct :revision revision)
	       (merge-constructs (reifier construct :revision revision)
				 reifier-topic)
	       reifier-topic)))
      (let ((all-constructs (map 'list #'reifiable-construct
				 (slot-p reifier-topic 'reified-construct))))
	(let ((merged-construct construct))
	  (cond ((reified-construct merged-reifier-topic :revision revision)
		 (let ((merged-reified
			(merge-constructs
			 (reified-construct merged-reifier-topic
					    :revision revision) construct)))
		   (setf merged-construct merged-reified)))
		((find construct all-constructs)
		 (let ((reifier-assoc
			(loop for reifier-assoc in
			     (slot-p merged-reifier-topic 'reified-construct)
			   when (eql (reifiable-construct reifier-assoc)
				     construct)
			   return reifier-assoc)))
		   (add-to-version-history reifier-assoc
					   :start-revision revision)))
		(t
		 (make-construct 'ReifierAssociationC
				 :reifiable-construct construct
				 :reifier-topic merged-reifier-topic
				 :start-revision revision)))
	  (add-version-info construct revision)
	  merged-construct)))))


(defgeneric private-delete-reifier (construct reifier &key revision)
  (:documentation "Sets the association object between the passed constructs
                   as mark-as-deleted.")
  (:method ((construct ReifiableConstructC) (reifier TopicC)
	    &key (revision (error (make-missing-argument-condition "From private-delete-reifier(): revision must be set" 'revision 'private-delete-reifier))))
    (let ((assoc-to-delete (loop for reifier-assoc in (slot-p construct 'reifier)
			      when (eql (reifier-topic reifier-assoc) reifier)
			      return reifier-assoc)))
      (when assoc-to-delete
	(mark-as-deleted assoc-to-delete :revision revision)
	construct))))


(defgeneric delete-reifier (construct reifier &key revision)
  (:documentation "See private-delete-reifier but adds the reified-construct
                   to the given version.")
  (:method ((construct ReifiableConstructC) (reifier TopicC)
	    &key (revision (error (make-missing-argument-condition "From delete-reifier(): revision must be set" 'revision 'delete-reifier))))
    (when (private-delete-reifier construct reifier :revision revision)
      (add-version-info construct revision)
      construct)))


(defmethod get-all-identifiers-of-construct ((construct ReifiableConstructC)
					     &key (revision *TM-REVISION*))
  (declare (integer revision))
  (item-identifiers construct :revision revision))


;;; TypableC
(defgeneric TypableC-p (class-symbol)
  (:documentation "Returns t if the passed class is equal to TypableC or
                   one of its subtypes.")
  (:method ((class-symbol symbol))
    (or (eql class-symbol 'TypableC)
	(AssociationC-p class-symbol)
	(RoleC-p class-symbol)
	(CharacteristicC-p class-symbol))))


(defgeneric complete-typable (construct instance-of &key start-revision)
  (:documentation "Adds the passed instance-of to the given construct.")
  (:method ((construct TypableC) instance-of
	    &key (start-revision *TM-REVISION*))
    (declare (integer start-revision) (type (or null TopicC) instance-of))
    (when instance-of
      (add-type construct instance-of :revision start-revision))
    construct))


(defgeneric equivalent-typable-construct (construct instance-of
						     &key start-revision)
  (:documentation "Returns t if the passed constructs are TMDM equal, i.e.
                   the typable constructs have to own the same type.")
  (:method ((construct TypableC) instance-of &key (start-revision *TM-REVISION*))
    (declare (integer start-revision)
	     (type (or null TopicC) instance-of))
    (eql (instance-of construct :revision start-revision) instance-of)))


;;; ScopableC
(defgeneric ScopableC-p (class-symbol)
  (:documentation "Returns t if the passed class is equal to ScopableC or
                   one of its subtypes.")
  (:method ((class-symbol symbol))
    (or (eql class-symbol 'ScopableC)
	(AssociationC-p class-symbol)
	(CharacteristicC-p class-symbol))))


(defgeneric complete-scopable (construct themes &key start-revision)
  (:documentation "Adds all passed themes to the given construct.")
  (:method ((construct ScopableC) (themes list)
	    &key (start-revision *TM-REVISION*))
    (declare (integer start-revision))
    (dolist (theme themes)
      (add-theme construct theme :revision start-revision))
    construct))


(defgeneric equivalent-scopable-construct (construct themes &key start-revision)
  (:documentation "Returns t if the passed constructs are TMDM equal, i.e.
                   the scopable constructs have to own the same themes.")
  (:method ((construct ScopableC) themes &key (start-revision *TM-REVISION*))
    (declare (integer start-revision) (list themes))
    (not (set-exclusive-or (themes construct :revision start-revision)
			   themes))))


(defmethod delete-construct :before ((construct ScopableC))
  (dolist (scope-assoc-to-delete (slot-p construct 'themes))
    (delete-construct scope-assoc-to-delete)))


(defgeneric themes (construct &key revision)
  (:documentation "Returns all topics that correspond with the given revision
                   as a scope for the given topic.")
  (:method ((construct ScopableC) &key (revision *TM-REVISION*))
    (let ((valid-associations
	   (filter-slot-value-by-revision construct 'themes
					  :start-revision revision)))
      (map 'list #'theme-topic valid-associations))))


(defgeneric add-theme (construct theme-topic &key revision)
  (:documentation "Adds the given theme-topic to the passed
                   scopable-construct.")
  (:method ((construct ScopableC) (theme-topic TopicC)
	    &key (revision *TM-REVISION*))
    (let ((all-themes
	   (map 'list #'theme-topic (slot-p construct 'themes))))
      (if (find theme-topic all-themes)
	  (let ((theme-assoc
		 (loop for theme-assoc in (slot-p construct 'themes)
		    when (eql (theme-topic theme-assoc) theme-topic)
		    return theme-assoc)))
	    (add-to-version-history theme-assoc  :start-revision revision))
	  (make-construct 'ScopeAssociationC
			  :theme-topic theme-topic
			  :scopable-construct construct
			  :start-revision revision)))
    (when (typep construct 'VersionedConstructC)
      (add-to-version-history construct :start-revision revision))
    construct))


(defgeneric private-delete-theme (construct theme-topic &key revision)
  (:documentation "Deletes the passed theme by marking it's association as
                   deleted in the passed revision.")
  (:method ((construct ScopableC) (theme-topic TopicC)
	    &key (revision (error (make-missing-argument-condition "From private-delete-theme(): revision must be set" 'revision 'private-delete-theme))))
    (let ((assoc-to-delete (loop for theme-assoc in (slot-p construct 'themes)
			      when (eql (theme-topic theme-assoc) theme-topic)
			      return theme-assoc)))
      (when assoc-to-delete
	(mark-as-deleted assoc-to-delete :revision revision)
	construct))))


(defgeneric delete-theme (construct theme-topic &key revision)
  (:documentation "See private-delete-theme but adds the parent construct
                   to the given version.")
  (:method ((construct ScopableC) (theme-topic TopicC)
	    &key (revision (error (make-missing-argument-condition "From delete-theme(): revision must be set" 'revision 'delete-theme))))
    (when (private-delete-theme construct theme-topic :revision revision)
      (add-version-info construct revision)
      construct)))


;;; TypableC
(defmethod delete-construct :before ((construct TypableC))
  (dolist (type-assoc-to-delete (slot-p construct 'instance-of))
    (delete-construct type-assoc-to-delete)))


(defgeneric instance-of-p (construct)
  (:documentation "Returns t if there is any type set in this object.
                   t is also returned if the type is marked-as-deleted.")
  (:method ((construct TypableC))
    (when (slot-p construct 'instance-of)
      t)))


(defgeneric instance-of (construct &key revision)
  (:documentation "Returns the type topic that is set on the passed
                   revision.")
  (:method ((construct TypableC) &key (revision *TM-REVISION*))
    (let ((valid-associations
	   (filter-slot-value-by-revision construct 'instance-of
					  :start-revision revision)))
      (when valid-associations
	(type-topic (first valid-associations))))))


(defgeneric add-type (construct type-topic &key revision)
  (:documentation "Add the passed type-topic as type to the given
                   typed construct if there is no other type-topic
                   set at the same revision.")
  (:method ((construct TypableC) (type-topic TopicC)
	    &key (revision *TM-REVISION*))
    (let ((already-set-type (instance-of construct :revision revision))
	  (same-type-assoc
	   (loop for type-assoc in (slot-p construct 'instance-of)
	      when (eql (type-topic type-assoc) type-topic)
	      return type-assoc)))
      (when (and already-set-type
		 (not (eql type-topic already-set-type)))
	(error (make-tm-reference-condition (format nil "From add-type(): ~a can't be typed by ~a since it is typed by ~a"
						    construct type-topic already-set-type)
					    construct (instance-of construct :revision revision) type-topic)))
      (cond (already-set-type
	     (let ((type-assoc
		    (loop for type-assoc in (slot-p construct 'instance-of)
		       when (eql type-topic (type-topic type-assoc))
		       return type-assoc)))
	       (add-to-version-history type-assoc :start-revision revision)))
	    (same-type-assoc
	     (add-to-version-history same-type-assoc :start-revision revision))
	    (t
	     (make-construct 'TypeAssociationC
			     :type-topic type-topic
			     :typable-construct construct
			     :start-revision revision))))
    (when (typep construct 'VersionedConstructC)
      (add-to-version-history construct :start-revision revision))
    construct))


(defgeneric private-delete-type (construct type-topic &key revision)
  (:documentation "Deletes the passed type by marking it's association as
                   deleted in the passed revision.")
  (:method ((construct TypableC) (type-topic TopicC)
	    &key (revision (error (make-missing-argument-condition "From private-delete-type(): revision must be set" 'revision 'private-delete-type))))
    (let ((assoc-to-delete
	   (loop for type-assoc in (slot-p construct 'instance-of)
	      when (eql (type-topic type-assoc) type-topic)
	      return type-assoc)))
      (when assoc-to-delete
	(mark-as-deleted assoc-to-delete :revision revision)
	construct))))


(defgeneric delete-type (construct type-topic &key revision)
  (:documentation "See private-delete-type but adds the parent construct
                   to the given version.")
  (:method ((construct TypableC) (type-topic TopicC)
	    &key (revision (error (make-missing-argument-condition "From private-delete-type(): revision must be set" 'revision 'private-delete-type))))
    (when (private-delete-type construct type-topic :revision revision)
      (add-version-info construct revision)
      construct)))


;;; TopicMapC
(defmethod equivalent-constructs ((construct-1 TopicMapC) (construct-2 TopicMapC)
				  &key (revision *TM-REVISION*))
  "In this definition TopicMaps are alwayas equal,
   since item-identifiers and reifiers are not changing the result of
   the TMDM equality."
  (declare (ignorable revision))
  t)


(defgeneric TopicMapC-p (class-symbol)
  (:documentation "Returns t if the passed symbol is equal to TopicMapC.")
  (:method ((class-symbol symbol))
    (eql class-symbol 'TopicMapC)))


(defmethod equivalent-construct ((construct TopicMapC)
				 &key (start-revision *TM-REVISION*)
				 (reifier nil) (item-identifiers nil))
  "TopicMaps equality if they share the same item-identier or reifier."
  (declare (list item-identifiers) (integer start-revision)
	   (type (or null TopicC) reifier))
  (equivalent-reifiable-construct construct reifier item-identifiers
				  :start-revision start-revision))


(defmethod delete-construct :before ((construct TopicMapC))
  (dolist (top (slot-p construct 'topics))
    (remove-association construct 'topics top))
  (dolist (assoc (slot-p construct 'associations))
    (remove-association construct 'associations assoc)))


(defmethod add-to-tm ((construct TopicMapC) (construct-to-add TopicC))
  (add-association construct 'topics construct-to-add)
  construct-to-add)


(defmethod add-to-tm ((construct TopicMapC) (construct-to-add AssociationC))
  (add-association construct 'associations construct-to-add)
  construct-to-add)


(defmethod delete-from-tm ((construct TopicMapC) (construct-to-delete TopicC))
  (remove-association construct 'topics construct-to-delete))


(defmethod delete-from-tm ((construct TopicMapC)
			   (construct-to-delete AssociationC))
  (remove-association construct 'associations construct-to-delete))


(defgeneric in-topicmap (tm construct &key revision)
  (:documentation "Is a given construct (topic or assiciation) in this
                   topic map?"))


(defmethod in-topicmap ((tm TopicMapC) (top TopicC) &key
			(revision *TM-REVISION*))
  (when (find-item-by-revision top revision)
    (find (internal-id top) (topics tm) :test #'= :key #'internal-id)))


(defmethod in-topicmap ((tm TopicMapC) (ass AssociationC)
			&key (revision *TM-REVISION*))
  (when (find-item-by-revision ass revision)
    (find (internal-id ass) (associations tm)  :test #'= :key #'internal-id)))


;;; make-construct ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun make-construct (class-symbol &rest args)
  "Creates a new topic map construct if necessary or
   retrieves an equivalent one if available and updates the revision
   history accordingly. Returns the object in question. Methods use
   specific keyword arguments for their purpose."
  (declare (symbol class-symbol))
  (when (and (or (VersionedConstructC-p class-symbol)
		 (and (ReifiableConstructC-p class-symbol)
		      (or (getf args :item-identifiers) (getf args :reifier))))
	     (not (getf args :start-revision)))
    (error (make-missing-argument-condition "From make-construct(): start-revision must be set" 'start-revision 'make-construct)))
  (let ((construct
	 (cond
	   ((PointerC-p class-symbol)
	    (apply #'make-pointer class-symbol args))
	   ((CharacteristicC-p class-symbol)
	    (apply #'make-characteristic class-symbol args))
	   ((TopicC-p class-symbol)
	    (apply #'make-topic args))
	   ((TopicMapC-p class-symbol)
	    (apply #'make-tm args))
	   ((RoleC-p class-symbol)
	    (apply #'make-role args))
	   ((AssociationC-p class-symbol)
	    (apply #'make-association args))
	   ((VersionedConstructC-p class-symbol)
	    (apply #'make-instance class-symbol
		   (rec-remf args :start-revision)))
	   (t
	    (apply #'make-instance class-symbol args))))
	(start-revision (or (getf args :start-revision) *TM-REVISION*)))
    (when (typep construct 'TypableC)
      (complete-typable construct (getf args :instance-of)
			:start-revision start-revision))
    (when (typep construct 'ScopableC)
      (complete-scopable construct (getf args :themes)
			 :start-revision start-revision))
    (when (typep construct 'VersionedConstructC)
      (add-to-version-history construct :start-revision start-revision))
    (when (or (typep construct 'TopicC) (typep construct 'AssociationC))
      (dolist (tm (getf args :in-topicmaps))
	(add-to-tm tm construct)))
    (if (typep construct 'ReifiableConstructC)
	(complete-reifiable construct (getf args :item-identifiers)
			    (getf args :reifier) :start-revision start-revision)
	construct)))


(defun make-association (&rest args)
  "Returns an association object. If the association has already existed the
   existing one is returned otherwise a new one is created.
   This function exists only for being used by make-construct!"
  (let ((instance-of (getf args :instance-of))
	(start-revision (getf args :start-revision))
	(themes (getf args :themes))
	(roles (getf args :roles)))
    (when (and (or roles instance-of themes)
	       (not start-revision))
      (error (make-missing-argument-condition "From make-association(): start-revision must be set" 'start-revision 'make-association)))
    (let ((association
	   (let ((existing-associations
		  (remove-if
		   #'null
		   (map 'list #'(lambda(existing-association)
				  (when (equivalent-construct
					 existing-association
					 :start-revision start-revision
					 :roles roles :themes themes
					 :instance-of instance-of)
				    existing-association))
			(get-all-associations nil)))))
	     (cond ((> (length existing-associations) 1)
		    (merge-all-constructs existing-associations
					  :revision start-revision))
		   (existing-associations
		    (first existing-associations))
		   (t
		    (make-instance 'AssociationC))))))
      (dolist (role-plist roles)
	(add-role association
		  (apply #'make-construct 'RoleC
			 (append role-plist (list :parent association)))
		  :revision (getf role-plist :start-revision)))
      association)))


(defun make-role (&rest args)
  "Returns a role object. If the role has already existed the
   existing one is returned otherwise a new one is created.
   This function exists only for being used by make-construct!"
  (let ((parent (getf args :parent))
	(instance-of (getf args :instance-of))
	(player (getf args :player))
	(start-revision (getf args :start-revision)))
    (when (and (or instance-of player parent)
	       (not start-revision))
      (error (make-missing-argument-condition "From make-role(): start-revision must be set" 'start-revision 'make-role)))
    (let ((role
	   (let ((existing-roles
		  (when parent
		    (remove-if
		     #'null
		     (map 'list #'(lambda(existing-role)
				    (when (equivalent-construct
					   existing-role
					   :start-revision start-revision
					   :player player
					   :instance-of instance-of)
				      existing-role))
			  (map 'list #'role (slot-p parent 'roles)))))))
	     (if (and existing-roles
		      (or (eql parent (parent (first existing-roles)
					      :revision start-revision))
			  (not (parent (first existing-roles)
				       :revision start-revision))))
		 (progn
		   (add-role parent (first existing-roles)
			     :revision start-revision)
		   (first existing-roles))
		 (make-instance 'RoleC)))))
      (when player
	(add-player role player :revision start-revision))
      (when parent
	(add-parent role parent :revision start-revision))
      role)))


(defun make-tm (&rest args)
  "Returns a topic map object. If the topic map has already existed the
   existing one is returned otherwise a new one is created.
   This function exists only for being used by make-construct!"
  (let ((item-identifiers (getf args :item-identifiers))
	(reifier (getf args :reifier))
	(topics (getf args :topics))
	(assocs (getf args :associations))
	(start-revision (getf args :start-revision)))
    (when (and (or item-identifiers reifier)
	       (not start-revision))
      (error (make-missing-argument-condition "From make-tm(): start-revision must be set" 'start-revision 'make-tm)))
    (let ((tm
	   (let ((existing-tms
		  (remove-if
		   #'null
		   (map 'list #'(lambda(existing-tm)
				  (when (equivalent-construct
					 existing-tm
					 :item-identifiers item-identifiers
					 :reifier reifier)
				    existing-tm))
			(get-all-tms start-revision)))))
	     (cond ((> (length existing-tms) 1)
		    (merge-all-constructs existing-tms :revision start-revision))
		   (existing-tms
		    (first existing-tms))
		   (t
		    (make-instance 'TopicMapC))))))
      (dolist (top-or-assoc (union topics assocs))
	(add-to-tm tm top-or-assoc))
      tm)))
	   

(defun make-topic (&rest args)
  "Returns a topic object. If the topic has already existed the existing one is
   returned otherwise a new one is created.
   This function exists only for being used by make-construct!"
  (let ((start-revision (getf args :start-revision))
	(psis (getf args :psis))
	(locators (getf args :locators))
	(item-identifiers (getf args :item-identifiers))
	(topic-identifiers (getf args :topic-identifiers))
	(names (getf args :names))
	(occurrences (getf args :occurrences))
	(reified-construct (getf args :refied-construct)))
    (when (and (or psis locators item-identifiers topic-identifiers
		   names occurrences)
	       (not start-revision))
      (error (make-missing-argument-condition "From make-topic(): start-revision must be set" 'start-revision 'make-topic)))
    (let ((topic
	   (let ((existing-topics
		  (remove-if
		   #'null
		   (map 'list #'(lambda(existing-topic)
				  (when (equivalent-construct
					 existing-topic
					 :start-revision start-revision
					 :psis psis :locators locators
					 :item-identifiers item-identifiers
					 :topic-identifiers topic-identifiers)
				    existing-topic))
			(get-all-topics start-revision)))))
	     (cond ((> (length existing-topics) 1)
		    (merge-all-constructs existing-topics :revision start-revision))
		   (existing-topics
		    (first existing-topics))
		   (t
		    (make-instance 'TopicC))))))
      (let ((merged-topic topic))
	(dolist (tid topic-identifiers)
	  (setf merged-topic (add-topic-identifier merged-topic tid
						   :revision start-revision)))
	(dolist (psi psis)
	  (setf merged-topic (add-psi merged-topic psi
				      :revision start-revision)))
	(dolist (locator locators)
	  (setf merged-topic (add-locator merged-topic locator
					  :revision start-revision)))
	(dolist (name names)
	  (setf merged-topic (add-name merged-topic name
				       :revision start-revision)))
	(dolist (occ occurrences)
	  (add-occurrence merged-topic occ :revision start-revision))
	(when reified-construct
	  (add-reified-construct merged-topic reified-construct
				 :revision start-revision))
	merged-topic))))


(defun make-characteristic (class-symbol &rest args)
  "Returns a characteristic object with the passed parameters.
   If an equivalent construct has already existed this one is returned.
   To check if there is existing an equivalent construct the parameter
   parent-construct must be set.
   This function only exists for being used by make-construct!"
  (let ((charvalue (or (getf args :charvalue) ""))
	(start-revision (getf args :start-revision))
	(datatype (or (getf args :datatype) *xml-string*))
	(instance-of (getf args :instance-of))
	(themes (getf args :themes))
	(variants (getf args :variants))
	(parent (getf args :parent)))
    (when (and (or instance-of themes variants parent)
	       (not start-revision))
      (error (make-missing-argument-condition "From make-characteristic(): start-revision must be set" 'start-revision 'make-characgteristic)))
    (let ((characteristic
	   (let ((existing-characteristics
		  (when parent
		    (remove-if
		     #'null
		     (map 'list #'(lambda(existing-characteristic)
				    (when (equivalent-construct
					   existing-characteristic
					   :start-revision start-revision
					   :datatype datatype :variants variants
					   :charvalue charvalue :themes themes
					   :instance-of instance-of)
				      existing-characteristic))
			  (get-all-characteristics parent class-symbol))))))
	     (if (and existing-characteristics
		      (or (eql parent (parent (first existing-characteristics)
					      :revision start-revision))
			  (not (parent (first existing-characteristics)
				       :revision start-revision))))
		 (progn
		   (add-characteristic parent (first existing-characteristics)
				       :revision start-revision)
		   (first existing-characteristics))
		 (make-instance class-symbol :charvalue charvalue
				:datatype datatype)))))
      (when (typep characteristic 'NameC)
	(complete-name characteristic variants :start-revision start-revision))
      (when parent
	(add-parent characteristic parent :revision start-revision))
      characteristic)))


(defun make-pointer (class-symbol &rest args)
  "Returns a pointer object with the specified parameters.
   If an equivalen construct has already existed this one is returned.
   This function only exists for beoing used by make-construct!"
  (let ((uri (getf args :uri))
	(xtm-id (getf args :xtm-id))
	(start-revision (getf args :start-revision))
	(identified-construct (getf args :identified-construct))
	(err "From make-pointer(): "))
    (when (and identified-construct (not start-revision))
      (error (make-missing-argument-condition (format nil "~astart-revision must be set" err) 'start-revision 'make-pointer)))
    (unless uri
      (error (make-missing-argument-condition (format nil "~auri must be set" err) 'uri 'make-pointer)))
    (when (and (TopicIdentificationC-p class-symbol)
	       (not xtm-id))
      (error (make-missing-argument-condition (format nil "~axtm-id must be set" err) 'xtm-id 'make-pointer)))
    (let ((identifier
	   (let ((existing-pointer
		  (remove-if
		   #'null
		   (map 'list 
			#'(lambda(existing-pointer)
			    (when (and (typep existing-pointer class-symbol)
				       (equivalent-construct existing-pointer
							     :uri uri
							     :xtm-id xtm-id))
			      existing-pointer))
			(elephant:get-instances-by-value class-symbol 'd::uri uri)))))
	     (if existing-pointer
		 (first existing-pointer)
		 (make-instance class-symbol :uri uri :xtm-id xtm-id)))))
      (when identified-construct
	(cond ((TopicIdentificationC-p class-symbol)
	       (add-topic-identifier identified-construct identifier
				     :revision start-revision))
	      ((PersistentIdC-p class-symbol)
	       (add-psi identified-construct identifier :revision start-revision))
	      ((ItemIdentifierC-p class-symbol)
	       (add-item-identifier identified-construct identifier
				    :revision start-revision))
	      ((SubjectLocatorC-p class-symbol)
	       (add-locator identified-construct identifier
			    :revision start-revision))))
      identifier)))


;;; merge-constructs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgeneric move-identifiers (source destination &key revision)
  (:documentation "Sets all identifiers as mark as deleted in the given
                   version and adds the marked identifiers to the
                   destination construct."))


(defmethod move-identifiers ((source ReifiableConstructC)
			     (destination ReifiableConstructC)
			     &key (revision *TM-REVISION*))
  (declare (integer revision))
  (let ((iis (item-identifiers source :revision revision)))
    (dolist (ii iis)
      (private-delete-item-identifier source ii :revision revision)
      (add-item-identifier destination ii :revision revision))
    iis))


(defmethod move-identifiers ((source TopicC) (destination TopicC)
			     &key (revision *TM-REVISION*))
  (declare (integer revision))
  (let ((iis (call-next-method))
	(tids (topic-identifiers source :revision revision))
	(psis (psis source :revision revision))
	(sls (locators source :revision revision)))
    (dolist (tid tids)
      (private-delete-topic-identifier source tid :revision revision)
      (add-topic-identifier destination tid :revision revision))
    (dolist (psi psis)
      (private-delete-psi source psi :revision revision)
      (add-psi destination psi :revision revision))
    (dolist (sl sls)
      (private-delete-locator source sl :revision revision)
      (add-locator destination sl :revision revision))
    (append tids iis psis sls)))


(defgeneric move-referenced-constructs (source destination &key revision)
  (:documentation "Moves all referenced constructs in the given version from
                   the source TM-construct to the destination TM-construct."))


(defmethod move-referenced-constructs ((source ReifiableConstructC)
				       (destination ReifiableConstructC)
				       &key (revision *TM-REVISION*))
  (declare (integer revision))
  (remove-if
   #'null
   (append
    (move-identifiers source destination :revision revision)
    (let ((source-reifier (reifier source :revision revision))
	  (destination-reifier (reifier destination :revision revision)))
      (let ((result
	     (cond ((and source-reifier destination-reifier)
		    (private-delete-reifier (reified-construct source-reifier
						       :revision revision)
				    source-reifier :revision revision)
		    (private-delete-reifier (reified-construct destination-reifier
						       :revision revision)
				    destination-reifier :revision revision)
		    (let ((merged-reifier
			   (merge-constructs source-reifier destination-reifier
					     :revision revision)))
		      (add-reifier destination merged-reifier :revision revision)
		      merged-reifier))
		   (source-reifier
		    (private-delete-reifier (reified-construct source-reifier
						       :revision revision)
				    source-reifier :revision revision)
		    (add-reifier destination source-reifier :revision revision)
		    source-reifier)
		   (destination-reifier
		    (add-reifier destination destination-reifier :revision revision)
		    nil))))
	(when result
	  (list result)))))))


(defmethod move-referenced-constructs ((source NameC) (destination NameC)
				       &key (revision *TM-REVISION*))
  (declare (integer revision))
  (append (call-next-method)
	  (move-variants source destination :revision revision)))


(defmethod move-referenced-constructs ((source TopicC) (destination TopicC)
				       &key (revision *TM-REVISION*))
  (let ((roles (player-in-roles source :revision revision))
	(scopables (used-as-theme source :revision revision))
	(typables (used-as-type source :revision revision))
	(ids (move-identifiers source destination :revision revision)))
    (dolist (role roles)
      (private-delete-player role source :revision revision)
      (add-player role destination :revision revision))
    (dolist (scopable scopables)
      (private-delete-theme scopable source :revision revision)
      (add-theme scopable destination :revision revision))
    (dolist (typable typables)
      (private-delete-type typable source :revision revision)
      (add-type typable destination :revision revision))
    (remove-if #'null (append roles scopables typables ids))))


(defgeneric move-reified-construct (source destination &key revision)
  (:documentation "Moves the refied TM-construct from the source topic
                   to the given destination topic.")
  (:method ((source TopicC) (destination TopicC) &key (revision *TM-REVISION*))
    (declare (integer revision))
    (let ((source-reified (reified-construct source :revision revision))
	  (destination-reified (reified-construct destination
						  :revision revision)))
      (when (and source-reified destination-reified
		 (not (eql (type-of source-reified)
			   (type-of destination-reified))))
	(error (make-not-mergable-condition (format nil "From move-reified-construct(): ~a and ~a can't be merged since the reified-constructs are not of the same type ~a ~a"
						    source destination source-reified destination-reified)
					    source destination)))
      (cond ((and source-reified destination-reified)
	     (private-delete-reifier source-reified source :revision revision)
	     (private-delete-reifier destination-reified destination :revision revision)
	     (let ((merged-reified
		    (merge-constructs source-reified destination-reified
				      :revision revision)))
	       (add-reifier merged-reified destination :revision revision)
	       merged-reified))
	    (source-reified
	     (private-delete-reifier source source-reified :revision revision)
	     (add-reifier  source-reified destination :revision revision)
	     source-reified)
	    (destination-reified
	     (add-reifier destination-reified destination :revision revision)
	     destination-reified)))))


(defgeneric move-occurrences (source destination &key revision)
  (:documentation "Moves all occurrences from the source topic to the
                   destination topic. If occurrences are TMDM equal
                   they are merged, i.e. one is marked-as-deleted.")
  (:method ((source TopicC) (destination TopicC) &key (revision *TM-REVISION*))
    (declare (integer revision))
    (let ((occs-to-move (occurrences source :revision revision)))
      (dolist (occ occs-to-move)
	(private-delete-occurrence source occ :revision revision)
	(let ((equivalent-occ
	       (find-if #'(lambda (destination-occ)
			    (when 
				(strictly-equivalent-constructs
				 occ destination-occ :revision revision)
			      destination-occ))
			(occurrences destination :revision revision))))
	  (if equivalent-occ
	      (progn
		(add-occurrence destination equivalent-occ :revision revision)
		(move-referenced-constructs occ equivalent-occ
					    :revision revision))
	      (add-occurrence destination occ :revision revision))))
      occs-to-move)))


(defgeneric move-variants (source destination &key revision)
  (:documentation "Moves all variants from the source name to the destination
                   name. If any variants are TMDM equal they are merged -->
                   i.e. one of the variants is marked-as-deleted.")
  (:method ((source NameC) (destination NameC) &key (revision *TM-REVISION*))
    (declare (integer revision))
    (let ((vars-to-move (variants source :revision revision)))
      (dolist (var vars-to-move)
	(private-delete-variant source var :revision revision)
	(let ((equivalent-var
	       (find-if #'(lambda (destination-var)
			    (when 
				(strictly-equivalent-constructs
				 var destination-var :revision revision)
			      destination-var))
			(variants destination :revision revision))))
	  (if equivalent-var
	      (progn
		(add-variant destination equivalent-var :revision revision)
		(move-referenced-constructs var equivalent-var
					    :revision revision))
	      (add-variant destination var :revision revision))))
      vars-to-move)))


(defgeneric move-names (source destination &key revision)
  (:documentation "Moves all names from the source topic to the destination
                   topic. If any names are equal they are merged, i.e.
                   one of the names is marked-as-deleted.")
  (:method ((source TopicC) (destination TopicC) &key (revision *TM-REVISION*))
    (declare (integer revision))
    (let ((names-to-move (names source :revision revision)))
      (dolist (name names-to-move)
	(private-delete-name source name :revision revision)
	(let ((equivalent-name
	       (find-if #'(lambda (destination-name)
			    (when 
				(strictly-equivalent-constructs
				 name destination-name :revision revision)
			      destination-name))
			(names destination :revision revision))))
	  (if equivalent-name
	      (progn		
		(add-name destination equivalent-name :revision revision)
		(move-referenced-constructs name equivalent-name
					    :revision revision))
	      (add-name destination name :revision revision))))
      names-to-move)))


(defun merge-changed-constructs (older-topic &key (revision *TM-REVISION*))
  (declare (TopicC older-topic))
  (dolist (construct (append (used-as-type older-topic :revision revision)
			     (used-as-theme older-topic :revision revision)
			     (player-in-roles older-topic :revision revision)))
    (let ((parent (when (or (typep construct 'RoleC)
			    (typep construct 'CharacteristicC))
		    (parent construct :revision revision))))
      (let ((all-other (cond ((typep construct 'OccurrenceC)
			      (occurrences parent :revision revision))
			     ((typep construct 'NameC)
			      (names parent :revision revision))
			     ((typep construct 'VariantC)
			      (variants parent :revision revision))
			     ((typep construct 'RoleC)
			      (roles parent :revision revision)))))
	(let ((all-equivalent
	       (remove-if
		#'null
		(map 'list #'(lambda(other)
			       (when (strictly-equivalent-constructs
				      construct other :revision revision)
				 other))
		     all-other))))
	  (when all-equivalent
	    (merge-all-constructs (append all-equivalent (list construct))
				  :revision revision))))))
  (merge-changed-associations older-topic :revision revision))
  

(defun merge-changed-associations (older-topic &key (revision *TM-REVISION*))
  "Merges all associations that became TMDM-equal since two referenced topics
   were merged, e.g. the association types."
  (declare (TopicC older-topic))
  (let ((all-assocs
	 (remove-duplicates
	  (append 
	   (remove-if
	    #'null
	    (map 'list #'(lambda(role)
			   (parent role :revision revision))
		 (player-in-roles older-topic :revision revision)))
	    (remove-if
	     #'null
	     (map 
	      'list #'(lambda(constr)
			(when (typep constr 'AssociationC)
			  constr))
	      (append (used-as-type older-topic :revision revision)
		      (used-as-theme older-topic :revision revision))))))))
    (dolist (assoc all-assocs)
      (let ((all-equivalent
	     (remove-if
	      #'null
	      (map 'list #'(lambda(db-assoc)
			     (when (strictly-equivalent-constructs
				    assoc db-assoc :revision revision)
			       db-assoc))
		   (get-all-associations nil)))))
	(when all-equivalent
	  (merge-all-constructs (append all-equivalent (list assoc))
				:revision revision))))))
    

(defmethod merge-constructs ((construct-1 TopicC) (construct-2 TopicC)
			     &key (revision *TM-REVISION*))
  (if (eql construct-1 construct-2)
      construct-1
      (let ((older-topic (find-oldest-construct construct-1 construct-2)))
	(let ((newer-topic (if (eql older-topic construct-1)
			       construct-2
			       construct-1)))
	  (dolist (tm (in-topicmaps newer-topic :revision revision))
	    (add-to-tm tm older-topic))
	  (move-names newer-topic older-topic :revision revision)
	  (move-occurrences newer-topic older-topic :revision revision)
	  (move-referenced-constructs newer-topic older-topic :revision revision)
	  (move-reified-construct newer-topic older-topic :revision revision)
	  (merge-changed-constructs older-topic :revision revision)
	  (mark-as-deleted newer-topic :revision revision :source-locator nil)
	  (when (exist-in-version-history-p newer-topic)
	    (delete-construct newer-topic))
	  older-topic))))


(defmethod merge-constructs ((construct-1 CharacteristicC)
			     (construct-2 CharacteristicC)
			     &key (revision *TM-REVISION*))
  (declare (integer revision))
  (if (eql construct-1 construct-2)
      construct-1
      (let ((older-char (find-oldest-construct construct-1 construct-2)))
	(let ((newer-char (if (eql older-char construct-1)
			      construct-2
			      construct-1)))
	  (let ((parent-1 (parent older-char :revision revision))
		(parent-2 (parent newer-char :revision revision)))
	    (unless (strictly-equivalent-constructs construct-1 construct-2
						    :revision revision)
	      (error (make-not-mergable-condition (format nil "From merge-constructs(): ~a and ~a are not mergable" construct-1 construct-2)
						  construct-1 construct-2)))
	    (cond ((and parent-1 (eql parent-1 parent-2))
		   (move-referenced-constructs newer-char older-char
					       :revision revision)
		   (private-delete-characteristic parent-2 newer-char
					  :revision revision)
		   (let ((c-assoc
			  (find-if
			   #'(lambda(c-assoc)
			       (and (eql (characteristic c-assoc) older-char)
				    (eql (parent-construct c-assoc) parent-1)))
			   (cond ((typep older-char 'OccurrenceC)
				  (slot-p parent-1 'occurrences))
				 ((typep older-char 'NameC)
				  (slot-p parent-1 'names))
				 ((typep older-char 'VariantC)
				  (slot-p parent-1 'variants))))))
		     (add-to-version-history c-assoc :start-revision revision))
		   older-char)
		  ((and parent-1 parent-2)
		   (let ((active-parent (merge-constructs parent-1 parent-2
							  :revision revision)))
		     (let ((found-older-char
			    (cond ((typep older-char 'OccurrenceC)
				   (find older-char
					 (occurrences
					  active-parent :revision revision)))
				  ((typep older-char 'NameC)
				   (find older-char
					 (names
					  active-parent :revision revision)))
				  ((typep older-char 'VariantC)
				   (find-if
				    #'(lambda(name)
					(find older-char
					      (variants name
							:revision revision)))
				    (if (parent active-parent :revision revision)
					(names (parent active-parent :revision revision)
					       :revision revision)
					(list active-parent)))))))
		       (if found-older-char
			   older-char
			   newer-char))))
		  ((or parent-1 parent-2)
		   (let ((dst (if parent-1 older-char newer-char))
			 (src (if parent-1 newer-char older-char)))
		     (move-referenced-constructs src dst :revision revision)
		     (delete-if-not-referenced src)
		     dst))
		  (t
		   (move-referenced-constructs newer-char older-char
					       :revision revision)
		   (delete-if-not-referenced newer-char)
		   older-char)))))))


(defmethod merge-constructs ((construct-1 TopicMapC) (construct-2 TopicMapC)
			     &key (revision *TM-REVISION*))
  (declare (integer revision))
  (if (eql construct-1 construct-2)
      construct-1
      (let ((older-tm (find-oldest-construct construct-1 construct-2)))
	(let ((newer-tm (if (eql older-tm construct-1)
			    construct-2
			    construct-1)))
	  (move-referenced-constructs newer-tm older-tm :revision revision)
	  (dolist (top-or-assoc (append (topics newer-tm) (associations newer-tm)))
	    (add-to-tm older-tm top-or-assoc))
	  (add-to-version-history older-tm :start-revision revision)
	  (mark-as-deleted newer-tm :revision revision)
	  (when (exist-in-version-history-p newer-tm)
	    (delete-construct newer-tm))
	  older-tm))))


(defmethod merge-constructs ((construct-1 AssociationC) (construct-2 AssociationC)
			     &key revision)
  (declare (integer revision))
  (if (eql construct-1 construct-2)
      construct-1
      (let ((older-assoc (find-oldest-construct construct-1 construct-2)))
	(let ((newer-assoc (if (eql older-assoc construct-1)
			       construct-2
			       construct-1)))
	  ;(unless (strictly-equivalent-constructs construct-1 construct-2
	  ;					  :revision revision)
	  ;;associations that have different roles can be although merged, e.g.
          ;;two roles are in two different association objects references
          ;;the same item-identifier or reifier
	  (when (or (set-exclusive-or (themes construct-1 :revision revision)
				      (themes construct-2 :revision revision))
		    (not (eql (instance-of construct-1 :revision revision)
			      (instance-of construct-2 :revision revision))))
	    (error (make-not-mergable-condition (format nil "From merge-constructs(): ~a and ~a are not mergable" construct-1 construct-2)
						construct-1 construct-2)))
	  (dolist (tm (in-topicmaps newer-assoc :revision revision))
	    (add-to-tm tm older-assoc))
	  (private-delete-type newer-assoc (instance-of newer-assoc :revision revision)
		       :revision revision)
	  (move-referenced-constructs newer-assoc older-assoc)
	  (dolist (newer-role (roles newer-assoc :revision revision))
	    (let ((equivalent-role
		   (find-if #'(lambda(older-role)
				(strictly-equivalent-constructs
				 older-role newer-role :revision revision))
			    (roles older-assoc :revision revision))))
	      (when equivalent-role
		(move-referenced-constructs newer-role equivalent-role
					    :revision revision))
	      (private-delete-role newer-assoc newer-role :revision revision)
	      (add-role older-assoc (if equivalent-role
					equivalent-role
					newer-role)
			:revision revision)))
	  (mark-as-deleted newer-assoc :revision revision)
	  (when (exist-in-version-history-p newer-assoc)
	    (delete-construct newer-assoc))
	  older-assoc))))


(defmethod merge-constructs ((construct-1 RoleC) (construct-2 RoleC)
			     &key (revision *TM-REVISION*))
  (declare (integer *TM-REVISION*))
  (if (eql construct-1 construct-2)
      construct-1
      (let ((older-role (find-oldest-construct construct-1 construct-2)))
	(let ((newer-role (if (eql older-role construct-1)
			       construct-2
			       construct-1)))
	  (unless (strictly-equivalent-constructs construct-1 construct-2
						  :revision revision)
	    (error (make-not-mergable-condition (format nil "From merge-constructs(): ~a and ~a are not mergable" construct-1 construct-2)
						construct-1 construct-2)))
	  (let ((parent-1 (parent older-role :revision revision))
		(parent-2 (parent newer-role :revision revision)))
	    (cond ((and parent-1 (eql parent-1 parent-2))
		   (move-referenced-constructs newer-role older-role
					       :revision revision)
		   (private-delete-role parent-2 newer-role :revision revision)
		   (let ((r-assoc
			  (find-if
			   #'(lambda(r-assoc)
			       (and (eql (role r-assoc) older-role)
				    (eql (parent-construct r-assoc) parent-1)))
			   (slot-p parent-1 'roles))))
		     (add-to-version-history r-assoc :start-revision revision)
		     older-role))
		  ((and parent-1 parent-2)
		   (let ((active-assoc (merge-constructs parent-1 parent-2
							 :revision revision)))
		     (if (find older-role (roles active-assoc
						 :revision revision))
			 older-role
			 newer-role)))
		  ((or parent-1 parent-2)
		   (let ((dst (if parent-1 older-role newer-role))
			 (src (if parent-1 newer-role older-role)))
		     (move-referenced-constructs src dst :revision revision)
		     (delete-if-not-referenced src)
		     dst))
		  (t
		   (move-referenced-constructs newer-role older-role
					       :revision revision)
		   (delete-if-not-referenced newer-role)
		   older-role)))))))


(defmethod merge-if-equivalent ((new-role RoleC) (parent-construct AssociationC)
				&key (revision *TM-REVISION*))
  (declare (integer revision))
  (let ((possible-roles
	 (remove-if #'(lambda(role)
			(when (parent role :revision revision)
			  role))
		    (map 'list #'role (slot-p parent-construct 'roles)))))
    (let ((equivalent-role
	   (remove-if
	    #'null
	    (map 'list
		 #'(lambda(role)
		     (when
			 (strictly-equivalent-constructs role new-role
							 :revision revision)
		       role))
		 possible-roles))))
      (when equivalent-role
	(merge-constructs (first equivalent-role) new-role
			  :revision revision)))))
		      

(defmethod merge-if-equivalent ((new-characteristic CharacteristicC)
				(parent-construct ReifiableConstructC)
				&key (revision *TM-REVISION*))
  (declare (integer revision) (type (or TopicC NameC) parent-construct))
  (let ((all-existing-characteristics
	 (map 'list #'characteristic
	      (cond ((typep new-characteristic 'OccurrenceC)
		     (slot-p parent-construct 'occurrences))
		    ((typep new-characteristic 'NameC)
		     (slot-p parent-construct 'names))
		    ((typep new-characteristic 'VariantC)
		     (slot-p parent-construct 'variants))))))
    (let ((possible-characteristics ;all characteristics that are not referenced
				    ;other constructs at the given revision
	   (remove-if #'(lambda(char)
			  (parent char :revision revision))
		      all-existing-characteristics)))
      (let ((equivalent-construct
	     (remove-if
	      #'null
	      (map 'list
		   #'(lambda(char)
		       (when
			   (strictly-equivalent-constructs char new-characteristic
							   :revision revision)
			 char))
		   possible-characteristics))))
	(when equivalent-construct
	  (merge-constructs (first equivalent-construct) new-characteristic
			    :revision revision))))))