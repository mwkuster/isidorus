;;+-----------------------------------------------------------------------------
;;+  Isidorus
;;+  (c) 2008-2009 Marc Kuester, Christoph Ludwig, Lukas Giessmann
;;+
;;+  Isidorus is freely distributable under the LGPL license.
;;+  You can find a detailed description in trunk/docs/LGPL-LICENSE.txt.
;;+-----------------------------------------------------------------------------


;-*- standard-indent: 2; indent-tabs-mode: nil -*-
(defpackage :datamodel
  (:use :cl :elephant :constants)
  (:nicknames :d)
  (:import-from :exceptions
                missing-reference-error
                no-identifier-error
                duplicate-identifier-error
                object-not-found-error)
  (:export :AssociationC ;; types
           :CharacteristicC
           :FragmentC
           :IdentifierC
           :IdentityC 
           :ItemIdentifierC
           :NameC
           :OccurrenceC
           :PersistentIdC
           :ReifiableConstructC
           :RoleC
           :ScopableC
           :SubjectLocatorC
           :TopicC
           :TopicIdentificationC
           :TopicMapC
	   :TopicMapConstructC 
	   :TypableC
	   :VariantC

           ;; functions and slot accessors
           :in-topicmaps
           :add-to-topicmap
           :add-source-locator
           :associations
           :changed-p
           :charvalue
           :check-for-duplicate-identifiers
           :datatype
           :equivalent-constructs
           :find-item-by-revision
           :find-most-recent-revision
           :get-all-revisions
           :get-all-revisions-for-tm
           :get-fragment
           :get-fragments
           :get-revision
           :get-item-by-content
           :get-item-by-id
           :get-item-by-item-identifier
           :get-item-by-psi
           :identified-construct
           :identified-construct-p
           :in-topicmap
           :internal-id
           :instance-of
           :instance-of-p
           :item-identifiers
           :item-identifiers-p
           :list-instanceOf
	   :list-super-types
           :locators
           :locators-p
           :make-construct
           :mark-as-deleted
           :names
           :namevalue
           :occurrences
	   :name
           :parent
           :player
           :player-in-roles
           :players
           :psis
           :psis-p
           :referenced-topics
           :revision
           :RoleC-p
           :roleid
           :roles
           :themes
           :xtm-id
           :xtm-id-p
           :topic
           :topicid
           :topic-identifiers
           :topics
           :unique-id
	   :uri 
	   :uri-p
	   :used-as-type
	   :used-as-theme
	   :variants
	   :xor
           :create-latest-fragment-of-topic
	   :reified
	   :reifier
	   :add-reifier

           :*current-xtm* ;; special variables
           :*TM-REVISION*

           :with-revision ;;macros

	   :string-starts-with ;;helpers
           ))

(declaim (optimize (debug 3) (safety 3) (speed 0) (space 0)))
(in-package :datamodel)

(defparameter *current-xtm* nil "Represents the currently active TM")

(defmacro find-max-elem (candidate-list &key (relop #'> relop-p) (key #'identity key-p))
  "Given a non-empty list, return the maximum element in the list.
   If provided, then relop must be a relational operator that determines the ordering;
   else #'> is used. The keyword parameter key may name a function that is used to extract
   the sort key; otherwise the elements themselves are the sort keys."
  (let
      ((candidate-list-value-name (gensym))
       (relop-value-name (gensym))
       (key-value-name (gensym))
       (best-seen-cand-name (gensym))
       (max-key-name (gensym))
       (inspected-cand-name (gensym))
       (inspected-key-name (gensym)))
    (let
        ((max-key-init (if key-p
                           `(funcall ,key-value-name ,best-seen-cand-name)
                           best-seen-cand-name))
         (inspected-key-init (if key-p
                                 `(funcall ,key-value-name ,inspected-cand-name)
                                 inspected-cand-name))
         (relexp (if relop-p
                     `(funcall ,relop-value-name ,inspected-key-name ,max-key-name)
                     `(> ,inspected-key-name ,max-key-name))))
      (let
          ((initializers `((,candidate-list-value-name ,candidate-list)
                           (,best-seen-cand-name (first ,candidate-list-value-name))
                           (,max-key-name ,max-key-init))))
        (when relop-p
          (push `(,relop-value-name ,relop) initializers))
        (when key-p
          (push `(,key-value-name ,key) initializers))
        `(let*
          ,initializers
          (dolist (,inspected-cand-name (rest ,candidate-list-value-name))
            (let
                ((,inspected-key-name ,inspected-key-init))
              (when ,relexp
                (setf ,best-seen-cand-name ,inspected-cand-name)
                (setf ,max-key-name ,inspected-key-name))))
          ,best-seen-cand-name)))))

(defvar *TM-REVISION* 0)

(defmacro with-revision (revision &rest body)
  `(let
    ((*TM-REVISION* ,revision))
                                        ;(format t "*TM-REVISION* is ~a~&" *TM-REVISION*)
    ,@body))
    

(defmacro slot-predicate (instance slot)
  (let
      ((inst-name (gensym))
       (slot-name (gensym)))
    `(let
      ((,inst-name ,instance)
       (,slot-name ,slot))
      (and (slot-boundp ,inst-name ,slot-name)
       (slot-value ,inst-name ,slot-name)))))

(defmacro delete-1-n-association (instance slot)
  (let
      ((inst-name (gensym))
       (slot-name (gensym)))
    `(let
      ((,inst-name ,instance)
       (,slot-name ,slot))
      (when (slot-predicate ,inst-name ,slot-name)
        (elephant:remove-association ,inst-name ,slot-name (slot-value ,inst-name ,slot-name))))))

(defun xor (a1 a2)
  (and (or a1 a2) (not (and a1 a2)))
  )

(defun remove-nil-values (plist)
  (let
      ((result nil))
    (do* ((rest plist (cddr rest))
          (key (first rest) (first rest))
          (val (second rest) (second rest)))
         ((null rest))
      (when val
        (pushnew val result)
        (pushnew key result)))
    result))

(defun get-revision ()
  "TODO: replace by something that does not suffer from a 1 second resolution."
  (get-universal-time))

(defgeneric delete-construct (construct)
  (:documentation "drops recursively construct and all its dependent objects from the elephant store"))

(defmethod delete-construct ((construct elephant:persistent))
  nil)

(defmethod delete-construct :after ((construct elephant:persistent))
  (elephant:drop-instance construct))

(defgeneric find-all-equivalent (construct)
  (:method ((construct t)) nil)
  (:documentation "searches an existing object that is equivalent (but not identical) to construct"))


;;;;;;;;;;;;;;
;;
;; VersionInfoC


(elephant:defpclass VersionInfoC ()
  ((start-revision :accessor start-revision
                   :initarg :start-revision
                   :type integer
                   :initform 0          ;TODO: for now
                   :documentation "The first revison this AssociationC instance is associated with.")
   (end-revision :accessor end-revision
                 :initarg :end-revision
                 :type integer
                 :initform 0            ;TODO: for now
                 :documentation "The first revison this AssociationC instance is no longer associated with.")
   (versioned-construct :associate TopicMapConstructC
                        :accessor versioned-construct
                        :initarg :versioned-construct
                        :documentation "reifiable construct that is described by this info"))
  (:documentation "Version Info for individual revisions"))

(defgeneric versioned-construct-p (vi)
  (:documentation "t if this version info is already bound to a TM construct")
  (:method ((vi VersionInfoC)) (slot-predicate vi 'versioned-construct)))

(defmethod delete-construct :before ((vi VersionInfoC))
  (delete-1-n-association vi 'versioned-construct))

(defgeneric get-most-recent-version-info (construct))



;;;;;;;;;;;;;;
;;
;; TopicMapConstrucC


(elephant:defpclass TopicMapConstructC ()
  ((versions :associate (VersionInfoC versioned-construct)
             :accessor versions
             :initarg :versions
             :documentation "version infos for former versions of this reifiable construct")))

                                        ;TODO: if, one day, we allow merges of already existing constructs, we'll need
                                        ;a tree of predecessors rather then just a list of versions. A case in point
                                        ;may be if a newly imported topic carries the PSIs of two existing topics,
                                        ;thereby forcing a merge post factum"

(defmethod delete-construct :before ((construct TopicMapConstructC))
  (dolist (versioninfo (versions construct))
    (delete-construct versioninfo)))


(defgeneric add-to-version-history (construct &key start-revision end-revision)
  (:documentation "Add version history to a topic map construct"))

(defmethod add-to-version-history ((construct TopicMapConstructC) 
                                   &key 
                                   (start-revision (error "Start revision must be present") )
                                   (end-revision 0))
  "Adds relevant information to a construct's version info"
  (let
      ((current-version-info
        (get-most-recent-version-info construct)))
    (cond
      ((and current-version-info 
           (= (end-revision current-version-info) start-revision)) ;the item was just marked as deleted
       (setf (end-revision current-version-info) 0) ;just revitalize it, do not create a new version
       current-version-info)  ;TODO: this is not quite correct, the topic
                              ;might be recreated with new item
                              ;identifiers. Consider adding a new parameter
                              ;"revitalize"
      ((and 
        current-version-info 
        (= (end-revision current-version-info) 0))
       (setf (end-revision current-version-info) start-revision)
       (make-instance 
        'VersionInfoC 
        :start-revision start-revision
        :end-revision end-revision
        :versioned-construct construct))
      (t
       (make-instance 
        'VersionInfoC 
        :start-revision start-revision
        :end-revision end-revision
        :versioned-construct construct)))))

(defgeneric revision (constr)
  (:documentation "Essentially a convenience method for start-revision"))

(defmethod revision ((constr TopicMapConstructC))
  (start-revision constr))

(defmethod (setf revision) ((constr TopicMapConstructC) (revision integer))
  (setf (start-revision constr) revision))


(defgeneric find-item-by-revision (constr revision)
  (:documentation "Get a given version of a construct (if any, nil if none can be found)"))

(defmethod find-item-by-revision ((constr TopicMapConstructC) (revision integer))
  (cond
    ((= revision 0)
     (find-most-recent-revision constr))
    (t
     (when (find-if
            (lambda(version) 
              (and (>= revision (start-revision version))
                   (or
		    (< revision (end-revision version))
		    (= 0 (end-revision version)))))
            (versions constr))
       constr))))

(defgeneric find-most-recent-revision (construct)
  (:documentation "Get the most recent version of a construct (nil if
the construct doesn't have versions yet or not anymore)"))

(defmethod find-most-recent-revision ((construct TopicMapConstructC))
  (when (find 0 (versions construct) :key #'end-revision)
    construct))

(defmethod delete-construct :before ((construct TopicMapConstructC))
  (dolist (versionInfo (versions construct))
    (delete-construct versionInfo)))


(defgeneric check-for-duplicate-identifiers (top)
  (:documentation "Check for possibly duplicate identifiers and signal an
  duplicate-identifier-error is such duplicates are found"))

(defmethod check-for-duplicate-identifiers ((construct TopicMapConstructC))
  (declare (ignore construct))
                                        ;do nothing
  )

(defgeneric filter-slot-value-by-revision (construct slot-name &key start-revision)
  (:documentation "filter slot values by a given revision that is
  either provided directly through the keyword argument start-revision
  or through a bound variable named '*TM-REVISION*'"))

(defmethod filter-slot-value-by-revision ((construct TopicMapConstructC) (slot-name symbol) &key (start-revision 0 start-revision-provided-p))
  (let
      ((revision            ;avoids warnings about undefined variables
        (cond
          (start-revision-provided-p
           start-revision)
          ((boundp '*TM-REVISION*)
           (symbol-value '*TM-REVISION*))
          (t 0)))
       (properties (slot-value construct slot-name)))
       ;(format t "revision in filter-slot-value-by-revision is ~a~&" revision)
    (cond
      ((not properties) 
       nil)   ;if we don't have any properties, we don't have to worry
              ;about revisions
      ((= 0 revision)
       (remove 
        nil
        (map 'list #'find-most-recent-revision
             properties)))
      (t
       (remove nil
               (map 'list 
                    (lambda (constr)
                      (find-item-by-revision constr revision))
                    properties))))))

(defgeneric make-construct (classsymbol &key start-revision &allow-other-keys)
  (:documentation "create a new topic map construct if necessary or
retrieve an equivalent one if available and update the revision
history accordingly. Return the object in question. Methods use
specific keyword arguments for their purpose"))

(defmethod make-construct ((classsymbol symbol) &rest args
                           &key start-revision)
  (let*
      ((cleaned-args (remove-nil-values args))
       (new-construct (apply #'make-instance classsymbol cleaned-args))
       (existing-construct (first (find-all-equivalent new-construct))))
    (if existing-construct
        (progn
                                        ;change over new item identifiers to the old construct
          (when  (copy-item-identifiers
                  new-construct existing-construct)
                 ;an existing construct other than a topic (which is handled
                 ;separatedly below) has changed only if it has received a new
                 ;item identifier
            (add-to-version-history existing-construct :start-revision start-revision))
          (delete-construct new-construct)
          existing-construct)
        (progn
          (add-to-version-history new-construct :start-revision start-revision)
          (check-for-duplicate-identifiers new-construct)
          new-construct))))
    
(defmethod get-most-recent-version-info ((construct TopicMapConstructC))
  (find 0 (versions construct) :key #'end-revision))

(defgeneric equivalent-constructs (construct1 construct2)
  (:documentation "checks if two topic map constructs are equal according to the TMDM equality rules"))

(defgeneric strictly-equivalent-constructs (construct1 construct2)
  (:documentation "checks if two topic map constructs are not identical but equal according to the TMDM equality rules")
  (:method ((construct1 TopicMapConstructC) (construct2 TopicMapConstructC))
    (and (equivalent-constructs construct1 construct2)
         (not (eq construct1 construct2)))))

(defgeneric internal-id (construct)
  (:documentation "returns the internal id that uniquely identifies a
  construct (currently simply its OID)"))

(defmethod internal-id ((construct TopicMapConstructC))
  (slot-value construct (find-symbol "OID" 'elephant)))



;;;;;;;;;;;;;;
;;
;; PointerrC

(elephant:defpclass PointerC (TopicMapConstructC)
  ((uri :accessor uri
        :initarg :uri
        :type string
        :initform (error "The uri must be set for a pointer")
        :index t)
   (identified-construct :accessor identified-construct
                         :initarg :identified-construct
                         :associate ReifiableConstructC))
  (:documentation "Abstract base class for all types of pointers and identifiers"))

(defmethod delete-construct :before ((construct PointerC))
  (delete-1-n-association construct 'identified-construct))

(defmethod find-all-equivalent ((construct PointerC))
  (delete construct
          (elephant:get-instances-by-value (class-of construct)
                                           'uri
                                           (uri construct))
          :key #'internal-id))
(defgeneric uri-p (construct)
  (:documentation "Check if the slot uri is bound in an identifier and not nil")
  (:method ((identifier PointerC)) (slot-predicate identifier 'uri)))

(defgeneric identified-construct-p (construct)
  (:documentation "Check if the slot identified-construct is bound in an identifier and not nil")
  (:method ((identifier PointerC)) (slot-predicate identifier 'identified-construct)))

(defmethod print-object ((identifier PointerC) stream)
  (format stream 
          "~a(href: ~a; Construct: ~a)"
          (class-name (class-of identifier))
          (if (uri-p identifier)
              (uri identifier)
              "URI UNDEFINED")
          (if (identified-construct-p identifier)
              (identified-construct identifier) 
              "SLOT UNBOUND")))

(defmethod equivalent-constructs ((identifier1 PointerC) (identifier2 PointerC))
  (string= (uri identifier1) (uri identifier2)))

(defmethod initialize-instance :around ((identifier PointerC) &key
                                        (start-revision (error "Start revision must be present") )
                                        (end-revision 0))
  (call-next-method)
  (add-to-version-history identifier
                          :start-revision start-revision
                          :end-revision end-revision)
  identifier)

;;;;;;;;;;;;;;
;;
;; IdentifierC

(elephant:defpclass IdentifierC (PointerC)
  ()
  (:documentation "Abstract base class for ItemIdentifierC and
  PersistentIdC, primarily in view of the equality rules"))


;;;;;;;;;;;;;;
;;
;; ItemIdentifierC

(elephant:defpclass ItemIdentifierC (IdentifierC) 
  ()
  (:index t)
  (:documentation "Represents an item identifier"))


;;;;;;;;;;;;;;
;;
;; PSI

(elephant:defpclass PersistentIdC (IdentifierC) 
  ((identified-construct :accessor identified-construct
                         :initarg :identified-construct
                         :associate TopicC))
  (:index t)
  (:documentation "Represents a PSI"))



;;(defmethod print-object ((psi PersistentIdC) stream)
;;  (format stream "PSI(URI: ~a; TopicId: ~a)" (uri psi) (topicid (identified-construct psi))))


;;;;;;;;;;;;;;
;;
;; SubjectLocator

(elephant:defpclass SubjectLocatorC (IdentifierC)
  ((identified-construct :accessor identified-construct
                         :initarg :identified-construct
                         :associate TopicC))
  (:index t)
  (:documentation "Represents a subject locator"))

;;;;;;;;;;;;;;
;;
;; TopicIdentificationC

(elephant:defpclass TopicIdentificationC (PointerC) 
  ((xtm-id 
    :accessor xtm-id
    :type string
    :initarg :xtm-id
    :index t
    :documentation "ID of the TM this identification came from"))
  (:documentation "Identify topic items through generalized
  topicids. A topic may have many original topicids, the class
  representing one of them") )

(defmethod find-all-equivalent ((construct TopicIdentificationC))
  (delete (xtm-id construct) (call-next-method) :key #'xtm-id :test #'string=))

(defun init-topic-identification (top id xtm-id &key (revision *TM-REVISION*))
  "create a TopicIdentification object (if necessary) and initialize it with the
  combination of the current topicid and the ID of the current XTM id"
                                        ;(declare (TopicC top))
  (declare (string id))

  (flet    ;prevent unnecessary copies of TopicIdentificationC objects
      ((has-topic-identifier (top uri xtm-id)
         (remove-if-not
          (lambda (ident)
            (and (string= (uri ident) uri)
                 (string= (xtm-id ident) xtm-id)))
          (topic-identifiers top))))
    (unless (has-topic-identifier top id xtm-id)
      (let
          ((ti
            (make-instance 
             'TopicIdentificationC
             :uri id
             :xtm-id xtm-id
             :identified-construct top
             :start-revision revision)))
           ;(add-to-version-history ti :start-revision revision)
           ti))))

(defun xtm-id-p (xtm-id)
  "checks if a xtm-id has been used before"
  (elephant:get-instance-by-value 'TopicIdentificationC
                                  'xtm-id xtm-id))

;;;;;;;;;;;;;;
;;
;; ReifiableConstructC

(elephant:defpclass ReifiableConstructC (TopicMapConstructC)
  ((item-identifiers 
    :associate (ItemIdentifierC identified-construct)
    :inherit t
    :documentation "Slot that realizes a 1 to N
                     relation between reifiable constructs and their
                     identifiers; pseudo-initarg is :item-identifiers. Is inherited by all reifiable constructs")
   (reifier
    :associate TopicC
    :inherit t
    :documentation "Represents a reifier association to a topic, i.e.
                   it stands for a 1:1 association between this class and TopicC"))
  (:documentation "Reifiable constructs as per TMDM"))


(defgeneric reifier (construct &key revision)
  (:method ((construct ReifiableConstructC) &key (revision *TM-REVISION*))
    (when (slot-boundp construct 'reifier)
      (slot-value construct 'reifier))))

(defgeneric (setf reifier) (topic TopicC)
  (:method (topic (construct ReifiableConstructC))
    (setf (slot-value construct 'reifier) topic)))
;    (setf (reified topic) construct)))

(defgeneric item-identifiers (construct &key revision)
  (:method ((construct ReifiableConstructC) &key (revision *TM-REVISION*))
    (filter-slot-value-by-revision construct 'item-identifiers :start-revision revision)))

(defmethod initialize-instance :around ((instance ReifiableConstructC) &key (item-identifiers nil) (reifier nil))
  "adds associations to these ids after the instance was initialized."
  (declare (list item-identifiers))
  (call-next-method)
  (dolist (id item-identifiers)
    (declare (ItemIdentifierC id))
    (setf (identified-construct id) instance))
  (when reifier
    (setf (reifier instance) reifier))
  instance)

(defmethod delete-construct :before ((construct ReifiableConstructC))
  (dolist (id (item-identifiers construct))
    (delete-construct id))
  (when (reifier construct)
    (slot-makunbound (reifier construct) 'reified)))

(defgeneric item-identifiers-p (constr)
  (:documentation "Test for the existence of item identifiers")
  (:method ((construct ReifiableConstructC)) (slot-predicate construct 'item-identifiers)))

(defgeneric topicid (construct &optional xtm-id)
  (:documentation "Return the ID of a construct"))

(defmethod revision ((constr ReifiableConstructC))
  (start-revision constr))

(defgeneric (setf revision) (revision construct)
  (:documentation "The corresponding setter method"))

(defmethod (setf revision) ((revision integer) (constr ReifiableConstructC))
  (setf (start-revision constr) revision))

(defgeneric get-all-identifiers-of-construct (construct)
  (:documentation "Get all identifiers that a given construct has"))

(defmethod get-all-identifiers-of-construct ((construct ReifiableConstructC))
  (item-identifiers construct))

(defmethod check-for-duplicate-identifiers ((construct ReifiableConstructC))
  (dolist (id (get-all-identifiers-of-construct construct))
    (when (> (length 
              (union 
               (elephant:get-instances-by-value 'ItemIdentifierC 'uri (uri id))
               (union 
                (elephant:get-instances-by-value 'PersistentIdC 'uri (uri id))
                (elephant:get-instances-by-value 'SubjectLocatorC 'uri (uri id)))))
             1)
      (error 
       (make-condition 'duplicate-identifier-error 
                       :message (format nil "Duplicate Identifier ~a has been found" (uri id))
                       :uri (uri id))))))

(defmethod copy-item-identifiers ((from-construct ReifiableConstructC)
                                  (to-construct ReifiableConstructC))
  "Internal method to copy over item idenfiers from a construct to
another on. Returns the set of new identifiers"
  (mapc
   (lambda (identifier)
     (setf (identified-construct identifier) 
           to-construct))
   (set-difference (item-identifiers from-construct)
                   (item-identifiers to-construct)
                   :key #'uri :test #'string=)))

;;;;;;;;;;;;;;
;;
;; ScopableC

(elephant:defpclass ScopableC ()
  ((themes :accessor themes
           :associate (TopicC used-as-theme)
           :inherit t
           :many-to-many t
           :documentation "list of this scope's themes; pseudo-initarg is :themes")))

(defmethod initialize-instance :around ((instance ScopableC) &key (themes nil))
  (declare (list themes))
  (call-next-method)
  (dolist (theme themes)
    (elephant:add-association instance 'themes theme))
  instance)

(defmethod delete-construct :before ((construct ScopableC))
  (dolist (theme (themes construct))
    (elephant:remove-association construct 'themes theme)))


;;;;;;;;;;;;;;
;;
;; TypableC

(elephant:defpclass TypableC ()
  ((instance-of :accessor instance-of
                :initarg :instance-of
                :associate TopicC
                :inherit t
                :documentation "topic that this construct is an instance of")))

(defmethod delete-construct :before ((construct TypableC))
  (when (instance-of-p construct)
    (elephant:remove-association construct 'instance-of (instance-of construct))))

(defgeneric instance-of-p (construct)
  (:documentation "is the instance-of slot bound and not nil")
  (:method ((construct TypableC)) (slot-predicate construct 'instance-of)))


;; (defmethod equivalent-constructs ((scope1 ScopeC) (scope2 ScopeC))
;;   "scopes are equal if their themes are equal"
;;   (let
;;       ((themes1 
;;  (map 'list #'internal-id (themes scope1)))
;;        (themes2
;;  (map 'list #'internal-id (themes scope2))))
;;     (not (set-exclusive-or themes1 themes2 :key #'internal-id))))

;;;;;;;;;;;;;;
;;
;; CharacteristicC
       

(elephant:defpclass CharacteristicC (ReifiableConstructC ScopableC TypableC)
  ((topic :accessor topic
          :initarg :topic
          :associate TopicC
          :documentation "The topic that this characteristic belongs to")
   (charvalue :accessor charvalue
              :type string
              :initarg :charvalue
              :index t
              :documentation "the value of the characteristic in the given scope"))
  (:documentation "Scoped characteristic of a topic (meant to be used
  as an abstract class)"))

(defgeneric CharacteristicC-p (object)
  (:documentation "test if object is a of type CharacteristicC")
  (:method ((object t)) nil)
  (:method ((object CharacteristicC)) object))

(defmethod delete-construct :before ((construct CharacteristicC))
  (delete-1-n-association construct 'topic))

(defun get-item-by-content (content &key (revision *TM-REVISION*))
  "Find characteristis by their (atomic) content"
  (flet
      ((get-existing-instances (classname)
         (delete-if-not #'(lambda (constr)
                            (find-item-by-revision constr revision))
                        (elephant:get-instances-by-value classname 'charvalue content))))
    (nconc (get-existing-instances 'OccurenceC)
           (get-existing-instances 'NameC))))




;;;;;;;;;;;;;;
;;
;; VariantC

(elephant:defpclass VariantC (CharacteristicC)
  ((datatype :accessor datatype
             :initarg :datatype
             :initform nil
             :documentation "The XML Schema datatype of the occurrencevalue (optional, always IRI for resourceRef)")
   (name :accessor name
	  :initarg :name
	  :associate NameC
	  :documentation "references the NameC instance which is the owner of this element")))


(defgeneric VariantC-p (object)
  (:documentation "test if object is a of type VariantC")
  (:method ((object t)) nil)
  (:method ((object VariantC)) object))


(defmethod delete-construct :before ((construct VariantC))
  (delete-1-n-association construct 'name))


(defmethod find-all-equivalent ((construct VariantC))
  (let ((parent (and (slot-boundp construct 'name)
                     (name construct))))
    (when parent
      (delete-if-not #'(lambda(x)(strictly-equivalent-constructs construct x))
                     (slot-value parent 'variants)))))


(defmethod equivalent-constructs ((variant1 VariantC) (variant2 VariantC))
  "variant items are (TMDM(5.5)-)equal if the values of their
   [value], [datatype], [scope], and [parent] properties are equal"
  (and (string= (charvalue variant1) (charvalue variant2))
       (or (and (not (slot-boundp variant1 'datatype)) (not (slot-boundp variant2 'datatype)))
           (and (slot-boundp variant1 'datatype) (slot-boundp variant2 'datatype)
                (string= (datatype variant1) (datatype variant2))))
       (not (set-exclusive-or (themes variant1) (themes variant2) :key #'internal-id))))

                        

         
;;;;;;;;;;;;;;
;;
;; NameC

(elephant:defpclass NameC (CharacteristicC)
  ((variants ;:accessor variants
	     :associate (VariantC name)))
  (:documentation "Scoped name of a topic"))


(defgeneric variants (name &key revision)
  (:method ((name NameC) &key (revision *TM-REVISION*))
    (filter-slot-value-by-revision name 'variants :start-revision revision)))


(defgeneric NameC-p (object)
  (:documentation "test if object is a of type NameC")
  (:method ((object t)) nil)
  (:method ((object NameC)) object))


(defmethod find-all-equivalent ((construct NameC))
  (let
      ((parent (and (slot-boundp construct 'topic)
                    (topic construct))))
    (when parent
      (delete-if-not 
       #'(lambda (cand) (strictly-equivalent-constructs construct cand))
       (slot-value parent 'names)))))


(defmethod delete-construct :before ((construct NameC))
  (dolist (variant (variants construct))
    (delete-construct variant)))


(defmethod equivalent-constructs ((name1 NameC) (name2 NameC))
  "check for the equlity of two names by the TMDM's equality
rules (5.4)"
  (and
   (string= (charvalue name1) (charvalue name2))
   (or (and (instance-of-p name1)
            (instance-of-p name2)
            (= (internal-id (instance-of name1)) 
               (internal-id (instance-of name2))))
       (and (not (instance-of-p name1)) (not (instance-of-p name2))))
   (not (set-exclusive-or (themes name1) (themes name2) :key #'internal-id))))
           



;;;;;;;;;;;;;;
;;
;; OccurrenceC

(elephant:defpclass OccurrenceC (CharacteristicC)
  ((datatype :accessor datatype
             :initarg :datatype
             :initform nil
             :documentation "The XML Schema datatype of the occurrencevalue (optional, always IRI for resourceRef)")))


(defgeneric OccurrenceC-p (object)
  (:documentation "test if object is a of type OccurrenceC")
  (:method ((object t)) nil)
  (:method ((object OccurrenceC)) object))

(defmethod find-all-equivalent ((construct OccurrenceC))
  (let
      ((parent (and (slot-boundp construct 'topic)
                    (topic construct))))
    (when parent
      (delete-if-not  #'(lambda (cand) (strictly-equivalent-constructs construct cand))
                      (slot-value parent 'occurrences)))))

(defmethod equivalent-constructs ((occ1 OccurrenceC) (occ2 OccurrenceC))
  "Occurrence items are equal if the values of their [value], [datatype], [scope], [type], and [parent] properties are equal (TMDM 5.6)"
  (and
   (string= (charvalue occ1) (charvalue occ2))
   (not (set-exclusive-or (themes occ1) (themes occ2) :key #'internal-id))
   (= (internal-id (topic occ1)) (internal-id (topic occ2)))
   (or 
    (and (instance-of-p occ1) (instance-of-p occ2)
         (= 
          (internal-id (instance-of occ1))
          (internal-id (instance-of occ2))))
    (and (not (instance-of-p occ1)) (not (instance-of-p occ2))))))


;;;;;;;;;;;;;;;;;
;;
;; TopicC

(elephant:defpclass TopicC (ReifiableConstructC)
  ((topic-identifiers
    :accessor topic-identifiers
    :associate (TopicIdentificationC identified-construct))
   (psis                                ;accessor written below
    :associate (PersistentIdC identified-construct)
    :documentation "list of PSI objects associated with this
         topic")
   (locators
                                        ;accessor written below
    :associate (SubjectLocatorC identified-construct)
    :documentation "an optional URL that (if given) means that this topic is a subject locator")
   (names                               ;accessor written below
    :associate (NameC topic)
    :documentation "list of topic names (as TopicC objects)") 
   (occurrences         ;accessor occurrences explicitly written below
    :associate (OccurrenceC topic)
    :documentation "list of occurrences (as OccurrenceC objects)")
   (player-in-roles            ;accessor player-in-roles written below
    :associate (RoleC player)
    :documentation "the list of all role instances where this topic is a player in")
   (used-as-type                  ;accessor used-as-type written below
    :associate (TypableC instance-of)
    :documentation "list of all constructs that have this topic as their type")
   (used-as-theme                ;accessor used-as-theme written below
    :associate (ScopableC themes)
    :many-to-many t
    :documentation "list of all scopable objects this topic is a theme in")
   (in-topicmaps
    :associate (TopicMapC topics)
    :many-to-many t
    :documentation "list of all topic maps this topic is part of")
   (reified
    :associate ReifiableConstructC
    :documentation "contains a reified object, represented as 1:1 association"))
  (:documentation "Topic in a Topic Map"))


(defgeneric reified (topic &key revision)
  (:method ((topic TopicC) &key (revision *TM-REVISION*))
    (when (slot-boundp topic 'reified)
      (slot-value topic 'reified))))

(defgeneric (setf reified) (reifiable ReifiableConstructC)
  (:method (reifiable (topic TopicC))
    (setf (slot-value topic 'reified) reifiable)))
;    (setf (reifier reifiable) topic)))

(defgeneric occurrences (topic &key revision)
  (:method ((topic TopicC) &key (revision *TM-REVISION*))
    (filter-slot-value-by-revision topic 'occurrences :start-revision revision)))

(defgeneric names (topic &key revision)
  (:method ((topic TopicC) &key (revision *TM-REVISION*))
    (filter-slot-value-by-revision topic 'names :start-revision revision)))

(defgeneric psis (topic &key revision)
  (:method ((topic TopicC) &key (revision *TM-REVISION*))
    (filter-slot-value-by-revision 
     topic 'psis :start-revision revision)))

(defgeneric locators (topic &key revision)
  (:method ((topic TopicC) &key (revision *TM-REVISION*))
    (filter-slot-value-by-revision 
     topic 'locators :start-revision revision)))

(defgeneric player-in-roles (topic &key revision)
  (:method ((topic TopicC) &key (revision *TM-REVISION*))
    (filter-slot-value-by-revision 
     topic 'player-in-roles :start-revision revision)))

(defgeneric used-as-type (topic &key revision)
  (:method ((topic TopicC) &key (revision *TM-REVISION*))
    (filter-slot-value-by-revision topic 'used-as-type :start-revision revision)))

(defgeneric used-as-theme (topic &key revision)
  (:method ((topic TopicC) &key (revision *TM-REVISION*))
    (filter-slot-value-by-revision topic 'used-as-theme :start-revision revision)))

(defgeneric in-topicmaps (topic &key revision)
  (:method ((topic TopicC) &key (revision *TM-REVISION*))
    (filter-slot-value-by-revision topic 'in-topicmaps :start-revision revision)))

(defun move-identifiers(destination-topic source-topic &key (what 'item-identifiers))
  "Moves all identifiers from the source-topic to the destination topic."
  (declare (TopicC destination-topic source-topic))
  (let ((all-source-identifiers
	 (cond
	   ((eql what 'item-identifiers)
	    (item-identifiers source-topic))
	   ((eql what 'locators)
	    (locators source-topic))
	   (t
	    (psis source-topic))))
	(all-destination-identifiers
	 (cond
	   ((eql what 'item-identifiers)
	    (item-identifiers destination-topic))
	   ((eql what 'locators)
	    (locators destination-topic))
	   ((eql what 'psis)
	    (psis destination-topic))
	   ((eql what 'topic-identifiers)
	    (topic-identifiers destination-topic)))))
    (let ((identifiers-to-move
	   (loop for id in all-source-identifiers
	      when (not (find-if #'(lambda(x)
				     (if (eql what 'topic-identifiers)
					 (string= (xtm-id x) (xtm-id id))
					 (string= (uri x) (uri id))))
				 all-destination-identifiers))
	      collect id)))
      (dolist (item identifiers-to-move)
	(remove-association source-topic what item)
	(add-association destination-topic what item)))))

(defmethod initialize-instance :around ((instance TopicC) &key (psis nil) (locators nil) (reified nil))
  "implement the pseudo-initargs :topic-ids, :persistent-ids, and :subject-locators"
  (declare (list psis))
  (declare (list locators))
  (call-next-method)
  ;item-identifiers are handled in the around-method for ReifiableConstructs,
  ;TopicIdentificationCs are handled in make-construct of TopicC
  (dolist (persistent-id psis)
    (declare (PersistentIdC persistent-id))
    (setf (identified-construct persistent-id) instance))
  (dolist (subject-locator locators)
    (declare (SubjectLocatorC subject-locator))
    (setf (identified-construct subject-locator) instance))
  (when reified
    (setf (reified instance) reified)))


(defmethod delete-construct :before ((construct TopicC))
  (dolist (dependent (append (topic-identifiers construct)
                             (psis construct)
                             (locators construct)
                             (names construct)
                             (occurrences construct)
                             (player-in-roles construct)
                             (used-as-type construct)))
    (delete-construct dependent))
  (dolist (theme (used-as-theme construct))
    (elephant:remove-association construct 'used-as-theme theme))
  (dolist (tm (in-topicmaps construct))
    (elephant:remove-association construct 'in-topicmaps tm))
  (when (reified construct)
    (slot-makunbound (reified construct) 'reifier)))
  
(defun get-all-constructs-by-uri (uri)
  (delete 
   nil
   (mapcar 
    (lambda (identifier)
      (and
       (slot-boundp identifier 'identified-construct)
       (identified-construct identifier)))
    (union
     (union
      (elephant:get-instances-by-value 'ItemIdentifierC 'uri uri)
      (elephant:get-instances-by-value 'PersistentIdC 'uri uri))
     (elephant:get-instances-by-value 'SubjectLocatorC 'uri uri)))))


(defun find-existing-topic (item-identifiers locators psis)
  (let
      ((uris 
        (mapcar #'uri 
               (union (union item-identifiers locators) psis)))
       (existing-topics nil))
    (dolist (uri uris)
      (setf existing-topics
            (nunion existing-topics
                    (get-all-constructs-by-uri uri)
                    :key #'internal-id)))
    (assert (<= (length existing-topics) 1))
    (first existing-topics)))


(defmethod make-construct ((class-symbol (eql 'TopicC)) &rest args
                           &key start-revision item-identifiers locators psis topicid xtm-id)
  (let
      ((existing-topic 
        (find-existing-topic item-identifiers locators psis)))
    (if existing-topic
        (progn
          ;our problem with topics is that we know only after the
          ;addition of all the identifiers and characteristics if
          ;anything has changed. We can't decide that here, so we must
          ;add all revisions (real or imaginary) to version history
          ;and decide the rest in changed-p. Maybe somebody can think
          ;of a better way?
          (add-to-version-history existing-topic
                                  :start-revision start-revision)
          (init-topic-identification existing-topic topicid xtm-id
                                     :revision start-revision)
          (let*                 ;add new identifiers to existing topics
              ((all-new-identifiers 
                (union (union item-identifiers locators) psis))
               (all-existing-identifiers 
                (get-all-identifiers-of-construct existing-topic)))
            (mapc
             (lambda (identifier)
               (setf (identified-construct identifier) existing-topic))
             (set-difference all-new-identifiers all-existing-identifiers
                             :key #'uri :test #'string=))
            (mapc #'delete-construct 
                  (delete-if
                   (lambda (identifier) 
                     (slot-boundp identifier 'identified-construct))
                   all-new-identifiers)))
          (check-for-duplicate-identifiers existing-topic)
          existing-topic)
        (progn
          (let*
              ((cleaned-args (remove-nil-values args))
               (new-topic 
                (apply #'make-instance 'TopicC cleaned-args)))
            
            (init-topic-identification new-topic topicid xtm-id 
                                       :revision start-revision)
            (check-for-duplicate-identifiers new-topic)
            (add-to-version-history new-topic
                                    :start-revision start-revision)
            new-topic)))))

(defmethod make-construct :around ((class-symbol (eql 'TopicC))
                                   &key start-revision &allow-other-keys)
  (declare (ignorable start-revision))
  (call-next-method))

    
(defmethod equivalent-constructs ((topic1 TopicC) (topic2 TopicC))
  "TMDM, 5.3.5: Equality rule: Two topic items are equal if they have:

* at least one equal string in their [subject identifiers] properties,

* at least one equal string in their [item identifiers] properties,

* at least one equal string in their [subject locators] properties,

* an equal string in the [subject identifiers] property of the one
topic item and the [item identifiers] property of the other, or the
same information item in their [reified] properties (TODO: this rule
is currently ignored)" 
  ;(declare (optimize (debug 3)))
  (let
      ((psi-uris1
        (map 'list #'uri (psis topic1)))
       (psi-uris2
        (map 'list #'uri (psis topic2)))
       (ii-uris1
        (map 'list #'uri (item-identifiers topic1)))
       (ii-uris2
        (map 'list #'uri (item-identifiers topic2)))
       (locators1
        (map 'list #'uri (locators topic1)))
       (locators2
        (map 'list #'uri (locators topic2))))
    (let
        ((all-uris1
          (union psi-uris1 (union ii-uris1 locators1) :test #'string=))
         (all-uris2
          (union psi-uris2 (union ii-uris2 locators2) :test #'string=)))
      ;;TODO: consider what we should do about this. If the topic at a
      ;;given revision doesn't exist yet, it correctly has no uris
      ;;(for that version)
      ;; (when (= 0 (length all-uris1))
;;         (error (make-condition 'no-identifier-error :message "Topic1 has no identifier" :internal-id (internal-id topic1))))
;;       (when (= 0 (length all-uris2))
;;         (error (make-condition 'no-identifier-error :message "Topic2 has no identifier" :internal-id (internal-id topic2))))
      (intersection
       all-uris1 all-uris2
       :test #'string=))))
    
(defmethod get-all-identifiers-of-construct ((top TopicC))
  (append (psis top)
          (locators top)
          (item-identifiers top)))

  
(defmethod topicid ((top TopicC) &optional (xtm-id nil))
  "Return the primary id of this item (= essentially the OID). If
xtm-id is explicitly given, return one of the topicids in that
TM (which must then exist)"
  (if xtm-id
      (let 
          ((possible-identifications
            (remove-if-not
             (lambda (top-id)
               (string= (xtm-id top-id) xtm-id))
             (elephant:get-instances-by-value 
              'TopicIdentificationC
              'identified-construct
              top))))
        (unless possible-identifications
          (error (make-condition 
                  'object-not-found-error
                  :message 
                  (format nil "Could not find an object ~a in xtm-id ~a" top xtm-id))))
        (uri (first possible-identifications)))
      (format nil "t~a"
              (internal-id top))))
  

(defgeneric psis-p (top)
  (:documentation "Test for the existence of PSIs")
  (:method ((top TopicC)) (slot-predicate top 'psis)))

(defgeneric list-instanceOf (topic &key tm)
 (:documentation "Generate a list of all topics that this topic is an
  instance of, optionally filtered by a topic map"))

(defmethod list-instanceOf ((topic TopicC)  &key (tm nil))
  (remove-if 
   #'null
   (map 'list #'(lambda(x)
                  (when (loop for psi in (psis (instance-of x))
                           when (string= (uri psi) "http://psi.topicmaps.org/iso13250/model/instance")
                           return t)
                    (loop for role in (roles (parent x))
                       when (not (eq role x))
                       return (player role))))
        (if tm
            (remove-if-not 
             (lambda (role)
               (format t "player: ~a" (player role))
               (format t "parent: ~a" (parent role))
               (format t "topic: ~a~&" topic)
               (in-topicmap tm (parent role)))
             (player-in-roles topic))
            (player-in-roles topic)))))


(defgeneric list-super-types (topic &key tm)
 (:documentation "Generate a list of all topics that this topic is an
  subclass of, optionally filtered by a topic map"))


(defmethod list-super-types ((topic TopicC)  &key (tm nil))
  (remove-if 
   #'null
   (map 'list #'(lambda(x)
                  (when (loop for psi in (psis (instance-of x))
                           when (string= (uri psi) *subtype-psi*)
                           return t)
                    (loop for role in (roles (parent x))
                       when (not (eq role x))
                       return (player role))))
        (if tm
            (remove-if-not 
             (lambda (role)
               (format t "player: ~a" (player role))
               (format t "parent: ~a" (parent role))
               (format t "topic: ~a~&" topic)
               (in-topicmap tm (parent role)))
             (player-in-roles topic))
            (player-in-roles topic)))))


(defun string-starts-with (str prefix)
  "Checks if string str starts with a given prefix"
  (declare (string str prefix))
  (string= str prefix :start1 0 :end1
           (min (length prefix)
                (length str))))


(defun get-item-by-item-identifier (uri &key revision)
  "get a construct by its item identifier. Returns nil if the item does not exist in a
particular revision"
  (declare (string uri))
  (declare (integer revision))
  (let
      ((ii-obj 
        (elephant:get-instance-by-value 'ItemIdentifierC
                                        'uri uri)))
    (when ii-obj
      (find-item-by-revision 
       (identified-construct ii-obj) revision))))


(defun get-item-by-psi (psi &key (revision 0))
  "get a topic by its PSI. Returns nil if the item does not exist in a
particular revision"
  (declare (string psi))
  (declare (integer revision))
  (let
      ((psi-obj 
        (elephant:get-instance-by-value 'PersistentIdC
                                        'uri psi)))
    (when psi-obj
      (find-item-by-revision 
       (identified-construct psi-obj) revision))))

(defun get-item-by-id (topicid &key (xtm-id *current-xtm*) (revision 0) (error-if-nil nil))
  "get a topic by its id, assuming a xtm-id. If xtm-id is empty, the current TM
is chosen. If xtm-id is nil, choose the global TM with its internal ID, if
applicable in the correct revision. If revison is provided, then the code checks
if the topic already existed in this revision and returns nil otherwise.
If no item meeting the constraints was found, then the return value is either
NIL or an error is thrown, depending on error-if-nil."
  (declare (integer revision))
  (let
      ((result 
        (if xtm-id
            (let
                ((possible-items
                  (delete-if-not
                   (lambda (top-id)
                     (and
                      (string= (xtm-id top-id) xtm-id)
                      (string= (uri top-id) topicid))) ;fixes a bug in
                                                       ;get-instances-by-value
                                                       ;that does a
                                                       ;case-insensitive
                                                       ;comparision
                   (elephant:get-instances-by-value 
                    'TopicIdentificationC
                    'uri
                    topicid))))
              (when (and possible-items
                         (identified-construct-p (first possible-items)))
                (unless (= (length possible-items) 1)
                  (error (make-condition 'duplicate-identifier-error 
                                         :message 
                                         (format nil "(length possible-items ~a) for id ~a und xtm-id ~a > 1" possible-items topicid xtm-id)
                                         :uri topicid)))
                (let
                    ((found-topic 
                      (identified-construct (first possible-items))))
                  (if (= revision 0)
                      found-topic 
                      (find-item-by-revision found-topic revision)))))
            (make-instance 'TopicC :from-oid (subseq topicid 1)))))
    (if (and error-if-nil (not result))
        (error (format nil "no such item (id: ~a, tm: ~a, rev: ~a)" topicid xtm-id revision))
        result)))

      
;;;;;;;;;;;;;;;;;;
;;
;; RoleC

(elephant:defpclass RoleC (ReifiableConstructC TypableC)
  ((parent :accessor parent
           :initarg :parent
           :associate AssociationC
           :documentation "Association that this role belongs to")
   (player :accessor player
           :initarg :player
           :associate TopicC
           :documentation "references the topic that is the player in this role"))
  (:documentation "The role that this topic plays in an association (formerly member)"))



(defgeneric RoleC-p (object)
  (:documentation "test if object is a of type RoleC")
  (:method ((object t)) nil)
  (:method ((object RoleC)) object))


(defgeneric parent-p (vi)
  (:documentation "t if this construct has a parent construct")
  (:method ((constr RoleC)) (slot-predicate constr 'parent)))


(defmethod delete-construct :before ((construct RoleC))
                                        ;the way we use roles, we cannot just delete the parent association
                                        ;(at least the second role won't have one left then and will
                                        ;complain)
  (delete-1-n-association construct 'parent)
  (delete-1-n-association construct 'player))

(defmethod find-all-equivalent ((construct RoleC))
  (let
      ((parent (and (slot-boundp construct 'parent)
                    (parent construct))))
    (when parent
      (delete-if-not #'(lambda (cand) (strictly-equivalent-constructs construct cand))
                     (slot-value parent 'roles)))))


(defmethod equivalent-constructs ((role1 RoleC) (role2 RoleC))
  "Association role items are equal if the values of their [type], [player], and [parent] properties are equal (TMDM 5.8)"
                                        ;for the purposes for which we use this method (namely the
                                        ;construction of associations), roles will initially always be
                                        ;unequal regarding their parent properties
  (and
   (= (internal-id (instance-of role1)) (internal-id (instance-of role2)))
   (= (internal-id (player role1)) (internal-id (player role2)))))


;;;;;;;;;;;;;;;;;;
;;
;; AssociationC

(elephant:defpclass AssociationC (ReifiableConstructC ScopableC TypableC)
  ((roles :accessor roles
          :associate (RoleC parent)
          :documentation "(non-empty) list of this association's roles")
   (in-topicmaps
    :associate (TopicMapC associations)
    :many-to-many t
    :documentation "list of all topic maps this association is part of"))
  (:documentation "Association in a Topic Map")
  (:index t))


(defmethod in-topicmaps ((association AssociationC) &key (revision *TM-REVISION*))
  (filter-slot-value-by-revision association 'in-topicmaps :start-revision revision))


(defgeneric AssociationC-p (object)
  (:documentation "test if object is a of type AssociationC")
  (:method ((object t)) nil)
  (:method ((object AssociationC)) object))


(defmethod initialize-instance :around ((instance AssociationC) 
                                        &key 
                                        (roles nil))
  "implements the pseudo-initarg :roles"
  (declare (list roles))
  (let
      ((association (call-next-method)))  
    (dolist (role-tuple roles)
      (make-instance 
       'RoleC 
       :instance-of (getf role-tuple :instance-of)
       :player (getf role-tuple :player)
       :item-identifiers (getf role-tuple :item-identifiers)
       :parent association))))

(defmethod make-construct :around ((class-symbol (eql 'AssociationC))
                                   &key 
                                   start-revision 
                                   &allow-other-keys)
  (declare (ignorable start-revision))
  (let
      ((association
        (call-next-method)))
    (declare (AssociationC association))
    (dolist (role (slot-value association 'roles))
      (unless (versions role)
        (add-to-version-history role
                                :start-revision start-revision)))
    association))

(defmethod copy-item-identifiers :around
    ((from-construct AssociationC)
     (to-construct AssociationC))
  "Internal method to copy over item idenfiers from one association
with its roles to another one. Role identifiers are also
copied. Returns nil if neither association nor role identifiers had to be copied"
  (let
      ((item-identifiers-copied-p nil)) ;rather brutal solution. find a better one
    (when (call-next-method)
      (setf item-identifiers-copied-p t))
    (do ((from-roles (roles from-construct) (rest from-roles))
         (to-roles (roles to-construct) (rest to-roles)))
        ((null from-roles) 'finished)
      (let
          ((from-role (first from-roles))
           (to-role (first to-roles)))
        (when
            (mapc
             (lambda (identifier)
               (setf (identified-construct identifier) 
                     to-role))
             (set-difference (item-identifiers from-role)
                             (item-identifiers to-role)
                             :key #'uri :test #'string=))
          (setf item-identifiers-copied-p t))))
  item-identifiers-copied-p))

(defmethod delete-construct :before ((construct AssociationC))
  (dolist (role (roles construct))
    (delete-construct role))
  (dolist (tm (in-topicmaps construct))
    (elephant:remove-association construct 'in-topicmaps tm)))

(defmethod find-all-equivalent ((construct AssociationC))
  (let
      ((some-player (player (or
                             (second (roles construct))
                             (first (roles construct)))))) ;; dirty, dirty... but brings a tenfold speedup!
    (delete-if-not  
     #'(lambda (cand) 
         (unless (eq construct cand)
           (equivalent-constructs construct cand)))
                                        ;here we need to use the "internal" API and access the players
                                        ;with slot-value (otherwise we won't be able to merge with
                                        ;'deleted' associations)
     (mapcar #'parent (slot-value some-player 'player-in-roles)))))


(defmethod equivalent-constructs ((assoc1 AssociationC) (assoc2 AssociationC))
  "Association items are equal if the values of their [scope], [type], and [roles] properties are equal (TMDM 5.7)"
  (and
   (= (internal-id (instance-of assoc1)) (internal-id (instance-of assoc2)))
   (not (set-exclusive-or (themes assoc1) (themes assoc2) 
                          :key #'internal-id))
   (not (set-exclusive-or
         (roles assoc1)
         (roles assoc2)
         :test  #'equivalent-constructs))))


(elephant:defpclass TopicMapC (ReifiableConstructC)
  ((topics :accessor topics
          :associate (TopicC in-topicmaps)
          :documentation "list of topics that explicitly belong to this TM")
   (associations :accessor associations
                 :associate (AssociationC in-topicmaps)
                 :documentation "list of associations that belong to this TM"))
  (:documentation "Topic Map"))

(defmethod equivalent-constructs ((tm1 TopicMapC) (tm2 TopicMapC))
  "Topic Map items are equal if one of their identifiers is equal"
  ;Note: TMDM does not make any statement to this effect, but it's the
  ;one logical assumption
  (intersection
   (item-identifiers tm1)
   (item-identifiers tm2)
   :test  #'equivalent-constructs))

(defmethod find-all-equivalent ((construct TopicMapC))
  (let 
      ((tms (elephant:get-instances-by-class 'd:TopicMapC)))
    (delete-if-not 
     (lambda(tm)
         (strictly-equivalent-constructs construct tm))
     tms)))

(defgeneric add-to-topicmap (tm top)
  (:documentation "add a topic or an association to a topic
  map. Return the added construct"))

(defmethod add-to-topicmap ((tm TopicMapC) (top TopicC))
  ;TODO: add logic not to add pure topic stubs unless they don't exist yet in the store
;  (elephant:add-association tm 'topics top) ;by adding the elephant association in this order, there will be missing one site of this association
  (elephant:add-association top 'in-topicmaps tm)
  top)

(defmethod add-to-topicmap ((tm TopicMapC) (ass AssociationC))
   ;(elephant:add-association tm 'associations ass)
  (elephant:add-association ass 'in-topicmaps tm)
  ass)

(defgeneric in-topicmap (tm constr &key revision)
  (:documentation "Is a given construct (topic or assiciation) in this topic map?"))

(defmethod in-topicmap ((tm TopicMapC) (top TopicC) &key (revision 0))
  (when (find-item-by-revision top revision)
    (find (d:internal-id top) (d:topics tm) :test #'= :key #'d:internal-id)))


(defmethod in-topicmap ((tm TopicMapC) (ass AssociationC) &key (revision 0))
  (when (find-item-by-revision ass revision)
    (find (d:internal-id ass) (d:associations tm)  :test #'= :key #'d:internal-id)))

;;;;;;;;;;;;;;;;;
;; reification

(defgeneric add-reifier (construct reifier-topic)
  (:method ((construct ReifiableConstructC) reifier-topic)
    (let ((err "From add-reifier(): "))
      (declare (TopicC reifier-topic))
      (cond
	((and (not (reifier construct))
	      (not (reified reifier-topic)))
	 (setf (reifier construct) reifier-topic)
	 (setf (reified reifier-topic) construct))
	((and (not (reified reifier-topic))
	      (reifier construct))
	 (merge-reifier-topics (reifier construct) reifier-topic))
	((and (not (reifier construct))
	      (reified reifier-topic))
	 (error "~a~a ~a reifies already another object ~a"
		err (psis reifier-topic) (item-identifiers reifier-topic)
		(reified reifier-topic)))
	(t
	 (when (not (eql (reified reifier-topic) construct))
	   (error "~a~a ~a reifies already another object ~a"
		  err (psis reifier-topic) (item-identifiers reifier-topic)
		  (reified reifier-topic)))
	 (merge-reifier-topics (reifier construct) reifier-topic)))
      construct)))


(defgeneric merge-reifier-topics (old-topic new-topic)
  ;;the reifier topics are not only merged but also bound to the reified-construct
  (:method ((old-topic TopicC) (new-topic TopicC))
    (unless (eql old-topic new-topic)
      ;merges all identifiers
      (move-identifiers old-topic new-topic)
      (move-identifiers old-topic new-topic :what 'locators)
      (move-identifiers old-topic new-topic :what 'psis)
      (move-identifiers old-topic new-topic :what 'topic-identifiers)
      ;merges all typed-object-associations
      (dolist (typed-construct (used-as-type new-topic))
	(remove-association typed-construct 'instance-of new-topic)
	(add-association typed-construct 'instance-of old-topic))
      ;merges all scope-object-associations
      (dolist (scoped-construct (used-as-theme new-topic))
	(remove-association scoped-construct 'themes new-topic)
	(add-association scoped-construct 'themes old-topic))
      ;merges all topic-maps
      (dolist (tm (in-topicmaps new-topic))
	(add-association tm 'topic old-topic)) ;the new-topic is removed from this tm by deleting it
      ;merges all role-players
      (dolist (a-role (player-in-roles new-topic))
	(remove-association a-role 'player new-topic)
	(add-association a-role 'player old-topic))
      ;merges all names
      (dolist (name (names new-topic))
	(remove-association name 'topic new-topic)
	(add-association name 'topic old-topic))
      ;merges all occurrences
      (dolist (occurrence (occurrences new-topic))
	(remove-association occurrence 'topic new-topic)
	(add-association occurrence 'topic old-topic))
      ;merges all version-infos
      (let ((versions-to-move
	     (loop for vrs in (versions new-topic)
		when (not (find-if #'(lambda(x)
				       (and (= (start-revision x) (start-revision vrs))
					    (= (end-revision x) (end-revision vrs))))
				   (versions old-topic)))
		collect vrs)))
	(dolist (vrs versions-to-move)
	  (remove-association vrs 'versioned-construct new-topic)
	  (add-association vrs 'versioned-construct old-topic)))
      (delete-construct new-topic))
    ;TODO: order/repair all version-infos of the topic itself and add all new
    ;      versions to the original existing objects of the topic
    old-topic))