(asdf:operate 'asdf:load-op 'elephant)
(use-package :elephant)

(defpclass VersionInfoC()
  ((start-revision :initarg :start-revision
		   :accessor start-revision
		   :type integer
		   :initform 0)
   (end-revision :initarg :end-revision
		 :accessor end-revision
		 :type integer
		 :initform 0)
   (versioned-construct :initarg :versioned-construct
			:accessor versioned-construct
			:associate VersionedConstructC)))

(defpclass VersionedConstructC()
  ((versions :initarg :versions
	     :accessor versions
	     :inherit t
	     :associate (VersionInfoC versioned-construct))))


(defpclass VersionedAssociationC(VersionedConstructC)
  ())


(defpclass PointerAssociationC (VersionedAssociationC)
  ((identifier :initarg :identifier
	       :accessor identifier
	       :inherit t
	       :initform (error "From PointerAssociationC(): identifier must be set")
	       :associate PointerC)))


(defpclass ItemIdAssociationC(PointerAssociationC)
  ((parent-construct :initarg :parent-construct
		     :accessor parent-construct
		     :initform (error "From ItemIdAssociationC(): parent-construct must be set")
		     :associate ReifiableConstructC)))


(defpclass TopicMapConstructC()
  ())


(defpclass ReifiableConstructC(TopicMapConstructC)
  ((item-identifiers :associate (ItemIdAssociationC parent-construct)
		     :inherit t)))


(defpclass PointerC(TopicMapConstructC)
  ((uri :initarg :uri
	:accessor uri
	:inherit t
	:type string
	:initform (error "From PointerC(): uri must be set for a pointer")
	:index t)
   (identified-construct :associate (PointerAssociationC identifier)
			 :inherit t)))


(defpclass IdentifierC(PointerC)
  ())


(defpclass ItemIdentifierC(IdentifierC)
  ()
  (:index t))


(open-store '(:BDB "data_base"))
(defvar *p* (make-instance 'PointerC
			   :uri "anyUri"))
(defvar *pa* (make-instance 'PointerAssociationC
			    :identifier *p*))

(defvar *ii* (make-instance 'ItemIdentifierC
			    :uri "anyUri"))

(defvar *pa-ii* (make-instance 'PointerAssociationC
			       :identifier *ii*))

(defvar *ii-2* (make-instance 'ItemIdentifierC
			      :uri "anyUri"))

(defvar *rc* (make-instance 'ReifiableConstructC))


(defvar *ia* (make-instance 'ItemIdAssociationC
			    :identifier *ii-2*
			    :parent-construct *rc*))


(when (not (slot-value *p* 'identified-construct))
  (error ">> 1"))

(when (not (slot-value *pa* 'identifier))
  (error ">> 2"))

(when (not (slot-value *ii* 'identified-construct))
  (error ">> 3"))

(when (not (slot-value *pa-ii* 'identifier))
  (error ">> 4"))

(when (not (slot-value *ii-2* 'identified-construct))
  (error ">> 5"))

(when (not (slot-value *rc* 'item-identifiers))
  (error ">> 6"))

(when (not (slot-value *ia* 'parent-construct))
  (error ">> 7"))

(when (not (slot-value *ia* 'identifier))
  (error ">> 8"))