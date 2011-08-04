;;+-----------------------------------------------------------------------------
;;+  Isidorus
;;+  (c) 2008-2010 Marc Kuester, Christoph Ludwig, Lukas Georgieff
;;+
;;+  Isidorus is freely distributable under the LLGPL license.
;;+  You can find a detailed description in trunk/docs/LLGPL-LICENSE.txt and
;;+  trunk/docs/LGPL-LICENSE.txt.
;;+-----------------------------------------------------------------------------

(in-package :datamodel)

(defun get-all-revisions ()
  "Returns an ordered set of the start dates of all revisions in the engine"
  ;TODO: this is a very inefficient implementation... it would equally
  ;be possible to have a separate object that stored all such
  ;revisions and only make the search from the latest version that's
  ;stored their
  (let ((revision-set))
    (dolist (vi (elephant:get-instances-by-class 'VersionInfoC))
      (pushnew (start-revision vi) revision-set))
    (sort revision-set #'<)))


(defun get-all-revisions-for-tm (tm-id)
  "Returns an ordered set of the start dates of all revisions in the
engine for this Topic Map"
  (let*
      ((tm (get-item-by-item-identifier tm-id :revision 0))
       (tops-and-assocs (when tm (union (topics tm) (associations tm))))
       (revision-set nil))
    (dolist (vi (mapcan #'versions tops-and-assocs))
      (pushnew (start-revision vi) revision-set))
    (sort revision-set #'<)))


(defgeneric find-all-associations (instance &key revision)
  (:documentation "Finds all associations for a topic.")
  (:method ((instance TopicC) &key (revision *TM-REVISION*))
    (declare (type (or integer null) revision))
    (delete-if #'null
	       (remove-duplicates 
		(map 'list #'(lambda(role)
			       (parent role :revision revision))
		     (player-in-roles instance :revision revision))))))


(defgeneric find-associations (instance &key revision)
  (:documentation "Finds all associations of this topic except
                   type-instance-associations.")
  (:method ((instance TopicC) &key (revision *TM-REVISION*))
    (declare (type (or integer null) revision))
    (let ((type-instance-topic
	   (d:identified-construct
	    (elephant:get-instance-by-value
	     'PersistentIdC 'uri *type-instance-psi*))))
      (delete-if
       #'(lambda(assoc)
	   (eql (instance-of assoc :revision revision)
		type-instance-topic))
       (find-all-associations instance :revision revision)))))
  

(defgeneric find-referenced-topics (construct &key revision)
  (:documentation "find all the topics that are references from this construct as type, scope or player, as the case may be"))


(defmethod find-referenced-topics ((characteristic CharacteristicC)
				   &key (revision *TM-REVISION*))
  "Characteristics are scopable + typable + reifiable.
   Note the tmdm:topic-name is ignored if it is only set
   as a nametype."
  (append
   (when (reifier characteristic :revision revision)
     (list (reifier characteristic :revision revision)))
   (themes characteristic :revision revision)
   (when (and (not (and (typep characteristic 'NameC)
			(eql (instance-of characteristic :revision revision)
			     (get-item-by-psi *topic-name-psi* :revision revision))))
	      (instance-of characteristic :revision revision))
     (list (instance-of characteristic :revision revision)))
   (when (and (typep characteristic 'NameC)
	      (variants characteristic :revision revision))
     (delete-if #'null
		(loop for var in (variants characteristic :revision revision)
		   append (find-referenced-topics var :revision revision))))
   (when  (and (typep characteristic 'OccurrenceC)
              (> (length (charvalue characteristic)) 0)
              (eq #\# (elt (charvalue characteristic) 0)))
     (list (get-item-by-id (subseq (charvalue characteristic)  1)
			   :revision revision)))))


(defmethod find-referenced-topics ((role RoleC)
				   &key (revision *TM-REVISION*))
  (append
   (when (reifier role :revision revision)
     (list (reifier role :revision revision)))
   (list (instance-of role :revision revision))
   (list (player role :revision revision))))


(defmethod find-referenced-topics ((association AssociationC)
				   &key (revision *TM-REVISION*))
  "associations are scopable + typable"
  (append
   (when (reifier association :revision revision)
     (list (reifier association :revision revision)))
   (list (instance-of association :revision revision))
   (themes association :revision revision)
   (mapcan #'(lambda(role)
	       (find-referenced-topics role :revision revision))
	   (roles association :revision revision))))
  

(defmethod find-referenced-topics ((top TopicC)
				   &key (revision *TM-REVISION*))
  "Part 1b of the eGov-Share spec states:
# for each topicname in T export a topic stub for each scope topic
# for each occurrence in T export a topic stub for the occurrence type (if it exists)
# for each occurrence in T export a topic stub for each scope topic
# for each association A in which T plays a role export the association
# for each association A export a topic stub for the association type
# for each association A export a topic stub for each topic scope topic
# for each role R in A export a topic stub for the role type and one for the role player UNLESS the role player is T"
  (remove-duplicates
   (remove
    top
    (append
     (list-instanceOf top :revision revision)
     (mapcan #'(lambda(name)
		 (find-referenced-topics name :revision revision))
	     (names top :revision revision))
     (mapcan #'(lambda(variant)
		 (find-referenced-topics variant :revision revision))
	     (mapcan #'variants (names top :revision revision)))
     (mapcan #'(lambda(occ)
		 (find-referenced-topics occ :revision revision))
	     (occurrences top :revision revision))
     (mapcan #'(lambda(assoc)
		 (find-referenced-topics assoc :revision revision))
	     (find-associations top :revision revision))))))
   

(defgeneric initial-version-p (version-info)
  (:documentation "A helper function for changed-p that returns the passed
                   version-info object if it is the initial version-info object,
                   i.e. it owns the smallest start-revsion of the
                   version-construct.")
  (:method ((version-info VersionInfoC))
    (unless (find-if #'(lambda(vi)
			 (< (start-revision vi) (start-revision version-info)))
		     (versions (versioned-construct version-info)))
      version-info)))


(defgeneric changed-p (construct revision)
  (:documentation "Has the topic map construct changed in a given revision?
                   'Changed' can mean: 
    * newly created
    * deletion of an element
    * modified through the addition or removal of identifiers
    * (for associations) modified through the addition or removal of
       identifiers in the association or one of its roles
    * (for topics) modified through the addition or removal of identifiers
       or characteristics
    * (for topics) modified through the addition or removal of an association
       in which it is first player"))


(defmethod changed-p ((construct TopicMapConstructC) (revision integer))
  "changed-p returns nil for TopicMapConstructCs that are not specified
   more detailed. The actual algorithm is processed for all
   VersionedConstructCs."
  (declare (ignorable revision))
  nil)


(defmethod changed-p ((construct PointerC) (revision integer))
  "Returns t if the PointerC was added to a construct the first
   time in the passed revision"
  (let ((version-info (some #'(lambda(pointer-association)
				(changed-p pointer-association revision))
			    (slot-p construct 'identified-construct))))
    (when version-info
      (initial-version-p version-info))))


(defmethod changed-p ((construct VersionedConstructC) (revision integer))
  "changed-p returns t if there exist a VersionInfoC with the given start-revision."
  (let ((version-info
	 (find revision (versions construct) :test #'= :key #'start-revision)))
    (when version-info
      (initial-version-p version-info))))


(defmethod changed-p ((construct CharacteristicC) (revision integer))
  "Returns t if the CharacteristicC was added to a construct in the passed
   revision or if <ReifiableConstructC> changed."
  (or (call-next-method)
      (let ((version-info
	     (some #'(lambda(characteristic-association)
		       (changed-p characteristic-association revision))
		   (slot-p construct 'parent))))
	(when version-info
	  (initial-version-p version-info)))))


(defmethod changed-p ((construct RoleC) (revision integer))
  "Returns t if the RoleC was added to a construct in the passed
   revision or if <ReifiableConstructC> changed."
  (or (call-next-method)
      (let ((version-info
	     (some #'(lambda(role-association)
		       (changed-p role-association revision))
		   (slot-p construct 'parent))))
	(when version-info
	  (initial-version-p version-info)))))


(defgeneric end-revision-p (construct revision)
  (:documentation "A helper function for changed-p. It returns the latest
                   version-info if the passed versioned-construct was
                   marked-as-deleted in the version that is given.")
  (:method ((construct VersionedConstructC) (revision integer))
    (let ((version-info (find revision (versions construct)
			      :key #'end-revision :test #'=)))
      (when (and version-info
		 (not
		  (find-if
		   #'(lambda(vi)
		       (or (> (end-revision vi) (end-revision version-info))
			   (= (end-revision vi) 0)))
		   (versions construct))))
	version-info))))


(defmethod changed-p ((construct ReifiableConstructC) (revision integer))
  "Returns t if a ReifiableConstructC changed in the given version, i.e.
   an item-identifier or reifier was added to the construct itself."
  (or (some #'(lambda(vc)
		(changed-p vc revision))
	    (union (item-identifiers construct :revision revision)
		   (let ((reifier-top (reifier construct :revision revision)))
		     (when reifier-top
		       (list reifier-top)))))
      (some #'(lambda(vc)
		(end-revision-p vc revision))
	    (union (slot-p construct 'item-identifiers)
		   (slot-p construct 'reifier)))))


(defmethod changed-p ((construct NameC) (revision integer))
  "Returns t if the passed NameC changed in the given version, i.e.
   the <ReifiableConstructC> characteristics or the variants changed."
  (or (call-next-method)
      (some #'(lambda(var)
		(changed-p var revision))
	    (variants construct :revision revision))
      (some #'(lambda(vc)
		(end-revision-p vc revision))
	    (slot-p construct 'variants))))


(defmethod changed-p ((construct TopicC) (revision integer))
  "Returns t if the passed TopicC changed in the given version, i.e.
   the <ReifiableConstructC>, <PersistentIdC>, <LocatorC>, <NameC>,
   <OccurrenceC>, <AssociationC> or the reified-construct changed."
  (or (call-next-method)
      (some #'(lambda(vc)
		(changed-p vc revision))
	    (union
	     (union
	      (union (psis construct :revision revision)
		     (locators construct :revision revision))
	      (union (names construct :revision revision)
		     (occurrences construct :revision revision)))
	     (delete-if-not
	      (lambda (assoc)
		(eq (player (first (roles assoc :revision revision))
			    :revision revision)
		    construct))
	      (find-all-associations construct :revision revision))))
      (let ((rc (reified-construct construct :revision revision)))
	(when rc
	  (let ((ra (find-if #'(lambda(reifier-assoc)
				 (eql (reifiable-construct reifier-assoc) rc))
			     (slot-p construct 'reified-construct))))
	    (changed-p ra revision))))
      (some #'(lambda(vc)
		(end-revision-p vc revision))
	    (union (union (union (slot-p construct 'psis)
				 (slot-p construct 'locators))
			  (union (slot-p construct 'names)
				 (slot-p construct 'occurrences)))
		   (slot-p construct 'reified-construct)))))
	   


(defmethod changed-p ((construct AssociationC) (revision integer))
  "Returns t if the passed AssociationC changed in the given version, i.e.
   the <RoleC> or the <ReifiableConstructC> changed."
  (or (call-next-method)
      (some #'(lambda(role)
		(changed-p role revision))
	    (roles construct :revision revision))
      (some #'(lambda(vc)
		(end-revision-p vc revision))
	    (slot-p construct 'roles))))


(defpclass FragmentC ()
  ((revision :type integer
             :initarg :revision
             :accessor revision
             :index t
             :documentation "revision in question")
   (unique-id :initarg :unique-id
              :accessor unique-id
              :index t
              :documentation "a unique id for this fragment. for now
              just its OID, but may become a true UUID in the future")
   (topic :type TopicC
          :initarg :topic
          :accessor topic
          :index t
          :documentation "changed topic (topicSI in Atom")
   (serializer-cache :type String
		     :initform nil
		     :initarg :serializer-cache
		     :documentation "contains te serialized string
                                     value of this FragmentC instance,
                                     that can contain any string format,
                                     e.g. JTM, XTM, ... depending on the
                                     setter method.")
   (serializer-notes :type List
		     :initform nil
		     :initarg :serializer-notes
		     :documentation "contains a list of the forms
                                     (:psis <int> :iis <int> :sls <int>
                                      :names <int> :occurrences <int>
                                      :roles <int>) that indicates the
                                     number of elements this fragment's
                                     topic is bound to. It is only necessary
                                     to recognize mark-as-deleted elements,
                                     since newly added elements will result
                                     in a completely new fragment.")
   (referenced-topics
    :type list
    :initarg :referenced-topics
    :accessor referenced-topics
    :documentation "list of topics that this topic references somehow (through associations, types, scopes in the characteristics etc.")
   (associations
    :type list
    :initarg :associations
    :accessor associations
    :documentation "list of association that this topic is a player in")))

(defmethod initialize-instance :after ((fragment FragmentC) &key)
  "initialze the unique id of the fragment ot some suitable value"
  (setf (slot-value fragment 'unique-id) (elephant::oid fragment)))


(defun get-fragments (revision)
  "Gets the list of all fragments for a given revision. Returns a
list of FragmentC objects"
  (declare (integer revision))
  (let
      ((cached-fragments
        (elephant:get-instances-by-value 'FragmentC
                                         'revision
                                         revision)))
    (if cached-fragments
        cached-fragments
        (remove 
         nil 
         (map
	  'list 
	  (lambda (top)
	    (when (changed-p top revision)
	      (make-instance 'FragmentC
			     :revision revision
			     :associations (find-associations
					    top :revision revision)
					;TODO: this quite probably introduces
					;code duplication with query: Check!
			     :referenced-topics (find-referenced-topics
						 top :revision revision)
			     :topic top)))
	  (get-all-topics revision))))))

(defun get-fragment (unique-id)
  "get a fragment by its unique id"
  (declare (integer unique-id))
  (elephant:get-instance-by-value 'FragmentC 
                                  'unique-id
                                  unique-id))

(defgeneric add-source-locator (construct &key source-locator revision)
  (:documentation "adds an item identifier to a given construct based on the source
                   locator and an internally generated id (ideally a uuid)"))


(defmethod add-source-locator ((construct ReifiableConstructC) &key source-locator revision)
  (declare (integer revision))
  (unless
      (some (lambda (ii)
	      (string-starts-with (uri ii) source-locator))
	    (item-identifiers construct :revision revision))
    (let
        ((ii-uri (format nil "~a/~d" source-locator (internal-id construct))))
      (make-construct 'ItemIdentifierC
		      :uri ii-uri
		      :identified-construct construct
		      :start-revision revision))))


(defmethod add-source-locator ((top TopicC) &key source-locator revision)
  ;topics already have the source locator in (at least) one PSI, so we
  ;do not need to add an extra item identifier to them. However, we
  ;need to do that for all their characteristics + associations
  (mapc (lambda (name)
	  (add-source-locator name :revision revision
			      :source-locator source-locator))
	(names top :revision revision))
  (mapc (lambda (occ)
	  (add-source-locator occ :revision revision
			      :source-locator source-locator))
        (occurrences top :revision revision))
  (mapc (lambda (ass)
	  (add-source-locator ass :revision revision
			      :source-locator source-locator))
        (find-associations top :revision revision)))


(defun create-latest-fragment-of-topic (topic-psi)
  "Returns the latest fragment of the passed topic-psi"
  (declare (string topic-psi))
  (let ((topic (get-latest-topic-by-psi topic-psi)))
    (when topic
      (let ((start-revision
	     (start-revision
	      (find-if #'(lambda(x)
			   (when (= 0 (end-revision x))
			     t))
		       (versions topic)))))
	(let ((existing-fragment
	       (find-if #'(lambda(x)
			    (when (eq topic (topic x))
			      t))
			(get-fragments start-revision))))
	  (if existing-fragment
	      existing-fragment
	      (make-instance 'FragmentC
			     :revision start-revision
			     :associations (find-associations
					    topic :revision start-revision)
			     :referenced-topics (find-referenced-topics
						 topic :revision start-revision)
			     :topic topic)))))))


(defun get-latest-fragment-of-topic (topic-psi)
  "Returns the latest existing fragment of the passed topic-psi."
  (declare (string topic-psi))
  (let ((topic (get-latest-topic-by-psi topic-psi)))
    (when topic
      (let ((existing-fragments
	     (elephant:get-instances-by-value 'FragmentC 'topic topic)))
	(when existing-fragments
	  (first (sort existing-fragments
		       #'(lambda(frg-1 frg-2)
			   (> (revision frg-1) (revision frg-2))))))))))


(defgeneric serializer-cache (fragment)
  (:documentation "returns the slot value of serializer-cache or nil,
                   if it is unbound.")
  (:method ((fragment FragmentC))
    (when (slot-boundp fragment 'serializer-cache)
      (slot-value fragment 'serializer-cache))))


(defgeneric serializer-notes (fragment)
  (:documentation "returns the slot value of serializer-notes or nil,
                   if it is unbound.")
  (:method ((fragment FragmentC))
    (when (slot-boundp fragment 'serializer-notes)
      (slot-value fragment 'serializer-notes))))


(defgeneric serializer-notes-changed-p (fragment)
  (:documentation "Returns t if the serializer-notes slot contains
                   a value that does not correspond to the actual
                   values of the fragment.")
  (:method ((fragment FragmentC))
    (let ((top (topic fragment))
	  (sn (serializer-notes fragment)))
      (or (/= (length (psis top :revision 0))
	      (getf sn :psis))
	  (/= (length (item-identifiers top :revision 0))
	      (getf sn :iis))
	  (/= (length (locators top :revision 0))
	      (getf sn :sls))
	  (/= (length (names top :revision 0))
	      (getf sn :names))
	  (/= (length (occurrences top :revision 0))
	      (getf sn :occurrences))
	  (/= (length (player-in-roles top :revision 0))
	      (getf sn :roles))))))