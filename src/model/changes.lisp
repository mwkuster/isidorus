;;+-----------------------------------------------------------------------------
;;+  Isidorus
;;+  (c) 2008-2009 Marc Kuester, Christoph Ludwig, Lukas Giessmann
;;+
;;+  Isidorus is freely distributable under the LGPL license.
;;+  You can find a detailed description in trunk/docs/LGPL-LICENSE.txt.
;;+-----------------------------------------------------------------------------


;-*- standard-indent:2; tab-width:2; indent-tabs-mode:nil -*-
(in-package :datamodel)

(defun get-all-revisions ()
  "Returns an ordered set of the start dates of all revisions in the engine"
                                        ;TODO: this is a very inefficient implementation... it would equally
                                        ;be possible to have a separate object that stored all such
                                        ;revisions and only make the search from the latest version that's
                                        ;stored their
  (let
      ((revision-set))
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
    ;(format t "tops-and-assocs: ~a~&" (mapcan #'versions tops-and-assocs))
    (dolist (vi (mapcan #'versions tops-and-assocs))
      ;(format t "(start-revision vi): ~a~&" (start-revision vi))
      (pushnew (start-revision vi) revision-set))
    (sort revision-set #'<)))


(defun find-associations-for-topic (top)
  "find all associations of this topic"
  (let
      ((type-instance-topic
        (d:identified-construct
         (elephant:get-instance-by-value 'PersistentIdC
                                         'uri
                                         "http://psi.topicmaps.org/iso13250/model/type-instance"))))
  (remove 
   type-instance-topic
   (remove-duplicates 
    (map 'list #'parent (player-in-roles top))) 
   :key #'instance-of)))
  

(defgeneric find-referenced-topics (construct)
  (:documentation "find all the topics that are references from this construct as type, scope or player, as the case may be"))

(defmethod find-referenced-topics ((characteristic CharacteristicC))
  "characteristics are scopable + typable"
  (append
   (when (reifier characteristic)
     (list (reifier characteristic)))
   (themes characteristic)
   (when (instance-of-p characteristic)
     (list (instance-of characteristic)))
   (when  (and (typep characteristic 'OccurrenceC)
              (> (length (charvalue characteristic)) 0)
              (eq #\# (elt (charvalue characteristic) 0)))
     (list (get-item-by-id (subseq (charvalue characteristic)  1))))))


(defmethod find-referenced-topics ((role RoleC))
  (append
   (when (reifier role)
     (list (reifier role)))
   (list (instance-of role))
   (list (player role))))

(defmethod find-referenced-topics ((association AssociationC))
  "associations are scopable + typable"
  (append
   (when (reifier association)
     (list (reifier association)))
   (list (instance-of association))
   (themes association)
   (mapcan #'find-referenced-topics (roles association))))
  

(defmethod find-referenced-topics ((top TopicC))
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
     (list-instanceOf top)
     (mapcan #'find-referenced-topics (names top))
     (mapcan #'find-referenced-topics (mapcan #'variants (names top)))
     (mapcan #'find-referenced-topics (occurrences top))
     (mapcan #'find-referenced-topics (find-associations-for-topic top))))))
   

(defgeneric changed-p (construct revision)
  (:documentation "Has the topic map construct changed in a given revision? 'Changed' can mean: 
    * newly created
    * modified through the addition or removal of identifiers
    * (for associations) modified through the addition or removal of identifiers in the association or one of its roles
    * (for topics) modified through the addition or removal of identifiers or characteristics
    * (for topics) modified through the addition or removal of an association in which it is first player"))

(defmethod changed-p ((construct TopicMapConstructC) (revision integer))
  "The 'normal' case: changes only when new identifiers are added" 
  (find revision (versions construct) :test #'= :key #'start-revision))

;There is quite deliberately no method specialized on AssociationC as
;copy-item-identifiers for Associations already guarantees that the
;version history of an association is only updated when the
;association itself is really updated

(defmethod changed-p ((topic TopicC) (revision integer))
  "A topic is changed if one of its child elements (identifiers or
characteristics) or one of the associations in which it is first player has changed"
  (let*
      ((first-player-in-associations
        (remove-if-not
         (lambda (association)
           (eq (player (first (roles association)))
               topic))
         (find-associations-for-topic topic)))
       (all-constructs
        (union
         (get-all-identifiers-of-construct topic)
         (union 
          (names topic)
          (union
           (occurrences topic)
           first-player-in-associations)))))
    (some
     (lambda (construct)
       (changed-p construct revision))
     all-constructs)))


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
         (map 'list 
              (lambda (top)
                (when (changed-p top revision)
                  (make-instance 'FragmentC
                                 :revision revision
                                 :associations (find-associations-for-topic top) ;TODO: this quite probably introduces code duplication with query: Check!
                                 :referenced-topics (find-referenced-topics top)
                                 :topic top)))
              (elephant:get-instances-by-class 'TopicC))))))

(defun get-fragment (unique-id)
  "get a fragment by its unique id"
  (declare (integer unique-id))
  (elephant:get-instance-by-value 'FragmentC 
                                  'unique-id
                                  unique-id))

(defgeneric mark-as-deleted (construct &key source-locator revision)
  (:documentation "Mark a construct as deleted if it comes from the source indicated by
source-locator"))

(defmethod mark-as-deleted ((construct TopicMapConstructC) &key source-locator revision)
  "Mark a topic as deleted if it comes from the source indicated by
source-locator"
  (declare (ignorable source-locator))
  (let
      ((last-version ;the last active version
        (find 0 (versions construct) :key #'end-revision)))
    (when last-version
      (setf (end-revision last-version) revision))))

(defmethod mark-as-deleted :around ((ass AssociationC) &key source-locator revision)
  "Mark an association and its roles as deleted"
  (mapc (lambda (role) (mark-as-deleted role :revision revision :source-locator source-locator))
        (roles ass))
  (call-next-method))

(defmethod mark-as-deleted :around ((top TopicC) &key source-locator revision)
  "Mark a topic as deleted if it comes from the source indicated by
source-locator"
  ;;Part 1b, 1.4.3.3.1:
  ;; Let SP be the value of the ServerSourceLocatorPrefix element in the ATOM feed F
  ;; * Let SI be the value of TopicSI element in ATOM entry E
  ;; * feed F contains E
  ;; * entry E references topic fragment TF
  ;; * Let LTM be the local topic map
  ;; * Let T be the topic in LTM that has a subjectidentifier that matches SI
  ;; * For all names, occurrences and associations in which T plays a role, TMC
  ;;   * Delete all SrcLocators of TMC that begin with SP. If the count of srclocators on TMC = 0 then delete TMC 
  ;;   * Merge in the fragment TF using SP as the base all generated source locators. 

  (when
      (some (lambda (psi) (string-starts-with (uri psi) source-locator)) (psis top))
    (mapc (lambda (name) (mark-as-deleted name :revision revision :source-locator source-locator))
          (names top))
    (mapc (lambda (occ) (mark-as-deleted occ :revision revision :source-locator source-locator))
          (occurrences top))
    (mapc (lambda (ass) (mark-as-deleted ass :revision revision :source-locator source-locator))
          (find-associations-for-topic top))
    (call-next-method)))

(defgeneric add-source-locator (construct &key source-locator revision)
  (:documentation "adds an item identifier to a given construct based on the source
locator and an internally generated id (ideally a uuid)"))

(defmethod add-source-locator ((construct ReifiableConstructC) &key source-locator revision)
  (declare (ignorable revision))
  (unless
      (some (lambda (ii) (string-starts-with (uri ii) source-locator)) (item-identifiers construct))
    (let
        ((ii-uri (format nil "~a/~d" source-locator (internal-id construct))))
      (make-instance 'ItemIdentifierC :uri ii-uri :identified-construct construct :start-revision revision))))

(defmethod add-source-locator ((top TopicC) &key source-locator revision)
  ;topics already have the source locator in (at least) one PSI, so we
  ;do not need to add an extra item identifier to them. However, we
  ;need to do that for all their characteristics + associations
  (mapc (lambda (name) (add-source-locator name :revision revision :source-locator source-locator))
          (names top))
  (mapc (lambda (occ) (add-source-locator occ :revision revision :source-locator source-locator))
        (occurrences top))
  (mapc (lambda (ass) (add-source-locator ass :revision revision :source-locator source-locator))
        (find-associations-for-topic top)))


(defun create-latest-fragment-of-topic (topic-psi)
  "Returns the latest fragment of the passed topic-psi"
  (declare (string topic-psi))
  (let ((topic
	 (get-item-by-psi topic-psi)))
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
			     :associations (find-associations-for-topic topic)
			     :referenced-topics (find-referenced-topics topic)
			     :topic topic)))))))


(defun get-latest-fragment-of-topic (topic-psi)
  "Returns the latest existing fragment of the passed topic-psi."
  (declare (string topic-psi))
  (let ((topic
	 (get-item-by-psi topic-psi)))
    (when topic
      (let ((existing-fragments
	     (elephant:get-instances-by-value 'FragmentC 'topic topic)))
	(when existing-fragments
	  (first (sort existing-fragments
		       #'(lambda(frg-1 frg-2)
			   (> (revision frg-1) (revision frg-2))))))))))