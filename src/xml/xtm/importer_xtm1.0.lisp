;;+-----------------------------------------------------------------------------
;;+  Isidorus
;;+  (c) 2008-2009 Marc Kuester, Christoph Ludwig, Lukas Giessmann
;;+
;;+  Isidorus is freely distributable under the LGPL license.
;;+  You can find a detailed description in trunk/docs/LGPL-LICENSE.txt.
;;+-----------------------------------------------------------------------------


(in-package :xml-importer)

(defun get-topic-id-xtm1.0 (topic-elem)
  "returns the id attribute of a topic element"
  (declare (dom:element topic-elem))
  (dom:node-value (dom:get-attribute-node topic-elem "id")))


(defun from-resourceRef-elem-xtm1.0 (resourceRef-elem)
  "returns the href attrubte of the passed resourceRef element"
  (when resourceRef-elem
    (dom:get-attribute-ns resourceRef-elem *xtm1.0-xlink* "href")))


(defun from-resourceX-elem-xtm1.0 (parent-elem)
  "handles the following problem: { resourceRef | resourceData }
   and returns a list with a data and a type node"
  (when parent-elem
    (let ((data
	   (let ((resourceRef-elem
		  (xpath-single-child-elem-by-qname parent-elem *xtm1.0-ns* "resourceRef")))
	     (if resourceRef-elem
		 (from-resourceRef-elem-xtm1.0 resourceRef-elem)
		 (let ((resourceData-elem
			(xpath-single-child-elem-by-qname parent-elem *xtm1.0-ns* "resourceData")))
		   (if resourceData-elem
		       (xpath-fn-string resourceData-elem)
		       nil)))))
	  (type
	   (let ((data-elem (xpath-single-child-elem-by-qname parent-elem *xtm1.0-ns* "resourceData")))
	     (declare (dom:element parent-elem))
	     (if data-elem
		 "http://www.w3.org/2001/XMLSchema#string"
		 "http://www.w3.org/2001/XMLSchema#anyURI"))))
      (unless data
	(error "from-resourceX-elem-xtm1.0: one of resourceRef or resourceData must be set"))
      (list :data data :type type))))


(defun from-variant-elem-xtm1.0 (variant-elem parent-construct start-revision &key (xtm-id *current-xtm*))
  "geberates a VariantC object;
   variant = element variant { parameters, variantName?, variant* }"
  (declare (dom:element variant-elem))
  (declare (CharacteristicC parent-construct)) ;;parent name or parent variant object
  (declare (optimize (debug 3)))
  (let ((parameters 
	 (remove-duplicates
	  (remove-if #'null
		     (append
		      (from-parameters-elem-xtm1.0
		       (xpath-single-child-elem-by-qname variant-elem *xtm1.0-ns* "parameters")
		       start-revision :xtm-id xtm-id)
		      (themes parent-construct)))))
	(variantName (from-resourceX-elem-xtm1.0
		      (xpath-single-child-elem-by-qname variant-elem *xtm1.0-ns* "variantName")))
	(parent-name (cond
		       ((typep parent-construct 'NameC)
			parent-construct)
		       ((typep parent-construct 'VariantC)
			(name parent-construct))
		       (t
			(error "from-variant-elem-xtm1.0: parent-cosntruct is neither NameC nor VariantC")))))
    (unless (and variantName parameters)
      (error "from-variant-elem-xtm1.0: parameters and variantName must be set"))
    (let ((variant (make-construct 'VariantC
				   :start-revision start-revision
				   :themes parameters
				   :charvalue (getf variantName :data)
				   :datatype (getf variantName :type)
				   :name parent-name)))
      (let ((inner-variants
	     (map 'list #'(lambda(x)
			    (from-variant-elem-xtm1.0 x variant start-revision :xtm-id xtm-id))
		  (xpath-child-elems-by-qname variant-elem *xtm1.0-ns* "variant"))))
	(append (list variant) inner-variants)))))


(defun from-parameters-elem-xtm1.0 (parameters-elem start-revision &key (xtm-id *current-xtm*))
  "handles a parameters element and returns all referenced TopicC objects
   parameters = element { (topicRef | subjectIndicatorRef)+ }"
  (when parameters-elem
    (let ((parameters
	   (let ((topicRefs
		  (map 'list #'from-topicRef-elem-xtm1.0
		       (xpath-child-elems-by-qname parameters-elem *xtm1.0-ns* "topicRef")))
		 (subjectIndicatorRefs
		  (map 'list #'(lambda(x)
				 (get-xlink-attribute x "href"))
		       (xpath-child-elems-by-qname parameters-elem *xtm1.0-ns* "subjectIndicatorRef"))))
	     (let ((topic-list
		    (append
		     (map 'list #'(lambda(x)
				    (get-item-by-id x :xtm-id xtm-id :revision start-revision))
			  topicRefs)
		     (map 'list #'(lambda(x)
				    (get-item-by-psi x :revision start-revision))
			  subjectIndicatorRefs))))
	       (unless (= (+ (length topicRefs) (length subjectIndicatorRefs)) (length topic-list))
		 (error "from-parameters-elem-xtm1.0: a topic reference is missing"))
	       (remove-duplicates topic-list)))))
      (declare (dom:element parameters-elem))
      parameters)))


(defun get-xlink-attribute (elem attribute-name)
  "returns the attribute \"attribute-name of\" elem"
  (declare (dom:element elem))
  (declare (string attribute-name))
  (dom:get-attribute-ns elem *xtm1.0-xlink* attribute-name))


(defun from-baseName-elem-xtm1.0 (baseName-elem top start-revision &key (xtm-id *current-xtm*))
  "creates NameC instances for the TopicC top
   baseName = element baseName { scope?, baseNameString, variant* }"
  (declare (dom:element baseName-elem))
  (declare (TopicC top))
  (declare (optimize (debug 3)))
  (let ((themes (when (xpath-single-child-elem-by-qname baseName-elem *xtm1.0-ns* "scope")
		  (from-scope-elem-xtm1.0
		   (xpath-single-child-elem-by-qname baseName-elem *xtm1.0-ns* "scope")
		   :xtm-id xtm-id)))
	(baseNameString (xpath-fn-string
			 (xpath-single-child-elem-by-qname baseName-elem *xtm1.0-ns* "baseNameString"))))
    (unless baseNameString
      (error "A baseName must have exactly one baseNameString"))

    (let ((name (make-construct 'NameC 
				:start-revision start-revision
				:topic top
				:charvalue baseNameString
				:themes themes)))
      (map 'list #'(lambda(x)
		     (from-variant-elem-xtm1.0 x name start-revision :xtm-id xtm-id))
	   (xpath-child-elems-by-qname baseName-elem *xtm1.0-ns* "variant"))
      name)))


(defun from-topicRef-elem-xtm1.0 (topicRef-elem)
  "returns all the href attribute of the given topicRef-elem without
   '#' character"
  (when topicRef-elem
    (let ((href (get-xlink-attribute topicRef-elem "href")))
      (declare (dom:element topicRef-elem))
      (unless (char= (elt href 0) #\#)
	(error "cannot handle topicrefs that don't start with #"))
      (subseq href 1))))


(defun get-instanceOf-refs-xtm1.0 (parent-elem &key (xtm-id d:*current-xtm*))
  "returns the topic ids of the topics referenced by the element topicRef and
   subjectIndicatorRef as a list of strings"
  (when parent-elem
    (let ((instanceOf-elems (xpath-child-elems-by-qname parent-elem *xtm1.0-ns* "instanceOf")))
      (when (> (length instanceOf-elems) 0)
	(let ((topicRefs (map 'list #'(lambda(x)
					(when (xpath-single-child-elem-by-qname x *xtm1.0-ns* "topicRef")
					  (from-topicRef-elem-xtm1.0
					   (xpath-single-child-elem-by-qname x *xtm1.0-ns* "topicRef"))))
			      instanceOf-elems))
	      (subjectIndicatorRefs (map 'list #'(lambda(x)
						   (when (xpath-single-child-elem-by-qname
							  x *xtm1.0-ns* "subjectIndicatorRef")
						     (get-xlink-attribute
						      (xpath-single-child-elem-by-qname
						       x *xtm1.0-ns* "subjectIndicatorRef") "href")))
					 instanceOf-elems)))
	  (let ((ids (remove-if #'null(append
				       (map 'list #'(lambda(x)
						      (get-topicid-by-psi x :xtm-id xtm-id))
					    subjectIndicatorRefs)
				       topicRefs))))
	    (declare (dom:element parent-elem))
	    ids))))))


(defun from-roleSpec-elem-xtm1.0 (roleSpec-elem &key (xtm-id *current-xtm*))
  "returns the referenced topic of the roleSpec's topicRef and subjectIndicatorRef element."
  (when roleSpec-elem
    (let ((top-id (when (xpath-single-child-elem-by-qname roleSpec-elem *xtm1.0-ns* "topicRef")
		    (from-topicRef-elem-xtm1.0
		     (xpath-single-child-elem-by-qname roleSpec-elem *xtm1.0-ns* "topicRef"))))
	  (sIRs (map 'list #'(lambda(uri)(get-topicid-by-psi uri :xtm-id xtm-id))
		     (map 'list #'(lambda(x)
				    (dom:get-attribute-ns x *xtm1.0-xlink* "href"))
			  (xpath-child-elems-by-qname roleSpec-elem *xtm1.0-ns* "subjectIndicatorRef")))))
      (let ((ref-topic (first (remove-if #'null
					 (append
					  (list (get-item-by-id top-id :xtm-id xtm-id))
					  (map 'list #'(lambda(id)(get-item-by-id id :xtm-id xtm-id)) sIRs))))))
	(declare (dom:element roleSpec-elem))
	(unless ref-topic
	  (error (make-condition 'missing-reference-error
				 :message (format nil "from-roleSpec-elem-xtm1.0: could not resolve topicid ~a" top-id))))
	ref-topic))))


(defun from-scope-elem-xtm1.0 (scope-elem &key (xtm-id *current-xtm*))
  "returns the topics referenced by this scope element.
   the nested elements resourceRef and subjectIndicatorRef are ignored"
  (when scope-elem
    (when (xpath-child-elems-by-qname scope-elem *xtm1.0-ns* "topicRef")
      (let ((refs 
	     (append (map 'list #'from-topicRef-elem-xtm1.0
			  (xpath-child-elems-by-qname scope-elem *xtm1.0-ns* "topicRef"))
		     (map 'list #'(lambda(uri)(get-topicid-by-psi uri :xtm-id xtm-id))
			  (map 'list #'(lambda(x)
					 (dom:get-attribute-ns x *xtm1.0-xlink* "href"))
			       (xpath-child-elems-by-qname scope-elem *xtm1.0-ns* "subjectIndicatorRef"))))))
	(let ((ref-topics (map 'list
			       #'(lambda(x)
				   (let ((ref-topic (get-item-by-id x :xtm-id xtm-id)))
				     (if ref-topic
					 ref-topic
					 (error (make-condition 'missing-reference-error
								:message (format nil "from-scope-elem-xtm1.0: could not resolve reference ~a" x))))))
			       refs)))
	  (declare (dom:element scope-elem))
	  (unless (>= (length ref-topics) 1)
	    (error "need at least one topic in a scope"))
	  ref-topics)))))


(defun from-occurrence-elem-xtm1.0 (occ-elem top start-revision &key (xtm-id *current-xtm*))
  "creates instances of OccurrenceC with the nested elements instanceOf,
   scope, resourceRef and resourceData"
  (declare (dom:element occ-elem))
  (declare (TopicC top))
  (declare (integer start-revision))
  (let* 
      ((instanceOf (when (get-instanceOf-refs-xtm1.0 occ-elem :xtm-id xtm-id)
		       (get-item-by-id (first (get-instanceOf-refs-xtm1.0 occ-elem :xtm-id xtm-id)) :xtm-id xtm-id)))
       (themes (from-scope-elem-xtm1.0
                (xpath-single-child-elem-by-qname occ-elem *xtm1.0-ns* "scope") 
                :xtm-id xtm-id))
	 (occurrence-value
	  (from-resourceX-elem-xtm1.0 occ-elem)))
    (unless occurrence-value
      (error "from-occurrence-elem-xtm1.0: one of resourceRef and resourceData must be set"))
    (unless instanceOf
      (format t "from-occurrence-elem-xtm1.0: type is missing -> http://psi.topicmaps.org/iso13250/model/type-instance~%")
      (setf instanceOf (get-item-by-id "type-instance" :xtm-id "core.xtm")))
    (make-construct 'OccurrenceC
		    :start-revision start-revision
		    :topic top
                    :themes themes
                    :instance-of instanceOf
                    :charvalue (getf occurrence-value :data)
                    :datatype (getf occurrence-value :type))))


(defun from-subjectIdentity-elem-xtm1.0 (subjectIdentity-elem start-revision)
  "creates PersistentIdC's from the element subjectIdentity"
  (when subjectIdentity-elem
    (let ((psi-refs (map 'list #'(lambda(x)
				   (get-xlink-attribute x "href"))
			 (xpath-child-elems-by-qname subjectIdentity-elem *xtm1.0-ns* "subjectIndicatorRef")))
	  (locator-refs (map 'list #'(lambda(x)
				       (get-xlink-attribute x "href"))
			     (xpath-child-elems-by-qname subjectIdentity-elem *xtm1.0-ns* "resourceRef"))))

      (let ((psis (map 'list #'(lambda(uri)
				 (let ((id (make-instance 'PersistentIdC
							  :uri uri
							  :start-revision start-revision)))
				   ;(add-to-version-history id :start-revision start-revision)
				   id))
		       psi-refs))
	    (locators (map 'list #'(lambda(uri)
				     (let ((loc (make-instance 'SubjectLocatorC
							       :uri uri
							       :start-revision start-revision)))
				       ;(add-to-version-history loc :start-revision start-revision)
				       loc))
			   locator-refs)))
	(declare (dom:element subjectIdentity-elem))
	(declare (integer start-revision))
	(list :psis psis :locators locators)))))


(defun from-member-elem-xtm1.0 (member-elem &key (xtm-id *current-xtm*))
  "returns a list with the role- type, player and itemIdentities"
  (when member-elem
    (elephant:ensure-transaction (:txn-nosync t)
      (let 
          ((type (from-rolespec-elem-xtm1.0 (xpath-single-child-elem-by-qname member-elem *xtm1.0-ns* "roleSpec") :xtm-id xtm-id))
           (player (remove-if #'null 
                              (append
                               (list (get-item-by-id (from-topicRef-elem-xtm1.0
                                                      (xpath-single-child-elem-by-qname
                                                       member-elem
                                                       *xtm1.0-ns*
                                                       "topicRef"))
						      :xtm-id xtm-id))
				(map 'list #'(lambda(topicid)
					       (get-item-by-id topicid :xtm-id xtm-id))
				     (map 'list #'(lambda(uri)(get-topicid-by-psi uri :xtm-id xtm-id))
					  (map 'list #'(lambda(x)
							 (get-xlink-attribute x "href"))
					       (xpath-child-elems-by-qname
						member-elem
						*xtm1.0-ns*
						"subjectIndicatorRef"))))))))
	(declare (dom:element member-elem))
	(unless player ; if no type is given a standard type will be assigend later in from-assoc...
	  (error "from-member-elem-xtm1.0: missing player in role"))
	(list :instance-of type :player (first player) :item-identifiers nil)))))


(defun from-topic-elem-to-stub-xtm1.0 (topic-elem start-revision 
                                       &key 
                                       (xtm-id *current-xtm*))
  "creates a TopicC instance with a start-revision, all psis, the topicid and the xtm-id"
  (declare (dom:element topic-elem))
  (declare (integer start-revision))  
  ;(declare (optimize (debug 3)))
  (elephant:ensure-transaction (:txn-nosync t) 
    (let ((identifiers (from-subjectIdentity-elem-xtm1.0 (xpath-single-child-elem-by-qname
							  topic-elem
							  *xtm1.0-ns*
							  "subjectIdentity")
							 start-revision)))
      (make-construct 'TopicC :start-revision start-revision
                      :psis (getf identifiers :psis)
		      :locators (getf identifiers :locators)
                      :topicid (get-topic-id-xtm1.0 topic-elem)
		      :xtm-id xtm-id))))


(defun merge-topic-elem-xtm1.0 (topic-elem start-revision 
                                &key
                                tm
                                (xtm-id *current-xtm*))
  "Adds further elements (names, occurrences) and instanceOf
   associations to the topic"
  (declare (dom:element topic-elem))
  (declare (integer start-revision))
  (declare (TopicMapC tm))
  (elephant:ensure-transaction (:txn-nosync t)
    (let 
        ((top
          (get-item-by-id
           (get-topic-id-xtm1.0 topic-elem) 
           :xtm-id xtm-id :revision start-revision))
         (instanceOf-topicRefs (remove-if #'null (get-instanceOf-refs-xtm1.0 topic-elem :xtm-id xtm-id)))
         (baseName-elems (xpath-child-elems-by-qname topic-elem *xtm1.0-ns* "baseName"))
         (occ-elems (xpath-child-elems-by-qname topic-elem *xtm1.0-ns* "occurrence")))
      (unless top
	(error "topic ~a could not be found" (get-attribute topic-elem "id")))
      ;;names
      (map 'list #'(lambda(x)
		     (from-baseName-elem-xtm1.0 x top start-revision :xtm-id xtm-id))
	   baseName-elems)
      ;;occurrences
      (map 'list #'(lambda(x)
		     (from-occurrence-elem-xtm1.0 x top start-revision :xtm-id xtm-id))
	   occ-elems)
      ;;instanceOf
      (dolist (instanceOf-topicRef instanceOf-topicRefs)
	(create-instanceof-association instanceOf-topicRef top start-revision :xtm-id xtm-id
                                       :tm tm))
      (add-to-topicmap tm top))))


(defun from-association-elem-xtm1.0 (assoc-elem start-revision &key tm (xtm-id *current-xtm*))
  (declare (dom:element assoc-elem))
  (declare (integer start-revision))
  (declare (TopicMapC tm))
  (elephant:ensure-transaction (:txn-nosync t)
    (let ((type (when (get-instanceOf-refs-xtm1.0 assoc-elem :xtm-id xtm-id)
		  (get-item-by-id (first (get-instanceOf-refs-xtm1.0 assoc-elem :xtm-id xtm-id)) :xtm-id xtm-id)))
	  (themes 
           (from-scope-elem-xtm1.0 
            (xpath-single-child-elem-by-qname assoc-elem *xtm1.0-ns* "scope") 
            :xtm-id xtm-id))
	  (roles (map 'list 
                      #'(lambda(member-elem)
                          (from-member-elem-xtm1.0 
                           member-elem :xtm-id xtm-id))
                      (xpath-child-elems-by-qname assoc-elem *xtm1.0-ns* "member"))))
      ;(format t "type: ~A~%themes: ~A~%roles: ~A~%~%" type themes roles)
      (unless roles
	(error "from-association-elem-xtm1.0: roles are missing in association"))
      (setf roles (set-standard-role-types roles))
      (unless type
	(format t "from-association-elem-xtm1.0: type is missing -> http://www.topicmaps.org/xtm/1.0/core.xtm#association~%")
	(setf type (get-item-by-id "association" :xtm-id "core.xtm")))
      (let 
          ((association (make-construct 'AssociationC
                                        :start-revision start-revision
                                        :instance-of type
                                        :themes themes
                                        :roles roles)))
        (add-to-topicmap tm association)
	association))))


(defun set-standard-role-types (roles)
  "sets the missing role types of the passed roles to the default types."
  (when roles
    (let ((empty-roles (loop for role in roles
			  when (not (getf role :instance-of))
			  collect role)))
      (when empty-roles
	(let ((is-type (loop for role in roles
			  when (and (getf role :instance-of)
				    (loop for psi in (psis (getf role :instance-of))
				       when (string= (uri psi)
						     "http://psi.topicmaps.org/iso13250/model/type")
				       return t))
			  return t)))
	  (declare (list roles))
	  (when (not is-type)
	    (loop for role in roles
	       when (not (getf role :instance-of))
	       do (setf (getf role :instance-of) (get-item-by-id "type" :xtm-id "core.xtm"))
		  (format t "set-standard-role-types: role type is missing -> http://psi.topicmaps.org/iso13250/model/type~%")
		 (return t)))
	  (when (or (> (length empty-roles) 1) (and empty-roles (not is-type)))
	    (loop for role in roles
	       when (not (getf role :instance-of))
	       do (setf (getf role :instance-of) (get-item-by-id "instance" :xtm-id "core.xtm"))
		  (format t "set-standard-role-types: role type is missing -> http://psi.topicmaps.org/iso13250/model/instance~%"))))))
    roles))


(defun importer-xtm1.0 (xtm-dom 
                        &key 
                        (tm-id (error "you must provide a stable identifier (PSI-style) for this TM"))
                        (xtm-id d:*current-xtm*) 
                        (revision (get-revision)))
  "imports the elements topic and association from a passed dom by caling
   the from-<elem>-xtm1.0 functions"
  ;TODO: remove code duplication between importer-xtm1.0 and importer
  (declare (dom:element xtm-dom))
  (declare (integer revision))
  (assert elephant:*store-controller*)
  (with-writer-lock
    (with-tm (revision xtm-id tm-id)
      (let 
	  ((topic-vector (xpath-child-elems-by-qname xtm-dom *xtm1.0-ns* "topic"))
	   (assoc-vector (xpath-child-elems-by-qname  xtm-dom *xtm1.0-ns* "association")))    
	(loop for topic across topic-vector
	   do (from-topic-elem-to-stub-xtm1.0 topic revision 
					      :xtm-id xtm-id))
	(loop for top-elem across topic-vector
	   do
	     (format t "t")
	     (merge-topic-elem-xtm1.0 top-elem revision
				      :tm tm
				      :xtm-id xtm-id))
	(loop for assoc-elem across assoc-vector 
	   do
	     (format t "a")
	     (from-association-elem-xtm1.0 assoc-elem revision
					   :tm tm
					   :xtm-id xtm-id))))))
