;;+-----------------------------------------------------------------------------
;;+  Isidorus
;;+  (c) 2008-2009 Marc Kuester, Christoph Ludwig, Lukas Giessmann
;;+
;;+  Isidorus is freely distributable under the LGPL license.
;;+  You can find a detailed description in trunk/docs/LGPL-LICENSE.txt.
;;+-----------------------------------------------------------------------------
(in-package :rdf-importer)


(defvar *document-id* "isidorus-rdf-document")


(defun setup-rdf-module (rdf-xml-path repository-path 
                         &key tm-id (document-id (get-uuid)))
  "Sets up the data base by importing core_psis.xtm and
   rdf_core_psis.xtm afterwards the file corresponding
   to the give file path is imported."
  (declare ((or pathname string) rdf-xml-path))
  (declare ((or pathname string) repository-path))
  (unless elephant:*store-controller*
    (elephant:open-store  
     (get-store-spec repository-path)))
  (xml-importer:init-isidorus)
  (init-rdf-module)
  (rdf-importer rdf-xml-path repository-path :tm-id tm-id
		:document-id document-id)
  (when elephant:*store-controller*
    (elephant:close-store)))


(defun rdf-importer (rdf-xml-path repository-path 
		     &key 
		     (tm-id nil)
		     (document-id (get-uuid))
		     (start-revision (d:get-revision)))
  "Imports the file correponding to the given path."
  (setf *document-id* document-id)
  (tm-id-p tm-id "rdf-importer")
  (with-writer-lock
    (unless elephant:*store-controller*
      (elephant:open-store
       (get-store-spec repository-path)))
    (elephant:ensure-transaction (:txn-nosync t)
      (let ((rdf-dom
	     (dom:document-element (cxml:parse-file
				    (truename rdf-xml-path)
				    (cxml-dom:make-dom-builder)))))
	(import-dom rdf-dom start-revision :tm-id tm-id :document-id document-id))
      (setf *_n-map* nil))))


(defun init-rdf-module (&optional (revision (get-revision)))
  "Imports the file rdf_core_psis.xtm. core_psis.xtm has to be imported
   before."
  (with-writer-lock
    (with-tm (revision "rdf.xtm" "http://isidorus/rdf2tm_mapping/rdf.xtm")
      (let
	  ((core-dom 
	    (cxml:parse-file *rdf_core_psis.xtm* (cxml-dom:make-dom-builder))))
	(elephant:ensure-transaction (:txn-nosync t)
	  (loop for top-elem across 
	       (xpath-child-elems-by-qname (dom:document-element core-dom)
					   *xtm2.0-ns* "topic")
	     do
	       (let
		   ((top
		     (from-topic-elem-to-stub top-elem revision
					      :xtm-id *rdf-core-xtm*)))
		 (add-to-topicmap xml-importer::tm top))))))))


(defun import-dom (rdf-dom start-revision
		   &key (tm-id nil) (document-id *document-id*))
  "Imports the entire dom of a rdf-xml-file."
  (setf *_n-map* nil) ;in case of an failed last call
  (tm-id-p tm-id "import-dom")
  (let ((xml-base (get-xml-base rdf-dom))
	(xml-lang (get-xml-lang rdf-dom))
	(elem-name (get-node-name rdf-dom))
	(elem-ns (dom:namespace-uri rdf-dom)))
    (if (and (string= elem-ns *rdf-ns*)
	     (string= elem-name "RDF"))
	(let ((children (child-nodes-or-text rdf-dom :trim t)))
	  (when children
	    (loop for child across children
	       when (non-isidorus-type-p child tm-id :parent-xml-base xml-base)
	       do (import-node child tm-id start-revision :document-id document-id
			       :xml-base xml-base :xml-lang xml-lang))))
	(import-node rdf-dom tm-id start-revision :document-id document-id
		     :xml-base xml-base :xml-lang xml-lang)))
  (setf *_n-map* nil))


(defun import-node (elem tm-id start-revision &key (document-id *document-id*)
		    (xml-base nil) (xml-lang nil))
  (format t ">> import-node: ~a <<~%" (dom:node-name elem)) ;TODO: remove
  (tm-id-p tm-id "import-node")
  (parse-node elem)
  (let ((fn-xml-lang (get-xml-lang elem :old-lang xml-lang))
	(fn-xml-base (get-xml-base elem :old-base xml-base)))
    (let ((about (get-absolute-attribute elem tm-id xml-base "about"))	   
	  (nodeID (get-ns-attribute elem "nodeID"))
	  (ID (get-absolute-attribute elem tm-id xml-base "ID"))
	  (UUID (get-ns-attribute elem "UUID" :ns-uri *rdf2tm-ns*)))
      (parse-properties-of-node elem (or about nodeID ID UUID))
      ;TODO: create associaitons and roles
    (let ((literals (append (get-literals-of-node elem fn-xml-lang)
			    (get-literals-of-node-content
			     elem tm-id xml-base fn-xml-lang)))
	  (associations (get-associations-of-node-content elem tm-id xml-base))
	  (types (get-types-of-node elem tm-id :parent-xml-base xml-base))
	  (super-classes
	   (get-super-classes-of-node-content elem tm-id xml-base))
	  (subject-identities (make-isidorus-identifiers
				elem start-revision :what "subjectIdentifier"))
	  (item-identifiers (make-isidorus-identifiers elem start-revision))
	  (subject-locators (make-isidorus-identifiers elem start-revision
						       :what "subjectLocator")))
      (with-tm (start-revision document-id tm-id)
	(let ((this
	       (make-topic-stub
		about ID nodeID UUID start-revision xml-importer::tm
		:document-id document-id
		:additional-subject-identifiers subject-identities
		:item-identifiers item-identifiers
		:subject-locators subject-locators)))
	  (make-isidorus-names elem this tm-id start-revision
			       :owner-xml-base fn-xml-base)
	  ;TODO: create topic occurrences
	  (make-literals this literals tm-id start-revision
			 :document-id document-id)
	  (make-associations this associations xml-importer::tm
			     start-revision :document-id document-id)
	  (make-types this types xml-importer::tm start-revision
		      :document-id document-id)
	  (make-super-classes this super-classes xml-importer::tm
			      start-revision :document-id document-id)
	  (make-recursion-from-node elem tm-id start-revision
				    :document-id document-id
				    :xml-base xml-base
				    :xml-lang xml-lang)
	  this))))))



(defun make-isidorus-names (owner-elem owner-topic tm-id start-revision 
			    &key (owner-xml-base nil)
			    (document-id *document-id*))
  "Creates all names of a resource node that are in a property isidorus:name
   and have the type isidorus:Name."
  (declare (dom:element owner-elem))
  (declare (string tm-id))
  (declare (TopicC owner-topic))
  (let ((content (child-nodes-or-text owner-elem :trim t))
	(root (elt (dom:child-nodes (dom:owner-document owner-elem)) 0)))
    (when (and (not (stringp content))
	       (> (length content) 0))
      (loop for property across content
	 when (isidorus-type-p property tm-id 'name
			       :parent-xml-base owner-xml-base)
	 collect 
	   (let ((xml-base (get-xml-base property 
					 :old-base owner-xml-base)))
	     (let ((nodes 
		    (let ((nodeID (get-ns-attribute property "nodeID")))
		      (if nodeID
			  (get-all-isidorus-nodes-by-id
			   nodeID root *tm2rdf-name-type-uri*)
			  (list (self-or-child-node
				 property *tm2rdf-name-type-uri*
				 :xml-base xml-base))))))
	       (let ((item-identities
		      (remove-if #'null
				 (loop for node in nodes
				    append (make-isidorus-identifiers
					    (getf node :elem) start-revision))))
		     (name-type (make-name-type nodes tm-id start-revision
						:document-id document-id))
		     (name-value (getf (make-value nodes tm-id) :value))
		     (name-scopes (make-scopes nodes tm-id start-revision
					       :document-id document-id)))
		 (let ((this
			(make-construct 'NameC
					:start-revision start-revision
					:topic owner-topic
					:charvalue name-value
					:instance-of name-type
					:item-identifiers item-identities
					:themes name-scopes)))
		   (make-isidorus-variants nodes this tm-id start-revision
					   :document-id document-id)))))))))


(defun make-isidorus-variants (name-nodes owner-name tm-id start-revision
			       &key (document-id *document-id*))
  "Creates name variants of the passed name-nodes."
  (declare (NameC owner-name))
  (declare (string tm-id))
  (let ((root 
	 (when name-nodes
	   (elt (dom:child-nodes 
		 (dom:owner-document (getf (first name-nodes) :elem))) 0))))
    (remove-if
     #'null
     (loop for name-node in name-nodes
	collect (let ((content (child-nodes-or-text (getf name-node :elem))))
		  (when (and (not (stringp content))
			     (> (length content) 0))
		    (loop for property across content
		       when (isidorus-type-p
			     property tm-id 'variant
			     :parent-xml-base (getf name-node :xml-base))
		       collect 
			 (let ((nodes 
				(let ((nodeID 
				       (get-ns-attribute property "nodeID")))
				  (if nodeID
				      (get-all-isidorus-nodes-by-id
				       nodeID root *tm2rdf-name-type-uri*)
				      (list (self-or-child-node
					     property
					     *tm2rdf-variant-type-uri*
					     :xml-base 
					     (get-xml-base 
					      property
					      :old-base 
					      (getf name-node :xml-base))))))))
			   (let ((item-identities
				  (remove-if 
				   #'null
				   (loop for node in nodes
				      append (make-isidorus-identifiers
					      (getf node :elem) start-revision))))
				 (variant-scopes
				  (append
				   (make-scopes nodes tm-id start-revision
						:document-id document-id)
				   (themes owner-name))) ;XTM 2.0: 4.12
				 (value-and-type (make-value nodes tm-id)))	   
			     (make-construct 'VariantC
					     :start-revision start-revision
					     :item-identifiers item-identities
					     :themes variant-scopes
					     :charvalue 
					     (getf value-and-type :value)
					     :datatype 
					     (getf value-and-type :datatype)
					     :name owner-name))))))))))						  


(defun make-scopes (node-list tm-id start-revision
		    &key (document-id *document-id*))
  "Creates for every found scope a corresponding topic stub."
  (let ((properties
	 (remove-if
	  #'null
	  (loop for node in node-list
	     append (let ((content (child-nodes-or-text (getf node :elem)
							:trim t)))
		      (loop for property across content
			 when (let ((prop-ns (dom:namespace-uri property))
				    (prop-name (get-node-name property)))
				(string= (concatenate-uri prop-ns prop-name)
					 *tm2rdf-scope-property*))
			 collect (list :elem property
				       :xml-base (get-xml-base 
						  property
						  :old-base 
						  (getf node :xml-base)))))))))
    (let ((scope-uris
	   (remove-if #'null
		      (map 'list #'(lambda(x)
				     (get-ref-of-property (getf x :elem) tm-id 
							  (getf x :xml-base)))
			   properties))))
      (with-tm (start-revision document-id tm-id)
	(map 'list #'(lambda(x)
		       (let ((topicid (getf x :topicid))
			     (psi (getf x :psi)))
			 (make-topic-stub psi nil topicid nil start-revision
					  xml-importer::tm 
					  :document-id document-id)))
	     scope-uris)))))


(defun make-value (node-list tm-id)
  "Returns the literal value of a property of the type isidorus:value."
  (let ((property
	 (loop for node in node-list
	    when (or (let ((content (child-nodes-or-text (getf node :elem)
							 :trim t)))
		       (loop for property across content
			  when (let ((prop-ns (dom:namespace-uri property))
				     (prop-name (get-node-name property)))
				 (string= (concatenate-uri prop-ns prop-name)
					  *tm2rdf-value-property*))
			  return property))
		     (get-ns-attribute (getf node :elem) 
				       "value" :ns-uri *tm2rdf-ns*))
	    return (or (let ((content (child-nodes-or-text (getf node :elem)
							   :trim t)))
			 (loop for property across content
			    when (let ((prop-ns (dom:namespace-uri property))
				       (prop-name (get-node-name property)))
				   (string= (concatenate-uri prop-ns prop-name)
					    *tm2rdf-value-property*))
			    return property))
		       (get-ns-attribute (getf node :elem)
					 "value" :ns-uri *tm2rdf-ns*)))))
    (if property
	(if (stringp property)
	    (list :value property :datatype *xml-string*)
	    (let ((prop-content (child-nodes-or-text property))
		  (type (let ((dt
			       (get-datatype 
				property tm-id
				(find-if #'(lambda(x)
					     (eql property (getf x :elem)))
					 node-list))))
			  (if dt dt *xml-string*))))
	      (cond
		((= (length prop-content) 0)
		 (list :value "" :datatype type))
		((not (stringp prop-content)) ;must be an element
		 (let ((text-val ""))
		   (when (dom:child-nodes property)
		     (loop for content-node across
			  (dom:child-nodes property)
			do (push-string
			    (node-to-string content-node)
			    text-val)))
		   (list :value text-val :datatype type)))
		(t (list :value prop-content :datatype type)))))
	(list :value "" :datatype *xml-string*))))
  
  

(defun make-name-type (node-list tm-id start-revision 
		       &key (document-id *document-id*))
  "Creates a topic stub that is the type of the name represented by the
   passed nodes."
  (let ((property
	 (loop for node in node-list
	    when (let ((content (child-nodes-or-text (getf node :elem) 
						     :trim t)))
		   (loop for property across content
		      when (let ((prop-ns (dom:namespace-uri property))
				 (prop-name (get-node-name property)))
			     (string= (concatenate-uri prop-ns prop-name)
				      *tm2rdf-nametype-property*))
		      return property))
	    return (let ((content (child-nodes-or-text (getf node :elem)
						       :trim t)))
		     (loop for property across content
			when (let ((prop-ns (dom:namespace-uri property))
				   (prop-name (get-node-name property)))
			       (string= (concatenate-uri prop-ns prop-name)
					*tm2rdf-nametype-property*))
			return (list
				:elem property 
				:xml-base (get-xml-base property
							:old-base
							(getf
							 node 
							 :xml-base))))))))
    (when property
      (let ((type-uri (get-ref-of-property (getf property :elem) tm-id
					   (getf property :xml-base))))
	(unless type-uri
	  (error "From make-name-type(): type-uri is missing!"))
	(with-tm (start-revision document-id tm-id)
	  (make-topic-stub (getf type-uri :psi) nil 
			   (getf type-uri :topicid) nil start-revision
			   xml-importer::tm :document-id document-id))))))


(defun import-arc (elem tm-id start-revision
		   &key (document-id *document-id*)
		   (xml-base nil) (xml-lang nil))
  "Imports a property that is an blank_node and continues the recursion
   on this element."
  (declare (dom:element elem))
  (format t ">> import-arc: ~a <<~%" (dom:node-name elem)) ;TODO: remove
  (let ((fn-xml-lang (get-xml-lang elem :old-lang xml-lang))
	(UUID (get-ns-attribute elem "UUID" :ns-uri *rdf2tm-ns*))
	(parseType (get-ns-attribute elem "parseType"))
	(content (child-nodes-or-text elem :trim t)))
    (with-tm (start-revision document-id tm-id)
      (if (and (string= parseType "Collection")
	       (= (length content) 0))
	    (make-topic-stub *rdf-nil* nil nil nil start-revision
			     xml-importer::tm :document-id document-id)
	  (let ((this-topic
		 (when (or (not parseType)
			   (and parseType
				(string/= parseType "Collection")))
		   (when UUID
		     (parse-properties-of-node elem UUID)
		     (let ((subject-identifiers 
			    (make-isidorus-identifiers
			     elem start-revision :what "subjectIdentifier"))
			   (item-identities
			    (make-isidorus-identifiers elem start-revision))
			   (subject-locators
			    (make-isidorus-identifiers elem start-revision
						       :what "subjectLocator")))
		       (let ((this
			      (make-topic-stub
			       nil nil nil UUID start-revision xml-importer::tm
			       :additional-subject-identifiers 
			       subject-identifiers
			       :item-identifiers item-identities
			       :subject-locators subject-locators
			       :document-id document-id)))
			 (let ((literals
				(append (get-literals-of-property
					 elem fn-xml-lang)
					(get-literals-of-node-content
					 elem tm-id xml-base fn-xml-lang)))
			       (associations
				(get-associations-of-node-content
				 elem tm-id xml-base))
			       (types (get-types-of-property
				       elem tm-id
				       :parent-xml-base xml-base))
			       (super-classes
				(get-super-classes-of-node-content
				 elem tm-id xml-base)))
			   (make-isidorus-names elem this tm-id start-revision
						:owner-xml-base xml-base
						:document-id document-id)
			   ;TDOD: create topic occurrences
			   (make-literals this literals tm-id start-revision
					  :document-id document-id)
			   (make-associations
			    this associations xml-importer::tm
			    start-revision :document-id document-id)
			   (make-types this types xml-importer::tm start-revision
				       :document-id document-id)
			   (make-super-classes
			    this super-classes xml-importer::tm
			    start-revision :document-id document-id))
			 this))))))
	    (make-recursion-from-arc elem tm-id start-revision
				     :document-id document-id
				     :xml-base xml-base :xml-lang xml-lang)
	    this-topic)))))


(defun make-collection (elem tm-id start-revision
			&key (document-id *document-id*)
			(xml-base nil) (xml-lang nil))
  "Creates a collection structure of a node that contains
   parseType='Collection."
  (declare (dom:element elem))
  (with-tm (start-revision document-id tm-id)
    (let ((fn-xml-base (get-xml-base elem :old-base xml-base))
	  (fn-xml-lang (get-xml-lang elem :old-lang xml-lang))
	  (UUID (get-ns-attribute elem "UUID" :ns-uri *rdf2tm-ns*)))
      (let ((this (make-topic-stub nil nil nil UUID start-revision
				   xml-importer::tm
				   :document-id document-id))
	    (items (loop for item across (child-nodes-or-text elem :trim t)
		      collect (import-node item tm-id start-revision
					   :document-id document-id
					   :xml-base fn-xml-base
					   :xml-lang fn-xml-lang))))
	(let ((last-blank-node this))
	  (dotimes (index (length items))
	    (let ((is-end
		   (if (= index (- (length items) 1))
		       t
		       nil)))
	      (let ((new-blank-node
		     (make-collection-association
		      last-blank-node (elt items index) tm-id  start-revision
		      :is-end is-end :document-id document-id)))
		(setf last-blank-node new-blank-node)))))))))


(defun make-collection-association (current-blank-node first-object tm-id
				    start-revision &key (is-end nil)
				    (document-id *document-id*))
  "Creates a 'first'-association between the current-blank-node and the
   first-object. If is-end is set to true another association between
   current-blank-node and the topic rdf:nil is created. Otherwise this
   associaiton is made from the current-blank-node to a new created blank
   node."
  (declare (d:TopicC current-blank-node first-object))
  (with-tm (start-revision document-id tm-id)
    (let ((first-arc
	   (make-topic-stub *rdf-first* nil nil nil start-revision 
			    xml-importer::tm :document-id document-id))
	  (rest-arc
	   (make-topic-stub *rdf-rest* nil nil nil start-revision
			    xml-importer::tm :document-id document-id)))
      (make-association-with-nodes current-blank-node first-object first-arc
				   xml-importer::tm start-revision
				   :document-id document-id)
      (if is-end
	  (let ((rdf-nil (make-topic-stub *rdf-nil* nil nil nil
					  start-revision xml-importer::tm
					  :document-id document-id)))
	    (make-association-with-nodes
	     current-blank-node rdf-nil rest-arc xml-importer::tm
	     start-revision :document-id document-id)
	    nil)
	  (let ((new-blank-node (make-topic-stub
				 nil nil nil (get-uuid) start-revision
				 xml-importer::tm :document-id document-id)))
	    (make-association-with-nodes
	     current-blank-node new-blank-node rest-arc xml-importer::tm
	     start-revision :document-id document-id)
	    new-blank-node)))))


(defun make-literals (owner-top literals tm-id start-revision
		      &key (document-id *document-id*))
  "Creates Topic Maps constructs (occurrences) of the passed 
   named list literals related to the topic owner-top."
  (declare (d:TopicC owner-top))
  (map 'list #'(lambda(literal)
		 (make-occurrence owner-top literal start-revision
				  tm-id :document-id document-id))
       (filter-isidorus-literals literals)))


(defun make-associations (owner-top associations tm start-revision
			  &key (document-id *document-id*))
  "Creates Topic Maps constructs (assocaitions) of the passed 
   named list literals related to the topic owner-top."
  (declare (d:TopicC owner-top))
  (map 'list #'(lambda(assoc)
		 (make-association owner-top assoc tm
				   start-revision
				   :document-id document-id))
       associations))


(defun make-types (owner-top types tm start-revision
		   &key (document-id *document-id*))
  "Creates instance-of associations corresponding to the passed
   topic owner-top and the passed types."
  (declare (d:TopicC owner-top))
  (map 'list
       #'(lambda(type)
	   (let ((type-topic
		  (make-topic-stub (getf type :psi)
				   nil
				   (getf type :topicid)
				   nil start-revision tm
				   :document-id document-id))
		 (ID (getf type :ID)))
	     (make-instance-of-association owner-top type-topic
					   ID start-revision tm
					   :document-id document-id)))
       types))


(defun make-super-classes (owner-top super-classes tm start-revision
			   &key (document-id *document-id*))
  "Creates supertype-subtype associations corresponding to the passed
   topic owner-top and the passed super classes."
  (declare (d:TopicC owner-top))
  (map 'list
       #'(lambda(class)
	   (let ((class-topic
		  (make-topic-stub (getf class :psi)
				   nil
				   (getf class :topicid)
				   nil start-revision tm
				   :document-id document-id))
		 (ID (getf class :ID)))
	     (make-supertype-subtype-association
	      owner-top class-topic ID start-revision tm
	      :document-id document-id)))
       super-classes))


(defun make-supertype-subtype-association (sub-top super-top reifier-id
					   start-revision tm
					   &key (document-id *document-id*))
  "Creates an supertype-subtype association."
  (declare (TopicC sub-top super-top))
  (declare (TopicMapC tm))
  (let ((assoc-type
	 (make-topic-stub *supertype-subtype-psi* nil nil nil
			  start-revision tm :document-id document-id))
	(role-type-1
	 (make-topic-stub *supertype-psi* nil nil nil
			  start-revision tm :document-id document-id))
	(role-type-2
	 (make-topic-stub *subtype-psi* nil nil nil
			  start-revision tm :document-id document-id))
	(err-pref "From make-supertype-subtype-association(): "))
    (unless assoc-type
      (error "~athe association type ~a is missing!"
	     err-pref *supertype-subtype-psi*))
    (unless (or role-type-1 role-type-2)
      (error "~aone of the role types ~a ~a is missing!"
	     err-pref *supertype-psi* *subtype-psi*))
    (let ((a-roles (list (list :instance-of role-type-1
			       :player super-top)
			 (list :instance-of role-type-2
			       :player sub-top))))
      (when reifier-id
	(make-reification reifier-id sub-top super-top
			  assoc-type start-revision tm
			  :document-id document-id))
      (add-to-topicmap
       tm
       (make-construct 'AssociationC
		       :start-revision start-revision
		       :instance-of assoc-type
		       :roles a-roles)))))


(defun make-instance-of-association (instance-top type-top reifier-id
				     start-revision tm
				     &key (document-id *document-id*))
  "Creates and returns an instance-of association."
  (declare (TopicC type-top instance-top))
  (declare (TopicMapC tm))
  (let ((assoc-type
	 (make-topic-stub *type-instance-psi* nil nil nil
			  start-revision tm :document-id document-id))
	(roletype-1
	 (make-topic-stub *type-psi* nil nil nil
			  start-revision tm :document-id document-id))
	(roletype-2
	 (make-topic-stub *instance-psi* nil nil nil
			  start-revision tm :document-id document-id))
	(err-pref "From make-instance-of-association(): "))
    (unless assoc-type
      (error "~athe association type ~a is missing!"
	     err-pref *type-instance-psi*))
    (unless (or roletype-1 roletype-2)
      (error "~aone of the role types ~a ~a is missing!"
	     err-pref *type-psi* *instance-psi*))
    (let ((a-roles (list (list :instance-of roletype-1
			       :player type-top)
			 (list :instance-of roletype-2
			       :player instance-top))))
      (when reifier-id
	(make-reification reifier-id instance-top type-top
			  assoc-type start-revision tm
			  :document-id document-id))
      (add-to-topicmap
       tm
       (make-construct 'AssociationC
		       :start-revision start-revision
		       :instance-of assoc-type
		       :roles a-roles)))))


(defun make-topic-stub (about ID nodeId UUID start-revision
			tm &key (document-id *document-id*)
			(additional-subject-identifiers nil)
			(item-identifiers nil) (subject-locators nil))
  "Returns a topic corresponding to the passed parameters.
   When the searched topic does not exist there will be created one.
   If about or ID is set there will also be created a new PSI."
  (declare (TopicMapC tm))
  (let ((topic-id (or about ID nodeID UUID))
	(psi-uri (or about ID)))
    (let ((top 
	   ;seems like there is a bug in get-item-by-id:
	   ;this functions returns an emtpy topic although there is no one
	   ;witha corresponding topic id and/or version and/or xtm-id
	   (let ((inner-top
		  (get-item-by-id topic-id :xtm-id document-id
				  :revision start-revision)))
	     (when (and inner-top
			(find-if #'(lambda(x)
				     (= (d::start-revision x) start-revision))
				 (d::versions inner-top)))
	       inner-top))))
      (if top
	  top
	  (let ((psis (if psi-uri
			  (remove-if
			   #'null
			   (append
			    (list 
			     (make-instance 'PersistentIdC
					    :uri psi-uri
					    :start-revision start-revision))
			    additional-subject-identifiers))
			  additional-subject-identifiers)))
	    (handler-case (add-to-topicmap
			   tm
			   (make-construct 'TopicC
					   :topicid topic-id
					   :psis psis
					   :item-identifiers item-identifiers
					   :locators subject-locators
					   :xtm-id document-id
					   :start-revision start-revision))
	      (Condition (err)(error "Creating topic ~a failed: ~a"
				     topic-id err))))))))


(defun make-lang-topic (lang start-revision tm
			&key (document-id *document-id*))
  "Returns a topic with the topicid tm-id/lang. If no such topic exist
   there will be created one."
  (when lang
    (let ((psi-and-topic-id
	   (concatenate-uri *rdf2tm-scope-prefix* lang)))
      (let ((top (get-item-by-id psi-and-topic-id :xtm-id document-id
				 :revision start-revision)))
	(if top
	    top
	    (make-topic-stub psi-and-topic-id nil nil nil start-revision
			     tm :document-id document-id))))))


(defun make-association (top association tm start-revision
			 &key (document-id *document-id*))
  "Creates an association depending on the given parameters and
   returns the elephat-associaton object."
  (declare (TopicC top))
  (declare (TopicMapC tm))
  (let ((type (getf association :type))
	(player-id (getf association :topicid))
	(player-psi (getf association :psi))
	(ID (getf association :ID)))
    (let ((player-1 (make-topic-stub player-psi nil player-id nil
				     start-revision
				     tm :document-id document-id))
	  (role-type-1
	   (make-topic-stub *rdf2tm-object* nil nil nil
			    start-revision tm :document-id document-id))
	  (role-type-2
	   (make-topic-stub *rdf2tm-subject* nil nil nil
			    start-revision tm :document-id document-id))
	  (type-top (make-topic-stub type nil nil nil start-revision
				     tm :document-id document-id)))
      (let ((roles (list (list :instance-of role-type-1
			       :player player-1)
			 (list :instance-of role-type-2
			       :player top))))
	(when ID
	  (make-reification ID top player-1 type-top start-revision
			    tm :document-id document-id))
	(add-to-topicmap tm (make-construct 'AssociationC
					    :start-revision start-revision
					    :instance-of type-top
					    :roles roles))))))


(defun make-association-with-nodes (subject-topic object-topic
				    associationtype-topic tm start-revision
				    &key (document-id *document-id*))
  "Creates an association with two roles that contains the given players."
  (declare (TopicC subject-topic object-topic associationtype-topic))
  (declare (TopicMapC tm))
  (let ((role-type-1
	 (make-topic-stub *rdf2tm-subject* nil nil nil start-revision
			  tm :document-id document-id))
	(role-type-2
	 (make-topic-stub *rdf2tm-object* nil nil nil start-revision
			  tm :document-id document-id)))
    (let ((roles (list (list :instance-of role-type-1
			     :player subject-topic)
		       (list :instance-of role-type-2
			     :player object-topic))))
      (add-to-topicmap tm (make-construct 'AssociationC
					  :start-revision start-revision
					  :instance-of associationtype-topic
					  :roles roles)))))


(defun make-reification (reifier-id subject object predicate start-revision tm
			 &key document-id)
  "Creates a reification construct."
  (declare (string reifier-id))
  (declare ((or OccurrenceC TopicC) object))
  (declare (TopicC subject predicate))
  (declare (TopicMapC tm))

  (let ((reifier (make-topic-stub reifier-id nil nil nil start-revision tm
				  :document-id document-id))
	(predicate-arc (make-topic-stub *rdf-predicate* nil nil nil start-revision
					tm :document-id document-id))
	(object-arc (make-topic-stub *rdf-object* nil nil nil start-revision
				     tm :document-id document-id))
	(subject-arc (make-topic-stub *rdf-subject* nil nil nil start-revision
				      tm :document-id document-id))
	(statement (make-topic-stub *rdf-statement* nil nil nil start-revision
				    tm :document-id document-id)))
    (make-instance-of-association reifier statement nil start-revision tm
				  :document-id document-id)
    (make-association-with-nodes reifier subject subject-arc tm
				 start-revision :document-id document-id)
    (make-association-with-nodes reifier predicate predicate-arc
				 tm start-revision :document-id document-id)
    (if (typep object 'd:TopicC)
	(make-association-with-nodes reifier object object-arc
				     tm start-revision
				     :document-id document-id)
	(make-construct 'd:OccurrenceC
			:start-revision start-revision
			:topic reifier
			:themes (themes object)
			:instance-of (instance-of object)
			:charvalue (charvalue object)
			:datatype (datatype object)))))


(defun make-occurrence (top literal start-revision tm-id 
			&key (document-id *document-id*))
  "Creates an accorrence from the literal list and returns
   the created elephant-occurrence-object."
  (declare (TopicC top))
  (tm-id-p tm-id "make-occurrence")
  (with-tm (start-revision document-id tm-id)
    (let ((type (getf literal :type))
	  (value (getf literal :value))
	  (lang (getf literal :lang))
	  (datatype (getf literal :datatype))
	  (ID (getf literal :ID)))
      (let ((type-top (make-topic-stub type nil nil nil start-revision
				       xml-importer::tm
				       :document-id document-id))
	    (lang-top (make-lang-topic lang start-revision
				       xml-importer::tm
				       :document-id document-id)))
	(let ((occurrence
	       (make-construct 'OccurrenceC 
			       :start-revision start-revision
			       :topic top
			       :themes (when lang-top
					 (list lang-top))
			       :instance-of type-top
			       :charvalue value
			       :datatype datatype)))
	  (when ID
	    (make-reification ID top occurrence type-top start-revision
			      xml-importer::tm :document-id document-id))
	  occurrence)))))
	    

(defun get-literals-of-node-content (node tm-id xml-base xml-lang)
  "Returns a list of literals that is produced of a node's content."
  (declare (dom:element node))
  (tm-id-p tm-id "get-literals-of-noode-content")
  (let ((properties (non-isidorus-child-nodes-or-text node :trim t))
	(fn-xml-base (get-xml-base node :old-base xml-base))
	(fn-xml-lang (get-xml-lang node :old-lang xml-lang)))
    (let ((literals
	   (when properties
	     (loop for property across properties
		when (let ((datatype (get-ns-attribute property "datatype"))
			   (parseType (get-ns-attribute property "parseType"))
			   (nodeID (get-ns-attribute property "nodeID"))
			   (resource (get-ns-attribute property "resource"))
			   (UUID (get-ns-attribute property "UUID"
						   :ns-uri *rdf2tm-ns*))
			   (type (get-ns-attribute property "type"))
			   (prop-literals (get-literals-of-property
					   property nil))
			   (prop-content (child-nodes-or-text property)))
		       (and (or datatype
				(and parseType
				     (string= parseType "Literal"))
				(and (not (or nodeID resource UUID parseType))
				     (or (not prop-content)
					 (stringp prop-content))))
			    (not (or prop-literals type))
			    (string/= parseType "Collection")
			    (string/= parseType "Resource")))
		collect (let ((content (child-nodes-or-text property))
			      (ID (get-absolute-attribute property tm-id
							  fn-xml-base "ID"))
			      (child-xml-lang
			       (get-xml-lang property :old-lang fn-xml-lang)))
			  (let ((full-name (get-type-of-node-name property))
				(datatype (get-datatype property tm-id fn-xml-base))
				(text
				 (cond
				   ((= (length content) 0)
				    "")
				   ((not (stringp content)) ;must be an element
				    (let ((text-val ""))
				      (when (dom:child-nodes property)
					(loop for content-node across
					     (dom:child-nodes property)
					   do (push-string
					       (node-to-string content-node)
					       text-val)))
				      text-val))
				   (t content))))
			    (list :type full-name
				  :value text
				  :ID ID
				  :lang child-xml-lang
				  :datatype datatype)))))))
      literals)))


(defun get-types-of-node-content (node tm-id xml-base)
  "Returns a list of type-uris that corresponds to the node's content
   or attributes."
  (tm-id-p tm-id "get-types-of-node-content")
  (let ((fn-xml-base (get-xml-base node :old-base xml-base)))
    (let ((attr-type
	   (if (get-ns-attribute node "type")
	       (list
		(list :topicid (absolutize-value (get-ns-attribute node "type")
						 fn-xml-base tm-id)
		      :psi (absolutize-value (get-ns-attribute node "type")
					     fn-xml-base tm-id)
		      :ID nil))
	       nil))
	  (content-types
	   (when (non-isidorus-child-nodes-or-text node :trim t)
	     (loop for child across (non-isidorus-child-nodes-or-text node :trim t)
		when (and (string= (dom:namespace-uri child) *rdf-ns*)
			  (string= (get-node-name child) "type"))
		collect (let ((nodeID (get-ns-attribute child "nodeID"))
			      (resource (get-absolute-attribute
					 child tm-id fn-xml-base "resource"))
			      (UUID (get-ns-attribute child "UUID"
						      :ns-uri *rdf2tm-ns*))
			      (ID (get-absolute-attribute child tm-id
							  fn-xml-base "ID")))
			  (if (or nodeID resource UUID)
			      (list :topicid (or nodeID resource UUID)
				    :psi resource
				    :ID ID)
			      (let ((child-xml-base
				     (get-xml-base child :old-base fn-xml-base)))
				(let ((refs
				       (get-node-refs
					(child-nodes-or-text child :trim t)
					tm-id child-xml-base)))
				  (list :topicid (getf (first refs) :topicid)
					:psi (getf (first refs) :psi)
					:ID ID)))))))))
      (remove-if #'null (append attr-type content-types)))))


(defun get-literals-of-property (property xml-lang)
  "Returns a list of attributes that are treated as literal nodes."
  (let ((fn-xml-lang (get-xml-lang property :old-lang xml-lang))
	(attributes nil))
    (dom:map-node-map
     #'(lambda(attr)
	 (let ((attr-ns (dom:namespace-uri attr))
	       (attr-name (get-node-name attr)))
	   (let ((l-type (get-type-of-node-name attr))
		 (l-value (if (get-ns-attribute property attr-name
						:ns-uri attr-ns)
			      (get-ns-attribute property attr-name
						:ns-uri attr-ns)
			      "")))
	     (cond
	       ((string= attr-ns *rdf-ns*)
		(unless (or (string= attr-name "ID")
			    (string= attr-name "resource")
			    (string= attr-name "nodeID")
			    (string= attr-name "type")
			    (string= attr-name "parseType")
			    (string= attr-name "datatype"))
		  (push (list :type l-type
			      :value l-value
			      :ID  nil
			      :lang fn-xml-lang
			      :datatype *xml-string*)
			attributes)))
	       ((or (string= attr-ns *xml-ns*)
		    (string= attr-ns *xmlns-ns*))
		nil);;do nothing, all xml-attributes are no literals
	       (t
		(unless (and (string= attr-ns *rdf2tm-ns*)
			     (string= attr-name "UUID"))
		  (push (list :type l-type
			      :value l-value
			      :ID nil
			      :lang fn-xml-lang
			      :datatype *xml-string*)
			attributes)))))))
     (dom:attributes property))
    attributes))


(defun get-literals-of-node (node xml-lang)
  "Returns alist of attributes that are treated as literal nodes."
  (let ((fn-xml-lang (get-xml-lang node :old-lang xml-lang))
	(attributes nil))
    (dom:map-node-map
     #'(lambda(attr)
	 (let ((attr-ns (dom:namespace-uri attr))
	       (attr-name (get-node-name attr)))
	   (let ((l-type (get-type-of-node-name attr))
		 (l-value (if (get-ns-attribute node attr-name :ns-uri attr-ns)
			      (get-ns-attribute node attr-name :ns-uri attr-ns)
			      "")))
	     (cond
	       ((string= attr-ns *rdf-ns*)
		(unless (or (string= attr-name "ID")
			    (string= attr-name "about")
			    (string= attr-name "nodeID")
			    (string= attr-name "type"))
		  (push (list :type l-type
			      :value l-value
			      :ID nil
			      :lang fn-xml-lang
			      :datatype *xml-string*)
			attributes)))
	       ((or (string= attr-ns *xml-ns*)
		    (string= attr-ns *xmlns-ns*))
		nil);;do nothing, all xml-attributes are no literals
	       (t
		(unless (and (string= attr-ns *rdf2tm-ns*)
			     (string= attr-name "UUID"))
		  (push (list :type l-type
			      :value l-value
			      :ID nil
			      :lang fn-xml-lang
			      :datatype *xml-string*)
			attributes)))))))
     (dom:attributes node))
    attributes))


(defun get-super-classes-of-node-content (node tm-id xml-base)
  "Returns a list of super-classes and IDs."
  (declare (dom:element node))
  (tm-id-p tm-id "get-super-classes-of-node-content")
  (let ((content (non-isidorus-child-nodes-or-text node :trim t))
	(fn-xml-base (get-xml-base node :old-base xml-base)))
    (when content
      (loop for property across content
	 when (let ((prop-name (get-node-name property))
		    (prop-ns (dom:namespace-uri property)))
		(and (string= prop-name "subClassOf")
		     (string= prop-ns *rdfs-ns*)))
	 collect (let ((prop-xml-base (get-xml-base property
						    :old-base fn-xml-base)))
		   (let ((ID (get-absolute-attribute property tm-id
						     fn-xml-base "ID"))
			 (nodeID (get-ns-attribute property "nodeID"))
			 (resource
			  (get-absolute-attribute property tm-id
						  fn-xml-base "resource"))
			 (UUID (get-ns-attribute property "UUID"
						 :ns-uri *rdf2tm-ns*)))
		     (if (or nodeID resource UUID)
			 (list :topicid (or nodeID resource UUID)
			       :psi resource
			       :ID ID)
			 (let ((refs (get-node-refs
				      (child-nodes-or-text property :trim t)
				      tm-id prop-xml-base)))
			   (list :topicid (getf (first refs) :topicid)
				 :psi (getf (first refs) :psi)
				 :ID ID)))))))))


(defun get-associations-of-node-content (node tm-id xml-base)
  "Returns a list of associations with a type, value and ID member."
  (declare (dom:element node))
  (let ((properties (non-isidorus-child-nodes-or-text node :trim t))
	(fn-xml-base (get-xml-base node :old-base xml-base)))
    (loop for property across properties
       when (let ((prop-name (get-node-name property))
		  (prop-ns (dom:namespace-uri property))
		  (prop-content (child-nodes-or-text property))
		  (resource (get-absolute-attribute property tm-id
						    fn-xml-base "resource"))
		  (nodeID (get-ns-attribute property "nodeID"))
		  (type (get-ns-attribute property "type"))
		  (parseType (get-ns-attribute property "parseType"))
		  (UUID (get-ns-attribute property "UUID"
					  :ns-uri *rdf2tm-ns*)))
	      (and (or resource nodeID type UUID
		       (and parseType
			    (or (string= parseType "Collection")
				(string= parseType "Resource")))
		       (and (> (length prop-content) 0)
			    (not (stringp prop-content)))
		       (> (length (get-literals-of-property property nil)) 0))
		   (not (and (string= prop-name "type")
			     (string= prop-ns *rdf-ns*)))
		   (not (and (string= prop-name "subClassOf")
			     (string= prop-ns *rdfs-ns*)))))
       collect (let ((prop-xml-base (get-xml-base property
						  :old-base fn-xml-base))
		     (content (child-nodes-or-text property :trim t))
		     (parseType (get-ns-attribute property "parseType")))
		 (let ((resource
			(if (and (string= parseType "Collection")
				 (= (length content) 0))
			    *rdf-nil*
			    (get-absolute-attribute property tm-id
						    fn-xml-base "resource")))
		       (nodeID (get-ns-attribute property "nodeID"))
		       (UUID (get-ns-attribute property "UUID"
					       :ns-uri *rdf2tm-ns*))
		       (ID (get-absolute-attribute property tm-id
						   fn-xml-base "ID"))
		       (full-name (get-type-of-node-name property)))
		   (if (or nodeID resource UUID)
		       (list :type full-name
			     :topicid (or resource nodeID UUID)
			     :psi resource
			     :ID ID)
		       (let ((refs (get-node-refs
				    (child-nodes-or-text property :trim t)
				    tm-id prop-xml-base)))
			 (list :type full-name
			       :topicid (getf (first refs) :topicid)
			       :psi (getf (first refs) :psi)
			       :ID ID))))))))


(defun make-recursion-from-node (node tm-id start-revision
				 &key (document-id *document-id*)
				 (xml-base nil) (xml-lang nil))
  "Calls the next function that handles all DOM child elements
   of the passed element as arcs."
  (declare (dom:element node))
  (let ((content (non-isidorus-child-nodes-or-text node :trim t))
	(err-pref "From make-recursion-from-node(): ")
	(fn-xml-base (get-xml-base node :old-base xml-base))
	(fn-xml-lang (get-xml-lang node :old-lang xml-lang)))
    (when (stringp content)
      (error "~aliteral content not allowed here: ~a"
	     err-pref content))
    (loop for arc across content
       collect (import-arc arc tm-id start-revision :document-id document-id
			   :xml-base fn-xml-base :xml-lang fn-xml-lang))))


(defun make-recursion-from-arc (arc tm-id start-revision
				&key (document-id *document-id*)
				(xml-base nil) (xml-lang nil))
  "Calls the next function that handles the arcs content nodes/arcs."
  (declare (dom:element arc))
  (let ((fn-xml-base (get-xml-base arc :old-base xml-base))
	(fn-xml-lang (get-xml-lang arc :old-lang xml-lang))
	(content (non-isidorus-child-nodes-or-text arc))
	(parseType (get-ns-attribute arc "parseType")))
    (let ((datatype (get-absolute-attribute arc tm-id xml-base "datatype"))
	  (type (get-absolute-attribute arc tm-id xml-base "type"))
	  (resource (get-absolute-attribute arc tm-id xml-base "resource"))
	  (nodeID (get-ns-attribute arc "nodeID"))
	  (literals (get-literals-of-property arc xml-lang)))
      (if (and parseType
	       (string= parseType "Collection"))
	  (make-collection arc tm-id start-revision
			   :document-id document-id
			   :xml-base xml-base
			   :xml-lang xml-lang)
	  (if (or datatype resource nodeID
		  (and parseType
		       (string= parseType "Literal"))
		  (and content
		       (stringp content)))
	      nil;; do nothing current elem is a literal node that has been
	         ;; already imported as an occurrence
	      (if (or type literals
		      (and parseType
			   (string= parseType "Resource")))
		  (loop for item across content
		     collect (import-arc item tm-id start-revision
					 :document-id document-id
					 :xml-base fn-xml-base
					 :xml-lang fn-xml-lang))
		  (loop for item across content
		     collect (import-node item tm-id start-revision
					  :document-id document-id
					  :xml-base xml-base
					  :xml-lang xml-lang))))))))


(defun make-isidorus-identifiers (owner-elem start-revision &key (what "itemIdentity"))
  "Returns a list oc created identifier objects that can be
   used directly in make-topic-stub."
  (declare (dom:element owner-elem))
  (declare (string what))
  (when (and (string/= what "itemIdentity")
	     (string/= what "subjectIdentifier")
	     (string/= what "subjectLocator"))
    (error "From make-identifiers(): what must be set to: ~a but is ~a"
	   (list "itemIdentity" "subjectIdentifiers" "subjectLocator")
	   what))
  (let ((content (child-nodes-or-text owner-elem :trim t))
	(class-symbol (cond
			((string= what "itemIdentity")
			 'ItemIdentifierC)
			((string= what "subjectIdentifier")
			 'PersistentIdC)
			((string= what "subjectLocator")
			 'SubjectLocatorC))))
    (unless (stringp content)
      (let ((identifiers
	     (loop for property across content
		when (let ((prop-ns (dom:namespace-uri property))
			   (prop-name (get-node-name property))
			   (prop-content (child-nodes-or-text property :trim t)))
		       (and (string= prop-ns *tm2rdf-ns*)
			    (string= prop-name what)
			    (stringp prop-content)
			    (> (length prop-content) 0)))
		collect (let ((uri (child-nodes-or-text property :trim t)))
			  (make-instance class-symbol 
					 :uri uri
					 :start-revision start-revision))))
	    (identifier-attr
	     (let ((attr (get-ns-attribute owner-elem what :ns-uri *tm2rdf-ns*)))
	       (when attr
		 (list (make-instance class-symbol
				      :uri attr
				      :start-revision start-revision))))))
	(remove-if #'null (append identifiers identifier-attr))))))