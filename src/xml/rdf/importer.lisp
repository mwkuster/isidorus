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
  (rdf-importer rdf-xml-path repository-path :tm-id tm-id)
		:document-id document-id
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
    (let ((rdf-dom
	   (dom:document-element (cxml:parse-file
				  (truename rdf-xml-path)
				  (cxml-dom:make-dom-builder)))))
      (import-dom rdf-dom start-revision :tm-id tm-id :document-id document-id))
    (setf *_n-map* nil)))


(defun init-rdf-module (&optional (revision (get-revision)))
  "Imports the file rdf_core_psis.xtm. core_psis.xtm has to be imported
   before."
  (with-writer-lock
    (with-tm (revision "rdf.xtm" "http://isidorus/rdf2tm_mapping/rdf.xtm")
      (let
	  ((core-dom 
	    (cxml:parse-file *rdf_core_psis.xtm* (cxml-dom:make-dom-builder))))
	(loop for top-elem across 
	     (xpath-child-elems-by-qname (dom:document-element core-dom)
					 *xtm2.0-ns* "topic")
	   do
	     (let
		 ((top
		   (from-topic-elem-to-stub top-elem revision
					    :xtm-id *rdf-core-xtm*)))
	       (add-to-topicmap xml-importer::tm top)))))))


(defun tm-id-p (tm-id fun-name)
  "Checks the validity of the passed tm-id."
  (unless (absolute-uri-p tm-id)
    (error "From ~a(): you must provide a stable identifier (PSI-style) for this TM: ~a!"
	   fun-name tm-id)))


(defun import-dom (rdf-dom start-revision
		   &key (tm-id nil) (document-id *document-id*))
  "Imports the entire dom of a rdf-xml-file."
  (tm-id-p tm-id "import-dom")
  (let ((xml-base (get-xml-base rdf-dom))
	(xml-lang (get-xml-lang rdf-dom))
	(elem-name (get-node-name rdf-dom))
	(elem-ns (dom:namespace-uri rdf-dom)))
    (if (and (string= elem-ns *rdf-ns*)
	     (string= elem-name "RDF"))
	(let ((children (child-nodes-or-text rdf-dom)))
	  (when children
	    (loop for child across children
	       do (import-node child tm-id start-revision :document-id document-id
			       :xml-base xml-base :xml-lang xml-lang))))
	(import-node rdf-dom tm-id start-revision :document-id document-id
		     :xml-base xml-base :xml-lang xml-lang))))


(defun import-node (elem tm-id start-revision &key (document-id *document-id*)
		    (xml-base nil) (xml-lang nil))
  (remove-node-properties-from-*_n-map* elem) ;in case of an failed last call
  (tm-id-p tm-id "import-node")
  (parse-node elem)
  (let ((fn-xml-base (get-xml-base elem :old-base xml-base)))
    (parse-properties-of-node elem)
    (let ((about (get-absolute-attribute elem tm-id xml-base "about"))	   
	  (nodeID (get-ns-attribute elem "nodeID"))
	  (ID (get-absolute-attribute elem tm-id xml-base "ID"))
	  (UUID (get-ns-attribute elem "UUID" :ns-uri *rdf2tm-ns*))
	  (literals (append (get-literals-of-node elem xml-lang)
			    (get-literals-of-node-content elem tm-id
							  xml-base xml-lang)))
	  (associations (get-associations-of-node-content elem tm-id xml-base))
	  (types (remove-if
		  #'null
		  (append (list
			   (unless (string= (get-type-of-node-name elem)
					    (concatenate 'string *rdf-ns*
							 "Description"))
			     (list :topicid (get-type-of-node-name elem)
				   :psi (get-type-of-node-name elem)
				   :ID nil)))
			  (get-types-of-node-content elem tm-id fn-xml-base))))
	  (super-classes
	   (get-super-classes-of-node-content elem tm-id xml-base)))
      (with-tm (start-revision document-id tm-id)
	(elephant:ensure-transaction (:txn-nosync t)
	  (let ((topic-stub
		 (make-topic-stub
		  about ID nodeID UUID start-revision xml-importer::tm
		  :document-id document-id)))
	    (map 'list #'(lambda(literal)
			   (make-occurrence topic-stub literal start-revision
					    tm-id :document-id document-id))
		 literals)
	    (map 'list #'(lambda(assoc)
			   (make-association topic-stub assoc xml-importer::tm
					     start-revision
					     :document-id document-id))
		 associations)
	    (map 'list
		 #'(lambda(type)
		     (let ((type-topic
			    (make-topic-stub (getf type :psi)
					     nil
					     (getf type :topicid)
					     nil start-revision
					     xml-importer::tm
					     :document-id document-id))
			   (ID (getf type :ID)))
		       (make-instance-of-association topic-stub type-topic
						     ID start-revision
						     xml-importer::tm
						     :document-id document-id)))
		 types)
	    (map 'list
		 #'(lambda(class)
		     (let ((class-topic
			    (make-topic-stub (getf class :psi)
					     nil
					     (getf class :topicid)
					     nil start-revision
					     xml-importer::tm
					     :document-id document-id))
			   (ID (getf class :ID)))
		       (make-supertype-subtype-association
			topic-stub class-topic ID start-revision
			xml-importer::tm :document-id document-id)))
		 super-classes)
	    
	    ;TODO: start recursion ...
	    (remove-node-properties-from-*_n-map* elem)))))))


(defun make-supertype-subtype-association (sub-top super-top reifier-id
					   start-revision tm
					   &key (document-id *document-id*))
  "Creates an supertype-subtype association."
  (declare (TopicC sub-top super-top))
  (declare (TopicMapC tm))
  (let ((assoc-type (get-item-by-psi *supertype-subtype-psi*))
	(role-type-1 (get-item-by-psi *supertype-psi*))
	(role-type-2 (get-item-by-psi *subtype-psi*))
	(err-pref "From make-supertype-subtype-association(): "))
    (unless assoc-type
      (error "~athe association type ~a is missing!"
	     err-pref *supertype-subtype-psi*))
    (unless (or role-type-1 role-type-2)
      (error "~aone of the role types ~a ~a is missing!"
	     err-pref *supertype-psi* *subtype-psi*))
    (elephant:ensure-transaction (:txn-nosync t)
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
			 :roles a-roles))))))


(defun make-instance-of-association (instance-top type-top reifier-id
				     start-revision tm
				     &key (document-id *document-id*))
  "Creates and returns an instance-of association."
  (declare (TopicC type-top instance-top))
  (declare (TopicMapC tm))
  (let ((assoc-type
	 (get-item-by-psi *type-instance-psi*))
	(roletype-1
	 (get-item-by-psi *type-psi*))
	(roletype-2
	 (get-item-by-psi *instance-psi*))
	(err-pref "From make-instance-of-association(): "))
    (unless assoc-type
      (error "~athe association type ~a is missing!"
	     err-pref *type-instance-psi*))
    (unless (or roletype-1 roletype-2)
      (error "~aone of the role types ~a ~a is missing!"
	     err-pref *type-psi* *instance-psi*))
    (elephant:ensure-transaction (:txn-nosync t)
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
			 :roles a-roles))))))


(defun make-topic-stub (about ID nodeId UUID start-revision
			tm &key (document-id *document-id*))
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
	  (elephant:ensure-transaction (:txn-nosync t)
	    (let ((psi (when psi-uri
			 (make-instance 'PersistentIdC
					:uri psi-uri
					:start-revision start-revision))))
	      (add-to-topicmap
	       tm
	       (make-construct 'TopicC
			       :topicid topic-id
			       :psis (when psi (list psi))
			       :xtm-id document-id
			       :start-revision start-revision))))))))


(defun make-lang-topic (lang tm-id start-revision tm
			&key (document-id *document-id*))
  "Returns a topic with the topicid tm-id/lang. If no such topic exist
   there will be created one."
  (declare (TopicMapC tm))
  (when (and lang tm-id)
    (tm-id-p tm-id "make-lang-topic")
    (let ((psi-and-topic-id
	   (absolutize-value lang nil tm-id)))
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
    (elephant:ensure-transaction (:txn-nosync t)
      (let ((player-1 (make-topic-stub player-psi nil player-id nil
				       start-revision
				       tm :document-id document-id))
	    (role-type-1 (get-item-by-psi *rdf2tm-object*))
	    (role-type-2 (get-item-by-psi *rdf2tm-subject*))
	    (type-top (make-topic-stub type nil nil nil start-revision
				       tm :document-id document-id)))
	(let ((roles (list (list :instance-of role-type-1
				 :player player-1)
			   (list :instance-of role-type-2
				 :player top))))
	  (when ID
	    (make-reification ID top type-top player-1 start-revision
			      tm :document-id document-id))
	  (add-to-topicmap tm (make-construct 'AssociationC
					      :start-revision start-revision
					      :instance-of type-top
					      :roles roles)))))))
  

(defun make-association-with-nodes (subject-topic object-topic
				    associationtype-topic tm start-revision)
  "Creates an association with two roles that contains the given players."
  (declare (TopicC subject-topic object-topic associationtype-topic))
  (declare (TopicMapC tm))
  (let ((role-type-1 (get-item-by-psi *rdf2tm-subject*))
	(role-type-2 (get-item-by-psi *rdf2tm-object*)))
    (let ((roles (list (list :instance-of role-type-1
			     :player subject-topic)
		       (list :instance-of role-type-2
			     :player object-topic))))
      (elephant:ensure-transaction (:txn-nosync t)
	(add-to-topicmap tm (make-construct 'AssociationC
					    :start-revision start-revision
					    :instance-of associationtype-topic
					    :roles roles))))))


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
    (elephant:ensure-transaction (:txn-nosync t)
      (make-instance-of-association reifier statement nil start-revision tm
				    :document-id document-id)
      (make-association-with-nodes reifier subject subject-arc tm
				   start-revision)
      (make-association-with-nodes reifier predicate predicate-arc
				   tm start-revision)
      (if (typep object 'TopicC)
	  (make-association-with-nodes reifier object object-arc
				       tm start-revision)
	  (make-construct 'OccurrenceC
			  :start-revision start-revision
			  :topic reifier
			  :themes (themes object)
			  :instance-of (instance-of object)
			  :charvalue (charvalue object)
			  :datatype (datatype object))))))


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
      (elephant:ensure-transaction (:txn-nosync t)
	(let ((type-top (make-topic-stub type nil nil nil start-revision
					 xml-importer::tm
					 :document-id document-id))
	      (lang-top (make-lang-topic lang tm-id start-revision
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
	      (make-reification ID top type-top occurrence start-revision
				xml-importer::tm :document-id document-id))
	    occurrence))))))
	    

(defun get-literals-of-node-content (node tm-id xml-base xml-lang)
  "Returns a list of literals that is produced of a node's content."
  (declare (dom:element node))
  (tm-id-p tm-id "get-literals-of-content")
  (let ((properties (child-nodes-or-text node))
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
				(string= parseType "Literal")
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
	   (when (child-nodes-or-text node)
	     (loop for child across (child-nodes-or-text node)
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
					(child-nodes-or-text child)
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
  (let ((content (child-nodes-or-text node))
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
				      (child-nodes-or-text property)
				      tm-id prop-xml-base)))
			   (list :topicid (getf (first refs) :topicid)
				 :psi (getf (first refs) :psi)
				 :ID ID)))))))))


(defun get-associations-of-node-content (node tm-id xml-base)
  "Returns a list of associations with a type, value and ID member."
  (declare (dom:element node))
  (let ((properties (child-nodes-or-text node))
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
						  :old-base fn-xml-base)))
		 (let ((resource
			(get-absolute-attribute property tm-id
						fn-xml-base "resource"))
		       (nodeID (get-ns-attribute property "nodeID"))
		       (UUID (get-ns-attribute property "UUID"
					       :ns-uri *rdf2tm-ns*))
		       (ID (get-absolute-attribute property tm-id
						   fn-xml-base "ID"))
		       (full-name (get-type-of-node-name property)))
		   (if (or nodeID resource UUID)
		       (list :type full-name
			     :topicid (or nodeID resource UUID)
			     :psi resource
			     :ID ID)
		       (let ((refs (get-node-refs
				    (child-nodes-or-text property)
				    tm-id prop-xml-base)))
			 (list :type full-name
			       :topicid (getf (first refs) :topicid)
			       :psi (getf (first refs) :psi)
			       :ID ID))))))))