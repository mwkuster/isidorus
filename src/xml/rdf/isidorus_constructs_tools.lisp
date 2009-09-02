;;+-----------------------------------------------------------------------------
;;+  Isidorus
;;+  (c) 2008-2009 Marc Kuester, Christoph Ludwig, Lukas Giessmann
;;+
;;+  Isidorus is freely distributable under the LGPL license.
;;+  You can find a detailed description in trunk/docs/LGPL-LICENSE.txt.
;;+-----------------------------------------------------------------------------
(in-package :rdf-importer)


(defun get-type-psis (elem tm-id
		      &key (parent-xml-base nil))
  "Returns a list of type-uris of the passed node."
  (let ((types (get-types-of-node elem tm-id
				  :parent-xml-base parent-xml-base)))
    (remove-if #'null
	       (map 'list #'(lambda(x)
			      (getf x :psi))
		    types))))


(defun get-all-type-psis-of-id (nodeID tm-id document)
  "Returns a list of type-uris for resources identified by the given
   nodeID by analysing the complete XML-DOM."
  (let ((root (elt (dom:child-nodes document) 0)))
    (remove-duplicates
     (remove-if #'null
		(if (and (string= (dom:namespace-uri root) *rdf-ns*)
			 (string= (get-node-name root)"RDF"))
		    (loop for node across (child-nodes-or-text root)
		       append (get-all-type-psis-across-dom
			       root tm-id :resource-id nodeID))
		    (get-all-type-psis-across-dom
		     root tm-id :resource-id nodeID)))
     :test #'string=)))


(defun get-all-type-psis (elem tm-id &key (parent-xml-base nil))
  "Returns a list of type-uris for the element by analysing the complete
   XML-DOM."
  (let ((xml-base (get-xml-base elem :old-base parent-xml-base)))
    (let ((root (elt (dom:child-nodes (dom:owner-document elem)) 0))
	  (nodeID (get-ns-attribute elem "nodeID"))
	  (about (get-absolute-attribute elem tm-id xml-base "about")))
      (remove-duplicates
       (remove-if #'null
		  (if (or nodeID about)
		      (if (and (string= (dom:namespace-uri root) *rdf-ns*)
			       (string= (get-node-name root) "RDF"))
			  (loop for node across (child-nodes-or-text root)
			     append (get-all-type-psis-across-dom
				     root tm-id :resource-uri about
				     :resource-id nodeID))
			  (get-all-type-psis-across-dom
			   root tm-id :resource-uri about
			   :resource-id nodeID))
		      (get-type-psis elem tm-id :parent-xml-base parent-xml-base)))
       :test #'string=))))


(defun get-all-type-psis-across-dom (elem tm-id &key (parent-xml-base nil)
				     (resource-uri nil) (resource-id nil)
				     (types nil))
  "Returns a list of type PSI strings collected over the complete XML-DOM
   corresponding to the passed id's or uri."
  (when (or resource-uri resource-id)
    (let ((xml-base (get-xml-base elem :old-base parent-xml-base)))
      (let ((datatype (when (get-ns-attribute elem "datatype")
			t))
	    (parseType (when (get-ns-attribute elem "parseType")
			 (string= (get-ns-attribute elem "parseType")
				  "Literal"))))
	(if (or datatype parseType)
	    types
	    (let ((nodeID (get-ns-attribute elem "nodeID"))
		  (about (get-absolute-attribute elem tm-id xml-base "about")))
	      (let ((fn-types
		     (append types
			     (when (or (and about resource-uri
					    (string= about resource-uri))
				       (and nodeID resource-id
					    (string= nodeID resource-id)))
			       (get-type-psis elem tm-id
					      :parent-xml-base xml-base))))
		    (content (child-nodes-or-text elem :trim t)))
		(if (or (stringp content)
			(not content))
		    fn-types
		    (loop for child-node across content
		       append (get-all-type-psis-across-dom
			       child-node tm-id :parent-xml-base xml-base
			       :resource-uri resource-uri
			       :resource-id resource-id
			       :types fn-types))))))))))


(defun type-p (elem type-uri tm-id &key (parent-xml-base nil))
  "Returns t if the type-uri is a type of elem."
  (declare (string tm-id type-uri))
  (declare (dom:element elem))
  (tm-id-p tm-id "type-p")
  (find type-uri (get-all-type-psis elem tm-id
				    :parent-xml-base parent-xml-base)
	:test #'string=))


(defun type-of-id-p (node-id type-uri tm-id document)
  "Returns t if type-uri is a type of the passed node-id."
  (declare (string node-id type-uri tm-id))
  (declare (dom:document document))
  (tm-id-p tm-id "type-of-ndoe-id-p")
  (find type-uri (get-all-type-psis-of-id node-id tm-id document)
	:test #'string=))


(defun non-isidorus-type-p (elem tm-id &key (parent-xml-base nil)
			    (ignore-topic nil))
  "Returns t if the passed element is not of an isidorus' type.
   The environmental property is not analysed by this function!"
  (declare (dom:element elem))
  (declare (string tm-id))
  (let ((nodeID (get-ns-attribute elem "nodeID"))
	(document (dom:owner-document elem))
	(types 
	 (let ((b-types
		(list 
		 *tm2rdf-name-type-uri* *tm2rdf-variant-type-uri*
		 *tm2rdf-occurrence-type-uri* *tm2rdf-association-type-uri*
		 *tm2rdf-role-type-uri*))
	       (a-types (list *tm2rdf-topic-type-uri*)))
	   (if ignore-topic
	       b-types
	       (append a-types b-types)))))
    (if nodeID
	(not (loop for type in types
		when (type-of-id-p nodeId type tm-id document)
		return t))
	(not (loop for type in types
		when (type-p elem type tm-id 
			     :parent-xml-base parent-xml-base)
		return t)))))


(defun isidorus-type-p (property-elem-or-node-elem tm-id what
			&key(parent-xml-base nil))
  "Returns t if the node elem is of the type isidorus:<Type> and is
   contained in a porperty isidorus:<type>."
  (declare (dom:element property-elem-or-node-elem))
  (declare (symbol what))
  (tm-id-p tm-id "isidorus-type-p")
  (let ((xml-base (get-xml-base property-elem-or-node-elem
				:old-base parent-xml-base))
	(type-and-property (cond
			     ((eql what 'name)
			      (list :type *tm2rdf-name-type-uri*
				    :property *tm2rdf-name-property*))
			     ((eql what 'variant)
			      (list :type *tm2rdf-variant-type-uri*
				    :property *tm2rdf-variant-property*))
			     ((eql what 'occurrence)
			      (list :type *tm2rdf-occurrence-type-uri*
				    :property *tm2rdf-occurrence-property*))
			     ((eql what 'role)
			      (list :type *tm2rdf-role-type-uri*
				    :property *tm2rdf-role-property*))
			     ((eql what 'topic)
			      (list :type *tm2rdf-topic-type-uri*))
			     ((eql what 'association)
			      (list :type 
				    *tm2rdf-association-type-uri*)))))
    (when type-and-property
      (let ((type (getf type-and-property :type))
	    (property (getf type-and-property :property))
	    (nodeID (get-ns-attribute property-elem-or-node-elem "nodeID"))
	    (document (dom:owner-document property-elem-or-node-elem))
	    (elem-uri (concatenate-uri
		       (dom:namespace-uri
			property-elem-or-node-elem)
		       (get-node-name property-elem-or-node-elem))))
	(if (or (string= type *tm2rdf-topic-type-uri*)
		(string= type *tm2rdf-association-type-uri*)
		(let ((parseType (get-ns-attribute property-elem-or-node-elem
						   "parseType")))
		  (and parseType
		       (string= parseType "Resource")))
		(get-ns-attribute property-elem-or-node-elem "type")
		(get-ns-attribute property-elem-or-node-elem "value"
				  :ns-uri *tm2rdf-ns*)
		(get-ns-attribute property-elem-or-node-elem "itemIdentity"
				  :ns-uri *tm2rdf-ns*))
	    (type-p property-elem-or-node-elem type tm-id
		    :parent-xml-base parent-xml-base)
	    (when (string= elem-uri property)
	      (if nodeID
		  (type-of-id-p nodeId type tm-id document)
		  (let ((content (child-nodes-or-text  property-elem-or-node-elem
						       :trim t)))
		    (when (and (= (length content) 1)
			       (not (stringp content)))
		      (type-p (elt content 0) type tm-id
			      :parent-xml-base xml-base))))))))))


(defun non-isidorus-child-nodes-or-text (elem &key (trim nil))
  "Returns a list of node elements that are no isidorus properties, e.g.
   isidorus:name, string-content or nil."
  (let ((content (child-nodes-or-text elem :trim trim)))
    (if (or (not content)
	    (stringp content))
	content
	(remove-if #'(lambda(x)
		       (let ((x-uri (if (dom:namespace-uri x)
					(concatenate-uri (dom:namespace-uri x)
							 (get-node-name x))
					(get-node-name x))))
			 (or (string= x-uri *tm2rdf-name-property*)
			     (string= x-uri *tm2rdf-variant-property*)
			     (string= x-uri *tm2rdf-occurrence-property*)
			     (string= x-uri *tm2rdf-role-property*)
			     (string= x-uri *tm2rdf-subjectIdentifier-property*)
			     (string= x-uri *tm2rdf-itemIdentity-property*)
			     (string= x-uri *tm2rdf-value-property*)
			     (string= x-uri *tm2rdf-scope-property*)
			     (string= x-uri *tm2rdf-nametype-property*)
			     (string= x-uri *tm2rdf-varianttype-property*)
			     (string= x-uri *tm2rdf-associationtype-property*)
			     (string= x-uri *tm2rdf-occurrencetype-property*)
			     (string= x-uri *tm2rdf-roletype-property*)
			     (string= x-uri *tm2rdf-subjectLocator-property*))))
		   content))))


(defun get-all-isidorus-nodes-by-id (node-id current-node type-uri
					&key (parent-xml-base nil)
				     (collected-nodes nil))
  "Returns a list of all nodes that own the given nodeID and are of
   type type-uri, rdf:Description or when the rdf:parseType is set to
   Resource or the isidorus:value attribute is set."
  (declare (dom:element current-node))
  (declare (string node-id))
  (let ((datatype (when (get-ns-attribute current-node "datatype")
		    t))
	(parseType (let ((attr (get-ns-attribute current-node "parseType")))
		     (when (and attr
				(string= attr "Literal"))
		       t)))
	(content (child-nodes-or-text current-node :trim t))
	(xml-base (get-xml-base current-node :old-base parent-xml-base))
	(nodeID (get-ns-attribute current-node "nodeID"))
	(node-uri-p (let ((node-uri
			   (concatenate-uri (dom:namespace-uri current-node)
					    (get-node-name current-node)))
			  (description (concatenate 'string *rdf-ns* 
						    "Description")))
		      (or (string= node-uri (if type-uri type-uri ""))
			  (string= node-uri description)
			  (get-ns-attribute current-node "type")
			  (get-ns-attribute current-node "value" 
					    :ns-uri *tm2rdf-ns*)
			  (get-ns-attribute current-node "itemIdentity"
					    :ns-uri *tm2rdf-ns*)
			  (let ((parseType (get-ns-attribute current-node 
							     "parseType")))
			    (when parseType
			      (string= parseType "Resource")))))))
    (remove-duplicates
     (remove-if 
      #'null
      (if (or datatype parseType (stringp content) (not content))
	  (if (and (string= nodeID node-id) node-uri-p)
	      (append (list (list :elem current-node
				  :xml-base xml-base))
		      collected-nodes)
	      collected-nodes)
	  (if (and (string= nodeID node-id) node-uri-p)
	      (loop for item across content
		 append (get-all-isidorus-nodes-by-id
			 node-id item type-uri
			 :collected-nodes (append
					   (list (list :elem current-node
						       :xml-base xml-base))
					   collected-nodes)
			 :parent-xml-base xml-base))
	      (loop for item across content
		 append (get-all-isidorus-nodes-by-id 
			 node-id item type-uri 
			 :collected-nodes collected-nodes
			 :parent-xml-base xml-base)))))
     :test #'(lambda(x y)
	       (eql (getf x :elem) (getf y :elem))))))


(defun filter-isidorus-literals (literals)
  "Removes all literals that are known isidorus properties which
   are able to contain literal data."
  (remove-if #'(lambda(x)
		 (or (string= (getf x :type)
			      *tm2rdf-subjectIdentifier-property*)
		     (string= (getf x :type)
			      *tm2rdf-itemIdentity-property*)
		     (string= (getf x :type)
			      *tm2rdf-subjectLocator-property*)))
	     literals))


(defun self-or-child-node (property-node type-uri &key (xml-base))
  "Returns either the passed node or the child-node when it is
   rdf:Description."
  (declare (dom:element property-node))
  (let ((content (child-nodes-or-text property-node :trim t)))
    (if (and (= (length content) 1)
	     (or (and (string= (dom:namespace-uri (elt content 0)) *rdf-ns*)
		      (string= (get-node-name (elt content 0)) "Description"))
		 (string= (concatenate-uri (dom:namespace-uri (elt content 0))
					   (get-node-name (elt content 0)))
			  type-uri)))
	(list :elem (elt content 0)
	      :xml-base (get-xml-base (elt content 0) :old-base xml-base))
	(list :elem property-node
	      :xml-base xml-base))))