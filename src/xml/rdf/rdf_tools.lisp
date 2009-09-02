;;+-----------------------------------------------------------------------------
;;+  Isidorus
;;+  (c) 2008-2009 Marc Kuester, Christoph Ludwig, Lukas Giessmann
;;+
;;+  Isidorus is freely distributable under the LGPL license.
;;+  You can find a detailed description in trunk/docs/LGPL-LICENSE.txt.
;;+-----------------------------------------------------------------------------

(defpackage :rdf-importer
  (:use :cl :cxml :elephant :datamodel :isidorus-threading :datamodel)
  (:import-from :constants
		*rdf-ns*
		*rdfs-ns*
		*xml-ns*
		*xmlns-ns*
		*xml-string*
		*rdf2tm-ns*
		*xtm2.0-ns*
		*type-instance-psi*
		*type-psi*
		*instance-psi*
		*rdf-statement*
		*rdf-object*
		*rdf-subject*
		*rdf-predicate*
		*rdf2tm-object*
		*rdf2tm-subject*
		*supertype-psi*
		*subtype-psi*
		*supertype-subtype-psi*
		*rdf-nil*
		*rdf-first*
		*rdf-rest*
		*rdf2tm-scope-prefix*
		*tm2rdf-topic-type-uri*
		*tm2rdf-name-type-uri*
		*tm2rdf-name-property*
		*tm2rdf-variant-type-uri*
		*tm2rdf-variant-property*
		*tm2rdf-occurrence-type-uri*
		*tm2rdf-occurrence-property*
		*tm2rdf-role-type-uri*
		*tm2rdf-role-property*
		*tm2rdf-association-type-uri*
		*tm2rdf-association-property*
		*tm2rdf-subjectIdentifier-property*
		*tm2rdf-itemIdentity-property*
		*tm2rdf-subjectLocator-property*
		*tm2rdf-ns*
		*tm2rdf-value-property*
		*tm2rdf-nametype-property*
		*tm2rdf-scope-property*
		*tm2rdf-varianttype-property*
		*tm2rdf-occurrencetype-property*
		*tm2rdf-roletype-property*
		*tm2rdf-associationtype-property*)
  (:import-from :xml-constants
		*rdf_core_psis.xtm*
		*core_psis.xtm*)
  (:import-from :xml-tools
                get-attribute
                xpath-fn-string
                xpath-child-elems-by-qname
                xpath-single-child-elem-by-qname
                xpath-select-location-path
                xpath-select-single-location-path
		get-ns-attribute
		clear-child-nodes
		has-qname
		absolute-uri-p
		get-node-name
		child-nodes-or-text
		get-xml-lang
		get-xml-base
		absolutize-value
		absolutize-id
		concatenate-uri
		push-string
		node-to-string)
  (:import-from :xml-importer
		get-uuid
		get-store-spec
		with-tm
		from-topic-elem-to-stub)
  (:import-from :isidorus-threading
		with-reader-lock
		with-writer-lock)
  (:import-from :exceptions
                missing-reference-error
                duplicate-identifier-error)
  (:export :setup-rdf-module 
	   :rdf-importer
	   :init-rdf-module
	   :*rdf-core-xtm*))

(in-package :rdf-importer)

(defvar *rdf-types* (list "Description" "List" "Alt" "Bag" "Seq"
			  "Statement" "Property" "XMLLiteral" "nil"))

(defvar *rdf-properties* (list "type" "first" "rest" "subject" "predicate"
			       "object" "li" "first" "rest"))

(defvar *rdfs-types* (list "Resource" "Literal" "Class" "Datatype"
			   "Container" "ContainerMembershipProperty"))

(defvar *rdfs-properties* (list "subClassOf" "subPropertyOf" "domain"
				"range" "range" "label" "comment"
				"member" "seeAlso" "isDefinedBy"))

(defvar *rdf-core-xtm* "rdf_core.xtm")

(defvar *_n-map* nil)


(defun _n-p (node)
  "Returns t if the given value is of the form _[0-9]+"
  (let ((node-name (get-node-name node)))
    (when (and node-name
	       (> (length node-name) 0)
	       (eql (elt node-name 0) #\_))
      (let ((rest
	     (subseq node-name 1 (length node-name))))
	(declare (string node-name))
	(handler-case (let ((int
			     (parse-integer rest)))
			int)
	  (condition () nil))))))



(defun find-_n-name-of-property (property)
  "Returns the properties name of the form rdf:_n or nil."
  (let ((owner
	 (find-if
	  #'(lambda(x)
	      (find-if
	       #'(lambda(y)
		   (eql (getf y :elem) property))
	       (getf x :props)))
	  *_n-map*)))
    (let ((elem (find-if #'(lambda(x)
			     (eql (getf x :elem) property))
			 (getf owner :props))))
      (when elem
	(getf elem :name)))))



(defun find-_n-name (owner-identifier property)
  "Returns a name of the form rdf:_n of the property element
   when it owns the tagname rdf:li and exists in the *_n-map* list.
   Otherwise the return value is nil."
  (let ((owner (find-if #'(lambda(x)
			    (string= (getf x :owner) owner-identifier))
			*_n-map*)))
   (when owner
     (let ((prop (find-if #'(lambda(x)
			      (eql (getf x :elem) property))
			  (getf owner :props))))
       (getf prop :name)))))


(defun set-_n-name (owner-identifier property)
  "Sets a new name of the form _n for the passed property element and
   adds it to the list *_n-map*. If the property already exists in the
   *_n-map* list, there won't be created a new entry but returned the
   stored value name."
  (let ((name (find-_n-name owner-identifier property)))
    (if name
	name
	(let ((owner (find-if #'(lambda(x)
				  (string= (getf x :owner) owner-identifier))
			      *_n-map*)))
	  (if owner
	      (let ((new-name
		     (concatenate
		      'string *rdf-ns* "_"
		      (write-to-string (+ (length (getf owner :props)) 1)))))
		(push (list :elem property
			    :name new-name)
		      (getf owner :props))
		new-name)
	      (progn
		(push 
		 (list :owner owner-identifier
		       :props (list
			       (list :elem property
				     :name (concatenate 'string *rdf-ns* "_1"))))
		 *_n-map*)
		"_1"))))))


(defun get-type-of-node-name (node)
  (let ((map-item (find-_n-name-of-property node)))
    (if map-item
	map-item
	(let ((node-name (get-node-name node))
	      (node-ns (dom:namespace-uri node)))
	  (concatenate-uri node-ns node-name)))))


(defun parse-node-name (node)
  "Parses the given node's name to the known rdf/rdfs nodes and arcs.
   If the given name es equal to a property an error is thrown otherwise
   there is displayed a warning when the rdf ord rdfs namespace is used."
  (declare (dom:element node))
  (let ((node-name (get-node-name node))
	(node-ns (dom:namespace-uri node))
	(err-pref "From parse-node-name(): "))
    (when (string= node-ns *rdf-ns*)
      (when (find node-name *rdf-properties* :test #'string=)
	(error "~ardf:~a is a property and not allowed here!"
	       err-pref node-name))
      (when (string= node-name "RDF")
	(error "~ardf:RDF not allowed here!"
	       err-pref))
      (unless (find node-name *rdf-types* :test #'string=)
	(format t "~aWarning: ~a is not a known RDF type!~%"
		err-pref node-name)))
    (when (string= node-ns *rdfs-ns*)
      (when (find node-name *rdfs-properties* :test #'string=)
	(error "~ardfs:~a is a property and not allowed here!"
	       err-pref node-name))
      (unless (find node-name *rdfs-types* :test #'string=)
	(format t "~aWarning: rdfs:~a is not a known rdfs:type!~%"
		err-pref node-name))))
  t)


(defun parse-node(node)
  "Parses a node that represents a rdf-resource."
  (declare (dom:element node))
  (parse-node-name node)
  (let ((ID  (get-ns-attribute node "ID"))
	(nodeID (get-ns-attribute node "nodeID"))
	(about (get-ns-attribute node "about"))
	(err-pref "From parse-node(): ")
	(resource (get-ns-attribute node "resource"))
	(datatype (get-ns-attribute node "datatype"))
	(parseType (get-ns-attribute node "parseType"))
	(class (get-ns-attribute node "Class" :ns-uri *rdfs-ns*))
	(subClassOf (get-ns-attribute node "subClassOf" :ns-uri *rdfs-ns*)))
    (when (and about nodeID)
      (error "~ardf:about and rdf:nodeID are not allowed in parallel use: (~a) (~a)!"
	     err-pref about nodeID))
    (when (and ID
	       (or about nodeID))
      (error "~awhen rdf:ID is set the attributes rdf:~a is not allowed: ~a!"
	     err-pref (if about "about" "nodeID") (or about nodeID)))
    (unless (or ID nodeID about (dom:has-attribute-ns node *rdf2tm-ns* "UUID"))
      (dom:set-attribute-ns node *rdf2tm-ns* "UUID" (get-uuid)))
    (handler-case (let ((content (child-nodes-or-text node :trim t)))
		    (when (stringp content)
		      (error "text-content not allowed here!")))
      (condition (err) (error "~a~a" err-pref err)))
    (when (or resource datatype parseType class subClassOf)
      (error "~a~a is not allowed here (~a)!"
	     err-pref (cond
			(resource (concatenate 'string "resource("
					       resource ")"))
			(datatype (concatenate 'string "datatype("
					       datatype ")"))
			(parseType (concatenate 'string "parseType("
						parseType ")"))
			(class (concatenate 'string "Class(" class ")"))
			(subClassOf (concatenate 'string "subClassOf("
						 subClassOf ")")))
	     (dom:node-name node)))
    (dolist (item *rdf-types*)
      (when (get-ns-attribute node item)
	(error "~ardf:~a is a type and not allowed here!"
	       err-pref item)))
    (dolist (item *rdfs-types*)
      (when (get-ns-attribute node item :ns-uri *rdfs-ns*)
	(error "~ardfs:~a is a type and not allowed here!"
	       err-pref item))))
  t)


(defun get-node-refs (nodes tm-id xml-base)
  "Returns a list of node references that can be used as topic IDs."
  (when (and nodes
	     (> (length nodes) 0))
    (loop for node across nodes
       collect (let ((fn-xml-base (get-xml-base node :old-base xml-base)))
		 (parse-node node)
		 (let ((ID (when (get-ns-attribute node "ID")
			     (absolutize-id (get-ns-attribute node "ID")
					    fn-xml-base tm-id)))
		       (nodeID (get-ns-attribute node "nodeID"))
		       (about (when (get-ns-attribute node "about")
				(absolutize-value
				 (get-ns-attribute node "about")
				 fn-xml-base tm-id)))
		       (UUID (get-ns-attribute node "UUID" :ns-uri *rdf2tm-ns*)))
		   (list :topicid (or ID about nodeID UUID)
			 :psi (or ID about)))))))


(defun get-ref-of-property (property-elem tm-id xml-base)
  "Returns a plist of the form (:topicid <string> :psi <string>).
   That contains the property's value."
  (declare (dom:element property-elem))
  (declare (string tm-id))
  (let ((nodeId (get-ns-attribute property-elem "nodeID"))
	(resource (get-ns-attribute property-elem "resource"))
	(content (let ((node-refs
			(get-node-refs (child-nodes-or-text property-elem)
				       tm-id xml-base)))
		   (when node-refs
		     (first node-refs)))))
    (cond
      (nodeID
       (list :topicid nodeID
	     :psi nil))
      (resource
       (list :topicid resource
	     :psi resource))
      (content
       content))))


(defun parse-property-name (property owner-identifier)
  "Parses the given property's name to the known rdf/rdfs nodes and arcs.
   If the given name es equal to an node an error is thrown otherwise
   there is displayed a warning when the rdf ord rdfs namespace is used."
  (declare (dom:element property))
  (let ((property-name (get-node-name property))
	(property-ns (dom:namespace-uri property))
	(err-pref "From parse-property-name(): "))
    (when (string= property-ns *rdf-ns*)
      (when (find property-name *rdf-types* :test #'string=)
	(error "~ardf:~a is a node and not allowed here!"
	       err-pref property-name))
      (when (string= property-name "RDF")
	(error "~ardf:RDF not allowed here!"
	       err-pref))
      (unless (or (find property-name *rdf-properties* :test #'string=)
		  (_n-p property))
	(format t "~aWarning: rdf:~a is not a known RDF property!~%"
		err-pref property-name)))
    (when (string= property-ns *rdfs-ns*)
      (when (find property-name *rdfs-types* :test #'string=)
	(error "~ardfs:~a is a type and not allowed here!"
	       err-pref property-name))
      (unless (find property-name *rdfs-properties* :test #'string=)
	(format t "~aWarning: rdfs:~a is not a known rdfs:type!~%"
		err-pref property-name)))
    (when (and (string= property-ns *rdf-ns*)
	       (string= property-name "li"))
      (set-_n-name owner-identifier property)))
  t)


(defun parse-property (property owner-identifier)
  "Parses a property that represents a rdf-arc."
  (declare (dom:element property))
  (let ((err-pref "From parse-property(): ")
	(node-name (get-node-name property))
	(node-ns (dom:namespace-uri property))
	(nodeID (get-ns-attribute property "nodeID"))
	(resource (get-ns-attribute property "resource"))
	(datatype (get-ns-attribute property "datatype"))
	(type (get-ns-attribute property "type"))
	(parseType (get-ns-attribute property "parseType"))
	(about (get-ns-attribute property "about"))
	(subClassOf (get-ns-attribute property "subClassOf" :ns-uri *rdfs-ns*))
	(literals (get-literals-of-property property nil))
	(content (child-nodes-or-text property :trim t)))
    (parse-property-name property owner-identifier)
    (when (and parseType
	       (or nodeID resource datatype type literals))
      (error "~awhen rdf:parseType is set the attributes: ~a => ~a are not allowed!"
	     err-pref
	     (append (list (cond (nodeID "rdf:nodeID")
				 (resource "rdf:resource")
				 (datatype "rdf:datatype")
				 (type "rdf:type")))
		     (map 'list #'(lambda(x)(getf x :type)) literals))
	     (append (list (or nodeID resource datatype type))
		     (map 'list #'(lambda(x)(getf x :value)) literals))))
    (when (and parseType
	       (not (or (string= parseType "Resource")
			(string= parseType "Literal")
			(string= parseType "Collection"))))
      (error "~aunknown rdf:parseType: ~a"
	     err-pref parseType))
    (when (and parseType
	       (or (string= parseType "Resource")
		   (string= parseType "Collection")))
      (unless (dom:has-attribute-ns property *rdf2tm-ns* "UUID")
	(dom:set-attribute-ns property *rdf2tm-ns* "UUID" (get-uuid))))
    (when (and parseType (string= parseType "Resource") (stringp content))
      (error "~ardf:parseType is set to 'Resource' expecting xml content: ~a!"
	     err-pref content))
    (when (and parseType
	       (string= parseType "Collection")
	       (stringp content))
      (error "~ardf:parseType is set to 'Collection' expecting resource content: ~a"
	     err-pref content))
    (when (and nodeID resource)
      (error "~aondly one of rdf:nodeID and rdf:resource is allowed: (~a) (~a)!"
	     err-pref nodeID resource))
    (when (and (or nodeID resource type literals)
	       datatype)
      (error "~aonly one of ~a and rdf:datatype (~a) is allowed!"
	     err-pref
	     (cond
	       (nodeID (concatenate 'string "rdf:nodeID (" nodeID ")"))
	       (resource (concatenate 'string "rdf:resource (" resource ")"))
	       (type (concatenate 'string "rdf:type (" type ")"))
	       (literals literals))
	     datatype))
    (when (and (or nodeID resource)
	       (> (length content) 0))
     (error "~awhen ~a is set no content is allowed: ~a!"
	     err-pref
	     (cond
	       (nodeID (concatenate 'string "rdf:nodeID (" nodeID ")"))
	       (resource (concatenate 'string "rdf:resource (" resource ")")))
	     content))
    (when (and type
	       (stringp content)
	       (> (length content) 0))
      (error "~awhen rdf:type is set no literal content is allowed: ~a!"
	     err-pref content))
    (when (and (or type
		   (and (string= node-name "type")
			(string= node-ns *rdf-ns*))
		   (> (length literals) 0))
	       (not (or nodeID resource))
	       (not content))
      (unless (dom:has-attribute-ns property *rdf2tm-ns* "UUID")
	(dom:set-attribute-ns property *rdf2tm-ns* "UUID" (get-uuid))))
    (when (or about subClassOf)
      (error "~a~a not allowed here!"
	     err-pref
	     (if about
		 (concatenate 'string "rdf:about (" about ")")
		 (concatenate 'string "rdfs:subClassOf (" subClassOf ")"))))
    (when (and (string= node-name "subClassOf")
	       (string= node-ns *rdfs-ns*)
	       (not (or nodeID resource content)))
      (unless (dom:has-attribute-ns property *rdf2tm-ns* "UUID")
	(dom:set-attribute-ns property *rdf2tm-ns* "UUID" (get-uuid))))
    (when (and (or (and (string= node-name "type")
			(string= node-ns *rdf-ns*))
		   (and (string= node-name "subClassOf")
			(string= node-ns *rdfs-ns*)))
	       (and (> (length content) 0)
		    (stringp content)))
      (error "~awhen property is ~a literal content is not allowed: ~a!"
	     err-pref (if (string= node-name "type")
			  "rdf:type"
			  "rdfs:subClassOf")
	     content))
    (dolist (item *rdf-types*)
      (when (get-ns-attribute property item)
	(error "~ardf:~a is a type and not allowed here!"
	       err-pref item)))
    (dolist (item *rdfs-types*)
      (when (get-ns-attribute property item :ns-uri *rdfs-ns*)
	(error "~ardfs:~a is a type and not allowed here!"
	       err-pref item))))
  t)


(defun parse-properties-of-node (node owner-identifier)
  "Parses all node's properties by calling the parse-propery
   function and sets all rdf:li properties as a tupple to the
   *_n-map* list."
  (let ((child-nodes (child-nodes-or-text node :trim t)))
    (when (get-ns-attribute node "li")
      (dom:map-node-map
       #'(lambda(attr)
	   (when (and (string= (get-node-name attr) "li")
		      (string= (dom:namespace-uri attr) *rdf-ns*))
	     (set-_n-name owner-identifier attr)))
	     (dom:attributes node)))
    (when child-nodes
      (loop for property across child-nodes
	 do (parse-property property owner-identifier))))
  t)


(defun get-absolute-attribute (elem tm-id xml-base attr-name
			       &key (ns-uri *rdf-ns*))
  "Returns an absolute 'attribute' or nil."
  (declare (dom:element elem))
  (declare (string attr-name))
  (tm-id-p tm-id "get-ID")
  (let ((attr (get-ns-attribute elem attr-name :ns-uri ns-uri))
	(fn-xml-base (get-xml-base elem :old-base xml-base)))
    (when attr
      (if (and (string= ns-uri *rdf-ns*)
	       (string= attr-name "ID"))
	  (absolutize-id attr fn-xml-base tm-id)
	  (absolutize-value attr fn-xml-base tm-id)))))


(defun get-datatype (elem tm-id xml-base)
  "Returns a datatype value. The default is xml:string."
  (let ((fn-xml-base (get-xml-base elem :old-base xml-base)))
    (let ((datatype
	   (get-absolute-attribute elem tm-id fn-xml-base "datatype")))
      (if datatype
	  datatype
	  *xml-string*))))


(defun tm-id-p (tm-id fun-name)
  "Checks the validity of the passed tm-id."
  (unless (absolute-uri-p tm-id)
    (error "From ~a(): you must provide a stable identifier (PSI-style) for this TM: ~a!"
	   fun-name tm-id)))


(defun get-types-of-node (elem tm-id &key (parent-xml-base nil))
  "Returns a plist of all node's types of the form
   (:topicid <string> :psi <string> :ID <string>)."
  (let ((xml-base (get-xml-base elem :old-base parent-xml-base)))
    (remove-if
     #'null
     (append (unless (string= (get-type-of-node-name elem)
			      (concatenate 'string *rdf-ns*
					   "Description"))
	       (list 
		(list :topicid (get-type-of-node-name elem)
		      :psi (get-type-of-node-name elem)
		      :ID nil)))
	     (get-types-of-node-content elem tm-id xml-base)))))


(defun get-types-of-property (elem tm-id &key (parent-xml-base nil))
  "Returns a plist of all property's types of the form
   (:topicid <string> :psi <string> :ID <string>)."
  (let ((xml-base (get-xml-base elem :old-base parent-xml-base)))
    (remove-if #'null
	       (append
		(get-types-of-node-content elem tm-id xml-base)
		(when (get-ns-attribute elem "type")
		  (list :ID nil
			:topicid (get-ns-attribute elem "type")
			:psi (get-ns-attribute elem "type")))))))


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


(defun property-name-of-node-p (elem property-name-uri)
  "Returns t if the elements tag-name and namespace are equal
   to the given uri."
  (declare (dom:element elem))
  (declare (string property-name-uri))
  (when property-name-uri
    (let ((uri (concatenate-uri (dom:namespace-uri elem)
				(get-node-name elem))))
      (string= uri property-name-uri))))


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