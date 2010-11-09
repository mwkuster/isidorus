;;+-----------------------------------------------------------------------------
;;+  Isidorus
;;+  (c) 2008-2010 Marc Kuester, Christoph Ludwig, Lukas Georgieff
;;+
;;+  Isidorus is freely distributable under the LLGPL license.
;;+  You can find a detailed description in trunk/docs/LLGPL-LICENSE.txt and
;;+  trunk/docs/LGPL-LICENSE.txt.
;;+-----------------------------------------------------------------------------

(defpackage :rdf-importer
  (:use :cl :cxml :elephant :datamodel :isidorus-threading :datamodel
	:base-tools)
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
		*tm2rdf-scope-property*
		*tm2rdf-nametype-property*
		*tm2rdf-occurrencetype-property*
		*tm2rdf-roletype-property*
		*tm2rdf-player-property*
		*tm2rdf-associationtype-property*
		*rdf2tm-blank-node-prefix*
		*tm2rdf-reifier-property*)
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
	   :*rdf-core-xtm*
	   :*document-id*))

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

(defvar *document-id* "isidorus-rdf-document")


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


(defun get-node-refs (nodes tm-id parent-xml-base)
  "Returns a list of node references that can be used as topic IDs."
  (when (and nodes
	     (> (length nodes) 0))
    (loop for node across nodes
       collect (let ((xml-base (get-xml-base node :old-base parent-xml-base)))
		 (parse-node node)
		 (let ((ID (when (get-ns-attribute node "ID")
			     (absolutize-id (get-ns-attribute node "ID")
					    xml-base tm-id)))
		       (nodeID (get-ns-attribute node "nodeID"))
		       (about (when (get-ns-attribute node "about")
				(absolutize-value
				 (get-ns-attribute node "about")
				 xml-base tm-id)))
		       (UUID (get-ns-attribute node "UUID" :ns-uri *rdf2tm-ns*)))
		   (list :topicid (or ID about nodeID UUID)
			 :psi (or ID about)))))))


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


(defun get-absolute-attribute (elem tm-id parent-xml-base attr-name
			       &key (ns-uri *rdf-ns*))
  "Returns an absolute 'attribute' or nil."
  (declare (dom:element elem))
  (declare (string attr-name))
  (tm-id-p tm-id "get-ID")
  (let ((attr (get-ns-attribute elem attr-name :ns-uri ns-uri))
	(xml-base (get-xml-base elem :old-base parent-xml-base)))
    (when attr
      (if (and (string= ns-uri *rdf-ns*)
	       (string= attr-name "ID"))
	  (absolutize-id attr xml-base tm-id)
	  (absolutize-value attr xml-base tm-id)))))


(defun get-datatype (elem tm-id parent-xml-base)
  "Returns a datatype value. The default is xml:string."
  (let ((datatype
	 (get-absolute-attribute elem tm-id parent-xml-base "datatype")))
    (if datatype
	datatype
	*xml-string*)))


(defun tm-id-p (tm-id fun-name)
  "Checks the validity of the passed tm-id."
  (unless (absolute-uri-p tm-id)
    (error "From ~a(): you must provide a stable identifier (PSI-style) for this TM: ~a!"
	   fun-name tm-id)))


(defun get-types-of-node (elem tm-id &key (parent-xml-base nil))
  "Returns a plist of all node's types of the form
   (:topicid <string> :psi <string> :ID <string>)."
  (remove-if
   #'null
   (append (unless (string= (get-type-of-node-name elem)
			    (concatenate 'string *rdf-ns*
					 "Description"))
	     (list 
	      (list :topicid (get-type-of-node-name elem)
		    :psi (get-type-of-node-name elem)
		    :ID nil)))
	   (get-types-of-node-content elem tm-id parent-xml-base))))