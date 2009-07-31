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
		*rdf2tm-ns*)
  (:import-from :xml-constants
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
		get-store-spec)
  (:import-from :exceptions
                missing-reference-error
                duplicate-identifier-error))

(in-package :rdf-importer)

(defvar *rdf-types* (list "Description" "List" "Alt" "Bag" "Seq"
			  "Statement" "Property" "XMLLiteral"))

(defvar *rdf-properties* (list "type" "first" "rest" "subject" "predicate"
			       "object" "li"))

(defvar *rdfs-types* (list "Resource" "Literal" "Class" "Datatype"
			   "Container" "ContainerMembershipProperty"))

(defvar *rdfs-properties* (list "subClassOf" "subPropertyOf" "domain"
				"range" "range" "label" "comment"
				"member" "seeAlso" "isDefinedBy"))

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


(defun set-_n-name (property _n-counter)
  "Returns a name of the form <rdf>_[1-9][0-9]* and adds a tupple
   of the form :elem <dom-elem> :type<<rdf>_[1-9][0-9]*> to the
   list *_n-map*.
   If the dom-elem is already contained in the list only the
   <rdf>_[1-9][0-9]* name is returned."
  (let ((map-item (find-if #'(lambda(x)
			       (eql (getf x :elem) property))
			   *_n-map*)))
    (if map-item
	(getf map-item :type)
	(let ((new-type-name
	       (concatenate 'string *rdf-ns* "_" (write-to-string _n-counter))))
	  (push (list :elem property
		      :type new-type-name)
		*_n-map*)
	  new-type-name))))


(defun unset-_n-name (property)
  "Deletes the passed property tupple of the *_n-map* list."
  (setf *_n-map* (remove-if #'(lambda(x)
				(eql (getf x :elem) property))
			    *_n-map*)))


(defun remove-node-properties-from-*_n-map* (node)
  "Removes all node's properties from the list *_n-map*."
  (declare (dom:element node))
  (let ((properties (child-nodes-or-text node)))
    (when properties
      (loop for property across properties
	 do (unset-_n-name property))))
  (dom:map-node-map
   #'(lambda(attr) (unset-_n-name attr))
   (dom:attributes node)))


(defun get-type-of-node-name (node)
  "Returns the type of the node name (namespace + tagname).
   When the node is contained in *_n-map* the corresponding
   value of this map will be returned."
  (let ((map-item (find-if #'(lambda(x)
			       (eql (getf x :elem) node))
			   *_n-map*)))
    (if map-item
	(getf map-item :type)
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
    (unless (or ID nodeID about)
      (dom:set-attribute-ns node *rdf2tm-ns* "UUID" (get-uuid)))
    (handler-case (let ((content (child-nodes-or-text node :trim t)))
		    (when (stringp content)
		      (error "text-content not allowed here!")))
      (condition (err) (error "~a~a" err-pref err)))
    (when (or resource datatype parseType class subClassOf)
      (error "~a~a is not allowed here!"
	     err-pref (cond
			(resource (concatenate 'string "resource("
					       resource ")"))
			(datatype (concatenate 'string "datatype("
					       datatype ")"))
			(parseType (concatenate 'string "parseType("
						parseType ")"))
			(class (concatenate 'string "Class(" class ")"))
			(subClassOf (concatenate 'string "subClassOf("
						 subClassOf ")")))))
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


(defun parse-property-name (property _n-counter)
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
      (set-_n-name property _n-counter)))
  t)


(defun parse-property (property _n-counter)
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
    (parse-property-name property _n-counter)
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
      (dom:set-attribute-ns property *rdf2tm-ns* "UUID" (get-uuid)))
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
    (when (and (or nodeID resource type)
	       datatype)
      (error "~aonly one of ~a and rdf:datatype (~a) is allowed!"
	     err-pref
	     (cond
	       (nodeID (concatenate 'string "rdf:nodeID (" nodeID ")"))
	       (resource (concatenate 'string "rdf:resource (" resource ")"))
	       (type (concatenate 'string "rdf:type (" type ")")))
	     datatype))
    (when (and (or type nodeID resource)
	       (> (length content) 0))
      (error "~awhen ~a is set no content is allowed: ~a!"
	     err-pref
	     (cond
	       (type (concatenate 'string "rdf:type (" type ")"))
	       (nodeID (concatenate 'string "rdf:nodeID (" nodeID ")"))
	       (resource (concatenate 'string "rdf:resource (" resource ")")))
	     content))
    (when (and (or type
		   (and (string= node-name "type")
			(string= node-ns *rdf-ns*))
		   (> (length literals) 0))
	       (not (or nodeID resource))
	       (not content))
      (dom:set-attribute-ns property *rdf2tm-ns* "UUID" (get-uuid)))
    (when (or about subClassOf)
      (error "~a~a not allowed here!"
	     err-pref
	     (if about
		 (concatenate 'string "rdf:about (" about ")")
		 (concatenate 'string "rdfs:subClassOf (" subClassOf ")"))))
    (when (and (string= node-name "subClassOf")
	       (string= node-ns *rdfs-ns*)
	       (not (or nodeID resource content)))
      (dom:set-attribute-ns property *rdf2tm-ns* "UUID" (get-uuid)))
    (when (and (or (and (string= node-name "type")
			(string= node-ns *rdf-ns*))
		   (and (string= node-name "subClassOf")
			(string= node-ns *rdfs-ns*)))
	       (and (> (length content) 0)
		    (stringp content)))
      (error "~awhen ~a not allowed to own literal content: ~a!"
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


(defun parse-properties-of-node (node)
  "Parses all node's properties by calling the parse-propery
   function and sets all rdf:li properties as a tupple to the
   *_n-map* list."
  (let ((child-nodes (child-nodes-or-text node))
	(_n-counter 0))
    (when (get-ns-attribute node "li")
      (dom:map-node-map
       #'(lambda(attr)
	   (when (and (string= (get-node-name attr) "li")
		      (string= (dom:namespace-uri attr) *rdf-ns*))
	     (incf _n-counter)
	     (set-_n-name attr _n-counter)))
	     (dom:attributes node)))
    (when child-nodes
      (loop for property across child-nodes
	 do (let ((prop-name (get-node-name property))
		  (prop-ns (dom:namespace-uri node)))
	      (when (and (string= prop-name "li")
			 (string= prop-ns *rdf-ns*))
		(incf _n-counter))
	      (parse-property property _n-counter)))))
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
				 