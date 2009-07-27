;;+-----------------------------------------------------------------------------
;;+  Isidorus
;;+  (c) 2008-2009 Marc Kuester, Christoph Ludwig, Lukas Giessmann
;;+
;;+  Isidorus is freely distributable under the LGPL license.
;;+  You can find a detailed description in trunk/docs/LGPL-LICENSE.txt.
;;+-----------------------------------------------------------------------------

(defpackage :rdf-importer
  (:use :cl :cxml :elephant :datamodel :isidorus-threading)
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
		concatenate-uri
		push-string)
  (:import-from :xml-importer
		get-uuid
		get-store-spec)
  (:import-from :exceptions
                missing-reference-error
                duplicate-identifier-error))

(in-package :rdf-importer)


(defun _n-p (node-name)
  "Returns t if the given value is of the form _[0-9]+"
  (when (and node-name
	     (> (length node-name) 0)
	     (eql (elt node-name 0) #\_))
    (let ((rest
	   (subseq node-name 1 (length node-name))))
      (declare (string node-name))
      (handler-case (let ((int
			   (parse-integer rest)))
		      int)
	(condition () nil)))))


(defun parse-node-name (node)
  "Parses the given node's name to the known rdf/rdfs nodes and arcs.
   If the given name es equal to a property an error is thrown otherwise
   there is displayed a warning."
  (declare (dom:element node))
  (let ((node-name (get-node-name node))
	(node-ns (dom:namespace-uri node)))
    (when (string= node-ns *rdf-ns*)
      (when (or (string= node-name "type")
		(string= node-name "first")
		(string= node-name "rest")
		(string= node-name "subject")
		(string= node-name "predicate")
		(string= node-name "object"))
	(error "From parse-node-name(): rdf:~a is a property and not allowed here!"
	       node-name))
      (when (string= node-name "RDF")
	(error "From parse-node-name(): rdf:RDF not allowed here!"))
      (unless (or (string= node-name "Description")
		  (string= node-name "List")
		  (string= node-name "Alt")
		  (string= node-name "Bag")
		  (string= node-name "Seq")
		  (string= node-name "Statement")
		  (string= node-name "Property")
		  (string= node-name "XMLLiteral"))
	(format t "From parse-node-name(): Warning: ~a is not a known rdf:type!~%"
		node-name)))
    (when (string= node-ns *rdfs-ns*)
      (when (or (string= node-name "subClassOf")
		(string= node-name "subPropertyOf")
		(string= node-name "domain")
		(string= node-name "range")
		(string= node-name "label")
		(string= node-name "comment")
		(string= node-name "member")
		(string= node-name "seeAlso")
		(string= node-name "isDefinedBy"))
	(error "From parse-node-name(): rdfs:~a is a property and not allowed here!"
	       node-name))
      (unless (and (string= node-name "Resource")
		   (string= node-name "Literal")
		   (string= node-name "Class")
		   (string= node-name "Datatype")
		   (string= node-name "Cotnainer")
		   (string= node-name "ContainerMembershipProperty"))
	(format t "From parse-node-name(): Warning: rdfs:~a is not a known rdfs:type!~%"
		node-name))))
  t)


(defun parse-node(node)
  "Parses a node that represents a rdf-resource."
  (declare (dom:element node))
  (parse-node-name node)
  (let ((ID  (get-ns-attribute node "ID"))
	(nodeID (get-ns-attribute node "nodeID"))
	(about (get-ns-attribute node "about"))
	(err-pref "From parse-node(): "))
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
      (condition (err) (error "~a~a" err-pref err))))
  t)



(defun get-literals-of-node (node)
  "Returns alist of attributes that are treated as literal nodes."
  (let ((attributes nil))
    (dom:map-node-map
     #'(lambda(attr)
	 (let ((attr-ns (dom:namespace-uri attr))
	       (attr-name (get-node-name attr)))
	   (cond
	     ((string= attr-ns *rdf-ns*)
	      (unless (or (string= attr-name "ID")
			  (string= attr-name "about")
			  (string= attr-name "nodeID")
			  (string= attr-name "type"))
		(push (list :type (concatenate-uri attr-ns attr-name)
			    :value (get-ns-attribute node attr-name))
		      attributes)))
	     ((or (string= attr-ns *xml-ns*)
		  (string= attr-ns *xmlns-ns*))
	      nil);;do nothing, all xml-attributes are no literals
	     ((string= attr-ns *rdfs-ns*)
	      (if (or (string= attr-name "subClassOf")
		      (string= attr-name "Class"))
		  (error "From get-literals-of-node(): rdfs:~a is not allowed here"
			 attr-name)
		  (push (list :type (concatenate-uri attr-ns attr-name)
			      :value (get-ns-attribute node attr-name
						       :ns-uri attr-ns))
			attributes)))
	     (t
	      (push (list :type (concatenate-uri attr-ns attr-name)
			  :value (get-ns-attribute node attr-name
						   :ns-uri attr-ns))
		    attributes)))))
     (dom:attributes node))
    attributes))