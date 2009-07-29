;;+-----------------------------------------------------------------------------
;;+  Isidorus
;;+  (c) 2008-2009 Marc Kuester, Christoph Ludwig, Lukas Giessmann
;;+
;;+  Isidorus is freely distributable under the LGPL license.
;;+  You can find a detailed description in trunk/docs/LGPL-LICENSE.txt.
;;+-----------------------------------------------------------------------------
(in-package :rdf-importer)


(defvar *document-id* nil)


(defun tm-id-p (tm-id fun-name)
  "Checks the validity of the passed tm-id."
  (unless (absolute-uri-p tm-id)
    (error "From ~a(): you must provide a stable identifier (PSI-style) for this TM: ~a!"
	   fun-name tm-id)))


(defun rdf-importer (rdf-xml-path repository-path 
		     &key 
		     (tm-id nil)
		     (document-id (get-uuid)))
  (setf *document-id* document-id)
  (tm-id-p tm-id "rdf-importer")
  (let ((rdf-dom
	 (dom:document-element (cxml:parse-file
				(truename rdf-xml-path)
				(cxml-dom:make-dom-builder)))))
    (unless elephant:*store-controller*
      (elephant:open-store
       (get-store-spec repository-path)))
    (import-dom rdf-dom :tm-id tm-id :document-id document-id)))



(defun import-dom (rdf-dom &key (tm-id nil) (document-id *document-id*))
  (tm-id-p tm-id "import-dom")
  (let ((xml-base (get-xml-base rdf-dom))
	(xml-lang (get-xml-lang rdf-dom))
	(elem-name (get-node-name rdf-dom))
	(elem-ns (dom:namespace-uri rdf-dom)))

    (if (and (string= elem-ns *rdf-ns*)
	     (string= elem-name "RDF"))
	(let ((children (child-nodes-or-text rdf-dom)))
	  (loop for child across children
	     do (import-node child tm-id :document-id document-id
			     :xml-base xml-base :xml-lang xml-lang)))
	  (import-node rdf-dom tm-id :document-id document-id
		       :xml-base xml-base :xml-lang xml-lang))))


(defun import-node (elem tm-id &key (document-id *document-id*)
		    (xml-base nil) (xml-lang nil))
  (declare (ignorable document-id)) ;TODO: remove
  (tm-id-p tm-id "import-node")
  (parse-node elem)
  (let ((fn-xml-base (get-xml-base elem :old-base xml-base)))
    (loop for property across (child-nodes-or-text elem)
       do (parse-property property))
    (let ((about
	   (if (get-ns-attribute elem "about")
	       (absolutize-value (get-ns-attribute elem "about")
				 fn-xml-base tm-id)
	       nil))
	  (nodeID (get-ns-attribute elem "nodeID"))
	  (ID (get-ns-attribute elem "ID"))
	  (UUID (get-ns-attribute elem "UUID" :ns-uri *rdf2tm-ns*))
	  (literals (append (get-literals-of-node elem xml-lang)
			    (get-literals-of-node-content elem tm-id
							  xml-base xml-lang)))
	  (associations nil)
	  (types (append (list
			  (list :value (get-type-of-node-name elem) :ID nil))
			 (get-types-of-node-content elem tm-id fn-xml-base)))
	  (super-classes nil)) ;TODO: implement
    (declare (ignorable about nodeID ID UUID literals associations ;TODO: remove
			types super-classes)))))


(defun get-literals-of-node-content (node tm-id xml-base xml-lang)
  "Returns a list of literals that is produced of a node's content."
  (declare (dom:element node))
  (tm-id-p tm-id "get-literals-of-content")
  (let ((properties (child-nodes-or-text node))
	(fn-xml-base (get-xml-base node :old-base xml-base))
	(fn-xml-lang (get-xml-lang node :old-lang xml-lang)))
    (let ((literals
	   (loop for property across properties
	      when (let ((prp-xml-base (get-xml-base property
						     :old-base fn-xml-base)))
		     (let ((datatype
			    (when (get-ns-attribute property "datatype")
			      (absolutize-value
			       (get-ns-attribute property "datatype")
			       prp-xml-base tm-id)))
			   (parseType (get-ns-attribute property "parseType"))
			   (nodeID (get-ns-attribute property "nodeID"))
			   (resource (get-ns-attribute property "resource"))
			   (UUID (get-ns-attribute property "UUID"
						   :ns-uri *rdf2tm-ns*)))
		       (or (or datatype
			       (string= parseType "Literal"))
			   (not (or nodeID resource UUID parseType)))))
	      collect (let ((content (child-nodes-or-text property))
			    (prp-xml-base (get-xml-base property
							:old-base fn-xml-base))
			    (ID (get-ns-attribute property "ID"))
			    (prp-name (get-node-name property))
			    (prp-ns (dom:namespace-uri property))
			    (child-xml-lang
			     (get-xml-lang property :old-lang fn-xml-lang)))
			(let ((full-name (concatenate-uri prp-ns prp-name))
			      (datatype
			       (if (get-ns-attribute property "datatype")
				   (absolutize-value
				    (get-ns-attribute property "datatype")
				    prp-xml-base tm-id)
				   *xml-string*))
			      (text
			       (cond
				 ((= (length content) 0)
				  "")
				 ((not (stringp content)) ;must be an element
				  (let ((text-val ""))
				    (loop for content-node across
					 (dom:child-nodes property)
				       do (push-string
					   (node-to-string content-node)
					   text-val))
				    text-val))
				 (t content))))
			  (list :type full-name
				:value text
				:ID ID
				:lang child-xml-lang
				:datatype datatype))))))
			  
      literals)))


(defun get-type-of-node-name (node)
  "Returns the type of the node name (namespace + tagname)."
  (let ((node-name (get-node-name node))
	(node-ns (dom:namespace-uri node)))
    (concatenate-uri node-ns node-name)))


(defun get-types-of-node-content (node tm-id xml-base)
  "Returns a list of type-uris that corresponds to the node's content
   or attributes."
  (let ((fn-xml-base (get-xml-base node :old-base xml-base)))
    (let ((attr-type
	   (if (get-ns-attribute node "type")
	       (list
		(list :value (absolutize-value (get-ns-attribute node "type")
					       fn-xml-base tm-id)
		      :ID nil))
	       nil))
	  (content-types
	   (loop for child across (child-nodes-or-text node)
	      when (and (string= (dom:namespace-uri child) *rdf-ns*)
			(string= (get-node-name child) "type"))
	      collect (let ((nodeID (get-ns-attribute child "nodeID"))
			    (resource (if (get-ns-attribute child "resource")
					  (absolutize-value 
					   (get-ns-attribute child "resource")
					   fn-xml-base tm-id)))
			    (UUID (get-ns-attribute child "UUID"
						    :ns-uri *rdf2tm-ns*))
			    (ID (get-ns-attribute child "ID")))
			(if (or nodeID resource UUID)
			    (list :value (or nodeID resource UUID)
				  :ID ID)
			    (let ((child-xml-base
				   (get-xml-base child :old-base fn-xml-base)))
			      (loop for ref in 
				   (get-node-refs (child-nodes-or-text child)
						  tm-id child-xml-base)
				 append (list :value ref
					      :ID ID))))))))
      (remove-if #'null (append attr-type content-types)))))


(defun get-literals-of-property (property xml-lang)
  "Returns a list of attributes that are treated as literal nodes."
  (let ((fn-xml-lang (get-xml-lang property :old-lang xml-lang))
	(attributes nil))
    (dom:map-node-map
     #'(lambda(attr)
	 (let ((attr-ns (dom:namespace-uri attr))
	       (attr-name (get-node-name attr)))
	   (let ((l-type (concatenate-uri attr-ns attr-name))
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
	   (let ((l-type (concatenate-uri attr-ns attr-name))
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


