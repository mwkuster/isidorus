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
		     (document-id (get-uuid))
		     (start-revision (d:get-revision)))
  (setf *document-id* document-id)
  (tm-id-p tm-id "rdf-importer")
  (let ((rdf-dom
	 (dom:document-element (cxml:parse-file
				(truename rdf-xml-path)
				(cxml-dom:make-dom-builder)))))
    (unless elephant:*store-controller*
      (elephant:open-store
       (get-store-spec repository-path)))
    (import-dom rdf-dom start-revision :tm-id tm-id :document-id document-id))
  (setf *_n-map* nil))


(defun import-dom (rdf-dom start-revision
		   &key (tm-id nil) (document-id *document-id*))
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
	  (types (append (list
			  (list :topicid (get-type-of-node-name elem)
				:psi (get-type-of-node-name elem)
				:ID nil))
			 (get-types-of-node-content elem tm-id fn-xml-base)))
	  (super-classes (get-super-classes-of-node-content elem tm-id xml-base)))
      (let ((topic-stub (make-topic-stub-from-node about ID nodeID UUID
						   start-revision
						   :document-id document-id)))
	
      ;TODO:
      ;*get-topic by topic id
      ;*make psis
      ;*if the topic does not exist create one with topic id
      ;*add psis
      ;make instance-of associations
      ;make topictype topics with topic id
      ;make super-sub-class associations
      ;make occurrencetype topics with topic id
      ;make and add occurrences
      ;make referenced topic with topic id
      ;make and add associations


      ;TODO: start recursion ...
      (remove-node-properties-from-*_n-map* elem)
      (or tm-id document-id topic-stub nodeID UUID literals ;TODO: remove
	  associations types super-classes)))))


(defun make-topic-stub-from-node (about ID nodeId UUID start-revision
				  &key (document-id *document-id*))
  "Returns a topic corresponding to the passed parameters.
   When the searched topic does not exist there will be created one.
   If about or ID is set there will aslo be created a new PSI."
;  (let ((topic-id (or about ID nodeID UUID))
;	(psi-value (or about ID))
;	(err-pref "From make-topic-stub-from-node(): "))
;    (unless topic-id
;      (error "~aone of about ID nodeID UUID must be set!"
;	     err-pref))
;    (elephant:ensure-transaction (:txn-nosync t)
;      (let ((top (get-item-by-id topic-id :xtm-id document-id
;				 :revision start-revision)))
;	(let ((topic-psis (map 'list #'d:uri (d:psis top))))
;	  (if (and psi-value
;		   (not (find psi-value topic-psis :test #'string=)))
;	      (let ((psis (list (d::make-instance
;				 'd:PersistentIdC
;				 :uri psi-value
;				 :start-revision start-revision))))
;		;create only a new topic if there existed no one
;		(d::make-instance 'd:TopicC
;				  :topicid topic-id
;				  :psis psis
;				  :xtm-id document-id
;				  :start-revision start-revision))
;	      top))))))
)


(defun make-occurrence-from-node (top literals start-revision
				  &key (document-id *document-id*))
;  (loop for literal in literals
;     do (let ((type
  )


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
					   property nil)))
		       (and (or (or datatype
				    (string= parseType "Literal"))
				(not (or nodeID resource UUID parseType)))
			    (not (or type prop-literals))))
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