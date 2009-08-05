;;+-----------------------------------------------------------------------------
;;+  Isidorus
;;+  (c) 2008-2009 Marc Kuester, Christoph Ludwig, Lukas Giessmann
;;+
;;+  Isidorus is freely distributable under the LGPL license.
;;+  You can find a detailed description in trunk/docs/LGPL-LICENSE.txt.
;;+-----------------------------------------------------------------------------


(defpackage :rdf-importer-test
  (:use 
   :common-lisp
   :xml-importer
   :datamodel
   :it.bese.FiveAM
   :fixtures)
  (:import-from :constants
                *rdf-ns*
		*rdfs-ns*
		*rdf2tm-ns*
		*xml-ns*
		*xml-string*
		*instance-psi*
		*type-psi*
		*type-instance-psi*
		*subtype-psi*
		*supertype-psi*
		*supertype-subtype-psi*
		*xml-string*
		*rdf2tm-object*
		*rdf2tm-subject*
		*rdf-subject*
		*rdf-object*
		*rdf-predicate*
		*rdf-statement*)
  (:import-from :xml-tools
                xpath-child-elems-by-qname
		xpath-single-child-elem-by-qname
                xpath-select-location-path
		get-ns-attribute
		absolute-uri-p)
  (:export :rdf-importer-test
	   :test-get-literals-of-node
	   :test-parse-node
	   :run-rdf-importer-tests
	   :test-get-literals-of-property
	   :test-parse-property
	   :test-get-types
	   :test-get-literals-of-content
	   :test-get-super-classes-of-node-content
	   :test-get-associations-of-node-content
	   :test-parse-properties-of-node
	   :test-import-node-1
	   :test-import-node-reification
	   :test-import-dom))

(declaim (optimize (debug 3) (speed 0) (safety 3) (space 0) (compilation-speed 0)))

(in-package :rdf-importer-test)


(def-suite rdf-importer-test
     :description "tests  various key functions of the importer")

(in-suite rdf-importer-test)


(defun rdf-init-db (&key (db-dir "data_base") (start-revision (get-revision)))
  "Empties the data base files and initializes isidorus for rdf."
  (when elephant:*store-controller*
    (elephant:close-store))
  (clean-out-db db-dir)
  (elephant:open-store (xml-importer:get-store-spec db-dir))
  (xml-importer:init-isidorus start-revision)
  (rdf-importer:init-rdf-module start-revision))


(test test-get-literals-of-node
  "Tests the helper function get-literals-of-node."
  (let ((doc-1
	 (concatenate 'string "<rdf:Description xmlns:rdf=\"" *rdf-ns* "\" "
		      "xmlns:isi=\"http://isidorus/test#\" "
		      "rdf:type=\"rdfType\" rdf:ID=\"rdfID\" rdf:nodeID=\""
		      "rdfNodeID\" rdf:unknown=\"rdfUnknown\" "
		      "isi:ID=\"isiID\" isi:arc=\"isiArc\" "
		      "isi:empty=\"\"/>")))
    (let ((dom-1 (cxml:parse doc-1 (cxml-dom:make-dom-builder))))
      (is (= (length (dom:child-nodes dom-1)) 1))
      (let ((literals (rdf-importer::get-literals-of-node
		       (elt (dom:child-nodes dom-1) 0) nil)))
	(is-true literals)
	(is (= (length literals) 4))
	(is-true (find-if #'(lambda(x)
			      (and
			       (string= (getf x :value) "rdfUnknown")
			       (string= (getf x :type)
					(concatenate 'string *rdf-ns* "unknown"))
			       (not (getf x :ID))))
			      literals))
	(is-true (find-if #'(lambda(x)
			      (and
			       (string= (getf x :value) "isiID")
			       (string= (getf x :type)
					"http://isidorus/test#ID")
			       (not (getf x :ID))))
			  literals))
	(is-true (find-if #'(lambda(x)
			      (and 
			       (string= (getf x :value) "isiArc")
			       (string= (getf x :type)
					"http://isidorus/test#arc")
			       (not (getf x :ID))))
			  literals))
	(is-true (find-if #'(lambda(x)
			      (and 
			       (string= (getf x :value) "")
			       (string= (getf x :type)
					"http://isidorus/test#empty")
			       (not (getf x :ID))))
			  literals))
	(map 'list #'(lambda(x) (is-false (getf x :lang)))
	     literals)))
      
    (let ((dom-1 (cxml:parse doc-1 (cxml-dom:make-dom-builder))))
      (is (= (length (dom:child-nodes dom-1)) 1))
      (dom:set-attribute-ns (elt (dom:child-nodes dom-1) 0)
			    *xml-ns* "lang" "de")
      (let ((literals (rdf-importer::get-literals-of-node
		       (elt (dom:child-nodes dom-1) 0) "en")))
	(is-true literals)
	(is (= (length literals) 4))
	(is-true (find-if #'(lambda(x)
			      (and
			       (string= (getf x :value) "rdfUnknown")
			       (string= (getf x :type)
					(concatenate 'string *rdf-ns* "unknown"))
			       (not (getf x :ID))))
			      literals))
	(is-true (find-if #'(lambda(x)
			      (and
			       (string= (getf x :value) "isiID")
			       (string= (getf x :type)
					"http://isidorus/test#ID")
			       (not (getf x :ID))))
			  literals))
	(is-true (find-if #'(lambda(x)
			      (and 
			       (string= (getf x :value) "isiArc")
			       (string= (getf x :type)
					"http://isidorus/test#arc")
			       (not (getf x :ID))))
			  literals))
	(is-true (find-if #'(lambda(x)
			      (and 
			       (string= (getf x :value) "")
			       (string= (getf x :type)
					"http://isidorus/test#empty")
			       (not (getf x :ID))))
			  literals))
	(map 'list #'(lambda(x) (is-true (string= (getf x :lang) "de")))
	     literals)))))


(test test-parse-node
  "Tests the parse-node function."
  (let ((doc-1
	 (concatenate 'string "<rdf:UnknownType xmlns:rdf=\"" *rdf-ns* "\" "
		      "xmlns:isi=\"" *rdf2tm-ns* "\" "
		      "xmlns:arcs=\"http://test/arcs/\" "
		      "rdf:ID=\"rdfID\" xml:base=\"xmlBase\" "
		      "arcs:arc=\"arcsArc\">"
		      "<arcs:rel>"
		      "<rdf:Description rdf:about=\"element\"/>"
		      "</arcs:rel>"
		      "</rdf:UnknownType>")))
    (let ((dom-1 (cxml:parse doc-1 (cxml-dom:make-dom-builder))))
      (is (length (dom:child-nodes dom-1)) 1)
      (let ((node (elt (dom:child-nodes dom-1) 0)))
	(is-true (rdf-importer::parse-node node))
	(is-false (get-ns-attribute node "UUID" :ns-uri *rdf2tm-ns*))
	(dom:set-attribute-ns node *rdf-ns* "about" "rdfAbout")
	(signals error (rdf-importer::parse-node node))
	(dom:set-attribute-ns node *rdf-ns* "nodeID" "rdfNodeID")
	(signals error (rdf-importer::parse-node node))
	(dom:remove-attribute-ns node *rdf-ns* "about")
	(signals error (rdf-importer::parse-node node))
	(dom:remove-attribute-ns node *rdf-ns* "ID")
	(is-true (rdf-importer::parse-node node))
	(dom:set-attribute-ns node *rdf-ns* "about" "rdfAbout")
	(signals error (rdf-importer::parse-node node))
	(is-false (get-ns-attribute node "UUID" :ns-uri *rdf2tm-ns*))
	(dom:remove-attribute-ns node *rdf-ns* "about")
	(dom:remove-attribute-ns node *rdf-ns* "nodeID")
	(is-true (rdf-importer::parse-node node))
	(is-true (get-ns-attribute node "UUID" :ns-uri *rdf2tm-ns*))
	(dom:set-attribute-ns node *rdf-ns* "resource" "rdfResource")
	(signals error (rdf-importer::parse-node node))
	(dom:set-attribute-ns node *rdf-ns* "resource" "")
	(is-true (rdf-importer::parse-node node))
	(dom:replace-child node (dom:create-text-node dom-1 "anyText")
			   (xpath-single-child-elem-by-qname
			    node "http://test/arcs/" "rel"))
	(signals error (rdf-importer::parse-node node))))))


(test test-get-literals-of-property
  "Tests the function get-literals-or-property."
  (let ((doc-1
	 (concatenate 'string "<prop:property xmlns:prop=\"http://props/\" "
		      "xmlns:rdf=\"" *rdf-ns* "\" "
		      "xmlns:rdfs=\"" *rdfs-ns* "\" "
		      "rdf:type=\"rdfType\" rdf:resource=\"rdfResource\" "
		      "rdf:nodeID=\"rdfNodeID\" "
		      "prop:prop1=\"http://should/be/a/literal\" "
		      "prop:prop2=\"prop-2\" "
		      "prop:prop3=\"\">content-text</prop:property>")))
    (let ((dom-1 (cxml:parse doc-1 (cxml-dom:make-dom-builder))))
      (is-true dom-1)
      (is (= (length (dom:child-nodes dom-1)) 1))
      (let ((property (elt (dom:child-nodes dom-1) 0)))
	(let ((literals (rdf-importer::get-literals-of-property property nil)))
	  (is (= (length literals) 3))
	  (is-true (find-if #'(lambda(x)
				(and
				 (string= (getf x :value)
					  "http://should/be/a/literal")
				 (string= (getf x :type) "http://props/prop1")
				 (not (getf x :ID))))
			    literals))
	  (is-true (find-if #'(lambda(x)
				(and
				 (string= (getf x :value) "prop-2")
				 (string= (getf x :type) "http://props/prop2")
				 (not (getf x :ID))))
			    literals))
	  (is-true (find-if #'(lambda(x)
				(and
				 (string= (getf x :value) "")
				 (string= (getf x :type) "http://props/prop3")
				 (not (getf x :ID))))
			    literals)))))))


(test test-parse-property
  "Tests the function parse-property."
  (let ((doc-1
	 (concatenate 'string "<rdf:Description xmlns:rdf=\"" *rdf-ns* "\" "
		      "xmlns:rdfs=\"" *rdfs-ns* "\" "
		      "xmlns:prop=\"http://isidorus/props/\">"
		      "<prop:prop0 rdf:parseType=\"Resource\" />"
		      "<prop:prop1 rdf:parseType=\"Resource\">"
		      "<prop:prop1_0 rdf:resource=\"prop21\" />"
		      "</prop:prop1>"
		      "<prop:prop2 rdf:parseType=\"Literal\">"
		      "<content_root>content-text</content_root>"
		      "</prop:prop2>"
		      "<prop:prop3 rdf:parseType=\"Collection\" />"
		      "<prop:prop4 rdf:parseType=\"Collection\">"
		      "<prop:prop4_0 rdf:resource=\"prop5_1\" />"
		      "<prop:prop4_1 rdf:nodeID=\"prop5_2\" />"
		      "<prop:prop4_2/>"
		      "</prop:prop4>"
		      "<prop:prop5 />"
		      "<prop:prop6>prop6</prop:prop6>"
		      "<prop:prop7 rdf:nodeID=\"prop7\"/>"
		      "<prop:prop8 rdf:resource=\"prop8\" />"
		      "<prop:prop9 rdf:type=\"typeProp9\">   </prop:prop9>"
		      "<prop:prop10 rdf:datatype=\"datatypeProp10\" />"
		      "<prop:prop11 rdf:ID=\"IDProp11\">   </prop:prop11>"
		      "<prop:prop12 rdf:ID=\"IDprop12\" rdf:nodeID=\"prop12\">"
		      "      </prop:prop12>"
		      "<prop:prop13 />"
		      "<prop:prop14>prop14</prop:prop14>"
		      "<prop:prop15 rdf:nodeID=\"prop15\"/>"
		      "<prop:prop16 rdf:resource=\"prop16\" />"
		      "<prop:prop17 rdf:type=\"typeProp17\">   </prop:prop17>"
		      "<prop:prop18 rdf:ID=\"IDprop18\" rdf:nodeID=\"prop18\">"
		      "      </prop:prop18>"
		      "</rdf:Description>")))
    (let ((dom-1 (cxml:parse doc-1 (cxml-dom:make-dom-builder))))
      (is-true dom-1)
      (is (= (length (dom:child-nodes dom-1)) 1))
      (let ((child (elt (dom:child-nodes dom-1) 0)))
	(let ((children (rdf-importer::child-nodes-or-text child))
	      (text-node (dom:create-text-node dom-1 "new text node")))
	(is (= (length children) 19))
	(loop for property across children
	   do (is-true (rdf-importer::parse-property property 0)))
	(dotimes (i (length children))
	  (if (or (= i 0) (= i 1) (= i 3) (= i 4) (= i 9) (= i 17))
	      (is-true (get-ns-attribute (elt children i) "UUID"
					 :ns-uri *rdf2tm-ns*))
	      (is-false (get-ns-attribute (elt children i) "UUID"
					 :ns-uri *rdf2tm-ns*))))
	(let ((prop (elt children 0)))
	  (dom:set-attribute-ns prop *rdf-ns* "parseType" "Unknown")
	  (signals error (rdf-importer::parse-property prop 0))
	  (dom:set-attribute-ns prop *rdf-ns* "parseType" "Resource")
	  (is-true (rdf-importer::parse-property prop 0))
	  (dom:set-attribute-ns prop *rdf-ns* "ID" "newID")
	  (is-true (rdf-importer::parse-property prop 0))
	  (dom:set-attribute-ns prop *rdf-ns* "bad" "bad")
	  (signals error (rdf-importer::parse-property prop 0))
	  (dom:remove-attribute-ns prop *rdf-ns* "bad")
	  (is-true (rdf-importer::parse-property prop 0))
	  (dom:append-child prop text-node)
	  (signals error (rdf-importer::parse-property prop 0))
	  (dom:remove-child prop text-node)
	  (is-true (rdf-importer::parse-property prop 0)))
	(let ((prop (elt children 1)))
	  (dom:set-attribute-ns prop *rdf-ns* "nodeID" "bad")
	  (signals error (rdf-importer::parse-property prop 0))
	  (dom:remove-attribute-ns prop *rdf-ns* "nodeID")
	  (is-true (rdf-importer::parse-property prop 0))
	  (dom:set-attribute-ns prop *rdf-ns* "ID" "newID")
	  (is-true (rdf-importer::parse-property prop 0))
	  (dom:append-child prop text-node)
	  (signals error (rdf-importer::parse-property prop 0))
	  (dom:remove-child prop text-node)
	  (is-true (rdf-importer::parse-property prop 0)))
	(let ((prop (elt children 3)))
	  (dom:append-child prop text-node)
	  (signals error (rdf-importer::parse-property prop 0))
	  (dom:remove-child prop text-node)
	  (is-true (rdf-importer::parse-property prop 0)))
	(let ((prop (elt children 4)))
	  (dom:append-child prop text-node)
	  (signals error (rdf-importer::parse-property prop 0))
	  (dom:remove-child prop text-node)
	  (is-true (rdf-importer::parse-property prop 0)))
	(let ((prop (elt children 5)))
	  (dom:set-attribute-ns prop *rdf-ns* "type" "newType")
	  (is-true (rdf-importer::parse-property prop 0))
	  (dom:set-attribute-ns prop *rdf-ns* "unknown" "unknown")
	  (is-true (rdf-importer::parse-property prop 0))
	  (dom:append-child prop text-node)
	  (signals error (rdf-importer::parse-property prop 0))
	  (dom:remove-child prop text-node)
	  (is-true (rdf-importer::parse-property prop 0))
	  (dom:remove-attribute-ns prop *rdf-ns* "unknown")
	  (is-true (rdf-importer::parse-property prop 0))
	  (dom:append-child prop text-node)
	  (signals error (rdf-importer::parse-property prop 0))
	  (dom:remove-child prop text-node)
	  (is-true (rdf-importer::parse-property prop 0)))
	(let ((prop (elt children 10)))
	  (dom:set-attribute-ns prop *rdf-ns* "type" "newType")
	  (signals error (rdf-importer::parse-property prop 0))
	  (dom:remove-attribute-ns prop *rdf-ns* "type")
	  (is-true (rdf-importer::parse-property prop 0))
	  (dom:set-attribute-ns prop *rdf-ns* "nodeID" "newNodeID")
	  (signals error (rdf-importer::parse-property prop 0))
	  (dom:remove-attribute-ns prop *rdf-ns* "nodeID")
	  (is-true (rdf-importer::parse-property prop 0))
	  (dom:set-attribute-ns prop *rdf-ns* "resource" "newResource")
	  (signals error (rdf-importer::parse-property prop 0))
	  (dom:remove-attribute-ns prop *rdf-ns* "resource")
	  (is-true (rdf-importer::parse-property prop 0))
	  (dom:set-attribute-ns prop *rdf-ns* "ID" "newID")
	  (is-true (rdf-importer::parse-property prop 0))))))))


(test test-get-types
  "Tests the functions get-type-of-node-name, get-types-of-content,
   get-node-rerfs, absolute-uri-p, absolutize-value and absolutize-id."
  (let ((tm-id "http://test-tm")
	(doc-1
	 (concatenate 'string "<rdf:anyType xmlns:rdf=\"" *rdf-ns* "\" "
		      "xmlns:isi=\"" *rdf2tm-ns* "\" "
		      "xmlns:arcs=\"http://test/arcs/\" "
                      "xml:base=\"xml-base/first\" "
		      "rdf:about=\"resource\" rdf:type=\"attr-type\">"
		      "<rdf:type rdf:ID=\"rdfID\" "
		      "rdf:resource=\"content-type-1\"/>"
		      "<rdf:type /><!-- blank_node -->"
		      "<rdf:type arcs:arc=\"literalArc\"/>"
		      "<rdf:type rdf:parseType=\"Collection\" "
		      "          xml:base=\"http://xml-base/absolute/\">"
		      "<!-- blank_node that is a list -->"
		      "<rdf:Description rdf:about=\"c-about-type\"/>"
		      "<rdf:Description rdf:ID=\"c-id-type\"/>"
		      "<rdf:Description rdf:nodeID=\"c-nodeID-type\"/>"
		      "<rdf:Description/><!-- blank_node -->"
		      "</rdf:type>"
		      "<rdf:type rdf:ID=\"rdfID2\">"
		      "<rdf:Description rdf:about=\"c-about-type-2\"/>"
		      "</rdf:type>"
		      "<rdf:type>"
		      "<rdf:Description rdf:nodeID=\"c-nodeID-type-2\"/>"
		      "</rdf:type>"
		      "<rdf:type xml:base=\"http://new-base/\">"
		      "<rdf:Description rdf:ID=\"c-ID-type-2\"/>"
		      "</rdf:type>"
		      "<rdf:type rdf:ID=\"rdfID3\">"
		      "<rdf:Description/>"
		      "</rdf:type>"
		      "<arcs:arc rdf:resource=\"anyArc\"/>"
		      "<rdf:arc>"
		      "<rdf:Description rdf:about=\"anyResource\"/>"
		      "</rdf:arc>"
		      "</rdf:anyType>")))
    (let ((dom-1 (cxml:parse doc-1 (cxml-dom:make-dom-builder))))
      (is-true dom-1)
      (is (= (length (dom:child-nodes dom-1)) 1))
      (is-true (absolute-uri-p tm-id))
      (is-false (absolute-uri-p "http//bad"))
      (is-false (absolute-uri-p ""))
      (is-false (absolute-uri-p "          "))
      (is-false (absolute-uri-p nil))
      (let ((node (elt (dom:child-nodes dom-1) 0)))
	(loop for property across (rdf-importer::child-nodes-or-text node)
	   do (rdf-importer::parse-property property 0))
	(let ((types
	       (append
		(list (list
		       :topicid (rdf-importer::get-type-of-node-name node)
		       :psi (rdf-importer::get-type-of-node-name node)
		       :ID nil))
		(rdf-importer::get-types-of-node-content node tm-id nil)))
	      (node-uuid (get-ns-attribute
			  (elt (rdf-importer::child-nodes-or-text
				(elt (rdf-importer::child-nodes-or-text node) 7))
			       0)
			  "UUID" :ns-uri *rdf2tm-ns*)))
	  (is (= (length types) 10))
	  (is-true (find-if
		    #'(lambda(x)
			(and (string= (getf x :topicid)
				      (concatenate
				       'string *rdf-ns* "anyType"))
			     (string= (getf x :topicid)
				      (concatenate
				       'string *rdf-ns* "anyType"))
			     (not (getf x :ID))))
		    types))
	  (is-true (find-if
		    #'(lambda(x)
			(and (string= (getf x :topicid) 
				      (concatenate
				       'string tm-id
				       "/xml-base/first/attr-type"))
			     (string= (getf x :psi) 
				      (concatenate
				       'string tm-id
				       "/xml-base/first/attr-type"))
			     (not (getf x :ID))))
		    types))
	  (is-true (find-if 
		    #'(lambda(x)
			(and (string= (getf x :topicid) 
				      "http://test-tm/xml-base/first/content-type-1")
			     (string= (getf x :psi) 
				      "http://test-tm/xml-base/first/content-type-1")
			     (string= (getf x :ID)
				      "http://test-tm/xml-base/first#rdfID")))
		    types))
	  (is-true (find-if
		    #'(lambda(x)
			(and (string= (getf x :topicid) 
				      (concatenate
				       'string tm-id
				       "/xml-base/first/c-about-type-2"))
			     (string= (getf x :psi) 
				      (concatenate
				       'string tm-id
				       "/xml-base/first/c-about-type-2"))
			     (string= (getf x :ID)
				      "http://test-tm/xml-base/first#rdfID2")))
		    types))
	  (is-true (find-if
		    #'(lambda(x)
			(and (string= (getf x :topicid) "c-nodeID-type-2")
			     (not (getf x :psi))
			     (not (getf x :ID))))
		    types))
	  (is-true (find-if 
		    #'(lambda(x)
			(and (string= (getf x :topicid) 
				      "http://new-base#c-ID-type-2")
			     (string= (getf x :psi) 
				      "http://new-base#c-ID-type-2")
			     (not (getf x :ID))))
		    types))
	  (is-true (find-if 
		    #'(lambda(x)
			(and (string= (getf x :topicid) node-uuid)
			     (not (getf x :psi))
			     (string= (getf x :ID)
				      "http://test-tm/xml-base/first#rdfID3")))
		    types))
	  (is-true (= 10 (count-if #'(lambda(x)
				      (> (length (getf x :topicid)) 0))
				  types))))))))


(test test-get-literals-of-content
  (let ((doc-1
	 (concatenate 'string "<rdf:Description xmlns:rdf=\"" *rdf-ns* "\" "
		      "xmlns:rdfs=\"" *rdfs-ns* "\" "
		      "xmlns:prop=\"http://isidorus/props/\" "
                      "xml:base=\"base/first\" xml:lang=\"de\" >"
		      "<prop:lit0>text0</prop:lit0>"
		      "<prop:lit1 rdf:parseType=\"Literal\">text1</prop:lit1>"
		      "<prop:lit2 xml:base=\"http://base/absolute\" "
		      "rdf:datatype=\"dType1\">text2</prop:lit2>"
		      "<prop:arc rdf:parseType=\"Collection\"/>"
		      "<prop:lit3 xml:lang=\"en\" rdf:datatype=\"dType2\">"
		      "<![CDATA[text3]]></prop:lit3>"
		      "<prop:lit4 rdf:datatype=\"dType2\"><root><child/></root>"
		      "  </prop:lit4>"
		      "<prop:lit5 rdf:ID=\"rdfID\" "
		      "rdf:parseType=\"Literal\"><root><child>"
		      "childText5</child>   </root></prop:lit5>"
		      "<prop:lit6 xml:lang=\"\" rdf:parseType=\"Literal\">"
		      "   <![CDATA[text6]]>  abc "
		      "</prop:lit6>"
		      "</rdf:Description>")))
    (let ((dom-1 (cxml:parse doc-1 (cxml-dom:make-dom-builder)))
	  (tm-id "http://test-tm"))
      (is-true dom-1)
      (is (= (length (dom:child-nodes dom-1)) 1))
      (let ((node (elt (dom:child-nodes dom-1) 0)))
	(dotimes (iter (length (dom:child-nodes node)))
	  (is-true (rdf-importer::parse-property
		    (elt (dom:child-nodes node) iter) 0)))
	(let ((literals (rdf-importer::get-literals-of-node-content
			 node tm-id nil nil)))
	  (is (= (length literals) 7))
	  (is-true (find-if
		    #'(lambda(x)
			(and (string= (getf x :value) "text0")
			     (string= (getf x :type)
				      "http://isidorus/props/lit0")
			     (not (getf x :ID))
			     (string= (getf x :lang) "de")
			     (string= (getf x :datatype) *xml-string*)))
		    literals))
	  (is-true (find-if
		    #'(lambda(x)
			(and (string= (getf x :value) "text1")
			     (string= (getf x :type)
				      "http://isidorus/props/lit1")
			     (not (getf x :ID))
			     (string= (getf x :lang) "de")
			     (string= (getf x :datatype) *xml-string*)))
		    literals))
	  (is-true (find-if
		    #'(lambda(x)
			(and (string= (getf x :value) "text2")
			     (string= (getf x :type)
				      "http://isidorus/props/lit2")
			     (not (getf x :ID))
			     (string= (getf x :lang) "de")
			     (string= (getf x :datatype)
				      "http://base/absolute/dType1")))
		    literals))
	  (is-true (find-if
		    #'(lambda(x)
			(and (string= (getf x :value) "text3")
			     (string= (getf x :type)
				      "http://isidorus/props/lit3")
			     (not (getf x :ID))
			     (string= (getf x :lang) "en")
			     (string= (getf x :datatype)
				      "http://test-tm/base/first/dType2")))
		    literals))
	  (is-true (find-if
		    #'(lambda(x)
			(and (string= (getf x :value)
				      "<root><child></child></root>  ")
			     (string= (getf x :type)
				      "http://isidorus/props/lit4")
			     (not (getf x :ID))
			     (string= (getf x :lang) "de")
			     (string= (getf x :datatype)
				      "http://test-tm/base/first/dType2")))
		    literals))
	  (is-true (find-if
		    #'(lambda(x)
			(and (string= (getf x :value)
				      "<root><child>childText5</child>   </root>")
			     (string= (getf x :type)
				      "http://isidorus/props/lit5")
			     (string= (getf x :ID)
				      "http://test-tm/base/first#rdfID")
			     (string= (getf x :lang) "de")
			     (string= (getf x :datatype) *xml-string*)))
		    literals))
	  (is-true (find-if
		    #'(lambda(x)
			(and (string= (getf x :value) "   text6  abc ")
			     (string= (getf x :type)
				      "http://isidorus/props/lit6")
			     (not (getf x :ID))
			     (not (getf x :lang))
			     (string= (getf x :datatype) *xml-string*)))
		    literals)))))))


(test test-get-super-classes-of-node-content
  (let ((doc-1
	 (concatenate 'string "<rdf:Description xmlns:rdf=\"" *rdf-ns* "\" "
		      "xmlns:isi=\"" *rdf2tm-ns* "\" "
		      "xmlns:rdfs=\"" *rdfs-ns* "\" "
		      "xmlns:arcs=\"http://test/arcs/\" "
                      "xml:base=\"xml-base/first\" "
		      "rdf:about=\"resource\" rdf:type=\"attr-type\">"
		      "<rdfs:subClassOf rdf:ID=\"rdfID\" "
		      "rdf:resource=\"content-type-1\"/>"
		      "<rdfs:subClassOf /><!-- blank_node -->"
		      "<rdfs:subClassOf arcs:arc=\"literalArc\"/>"
		      "<rdfs:subClassOf rdf:parseType=\"Collection\" "
		      "          xml:base=\"http://xml-base/absolute/\">"
		      "<!-- blank_node that is a list -->"
		      "<rdf:Description rdf:about=\"c-about-type\"/>"
		      "<rdf:Description rdf:ID=\"c-id-type\"/>"
		      "<rdf:Description rdf:nodeID=\"c-nodeID-type\"/>"
		      "<rdf:Description/><!-- blank_node -->"
		      "</rdfs:subClassOf>"
		      "<rdfs:subClassOf rdf:ID=\"rdfID2\">"
		      "<rdf:Description rdf:about=\"c-about-type-2\"/>"
		      "</rdfs:subClassOf>"
		      "<rdfs:subClassOf>"
		      "<rdf:Description rdf:nodeID=\"c-nodeID-type-2\"/>"
		      "</rdfs:subClassOf>"
		      "<rdfs:subClassOf xml:base=\"http://new-base/\">"
		      "<rdf:Description rdf:ID=\"c-ID-type-2\"/>"
		      "</rdfs:subClassOf>"
		      "<rdfs:subClassOf rdf:ID=\"rdfID3\">"
		      "<rdf:Description/>"
		      "</rdfs:subClassOf>"
		      "<arcs:arc rdf:resource=\"anyArc\"/>"
		      "<rdfs:arc>"
		      "<rdf:Description rdf:about=\"anyResource\"/>"
		      "</rdfs:arc>"
		      "</rdf:Description>")))
    (let ((dom-1 (cxml:parse doc-1 (cxml-dom:make-dom-builder))))
      (is-true dom-1)
      (is (= (length (dom:child-nodes dom-1))))
      (let ((node (elt (dom:child-nodes dom-1) 0))
	    (tm-id "http://test-tm")
	    (xml-base "/base/initial"))
	(is-true node)
	(is-true (rdf-importer::parse-node node))
	(loop for property across (rdf-importer::child-nodes-or-text node)
	   do (is-true (rdf-importer::parse-property property 0)))
	(let ((super-classes (rdf-importer::get-super-classes-of-node-content
			      node tm-id xml-base)))
	  (is (= (length super-classes) 8))
	  (is-true
	   (find-if 
	    #'(lambda(x)
		(and
		 (string=
		  (getf x :psi)
		  "http://test-tm/base/initial/xml-base/first/content-type-1")
		 (string=
		  (getf x :topicid)
		  "http://test-tm/base/initial/xml-base/first/content-type-1")
		 (string=
		  (getf x :ID)
		  "http://test-tm/base/initial/xml-base/first#rdfID")))
	    super-classes))
	  (is-true (find-if
		    #'(lambda(x)
			(and
			 (string=
			  (getf x :topicid)
			  (concatenate 'string tm-id xml-base 
				       "/xml-base/first/c-about-type-2"))
			 (string=
			  (getf x :psi)
			  (concatenate 'string tm-id xml-base 
				       "/xml-base/first/c-about-type-2"))
			 (string= (getf x :ID)
				  (concatenate 'string tm-id xml-base 
					       "/xml-base/first#rdfID2"))))
		    super-classes))
	  (is-true (find-if
		    #'(lambda(x)
			(and (string= (getf x :topicid) "c-nodeID-type-2")
			     (not (getf x :psi))
			     (not (getf x :ID))))
		    super-classes))
	  (is-true (find-if
		    #'(lambda(x)
			(and (string= (getf x :topicid)
				      "http://new-base#c-ID-type-2")
			     (string= (getf x :psi)
				      "http://new-base#c-ID-type-2")
			     (not (getf x :ID))))
		    super-classes))
	  (is (= (count-if  #'(lambda(x) (> (length (getf x :topicid)) 0))
			    super-classes)
		 8))
	  (is-true (find-if
		    #'(lambda(x)
			(and
			 (string=
			  (getf x :ID)
			  "http://test-tm/base/initial/xml-base/first#rdfID3")
			 (not (getf x :psi))
			 (> (length (getf x :topicid)))))
		    super-classes))
	  (dom:append-child (elt (rdf-importer::child-nodes-or-text node) 1)
			    (dom:create-text-node dom-1 "new text"))
	  (signals error (rdf-importer::parse-property
			  (elt (rdf-importer::child-nodes-or-text node) 1) 0)))))))


(test test-get-associations-of-node-content
  (let ((doc-1
	 (concatenate 'string "<rdf:Description xmlns:rdf=\"" *rdf-ns* "\" "
		      "xmlns:isi=\"" *rdf2tm-ns* "\" "
		      "xmlns:rdfs=\"" *rdfs-ns* "\" "
		      "xmlns:arcs=\"http://test/arcs/\" "
                      "xml:base=\"http://xml-base/first\" "
		      "rdf:about=\"resource\" rdf:type=\"attr-type\">"
		      "<rdf:type rdf:resource=\"anyType\" />"
		      "<rdf:type>   </rdf:type>"
		      "<rdfs:subClassOf rdf:nodeID=\"anyClass\" />"
		      "<rdfs:subClassOf>   </rdfs:subClassOf>"
		      "<rdf:unknown rdf:resource=\"assoc-1\"/>"
		      "<rdfs:unknown rdf:type=\"assoc-2-type\">"
		      "   </rdfs:unknown>"
		      "<arcs:arc1 rdf:ID=\"rdfID-1\" "
		      "rdf:nodeID=\"arc1-nodeID\"/>"
		      "<arcs:arc2 rdf:parseType=\"Collection\">"
		      "<rdf:Description rdf:about=\"col\" />"
		      "</arcs:arc2>"
		      "<arcs:arc3 rdf:parseType=\"Resource\" "
		      "rdf:ID=\"rdfID-2\" />"
		      "<arcs:lit rdf:parseType=\"Literal\" />"
		      "<arcs:arc4 arcs:arc5=\"text-arc5\" />"
		      "<arcs:arc6 rdf:ID=\"rdfID-3\">"
		      "<rdf:Description rdf:about=\"con-1\" />"
		      "</arcs:arc6>"
		      "<arcs:arc7>"
		      "<rdf:Description rdf:nodeID=\"con-2\" />"
		      "</arcs:arc7>"
		      "<arcs:arc8>"
		      "<rdf:Description rdf:ID=\"rdfID-4\" />"
		      "</arcs:arc8>"
		      "<arcs:arc9 rdf:ID=\"rdfID-5\" xml:base=\"add\">"
		      "<rdf:Description />"
		      "</arcs:arc9>"
		      "<rdfs:type rdf:resource=\"assoc-11\">   </rdfs:type>"
		      "<rdf:subClassOf rdf:nodeID=\"assoc-12\" />"
		      "</rdf:Description>")))
    (let ((dom-1 (cxml:parse doc-1 (cxml-dom:make-dom-builder)))
	  (tm-id "http://test-tm"))
      (is-true dom-1)
      (is (= (length (dom:child-nodes dom-1)) 1))
      (let ((node (elt (dom:child-nodes dom-1) 0)))
	(loop for property across (rdf-importer::child-nodes-or-text node)
	   do (is-true (rdf-importer::parse-property property 0)))
	(let ((associations
	       (rdf-importer::get-associations-of-node-content node tm-id nil)))
	  (is (= (length associations) 12))
	  (is-true (find-if
		    #'(lambda(x)
			(and (string= (getf x :type)
				      (concatenate 'string *rdf-ns* "unknown"))
			     (string= (getf x :topicid)
				      "http://xml-base/first/assoc-1")
			     (string= (getf x :psi)
				      "http://xml-base/first/assoc-1")
			     (not (getf x :ID))))
		    associations))
	  (is-true (find-if
		    #'(lambda(x)
			(and (string= (getf x :type) "http://test/arcs/arc1")
			     (string= (getf x :ID) "http://xml-base/first#rdfID-1")
			     (string= (getf x :topicid) "arc1-nodeID")
			     (not (getf x :psi))))
		    associations))
	  (is-true (find-if
		    #'(lambda(x)
			(and (string= (getf x :type) "http://test/arcs/arc2")
			     (> (length (getf x :topicid)) 0)
			     (not (getf x :psi))
			     (not (getf x :ID))))
		    associations))
	  (is-true (find-if
		    #'(lambda(x)
			(and (string= (getf x :type) "http://test/arcs/arc3")
			     (string= (getf x :ID)
				      "http://xml-base/first#rdfID-2")
			     (not (getf x :psi))
			     (> (length (getf x :topicid)) 0)))
		    associations))
	  (is-true (find-if
		    #'(lambda(x)
			(and (string= (getf x :type) "http://test/arcs/arc4")
			     (not (getf x :ID))
			     (not (getf x :psi))
			     (> (length (getf x :topicid)) 0)))
		    associations))
	  (is-true (find-if
		    #'(lambda(x)
			(and (string= (getf x :type) "http://test/arcs/arc4")
			     (not (getf x :ID))
			     (not (getf x :psi))
			     (> (length (getf x :topicid)) 0)))
		    associations))
	  (is-true (find-if
		    #'(lambda(x)
			(and (string= (getf x :type) "http://test/arcs/arc6")
			     (string= (getf x :ID)
				      "http://xml-base/first#rdfID-3")
			     (string= (getf x :topicid)
				      "http://xml-base/first/con-1")
			     (string= (getf x :psi)
				      "http://xml-base/first/con-1")))
		    associations))
	  (is-true (find-if
		    #'(lambda(x)
			(and (string= (getf x :type) "http://test/arcs/arc7")
			     (not (getf x :ID))
			     (string= (getf x :topicid) "con-2")
			     (not (getf x :psi))))
		    associations))
	  (is-true (find-if
		    #'(lambda(x)
			(and (string= (getf x :type) "http://test/arcs/arc8")
			     (not (getf x :ID))
			     (string= (getf x :topicid)
				      "http://xml-base/first#rdfID-4")
			     (string= (getf x :psi)
				      "http://xml-base/first#rdfID-4")))
		    associations))
	  (is-true (find-if
		    #'(lambda(x)
			(and (string= (getf x :type) "http://test/arcs/arc9")
			     (string= (getf x :ID)
				      "http://xml-base/first/add#rdfID-5")
			     (not (getf x :psi))
			     (> (length (getf x :topicid)))))
		    associations))
	  (is-true (find-if
		    #'(lambda(x)
			(and (string= (getf x :type)
				      (concatenate 'string *rdfs-ns* "type"))
			     (not (getf x :ID))
			     (string= (getf x :psi)
				      "http://xml-base/first/assoc-11")
			     (string= (getf x :topicid)
				      "http://xml-base/first/assoc-11")))
		    associations))
	  (is-true (find-if
		    #'(lambda(x)
			(and (string= (getf x :type)
				      (concatenate 'string *rdf-ns*
						   "subClassOf"))
			     (not (getf x :ID))
			     (not (getf x :psi))
			     (string= (getf x :topicid) "assoc-12")))
		    associations)))))))


(test test-parse-properties-of-node
  (let ((doc-1
	 (concatenate 'string "<rdf:Description xmlns:rdf=\"" *rdf-ns* "\" "
		      "xmlns:arcs=\"http://test/arcs/\" "
                      "xml:base=\"http://xml-base/first\" "
		      "rdf:about=\"resource\" rdf:type=\"attr-type\" "
		      "rdf:li=\"li-attr\">"
		      "<rdf:li rdf:resource=\"anyType\" />"
		      "<rdf:li> text-1  </rdf:li>"
		      "<rdf:li rdf:nodeID=\"anyClass\" />"
		      "<rdf:li>    </rdf:li>"
		      "<rdf:li rdf:resource=\"assoc-1\"/>"
		      "<rdf:li rdf:type=\"assoc-2-type\">"
		      "   </rdf:li>"
		      "<rdf:li rdf:parseType=\"Literal\" > text-3</rdf:li>"
		      "<rdf:_123 arcs:arc5=\"text-arc5\"/>"
		      "<rdf:arc6 rdf:ID=\"rdfID-3\"> text-4 </rdf:arc6>"
		      "<rdf:arcs rdf:ID=\"rdfID-4\" xml:lang=\" \">"
		      "text-5</rdf:arcs>"
		      "</rdf:Description>")))
    (let ((dom-1 (cxml:parse doc-1 (cxml-dom:make-dom-builder)))
	  (tm-id "http://test-tm"))
      (setf rdf-importer::*_n-map* nil)
      (is-true dom-1)
      (is (= (length (dom:child-nodes dom-1))))
      (let ((node (elt (dom:child-nodes dom-1) 0)))
	(is-true (rdf-importer::parse-node node))
	(is-true (rdf-importer::parse-properties-of-node node))
	(is (= (length rdf-importer::*_n-map*) 8))
	(dotimes (iter (length rdf-importer::*_n-map*))
	  (is-true (find-if
		    #'(lambda(x)
			(string= (getf x :type)
				 (concatenate
				  'string *rdf-ns* "_"
				  (write-to-string (+ 1 iter)))))
		    rdf-importer::*_n-map*)))
	(let ((assocs
	       (rdf-importer::get-associations-of-node-content node tm-id nil))
	      (content-literals
	       (rdf-importer::get-literals-of-node-content node tm-id nil "de"))
	      (attr-literals
	       (rdf-importer::get-literals-of-node node nil)))
	  (is (= (length assocs) 5))
	  (is (= (length content-literals) 5))
	  (is (= (length attr-literals) 1))
	  (is-true (find-if #'(lambda(x)
				(and (string= (getf x :type)
					      (concatenate 'string *rdf-ns* "_1"))
				     (not (getf x :lang))
				     (string= (getf x :value) "li-attr")
				     (not (getf x :lang))
				     (not (getf x :ID))))
			    attr-literals))
	  (is-true (find-if #'(lambda(x)
				(and (string= (getf x :topicid)
					      "http://xml-base/first/anyType")
				     (string= (getf x :psi)
					      "http://xml-base/first/anyType")
				     (string= (getf x :type)
					      (concatenate 'string *rdf-ns* "_2"))
				     (not (getf x :ID))))
			    assocs))
	  (is-true (find-if #'(lambda(x)
				(and (string= (getf x :value) " text-1  ")
				     (string= (getf x :lang) "de")
				     (string= (getf x :datatype) *xml-string*)
				     (string= (getf x :type)
					      (concatenate 'string *rdf-ns* "_3"))
				     (not (getf x :ID))))
			    content-literals))
	  (is-true (find-if #'(lambda(x)
				(and (string= (getf x :topicid) "anyClass")
				     (not (getf x :psi))
				     (string= (getf x :type)
					      (concatenate 'string *rdf-ns* "_4"))
				     (not (getf x :ID))))
			    assocs))
	  (is-true (find-if #'(lambda(x)
				(and (string= (getf x :value) "    ")
				     (string= (getf x :type)
					      (concatenate 'string *rdf-ns* "_5"))
				     (string= (getf x :datatype) *xml-string*)
				     (string= (getf x :lang) "de")
				     (not (getf x :ID))))
			    content-literals))
	  (is-true (find-if #'(lambda(x)
				(and (string= (getf x :topicid)
					      "http://xml-base/first/assoc-1")
				     (string= (getf x :psi)
					      "http://xml-base/first/assoc-1")
				     (string= (getf x :type)
					      (concatenate 'string *rdf-ns* "_6"))
				     (not (getf x :ID))))
			    assocs))
	  (is-true (find-if #'(lambda(x)
				(and (> (length (getf x :topicid)) 0)
				     (not (getf x :psi))
				     (string= (getf x :type)
					      (concatenate 'string *rdf-ns* "_7"))
				     (not (getf x :ID))))
			    assocs))
	  (is-true (find-if #'(lambda(x)
				(and (string= (getf x :value) " text-3")
				     (string= (getf x :lang) "de")
				     (string= (getf x :datatype) *xml-string*)
				     (string= (getf x :type)
					      (concatenate 'string *rdf-ns* "_8"))
				     (not (getf x :ID))))
			    content-literals))
	  (is-true (find-if #'(lambda(x)
				(and (string= (getf x :value) " text-4 ")
				     (string= (getf x :lang) "de")
				     (string= (getf x :datatype) *xml-string*)
				     (string=
				      (getf x :type)
				      (concatenate 'string *rdf-ns* "arc6"))
				     (string= 
				      (getf x :ID)
				      "http://xml-base/first#rdfID-3")))
			    content-literals))
	  (is-true (find-if #'(lambda(x)
				(and (string= (getf x :value) "text-5")
				     (string= (getf x :lang) nil)
				     (string= (getf x :datatype) *xml-string*)
				     (string=
				      (getf x :type)
				      (concatenate 'string *rdf-ns* "arcs"))
				     (string= 
				      (getf x :ID)
				      "http://xml-base/first#rdfID-4")))
			    content-literals)))
	(rdf-importer::remove-node-properties-from-*_n-map* node)
	(is (= (length rdf-importer::*_n-map*) 0))))))


(test test-import-node-1
  "Tests the function import-node non-recursively."
  (let ((db-dir "data_base")
	(tm-id "http://test-tm/")
	(revision-1 100)
	(revision-2 200)
	(revision-3 300)
	(document-id "doc-id")
	(doc-1
	 (concatenate 'string "<rdf:RDF xmlns:rdf=\"" *rdf-ns* "\" "
		      "xmlns:arcs=\"http://test/arcs/\" "
		      "xmlns:rdfs=\"" *rdfs-ns* "\">"
		      "<rdf:Description rdf:about=\"first-node\">"
		      "<rdf:type rdf:resource=\"first-type\" />"
		      "</rdf:Description>"
		      "<rdf:Description rdf:type=\"second-type\" "
		      "rdf:nodeID=\"second-node\">"
		      "<rdfs:subClassOf>"
		      "<rdf:Description rdf:ID=\"third-node\" />"
		      "</rdfs:subClassOf>"
		      "</rdf:Description>"
		      "<rdf:Description arcs:arc1=\"arc-1\">"
		      "<arcs:arc2 rdf:datatype=\"dt\">arc-2</arcs:arc2>"
		      "</rdf:Description>"
		      "<rdf:Description rdf:about=\"fourth-node\">"
		      "<arcs:arc3 rdf:parseType=\"Literal\"><root>"
		      "<content type=\"anyContent\">content</content>"
		      "</root></arcs:arc3>"
		      "</rdf:Description>"
		      "<rdf:Description rdf:ID=\"fifth-node\">"
		      "<arcs:arc4 rdf:parseType=\"Resource\">"
		      "<arcs:arc5 rdf:resource=\"arc-5\" />"
		      "</arcs:arc4>"
		      "</rdf:Description>"
		      "</rdf:RDF>")))
    (let ((dom-1 (cxml:parse doc-1 (cxml-dom:make-dom-builder))))
      (is-true dom-1)
      (is (= (length (dom:child-nodes dom-1)) 1))
      (let ((rdf-node (elt (dom:child-nodes dom-1) 0)))
	(is (= (length (dom:child-nodes rdf-node)) 5))
	(let ((node (elt (dom:child-nodes rdf-node) 0)))
	  (rdf-init-db :db-dir db-dir :start-revision revision-1)
	  (rdf-importer::import-node node tm-id revision-2
				     :document-id document-id)
	  (is (= (length (elephant:get-instances-by-class 'd:TopicC)) 20))
	  (let ((first-node (get-item-by-id "http://test-tm/first-node"
					    :xtm-id document-id))
		(first-type (get-item-by-id "http://test-tm/first-type"
					    :xtm-id document-id)))
	    (is-true first-node)
	    (is (= (length (d::versions first-node)) 1))
	    (is (= (d::start-revision (first (d::versions first-node)))
		   revision-2))
	    (is (= (d::end-revision (first (d::versions first-node))) 0))
	    (is-true first-type)
	    (is (= (length (d:player-in-roles first-node)) 1))
	    (is (= (length (d:player-in-roles first-type)) 1))
	    (let ((instance-role
		   (first (d:player-in-roles first-node)))
		  (type-role
		   (first (d:player-in-roles first-type)))
		  (type-assoc
		   (d:parent (first (d:player-in-roles first-node)))))
	      (is (= (length (d::versions type-assoc)) 1))
	      (is (= (d::start-revision (first (d::versions type-assoc)))
		     revision-2))
	      (is (eql (d:instance-of instance-role)
		       (d:get-item-by-psi *instance-psi*)))
	      (is (eql (d:instance-of type-role)
		       (d:get-item-by-psi *type-psi*)))
	      (is (eql (d:instance-of type-assoc)
		       (d:get-item-by-psi *type-instance-psi*)))
	      (is (= (length (d:roles type-assoc)) 2))
	      (is (= (length (d:psis first-node)) 1))
	      (is (= (length (d:psis first-type)) 1))
	      (is (string= (d:uri (first (d:psis first-node)))
			   "http://test-tm/first-node"))
	      (is (string= (d:uri (first (d:psis first-type)))
			   "http://test-tm/first-type"))
	      (is (= (length (elephant:get-instances-by-class 'd:OccurrenceC))))
	      (is (= (length (elephant:get-instances-by-class 'd:NameC))))
	      (is (= (length (elephant:get-instances-by-class 'd:VariantC)))))
	    (dotimes (iter (length (dom:child-nodes rdf-node)))
	      (rdf-importer::import-node (elt (dom:child-nodes rdf-node) iter)
					 tm-id revision-3
					 :document-id document-id))
	    (let ((first-node (get-item-by-id "http://test-tm/first-node"
					      :xtm-id document-id))
		  (first-type (get-item-by-id "http://test-tm/first-type"
					      :xtm-id document-id))
		  (second-node (get-item-by-id "second-node"
					       :xtm-id document-id))
		  (second-type (get-item-by-id "http://test-tm/second-type"
					       :xtm-id document-id))
		  (third-node (get-item-by-id "http://test-tm#third-node"
					      :xtm-id document-id)))
	      (is-true second-node)
	      (is-false (d:psis second-node))
	      (is-false (d:occurrences second-node))
	      (is-false (d:names second-node))
	      (is-true first-node)
	      (is (= (length (d::versions first-node)) 2))
	      (is-true (find-if #'(lambda(x)
				    (and (= (d::start-revision x) revision-2)
					 (= (d::end-revision x) revision-3)))
				(d::versions first-node)))
	      (is-true (find-if #'(lambda(x)
				    (and (= (d::start-revision x) revision-3)
					 (= (d::end-revision x) 0)))
				(d::versions first-node)))
	      (let ((instance-role
		     (first (d:player-in-roles first-node)))
		    (type-role
		     (first (d:player-in-roles first-type)))
		    (type-assoc
		     (d:parent (first (d:player-in-roles first-node))))
		    (type-topic (get-item-by-psi *type-psi*))
		    (instance-topic (get-item-by-psi *instance-psi*))
		    (type-instance-topic (get-item-by-psi *type-instance-psi*))
		    (supertype-topic (get-item-by-psi *supertype-psi*))
		    (subtype-topic (get-item-by-psi *subtype-psi*))
		    (supertype-subtype-topic
		     (get-item-by-psi *supertype-subtype-psi*))
		    (arc2-occurrence (elephant:get-instance-by-value
				      'd:OccurrenceC 'd:charvalue "arc-2"))
		    (arc3-occurrence
		     (elephant:get-instance-by-value
		      'd:OccurrenceC 'd:charvalue
		      "<root><content type=\"anyContent\">content</content></root>"))
		    (fifth-node (d:get-item-by-id "http://test-tm#fifth-node"
						  :xtm-id document-id)))
		(is (eql (d:instance-of instance-role)
			 (d:get-item-by-psi *instance-psi*)))
		(is (eql (d:instance-of type-role)
			 (d:get-item-by-psi *type-psi*)))
		(is (eql (d:instance-of type-assoc)
			 (d:get-item-by-psi *type-instance-psi*)))
		(is (= (length (d:roles type-assoc)) 2))
		(is (= (length (d:psis first-node)) 1))
		(is (= (length (d:psis first-type)) 1))
		(is (= (length (d::versions type-assoc)) 1))
		(is (= (length (d:player-in-roles second-node)) 2))
		(is-true (find-if
			  #'(lambda(x)
			      (and (eql (d:instance-of x) instance-topic)
				   (eql (d:instance-of (d:parent x) )
					type-instance-topic)))
			  (d:player-in-roles second-node)))
		(is-true (find-if
			  #'(lambda(x)
			      (and (eql (d:instance-of x) subtype-topic)
				   (eql (d:instance-of (d:parent x) )
					supertype-subtype-topic)))
			  (d:player-in-roles second-node)))
		(is-true (find-if
			  #'(lambda(x)
			      (and (eql (d:instance-of x) type-topic)
				   (eql (d:instance-of (d:parent x) )
					type-instance-topic)))
			  (d:player-in-roles second-type)))
		(is-true (find-if
			  #'(lambda(x)
			      (and (eql (d:instance-of x) supertype-topic)
				   (eql (d:instance-of (d:parent x) )
					supertype-subtype-topic)))
			  (d:player-in-roles third-node)))
		(is-true arc2-occurrence)
		(is (string= (d:datatype arc2-occurrence) "http://test-tm/dt"))
		(is-false (d:psis (d:topic arc2-occurrence)))
		(is (= (length (d::versions (d:topic arc2-occurrence))) 1))
		(is (= (d::start-revision
			(first (d::versions (d:topic arc2-occurrence))))
		       revision-3))
		(is (= (d::end-revision
			(first (d::versions (d:topic arc2-occurrence)))) 0))
		(is-true arc3-occurrence)
		(is (= (length (d:psis (d:topic arc3-occurrence)))))
		(is (string= (d:uri (first (d:psis (d:topic arc3-occurrence))))
			     "http://test-tm/fourth-node"))
		(is (string= (d:datatype arc3-occurrence)
			     *xml-string*))
		(is-true fifth-node)
		(is (= (length (d:psis fifth-node)) 1))
		(is (string= (d:uri (first (d:psis fifth-node)))
			     "http://test-tm#fifth-node"))
		(is-false (d:occurrences fifth-node))
		(is-false (d:names fifth-node))
		(is (= (length (d:player-in-roles fifth-node))))
		(let ((assoc (d:parent (first (d:player-in-roles
					       fifth-node)))))
		  (is-true assoc)
		  (let ((object-role
			 (find-if
			  #'(lambda(role)
			      (eql (d:instance-of role)
				   (d:get-item-by-psi *rdf2tm-object*)))
			  (d:roles assoc)))
			(subject-role
			 (find-if
			  #'(lambda(role)
			      (eql (d:instance-of role)
				   (d:get-item-by-psi *rdf2tm-subject*)))
			  (d:roles assoc))))
		    (is-true object-role)
		    (is-true subject-role)
		    (is (eql (d:player subject-role) fifth-node))
		    (is-false (d:psis (d:player object-role))))))))))))
  (elephant:close-store))

  
(test test-import-node-reification
  "Tests the function import-node non-recursively. Especially the reification
   of association- and occurrence-arcs."
  (let ((db-dir "data_base")
	(tm-id "http://test-tm/")
	(revision-1 100)
	(document-id "doc-id")
	(doc-1
	 (concatenate 'string "<rdf:RDF xmlns:rdf=\"" *rdf-ns* "\" "
		      "xmlns:arcs=\"http://test/arcs/\" "
		      "xmlns:rdfs=\"" *rdfs-ns* "\">"
		      "<rdf:Description rdf:about=\"first-node\">"
		      "<arcs:arc1 rdf:ID=\"reification-1\">"
		      "<rdf:Description rdf:about=\"second-node\" />"
		      "</arcs:arc1>"
		      "</rdf:Description>"
		      "<rdf:Description rdf:ID=\"#reification-1\">"
		      "<arcs:arc2 rdf:resource=\"third-node\"/>"
		      "</rdf:Description>"
		      "<rdf:Description rdf:nodeID=\"fourth-node\">"
		      "<arcs:arc3 rdf:ID=\"reification-2\" rdf:datatype=\"dt\">"
		      "occurrence data"
		      "</arcs:arc3>"
		      "</rdf:Description>"
		      "<rdf:Description rdf:ID=\"#reification-2\">"
		      "<arcs:arc4 rdf:resource=\"fifth-node\" />"
		      "</rdf:Description>"
		      "</rdf:RDF>")))
    (let ((dom-1 (cxml:parse doc-1 (cxml-dom:make-dom-builder))))
      (is-true dom-1)
      (is (= (length (dom:child-nodes dom-1)) 1))
      (let ((rdf-node (elt (dom:child-nodes dom-1) 0)))
	(is (= (length (dom:child-nodes rdf-node)) 4))
	(rdf-init-db :db-dir db-dir :start-revision revision-1)
	(dotimes (iter (length (dom:child-nodes rdf-node)))
	  (rdf-importer::import-node (elt (dom:child-nodes rdf-node) iter)
				     tm-id revision-1
				     :document-id document-id))
	(let ((reification-1 (d:get-item-by-id "http://test-tm#reification-1"
					     :xtm-id document-id))
	      (reification-2 (d:get-item-by-id "http://test-tm#reification-2"
					       :xtm-id document-id))
	      (first-node (d:get-item-by-id "http://test-tm/first-node"
					  :xtm-id document-id))
	      (second-node (d:get-item-by-id "http://test-tm/second-node"
					   :xtm-id document-id))
	      (third-node (d:get-item-by-id "http://test-tm/third-node"
					  :xtm-id document-id))
	      (fourth-node (d:get-item-by-id "fourth-node"
					     :xtm-id document-id))
	      (fifth-node (d:get-item-by-id "http://test-tm/fifth-node"
					    :xtm-id document-id))
	      (arc1 (d:get-item-by-id "http://test/arcs/arc1"
				    :xtm-id document-id))
	      (arc2 (d:get-item-by-id "http://test/arcs/arc2"
				    :xtm-id document-id))
	      (arc3 (d:get-item-by-id "http://test/arcs/arc3"
				      :xtm-id document-id))
	      (arc4 (d:get-item-by-id "http://test/arcs/arc4"
				      :xtm-id document-id))
	      (statement (d:get-item-by-psi *rdf-statement*))
	      (object (d:get-item-by-psi *rdf-object*))
	      (subject (d:get-item-by-psi *rdf-subject*))
	      (predicate (d:get-item-by-psi *rdf-predicate*))
	      (type (d:get-item-by-psi *type-psi*))
	      (instance (d:get-item-by-psi *instance-psi*))
	      (type-instance (d:get-item-by-psi *type-instance-psi*))
	      (isi-subject (d:get-item-by-psi *rdf2tm-subject*))
	      (isi-object (d:get-item-by-psi *rdf2tm-object*)))
	  (is (= (length (d:psis reification-1)) 1))
	  (is (string= (d:uri (first (d:psis reification-1)))
		       "http://test-tm#reification-1"))
	  (is (= (length (d:psis reification-2)) 1))
	  (is (string= (d:uri (first (d:psis reification-2)))
		       "http://test-tm#reification-2"))
	  (is (= (length (d:psis first-node)) 1))
	  (is (string= (d:uri (first (d:psis first-node)))
		       "http://test-tm/first-node"))
	  (is (= (length (d:psis second-node)) 1))
	  (is (string= (d:uri (first (d:psis second-node)))
		       "http://test-tm/second-node"))
	  (is (= (length (d:psis third-node)) 1))
	  (is (string= (d:uri (first (d:psis third-node)))
		       "http://test-tm/third-node"))
	  (is (= (length (d:psis fourth-node)) 0))
	  (is (= (length (d:psis fifth-node)) 1))
	  (is (string= (d:uri (first (d:psis fifth-node)))
		       "http://test-tm/fifth-node"))
	  (is (= (length (d:psis arc1)) 1))
	  (is (string= (d:uri (first (d:psis arc1)))
		       "http://test/arcs/arc1"))
	  (is (= (length (d:psis arc2))))
	  (is (string= (d:uri (first (d:psis arc2)))
		       "http://test/arcs/arc2"))
	  (is (= (length (d:psis arc3))))
	  (is (string= (d:uri (first (d:psis arc3)))
		       "http://test/arcs/arc3"))
	  (is (= (length (d:psis arc4))))
	  (is (string= (d:uri (first (d:psis arc4)))
		       "http://test/arcs/arc4"))
	  (is-true statement)
	  (is-true object)
	  (is-true subject)
	  (is-true predicate)
	  (is-true type)
	  (is-true instance)
	  (is-true type-instance)
	  (is (= (length (d:player-in-roles first-node)) 2))
	  (is-true (find-if #'(lambda(x)
				(and (eql (d:instance-of x) isi-subject)
				     (eql (d:instance-of (d:parent x)) arc1)))
			    (d:player-in-roles first-node)))
	  (is-true (find-if #'(lambda(x)
				(and (eql (d:instance-of x) isi-object)
				     (eql (d:instance-of (d:parent x))
					  subject)))
			    (d:player-in-roles first-node)))
	  (is (= (length (d:player-in-roles second-node)) 2))
	  (is-true (find-if #'(lambda(x)
				(and (eql (d:instance-of x) isi-object)
				     (eql (d:instance-of (d:parent x)) arc1)))
			    (d:player-in-roles second-node)))
	  (is-true (find-if #'(lambda(x)
				(and (eql (d:instance-of x) isi-object)
				     (eql (d:instance-of (d:parent x))
					  object)))
			    (d:player-in-roles second-node)))
	  (is (= (length (d:player-in-roles statement)) 2))
	  (is-true (find-if #'(lambda(x)
				(and (eql (d:instance-of x) type)
				     (eql (d:instance-of (d:parent x))
					  type-instance)))
			    (d:player-in-roles statement)))
	  (is (= (length (d:player-in-roles arc1)) 1))
	  (is-true (find-if #'(lambda(x)
				(and (eql (d:instance-of x) isi-object)
				     (eql (d:instance-of (d:parent x))
					  predicate)))
			    (d:player-in-roles arc1)))
	  (is (= (length (d:player-in-roles third-node)) 1))
	  (is-true (find-if #'(lambda(x)
				(and (eql (d:instance-of x) isi-object)
				     (eql (d:instance-of (d:parent x))
					  arc2)))
			    (d:player-in-roles third-node)))
	  (is (= (length (d:player-in-roles reification-1)) 5))
	  (is-true (find-if #'(lambda(x)
				(and (eql (d:instance-of x) isi-subject)
				     (eql (d:instance-of (d:parent x))
					  subject)))
			    (d:player-in-roles reification-1)))
	  (is-true (find-if #'(lambda(x)
				(and (eql (d:instance-of x) isi-subject)
				     (eql (d:instance-of (d:parent x))
					  object)))
			    (d:player-in-roles reification-1)))
	  (is-true (find-if #'(lambda(x)
				(and (eql (d:instance-of x) instance)
				     (eql (d:instance-of (d:parent x))
					  type-instance)))
			    (d:player-in-roles reification-1)))
	  (is-true (find-if #'(lambda(x)
				(and (eql (d:instance-of x) isi-subject)
				     (eql (d:instance-of (d:parent x))
					  object)))
			    (d:player-in-roles reification-1)))
	  (is-true (find-if #'(lambda(x)
				(and (eql (d:instance-of x) isi-subject)
				     (eql (d:instance-of (d:parent x))
					  predicate)))
			    (d:player-in-roles reification-1)))
	  (is-true (find-if #'(lambda(x)
				(and (eql (d:instance-of x) isi-subject)
				     (eql (d:instance-of (d:parent x))
					  arc2)))
			    (d:player-in-roles reification-1)))
	  (is (= (length (d:occurrences fourth-node)) 1))
	  (is (string= (d:charvalue (first (d:occurrences fourth-node)))
		       "occurrence data"))
	  (is (string= (d:datatype (first (d:occurrences fourth-node)))
		       "http://test-tm/dt"))
	  (is (eql (d:instance-of (first (d:occurrences fourth-node)))
		   arc3))
	  (is (= (length (d:player-in-roles fourth-node)) 1))
	  (is-true (find-if #'(lambda(x)
				(and (eql (d:instance-of x) isi-object)
				     (eql (d:instance-of (d:parent x))
					  subject)))
			    (d:player-in-roles fourth-node)))
	  (is (= (length (d:player-in-roles arc3)) 1))
	  (is-true (find-if #'(lambda(x)
				(and (eql (d:instance-of x) isi-object)
				     (eql (d:instance-of (d:parent x))
					  predicate)))
			    (d:player-in-roles arc3)))
	  (is (= (length (d:player-in-roles fifth-node)) 1))
	  (is-true (find-if #'(lambda(x)
				(and (eql (d:instance-of x) isi-object)
				     (eql (d:instance-of (d:parent x))
					  arc4)))
			    (d:player-in-roles fifth-node)))
	  (is (= (length (d:occurrences reification-2)) 1))
	  (is (string= (d:charvalue (first (d:occurrences reification-2)))
		       "occurrence data"))
	  (is (string= (d:datatype (first (d:occurrences reification-2)))
		       "http://test-tm/dt"))
	  (is (= (length (d:player-in-roles reification-2)) 4))
	  (is-true (find-if #'(lambda(x)
				(and (eql (d:instance-of x) isi-subject)
				     (eql (d:instance-of (d:parent x))
					  subject)))
			    (d:player-in-roles reification-2)))
	  (is-true (find-if #'(lambda(x)
				(and (eql (d:instance-of x) isi-subject)
				     (eql (d:instance-of (d:parent x))
					  predicate)))
			    (d:player-in-roles reification-2)))
	  (is-true (find-if #'(lambda(x)
				(and (eql (d:instance-of x) isi-subject)
				     (eql (d:instance-of (d:parent x))
					  arc4)))
			    (d:player-in-roles reification-2)))
	  (is-true (find-if #'(lambda(x)
				(and (eql (d:instance-of x) instance)
				     (eql (d:instance-of (d:parent x))
					  type-instance)))
			    (d:player-in-roles reification-2)))
	  (elephant:close-store))))))


(test test-import-dom
  "Tests the function import-node when used recursively."
  (let ((db-dir "data_base")
	(tm-id "http://test-tm/")
	(revision-1 100)
	(document-id "doc-id")
	(doc-1
	 (concatenate 'string "<rdf:RDF xmlns:rdf=\"" *rdf-ns* "\" "
		      "xmlns:arcs=\"http://test/arcs/\" "
		      "xmlns:rdfs=\"" *rdfs-ns* "\">"
		      "<rdf:Description1 rdf:about=\"first-node\">"
		      "<rdf:type rdf:nodeID=\"second-node\"/>"
		      "<arcs:arc1 rdf:resource=\"third-node\"/>"
		      "<arcs:arc2 rdf:datatype=\"long\">123</arcs:arc2>"
		      "<arcs:arc3>"
		      "<rdf:Description3>"
		      "<arcs:arc4 rdf:parseType=\"Collection\">"
		      "<rdf:Description4 rdf:about=\"item-1\"/>"
		      "<rdf:Description5 rdf:about=\"item-2\">"
		      "<arcs:arc5 rdf:parseType=\"Resource\">"
		      "<arcs:arc7 rdf:resource=\"fourth-node\"/>"
		      "<arcs:arc8 rdf:parseType=\"Collection\" />"
		      "</arcs:arc5>"
		      "</rdf:Description5>"
		      "</arcs:arc4>"
		      "</rdf:Description3>"
		      "</arcs:arc3>"
		      "</rdf:Description1>"
		      "<rdf:Description2 rdf:nodeID=\"second-node\" />"
		      "</rdf:RDF>")))
	(let ((dom-1 (cxml:parse doc-1 (cxml-dom:make-dom-builder))))
	  (is-true dom-1)
	  (is (= (length (dom:child-nodes dom-1)) 1))
	  (rdf-init-db :db-dir db-dir :start-revision revision-1)
	  (let ((rdf-node (elt (dom:child-nodes dom-1) 0)))
	    (is (= (length (dom:child-nodes rdf-node)) 2))
	    (rdf-importer::import-dom rdf-node revision-1 :tm-id tm-id
				      :document-id document-id)))))



(defun run-rdf-importer-tests()
  (it.bese.fiveam:run! 'test-get-literals-of-node)
  (it.bese.fiveam:run! 'test-parse-node)
  (it.bese.fiveam:run! 'test-get-literals-of-property)
  (it.bese.fiveam:run! 'test-parse-property)
  (it.bese.fiveam:run! 'test-get-types)
  (it.bese.fiveam:run! 'test-get-literals-of-content)
  (it.bese.fiveam:run! 'test-get-super-classes-of-node-content)
  (it.bese.fiveam:run! 'test-get-associations-of-node-content)
  (it.bese.fiveam:run! 'test-parse-properties-of-node)
  (it.bese.fiveam:run! 'test-import-node-1)
  (it.bese.fiveam:run! 'test-import-node-reification)
  (it.bese.fiveam:run! 'test-import-dom))