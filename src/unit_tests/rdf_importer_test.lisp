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
		*xml-string*)
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
	   :test-parse-properties-of-node))

(declaim (optimize (debug 3) (speed 0) (safety 3) (space 0) (compilation-speed 0)))

(in-package :rdf-importer-test)


(def-suite rdf-importer-test
     :description "tests  various key functions of the importer")

(in-suite rdf-importer-test)


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
		       :value (rdf-importer::get-type-of-node-name node)
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
			(and (string= (getf x :value) 
				      (concatenate
				       'string *rdf-ns* "anyType"))
			     (not (getf x :ID))))
		    types))
	  (is-true (find-if
		    #'(lambda(x)
			(and (string= (getf x :value) 
				      (concatenate
				       'string tm-id
				       "/xml-base/first/attr-type"))
			     (not (getf x :ID))))
		    types))
	  (is-true (find-if 
		    #'(lambda(x)
			(and (string= (getf x :value) 
				      "http://test-tm/xml-base/first/content-type-1")
			     (string= (getf x :ID)
				      "http://test-tm/xml-base/first#rdfID")))
		    types))
	  (is-true (find-if
		    #'(lambda(x)
			(and (string= (getf x :value) 
				      (concatenate
				       'string tm-id
				       "/xml-base/first/c-about-type-2"))
			     (string= (getf x :ID)
				      "http://test-tm/xml-base/first#rdfID2")))
		    types))
	  (is-true (find-if
		    #'(lambda(x)
			(and (string= (getf x :value) "c-nodeID-type-2")
			     (not (getf x :ID))))
		    types))
	  (is-true (find-if 
		    #'(lambda(x)
			(and (string= (getf x :value) 
				      "http://new-base#c-ID-type-2")
			     (not (getf x :ID))))
		    types))
	  (is-true (find-if 
		    #'(lambda(x)
			(and (string= (getf x :value) node-uuid)
			     (string= (getf x :ID)
				      "http://test-tm/xml-base/first#rdfID3")))
		    types))
	  (is-true (= 10 (count-if #'(lambda(x)
				      (> (length (getf x :value)) 0))
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
	  (is-true (find-if 
		    #'(lambda(x)
			(string= (getf x :ID)
				 "http://test-tm/base/initial/xml-base/first#rdfID"))
		    super-classes))
	  (is-true (map 'list
			#'(lambda(x)
			    (and
			     (> (length (getf x :value)) 0)
			     (string=
			      (getf x :ID)
			      (concatenate 'string tm-id xml-base 
					   "/xml-base/first/c-about-type-2"))))
			super-classes))
	  (is-true (map 'list
			#'(lambda(x)
			    (and (string= (getf x :value) "c-nodeID-type-2")
				 (not (getf x :ID))))
			super-classes))
	  (is-true (map 'list
			#'(lambda(x)
			    (and (string= (getf x :value)
					  "http://new/base#c-ID-type-2")
				 (not (getf x :ID))))
			super-classes))
	  (is (= (count-if  #'(lambda(x) (> (length (getf x :value)) 0))
			    super-classes)
		 8))
	  (is-true (find-if #'(lambda(x)
				(string= (getf x :ID)
					 "http://test-tm/base/initial/xml-base/first#rdfID3"))
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
			     (string= (getf x :value)
				      "http://xml-base/first/assoc-1")
			     (not (getf x :ID))))
		    associations))
	  (is-true (find-if
		    #'(lambda(x)
			(and (string= (getf x :type) "http://test/arcs/arc1")
			     (string= (getf x :ID) "http://xml-base/first#rdfID-1")
			     (string= (getf x :value) "arc1-nodeID")))
		    associations))
	  (is-true (find-if
		    #'(lambda(x)
			(and (string= (getf x :type) "http://test/arcs/arc2")
			     (> (length (getf x :value)) 0)
			     (not (getf x :ID))))
		    associations))
	  (is-true (find-if
		    #'(lambda(x)
			(and (string= (getf x :type) "http://test/arcs/arc3")
			     (string= (getf x :ID)
				      "http://xml-base/first#rdfID-2")
			     (> (length (getf x :value)) 0)))
		    associations))
	  (is-true (find-if
		    #'(lambda(x)
			(and (string= (getf x :type) "http://test/arcs/arc4")
			     (not (getf x :ID))
			     (> (length (getf x :value)) 0)))
		    associations))
	  (is-true (find-if
		    #'(lambda(x)
			(and (string= (getf x :type) "http://test/arcs/arc4")
			     (not (getf x :ID))
			     (> (length (getf x :value)) 0)))
		    associations))
	  (is-true (find-if
		    #'(lambda(x)
			(and (string= (getf x :type) "http://test/arcs/arc6")
			     (string= (getf x :ID)
				      "http://xml-base/first#rdfID-3")
			     (string= (getf x :value)
				      "http://xml-base/first/con-1")))
		    associations))
	  (is-true (find-if
		    #'(lambda(x)
			(and (string= (getf x :type) "http://test/arcs/arc7")
			     (not (getf x :ID))
			     (string= (getf x :value) "con-2")))
		    associations))
	  (is-true (find-if
		    #'(lambda(x)
			(and (string= (getf x :type) "http://test/arcs/arc8")
			     (not (getf x :ID))
			     (string= (getf x :value)
				      "http://xml-base/first#rdfID-4")))
		    associations))
	  (is-true (find-if
		    #'(lambda(x)
			(and (string= (getf x :type) "http://test/arcs/arc9")
			     (string= (getf x :ID)
				      "http://xml-base/first/add#rdfID-5")
			     (> (length (getf x :value)))))
		    associations))
	  (is-true (find-if
		    #'(lambda(x)
			(and (string= (getf x :type)
				      (concatenate 'string *rdfs-ns* "type"))
			     (not (getf x :ID))
			     (string= (getf x :value)
				      "http://xml-base/first/assoc-11")))
		    associations))
	  (is-true (find-if
		    #'(lambda(x)
			(and (string= (getf x :type)
				      (concatenate 'string *rdf-ns*
						   "subClassOf"))
			     (not (getf x :ID))
			     (string= (getf x :value) "assoc-12")))
		    associations)))))))


(test test-parse-properties-of-node
  (let ((doc-1
	 (concatenate 'string "<rdf:Description xmlns:rdf=\"" *rdf-ns* "\" "
		      "xmlns:arcs=\"http://test/arcs/\" "
                      "xml:base=\"http://xml-base/first\" "
		      "rdf:about=\"resource\" rdf:type=\"attr-type\">"
		      "<rdf:li rdf:resource=\"anyType\" />"
		      "<rdf:li>   </rdf:li>"
		      "<rdf:li rdf:nodeID=\"anyClass\" />"
		      "<rdf:li>   </rdf:li>"
		      "<rdf:li rdf:resource=\"assoc-1\"/>"
		      "<rdf:li rdf:type=\"assoc-2-type\">"
		      "   </rdf:li>"
		      "<rdf:li rdf:parseType=\"Literal\" />"
		      "<rdf:_123 arcs:arc5=\"text-arc5\" />"
		      "<rdf:arc6 rdf:ID=\"rdfID-3\"/>"
		      "<rdf:arcs rdf:ID=\"rdfID-4\"/>"
		      "</rdf:Description>")))
    (let ((dom-1 (cxml:parse doc-1 (cxml-dom:make-dom-builder))))
      (is-true dom-1)
      (is (= (length (dom:child-nodes dom-1))))
      (let ((node (elt (dom:child-nodes dom-1) 0)))
	(is-true (rdf-importer::parse-properties-of-node node))
	(is (= (length rdf-importer::*_n-map*) 7))
	(format t "~a~%" rdf-importer::*_n-map*)
	(dotimes (iter (length rdf-importer::*_n-map*))
	  (is-true (find-if
		    #'(lambda(x)
			(string= (getf x :type)
				 (concatenate
				  'string *rdf-ns* "_"
				  (write-to-string (+ 1 iter)))))
		    rdf-importer::*_n-map*)))
	(rdf-importer::remove-node-properties-from-*_n-map* node)
	(is (= (length rdf-importer::*_n-map*) 0))))))
  


(defun run-rdf-importer-tests()
  (it.bese.fiveam:run! 'test-get-literals-of-node)
  (it.bese.fiveam:run! 'test-parse-node)
  (it.bese.fiveam:run! 'test-get-literals-of-property)
  (it.bese.fiveam:run! 'test-parse-property)
  (it.bese.fiveam:run! 'test-get-types)
  (it.bese.fiveam:run! 'test-get-literals-of-content)
  (it.bese.fiveam:run! 'test-get-super-classes-of-node-content)
  (it.bese.fiveam:run! 'test-get-associations-of-node-content)
  (it.bese.fiveam:run! 'test-parse-properties-of-node))