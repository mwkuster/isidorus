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
   :unittests-constants
   :fixtures)
  (:import-from :constants
                *rdf-ns*
		*rdfs-ns*
		*rdf2tm-ns*)
  (:import-from :xml-tools
                xpath-child-elems-by-qname
		xpath-single-child-elem-by-qname
                xpath-select-location-path
		get-ns-attribute)
  (:export :test-get-literals-of-node
	   :test-parse-node
	   :run-rdf-importer-tests))

(declaim (optimize (debug 3) (speed 0) (safety 3) (space 0) (compilation-speed 0)))

(in-package :rdf-importer-test)


(def-suite importer-test
     :description "tests  various key functions of the importer")

(in-suite importer-test)


(test test-get-literals-of-node
  "Tests the helper function get-literals-of-node."
  (let ((doc-1
	 (concatenate 'string "<rdf:Description xmlns:rdf=\"" *rdf-ns* "\" "
		      "xmlns:isi=\"http://isidorus/test#\" "
		      "rdf:type=\"rdfType\" rdf:ID=\"rdfID\" rdf:nodeID=\""
		      "rdfNodeID\" rdf:unknown=\"rdfUnknown\" "
		      "isi:ID=\"isiID\" isi:arc=\"isiArc\"/>"))
	(doc-2
	 (concatenate 'string "<rdf:Description xmlns:rdf=\"" *rdf-ns* "\" "
		      "xmlns:rdfs=\"" *rdfs-ns* "\" "
		      "rdfs:subClassOf=\"rdfsSubClassOf\" />")))
    (let ((dom-1 (cxml:parse doc-1 (cxml-dom:make-dom-builder)))
	  (dom-2 (cxml:parse doc-2 (cxml-dom:make-dom-builder))))
      (is (= (length (dom:child-nodes dom-1)) 1))
      (is (= (length (dom:child-nodes dom-2)) 1))
      (let ((literals (rdf-importer::get-literals-of-node
		       (elt (dom:child-nodes dom-1) 0))))
	(is-true literals)
	(is (= (length literals) 3))
	(is-true (find-if #'(lambda(x)
			      (and
			       (string= (getf x :value) "rdfUnknown")
			       (string= (getf x :type)
					(concatenate 'string *rdf-ns* "unknown"))))
			      literals))
	(is-true (find-if #'(lambda(x)
			      (and
			       (string= (getf x :value) "isiID")
			       (string= (getf x :type)
					"http://isidorus/test#ID")))
			  literals))
	(is-true (find-if #'(lambda(x)
			      (and 
			       (string= (getf x :value) "isiArc")
			       (string= (getf x :type)
					"http://isidorus/test#arc")))
			  literals)))
      (signals error (rdf-importer::get-literals-of-node
		      (elt (dom:child-nodes dom-2) 0))))))


(test test-parse-node
  "Tests the parse-node function."
  (let ((doc-1
	 (concatenate 'string "<rdf:Description xmlns:rdf=\"" *rdf-ns* "\" "
		      "xmlns:isi=\"" *rdf2tm-ns* "\" "
		      "xmlns:arcs=\"http://test/arcs/\" "
		      "rdf:ID=\"rdfID\" xml:base=\"xmlBase\" "
		      "arcs:arc=\"arcsArc\">"
		      "<arcs:rel>"
		      "<rdf:Element rdf:about=\"element\"/>"
		      "</arcs:rel>"
		      "</rdf:Description>")))
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
	(dom:replace-child node (dom:create-text-node dom-1 "anyText")
			   (xpath-single-child-elem-by-qname
			    node "http://test/arcs/" "rel"))
	(signals error (rdf-importer::parse-node node))))))






(defun run-rdf-importer-tests()
  (it.bese.fiveam:run! 'test-get-literals-of-node)
  (it.bese.fiveam:run! 'test-parse-node))