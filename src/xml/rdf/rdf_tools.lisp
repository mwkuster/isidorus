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
		*xml-string*)
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