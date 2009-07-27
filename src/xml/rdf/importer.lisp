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
  (parse-node elem)
  )