;;+-----------------------------------------------------------------------------
;;+  Isidorus
;;+  (c) 2008-2009 Marc Kuester, Christoph Ludwig, Lukas Giessmann
;;+
;;+  Isidorus is freely distributable under the LGPL license.
;;+  You can find a detailed description in trunk/docs/LGPL-LICENSE.txt.
;;+-----------------------------------------------------------------------------
(in-package :rdf-importer)


;(defun rdf-importer (rdf-xml-path repository-path 
;		     &key 
;		     (tm-id (error "you must provide a stable identifier (PSI-style) for this TM"))
;		     (document-id (get-uuid)))
;  (unless (absolute-uri-p tm-id)
;    (error "From rdf-impoert(): you must provide a stable identifier (PSI-style) for this TM"))
;  (let ((rdf-dom
;	 (dom:document-element (cxml:parse-file
;				(truename rdf-xml-path)
;				(cxml-dom:make-dom-builder)))))
;    (unless elephant:*store-controller*
;      (elephant:open-store
;       (get-store-spec repository-path)))
;    (import-nodes rdf-dom :tm-id tm-id :document-id document-id))
;  (setf *arc-uuids* nil))






