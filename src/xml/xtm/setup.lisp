;;+-----------------------------------------------------------------------------
;;+  Isidorus
;;+  (c) 2008-2010 Marc Kuester, Christoph Ludwig, Lukas Georgieff
;;+
;;+  Isidorus is freely distributable under the LLGPL license.
;;+  You can find a detailed description in trunk/docs/LLGPL-LICENSE.txt and
;;+  trunk/docs/LGPL-LICENSE.txt.
;;+-----------------------------------------------------------------------------

(in-package :xtm-importer)

(defun get-uuid ()
  "Little helper funtion that gets a UUID as a string"
  (format nil "~a" (uuid:make-v4-uuid)))


(defun import-from-xtm (xtm-path repository-path &key 
			(tm-id (error "you must provide a stable identifier (PSI-style) for this TM"))
			(xtm-format :2.0)
			(xtm-id (get-uuid)))
  "Imports an XTM file into an existing repository using the correct
   importer for the XTM version. Does *not* close the store afterwards"
  (declare (type (or pathname string) xtm-path repository-path)
	   (String tm-id xtm-id)
	   (Keyword xtm-format))
  (let ((xtm-dom (dom:document-element
		  (cxml:parse-file
		   (truename xtm-path) (cxml-dom:make-dom-builder)))))
    (open-tm-store repository-path)
	 ;create the topic stubs so that we can refer to them later on
    (setf d:*current-xtm* xtm-id)
    (if (eq xtm-format :2.0)
	(importer xtm-dom :tm-id tm-id :xtm-id xtm-id)
	(importer-xtm1.0 xtm-dom :tm-id tm-id :xtm-id xtm-id))
    (with-reader-lock
      (format t "#Objects in the store: Topics: ~a, Associations: ~a~%"
	      (length (elephant:get-instances-by-class 'TopicC))
	      (length (elephant:get-instances-by-class 'AssociationC))))))


(defun setup-repository (xtm-path repository-path 
                         &key
                         (tm-id (error "you must provide a stable identifier (PSI-style) for this TM"))
                         (xtm-id (get-uuid))
                         (xtm-format :2.0))
  "Initializes a repository and imports a XTM file into it"
  (declare (type (or pathname string) xtm-path repository-path)
	   (String tm-id xtm-id)
	   (Keyword xtm-format))
  (open-tm-store repository-path)
  (init-isidorus)
  (import-from-xtm xtm-path repository-path :tm-id tm-id :xtm-id xtm-id
		   :xtm-format xtm-format)
  (when elephant:*store-controller*
    (close-tm-store)))