;;+-----------------------------------------------------------------------------
;;+  Isidorus
;;+  (c) 2008-2009 Marc Kuester, Christoph Ludwig, Lukas Giessmann
;;+
;;+  Isidorus is freely distributable under the LGPL license.
;;+  You can find a detailed description in trunk/docs/LGPL-LICENSE.txt.
;;+-----------------------------------------------------------------------------

(defpackage :rdf-exporter
  (:use :cl :cxml :elephant :datamodel :isidorus-threading :datamodel)
  (:import-from :constants
		*rdf-ns*
		*rdfs-ns*
		*xml-ns*
		*xml-string*
		*xml-uri*
		*rdf2tm-ns*
		*rdf2tm-object*
		*rdf2tm-subject*
		*rdf2tm-scope-prefix*
		*tm2rdf-ns*)
  (:import-from :isidorus-threading
		with-reader-lock
		with-writer-lock)
  (:import-from :exporter
		*export-tm*
		export-to-elem)
  (:export :export-rdf))

(in-package :rdf-exporter)


(defvar *ns-map* nil) ;; ((:prefix <string> :uri <string>))


(defun export-rdf (rdf-path &key tm-id (revision (get-revision)))
  "Exports the topoic map bound to tm-id as RDF."
  (with-reader-lock
    (let ((tm (when tm-id
		(get-item-by-item-identifier tm-id :revision revision))))
      (setf *ns-map* nil)
      (setf *export-tm* tm)
      (with-revision revision
	(with-open-file (stream rdf-path :direction :output)
	  (cxml:with-xml-output (cxml:make-character-stream-sink
				 stream :canonical nil)
	    (cxml:with-namespace ("isi" *tm2rdf-ns*)
	      (cxml:with-namespace ("rdf" *rdf-ns*)
		(cxml:with-namespace ("rdfs" *rdfs-ns*)
		  (cxml:with-namespace ("xml" *xml-ns*)
		    (cxml:with-element "rdf:RDF"
		      (export-to-elem tm #'to-rdf-elem)))))))))))
  (setf *ns-map* nil))


(defun get-ns-prefix (ns-uri)
  (let ((ns-entry
	 (find-if #'(lambda(x)
		      (string= (getf x :uri)
			       ns-uri))
		  *ns-map*)))
    (if ns-entry
	(getf ns-entry :prefix)
	(let ((new-name (concatenate
			 'string "ns"
			 (write-to-string (+ 1 (length *ns-map*))))))
	  (push (list :prefix new-name
		      :uri ns-uri)
		*ns-map*)
	  new-name))))


(defun separate-uri (uri)
  (when (or (not uri)
	    (= (length uri) 0)
	    (and uri
		 (> (length uri) 0)
		 (or (eql (elt uri (- (length uri) 1)) #\#)
		     (eql (elt uri (- (length uri) 1)) #\/)
		     (eql (elt uri 0) #\#)
		     (eql (elt uri 0) #\/))))
    (error "From separate-uri(): bad ns-uri: ~a" uri))
  (let ((pos-hash (position #\# uri :from-end t))
	(pos-slash (position #\/ uri :from-end t)))
    (unless (or pos-hash pos-slash)
      (error "From separate-uri(): bad ns-uri: ~a" uri))
    (if (not (or pos-hash pos-slash))
	(list :prefix *tm2rdf-ns*
	      :suffix uri)
	(let ((prefix (subseq uri 0 (+ (max (or pos-hash 0) (or pos-slash 0)) 1)))
	      (suffix (subseq uri (+ (max (or pos-hash 0) (or pos-slash 0)) 1))))
	  (list :prefix prefix
		:suffix suffix)))))


(defun get-xml-lang (topic)
  (declare (TopicC topic))
  (when (xml-lang-p topic)
    (subseq (uri (first (psis topic))) (length *rdf2tm-scope-prefix*))))


(defun xml-lang-p (topic)
  (declare (TopicC topic))
  (when (= (length (psis topic)) 1)
    (when (string-starts-with (uri (first (psis topic)))
			      *rdf2tm-scope-prefix*)
      t)))


(defun make-topic-id (topic)
  (declare (TopicC topic))
  (concatenate 'string "id_" (write-to-string (elephant::oid topic))))


(defun make-topic-reference (topic)
  (declare (TopicC topic))
  (if (psis topic)
      (cxml:attribute "rdf:resource" (uri (first (psis topic))))
      (cxml:attribute "rdf:nodeID" (make-topic-id topic))))
		      


(defgeneric to-rdf-elem (construct)
  (:documentation "Exports Topic Maps Constructs as RDF. "))


(defmethod to-rdf-elem ((construct PersistentIdC))
  (cxml:with-element "isi:subjectIdentifier"
    (cxml:attribute "rdf:datatype" *xml-uri*)
    (cxml:text (uri construct))))


(defmethod to-rdf-elem ((construct SubjectLocatorC))
  (cxml:with-element "isi:subjectLocator"
    (cxml:attribute "rdf:datatype" *xml-uri*)
    (cxml:text (uri construct))))


(defmethod to-rdf-elem ((construct ItemIdentifierC))
  (cxml:with-element "isi:itemIdentity"
    (cxml:attribute "rdf:datatype" *xml-uri*)
    (cxml:text (uri construct))))


(defun scopes-to-rdf-elems (owner-construct)
  (declare ((or AssociationC OccurrenceC NameC VariantC RoleC) owner-construct))
  (map 'list #'(lambda(x)
		 (cxml:with-element "isi:scope"
		   (make-topic-reference x)))
       (themes owner-construct)))


(defun resourceX-to-rdf-elem (owner-construct)
  (declare ((or OccurrenceC VariantC) owner-construct))
  (cxml:with-element "isi:value"
    (cxml:attribute "rdf:datatype" (datatype owner-construct))
    (cxml:text (charvalue owner-construct))))


(defmethod to-rdf-elem ((construct VariantC))
  (cxml:with-element "isi:variant"
    (cxml:attribute "rdf:parseType" "Resource")
    (map 'list #'to-rdf-elem (item-identifiers construct))
    (scopes-to-rdf-elems construct)
    (resourceX-to-rdf-elem construct)))


(defmethod to-rdf-elem ((construct NameC))
  (cxml:with-element "isi:name"
    (cxml:attribute "rdf:parseType" "Resource")
    (map 'list #'to-rdf-elem (item-identifiers construct))
    (cxml:with-element "isi:nametype"
      (make-topic-reference (instance-of construct)))
    (scopes-to-rdf-elems construct)
    (cxml:with-element "isi:value"
      (cxml:attribute "rdf:datatype" *xml-string*)
      (cxml:text (charvalue construct)))
    (map 'list #'to-rdf-elem (variants construct))))


(defmethod to-rdf-elem ((construct OccurrenceC))
  (let ((scopes (when (themes construct)
		  (loop for theme in (themes construct)
		     when (not (xml-lang-p theme))
		     collect theme))))
    (if (or scopes
	    (item-identifiers construct)
	    (/= (length (psis (instance-of construct))) 1))
	(cxml:with-element "isi:occurrence"
	  (cxml:attribute "rdf:parseType" "Resource")
	  (map 'list #'to-rdf-elem (item-identifiers construct))
	  (cxml:with-element "isi:occurrencetype"
	    (make-topic-reference (instance-of construct)))
	  (scopes-to-rdf-elems construct)
	  (resourceX-to-rdf-elem construct))
	(let ((ns-list
	       (separate-uri (uri (first (psis (instance-of construct)))))))
	  (let ((ns (getf ns-list :prefix))
		(tag-name (getf ns-list :suffix)))
	    (cxml:with-namespace ((get-ns-prefix ns) ns)
	      (cxml:with-element (concatenate 'string (get-ns-prefix ns)
					      ":" tag-name)
		(cxml:attribute "rdf:datatype" (datatype construct))
		(when (themes construct)
		  (cxml:attribute "xml:lang" (get-xml-lang
					      (first (themes construct)))))
		(cxml:text (charvalue construct)))))))))


(defmethod to-rdf-elem ((construct TopicC))
  ;TODO: what's with used-as-player and core-topics
  (format t "--> ~a " (if (psis construct)
			  (uri (first (psis construct)))
			  (make-topic-id construct)))
  (if (and (not (or (> (length (psis construct)) 1)
		    (item-identifiers construct)
		    (locators construct)
		    (names construct)
		    (occurrences construct)))
	   (or (used-as-type construct)
	       (used-as-theme construct)))
      nil ;; do not export this topic explicitly, since it is exported as
          ;; rdf:resource, rdf:about or any other reference
      (cxml:with-element "rdf:Description"
	(let ((psi (when (psis construct)
		     (first (psis construct)))))
	  (if psi
	      (cxml:attribute "rdf:about" (uri psi))
	      (cxml:attribute "rdf:nodeID" (make-topic-id construct)))
	  (map 'list #'to-rdf-elem (remove psi (psis construct)))
	  (map 'list #'to-rdf-elem (locators construct))
	  (map 'list #'to-rdf-elem (item-identifiers construct))
	  (map 'list #'(lambda(x)
			 (cxml:with-element "rdf:type"
			   (make-topic-reference x)))
	       (list-instanceOf construct))
	  (map 'list #'(lambda(x)
			 (cxml:with-element "rdfs:subClassOf"
			   (make-topic-reference x)))
	       (list-super-types construct))
	  (map 'list #'to-rdf-elem (names construct))
	  (map 'list #'to-rdf-elem (occurrences construct)))))
  (format t "<--~%"))
  

(defmethod to-rdf-elem ((construct AssociationC))
  ;TODO: check if the association has to be exported or not
  )