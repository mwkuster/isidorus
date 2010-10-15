;;+-----------------------------------------------------------------------------
;;+  Isidorus
;;+  (c) 2008-2010 Marc Kuester, Christoph Ludwig, Lukas Georgieff
;;+
;;+  Isidorus is freely distributable under the LLGPL license.
;;+  You can find a detailed description in trunk/docs/LLGPL-LICENSE.txt and
;;+  trunk/docs/LGPL-LICENSE.txt.
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
		*tm2rdf-ns*
		*type-instance-psi*
		*supertype-subtype-psi*
		*tm2rdf-name-type-uri*
		*tm2rdf-variant-type-uri*
		*tm2rdf-occurrence-type-uri*
		*tm2rdf-topic-type-uri*
		*tm2rdf-association-type-uri*
		*tm2rdf-role-type-uri*
		*tm2rdf-reifier-property*)
  (:import-from :isidorus-threading
		with-reader-lock
		with-writer-lock)
  (:export :export-rdf
	   :to-rdf-string))

(in-package :rdf-exporter)


(defvar *export-tm* nil "TopicMap which is exported (nil if all is
                         to be exported, the same mechanism as
                         in xtm-exporter")

(defvar *ns-map* nil "((:prefix <string> :uri <string>))")


(defun rdf-li-or-uri (uri)
  "Returns a string which represents an URI. If the given URI is
   of the type rdf:_n there will be returned rdf:li."
  (let ((rdf-len (length *rdf-ns*)))
    (let ((prep-uri (when (string-starts-with
			   uri (concatenate 'string *rdf-ns* "_"))
		      (subseq uri (+ rdf-len 1)))))
      (if prep-uri
	  (handler-case (progn
			  (parse-integer prep-uri)
			  (concatenate 'string *rdf-ns* "li"))
	    (condition () uri))
	  uri))))


(defun init-*ns-map* ()
  "Initializes the variable *ns-map* with some prefixes and corresponding
   namepsaces. So the predifend namespaces are not contain ed twice."
  (setf *ns-map* (list
		  (list :prefix "isi"
			:uri *tm2rdf-ns*)
		  (list :prefix "rdf"
			:uri *rdf-ns*)
		  (list :prefix "rdfs"
			:uri *rdfs-ns*)
		  (list :prefix "xml"
			:uri *xml-ns*))))


(defmacro with-property (construct &body body)
  "Generates a property element with a corresponding namespace
   and tag name before executing the body. This macro is for using
   in occurrences and associations that are mapped to RDF properties."
  `(let ((ns-list
	  (separate-uri (rdf-li-or-uri
			 (uri (first (psis (instance-of ,construct))))))))
     (declare ((or OccurrenceC AssociationC) ,construct))
     (let ((ns (getf ns-list :prefix))
	   (tag-name (getf ns-list :suffix)))
       (cxml:with-namespace ((get-ns-prefix ns) ns)
	 (cxml:with-element (concatenate 'string (get-ns-prefix ns)
					 ":" tag-name)
	   ,@body)))))


(defmacro export-to-elem (tm to-elem)
  "Exports all topics and associations depending to the given
   tm. If tm is nil all topics and associations are exported.
   Thic macro is equal to the one in xtm-exporter with a different
   handler for associations."
  `(setf *export-tm* ,tm)
  `(format t "*export-tm*: ~a" *export-tm*)
  `(map 'list 
        ,to-elem
        (remove-if 
         #'null 
         (map 'list 
              #'(lambda(top)
                  (d:find-item-by-revision top revision))
              (if ,tm
                  (union
                    (d:topics ,tm) (intersection (list-tm-associations) (d:associations ,tm)))
                  (union
                   (elephant:get-instances-by-class 'd:TopicC)
                   (list-tm-associations)))))))


(defun export-rdf (rdf-path &key tm-id (revision (get-revision)))
  "Exports the topoic map bound to tm-id as RDF."
  (with-reader-lock
    (let ((tm (when tm-id
		(get-item-by-item-identifier tm-id :revision revision))))
      (init-*ns-map*)
      (setf *export-tm* tm)
      (with-revision revision
	(setf SB-IMPL::*DEFAULT-EXTERNAL-FORMAT* :utf-8)
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


(defun make-isi-type (type-uri)
  "Creates a rdf:type property with the URL-prefix of *tm2rdf-ns*."
  (declare (string type-uri))
  (cxml:with-element "rdf:type"
    (cxml:attribute "rdf:resource" type-uri)))


(defun get-ns-prefix (ns-uri)
  "Returns a namespace prefix of the form ns<integer>
   that is given for a name space during serialization.
   This mechanism is needed, since relations in RDF have
   a variable tag name and namespace, so this function
   uses the namespace map *ns-map*."
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
  "Returns a plist of the form (:prefix <string> :suffix <string>)
   that contains the prefix part of the passed uri and the suffix
   part separated by a '/' or '#'."
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
  "Returns t if the topic was an imported xml:lang attribute
   of RDF/XML. This is the case if the topic has exactly one PSI
   with the uri-prefix *rdf2tm-scope-prefix*."
  (declare (TopicC topic))
  (when (= (length (psis topic)) 1)
    (when (string-starts-with (uri (first (psis topic)))
			      *rdf2tm-scope-prefix*)
      t)))


(defun make-object-id (object)
  "Returns a string of the form id_<integer> which can be used
   as nodeID."
  (concatenate 'string "id_" (write-to-string (elephant::oid object))))


(defun make-topic-reference (topic)
  "Creates a topic refenrence by using the attributes rdf:resource
   or rdf:nodeID, this depends on the PSIS of the topic."
  (declare (TopicC topic))
  (if (psis topic)
      (cxml:attribute "rdf:resource"
		      (if (reified-construct topic)
			  (let ((psi (get-reifier-psi topic)))
			    (if psi
				(concatenate 'string "#" (get-reifier-uri topic))
				(uri (first (psis topic)))))
			  (uri (first (psis topic)))))
      (cxml:attribute "rdf:nodeID" (make-object-id topic))))


(defun isi-occurrence-p (owner-topic)
  "Returns t if the owner topic has an occurrence that will
   be mapped to an RDF occurrence node and no an
   usual RDF property."
  (declare (TopicC owner-topic))
  (loop for occ in (occurrences owner-topic)
     when (let ((ii (item-identifiers occ))
		(scopes (loop for scope in (themes occ)
			   when (not (xml-lang-p scope))
			   collect scope)))
	    (or ii scopes
		(> (length (themes occ)) 1)))
     return t))


(defgeneric to-rdf-elem (construct)
  (:documentation "Exports Topic Maps Constructs as RDF. "))


(defmethod to-rdf-elem ((construct PersistentIdC))
  "Creates a property which described a PSI."
  (cxml:with-element "isi:subjectIdentifier"
    (cxml:attribute "rdf:datatype" *xml-uri*)
    (cxml:text (uri construct))))


(defmethod to-rdf-elem ((construct SubjectLocatorC))
  "Creates a property which describes a subjectLocator."
  (cxml:with-element "isi:subjectLocator"
    (cxml:attribute "rdf:datatype" *xml-uri*)
    (cxml:text (uri construct))))


(defmethod to-rdf-elem ((construct ItemIdentifierC))
  "Creates a property which creates an itemIdentifier."
  (cxml:with-element "isi:itemIdentity"
    (cxml:attribute "rdf:datatype" *xml-uri*)
    (cxml:text (uri construct))))


(defun scopes-to-rdf-elems (owner-construct)
  "Creates a set of properties. Everyone contains a reference to
   a scope topic."
  (declare ((or AssociationC OccurrenceC NameC VariantC RoleC) owner-construct))
  (map 'list #'(lambda(x)
		 (cxml:with-element "isi:scope"
		   (make-topic-reference x)))
       (themes owner-construct)))


(defun resourceX-to-rdf-elem (owner-construct)
  "Creates a property that contains a literal value and a datatype
   depending on occurrences or variants."
  (declare ((or OccurrenceC VariantC) owner-construct))
  (cxml:with-element "isi:value"
    (cxml:attribute "rdf:datatype" (datatype owner-construct))
    (cxml:text (charvalue owner-construct))))


(defmethod to-rdf-elem ((construct VariantC))
  "Creates a blank node that represents a VariantC element with the
   properties itemIdentity, scope and value."
  (cxml:with-element "isi:variant"
    (cxml:with-element "rdf:Description"
      (cxml:attribute "rdf:nodeID" (make-object-id construct))
      (make-isi-type *tm2rdf-variant-type-uri*)
      (export-reifier-as-mapping construct)
      (map 'list #'to-rdf-elem (item-identifiers construct))
      (scopes-to-rdf-elems construct)
      (resourceX-to-rdf-elem construct))))


(defmethod to-rdf-elem ((construct NameC))
  "Creates a blank node that represents a name element with the
   properties itemIdentity, nametype, value, variant and scope."
  (cxml:with-element "isi:name"
    (cxml:with-element "rdf:Description"
      (cxml:attribute "rdf:nodeID" (make-object-id construct))
      (make-isi-type *tm2rdf-name-type-uri*)
      (export-reifier-as-mapping construct)
      (map 'list #'to-rdf-elem (item-identifiers construct))
      (when (instance-of construct)
	(cxml:with-element "isi:nametype"
	  (make-topic-reference (instance-of construct))))
      (scopes-to-rdf-elems construct)
      (cxml:with-element "isi:value"
	(cxml:attribute "rdf:datatype" *xml-string*)
	(cxml:text (charvalue construct)))
      (map 'list #'to-rdf-elem (variants construct)))))


(defmethod to-rdf-elem ((construct OccurrenceC))
  "Creates a blank node that represents an occurrence element with the
   properties itemIdentity, occurrencetype, value and scope."
  (let ((scopes (when (themes construct)
		  (loop for theme in (themes construct)
		     when (not (xml-lang-p theme))
		     collect theme))))
    (if (or scopes
	    (> (length (themes construct)) 1)
	    (item-identifiers construct)
	    (/= (length (psis (instance-of construct))) 1))
	(cxml:with-element "isi:occurrence"
	  (cxml:with-element "rdf:Description"
	    (cxml:attribute "rdf:nodeID" (make-object-id construct))
	    (make-isi-type *tm2rdf-occurrence-type-uri*)
	    (export-reifier-as-mapping construct)
	    (map 'list #'to-rdf-elem (item-identifiers construct))
	    (cxml:with-element "isi:occurrencetype"
	      (make-topic-reference (instance-of construct)))
	    (scopes-to-rdf-elems construct)
	    (resourceX-to-rdf-elem construct)))
	(with-property construct
	  (cxml:attribute "rdf:datatype" (datatype construct))
	  (export-reifier construct)
	  (when (themes construct)
	    (cxml:attribute "xml:lang" (get-xml-lang
					(first (themes construct)))))
	  (cxml:text (charvalue construct))))))


(defmethod to-rdf-elem ((construct TopicC))
  "Creates a node that describes a TM topic."
  (if (and (not (or (> (length (psis construct)) 1)
		    (item-identifiers construct)
		    (locators construct)
		    (names construct)
		    (occurrences construct)))
	   (or (used-as-type construct)
	       (used-as-theme construct)
	       (xml-lang-p construct)))
      nil ;; do not export this topic explicitly, since it has been exported as
          ;; rdf:resource, property or any other reference
      (topic-to-rdf-elem construct)))


(defun sort-constructs (constructs)
  "Sorts names and associations by the instance-of name.
   So rdf:_n can be exported in the correct order."
  (sort constructs #'(lambda(x y)
		       (declare ((or OccurrenceC AssociationC) x y))
		       (let ((x-psi (when (psis (instance-of x))
				      (uri (first (psis (instance-of x))))))
			     (y-psi (when (psis (instance-of y))
				      (uri (first (psis (instance-of y)))))))
			 (string< x-psi y-psi)))))
  

(defmethod to-rdf-elem ((construct AssociationC))
  "Exports association elements as RDF properties."
  (let ((type-instance (get-item-by-psi *type-instance-psi*))
	(supertype-subtype (get-item-by-psi *supertype-subtype-psi*))
	(association-type (instance-of construct)))
    (if (or (eql type-instance association-type)
	    (eql supertype-subtype association-type))
	nil ;; do nothing, the association has been already exported
	    ;; either as rdf:type or rdfs:subClassOf
	(let ((isi-subject (get-item-by-psi *rdf2tm-subject*))
	      (isi-object (get-item-by-psi *rdf2tm-object*))
	      (association-roles (roles construct))
	      (ii (item-identifiers construct))
	      (scopes (themes construct)))
	  (let ((subject-role (find-if #'(lambda(x)
					   (eql isi-subject (instance-of x)))
				       association-roles))
		(object-role (find-if #'(lambda(x)
					  (eql isi-object (instance-of x)))
				      association-roles)))
	    (if (and subject-role object-role (not ii) (not scopes)
		     (= (length association-roles) 2))
		(rdf-mapped-association-to-rdf-elem construct)
		(tm-association-to-rdf-elem construct)))))))


(defun tm-association-to-rdf-elem (association)
  "Exports a TM association as an RDF resource with special
   properties, that descirbes this association."
  (declare (AssociationC association))
  (let ((ii (item-identifiers association))
	(association-type (instance-of association))
	(association-roles (roles association)))
    (cxml:with-element "rdf:Description" 
      (cxml:attribute "rdf:nodeID" (make-object-id association))
      (make-isi-type *tm2rdf-association-type-uri*)
      (export-reifier-as-mapping association)
      (cxml:with-element "isi:associationtype"
	(make-topic-reference association-type))
      (map 'list #'to-rdf-elem ii)
      (scopes-to-rdf-elems association)
      (map 'list #'to-rdf-elem association-roles))))


(defmethod to-rdf-elem ((construct RoleC))
  "Exports a TM role as RDF resource with the properties
   isi:roletype, isi:itemIdentity and isi:player."
  (let ((ii (item-identifiers construct))
	(role-type (instance-of construct))
	(player-top (player construct)))
    (cxml:with-element "isi:role"
      (cxml:with-element "rdf:Description"
	(cxml:attribute "rdf:nodeID" (make-object-id construct))
	(export-reifier-as-mapping construct)
	(make-isi-type *tm2rdf-role-type-uri*)
	(map 'list #'to-rdf-elem ii)
	(cxml:with-element "isi:roletype"
	  (make-topic-reference role-type))
	(cxml:with-element "isi:player"
	  (make-topic-reference player-top))))))


(defun rdf-mapped-association-to-rdf-elem (association)
  "Exports an TM association as RDF that was imported from RDF.
   This is indicated by the existence of exactly two roles. One
   of the type isi:object, the other of the type isi:subject.
   Scopes or itemIdentifiers are also forbidden.
   If the contained roles own any reifiers they are ignored."
  (declare (AssociationC association))
  (let ((isi-subject (get-item-by-psi *rdf2tm-subject*))
	(isi-object (get-item-by-psi *rdf2tm-object*))
	(association-roles (roles association)))
    (let ((subject-role (find-if #'(lambda(x)
				     (eql isi-subject (instance-of x)))
				 association-roles))
	  (object-role (find-if #'(lambda(x)
				    (eql isi-object (instance-of x)))
				association-roles)))
      (when (and subject-role object-role
		 (= (length association-roles) 2))
	(with-property association
	  (export-reifier association)
	  (make-topic-reference (player object-role)))))))


(defun list-rdf-mapped-associations(subject-topic)
  "Returns all associations that were mapped from RDF to TM
   and are still having two roles of the type isi:subject and
   isi:object."
  (declare (TopicC subject-topic))
  (let ((isi-subject (get-item-by-psi *rdf2tm-subject*))
	(isi-object (get-item-by-psi *rdf2tm-object*)))
    (let ((topic-roles
	   (remove-if
	    #'null
	    (map 'list 
		 #'(lambda(x)
		     (when (and (eql (instance-of x) isi-subject)
				(= (length (roles (parent x))) 2)
				(find-if #'(lambda(y)
					     (eql (instance-of y) isi-object))
					 (roles (parent x))))
		       x))
		 (player-in-roles subject-topic)))))
      (map 'list #'parent topic-roles))))


(defun list-tm-associations()
  "Returns a list of associations that were not mapped from RDF
   and are not of the type type-instance or supertype-subtype."
  (let ((isi-subject (get-item-by-psi *rdf2tm-subject*))
	(isi-object (get-item-by-psi *rdf2tm-object*))
	(type-instance (get-item-by-psi *type-instance-psi*))
	(supertype-subtype (get-item-by-psi *supertype-subtype-psi*)))
    (remove-if 
     #'null
     (map 'list 
	  #'(lambda(x)
	      (when (and
		     (not (or (eql (instance-of x) type-instance)
			      (eql (instance-of x) supertype-subtype)))
		     (or (/= (length (roles x)) 2)
			 (not (find-if #'(lambda(y)
					   (eql (instance-of y) isi-object))
				       (roles x)))
			 (not (find-if #'(lambda(y)
					   (eql (instance-of y) isi-subject))
				       (roles x)))))
		x))
	  (elephant:get-instances-by-class 'AssociationC)))))


(defun export-reifier(reifiable-construct)
  "Exports the reifier-ID-attribute"
  (declare (ReifiableConstructC reifiable-construct))
  (let ((reifier-topic (reifier reifiable-construct)))
    (when (and reifier-topic
	       (psis reifier-topic))
      (let ((reifier-uri (get-reifier-uri reifier-topic)))
	(when reifier-uri
	  (cxml:attribute "rdf:ID" reifier-uri))))))


(defun export-reifier-as-mapping (reifiable-construct)
  "Exports the reifier as isi:reifier property."
  (declare (ReifiableConstructC reifiable-construct))
  (let ((reifier-topic (reifier reifiable-construct)))
    (when (and reifier-topic
	       (psis reifier-topic))
      (let ((reifier-uri (get-reifier-uri reifier-topic)))
	(when reifier-uri
	  (cxml:with-element "isi:reifier"
	    (cxml:attribute "rdf:resource" reifier-uri)))))))


(defun get-reifier-uri (top)
  "Returns the uri that represents the reifier-id of a resource node.
   When the topic does not own a psi the return value is nil."
  (declare (TopicC top))
  (when (psis top)
    (let ((full-uri
	   (let ((reifier-psi (get-reifier-psi top)))
	     (when reifier-psi
	       (uri reifier-psi))))
	  (err "From get-reifier-uri(): "))
      (let ((slash-position (position #\/ full-uri :from-end t)))
	(let ((hash-position (position #\# full-uri)))
	  (if (and hash-position
		   (/= (- (length full-uri) 1) hash-position))
	      (subseq full-uri (+ hash-position 1))
	      (if (and slash-position
		       (/= (- (length full-uri) 1) slash-position))
		  (subseq full-uri (+ 1 slash-position))
		  (if (= hash-position (+ (length full-uri) 1))
		      (error "~athe PSI-URI ~a ends with an #" err full-uri)
		      full-uri))))))))


(defun get-reifier-psi(topic)
  "Returns the first found psi that can be used as a reifier-id, i.e.
   the psi-uri must contain a '#' or '/'."
  (declare (TopicC topic))
  (find-if #'(lambda(psi)
	       (let ((hash-position (position #\# (uri psi) :from-end t))
		     (slash-position (position #\/ (uri psi) :from-end t)))
		 (if (or (and hash-position
			      (< hash-position (- (length (uri psi)) 1)))
			 (and slash-position
			      (< slash-position (- (length (uri psi)) 1))))
		     psi
		     nil)))
	   (psis topic)))


(defmethod to-rdf-elem ((construct FragmentC))
  "Exports TM-Fragments as RDF/XML data."
  (topic-to-rdf-elem (topic construct))
  (map 'list #'(lambda(top)
		 (when (or (> (length (psis top)) 1)
			   (item-identifiers top)
			   (locators top))
		   (topic-to-rdf-stub-elem top)))
       (referenced-topics construct))
  ;all other stubs are exported implicitely by references of the main topic or associations
  (map 'list #'to-rdf-elem (intersection (list-tm-associations) (associations construct))))


(defun topic-to-rdf-elem (construct)
  "Creates a node that describes a TM topic. The passed topic is exported
   explicitely, although it was exported as a resource-reference."
  (declare (TopicC construct))
  (cxml:with-element "rdf:Description"
    (let ((psi (get-reifier-psi construct))
	  (ii (item-identifiers construct))
	  (sl (locators construct))
	  (t-names (names construct))
	  (t-occs (occurrences construct))
	  (t-assocs (list-rdf-mapped-associations construct)))
      (if psi
	  (if (reified-construct construct)
	      (let ((reifier-uri (get-reifier-uri construct)))
		(if reifier-uri
		    (cxml:attribute "rdf:about" (concatenate 'string "#" (get-reifier-uri construct)))
		    (cxml:attribute "rdf:about" (uri psi))))
	      (cxml:attribute "rdf:about" (uri psi)))
	  (cxml:attribute "rdf:nodeID" (make-object-id construct)))
      (when (or (> (length (psis construct)) 1)
		ii sl t-names
		(isi-occurrence-p construct))
	(make-isi-type *tm2rdf-topic-type-uri*))
      (map 'list #'to-rdf-elem (remove psi (psis construct)))
      (map 'list #'to-rdf-elem sl)
      (map 'list #'to-rdf-elem ii)
      (map 'list #'(lambda(x)
		     (cxml:with-element "rdf:type"
		       (make-topic-reference x)))
	   (list-instanceOf construct))
      (map 'list #'(lambda(x)
		     (cxml:with-element "rdfs:subClassOf"
		       (make-topic-reference x)))
	   (list-super-types construct))
      (map 'list #'to-rdf-elem t-names)
      (map 'list #'to-rdf-elem (sort-constructs
				(union t-occs t-assocs))))))


(defun topic-to-rdf-stub-elem (construct)
  "Exports a topic as a stub."
  (declare (TopicC construct))
  (cxml:with-element "rdf:Description"
    (let ((psi (get-reifier-psi construct))
	  (ii (item-identifiers construct))
	  (sl (locators construct)))
      (if psi
	  (if (reified-construct construct)
	      (let ((reifier-uri (get-reifier-uri construct)))
		(if reifier-uri
		    (cxml:attribute "rdf:about" (concatenate 'string "#" (get-reifier-uri construct)))
		    (cxml:attribute "rdf:about" (uri psi))))
	      (cxml:attribute "rdf:about" (uri psi)))
	  (cxml:attribute "rdf:nodeID" (make-object-id construct)))
      (map 'list #'to-rdf-elem (remove psi (psis construct)))
      (map 'list #'to-rdf-elem sl)
      (map 'list #'to-rdf-elem ii))))


(defgeneric to-rdf-string (construct)
  (:documentation "Prints the string representation of a Fragment element as RDF/XML"))


(defmethod to-rdf-string ((construct FragmentC))
  "Exports a FragmentC object as a string in RDF/XML representation."
  (init-*ns-map*)
  (let ((str
	 (cxml:with-xml-output (cxml:make-string-sink :indentation 2 :canonical nil)
	   (cxml:with-namespace ("isi" *tm2rdf-ns*)
	     (cxml:with-namespace ("rdf" *rdf-ns*)
	       (cxml:with-namespace ("rdfs" *rdfs-ns*)
		 (cxml:with-namespace ("xml" *xml-ns*)
		   (cxml:with-element "rdf:RDF"
		     (to-rdf-elem construct)))))))))
    (setf *ns-map* nil)
    str))


(defmethod to-rdf-string ((construct TopicMapConstructC))
  "Exports a TopicMapConstructC object as a string in RDF/XML representation."
  (init-*ns-map*)
  (let ((str
	 (cxml:with-xml-output (cxml:make-string-sink :indentation 2 :canonical nil)
	   (cxml:with-namespace ("isi" *tm2rdf-ns*)
	     (cxml:with-namespace ("rdf" *rdf-ns*)
	       (cxml:with-namespace ("rdfs" *rdfs-ns*)
		 (cxml:with-namespace ("xml" *xml-ns*)
		   (cxml:with-element "rdf:RDF"
		     (to-rdf-elem construct)))))))))
    (setf *ns-map* nil)
    str))


