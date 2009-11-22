;;+-----------------------------------------------------------------------------
;;+  Isidorus
;;+  (c) 2008-2009 Marc Kuester, Christoph Ludwig, Lukas Giessmann
;;+
;;+  Isidorus is freely distributable under the LGPL license.
;;+  You can find a detailed description in trunk/docs/LGPL-LICENSE.txt.
;;+-----------------------------------------------------------------------------


(in-package :xml-importer)

(defun set-reifier (reifiable-elem reifiable-construct)
  "Sets the reifier-topic of the passed elem to the passed construct."
  (declare (dom:element reifiable-elem))
  (declare (ReifiableConstructC reifiable-construct))
  (let ((reifier-uri (get-attribute reifiable-elem "reifier")))
    (when (and (stringp reifier-uri)
	       (> (length reifier-uri) 0))
      (add-reifier reifiable-construct reifier-uri t))
    reifiable-construct))


(defun from-identifier-elem (classsymbol elem start-revision)
  "Generate an identifier object of type 'classsymbol' (a subclass of
IdentifierC) from a given identifier element for a revision and return
that object"
  (declare (symbol classsymbol))
  (declare (dom:element elem))
  (declare (integer start-revision))

;;   (make-construct classsymbol
;;                   :uri (get-attribute elem "href")
;;                   :start-revision start-revision))
  (let
      ((id (make-instance classsymbol
			  :uri (get-attribute elem "href")
			  :start-revision start-revision)))
    ;(add-to-version-history id :start-revision start-revision)
    id))
  
         
(defun make-identifiers (classsymbol start-elem elem-name start-revision)
  (map 'list 
       (lambda (elem) 
         (from-identifier-elem classsymbol elem start-revision))
       (xpath-child-elems-by-qname 
        start-elem
        *xtm2.0-ns* elem-name)))


(defun from-type-elem (type-elem &key (xtm-id *current-xtm*))
  "Returns the topic that reifies this type or nil if no element is
input"
  ; type = element type { topicRef }
  ;the from-type-elem function does not need a revision element as it
  ;just points to an existing topic with its own revision history
  (when type-elem
    (let*
        ((topicid 
          (get-topicref-uri 
            (xpath-single-child-elem-by-qname 
             type-elem 
             *xtm2.0-ns* "topicRef")))
         (top (get-item-by-id topicid :xtm-id xtm-id)))
      (declare (dom:element type-elem))
      (unless top
        (error (make-condition 'missing-reference-error
                               :message (format nil "Could not resolve topicid ~a" topicid))))
      top)))


(defun from-scope-elem (scope-elem &key (xtm-id *current-xtm*))
  "Generate set of themes (= topics) from this scope element and
return that set. If the input is nil, the list of themes is empty
 scope = element scope { topicRef+ }"
  ;the from-scope-elem function does not need a revision element as it
  ;just points to existing topics with their own revision histories
  (when scope-elem
    (let*
        ((topicrefs
          (map 'list 
               #'get-topicref-uri
               (xpath-child-elems-by-qname 
                scope-elem 
                *xtm2.0-ns* "topicRef")))
         (tops 
          (map 'list 
               (lambda (topicid)
                 (let
                     ((top
                       (get-item-by-id
                        topicid :xtm-id xtm-id)))
                   (if top
                       top
                       (error (make-condition 'missing-reference-error
                               :message (format nil "from-scope-elem: could not resolve reference ~a" topicid))))))
               topicrefs)))
      (declare (dom:element scope-elem))
      
      (unless (>= (length tops) 1)
        (error "need at least one topic in a scope"))
      tops)))

   
(defun from-name-elem (name-elem top start-revision &key (xtm-id *current-xtm*))
  "Generate a NameC object from a name element name = element name {
   reifiable, type?, scope?, value, variant* }. If a topic is given, the
   name first checks for possible equality and then adds an association
   to it"
  (declare (dom:element name-elem))
  (declare (TopicC top))
  (declare (optimize (debug 3)))
  (let 
      ((item-identifiers
        (make-identifiers 'ItemIdentifierC name-elem "itemIdentity" start-revision))
       (namevalue
         (xpath-fn-string (xpath-single-child-elem-by-qname 
          name-elem 
          *xtm2.0-ns* "value")))
       (themes
        (from-scope-elem 
         (xpath-single-child-elem-by-qname 
          name-elem 
          *xtm2.0-ns* "scope") :xtm-id xtm-id))      
        (instance-of
         (from-type-elem (xpath-single-child-elem-by-qname 
                          name-elem 
                          *xtm2.0-ns* "type") :xtm-id xtm-id)))
    (unless namevalue
        (error "A name must have exactly one namevalue"))

    (let ((name (make-construct 'NameC 
				:start-revision start-revision
				:topic top
				:charvalue namevalue
				:instance-of instance-of
				:item-identifiers item-identifiers
				:themes themes)))
      (loop for variant-elem across (xpath-child-elems-by-qname name-elem *xtm2.0-ns* "variant")
	 do (from-variant-elem variant-elem name start-revision :xtm-id xtm-id))
      (set-reifier name-elem name))))


(defun from-resourceX-elem (parent-elem)
  "handles the following problem: { resourceRef | resourceData }
   and returns a list with a data and a type node"
  (when parent-elem
       (let ((data
	      (let
		  ((href 
		    (get-attribute
		     (xpath-single-child-elem-by-qname
		      parent-elem
		      *xtm2.0-ns* "resourceRef") "href")))
		(if href
		    href
		    (xpath-fn-string 
		     (xpath-single-child-elem-by-qname
		      parent-elem
		      *xtm2.0-ns* "resourceData")))))
	     (type
	      (let
		  ((resourcedata-elem
		    (xpath-single-child-elem-by-qname
		     parent-elem
		     *xtm2.0-ns* "resourceData")))
		(if resourcedata-elem
		    (let ((attr (get-attribute resourcedata-elem "datatype")))
		      (if attr
			  attr		
			  "http://www.w3.org/2001/XMLSchema#string"))
		    "http://www.w3.org/2001/XMLSchema#anyURI")))) ;the datatype of resourceRefs is IRI, cf. TMDM 4.4 and 5.6
	 (unless data
	   (error "from-resourceX-elem: one of resourceRef or resourceData must be set"))
	 (list :data data :type type))))


(defun from-variant-elem (variant-elem name start-revision &key (xtm-id *current-xtm*))
  "Generate a VariantC object from a variant element
   variant = element variant { reifiable, scope, (resourceRef | resourceData) }"
  (declare (dom:element variant-elem))
  (declare (optimize (debug 3)))
  (declare (NameC name))
  (let 
      ((item-identifiers (make-identifiers 'ItemIdentifierC variant-elem "itemIdentity" start-revision))
       ;;all themes of the parent name element are inherited to the variant elements
       (themes (append
		(from-scope-elem (xpath-single-child-elem-by-qname variant-elem *xtm2.0-ns* "scope") :xtm-id xtm-id)
		(themes name)))
       (variant-value (from-resourceX-elem variant-elem)))
    (unless variant-value
      (error "VariantC: one of resourceRef and resourceData must be set"))
       
       (let ((variant (make-construct 'VariantC
				      :start-revision start-revision
				      :item-identifiers item-identifiers
				      :themes themes
				      :charvalue (getf variant-value :data)
				      :datatype (getf variant-value :type)
				      :name name)))
	 (set-reifier variant-elem variant))))
		           

(defun from-occurrence-elem (occ-elem top start-revision &key (xtm-id *current-xtm*))
  "Generate an OccurrenceC object from an occurrence element
occurrence = element occurrence { reifiable,
  type, scope?, ( resourceRef | resourceData ) }"
  (declare (dom:element occ-elem))
  (declare (TopicC top))
  (declare (integer start-revision))

  (let
      ((themes
        (from-scope-elem (xpath-single-child-elem-by-qname 
                          occ-elem 
                          *xtm2.0-ns* "scope")))
       (item-identifiers
        (make-identifiers 'ItemIdentifierC occ-elem "itemIdentity" start-revision))
       (instance-of 
        (from-type-elem (xpath-single-child-elem-by-qname 
                          occ-elem 
                          *xtm2.0-ns* "type") :xtm-id xtm-id))
       (occurrence-value (from-resourceX-elem occ-elem)))
    (unless occurrence-value
      (error "OccurrenceC: one of resourceRef and resourceData must be set"))
    (let ((occurrence (make-construct 'OccurrenceC 
				      :start-revision start-revision
				      :topic top
				      :themes themes
				      :item-identifiers item-identifiers
				      :instance-of instance-of
				      :charvalue (getf occurrence-value :data)
				      :datatype (getf occurrence-value :type))))
      (set-reifier occ-elem occurrence))))
    
    

(defun from-topic-elem-to-stub (topic-elem start-revision
				&key 
				(xtm-id d:*current-xtm*))
  "Creates the pure stub of a topic together with topicid, PSI and
subject locators. Merges new topic stubs with existing stubs if
applicable"
  (declare (dom:element topic-elem))
  (declare (integer start-revision))
  ;(declare (optimize (debug 3)))
  (elephant:ensure-transaction (:txn-nosync t) 
    (let 
        ((itemidentifiers
          (make-identifiers 'ItemIdentifierC topic-elem "itemIdentity" start-revision))
         (subjectidentifiers
          (make-identifiers 'PersistentIdC topic-elem "subjectIdentifier" start-revision))
         (subjectlocators
          (make-identifiers 'SubjectLocatorC topic-elem "subjectLocator" start-revision)))
      
     
      (make-construct 'TopicC :start-revision start-revision
                      :item-identifiers itemidentifiers
                      :locators subjectlocators
                      :psis subjectidentifiers
                      :topicid (get-attribute topic-elem "id")
                      :xtm-id xtm-id))))
          

(defun merge-topic-elem (topic-elem start-revision
                         &key 
                         tm
                         (xtm-id *current-xtm*))
  "Adds further elements (names, occurrences) and instanceOf
associations to the topic"
  ;TODO: solve merging through reifying
  (declare (dom:element topic-elem))
  (declare (integer start-revision))
  (declare (TopicMapC tm))
  ;(format t "xtm-id: ~a current-xtm: ~a revision: ~a~&" xtm-id *current-xtm* start-revision)
  (elephant:ensure-transaction (:txn-nosync t) 
    (let
        ((top  ;retrieve the already existing topic stub
          (get-item-by-id
           (get-attribute topic-elem "id") 
           :xtm-id xtm-id :revision start-revision)))
      (let
	  ((instanceof-topicrefs
	    (map 'list 
               #'get-topicref-uri
               (xpath-select-location-path
                topic-elem
                '((*xtm2.0-ns* "instanceOf")
                  (*xtm2.0-ns* "topicRef"))))))
      (unless top
        (error "topic ~a could not be found" (get-attribute topic-elem "id")))
      (map 'list
       (lambda
	   (name-elem)
	 (from-name-elem name-elem top start-revision :xtm-id xtm-id))
       (xpath-child-elems-by-qname 
	topic-elem 
	*xtm2.0-ns* "name"))
      (map 'list
       (lambda
	   (occ-elem)
	 (from-occurrence-elem occ-elem top start-revision :xtm-id xtm-id))
       (xpath-child-elems-by-qname 
	topic-elem 
	*xtm2.0-ns* "occurrence"))

      ;this is a very special process
      (dolist (topicref instanceof-topicrefs)
        (create-instanceof-association topicref top start-revision
                                       :tm tm
                                       :xtm-id xtm-id))
      (add-to-topicmap tm top)
      top))))


(defun from-role-elem (role-elem start-revision &key (xtm-id *current-xtm*))
  "Constructs a tuple of (instance-of, player, item-identifiers) from
a role element and returns it role = element role { reifiable, type,
topicRef }"
  (declare (dom:element role-elem))
  (declare (integer start-revision))
  (elephant:ensure-transaction (:txn-nosync t) 
    (let 
        ((item-identifiers
          (make-identifiers 'ItemIdentifierC role-elem "itemIdentity" start-revision))
         (instance-of
          (from-type-elem 
           (xpath-single-child-elem-by-qname
            role-elem
            *xtm2.0-ns*
            "type") :xtm-id xtm-id))
         (player
          (get-item-by-id
           (get-topicref-uri 
            (xpath-single-child-elem-by-qname 
             role-elem
             *xtm2.0-ns*
             "topicRef")) :xtm-id xtm-id))
	 (reifier-uri
	  (let ((value (get-attribute role-elem "reifier")))
	    (if (and (stringp value)
		     (> (length value) 0))
		value
		nil))))
;      (unless (and player instance-of)
;        (error "Role in association not complete"))
      (unless player ;instance-of will be set later - if there is no one
        (error "Role in association with topicref ~a not complete" (get-topicref-uri 
            (xpath-single-child-elem-by-qname 
             role-elem
             *xtm2.0-ns*
             "topicRef"))))
      (list :reifier-uri reifier-uri
	    :instance-of instance-of
	    :player player
	    :item-identifiers item-identifiers))))


(defun from-association-elem (assoc-elem start-revision
                              &key
                              tm
                              (xtm-id *current-xtm*))
  "Constructs an AssociationC object from an association element
   association = element association { reifiable, type, scope?, role+ }"
  (declare (dom:element assoc-elem))
  (declare (integer start-revision))
  (declare (TopicMapC tm))
  (elephant:ensure-transaction (:txn-nosync t) 
    (let 
        ((item-identifiers 
          (make-identifiers 'ItemIdentifierC assoc-elem "itemIdentity" start-revision))
         (instance-of
          (from-type-elem 
           (xpath-single-child-elem-by-qname 
            assoc-elem 
            *xtm2.0-ns* "type") :xtm-id xtm-id))
         (themes
          (from-scope-elem 
           (xpath-single-child-elem-by-qname 
            assoc-elem 
            *xtm2.0-ns* "scope")))
         (roles ;a list of tuples
          (map 'list 
               (lambda 
                   (role-elem)
                 (from-role-elem role-elem start-revision :xtm-id xtm-id))
               (xpath-child-elems-by-qname 
                assoc-elem
                *xtm2.0-ns* "role"))))
      (setf roles (set-standard-role-types roles)); sets standard role types if there are missing some of them
      (let ((assoc (add-to-topicmap
		    tm 
		    (make-construct 'AssociationC
				    :start-revision start-revision
				    :item-identifiers item-identifiers
				    :instance-of instance-of
				    :themes themes
				    :roles roles))))
	(map 'list #'(lambda(assoc-role)
		       (map 'list #'(lambda(list-role)
				      (when (and (eql (instance-of assoc-role)
						      (getf list-role :instance-of))
						 (eql (player assoc-role)
						      (getf list-role :player))
						 (getf list-role :reifier-uri))
					(add-reifier assoc-role (getf list-role :reifier-uri) t)))
			    roles))
	     (roles assoc))
	(set-reifier assoc-elem assoc)))))



(defun get-topic-elems (xtm-dom)
  (xpath-child-elems-by-qname xtm-dom
                              *xtm2.0-ns* "topic"))


(defun get-association-elems (xtm-dom)
  (xpath-child-elems-by-qname  xtm-dom
                               *xtm2.0-ns* "association"))

(defun import-only-topics
    (xtm-dom
     &key    
     (tm-id (error "you must provide a stable identifier (PSI-style) for this TM"))
     (xtm-id d:*current-xtm*)
     (revision (get-revision)))
  (with-tm (revision xtm-id tm-id)
    (let
        ((topic-vector (get-topic-elems xtm-dom)))
      (loop for top-elem across topic-vector do
           (add-to-topicmap 
            tm  
            (from-topic-elem-to-stub top-elem revision 
                                     :xtm-id xtm-id))))))

(defun importer (xtm-dom 
                 &key 
                 (tm-id (error "you must provide a stable identifier (PSI-style) for this TM"))
                 (xtm-id d:*current-xtm*)
                 (revision (get-revision)))
  (declare (dom:element xtm-dom))
  (declare (integer revision))        ;all topics that are imported in one go share the same revision
  (assert elephant:*store-controller*)
  (with-writer-lock
    (with-tm (revision xtm-id tm-id)
      (let
	  ((topic-vector (get-topic-elems xtm-dom))
	   (assoc-vector (get-association-elems xtm-dom)))
	(loop for top-elem across topic-vector do
	     (from-topic-elem-to-stub top-elem revision 
				      :xtm-id xtm-id))
	(loop for top-elem across topic-vector do
	     (format t "t")
	     (merge-topic-elem top-elem revision 
			       :tm tm
			       :xtm-id xtm-id))
	(loop for assoc-elem across assoc-vector do
	     (format t "a")
	     (from-association-elem assoc-elem revision 
				    :tm tm
				    :xtm-id xtm-id))))))
