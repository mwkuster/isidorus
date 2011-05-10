;;+-----------------------------------------------------------------------------
;;+  Isidorus
;;+  (c) 2008-2010 Marc Kuester, Christoph Ludwig, Lukas Georgieff
;;+
;;+  Isidorus is freely distributable under the LLGPL license.
;;+  You can find a detailed description in trunk/docs/LLGPL-LICENSE.txt and
;;+  trunk/docs/LGPL-LICENSE.txt.
;;+-----------------------------------------------------------------------------

(defpackage :json-importer
  (:use :cl :json :datamodel :xtm-importer :constants)
  (:export :import-from-isidorus-json
	   :*json-xtm*))

(in-package :json-importer)

;; the json schema for our datamodel is in "docs/xtm_json.txt"


(defvar *json-xtm* "json-xtm"); Represents the currently active TM of the JSON-Importer


(defun import-from-isidorus-json(json-string &key (xtm-id *json-xtm*))
  "creates all objects (topics, topic stubs, associations)
   of the passed json-decoded-list (=fragment)"
  (declare (type (or string null) json-string xtm-id))
  (when json-string
    (let ((fragment-values
	   (get-fragment-values-from-json-list
	    (json:decode-json-from-string json-string))))
      (let ((topic-values (getf fragment-values :topic))
	    (topicStubs-values (getf fragment-values :topicStubs))
	    (associations-values (getf fragment-values :associations))
	    (rev (get-revision)) ; creates a new revision, equal for all elements of the passed fragment
	    (tm-ids (getf fragment-values :tm-ids)))
	(unless tm-ids
	  (error "From import-from-isidorus-json(): tm-ids must be set"))
	(let ((psi-of-topic
	       (let ((psi-uris (getf topic-values :subjectIdentifiers)))
		 (when psi-uris
		   (first psi-uris)))))
	  (elephant:ensure-transaction (:txn-nosync nil) 
	    (xtm-importer:with-tm (rev xtm-id (first tm-ids))
	      (loop for topicStub-values in
		   (append topicStubs-values (list topic-values))
		 do (json-to-stub topicStub-values rev :tm xtm-importer::tm
				  :xtm-id xtm-id))
	      (json-merge-topic topic-values rev :tm xtm-importer::tm :xtm-id xtm-id)
	      (loop for association-values in associations-values
		 do (json-to-association association-values rev
					 :tm xtm-importer::tm))))
	  (when psi-of-topic
	    (create-latest-fragment-of-topic psi-of-topic)))))))


(defun json-to-association (json-decoded-list start-revision
			    &key tm)
  "creates an association element of the passed json-decoded-list"
  (elephant:ensure-transaction (:txn-nosync t) 
    (let 
        ((item-identifiers 
          (map 'list #'(lambda(uri)
			 (make-identifier 'ItemIdentifierC uri start-revision))
	       (getf json-decoded-list :itemIdentities)))
         (instance-of
          (psis-to-topic (getf json-decoded-list :type) :revision start-revision))
         (themes
          (json-to-scope (getf json-decoded-list :scopes) start-revision))
         (roles 
          (map 'list #'(lambda(role-values)
			 (json-to-role role-values start-revision))
	       (getf json-decoded-list :roles))))
      (declare (list json-decoded-list))
      (declare (integer start-revision))
      (declare (TopicMapC tm))
      (setf roles (xtm-importer::set-standard-role-types roles start-revision))
      (add-to-tm tm 
		 (make-construct 'AssociationC
				 :start-revision start-revision
				 :item-identifiers item-identifiers
				 :instance-of instance-of
				 :themes themes
				 :roles roles)))))
    

(defun json-to-role (json-decoded-list start-revision)
  "creates a role element"
  (when json-decoded-list
  (elephant:ensure-transaction (:txn-nosync t) 
    (let
        ((item-identifiers
          (map 'list #'(lambda(uri)
			 (make-identifier 'ItemIdentifierC uri start-revision))
	       (getf json-decoded-list :itemIdentities)))
         (instance-of
          (psis-to-topic (getf json-decoded-list :type) :revision start-revision))
         (player
	  (psis-to-topic (getf json-decoded-list :topicRef)
			 :revision start-revision)))
      (declare (list json-decoded-list))
      (declare (integer start-revision))
      (unless player
        (error "Role in association with topicref ~a not complete"
	       (getf json-decoded-list :topicRef)))
      (list :instance-of instance-of
	    :player player
	    :item-identifiers item-identifiers
	    :start-revision start-revision)))))


(defun json-merge-topic (json-decoded-list start-revision
                         &key tm (xtm-id *json-xtm*))
  "merges the a topic by setting the name, occurrence and instanceOf
   elements from the json-decoded-list"
  (when json-decoded-list
    (elephant:ensure-transaction (:txn-nosync t) 
      (let ((top
	     (d:get-item-by-id
	      (getf json-decoded-list :id)
	      :revision start-revision
	      :xtm-id xtm-id)))
	(declare (list json-decoded-list))
	(declare (integer start-revision))
	(declare (TopicMapC tm))
	(unless top
	  (error "topic ~a could not be found" (getf json-decoded-list :id)))
	(let ((instanceof-topics
	       (remove-duplicates
		(map 'list
		     #'(lambda(psis)
			 (psis-to-topic psis :revision start-revision))
		     (getf json-decoded-list :instanceOfs)))))

	  (loop for name-values in (getf json-decoded-list :names)
	     do (json-to-name name-values top start-revision))
	  (loop for occurrence-values in (getf json-decoded-list :occurrences)
	     do (json-to-occurrence occurrence-values top start-revision))
	  (dolist (instanceOf-top instanceof-topics)
	    (json-create-instanceOf-association instanceOf-top top start-revision
						:tm tm))
	  top)))))


(defun json-to-stub(json-decoded-list start-revision &key tm (xtm-id *json-xtm*))
  "creates a topic stub from the passed json-decoded list"
  (when json-decoded-list
    (elephant:ensure-transaction (:txn-nosync t) 
      (let ((item-identifiers
	     (map 'list #'(lambda(uri)
			    (make-identifier 'ItemIdentifierC uri start-revision))
		  (getf json-decoded-list :itemIdentities)))
	    (subject-identifiers
	     (map 'list #'(lambda(uri)
			    (make-identifier 'PersistentIdC uri start-revision))
		  (getf json-decoded-list :subjectIdentifiers)))
	    (subject-locators
	     (map 'list #'(lambda(uri)
			    (make-identifier 'SubjectLocatorC uri start-revision))
		  (getf json-decoded-list :subjectLocators)))
	    (topic-ids
	     (when (getf json-decoded-list :id)
	       (list
		(make-construct 'TopicIdentificationC
				:uri (getf json-decoded-list :id)
				:xtm-id xtm-id)))))
	;; all topic stubs has to be added top a topicmap object in this method
	;; becuase the only one topic that is handled in "json-merge-topic"
	;; is the main topic of the fragment
	(let ((top
	       (make-construct 'TopicC :start-revision start-revision
				       :item-identifiers item-identifiers
				       :locators subject-locators
				       :psis subject-identifiers
				       :topic-identifiers topic-ids)))
	  (add-to-tm tm top)
	  top)))))
	

(defun json-to-occurrence (json-decoded-list top start-revision)
  "Creates an occurrence element"
  (when json-decoded-list
    (let
      ((themes
        (json-to-scope (getf json-decoded-list :scopes) start-revision))
       (item-identifiers
	(map 'list #'(lambda(uri)
		       (make-identifier 'ItemIdentifierC uri start-revision))
	     (getf json-decoded-list :itemIdentities)))
       (instance-of 
        (psis-to-topic (getf json-decoded-list :type) :revision start-revision))
       (occurrence-value
	(json-to-resourceX json-decoded-list)))
      
      (unless occurrence-value
	(error "OccurrenceC: one of resourceRef and resourceData must be set"))
      (make-construct 'OccurrenceC 
		      :start-revision start-revision
		      :parent top
		      :themes themes
		      :item-identifiers item-identifiers
		      :instance-of instance-of
		      :charvalue (getf occurrence-value :data)
		      :datatype (getf occurrence-value :type)))))


(defun make-identifier (classsymbol uri start-revision)
  "creates an instance of a PersistentIdc, SubjectlocatorC or
   ItemIdentifierC"
  (declare (symbol classsymbol))
  (declare (string uri))
  (declare (integer start-revision))
  (make-construct classsymbol
		  :uri uri
		  :start-revision start-revision))


(defun json-to-scope (json-decoded-list start-revision)
  "Generate set of themes (= topics) from this scope element and
   return that set. If the input is nil, the list of themes is empty"
  (when json-decoded-list
    (let ((tops
	   (map 'list #'(lambda(psis)
			  (psis-to-topic psis :revision start-revision))
		json-decoded-list)))
      (declare (list json-decoded-list))
      (unless (>= (length tops) 1)
        (error "need at least one topic in a scope"))
      tops)))


(defun psis-to-topic(psis &key (revision *TM-REVISION*))
  "searches for a topic of the passed psis-list describing
   exactly one topic"
  (declare (list psis)
	   (type (or integer null) revision))
  (when psis
    (let ((top
	   (let ((psi
		  (loop for uri in psis
		     when (elephant:get-instance-by-value
			   'd:PersistentIdC 'd:uri uri)
		     return (elephant:get-instance-by-value
			     'd:PersistentIdC 'd:uri uri))))
	     (when psi
	       (d:identified-construct psi :revision revision)))))
      (unless top
	(error (make-condition 'missing-reference-error
			       :message (format nil "psis-to-topic: could not resolve reference ~a" psis))))
      top)))


(defun json-to-name (json-decoded-list top start-revision)
  "creates a name element (NameC) of the passed json-decoded-list"
  (when json-decoded-list
    (let ((item-identifiers
	   (map 'list #'(lambda(uri)
			  (make-identifier 'ItemIdentifierC uri start-revision))
		(getf json-decoded-list :itemIdentities)))
	  (namevalue (getf json-decoded-list :value))
	  (themes
	   (json-to-scope (getf json-decoded-list :scopes) start-revision))
	  (instance-of
	   (psis-to-topic (getf json-decoded-list :type) :revision start-revision)))
      (unless namevalue
        (error "A name must have exactly one namevalue"))
      (let ((name (make-construct
		   'NameC 
		   :start-revision start-revision
		   :parent top
		   :charvalue namevalue
		   :instance-of (if instance-of
				    instance-of
				    (get-item-by-psi *topic-name-psi*
						     :revision start-revision
						     :error-if-nil t))
		   :item-identifiers item-identifiers
		   :themes themes)))
	(loop for variant in (getf json-decoded-list :variants)
	   do (json-to-variant variant name start-revision))
	name))))


(defun json-to-variant(json-decoded-list name start-revision)
  "creates a variant element (VariantC) of the passed json-decoded-list"
  (when json-decoded-list
    (let ((item-identifiers
	   (map 'list #'(lambda(uri)
			  (make-identifier 'ItemIdentifierC uri start-revision))
		(getf json-decoded-list :itemIdentities)))
	  (themes
	   (remove-duplicates
	    (append (d:themes name)
		    (json-to-scope (getf json-decoded-list :scopes)
				   start-revision))))
	  (variant-value
	   (json-to-resourceX json-decoded-list)))
      (declare (list json-decoded-list))
      (make-construct 'VariantC
		      :start-revision start-revision
		      :item-identifiers item-identifiers
		      :themes themes
		      :charvalue (getf variant-value :data)
		      :datatype (getf variant-value :type)
		      :parent name))))


(defun json-to-resourceX(json-decoded-list)
  "creates a resourceRef or resourceData element"
  (when json-decoded-list
    (let ((resourceRef
	   (getf json-decoded-list :resourceRef))
	  (resourceData
	   (getf json-decoded-list :resourceData)))
      (declare (list json-decoded-list))
      (let ((value
	     (if resourceRef
		 (list :data resourceRef
		       :type "http://www.w3.org/2001/XMLSchema#anyURI")
		 (list :data (getf resourceData :value)
		       :type (if (getf resourceData :datatype)
				 (getf resourceData :datatype)
				 "http://www.w3.org/2001/XMLSchema#string")))))
	(unless (getf value :data)
	  (error "json-to-resourceX: one of resourceRef or resourceData must be set"))
	value))))


(defun json-create-instanceOf-association (supertype player2-obj start-revision 
                                      &key tm)
  "handle the instanceOf element. The instanceOf element is different
  from all the others in that it is not modelled one to one, but
  following the suggestion of the XTM 2.0 spec (4.9) and the
  TMDM (7.2) as an association"
  (declare (TopicC supertype player2-obj)
	   (TopicMapC tm))
  (let
      ((associationtype 
        (get-item-by-psi constants:*type-instance-psi* :revision start-revision))
       (roletype1
        (get-item-by-psi constants:*type-psi* :revision start-revision))
       (roletype2
        (get-item-by-psi constants:*instance-psi* :revision start-revision))
       (player1 supertype))
    (unless (and associationtype roletype1 roletype2)
      (error "Error in the creation of an instanceof association: core topics are missing"))
    (add-to-tm tm associationtype)
    (add-to-tm tm roletype1)
    (add-to-tm tm roletype2)
    (add-to-tm 
     tm
     (make-construct 
      'AssociationC
      :item-identifiers nil
      :themes nil
      :start-revision start-revision
      :instance-of associationtype
      :roles (list (list :instance-of roletype1
			 :player player1
			 :start-revision start-revision)
                   (list :instance-of roletype2
			 :player player2-obj
			 :start-revision start-revision))))))


(defun get-fragment-values-from-json-list(json-decoded-list)
  "returns all fragment values of the passed json-decoded-list
   as a named list"
  (when json-decoded-list
    (let ((topic nil)
	  (topicStubs nil)
	  (associations nil)
	  (tm-ids nil))
      (declare (list json-decoded-list))
      (loop for j-elem in json-decoded-list
	 do (cond
	      ((string= (car j-elem) :topic)
	       (setf topic (cdr j-elem)))
	      ((string= (car j-elem) :topic-Stubs)
	       (setf topicStubs (cdr j-elem)))
	      ((string= (car j-elem) :associations)
	       (setf associations (cdr j-elem)))
	      ((string= (car j-elem) :tm-Ids)
	       (setf tm-ids (cdr j-elem)))
	      (t
	       (error "json-importer:get-fragment-values-from-json-string:
                       bad item-specifier found in json-list"))))
      (unless topic
	(error "json-importer:get-fragment-values-from-json-string: the element topic must be set"))
      (unless (= (length tm-ids) 1)
	(error "There must be given exactly one tm-id in the tm-ids list"))
      (let ((topic-list (get-topic-values-from-json-list topic))
	    (topicStubs-list (map 'list #'get-topicStub-values-from-json-list topicStubs))
	    (associations-list (map 'list #'get-association-values-from-json-list associations)))
	(list :topic topic-list
	      :topicStubs topicStubs-list
	      :associations associations-list
	      :tm-ids tm-ids)))))


(defun get-topicStub-values-from-json-list (json-decoded-list)
  "returns all topicStub values of the passed json-decoded-list
   as a named list"
  (when json-decoded-list
    (let ((id nil)
	  (itemIdentities nil)
	  (subjectLocators nil)
	  (subjectIdentifiers nil))
      (declare (list json-decoded-list))
      (loop for j-elem in json-decoded-list
	 do (cond
	      ((string= (car j-elem) :ID)
	       (setf id (cdr j-elem)))
	      ((string= (car j-elem) :item-Identities)
	       (setf itemIdentities (cdr j-elem)))
	      ((string= (car j-elem) :subject-Locators)
	       (setf subjectLocators (cdr j-elem)))
	      ((string= (car j-elem) :subject-Identifiers)
	       (setf subjectIdentifiers (cdr j-elem)))
	      (t
	       (error "json-importer:get-topicStub-values-from-json-string:
                       bad item-specifier found in json-list"))))
       (unless subjectIdentifiers
	(error "json-importer:get-topicStub-values-from-json-string: the element subjectIdentifiers mus be set!"))
      (unless id
	(error "json-importer:get-topic-valuesStub-from-json-string: the element id must be set"))
      (list :id id
	    :itemIdentities itemIdentities
	    :subjectLocators subjectLocators
	    :subjectIdentifiers subjectIdentifiers))))
      

(defun get-topic-values-from-json-list (json-decoded-list)
  "extracts all values of the passed json-list and
   returns them as a named list"
  (when json-decoded-list
    (let ((id nil)
	  (itemIdentities nil)
	  (subjectLocators nil)
	  (subjectIdentifiers nil)
	  (instanceOfs nil)
	  (names nil)
	  (occurrences nil))
      (declare (list json-decoded-list))
      (loop for j-elem in json-decoded-list
	 do (cond
	      ((string= (car j-elem) :ID)
	       (setf id (cdr j-elem)))
	      ((string= (car j-elem) :item-Identities) ;json-decoder transforms camelcase to '-' from
	       (setf itemIdentities (cdr j-elem)))
	      ((string= (car j-elem) :subject-Locators)
	       (setf subjectLocators (cdr j-elem)))
	      ((string= (car j-elem) :subject-Identifiers)
	       (setf subjectIdentifiers (cdr j-elem)))
	      ((string= (car j-elem) :instance-Ofs)
	       (setf instanceOfs (cdr j-elem)))
	      ((string= (car j-elem) :names)
	       (setf names (cdr j-elem)))
	      ((string= (car j-elem) :occurrences)
	       (setf occurrences (cdr j-elem)))
	      (t
	       (error "json-importer:get-topic-values-from-json-string:
                       bad item-specifier found in json-list ~a" (car j-elem)))))
      (unless subjectIdentifiers
	(error "json-importer:get-topic-values-from-json-string: the element subjectIdentifiers must be set!"))
      (unless id
	(error "json-importer:get-topic-values-from-json-string: the element id must be set"))
      (let ((names-list (map 'list #'get-name-values-from-json-list names))
	    (occurrences-list (map 'list #'get-occurrence-values-from-json-list occurrences)))
	(list :id id
	      :itemIdentities itemIdentities
	      :subjectLocators subjectLocators
	      :subjectIdentifiers subjectIdentifiers
	      :instanceOfs instanceOfs
	      :names names-list
	      :occurrences occurrences-list)))))


(defun get-name-values-from-json-list (json-decoded-list)
  "returns all element values of a name element as
   a named list"
  (when json-decoded-list
    (let ((itemIdentities nil)
	  (type nil)
	  (scopes nil)
	  (value nil)
	  (variants nil))
      (declare (list json-decoded-list))
      (loop for j-elem in json-decoded-list
	 do (cond
	      ((string= (car j-elem) :item-Identities)
	       (setf itemIdentities (cdr j-elem)))
	      ((string= (car j-elem) :type)
	       (setf type (cdr j-elem)))
	      ((string= (car j-elem) :scopes)
	       (setf scopes (cdr j-elem)))
	      ((string= (car j-elem) :value)
	       (setf value (cdr j-elem)))
	      ((string= (car j-elem) :variants)
	       (setf variants (cdr j-elem)))
	      (t
	       (error "json-importer:get-name-values-from-json-list:
                       bad item-specifier found in json-list"))))
      (unless value
	(error "json-importer:get-name-values-from-json-list: value must be set"))
      (let ((variants-list (map 'list #'get-variant-values-from-json-list variants)))
	(list :itemIdentities itemIdentities
	      :type type
	      :scopes scopes
	      :value value
	      :variants variants-list)))))
	    

(defun get-variant-values-from-json-list (json-decoded-list)
  "returns all element values of a variant element as
   a named list"
  (when json-decoded-list
    (let ((itemIdentities nil)
	  (scopes nil)
	  (resourceRef nil)
	  (resourceData nil))
      (declare (list json-decoded-list))
      (loop for j-elem in json-decoded-list
	 do (cond
	      ((string= (car j-elem) :item-Identities)
	       (setf itemIdentities (cdr j-elem)))
	      ((string= (car j-elem) :scopes)
	       (setf scopes (cdr j-elem)))
	      ((string= (car j-elem) :resource-Ref)
	       (setf resourceRef (cdr j-elem)))
	      ((string= (car j-elem) :resource-Data)
	       (setf resourceData (cdr j-elem)))
	      (t
	       (error "json-importer:get-variant-values-from-json-list:
                       bad item-specifier found in json-list"))))
      (when (or (and (not resourceRef)
		     (not resourceData))
		(and resourceRef resourceData))
	(error "json-importer:get-variant-values-from-json-list: ONE of the elements
                  resourceRef or resourceData must be set"))
      (let ((resourceData-list (get-resourceData-values-from-json-list resourceData)))
	(list :itemIdentities itemIdentities  
	      :scopes scopes
	      :resourceRef resourceRef
	      :resourceData resourceData-list)))))
  

(defun get-resourceData-values-from-json-list (json-decoded-list)
  "returns the resourceData value and the datatype value, if there
   is no datatype given, there will be set the standard type string"
  (when json-decoded-list
    (let ((value nil)
	  (datatype nil))
      (declare (list json-decoded-list))
      (loop for j-elem in json-decoded-list
	 do (cond
	      ((string= (car j-elem) :value)
	       (setf value (cdr j-elem)))
	      ((string= (car j-elem) :datatype)
	       (setf datatype (cdr j-elem)))
	      (t
	       (error "json-importer:get-resourceData-values-from-json-list:
                       bad item-specifier found in json-list"))))
      (unless value
	(error "json-importer:get-resourceData-values-from-json-list: resourceData must have a value"))
      (list :value value
	    :datatype (if datatype datatype "http://www.w3.org/2001/XMLSchema#string")))))


(defun get-occurrence-values-from-json-list (json-decoded-list)
  "returns all occurrence values of the passed json-list as
   a named list"
  (when json-decoded-list
    (let ((itemIdentities nil)
	  (type nil)
	  (scopes nil)
	  (resourceRef nil)
	  (resourceData nil))
      (declare (list json-decoded-list))
      (loop for j-elem in json-decoded-list
	 do (cond
	      ((string= (car j-elem) :item-Identities)
	       (setf itemIdentities (cdr j-elem)))
	      ((string= (car j-elem) :type)
	       (setf type (cdr j-elem)))
	      ((string= (car j-elem) :scopes)
	       (setf scopes (cdr j-elem)))
	      ((string= (car j-elem) :resource-Ref)
	       (setf resourceRef (cdr j-elem)))
	      ((string= (car j-elem) :resource-Data)
	       (setf resourceData (cdr j-elem)))
	      (t
	       (error "json-importer:get-occurrence-values-from-json-list:
                       bad item-specifier found in json-list"))))
      (when (or (and (not resourceRef)
		     (not resourceData))
		(and resourceRef resourceData))
	(error "json-importer:get-occurrence-values-from-json-list: ONE of the elements
                  resourceRef or resourceData must be set"))
      (unless type
	(error "json-importer:get-occurrence-values-from-json-list: type must be set"))
      (let ((resourceData-list (get-resourceData-values-from-json-list resourceData)))
	(list :itemIdentities itemIdentities
	      :type type
	      :scopes scopes
	      :resourceRef resourceRef
	      :resourceData resourceData-list)))))


(defun get-association-values-from-json-list (json-decoded-list)
  "extracts all values of the passed json-list and
   returns them as a named list"
  (when json-decoded-list
    (let ((itemIdentities nil)
	  (type nil)
	  (scopes nil)
	  (roles nil))
      (declare (list json-decoded-list))
      (loop for j-elem in json-decoded-list
	 do (cond
	      ((string= (car j-elem) :item-Identities)
	       (setf itemIdentities (cdr j-elem)))
	      ((string= (car j-elem) :type)
	       (setf type (cdr j-elem)))
	      ((string= (car j-elem) :scopes)
	       (setf scopes (cdr j-elem)))
	      ((string= (car j-elem) :roles)
	       (setf roles (cdr j-elem)))
	      (t
	       (error "json-importer:get-association-values-from-json-list:
                       bad item-specifier found in json-list"))))
      (unless (and type roles)
	(error "json-importer:get-occurrence-values-from-json-list: type and role must be set"))
      (let ((roles-list (map 'list #'get-role-values-from-json-list roles)))
	(list :itemIdentities itemIdentities
	      :type type
	      :scopes scopes
	      :roles roles-list)))))
  

(defun get-role-values-from-json-list (json-decoded-list)
  "extracts all values of the passed json-list and
   returns them as a named list"
  (when json-decoded-list
    (let ((itemIdentities nil)
	  (type nil)
	  (topicRef nil))
      (declare (list json-decoded-list))
      (loop for j-elem in json-decoded-list
	 do (cond
	      ((string= (car j-elem) :item-Identities)
	       (setf itemIdentities (cdr j-elem)))
	      ((string= (car j-elem) :type)
	       (setf type (cdr j-elem)))
	      ((string= (car j-elem) :topic-Ref)
	       (setf topicRef (cdr j-elem)))
	      (t
	       (error "json-importer:get-role-values-from-json-list:
                       bad item-specifier found in json-list"))))
      (unless (and type topicRef)
	(error "json-importer:get-occurrence-values-from-json-list: type and topicRef must be set"))
      (list :itemIdentities itemIdentities
	    :type type
	    :topicRef topicRef))))


