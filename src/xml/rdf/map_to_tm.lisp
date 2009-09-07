;;+-----------------------------------------------------------------------------
;;+  Isidorus
;;+  (c) 2008-2009 Marc Kuester, Christoph Ludwig, Lukas Giessmann
;;+
;;+  Isidorus is freely distributable under the LGPL license.
;;+  You can find a detailed description in trunk/docs/LGPL-LICENSE.txt.
;;+-----------------------------------------------------------------------------
(in-package :rdf-importer)

(defun map-to-tm (tm-id start-revision
		  &key (document-id *document-id*))
  (let ((topics-to-map (get-isi-topics tm-id start-revision
				       :document-id document-id))
	(associations-to-map (get-isi-topics
			      tm-id start-revision
			      :document-id document-id
			      :type-psi *tm2rdf-association-type-uri*)))
    (let ((mapped-topics
	   (map 'list #'(lambda(top)
			  (map-isi-topic top start-revision))
		topics-to-map))
	  (mapped-associations associations-to-map))
		
      (append mapped-topics mapped-associations)
    ;check-for-duplicate-identifiers
    ;delete-construct:
    ;    *item-identifier-property
    ;    *subject-identifier-property
    ;    *subject-locator-proeprty*
    ;    *topic-type
    ;    *occurrence-type
    ;    *occurrence-property
    ;    *name-type
    ;    *name-property
    ;    *variant-type
    ;    *variant-property
    ;    *occurrence-type-property
    ;    *value-property
    ;    *scope-property
    ;    *nametype-property
      )))


(defun map-isi-topic(top start-revision)
  "maps a passed topic with all its isidorus:types to a TM topic."
  (declare (integer start-revision))
  (declare(TopicC top))
  (let ((new-psis (map-isi-identifiers
		   top start-revision
		   :id-type-uri *tm2rdf-subjectidentifier-property*))
	(new-locators (map-isi-identifiers
		       top start-revision
		       :id-type-uri *tm2rdf-subjectlocator-property*))
	(new-item-ids (map-isi-identifiers top start-revision))
	(occurrence-topics (get-isi-occurrences top start-revision))
	(name-topics (get-isi-names top start-revision)))
    (bound-subject-identifiers top new-psis)
    (bound-subject-locators top new-locators)
    (bound-item-identifiers top new-item-ids)
    (map 'list #'(lambda(occ-top)
		   (map-isi-occurrence top occ-top start-revision))
	 occurrence-topics)
    (map 'list #'(lambda(name-top)
		   (map-isi-name top name-top start-revision))
	 name-topics))
  top)


(defun get-isi-variants(name-top start-revision)
  "Returns all topics representing a name's variant."
  (declare (TopicC name-top))
  (declare (integer start-revision))
  (let ((variant-assocs
	 (get-associations-by-type name-top start-revision 
				   *tm2rdf-variant-property*
				   *rdf2tm-subject*)))
    (let ((players
	   (get-players-by-role-type variant-assocs start-revision
				     *rdf2tm-object*)))
      (map 'list #'d::delete-construct variant-assocs)
      players)))


(defun map-isi-variant (name variant-top start-revision)
  "Maps the passed variant-topic to a TM variant."
  (declare (TopicC variant-top))
  (declare (NameC name))
  (declare (integer start-revision))
  (let ((ids (map-isi-identifiers variant-top start-revision))
	(scope-assocs
	 (get-associations-by-type
	  variant-top start-revision
	  (concatenate 'string *tm2rdf-ns* "scope")
	  *rdf2tm-subject*))
	(value-type-topic 
	 (get-item-by-psi (concatenate 'string *tm2rdf-ns* "value"))))
    (let ((scopes (get-players-by-role-type
		   scope-assocs start-revision *rdf2tm-object*))
	  (value-and-datatype
	   (let ((value-occ
		  (find-if #'(lambda(occ)
			       (eql (instance-of occ) value-type-topic))
			   (occurrences variant-top))))
	     (if value-occ
		 (list :value (charvalue value-occ)
		       :datatype (datatype value-occ))
		 (list :value ""
		       :datatype *xml-string*)))))
      (elephant:ensure-transaction  (:txn-nosync t)
	(map 'list #'d::delete-construct scope-assocs)
	(d::delete-construct variant-top)
	(make-construct 'VariantC
			:start-revision start-revision
			:item-identifiers ids
			:themes scopes
			:charvalue (getf value-and-datatype :value)
			:datatype (getf value-and-datatype :datatype)
			:name name)))))


(defun map-isi-name (top name-top start-revision)
  "Maps the passed occurrence-topic to a TM occurrence."
  (declare (TopicC top name-top))
  (declare (integer start-revision))
  (let ((err-pref "From map-isi-name(): ")
	(ids (map-isi-identifiers name-top start-revision))
	(type-assocs
	 (get-associations-by-type
	  name-top start-revision
	  (concatenate 'string *tm2rdf-ns* "nametype")
	  *rdf2tm-subject*))
	(scope-assocs
	 (get-associations-by-type
	  name-top start-revision
	  (concatenate 'string *tm2rdf-ns* "scope")
	  *rdf2tm-subject*))
	(value-type-topic 
	 (get-item-by-psi (concatenate 'string *tm2rdf-ns* "value")))
	(variant-topics (get-isi-variants name-top start-revision)))
    (let ((types (get-players-by-role-type
		  type-assocs start-revision *rdf2tm-object*))
	  (scopes (get-players-by-role-type
		   scope-assocs start-revision *rdf2tm-object*))
	  (value 
	   (let ((value-occ
		  (find-if #'(lambda(occ)
			       (eql (instance-of occ) value-type-topic))
			   (occurrences name-top))))
	     (if value-occ
		 (charvalue value-occ)
		 ""))))
      (elephant:ensure-transaction  (:txn-nosync t)
	(map 'list #'d::delete-construct type-assocs)
	(map 'list #'d::delete-construct scope-assocs)
	(when (/= 1 (length types))
	  (error "~aexpect one type topic but found: ~a"
		 err-pref (length types)))
	(let ((name (make-construct 'NameC
				    :start-revision start-revision
				    :topic top
				    :charvalue value
				    :instance-of (first types)
				    :item-identifiers ids
				    :themes scopes)))
	  (map 'list #'(lambda(variant-top)
			 (map-isi-variant name variant-top start-revision))
	       variant-topics)
	  (d::delete-construct name-top)
	  name)))))


(defun get-isi-names(top start-revision)
  "Returns all topics that represents names for the passed top."
  (declare (TopicC top))
  (declare (integer start-revision))
  (let ((assocs (get-associations-by-type
		 top start-revision *tm2rdf-name-property*
		 *rdf2tm-subject*)))
    (let ((occ-tops (get-players-by-role-type
		     assocs start-revision *rdf2tm-object*)))
      (map 'list #'d::delete-construct assocs)
      occ-tops)))


(defun map-isi-occurrence(top occ-top start-revision)
  "Maps all topics that represents occurrences of the passed topic top
   to occurrence objects."
  (declare (TopicC top occ-top))
  (declare (integer start-revision))
  (let ((err-pref "From map-isi-occurrence(): ")
	(ids (map-isi-identifiers occ-top start-revision))
	(type-assocs
	 (get-associations-by-type
	  occ-top start-revision
	  (concatenate 'string *tm2rdf-ns* "occurrencetype")
	  *rdf2tm-subject*))
	(scope-assocs
	 (get-associations-by-type
	  occ-top start-revision
	  (concatenate 'string *tm2rdf-ns* "scope")
	  *rdf2tm-subject*))
	(value-type-topic 
	 (get-item-by-psi (concatenate 'string *tm2rdf-ns* "value"))))
    (let ((types (get-players-by-role-type
		  type-assocs start-revision *rdf2tm-object*))
	  (scopes (get-players-by-role-type
		   scope-assocs start-revision *rdf2tm-object*))
	  (value-and-datatype
	   (let ((value-occ
		  (find-if #'(lambda(occ)
			       (eql (instance-of occ) value-type-topic))
			   (occurrences occ-top))))
	     (if value-occ
		 (list :value (charvalue value-occ)
		       :datatype (datatype value-occ))
		 (list :value ""
		       :datatype *xml-string*)))))
      (elephant:ensure-transaction  (:txn-nosync t)
	(map 'list #'d::delete-construct type-assocs)
	(map 'list #'d::delete-construct scope-assocs)
	(when (/= 1 (length types))
	  (error "~aexpect one type topic but found: ~a"
		 err-pref (length types)))
	(d::delete-construct occ-top)
	(make-construct 'OccurrenceC
			:start-revision start-revision
			:topic top
			:themes scopes
			:item-identifiers ids
			:instance-of (first types)
			:charvalue (getf value-and-datatype :value)
			:datatype (getf value-and-datatype :datatype))))))


(defun get-isi-occurrences(top start-revision)
  "Returns all topics that represents occurrences for the passed top."
  (declare (TopicC top))
  (declare (integer start-revision))
  (let ((assocs (get-associations-by-type
		 top start-revision *tm2rdf-occurrence-property*
		 *rdf2tm-subject*)))
    (let ((occ-tops (get-players-by-role-type
		     assocs start-revision *rdf2tm-object*)))
      (map 'list #'d::delete-construct assocs)
      occ-tops)))


(defun get-isi-topics (tm-id start-revision
		       &key (document-id *document-id*)
		       (type-psi *tm2rdf-topic-type-uri*))
  "Returns all topics of the given tm and revision."
  (let ((type-topic (get-item-by-psi type-psi
				     :revision start-revision)))
    (when type-topic
      (let ((assocs (get-associations-by-type 
		     type-topic start-revision *type-instance-psi*
		     *type-psi*)))
	(let ((isi-topics (get-players-by-role-type
			   assocs start-revision *instance-psi*)))
	  (let ((topics-in-tm
		 (with-tm (start-revision document-id tm-id)
		   (intersection isi-topics (topics xml-importer::tm)))))
	    (map 'list #'(lambda(top)
			   (map 'list 
				#'(lambda(role)
				    (when (find (parent role) assocs)
				      (d::delete-construct (parent role))))
				(player-in-roles top)))
		 topics-in-tm)
	    topics-in-tm))))))
  

(defun get-associations-by-type (top start-revision association-type-psi
				 role-type-psi)
  "Returns all associations of the passed associaiton type where the 
   topic top is a player in a role of the given roletype."   
  (declare (TopicC top))
  (declare (string association-type-psi role-type-psi))
  (declare (integer start-revision))
  (let ((assoc-type (get-item-by-psi association-type-psi
				     :revision start-revision))
	(role-type (get-item-by-psi role-type-psi
				    :revision start-revision)))
    (when (and assoc-type role-type)
      (let ((assocs
	     (remove-if  
	      #'null
	      (map 'list
		   #'(lambda(role)
		       (when (and (eql (instance-of (parent role)) assoc-type)
				  (eql (instance-of role) role-type))
			 (parent role)))
		   (player-in-roles top)))))
	assocs))))


(defun get-players-by-role-type (associations start-revision
				 role-type-psi)
  "Returns all players of the passed associaiton that are contained
   in roles of the given type."
  (declare (list associations))
  (declare (integer start-revision))
  (declare (string role-type-psi))
  (let ((role-type (get-item-by-psi role-type-psi
				    :revision start-revision)))
    (let ((players
	   (remove-if
	    #'null
	    (map 'list
		 #'(lambda(assoc)
		     (let ((role 
			    (find-if #'(lambda(role)
					 (eql role-type (instance-of role)))
				     (roles assoc))))
		       (when role
			 (player role))))
		 associations))))
      players)))

  

(defun get-occurrences-by-type (top start-revision
			       &key (occurrence-type-uri
				     *tm2rdf-itemIdentity-property*))
  "Returns all occurrences of the given topic, that is of the type
   bound to occurrence-type-uri."
  (declare (TopicC top))
  (with-revision start-revision
    (let ((identifier-occs
	   (remove-if #'null
		      (map 'list
			   #'(lambda(occurrence)
			       (let ((type (instance-of occurrence)))
				 (let ((type-psi
					(find-if #'(lambda(psi)
						     (string= 
						      occurrence-type-uri 
						      (uri psi)))
						 (psis type))))
				   (when type-psi
				     occurrence))))
			   (occurrences top)))))
      identifier-occs)))


(defun map-isi-identifiers (top start-revision
			    &key (id-type-uri 
				  *tm2rdf-itemIdentity-property*))
  "Maps identifiers of the type depending on id-type-uri from topic occurrences
   imported from RDF to the corresponding TM constructs."
  (declare (TopicC top))
  (let ((id-occs (get-occurrences-by-type top start-revision
					  :occurrence-type-uri id-type-uri))
	(class-symbol (cond
			((string= id-type-uri
				  *tm2rdf-itemIdentity-property*)
			 'ItemIdentifierC)
			((string= id-type-uri
				  *tm2rdf-subjectLocator-property*)
			 'SubjectLocatorC)
			((string= id-type-uri
				  *tm2rdf-subjectIdentifier-property*)
			 'PersistentIdC))))
    (let ((id-uris (map 'list #'charvalue id-occs)))
      (elephant:ensure-transaction  (:txn-nosync t)
	(map 'list #'d::delete-construct id-occs)
	(let ((ids (map 'list 
			#'(lambda(id-uri)
			    (make-instance class-symbol
					   :uri id-uri
					   :start-revision start-revision))
			id-uris)))
	  ids)))))


(defun bound-item-identifiers (construct identifiers)
  "Bounds the passed item-identifier to the passed construct."
  (declare (ReifiableConstructC construct))
  (dolist (id identifiers)
    (declare (ItemIdentifierC id))
    (setf (identified-construct id) construct))
  construct)


(defun bound-subject-identifiers (top identifiers)
    "Bounds the passed psis to the passed topic."
  (declare (TopicC top))
  (dolist (id identifiers)
    (declare (PersistentIdC id))
    (setf (identified-construct id) top))
  top)


(defun bound-subject-locators (top locators)
  "Bounds the passed locators to the passed topic."
  (declare (TopicC top))
  (dolist (id locators)
    (declare (SubjectLocatorC id))
    (setf (identified-construct id) top))
  top)
