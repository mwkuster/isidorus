;;+-----------------------------------------------------------------------------
;;+  Isidorus
;;+  (c) 2008-2010 Marc Kuester, Christoph Ludwig, Lukas Georgieff
;;+
;;+  Isidorus is freely distributable under the LLGPL license.
;;+  You can find a detailed description in trunk/docs/LLGPL-LICENSE.txt and
;;+  trunk/docs/LGPL-LICENSE.txt.
;;+-----------------------------------------------------------------------------


(in-package :TM-SPARQL)


;TODO: create a macro for "filter-for-scopes", "filter-for-reifier", ...
;TODO: change (embrace-uri String) to (embrace-construct TopicMapsConstructC)
;        that creates a blank node when there is no identifier available
;         => change also any-id, so if there is no identifier a blank node
;            have to be returned
;         => change all when-do statements that call any-id




(defgeneric filter-by-special-uris (construct &key revision)
  (:documentation "Returns lists representing triples that handles special
                   predicate uris defined in tmsparql.")
  (:method ((construct SPARQL-Triple) &key (revision d:*TM-REVISION*))
    (let ((pred (predicate construct))
	  (subj-value (value (subject construct))))
      (if (variable-p pred)
	  (filter-for-special-uris construct :revision revision)
	  (cond ((and (has-identifier (value pred) *tms-reifier*)
		      (typep subj-value 'd:ReifiableConstructC))
		 (filter-for-reifier construct :revision revision))
		((and (has-identifier (value pred) *tms-scope*)
		      (typep subj-value 'd:ScopableC))
		 (filter-for-special-uris construct :revision revision))
		((and (has-identifier (value pred) *tms-value*)
		      (typep subj-value 'd:CharacteristicC))
		 (filter-for-values construct :revision revision))
		((and (has-identifier (value pred) *tms-topicProperty*)
		      (typep subj-value 'd:TopicC))
		 (filter-for-topicProperties construct :revision revision))
		((and (has-identifier (value pred) *tms-role*)
		      (typep subj-value 'd:AssociationC))
		 (filter-for-roles construct :revision revision))
		((and (has-identifier (value pred) *tms-player*)
		      (typep subj-value 'd:RoleC))
		 (filter-for-player construct :revision revision)))))))


(defgeneric filter-for-special-uris (construct &key revision)
  (:documentation "Returns a list of triples representing the subject
                   and its objects corresponding to the defined
                   special-uris, e.g. <subj> var <obj>.")
  (:method ((construct SPARQL-Triple) &key (revision *TM-REVISION*))
    (let* ((subj (subject construct))
	   (pred (predicate construct))
	   (old-pred-value (value pred))
	   (res-1
	    (when (or (typep (value subj) 'd:ReifiableConstructC)
		      (variable-p subj))
	      (setf (value pred) (get-item-by-psi *tms-reifier* :revision revision))
	      (filter-for-reifier construct :revision revision)
	      (setf (value pred) old-pred-value)))
	   (res-2
	    (when (or (typep (value subj) 'd:ScopableC)
		      (variable-p subj))
	      (setf (value pred) (get-item-by-psi *tms-scope* :revision revision))
	      (filter-for-scopes construct :revision revision)
	      (setf (value pred) old-pred-value)))
	   (res-3
	    (when (or (typep (value subj) 'd:CharacteristicC)
		      (variable-p subj))
	      (setf (value pred) (get-item-by-psi *tms-value* :revision revision))
	      (filter-for-values construct :revision revision)
	      (setf (value pred) old-pred-value)))
	   (res-4
	    (when (or (typep (value subj) 'd:AssociationC)
		      (variable-p subj))
	      (setf (value pred) (get-item-by-psi *tms-role* :revision revision))
	      (filter-for-values construct :revision revision)
	      (setf (value pred) old-pred-value)))
	   (res-5
	    (when (or (typep (value subj) 'd:RoleC)
		      (variable-p subj))
	      (setf (value pred) (get-item-by-psi *tms-player* :revision revision))
	      (filter-for-values construct :revision revision)
	      (setf (value pred) old-pred-value))))
      (append res-1 res-2 res-3 res-4 res-5))))


(defgeneric filter-for-player (construct &key revision)
  (:documentation "Returns a list with triples where the subject
                   represents a role and the object represents a player.")
  (:method ((construct SPARQL-Triple) &key (revision *TM-REVISION*))
      (unless (literal-p (object construct))
	(let* ((subj (subject construct))
	       (pred (predicate construct))
	       (obj (object construct))
	       (subj-uri (unless (variable-p subj)
			   (when-do id (any-id (value subj) :revision revision)
				    (embrace-uri (uri id)))))
	       (pred-uri (unless (variable-p pred)
			   (when-do id (any-id (value pred) :revision revision)
				    (embrace-uri (uri id)))))
	       (obj-uri (unless (variable-p obj)
			  (when-do id (any-id (value obj) :revision revision)
				   (embrace-uri (uri id))))))
	  (cond ((and (not (variable-p subj))
		      (not (variable-p obj)))
		 (when (eql (player (value subj) :revision revision)
			    (value obj))
		   (list (list :subject subj-uri
			       :predicate pred-uri
			       :object obj-uri))))
		((not (variable-p subj))
		 (let ((player-top
			(player (value subj) :revision revision)))
		   (when player-top
		     (list :subject subj-uri
			   :predicate pred-uri
			   :object (when-do id (any-id player-top :revision revision)
					    (embrace-uri (uri id)))))))
		((not (variable-p obj))
		 (let ((parent-roles
			(player-in-roles (value obj) :revision revision)))
		   (loop for role in parent-roles
		      collect (list :subject (when-do id (any-id role :revision revision)
						      (embrace-uri id))
				    :predicate pred-uri
				    :object
				    (when-do id (any-id (player role :revision revision)
							:revision revision)
					     (embrace-uri id))))))
		(t ; only pred is given
		 (let ((all-roles
			(remove-null
			 (map 'list #'(lambda(role)
					(when (player role :revision revision)
					  role))
			      (get-all-roles revision)))))
		   (loop for role in all-roles
		      collect (list :subject
				    (when-do id (any-id role :revision revision)
					     (embrace-uri (uri id)))
				    :predicate pred-uri
				    :object
				    (when-do id (any-id (player role :revision revision)
							:revision revision)
					     (embrace-uri id)))))))))))


(defgeneric filter-for-roles (construct &key revision)
  (:documentation "Returns a list of triples where the subject represents
                   an Association and the object represents a role.")
  (:method((construct SPARQL-Triple) &key (revision *TM-REVISION*))
    (unless (literal-p (object construct))
      (let* ((subj (subject construct))
	     (pred (predicate construct))
	     (obj (object construct))
	     (subj-uri (unless (variable-p subj)
			 (when-do id (any-id (value subj) :revision revision)
				  (embrace-uri (uri id)))))
	     (pred-uri (unless (variable-p pred)
			 (when-do id (any-id (value pred) :revision revision)
				  (embrace-uri (uri id)))))
	     (obj-uri (unless (variable-p obj)
			(when-do id (any-id (value obj) :revision revision)
				 (embrace-uri (uri id))))))
	(cond ((and (not (variable-p subj))
		    (not (variable-p obj)))
	       (when (find obj (roles (value subj) :revision revision))
		 (list (list :subject subj-uri
			     :predicate pred-uri
			     :object obj-uri))))
	      ((not (variable-p subj))
	       (loop for role in (roles (value subj) :revision revision)
		  collect (list :subject subj-uri
				:predicate pred-uri
				:object (when-do id (any-id role :revision revision)
						 (embrace-uri id)))))
	      ((not (variable-p obj))
	       (let ((parent-assoc (parent (value obj) :revision revision)))
		 (when revision
		   (list :subject (when-do id (any-id parent-assoc :revision revision)
					   (embrace-uri id))
			 :predicate pred-uri
			 :object obj-uri))))
	      (t ; only pred is given
	       (let ((assocs
		      (remove-null
		       (map 'list #'(lambda(assoc)
				      (when (roles assoc :revision revision)
					assoc))
			    (get-all-associations revision)))))
		 (loop for assoc in assocs
		      append (loop for role in (roles assoc :revision revision)
				collect (list :subject
					      (when-do id (any-id assoc
								  :revision revision)
						       (embrace-uri id))
					      :predicate pred-uri
					      :object
					      (when-do id (any-id role
								  :revision revision)
						       (embrace-uri id))))))))))))


(defgeneric filter-for-topicProperties (construct &key revision)
  (:documentation "Returns a list of triples where the subject represents
                   a topic and the object represents a name or occurrence.")
  (:method ((construct SPARQL-Triple) &key (revision *TM-REVISION*))
    (unless (literal-p (object construct))
      (let* ((subj (subject construct))
	     (pred (predicate construct))
	     (obj (object construct))
	     (subj-uri (unless (variable-p subj)
			 (when-do id (any-id (value subj) :revision revision)
				  (embrace-uri (uri id)))))
	     (pred-uri (unless (variable-p pred)
			 (when-do id (any-id (value pred) :revision revision)
				  (embrace-uri (uri id)))))
	     (obj-uri (unless (variable-p obj)
			(when-do id (any-id (value obj) :revision revision)
				 (embrace-uri (uri id))))))
	(cond ((and (not (variable-p subj))
		    (not (variable-p obj)))
	       (when (find obj (append (names (value subj) :revision revision)
				       (occurrences (value subj) :revision revision)))
		 (list (list :subject subj-uri
			     :predicate pred-uri
			     :object obj-uri))))
	      ((not (variable-p subj))
	       (loop for property in (append
				      (names (value subj) :revision revision)
				      (occurrences (value subj) :revision revision))
		  collect (list :subject subj-uri
				:predicate pred-uri
				:object (when-do id (any-id property :revision revision)
						 (embrace-uri id)))))
	      ((not (variable-p obj))
	       (let ((parent-top (parent (value obj) :revision revision)))
		 (when revision
		   (list :subject (when-do id (any-id parent-top :revision revision)
					   (embrace-uri id))
			 :predicate pred-uri
			 :object obj-uri))))
	      (t ; only pred is given
	       (let ((topics
		      (remove-null
		       (map 'list #'(lambda(top)
				      (when (append
					     (names top :revision revision)
					     (occurrences top :revision revision))
					top))
			    (get-all-topics revision)))))
		 (loop for top in topics
		      append (loop for prop in (append
						(names top :revision revision)
						(occurrences top :revision revision))
				collect (list :subject
					      (when-do id (any-id top :revision revision)
						       (embrace-uri id))
					      :predicate pred-uri
					      :object
					      (when-do id (any-id prop :revision revision)
						       (embrace-uri id))))))))))))


  (defgeneric filter-for-values (construct &key revision)
    (:documentation "Returns a list of triples that represent a
                   subject and its literal value as object.")
    (:method ((construct SPARQL-Triple) &key revision)
      (declare (ignorable revision))
      (when (or (literal-p (object construct))
		(variable-p (object construct)))
	(let* ((subj (subject construct))
	       (pred (predicate construct))
	       (obj (object construct))
	       (literal-datatype (literal-datatype obj))
	       (subj-uri (unless (variable-p subj)
			   (when-do id (any-id (value subj) :revision revision)
				    (embrace-uri (uri id)))))
	       (pred-uri (unless (variable-p pred)
			   (when-do id (any-id (value pred) :revision revision)
				    (embrace-uri (uri id))))))
	  (cond ((and (not (variable-p subj))
		      (not (variable-p obj)))
		 (when (or (and (typep subj 'NameC)
				(string= literal-datatype *xml-string*)
				(string= (charvalue subj) (value obj)))
			   (filter-datatypable-by-value subj obj literal-datatype))
		   (list (list :subject subj-uri
			       :predicate pred-uri
			       :object (value obj)
			       :literal-datatype literal-datatype))))
		((not (variable-p subj))
		 (list (list :subject subj-uri
			     :predicate pred-uri
			     :object (charvalue subj)
			     :literal-datatype (datatype subj))))
		((not (variable-p obj))
		 (loop for char in (return-characteristics (value obj) literal-datatype)
		    collect (list :subject (when-do id (any-id char :revision revision)
						    (embrace-uri id))
				  :predicate pred-uri
				  :object (charvalue char)
				  :literal-datatype (datatype char))))
		(t ;only pred is given
		 (let ((chars (append (get-all-names revision)
				      (get-all-occurrences revision)
				      (get-all-variants revision))))
		   (loop for char in chars
		      collect (list :subject (when-do id (any-id char :revision revision)
						      (embrace-uri id))
				    :predicate pred-uri
				    :object (charvalue char)
				    :literal-datatype (datatype char))))))))))


  (defgeneric filter-for-scopes (construct &key revision)
    (:documentation "Returns a list of triples that represent a subject as the
                   scoped item and the object as the scope-topic.")
    (:method ((construct SPARQL-Triple) &key (revision *TM-REVISION*))
      (unless (literal-p (object construct))
	(let* ((subj (subject construct))
	       (pred (predicate construct))
	       (obj (object construct))
	       (subj-uri (unless (variable-p subj)
			   (when-do id (any-id (value subj) :revision revision)
				    (embrace-uri (uri id)))))
	       (pred-uri (unless (variable-p pred)
			   (when-do id (any-id (value pred) :revision revision)
				    (embrace-uri (uri id)))))
	       (obj-uri (unless (variable-p obj)
			  (when-do id (any-id (value obj) :revision revision)
				   (embrace-uri (uri id))))))
	  (cond ((and (not (variable-p subj))
		      (not (variable-p obj)))
		 (when (find obj (themes (value subj) :revision revision))
		   (list (list :subject subj-uri
			       :predicate pred-uri
			       :object obj-uri))))
		((not (variable-p subj))
		 (loop for scope in (themes (value subj) :revision revision)
		    collect (list :subject subj-uri
				  :predicate pred-uri
				  :object (when-do id (any-id scope :revision revision)
						   (embrace-uri (uri id))))))
		((not (variable-p obj))
		 (let ((scoped-constructs
			(used-as-theme (value obj) :revision revision)))
		   (loop for construct in scoped-constructs
		      collect (list :subject (when-do id (any-id construct :revision revision)
						      (embrace-uri (uri id)))
				    :predicate pred-uri
				    :object obj-uri))))
		(t ;only pred is given
		 (let ((scoped-constructs
			(remove-null
			 (map 'list #'(lambda(construct)
					(when (themes construct :revision revision)
					  construct))
			      (append (get-all-associations revision)
				      (get-all-occurrences revision)
				      (get-all-names revision)
				      (get-all-variants))))))
		   (loop for construct in scoped-constructs
		      append (loop for scope in (themes construct :revision revision)
				collect
				  (list :subject (when-do id (any-id construct
								     :revision revision)
							  (embrace-uri id))
					:predicate pred-uri
					:object (when-do id (any-id construct
								    :revision revision)
							 (embrace-uri id))))))))))))


  (defgeneric filter-for-reifier (construct &key revision)
    (:documentation "Returns a list with triples representing a reifier
                     and the corresponding reified construct.")
    (:method ((construct SPARQL-Triple) &key (revision *TM-REVISION*))
      (unless (literal-p (object construct))
	(let* ((subj (subject construct))
	       (pred (predicate construct))
	       (obj (object construct))
	       (subj-uri (unless (variable-p subj)
			   (when-do id (any-id (value subj) :revision revision)
				    (embrace-uri (uri id)))))
	       (pred-uri (unless (variable-p pred)
			   (when-do id (any-id (value pred) :revision revision)
				    (embrace-uri (uri id)))))
	       (obj-uri (unless (variable-p obj)
			  (when-do id (any-id (value obj) :revision revision)
				   (embrace-uri (uri id))))))
	  (cond ((and (not (variable-p subj))
		      (not (variable-p obj)))
		 (when (eql (reifier (value subj) :revision revision)
			    (value obj))
		   (list (list :subject subj-uri
			       :predicate pred-uri
			       :object obj-uri))))
		((not (variable-p subj))
		 (let ((reifier-top
			(reifier (value subj) :revision revision)))
		   (when reifier-top
		     (list :subject subj-uri
			   :predicate pred-uri
			   :object (when-do id (any-id reifier-top :revision revision)
					    (embrace-uri (uri id)))))))
		((not (variable-p obj))
		 (let ((reified-cons
			(reified-construct (value obj) :revision revision)))
		   (when reified-cons
		     (list (list :subject
				 (when-do id (any-id reified-cons :revision revision)
					  (embrace-uri (uri id)))
				 :predicate pred-uri
				 :object obj-uri)))))
		(t ; only pred is given
		 (let ((topics
			(remove-null
			 (map 'list #'(lambda(top)
					(when (reified-construct top :revision revision)
					  top))
			      (get-all-topics revision)))))
		   (loop for top in topics
		      collect (list :subject
				    (when-do id (any-id (reified-construct
							 top :revision revision))
					     (embrace-uri (uri id)))
				    :predicate pred-uri
				    :object (when-do id (any-id top :revision revision)
						     (embrace-uri (uri id))))))))))))