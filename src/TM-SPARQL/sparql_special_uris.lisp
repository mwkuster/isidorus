;;+-----------------------------------------------------------------------------
;;+  Isidorus
;;+  (c) 2008-2010 Marc Kuester, Christoph Ludwig, Lukas Georgieff
;;+
;;+  Isidorus is freely distributable under the LLGPL license.
;;+  You can find a detailed description in trunk/docs/LLGPL-LICENSE.txt and
;;+  trunk/docs/LGPL-LICENSE.txt.
;;+-----------------------------------------------------------------------------


(in-package :TM-SPARQL)


(defmacro with-triple-nodes (triple-construct &body body)
  "Generates the variables subj, pred, obj that references the triple's
   nodes. Additionaly the variables subj-uri, pred-uri and obj-uri are
   generated when the corresponding node is a resource-nodes."
  `(let* ((subj (subject ,triple-construct))
	  (pred (predicate ,triple-construct))
	  (obj (object ,triple-construct))
	  (subj-uri (unless (variable-p subj)
		      (sparql-node (value subj) :revision revision)))
	  (pred-uri (unless (variable-p pred)
		      (sparql-node (value pred) :revision revision)))
	  (obj-uri (when (and (not (variable-p obj))
			      (not (literal-p obj)))
		     (sparql-node (value obj) :revision revision)))
	  (literal-datatype (when (literal-p obj)
			      (literal-datatype obj))))
     (declare (Ignorable subj-uri pred-uri obj-uri literal-datatype))
     ,@body))


(defgeneric filter-by-special-uris (construct &key revision)
  (:documentation "Returns lists representing triples that handles special
                   predicate uris defined in tmsparql.")
  (:method ((construct SPARQL-Triple) &key (revision d:*TM-REVISION*))
    (let ((pred (predicate construct))
	  (pred-val (value (predicate construct))))
      (if (variable-p pred)
	  (filter-for-special-uris construct :revision revision)
	  (cond ((has-identifier pred-val *tms-reifier*)
		 (filter-for-reifier construct :revision revision))
		((has-identifier pred-val *tms-scope*)
		 (filter-for-scopes construct :revision revision))
		((has-identifier pred-val *tms-value*)
		 (filter-for-values construct :revision revision))
		((has-identifier pred-val *tms-topicProperty*)
		 (filter-for-topicProperties construct :revision revision))
		((has-identifier pred-val *tms-role*)
		 (filter-for-roles construct :revision revision))
		((has-identifier pred-val *tms-player*)
		 (filter-for-player construct :revision revision)))))))


(defgeneric filter-for-special-uris (construct &key revision)
  (:documentation "Returns a list of triples representing the subject
                   and its objects corresponding to the defined
                   special-uris, e.g. <subj> var <obj>.")
  (:method ((construct SPARQL-Triple) &key (revision *TM-REVISION*))
    (let* ((pred (predicate construct))
	   (old-pred-value (value pred))
	   (res-1
	    (progn
	      (setf (value pred) (get-item-by-psi *tms-reifier* :revision revision))
	      (let ((val (filter-for-reifier construct :revision revision)))
		(setf (value pred) old-pred-value)
		val)))
	   (res-2
	    (progn
	      (setf (value pred) (get-item-by-psi *tms-scope* :revision revision))
	      (let ((val (filter-for-scopes construct :revision revision)))
		(setf (value pred) old-pred-value)
		val)))
	   (res-3
	    (progn
	      (setf (value pred) (get-item-by-psi *tms-value* :revision revision))
	      (let ((val (filter-for-values construct :revision revision)))
		(setf (value pred) old-pred-value)
		val)))
	   (res-4
	    (progn
	      (setf (value pred) (get-item-by-psi *tms-role* :revision revision))
	      (let ((val (filter-for-roles construct :revision revision)))
		(setf (value pred) old-pred-value)
		val)))
	   (res-5
	    (progn
	      (setf (value pred) (get-item-by-psi *tms-player* :revision revision))
	      (let ((val (filter-for-player construct :revision revision)))
		(setf (value pred) old-pred-value)
		val))))
      (append res-1 res-2 res-3 res-4 res-5))))


(defgeneric filter-for-player (construct &key revision)
  (:documentation "Returns a list with triples where the subject
                   represents a role and the object represents a player.")
  (:method ((construct SPARQL-Triple) &key (revision *TM-REVISION*))
      (unless (literal-p (object construct))
	(with-triple-nodes construct
	  (when (and (or (typep (value subj) 'RoleC)
			 (variable-p subj))
		     (or (typep (value obj) 'TopicC)
			 (variable-p obj)))
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
		       (list
			(list
			 :subject subj-uri
			 :predicate pred-uri
			 :object (sparql-node player-top :revision revision))))))
		  ((not (variable-p obj))
		   (let ((parent-roles
			  (player-in-roles (value obj) :revision revision)))
		     (loop for role in parent-roles
			collect (list
				 :subject (sparql-node role :revision revision)
				 :predicate pred-uri
				 :object (sparql-node (player role :revision revision)
							   :revision revision)))))
		  (t ; only pred is given
		   (let ((all-roles
			  (remove-null
			   (map 'list #'(lambda(role)
					  (when (player role :revision revision)
					    role))
				(get-all-roles revision)))))
		     (loop for role in all-roles
			collect (list :subject (sparql-node role :revision revision)
				      :predicate pred-uri
				      :object (sparql-node (player role :revision revision)
							   :revision revision)))))))))))


(defgeneric filter-for-roles (construct &key revision)
  (:documentation "Returns a list of triples where the subject represents
                   an Association and the object represents a role.")
  (:method((construct SPARQL-Triple) &key (revision *TM-REVISION*))
    (unless (literal-p (object construct))
      (with-triple-nodes construct
	(when (and (or (variable-p subj)
		       (typep (value subj) 'd:AssociationC))
		   (or (variable-p obj)
		       (typep (value obj) 'd:RoleC)))
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
				  :object (sparql-node role :revision revision))))
		((not (variable-p obj))
		 (let ((parent-assoc (parent (value obj) :revision revision)))
		   (when revision
		     (list
		      (list :subject (sparql-node parent-assoc :revision revision)
			    :predicate pred-uri
			    :object obj-uri)))))
		(t ; only pred is given
		 (let ((assocs
			(remove-null
			 (map 'list #'(lambda(assoc)
					(when (roles assoc :revision revision)
					  assoc))
			      (get-all-associations revision)))))
		   (loop for assoc in assocs
		      append (loop for role in (roles assoc :revision revision)
				collect (list :subject (sparql-node
							assoc :revision revision)
					      :predicate pred-uri
					      :object (sparql-node
						       role :revision revision))))))))))))


(defgeneric filter-for-topicProperties (construct &key revision)
  (:documentation "Returns a list of triples where the subject represents
                   a topic and the object represents a name or occurrence.")
  (:method ((construct SPARQL-Triple) &key (revision *TM-REVISION*))
    (unless (literal-p (object construct))
      (with-triple-nodes construct
	(when (and (or (variable-p subj)
		       (typep (value subj) 'd:TopicC))
		   (or (variable-p obj)
		       (typep (value obj) 'd:OccurrenceC)
		       (typep (value obj) 'd:NameC)))
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
				  :object
				  (sparql-node property :revision revision))))
		((not (variable-p obj))
		 (let ((parent-top (parent (value obj) :revision revision)))
		   (when revision
		     (list
		      (list :subject (sparql-node parent-top :revision revision)
			    :predicate pred-uri
			    :object obj-uri)))))
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
				collect (list :subject (sparql-node
							top :revision revision)
					      :predicate pred-uri
					      :object (sparql-node
						       prop :revision revision))))))))))))


(defgeneric filter-for-values (construct &key revision)
  (:documentation "Returns a list of triples that represent a
                   subject and its literal value as object.")
  (:method ((construct SPARQL-Triple) &key revision)
    (declare (ignorable revision))
    (with-triple-nodes construct
      (when (and (or (variable-p subj)
		     (typep (value subj) 'd:OccurrenceC)
		     (typep (value subj) 'd:NameC)
		     (typep (value subj) 'd:VariantC))
		 (or (variable-p obj)
		     (literal-p obj)))
	(cond ((and (not (variable-p subj))
		    (not (variable-p obj)))
	       (if (typep (value subj) 'NameC)
		   (when (and (string= literal-datatype *xml-string*)
			      (string= (charvalue (value subj)) (value obj)))
		     (list (list :subject subj-uri
				 :predicate pred-uri
				 :object (value obj)
				 :literal-datatype literal-datatype)))
		   (when (filter-datatypable-by-value (value subj) (value obj)
						      literal-datatype)
		     (list (list :subject subj-uri
				 :predicate pred-uri
				 :object (value obj)
				 :literal-datatype literal-datatype)))))
	      ((not (variable-p subj))
	       (list (list :subject subj-uri
			   :predicate pred-uri
			   :object (charvalue (value subj))
			   :literal-datatype (if (typep (value subj) 'd:NameC)
						 *xml-string*
						 (datatype (value subj))))))
	      ((not (variable-p obj))
	       (loop for char in (return-characteristics (value obj) literal-datatype)
		  collect (list :subject (sparql-node char :revision revision)
				:predicate pred-uri
				:object (charvalue char)
				:literal-datatype (if (typep char 'd:NameC)
						      *xml-string*
						      (datatype char)))))
	      (t ;only pred is given
	       (let ((chars (append (get-all-names revision)
				    (get-all-occurrences revision)
				    (get-all-variants revision))))
		 (loop for char in chars
		    collect (list :subject (sparql-node char :revision revision)
				  :predicate pred-uri
				  :object (charvalue char)
				  :literal-datatype (if (typep char 'd:NameC)
							*xml-string*
							(datatype char)))))))))))


  (defgeneric filter-for-scopes (construct &key revision)
    (:documentation "Returns a list of triples that represent a subject as the
                   scoped item and the object as the scope-topic.")
    (:method ((construct SPARQL-Triple) &key (revision *TM-REVISION*))
      (unless (literal-p (object construct))
	(with-triple-nodes construct
	  (when (and (or (variable-p subj)
			 (typep (value subj) 'd:ScopableC))
		     (or (variable-p obj)
			 (typep (value obj) 'd:TopicC)))
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
				    :object (sparql-node scope :revision revision))))
		  ((not (variable-p obj))
		   (let ((scoped-constructs
			  (used-as-theme (value obj) :revision revision)))
		     (loop for construct in scoped-constructs
			collect (list :subject (sparql-node construct :revision revision)
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
				    (list :subject (sparql-node
						    construct :revision revision)
					  :predicate pred-uri
					  :object (sparql-node
						   construct :revision revision))))))))))))


(defgeneric filter-for-reifier (construct &key revision)
  (:documentation "Returns a list with triples representing a reifier
                     and the corresponding reified construct.")
  (:method ((construct SPARQL-Triple) &key (revision *TM-REVISION*))
    (unless (literal-p (object construct))
      (with-triple-nodes construct
	(when (and (or (variable-p subj)
		       (typep (value subj) 'd:ReifiableConstructC))
		   (or (variable-p obj)
		       (typep (value obj) 'd:TopicC)))
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
		     (list
		      (list :subject subj-uri
			    :predicate pred-uri
			    :object (sparql-node reifier-top :revision revision))))))
		((not (variable-p obj))
		 (let ((reified-cons
			(reified-construct (value obj) :revision revision)))
		   (when reified-cons
		     (list (list :subject
				 (sparql-node reified-cons :revision revision)
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
				    (sparql-node (reified-construct top :revision revision)
						 :revision revision)
				    :predicate pred-uri
				    :object (sparql-node top :revision revision)))))))))))