;;+-----------------------------------------------------------------------------
;;+  Isidorus
;;+  (c) 2008-2010 Marc Kuester, Christoph Ludwig, Lukas Georgieff
;;+
;;+  Isidorus is freely distributable under the LLGPL license.
;;+  You can find a detailed description in trunk/docs/LLGPL-LICENSE.txt and
;;+  trunk/docs/LGPL-LICENSE.txt.
;;+-----------------------------------------------------------------------------


(in-package :datamodel)


(defgeneric roles-by-type (construct role-type &key revision)
  (:documentation "Returns all roles of the passed topic or
                   association that is of the specified role-type.
                   If role-type is set to nil all roles are returned."))


(defmethod roles-by-type ((construct TopicC) role-type &key (revision *TM-REVISION*))
  (declare (integer revision)
	   (type (or Null TopicC) role-type))
  (if role-type
      (remove-if #'null
		 (map 'list #'(lambda(role)
				(when (eql (instance-of role :revision revision)
					   role-type)
				  role))
		      (player-in-roles construct :revision revision)))
      (player-in-roles construct :revision revision)))


(defmethod roles-by-type ((construct AssociationC) role-type
			  &key (revision *TM-REVISION*))
  (declare (integer revision)
	   (type (or Null TopicC) role-type))
  (if role-type
      (remove-if #'null
		 (map 'list #'(lambda(role)
				(when (eql (instance-of role :revision revision)
					   role-type)
				  role))
		      (roles construct :revision revision)))
      (roles construct :revision revision)))


(defgeneric roles-by-player (construct role-player &key revision)
  (:documentation "Returns all roles that contains the corresponding player.
                   If the player is set to nil all roles are returned.")
  (:method ((construct AssociationC) role-player &key (revision *TM-REVISION*))
    (declare (integer revision)
	     (type (or Null TopicC) role-player))
    (if role-player
	(remove-if #'null
		   (map 'list #'(lambda(role)
				  (when (eql (player role :revision revision)
					     role-player)
				    role))
			(roles construct :revision revision)))
	(roles construct :revision revision))))


(defun filter-associations-by-type (associations association-type
				    &key (revision *TM-REVISION*))
  "Returns a list of associations that are an instance-of of the given
   association-type. If association-type is set to nil, all associations
   are returned."
  (declare (List associations)
	   (type (or Null TopicC) association-type)
	   (integer revision))
  (if association-type
      (remove-if #'(lambda(assoc)
		     (not (eql (instance-of assoc :revision revision)
			       association-type)))
		 associations)
      associations))


(defun filter-associations-by-role (associations role-type role-player
				 &key (revision *TM-REVISION*))
  "Returns associations that have a role corresponding to the passed
   values. If any of the passed role-values is set to nil, it won't be used
   for the evaluation of the result."
  (declare (List associations)
	   (type (or Null TopicC) role-type role-player))
  (remove-if #'null
	     (intersection
	      (map 'list #'(lambda(assoc)
			     (when (roles-by-type assoc role-type
						  :revision revision)
			       assoc))
		   associations)
	      (map 'list #'(lambda(assoc)
			     (when (roles-by-player assoc role-player
						    :revision revision)
			       assoc))
		   associations))))


(defgeneric associations-of (construct role-type association-type
				       other-role-type other-player
				       &key revision)
  (:documentation "Returns all associations of the passed topic (construct)
                   that corresponds to the given values.
                   If any of the passed values is set to nil, it won't be
                   used to evaluate the result.")
  (:method ((construct TopicC) role-type association-type other-role-type
	    other-player &key (revision *TM-REVISION*))
    (declare (integer revision)
	     (type (or Null TopicC) role-type association-type
		   other-role-type other-player))
    (let ((assocs-by-role (map 'list #'(lambda(role)
					 (parent role :revision revision))
			       (roles-by-type construct role-type
					      :revision revision))))
      (let ((assocs-by-type
	     (filter-associations-by-type assocs-by-role association-type
					  :revision revision)))
	(filter-associations-by-role assocs-by-type other-role-type
				     other-player :revision revision)))))


(defgeneric instance-of-associations (construct &key revision)
  (:documentation "Returns all type-instance associations of
                   the passed instance topic.")
  (:method ((construct TopicC) &key (revision *TM-REVISION*))
    (declare (integer revision))
    (let ((type-top
	   (get-item-by-psi *type-psi* :revision revision :error-if-nil t))
	  (instance-top
	   (get-item-by-psi *instance-psi* :revision revision :error-if-nil t))
	  (type-instance-top
	   (get-item-by-psi *type-instance-psi* :revision revision
			    :error-if-nil t)))
      (let ((possible-assocs
	     (map 'list #'(lambda(role)
			    (parent role :revision revision))
		  (roles-by-type construct instance-top :revision revision))))
	(let ((type-instance-assocs
	       (filter-associations-by-type possible-assocs type-instance-top
					    :revision revision)))
	  (filter-associations-by-role type-instance-assocs type-top nil
				       :revision revision))))))


(defgeneric supertype-associations (construct &key revision)
  (:documentation "Returns all supertype-subtype associations of
                   the passed subtype topic.")
  (:method ((construct TopicC) &key (revision *TM-REVISION*))
    (declare (integer revision))
    (let ((supertype-top
	   (get-item-by-psi *supertype-psi* :revision revision :error-if-nil t))
	  (subtype-top
	   (get-item-by-psi *subtype-psi* :revision revision :error-if-nil t))
	  (supertype-subtype-top
	   (get-item-by-psi *supertype-subtype-psi* :revision revision
			    :error-if-nil t)))
      (let ((possible-assocs
	     (map 'list #'(lambda(role)
			    (parent role :revision revision))
		  (roles-by-type construct subtype-top :revision revision))))
	(let ((type-instance-assocs
	       (filter-associations-by-type possible-assocs supertype-subtype-top
					    :revision revision)))
	  (filter-associations-by-role type-instance-assocs supertype-top nil
				       :revision revision))))))


(defgeneric direct-supertypes (construct &key revision)
  (:documentation "Returns all direct super type topics of the passed
                   construct.")
  (:method ((construct TopicC) &key (revision *TM-REVISION*))
    (declare (integer revision))
    (let ((assocs (supertype-associations construct :revision revision)))
      (remove-if #'null
		 (map 'list #'(lambda(assoc)
				(find-if-not
				 #'(lambda(role)
				     (eql (player role :revision revision)
					  construct))
				 (roles assoc :revision revision)))
		      assocs)))))


(defgeneric supertypes (construct &key revision valid-supertypes)
  (:documentation "Returns all super type topics of the passed
                   construct, also the transitive ones.")
  (:method ((construct TopicC) &key (revision *TM-REVISION*) valid-supertypes)
    (declare (integer revision))
    (let ((direct-super-types (direct-supertypes construct :revision revision)))
      (let ((current-valid-super-types
	     (append valid-supertypes direct-super-types)))
	(let ((recursive-super-types
	       (loop for direct-super-type in direct-super-types
		  append (supertypes
			  direct-super-type :revision revision
			  :valid-supertypes current-valid-super-types))))
	  (remove-duplicates
	   (remove-if #'null recursive-super-types)))))))


(defgeneric direct-instance-of (construct &key revision)
  (:documentation "Returns all direct type topics of the passed instance topic.")
  (:method ((construct TopicC) &key (revision *TM-REVISION*))
    (declare (integer revision))
    (let ((assocs (instance-of-associations construct :revision revision)))
      (remove-if #'null
		 (map 'list #'(lambda(assoc)
				(find-if-not
				 #'(lambda(role)
				     (eql (player role :revision revision)
					  construct))
				 (roles assoc :revision revision)))
		      assocs)))))


(defmethod instance-of (construct &key (revision *TM-REVISION*))
  "Returns all type topics of the passed construct and their super-types."
  (declare (integer revision))
  (let ((all-super-types (supertypes construct :revision revision)))
    (let ((all-types
	   (loop for topic in (append (list construct) all-super-types)
		append (direct-instance-of topic :revision revision))))
      (remove-duplicates
       (remove-if #'null all-types)))))


(defgeneric invoke-on (construct operation)
  (:documentation "Invokes the passed main operation on the characteristic's
                   value.
                   If cast-operation is set to a function the characteristic's
                   value is first casted by the cast-operation to another type
                   and afterwords processed by main-opertion.")
  (:method ((construct TopicC) (operation Function))
    (funcall operation (charvalue construct))))


(defgeneric names-by-type (construct type-identifier &key revision)
  (:documentation "Returns all names that are of the corresponding type.")
  (:method ((construct TopicC) (type-identifier IdentifierC)
	    &key (revision *TM-REVISION*))
    (declare (integer revision))
    (let ((type-topic (identified-construct type-identifier :revision revision)))
      (unless (typep type-topic 'TopicC)
	(error (make-bad-type-condition (format nil "from name-by-type(): expected a topic as instance-of but found ~a" (type-of type-topic)) 'TopicC type-topic)))
      (let ((results
	     (map 'list #'(lambda(name)
			    (when (instance-of name :revision revision)
			      name))
		  (names construct :revision revision))))
	(remove-if #'null results)))))


(defgeneric occurrences-by-type (construct type-identifier &key revision)
  (:documentation "Returns all names that are of the corresponding type.")
  (:method ((construct TopicC) (type-identifier IdentifierC)
	    &key (revision *TM-REVISION*))
    (declare (integer revision))
    (let ((type-topic (identified-construct type-identifier :revision revision)))
      (unless (typep type-topic 'TopicC)
	(error (make-bad-type-condition (format nil "from occurrence-by-type(): expected a topic as instance-of but found ~a" (type-of type-topic)) 'TopicC type-topic)))
      (let ((results
	     (map 'list #'(lambda(occ)
			    (when (instance-of occ :revision revision)
			      occ))
		  (occurrences construct :revision revision))))
	(remove-if #'null results)))))


(defgeneric characteristic-by-type (construct type-identifier &key revision)
  (:documentation "Returns all characteristics that are of the
                   corresponding type.")
  (:method ((construct TopicC) (type-identifier IdentifierC)
	    &key (revision *TM-REVISION*))
    (declare (integer revision))
    (union (names-by-type construct type-identifier :revision revision)
	   (occurrences-by-type construct type-identifier :revision revision))))


(defgeneric occurrences-by-value (construct filter &key revision)
  (:documentation "Returns a list of all occurrences of the passed
                   topic, that return a true value when calling filter
                   on their charvalue.")
  (:method ((construct TopicC) (filter Function) &key (revision *TM-REVISION*))
    (let ((results
	   (map 'list #'(lambda(occ)
			  (when (invoke-on occ filter)
			    occ))
		(occurrences construct :revision revision))))
      (remove-if #'null results))))


(defgeneric names-by-value (construct filter &key revision)
  (:documentation "Returns a list of all names of the passed
                   topic, that return a true value when calling filter
                   on their charvalue.")
  (:method ((construct TopicC) (filter Function) &key (revision *TM-REVISION*))
    (let ((results
	   (map 'list #'(lambda(name)
			  (when (invoke-on name filter)
			    name))
		(names construct :revision revision))))
      (remove-if #'null results))))


(defgeneric characteristic-by-value (construct filter &key revision)
  (:documentation "Returns a list of all characteristics of the passed
                   topic, that return a true value when calling filter.")
  (:method ((construct TopicC) (filter Function) &key (revision *TM-REVISION*))
    (declare (integer revision))
    (union (names-by-value construct filter :revision revision)
	   (occurrences-by-value construct filter :revision revision))))