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
      (remove-null
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
      (remove-null
       (map 'list #'(lambda(role)
		      (when (eql (instance-of role :revision revision)
				 role-type)
			role))
	    (roles construct :revision revision)))
      (roles construct :revision revision)))


(defgeneric roles-by-player (construct role-player
				       &key role-player-is-type revision)
  (:documentation "Returns all roles that contains the corresponding player.
                   If the player is set to nil all roles are returned.")
  (:method ((construct AssociationC) role-player
	    &key role-player-is-type (revision *TM-REVISION*))
    (declare (integer revision)
	     (type (or Null TopicC) role-player)
	     (boolean role-player-is-type))
    (if role-player
	(remove-null
	 (map 'list #'(lambda(role)
			(if role-player-is-type
			    (when (isa (player role :revision revision)
				       role-player)
			      role)
			    (when (eql (player role :revision revision)
				       role-player)
			      role)))
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
				 &key role-player-is-type (revision *TM-REVISION*))
  "Returns associations that have a role corresponding to the passed
   values. If any of the passed role-values is set to nil, it won't be used
   for the evaluation of the result."
  (declare (List associations)
	   (type (or Null TopicC) role-type role-player)
	   (boolean role-player-is-type))
  (remove-null
   (intersection
    (map 'list #'(lambda(assoc)
		   (when (roles-by-type assoc role-type
					:revision revision)
		     assoc))
	 associations)
    (map 'list #'(lambda(assoc)
		   (when (roles-by-player
			  assoc role-player
			  :role-player-is-type role-player-is-type
			  :revision revision)
		     assoc))
	 associations))))


(defgeneric associations-of (construct role-type association-type
				       other-role-type other-player
				       &key other-role-player-is-type
				       revision)
  (:documentation "Returns all associations of the passed topic (construct)
                   that corresponds to the given values.
                   If any of the passed values is set to nil, it won't be
                   used to evaluate the result.")
  (:method ((construct TopicC) role-type association-type other-role-type
	    other-player &key other-role-player-is-type
	    (revision *TM-REVISION*))
    (declare (integer revision)
	     (type (or Null TopicC) role-type association-type
		   other-role-type other-player)
	     (boolean other-role-player-is-type))
    (let ((assocs-by-role (map 'list #'(lambda(role)
					 (parent role :revision revision))
			       (roles-by-type construct role-type
					      :revision revision))))
      (let ((assocs-by-type
	     (filter-associations-by-type assocs-by-role association-type
					  :revision revision)))
	(filter-associations-by-role
	 assocs-by-type other-role-type other-player
	 :role-player-is-type other-role-player-is-type
	 :revision revision)))))


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
      (let ((other-roles
	     (remove-null
	      (map 'list
		   #'(lambda(assoc)
		       (find-if-not #'(lambda(role)
					(eql (player role :revision revision)
					     construct))
				    (roles assoc :revision revision)))
		   assocs))))
	(remove-null (map 'list #'(lambda(role)
				    (player role :revision revision))
			  other-roles))))))


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
	   (remove-null (union recursive-super-types
			       current-valid-super-types))))))))


(defgeneric direct-instance-of (construct &key revision)
  (:documentation "Returns all direct type topics of the passed instance topic.")
  (:method ((construct TopicC) &key (revision *TM-REVISION*))
    (declare (integer revision))
    (let ((assocs (instance-of-associations construct :revision revision)))
      (let ((other-roles
	     (remove-null
	      (map 'list #'(lambda(assoc)
			     (find-if-not #'(lambda(role)
					      (eql (player role :revision revision)
						   construct))
					  (roles assoc :revision revision)))
		   assocs))))
	(remove-null (map 'list #'(lambda(role)
				    (player role :revision revision))
			  other-roles))))))


(defmethod instance-of (construct &key (revision *TM-REVISION*))
  "Returns all type topics of the passed construct and their super-types."
  (declare (integer revision))
  (let ((direct-types (direct-instance-of construct :revision revision)))
    (let ((supertypes-of-types
	   (loop for type in direct-types
	      append (supertypes type :revision revision))))
      (union direct-types supertypes-of-types))))


(defgeneric invoke-on (construct operation)
  (:documentation "Invokes the passed main operation on the characteristic's
                   value.
                   If cast-operation is set to a function the characteristic's
                   value is first casted by the cast-operation to another type
                   and afterwords processed by main-opertion.")
  (:method ((construct CharacteristicC) (operation Function))
    (funcall operation (charvalue construct))))


(defgeneric names-by-type (construct nametype &key revision)
  (:documentation "Returns all names that are of the corresponding type.")
  (:method ((construct TopicC) nametype &key (revision *TM-REVISION*))
    (declare (integer revision)
	     (type (or Null TopicC) nametype))
    (remove-if-not #'(lambda(name)
		       (eql nametype (instance-of name :revision revision)))
		   (names construct :revision revision))))


(defgeneric occurrences-by-type (construct occurrencetype &key revision)
  (:documentation "Returns all names that are of the corresponding type.")
  (:method ((construct TopicC) (occurrencetype TopicC)
	    &key (revision *TM-REVISION*))
    (declare (integer revision))
    (remove-if-not #'(lambda(occ)
		       (eql occurrencetype (instance-of occ :revision revision)))
		   (occurrences construct :revision revision))))


(defgeneric characteristics-by-type (construct chartype &key revision)
  (:documentation "Returns all characteristics that are of the
                   corresponding type.")
  (:method ((construct TopicC) (chartype TopicC) &key (revision *TM-REVISION*))
    (declare (integer revision))
    (union (names-by-type construct chartype :revision revision)
	   (occurrences-by-type construct chartype :revision revision))))


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
      (remove-null results))))


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
      (remove-null results))))


(defgeneric characteristics-by-value (construct filter &key revision)
  (:documentation "Returns a list of all characteristics of the passed
                   topic, that return a true value when calling filter.")
  (:method ((construct TopicC) (filter Function) &key (revision *TM-REVISION*))
    (declare (integer revision))
    (union (names-by-value construct filter :revision revision)
	   (occurrences-by-value construct filter :revision revision))))


(defgeneric occurrences-by-datatype (construct datatype &key revision)
  (:documentation "Returns all occurrences of the specified datatype.")
  (:method ((construct TopicC) datatype &key (revision *TM-REVISION*))
    (declare (type (or Null String) datatype)
	     (Integer revision))
    (if datatype
	(remove-null
	 (map 'list #'(lambda(occ)
			(when (string= (datatype occ) datatype)
			  occ))
	      (occurrences construct :revision revision)))
	(occurrences construct :revision revision))))


(defgeneric isa (construct type &key revision)
  (:documentation "Returns all types if the passed construct
                   is of the specified type.")
  (:method ((construct TopicC) (type TopicC) &key (revision *TM-REVISION*))
    (declare (integer revision))
    (let ((all-types (instance-of construct :revision revision)))
      (when (find type all-types)
	all-types))))


(defgeneric aka (construct supertype &key revision)
  (:documentation "Returns all types if the passed construct
                   is of the specified type.")
  (:method ((construct TopicC) (supertype TopicC) &key (revision *TM-REVISION*))
    (declare (integer revision))
    (let ((all-supertypes (supertypes construct :revision revision)))
      (when (find supertype all-supertypes)
	all-supertypes))))
