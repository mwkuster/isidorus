;;+-----------------------------------------------------------------------------
;;+  Isidorus
;;+  (c) 2008-2010 Marc Kuester, Christoph Ludwig, Lukas Georgieff
;;+
;;+  Isidorus is freely distributable under the LLGPL license.
;;+  You can find a detailed description in trunk/docs/LLGPL-LICENSE.txt and
;;+  trunk/docs/LGPL-LICENSE.txt.
;;+-----------------------------------------------------------------------------

(defpackage :json-tmcl
  (:use :cl :datamodel :constants :json-tmcl-constants :json-importer)
  (:export :get-constraints-of-fragment
	   :topictype-p
	   :abstract-p
	   :valid-instance-p
	   :list-subtypes))


(in-package :json-tmcl)


(defun abstract-p (topic-instance &key (revision *TM-REVISION*))
  "Returns t if this topic type is an abstract topic type."
  (declare (type (or integer null) revision)
	   (TopicC topic-instance))
  (let ((constraint-role (get-item-by-psi *constraint-role-psi* :revision revision))
	(topictype-role (get-item-by-psi *topictype-role-psi* :revision revision))
	(applies-to (get-item-by-psi *applies-to-psi* :revision revision))
	(abstract-topictype-constraint
	 (get-item-by-psi *abstract-topictype-constraint-psi* :revision revision)))
    (loop for role in (player-in-roles topic-instance :revision revision)
       when (and (eq topictype-role (instance-of role :revision revision))
		 (eq applies-to (instance-of (parent role :revision revision)
					     :revision revision)))
       return (loop for other-role in (roles (parent role :revision revision)
					     :revision revision)
		 when (and (eq constraint-role (instance-of other-role
							    :revision revision))
			   (topictype-of-p (player other-role :revision revision)
					   abstract-topictype-constraint nil nil
					   nil revision))
		 return t))))


(defun topictype-of-p (topic-instance type-instance &optional
		       (topictype (get-item-by-psi *topictype-psi* :revision 0))
		       (topictype-constraint (is-type-constrained :revision 0))
		       checked-topics (revision *TM-REVISION*))
  "Returns a list of all types and supertypes of this topic if this topic is a
   valid instance-topic of the type-topic called type-instance. TMCL 4.4.2.
   When the type-instance is set to nil there will be checked only if the
   topic-instance is a valid instance."
  (declare (type (or integer null) revision)
	   (TopicC topic-instance)
	   (type (or TopicC null) topictype-constraint)
	   (list checked-topics))
  (let ((current-checked-topics (append checked-topics (list topic-instance)))
	(isas-of-this (get-direct-types-of-topic topic-instance :revision revision))
	(akos-of-this (get-direct-supertypes-of-topic topic-instance
						      :revision revision)))
    (when (eq topic-instance topictype)
      t)
    (when (and (not isas-of-this)
	       (not akos-of-this))
      (return-from topictype-of-p nil))
    (loop for isa-of-this in isas-of-this
       do (let ((found-topics
		 (topictype-p isa-of-this topictype topictype-constraint nil revision)))
	    (when (not found-topics)
	      (return-from topictype-of-p nil))
	    (dolist (item found-topics)
	      (pushnew item current-checked-topics))))
    (loop for ako-of-this in akos-of-this
       when (not (find ako-of-this current-checked-topics :test #'eq))
       do (let ((found-topics
		 (topictype-of-p ako-of-this type-instance topictype
				 topictype-constraint current-checked-topics
				 revision)))
	    (when (not found-topics)
	      (return-from topictype-of-p nil))
	    (dolist (item found-topics)
	      (pushnew item current-checked-topics))))
    (if type-instance
	(when (find type-instance current-checked-topics)
	  current-checked-topics)
	current-checked-topics)))


(defun topictype-p (topic-instance &optional
		    (topictype (get-item-by-psi *topictype-psi* :revision 0))
		    (topictype-constraint (is-type-constrained :revision 0))
		    (checked-topics nil) (revision *TM-REVISION*))
  "Returns a list of all instanceOf-topics and all Supertypes of this topic
   if this topic is a valid topic (-type). I.e. the passed topic is the
   topictype or it is an instanceOf of the topictype or it is a subtype of
   the topictype. TMDM 7.2 + TMDM 7.3"
  (declare (type (or integer null) revision)
	   (type (or TopicC null) topictype topic-instance)
	   (list checked-topics))
  (let ((current-checked-topics (append checked-topics (list topic-instance)))
	(akos-of-this (get-direct-supertypes-of-topic topic-instance
						      :revision revision))
	(isas-of-this (get-direct-types-of-topic topic-instance :revision revision)))
    (when (eq topictype topic-instance)
      (return-from topictype-p current-checked-topics))
    (when (not (union akos-of-this isas-of-this :test #'eq))
      (when topictype-constraint
	(error "~a is not a valid type for ~a"
	       (uri (first (psis topic-instance :revision revision)))
	       (uri (first (psis topictype :revision revision)))))
      (return-from topictype-p current-checked-topics))
    (let ((akos-are-topictype nil))
      (loop for ako-of-this in akos-of-this
	 when (not (find ako-of-this current-checked-topics))
	 do (let ((further-topics
		   (topictype-p ako-of-this topictype topictype-constraint
				nil revision)))
	      (if further-topics
		  (progn
		    (dolist (item further-topics)
		      (pushnew item current-checked-topics))
		    (pushnew ako-of-this akos-are-topictype))
		  (when topictype-constraint
		    (error "~a is not a valid type for ~a"
			   (uri (first (psis topic-instance :revision revision)))
			   (uri (first (psis topictype :revision revision))))))))
      (when isas-of-this
	(let ((topictype-topics-of-isas nil))
	  (loop for isa-of-this in isas-of-this
	     do (let ((topic-akos (subtype-p isa-of-this topictype nil revision)))
		  (when topic-akos
		    (pushnew isa-of-this topictype-topics-of-isas)
		    (pushnew isa-of-this current-checked-topics)
		    (dolist (item topic-akos)
		      (pushnew item current-checked-topics)))))
	  (when (and (not topictype-topics-of-isas)
		     (not akos-are-topictype)
		     topictype-constraint)
	    (error "~a is not a valid type for ~a"
		   (uri (first (psis topic-instance :revision revision)))
		   (uri (first (psis topictype :revision revision)))))
	  (loop for isa-of-this in isas-of-this
	     when (and (not (find isa-of-this current-checked-topics :test #'eq))
		       (not (find isa-of-this topictype-topics-of-isas :test #'eq)))
	     do (let ((further-topic-types
		       (topictype-p isa-of-this topictype topictype-constraint
				    current-checked-topics revision)))
		  (if further-topic-types
		      (dolist (item further-topic-types)
			(pushnew item current-checked-topics))
		      (when topictype-constraint
			(error "~a is not a valid type for ~a"
			       (uri (first (psis topic-instance :revision revision)))
			       (uri (first (psis topictype :revision revision)))))))))))
    current-checked-topics))


(defun subtype-p (topic-instance &optional
		  (topictype (get-item-by-psi *topictype-psi* :revision 0))
		  (checked-topics nil) (revision *TM-REVISION*))
  "Returns a list of all supertypes of the passed topic if the passed topic
   is not an instanceOf any other topic but a subtype of some supertypes
   of a topictype or it is the topictype-topic itself.
   This function isn't useable as a standalone function - it's only necessary
   for a special case in the function topictype-p."
  (declare (type (or integer null) revision)
	   (TopicC topic-instance)
	   (type (or TopicC null) topictype)
	   (list checked-topics))
  (let ((current-checked-topics
	 (remove-duplicates (append checked-topics (list topic-instance)))))
    (when (eq topictype topic-instance)
      (return-from subtype-p current-checked-topics))
    (when (get-direct-types-of-topic topic-instance :revision revision)
      (return-from subtype-p nil))
    (let ((supertypes-of-this
	   (get-direct-supertypes-of-topic topic-instance :revision revision)))
      (when (not supertypes-of-this)
	(return-from subtype-p nil))
      (when supertypes-of-this
	(loop for supertype-of-this in supertypes-of-this
	   when (not (find supertype-of-this current-checked-topics :test #'eq))
	   do (let ((further-supertypes
		     (subtype-p topictype supertype-of-this current-checked-topics
				revision)))
		(when (not further-supertypes)
		  (return-from subtype-p nil))
		(dolist (item further-supertypes)
		  (pushnew item current-checked-topics))))))
    current-checked-topics))


(defun get-direct-types-of-topic(topic-instance &key (revision *TM-REVISION*))
  "Returns the direct types of the topic as a list passed to this function.
   This function only returns the types of the type-instance-relationship -> TMDM 7.2
   This function was defined for the use in topictype-p and not for a standalone
   usage."
  (declare (type (or integer null) revision)
	   (TopicC topic-instance))
  (let ((type-instance (get-item-by-psi *type-instance-psi* :revision revision))
	(instance (get-item-by-psi *instance-psi* :revision revision))
	(type (get-item-by-psi *type-psi* :revision revision)))
    (let ((topic-types
	   (loop for role in (player-in-roles topic-instance :revision revision)
	      when (and (eq instance (instance-of role :revision revision))
			(parent role :revision revision))
	      collect (loop for other-role in
			   (roles (parent role :revision revision) :revision revision)
			 when (and (not (eq role other-role))
				   (eq type-instance (instance-of
						      (parent role :revision revision)
						      :revision revision))
				   (eq type (instance-of other-role
							 :revision revision)))
			 return (player other-role :revision revision)))))
      (when topic-types
	(remove-if #'null topic-types)))))


(defun get-direct-instances-of-topic(topic-instance &key (revision *TM-REVISION*))
  "Returns the direct instances of the topic as a list.
   This function only returns the types of the type-instance-relationship -> TMDM 7.2
   This function was defined for the use in topictype-p and not for a standalone
   usage."
  (declare (type (or integer null) revision)
	   (TopicC topic-instance))
  (let ((type-instance (get-item-by-psi *type-instance-psi* :revision revision))
	(instance (get-item-by-psi *instance-psi* :revision revision))
	(type (get-item-by-psi *type-psi* :revision revision)))
    (let ((topic-instances
	   (loop for role in (player-in-roles topic-instance :revision revision)
	      when (and (eql type (instance-of role :revision revision))
			(parent role :revision revision))
	      collect (loop for other-role in (roles (parent role :revision revision)
						     :revision revision)
			 when (and (not (eq role other-role))
				   (eq type-instance
				       (instance-of (parent role :revision revision)
						    :revision revision))
				   (eq instance (instance-of other-role
							     :revision revision)))
			 return (player other-role :revision revision)))))
      (when topic-instances
	(remove-if #'null topic-instances)))))


(defun get-direct-supertypes-of-topic(topic-instance &key (revision *TM-REVISION*))
  "Returns the direct supertypes of the topic as a list passed to this function.
   This function only returns the types of the supertype-subtype-relationship -> TMDM 7.3.
   This function was defined for the use in topictype-p and not for a standalone
   usage."
  (declare (type (or integer null) revision)
	   (TopicC topic-instance))
  (let ((supertype-subtype (get-item-by-psi *supertype-subtype-psi* :revision revision))
	(supertype (get-item-by-psi *supertype-psi* :revision revision))
	(subtype (get-item-by-psi *subtype-psi* :revision revision)))
    (let ((supertypes
	   (loop for role in (player-in-roles topic-instance :revision revision)
	      when (and (eq subtype (instance-of role :revision revision))
			(parent role :revision revision))
	      append (loop for other-role in (roles (parent role :revision revision)
						    :revision revision)
			 when (and (not (eq role other-role))
				   (eq supertype-subtype
				       (instance-of (parent role :revision revision)
						    :revision revision))
				   (eq supertype
				       (instance-of other-role :revision revision)))
			 collect (player other-role)))))
      (when supertypes
	(remove-if #'null supertypes)))))


(defun get-direct-subtypes-of-topic(topic-instance &key (revision *TM-REVISION*))
  "Returns the direct subtypes of the topic as a list.
   This function only returns the types of the supertype-subtype-relationship
   -> TMDM 7.3.
   This function was defined for the use in topictype-p and not for a standalone
   usage."
  (declare (type (or integer null) revision)
	   (TopicC topic-instance))
  (let ((supertype-subtype (get-item-by-psi *supertype-subtype-psi* :revision revision))
	(supertype (get-item-by-psi *supertype-psi* :revision revision))
	(subtype (get-item-by-psi *subtype-psi* :revision revision)))
    (let ((subtypes
	   (loop for role in (player-in-roles topic-instance :revision revision)
	      when (and (eq supertype (instance-of role :revision revision))
			(parent role :revision revision))
	      append (loop for other-role in (roles (parent role :revision revision)
						    :revision revision)
			 when (and (not (eq role other-role))
				   (eq supertype-subtype
				       (instance-of (parent role :revision revision)
						    :revision revision))
				   (eq subtype (instance-of other-role
							    :revision revision)))
			 collect (player other-role :revision revision)))))
      (when subtypes
	(remove-if #'null subtypes)))))


(defun list-subtypes (topic-instance &optional
		      (topictype (get-item-by-psi *topictype-psi* :revision 0))
		      (topictype-constraint (is-type-constrained :revision 0))
		      (checked-topics nil) (valid-subtypes nil)
		      (revision *TM-REVISION*))
  "Returns all valid subtypes of a topic, e.g.:
   nametype-constraint ako constraint .
   first-name isa nametype .
   first-name-1 ako first-name .
   // ...
   The return value is a named list of the form (:subtypes (<topic> <...>)
   :checked-topics (<topic> <...>)"
  (let ((current-checked-topics (append checked-topics (list topic-instance))))
    (handler-case (topictype-p topic-instance topictype topictype-constraint
			       nil revision)
      (condition () (return-from list-subtypes
		      (list :subtypes nil :checked-topics current-checked-topics))))
    (let ((subtype (get-item-by-psi *subtype-psi* :revision revision))
	  (supertype (get-item-by-psi *supertype-psi* :revision revision))
	  (supertype-subtype (get-item-by-psi *supertype-subtype-psi*
					      :revision revision))
	  (current-valid-subtypes (append valid-subtypes (list topic-instance))))
      (loop for role in (player-in-roles topic-instance :revision revision)
	 when (and (parent role :revision revision)
		   (eq supertype (instance-of role :revision revision))
		   (eq supertype-subtype
		       (instance-of (parent role :revision revision)
				    :revision revision)))
	 do (loop for other-role in (roles (parent role :revision revision)
					   :revision revision)
	       do (when (and (eq subtype (instance-of other-role :revision revision))
			     (not (find (player other-role :revision revision)
					current-checked-topics)))
		    (let ((new-values
			   (list-subtypes (player other-role :revision revision)
					  topictype topictype-constraint
					  current-checked-topics
					  current-valid-subtypes revision)))
		      (dolist (item (getf new-values :subtypes))
			(pushnew item current-valid-subtypes))
		      (dolist (item (getf new-values :checked-topics))
			(pushnew item current-checked-topics))))))
      (list :subtypes current-valid-subtypes :checked-topics current-checked-topics))))


(defun list-instances (topic-instance &optional
		       (topictype (get-item-by-psi *topictype-psi* :revision 0))
		       (topictype-constraint (is-type-constrained :revision 0))
		       (revision *TM-REVISION*))
  "Returns the topic-instance, all subtypes found by the function list-subtypes
   and all direct instances for the found subtypes."
  (let ((all-subtypes-of-this
	 (getf (list-subtypes topic-instance topictype topictype-constraint
			      nil nil revision)
	       :subtypes))
	(type (get-item-by-psi *type-psi* :revision revision))
	(instance (get-item-by-psi *instance-psi* :revision revision))
	(type-instance (get-item-by-psi *type-instance-psi* :revision revision)))
    (let ((all-instances-of-this
	   (remove-duplicates
	    (loop for subtype-of-this in all-subtypes-of-this
	       append (loop for role in (player-in-roles subtype-of-this
							 :revision revision)
			 when (and (parent role :revision revision)
				   (eq type (instance-of role :revision revision))
				   (eq type-instance
				       (instance-of (parent role :revision revision)
						    :revision revision)))
			 append (loop for other-role in
				     (roles (parent role :revision revision)
					    :revision revision)
				   when (eq instance (instance-of other-role
								  :revision revision))
				   collect (player other-role :revision revision)))))))
      (let ((all-subtypes-of-all-instances
	     (remove-if #'null
			(remove-duplicates
			 (loop for subtype in all-instances-of-this
			    append (getf
				    (list-subtypes subtype topictype
						   nil nil nil revision)
				    :subtypes))))))
	(union all-instances-of-this 
	       (remove-if #'null
			  (map 'list #'(lambda(x)
					 (handler-case (progn
							 (topictype-of-p x nil nil nil
									 nil revision)
							 x)
					   (condition () nil)))
			       all-subtypes-of-all-instances)))))))


(defun valid-instance-p (topic-instance &optional
			 (akos-checked nil) (all-checked-topics nil)
			 (revision *TM-REVISION*))
  "Returns a list of all checked topics or throws an exception if the given
   topic is not a valid instance of any topictype in elephant."
  (declare (type (or integer null) revision)
	   (TopicC topic-instance)
	   (list akos-checked all-checked-topics))
  (let ((isas-of-this
	 (get-direct-types-of-topic topic-instance :revision revision))
	(akos-of-this
	 (get-direct-supertypes-of-topic topic-instance :revision revision))
	(psi-of-this (uri (first (psis topic-instance :revision revision))))
	(topictype (get-item-by-psi *topictype-psi* :revision revision))
	(topictype-constraint (is-type-constrained :revision revision))
	(local-all-checked-topics all-checked-topics)
	(local-akos-checked))
    (when (not topictype-constraint)
      (return-from valid-instance-p (list topic-instance)))
    (when (and topictype-constraint
	       (not topictype))
      (error "From valid-instance-p(): The topic \"~a\" does not exist - please create it or remove the topic \"~a\""
	     *topictype-psi*
	     (uri (first (psis topictype-constraint :revision revision)))))
    (when (eql topic-instance topictype)
      (return-from valid-instance-p
	(remove-duplicates (append all-checked-topics (list topic-instance)))))
    (unless (or isas-of-this akos-of-this)
      (error "The topic \"~a\" is not a valid topic-instance for any topic-type"
	      psi-of-this))
    (when (find topic-instance akos-checked)
      (return-from valid-instance-p all-checked-topics))
    (pushnew topic-instance local-all-checked-topics)
    (pushnew topic-instance local-akos-checked)
    (dolist (isa isas-of-this)
      (handler-case (let ((topics
			   (topictype-p isa topictype topictype-constraint
					nil revision)))
		      (dolist (top topics)
			(pushnew top local-all-checked-topics)))
	(condition (err) (error "The topic \"~a\" is not a valid topic-instance for any topic-type~%~%~a"
				psi-of-this err))))

    (dolist (ako akos-of-this)
      (when (not (handler-case
		     (let ((topics
			    (topictype-p ako topictype topictype-constraint
					 all-checked-topics revision)))
				 (dolist (top topics)
				   (pushnew top local-all-checked-topics))
				 (pushnew ako local-akos-checked)
				 topics)
		   (condition () nil)))
	(handler-case 
	    (let ((topics
		   (valid-instance-p ako akos-checked (append all-checked-topics
							      (list ako)) revision)))
			(dolist (top topics)
			  (pushnew top local-all-checked-topics)
			  (pushnew top local-akos-checked))
			topics)
	  (condition (err) (error "The topic \"~a\" is not a valid topic-instance for any topic-type~%~%~a"
				  psi-of-this err)))))
    local-all-checked-topics))


(defun return-all-tmcl-types (&key (revision *TM-REVISION*))
  "Returns all topics that are valid tmcl-types"
  (declare (type (or integer null) revision))
  (let ((all-topics (get-all-topics revision))
	(topictype (get-item-by-psi json-tmcl-constants::*topictype-psi*
				    :revision revision))
	(topictype-constraint (is-type-constrained :revision revision)))
    (let ((all-types
	   (remove-if
	    #'null
	    (map 'list #'(lambda(x)
			   (handler-case
			       (progn
				 (topictype-p x topictype topictype-constraint
					      nil revision)
				 x)
			     (condition () nil))) all-topics))))
      (let ((not-abstract-types
	     (remove-if #'null
			(map 'list #'(lambda(x)
				       (unless (abstract-p x :revision revision)
					 x))
			     all-types))))
	not-abstract-types))))


(defun return-all-tmcl-instances (&key (revision *TM-REVISION*))
  "Returns all topics that are valid instances of any topic type.
   The validity is only oriented on the typing of topics, e.g.
   type-instance or supertype-subtype."
  (declare (type (or integer null) revision))
  (let ((all-topics (get-all-topics revision)))
    (let ((valid-instances
	   (remove-if
	    #'null
	    (map 'list #'(lambda(x)
			   (handler-case (progn
					   (valid-instance-p x nil nil revision)
					   x)
			     (condition () nil))) all-topics))))
      valid-instances)))


(defun is-type-constrained (&key (what *topictype-constraint-psi*)
			    (revision *TM-REVISION*))
  "Returns nil if there is no type-constraint otherwise the instance of
   the type-constraint."
  (declare (string what)
	   (type (or integer null) revision))
  (let ((topictype-constraint (get-item-by-psi what :revision revision)))
    (when topictype-constraint
      (let ((ttc
	     (remove-duplicates
	      (remove-if
	       #'null
	       (remove-if #'(lambda(x) (when (eql topictype-constraint x)
					 t))
			  (get-direct-instances-of-topic topictype-constraint
							 :revision revision))))))
	ttc))))


(defun list-all-supertypes (topic-instance &optional (checked-topics nil)
			    (revision *TM-REVISION*))
  "Returns all supertypes of the given topic recursively."
  (declare (type (or integer null) revision)
	   (TopicC topic-instance)
	   (list checked-topics))
  (let ((current-checked-topics (append checked-topics (list topic-instance)))
	(akos-of-this (get-direct-supertypes-of-topic topic-instance
						      :revision revision)))
    (dolist (ako-of-this akos-of-this)
      (when (not (find ako-of-this current-checked-topics))
	(let ((new-checked-topics
	       (list-all-supertypes ako-of-this current-checked-topics revision)))
	  (dolist (new-topic new-checked-topics)
	    (pushnew new-topic current-checked-topics)))))
    current-checked-topics))


(defun get-all-upper-constrainted-topics (topic &key (revision *TM-REVISION*))
  "Returns all topics that are supertypes or direct types
   of the given topic-type. So all direct constraints of the found
   topics are valid constraints for the given one."
  (declare (TopicC topic)
	   (type (or integer null) revision))
  ;; find all direct types
  (let ((direct-isas-of-this
	 (get-direct-types-of-topic topic :revision revision)))
  ;; find all supertypes (recursive -> transitive relationship
    (let ((all-akos-of-this
	   (list-all-supertypes topic nil revision)))
      (remove-duplicates (union direct-isas-of-this all-akos-of-this)))))