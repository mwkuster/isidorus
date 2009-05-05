;;+-----------------------------------------------------------------------------
;;+  Isidorus
;;+  (c) 2008-2009 Marc Kuester, Christoph Ludwig, Lukas Giessmann
;;+
;;+  Isidorus is freely distributable under the LGPL license.
;;+  You can find a detailed description in trunk/docs/LGPL-LICENSE.txt.
;;+-----------------------------------------------------------------------------


(defpackage :json-tmcl
  (:use :cl :datamodel :constants :json-tmcl-constants)
  (:export :get-constraints-of-fragment
	   :topictype-p
	   :abstract-p
	   :list-subtypes))


(in-package :json-tmcl)


(defun abstract-p (topic-instance)
  "Returns t if this topic type is an abstract topic type."
  (let ((constraint-role (get-item-by-psi *constraint-role-psi*))
	(topictype-role (get-item-by-psi *topictype-role-psi*))
	(applies-to (get-item-by-psi *applies-to-psi*))
	(abstract-topictype-constraint (get-item-by-psi *abstract-topictype-constraint-psi*)))
    
    (loop for role in (player-in-roles topic-instance)
       when (and (eq topictype-role (instance-of role))
		 (eq applies-to (instance-of (parent role))))
       return (loop for other-role in (roles (parent role))
		 when (and (eq constraint-role (instance-of other-role))
			   (eq abstract-topictype-constraint (player other-role)))
		 return t))))


(defun topictype-of-p (topic-instance type-instance &optional (topictype (get-item-by-psi *topictype-psi*))
		                                              (topictype-constraint (get-item-by-psi *topictype-constraint-psi*))
                                                               checked-topics)
  "Returns a list of all types and supertypes of this topic if this topic is a
   valid instance-topic of the type-topic called type-instance. TMCL 4.4.2.
   When the type-instance is set to nil there will be checked only if the
   topic-instance is a valid instance."
  (let ((current-checked-topics (append checked-topics (list topic-instance)))
	(isas-of-this (get-direct-types-of-topic topic-instance))
	(akos-of-this (get-direct-supertypes-of-topic topic-instance)))

    (when (eq topic-instance topictype)
      t)

    (when (and (not isas-of-this)
	       (not akos-of-this))
      (return-from topictype-of-p nil))

    (loop for isa-of-this in isas-of-this
       do (let ((found-topics (topictype-p isa-of-this topictype topictype-constraint)))
	    (when (not found-topics)
	      (return-from topictype-of-p nil))
	    (dolist (item found-topics)
	      (pushnew item current-checked-topics))))

    (loop for ako-of-this in akos-of-this
       when (not (find ako-of-this current-checked-topics :test #'eq))
       do (let ((found-topics (topictype-of-p ako-of-this type-instance topictype topictype-constraint current-checked-topics)))
	    (when (not found-topics)
	      (return-from topictype-of-p nil))
	    (dolist (item found-topics)
	      (pushnew item current-checked-topics))))

    (if type-instance
	(when (find type-instance current-checked-topics)
	  current-checked-topics)
	current-checked-topics)))


(defun topictype-p (topic-instance &optional (topictype (get-item-by-psi *topictype-psi*))
		                             (topictype-constraint (get-item-by-psi *topictype-constraint-psi*))
		                             (checked-topics nil))
  "Returns a list of all instanceOf-topics and all Supertypes of this topic
   if this topic is a valid topic (-type). I.e. the passed topic is the
   topictype or it is an instanceOf of the topictype or it is a subtype of
   the topictype. TMDM 7.2 + TMDM 7.3"
  ;(format t "~%~%topictype-p ~a~%" (uri (first (psis topic-instance))))
  (let ((current-checked-topics (append checked-topics (list topic-instance)))
	(akos-of-this (get-direct-supertypes-of-topic topic-instance))
	(isas-of-this (get-direct-types-of-topic topic-instance)))

    (when (eq topictype topic-instance)
      (return-from topictype-p current-checked-topics))

    (when (not (union akos-of-this isas-of-this :test #'eq))
      (when topictype-constraint
	;(return-from topictype-p nil))
	(error "~a is not a valid type for ~a" (uri (first (psis topic-instance))) (uri (first (psis topictype)))))
      (return-from topictype-p current-checked-topics))

    (let ((akos-are-topictype nil))
      (loop for ako-of-this in akos-of-this
	 when (not (find ako-of-this current-checked-topics))
	 do (let ((further-topics (topictype-p ako-of-this topictype topictype-constraint)))
	      (if further-topics
		  (progn
		    (dolist (item further-topics)
		      (pushnew item current-checked-topics))
		    (pushnew ako-of-this akos-are-topictype))
		  (when topictype-constraint
		    ;(return-from topictype-p nil)))))
		    (error "~a is not a valid type for ~a" (uri (first (psis topic-instance))) (uri (first (psis topictype))))))))

      (when isas-of-this
	(let ((topictype-topics-of-isas nil))
	  (loop for isa-of-this in isas-of-this
	     do (let ((topic-akos (subtype-p isa-of-this topictype)))
		  (when topic-akos
		    (pushnew isa-of-this topictype-topics-of-isas)
		    (pushnew isa-of-this current-checked-topics)
		    (dolist (item topic-akos)
		      (pushnew item current-checked-topics)))))
	  
	  (when (and (not topictype-topics-of-isas)
		     (not akos-are-topictype)
		     topictype-constraint)
	    ;(return-from topictype-p nil))
	    (error "~a is not a valid type for ~a" (uri (first (psis topic-instance))) (uri (first (psis topictype)))))
	  
	  (loop for isa-of-this in isas-of-this
	     when (and (not (find isa-of-this current-checked-topics :test #'eq))
		       (not (find isa-of-this topictype-topics-of-isas :test #'eq)))
	     do (let ((further-topic-types (topictype-p isa-of-this topictype topictype-constraint current-checked-topics)))
		  (if further-topic-types
		      (dolist (item further-topic-types)
			(pushnew item current-checked-topics))
		      (when topictype-constraint
			;(return-from topictype-p nil))))))))
			(error "~a is not a valid type for ~a" (uri (first (psis topic-instance))) (uri (first (psis topictype)))))))))))
    current-checked-topics))


(defun subtype-p (topic-instance &optional (topictype (get-item-by-psi *topictype-psi*)) (checked-topics nil))
  "Returns a list of all supertypes of the passed topic if the passed topic
   is not an instanceOf any other topic but a subtype of some supertypes
   of a topictype or it is the topictype-topic itself.
   This function isn't useable as a standalone function - it's only necessary
   for a special case in the function topictype-p."
  ;(format t "~%~%subtype-p ~a~%" (uri (first (psis topic-instance))))
  (let ((current-checked-topics (remove-duplicates (append checked-topics (list topic-instance)))))

    (when (eq topictype topic-instance)
      (return-from subtype-p current-checked-topics))

    (when (get-direct-types-of-topic topic-instance)
      (return-from subtype-p nil))

    (let ((supertypes-of-this (get-direct-supertypes-of-topic topic-instance)))
      (when (not supertypes-of-this)
	(return-from subtype-p nil))
      (when supertypes-of-this
	(loop for supertype-of-this in supertypes-of-this
	   when (not (find supertype-of-this current-checked-topics :test #'eq))
	   do (let ((further-supertypes (subtype-p topictype supertype-of-this current-checked-topics)))
		(when (not further-supertypes)
		  (return-from subtype-p nil))

		(dolist (item further-supertypes)
		  (pushnew item current-checked-topics))))))

    current-checked-topics))


(defun get-direct-types-of-topic(topic-instance)
  "Returns the direct types of the topic as a list passed to this function.
   This function only returns the types of the type-instance-relationship -> TMDM 7.2
   This function was defined for the use in topictype-p and not for a standalone
   usage."
  (let ((type-instance (get-item-by-psi *type-instance-psi*))
	(instance (get-item-by-psi *instance-psi*))
	(type (get-item-by-psi *type-psi*)))
    (let ((topic-types
	   (loop for role in (player-in-roles topic-instance)
	      when (eq instance (instance-of role))
	      collect (loop for other-role in (roles (parent role))
			 when (and (not (eq role other-role))
				   (eq type-instance (instance-of (parent role)))
				   (eq type (instance-of other-role)))
			 return (player other-role)))))
      (when topic-types
	(remove-if #'null topic-types)))))


(defun get-direct-supertypes-of-topic(topic-instance)
  "Returns the direct supertypes of the topic as a list passed to this function.
   This function only returns the types of the supertype-subtype-relationship -> TMDM 7.3.
   This function was defined for the use in topictype-p and not for a standalone
   usage."
  (let ((supertype-subtype (get-item-by-psi *supertype-subtype-psi*))
	(supertype (get-item-by-psi *supertype-psi*))
	(subtype (get-item-by-psi *subtype-psi*)))
    (let ((supertypes
	   (loop for role in (player-in-roles topic-instance)
	      when (eq subtype (instance-of role))
	      append (loop for other-role in (roles (parent role))
			 when (and (not (eq role other-role))
				   (eq supertype-subtype (instance-of (parent role)))
				   (eq supertype (instance-of other-role)))
			 collect (player other-role)))))
      (remove-if #'null supertypes))))


(defun list-subtypes (topic-instance &optional (topictype (get-item-by-psi *topictype-psi*))
		                               (topictype-constraint (get-item-by-psi *topictype-constraint-psi*))
		                               (checked-topics nil) (valid-subtypes nil))
  "Returns all valid subtypes of a topic, e.g.:
   nametype-constraint ako constraint .
   first-name isa nametype .
   first-name-1 ako first-name .
   // ...
   The return value is a named list of the form (:subtypes (<topic> <...>) :checked-topics (<topic> <...>)"
  (let ((current-checked-topics (append checked-topics (list topic-instance))))

    (handler-case (topictype-p topic-instance topictype topictype-constraint)
      (condition () (return-from list-subtypes (list :subtypes nil :checked-topics current-checked-topics))))

    (let ((subtype (get-item-by-psi *subtype-psi*))
	  (supertype (get-item-by-psi *supertype-psi*))
	  (supertype-subtype (get-item-by-psi *supertype-subtype-psi*))
	  (current-valid-subtypes (append valid-subtypes (list topic-instance))))
      (loop for role in (player-in-roles topic-instance)
	 when (and (eq supertype (instance-of role))
		   (eq supertype-subtype (instance-of (parent role))))
	 do (loop for other-role in (roles (parent role))			 
	       do (when (and (eq subtype (instance-of other-role))
			     (not (find (player other-role) current-checked-topics)))
		    (let ((new-values
			   (list-subtypes (player other-role) topictype topictype-constraint current-checked-topics current-valid-subtypes)))
		      (dolist (item (getf new-values :subtypes))
			(pushnew item current-valid-subtypes))
		      (dolist (item (getf new-values :checked-topics))
			(pushnew item current-checked-topics))))))
      (list :subtypes current-valid-subtypes :checked-topics current-checked-topics))))


(defun list-instances (topic-instance &optional (topictype (get-item-by-psi *topictype-psi*))
                                                (topictype-constraint (get-item-by-psi *topictype-constraint-psi*)))
  "Returns the topic-instance, all subtypes found by the function lis-subtypes and all direct
   instances for the found subtypes."
  (let ((all-subtypes-of-this
	 (getf (list-subtypes topic-instance topictype topictype-constraint) :subtypes))
	(type (get-item-by-psi *type-psi*))
	(instance (get-item-by-psi *instance-psi*))
	(type-instance (get-item-by-psi *type-instance-psi*)))
    (let ((all-instances-of-this
	   (remove-duplicates
	    (loop for subtype-of-this in all-subtypes-of-this
	       append (loop for role in (player-in-roles subtype-of-this)
			 when (and (eq type (instance-of role))
				   (eq type-instance (instance-of (parent role))))
			 append (loop for other-role in (roles (parent role))
				   when (eq instance (instance-of other-role))
				   collect (player other-role)))))))
      (let ((all-subtypes-of-all-instances
	     (remove-if #'null
			(remove-duplicates
			 (loop for subtype in all-instances-of-this
			    append (getf (list-subtypes subtype nil nil) :subtypes))))))
	(remove-if #'null
		   (map 'list #'(lambda(x)
				  (handler-case (progn
						  (topictype-of-p x nil)
						  x)
				    (condition () nil)))
			all-subtypes-of-all-instances))))))