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
	   :abstract-p))

(in-package :json-tmcl)


;; -----------------------------------------------------------------------------
;; --- all fragment constraints ------------------------------------------------
;; -----------------------------------------------------------------------------
(defun get-constraints-of-fragment(topic-psi &key (treat-as 'type))
  (let ((associationtype (get-item-by-psi *associationtype-psi*))
	(associationtype-constraint (get-item-by-psi *associationtype-constraint-psi*))
	(topic
	 (let ((psi
		(elephant:get-instance-by-value 'PersistentIdC 'uri topic-psi)))
	   (when psi
	     (identified-construct psi)))))
    (when topic
      (let ((topic-constraints
	     (let ((value
		    (get-constraints-of-topic topic :treat-as treat-as)))
	       (concatenate 'string "\"topicConstraints\":" value))))
	(let ((available-associations ;what's with association which have only a associationrole-constraint?
	       (get-available-associations-of-topic topic :treat-as treat-as)))
	  (dolist (item available-associations)
	    (topictype-p item associationtype associationtype-constraint))
	  (let ((associations-constraints
		 (concatenate 'string "\"associationsConstraints\":"
			      (let ((inner-associations-constraints "["))
				(loop for available-association in available-associations
				   do (let ((value
					     (get-constraints-of-association available-association)))
					(setf inner-associations-constraints
					      (concatenate 'string inner-associations-constraints value ","))))
				(if (string= inner-associations-constraints "[")
				    (setf inner-associations-constraints "null")
				    (setf inner-associations-constraints
					  (concatenate 'string (subseq inner-associations-constraints 0 (- (length inner-associations-constraints) 1)) "]")))))))
	    (let ((json-string
		   (concatenate 'string
				"{" topic-constraints "," associations-constraints "}")))
	      json-string)))))))
    

;; -----------------------------------------------------------------------------
;; --- all association constraints ---------------------------------------------
;; -----------------------------------------------------------------------------
(defun get-constraints-of-association (associationtype-topic)
  "Returns a list of constraints which are describing associations of the 
   passed associationtype-topic."
  (let ((constraint-topics
	 (get-all-constraint-topics-of-association associationtype-topic)))
    (let ((associationtypescope-constraints
	   (let ((value (get-typescope-constraints associationtype-topic :what 'association)))
	     (concatenate 'string "\"scopeConstraints\":" value)))
	  (associationrole-constraints
	   (let ((value
		  (get-associationrole-constraints (getf constraint-topics :associationrole-constraints))))
	     (concatenate 'string "\"associationRoleConstraints\":" value)))
	  (roleplayer-constraints
	   (let ((value
		  (get-roleplayer-constraints (getf constraint-topics :roleplayer-constraints))))
	     (concatenate 'string "\"rolePlayerConstraints\":"  value)))
	  (otherrole-constraints
	   (let ((value
		  (get-otherrole-constraints (getf constraint-topics :otherrole-constraints))))
	     (concatenate 'string "\"otherRoleConstraints\":" value))))
      (let ((json-string
	     (concatenate 'string "{" associationrole-constraints "," roleplayer-constraints ","
			  otherrole-constraints "," associationtypescope-constraints "}")))
	json-string))))


(defun get-otherrole-constraints (constraint-topics)
  "Returns a list of the form
   ((::role <topic> :player <topic> :otherrole <topic> :othertopic <topic> :card-min <string> :card-max <string>) <...>)
   which describes an otherrole constraint for the parent-association of a give type."
  (let ((applies-to (get-item-by-psi *applies-to-psi*))
	(constraint-role (get-item-by-psi *constraint-role-psi*))
	(topictype-role (get-item-by-psi *topictype-role-psi*))
	(roletype-role (get-item-by-psi *roletype-role-psi*))
	(othertopictype-role (get-item-by-psi *othertopictype-role-psi*))
	(otherroletype-role (get-item-by-psi *otherroletype-role-psi*))
	(roletype (get-item-by-psi *roletype-psi*))
	(roletype-constraint (get-item-by-psi *roletype-constraint-psi*)))
    (let ((otherrole-constraints
	   (loop for constraint-topic in constraint-topics
	      append (let ((players nil)
			   (roletypes nil)
			   (otherplayers nil)
			   (otherroletypes nil)
			   (constraint-list
			    (get-constraint-topic-values constraint-topic)))
		       (loop for role in (player-in-roles constraint-topic)
			  when (and (eq constraint-role (instance-of role))
				    (eq applies-to (instance-of (parent role))))
			  do (loop for other-role in (roles (parent role))
				do (let ((current-player (player other-role))
					 (current-role (instance-of other-role)))
				     (cond
				       ((eq topictype-role current-role)
					(push current-player players))
				       ((eq roletype-role current-role)
					(push current-player roletypes))
				       ((eq othertopictype-role current-role)
					(push current-player otherplayers))
				       ((eq otherroletype-role current-role)
					(push current-player otherroletypes))))))
		       (when (and (append players roletypes otherplayers otherroletypes)
				  (or (not players) (not roletypes) (not otherplayers) (not otherroletypes)))
			 (error "otherroletype-constraint ~a is not complete:~%players: ~a~%roletypes: ~a~%otherplayers: ~a~%otherroletypes: ~a~%"
				(uri (first (psis constraint-topic)))
				(map 'list #'(lambda(x)(uri (first (psis x)))) players)
				(map 'list #'(lambda(x)(uri (first (psis x)))) roletypes)
				(map 'list #'(lambda(x)(uri (first (psis x)))) otherplayers)
				(map 'list #'(lambda(x)(uri (first (psis x)))) otherroletypes)))
		       (let ((cross-product-1
			      (loop for player in players
				 append (loop for roletype in roletypes
					   collect (list :player player :role roletype))))
			     (cross-product-2
			      (loop for otherplayer in otherplayers
				   append (loop for otherroletype in otherroletypes
					     collect (list :otherplayer otherplayer :otherrole otherroletype)))))
			 (let ((cross-product
				(loop for tupple-1 in cross-product-1
				     append (loop for tupple-2 in cross-product-2
					       collect (append tupple-1 tupple-2 (list :constraint constraint-list))))))
			   cross-product))))))
      (let ((involved-topic-tupples
	     (remove-duplicates
	      (loop for otherrole-constraint in otherrole-constraints
		 collect (let ((player (getf otherrole-constraint :player))
			       (role-type (getf otherrole-constraint :role))
			       (otherplayer (getf otherrole-constraint :otherplayer))
			       (otherrole-type (getf otherrole-constraint :otherrole)))
			   (topictype-p player)
			   (topictype-p role-type roletype roletype-constraint)
			   (topictype-p otherplayer)
			   (topictype-p otherrole-type roletype roletype-constraint)
			   (list :player player
				 :role role-type
				 :otherplayer otherplayer
				 :otherrole otherrole-type)))
	      :test #'(lambda(x y)
			(and (eq (getf x :player) (getf y :player))
			     (eq (getf x :role) (getf y :role))
			     (eq (getf x :otherplayer) (getf y :otherplayer))
			     (eq (getf x :otherrole) (getf y :otherrole)))))))
	(let ((cleaned-otherrole-constraints "["))
	  (loop for involved-topic-tupple in involved-topic-tupples
	     do (let ((constraint-lists
		       (remove-duplicate-constraints
			(loop for otherrole-constraint in otherrole-constraints
			   when (and (eq (getf otherrole-constraint :player) (getf involved-topic-tupple :player))
				     (eq (getf otherrole-constraint :role) (getf involved-topic-tupple :role))
				     (eq (getf otherrole-constraint :otherplayer) (getf involved-topic-tupple :otherplayer))
				     (eq (getf otherrole-constraint :otherrole) (getf involved-topic-tupple :otherrole)))
			   collect (getf otherrole-constraint :constraint)))))
		  (when (> (length constraint-lists) 1)
		    (error "found contrary otherrole-constraints:~%player: ~a~%role: ~a~%otherplayer: ~a~%otherrole: ~a~% ~a~%"
			   (uri (first (psis (getf involved-topic-tupple :player))))
			   (uri (first (psis (getf involved-topic-tupple :role))))
			   (uri (first (psis (getf involved-topic-tupple :otherplayer))))
			   (uri (first (psis (getf involved-topic-tupple :otherrole))))
			   constraint-lists))
		  (let ((json-player
			 (concatenate 'string "\"playerType\":"
				      (json-exporter::identifiers-to-json-string (getf involved-topic-tupple :player))))
			(json-role
			 (concatenate 'string "\"roleType\":"
				      (json-exporter::identifiers-to-json-string (getf involved-topic-tupple :role))))
			(json-otherplayer
			 (concatenate 'string "\"otherPlayerType\":"
				      (json-exporter::identifiers-to-json-string (getf involved-topic-tupple :player))))
			(json-otherrole
			 (concatenate 'string "\"otherRoleType\":"
				      (json-exporter::identifiers-to-json-string (getf involved-topic-tupple :role))))
			(card-min
			 (concatenate 'string "\"cardMin\":" (getf (first constraint-lists) :card-min)))
			(card-max
			 (concatenate 'string "\"cardMax\":" (getf (first constraint-lists) :card-max))))
		    (setf cleaned-otherrole-constraints
			  (concatenate 'string cleaned-otherrole-constraints
				       "{" json-player "," json-role "," json-otherplayer "," json-otherrole "," card-min "," card-max "},")))))
	  (if (string= cleaned-otherrole-constraints "[")
	      (setf cleaned-otherrole-constraints "null")
	      (setf cleaned-otherrole-constraints 
		    (concatenate 'string (subseq cleaned-otherrole-constraints 0 (- (length cleaned-otherrole-constraints) 1)) "]")))
	  cleaned-otherrole-constraints)))))


(defun get-roleplayer-constraints (constraint-topics)
  "Returns a list of the form
   ((:role <topic> :player <topic> :card-min <string> :card-max <string>) <...>)
   which describes the cardinality of topctypes used as players in roles of given
   types in an association of a given type which is also the parent if this list."
  (let ((applies-to (get-item-by-psi *applies-to-psi*))
	(constraint-role (get-item-by-psi *constraint-role-psi*))
	(topictype-role (get-item-by-psI *topictype-role-psi*))
	(roletype-role (get-item-by-psi *roletype-role-psi*))
	(roletype (get-item-by-psi *roletype-psi*))
	(roletype-constraint (get-item-by-psi *roletype-constraint-psi*)))
    (let ((roleplayer-constraints
	   (loop for constraint-topic in constraint-topics
	      append (let ((constraint-list
			    (get-constraint-topic-values constraint-topic)))
		       (let ((players
			      (loop for role in (player-in-roles constraint-topic)
				 when (and (eq constraint-role (instance-of role))
					   (eq applies-to (instance-of (parent role))))
				 append (loop for other-role in (roles (parent role))
					   when (eq topictype-role (instance-of other-role))
					   collect (player other-role))))
			     (roles
			      (loop for role in (player-in-roles constraint-topic)
				 when (and (eq constraint-role (instance-of role))
					   (eq applies-to (instance-of (parent role))))
				 append (loop for other-role in (roles (parent role))
					   when (eq roletype-role (instance-of other-role))
					   collect (player other-role)))))
			 (when (or (and players (not roles))
				   (and roles (not players)))
			   (error "roleplayer-constraint ~a is not complete:~%players: ~a~%roles: ~a~%"
				  (uri (first (psis constraint-topic)))
				  (map 'list #'(lambda(x)(uri (first (psis x)))) players)
				  (map 'list #'(lambda(x)(uri (first (psis x)))) roles)))
			 (let ((cross-product
				(loop for player in players
				   append (loop for role in roles
					     collect (list :player player :role role :constraint constraint-list)))))
			   cross-product))))))

      (let ((role-player-tupples
	     (remove-duplicates
	      (loop for roleplayer-constraint in roleplayer-constraints
		 collect (let ((current-player (getf roleplayer-constraint :player))
			       (current-role (getf roleplayer-constraint :role)))
			   (topictype-p current-player)
			   (topictype-p current-role roletype roletype-constraint)
			   (list :player current-player
				 :role current-role)))
	      :test #'(lambda(x y)
			(and (eq (getf x :player) (getf y :player))
			     (eq (getf x :role) (getf y :role)))))))
			   
	(let ((cleaned-roleplayer-constraints "["))
	  (loop for role-player-tupple in role-player-tupples
	     do (let ((constraint-lists
		       (remove-duplicate-constraints
			(loop for roleplayer-constraint in roleplayer-constraints
			   when (and (eq (getf roleplayer-constraint :player) (getf role-player-tupple :player))
				     (eq (getf roleplayer-constraint :role) (getf role-player-tupple :role)))
			   collect (getf roleplayer-constraint :constraint)))))
		  (when (> (length constraint-lists) 1)
		    (error "found contrary roleplayer-constraints:~%role: ~a~%player: ~a~% ~a ~%"
			   (uri (first (psis (getf role-player-tupple :role))))
			   (uri (first (psis (getf role-player-tupple :player))))
			   constraint-lists))
		  (let ((json-player
			 (concatenate 'string "\"playerType\":"
				      (json-exporter::identifiers-to-json-string (getf role-player-tupple :player))))
			(json-role
			 (concatenate 'string "\"roleType\":"
				      (json-exporter::identifiers-to-json-string (getf role-player-tupple :role))))
			(card-min
			 (concatenate 'string "\"cardMin\":" (getf (first constraint-lists) :card-min)))
			(card-max
			 (concatenate 'string "\"cardMax\":" (getf (first constraint-lists) :card-max))))
		    (setf cleaned-roleplayer-constraints
			  (concatenate 'string cleaned-roleplayer-constraints
				       "{" json-player "," json-role "," card-min "," card-max "},")))))
	  (if (string= cleaned-roleplayer-constraints "[")
	      (setf cleaned-roleplayer-constraints "null")
	      (setf cleaned-roleplayer-constraints 
		    (concatenate 'string (subseq cleaned-roleplayer-constraints 0 (- (length cleaned-roleplayer-constraints) 1)) "]")))
	  cleaned-roleplayer-constraints)))))


(defun get-associationrole-constraints (constraint-topics)
  "Returns a list of the form
   ((:associationroletype <topic> :card-min <string> :card-max <string>), <...>)
   which describes all associationrole-constraints of the passed
   constraint-topics."
  (let ((applies-to (get-item-by-psi *applies-to-psi*))
	(roletype-role (get-item-by-psi *roletype-role-psi*))
	(constraint-role (get-item-by-psi *constraint-role-psi*))
	(roletype (get-item-by-psi *roletype-psi*))
	(roletype-constraint (get-item-by-psi *roletype-constraint-psi*)))
    (let ((associationrole-constraints
	   (loop for constraint-topic in constraint-topics
	      append (let ((constraint-list
			    (get-constraint-topic-values constraint-topic)))
		       (loop for role in (player-in-roles constraint-topic)
			  when (and (eq constraint-role (instance-of role))
				    (eq applies-to (instance-of (parent role))))
			  append (loop for other-role in (roles (parent role))
				    when (eq roletype-role (instance-of other-role))
				    collect (list :associationroletype (player other-role)
						  :constraint constraint-list)))))))
      (let ((associationroletype-topics
	     (remove-duplicates (map 'list #'(lambda(x)
					       (let ((associationroletype (getf x :associationroletype)))
						 (topictype-p associationroletype roletype roletype-constraint)
						 associationroletype))
				     associationrole-constraints))))
	(let ((cleaned-associationrole-constraints "["))
	  (loop for associationroletype-topic in associationroletype-topics
	     do (let ((constraint-lists
			    (remove-duplicate-constraints
			     (loop for associationrole-constraint in associationrole-constraints
				when (eq associationroletype-topic (getf associationrole-constraint :associationroletype))
				collect (getf associationrole-constraint :constraint)))))
		  (when (> (length constraint-lists) 1)
		    (error "found contrary associationrole-constraints: ~a ~a~%" (uri (first (psis associationroletype-topic))) constraint-lists))
		  (setf cleaned-associationrole-constraints
			(concatenate 'string
				     cleaned-associationrole-constraints
				     "{\"roleType\":" (json-exporter::identifiers-to-json-string associationroletype-topic)
				     ",\"cardMin\":" (getf (first constraint-lists) :card-min)
				     ",\"cardMax\":" (getf (first constraint-lists) :card-max) "},"))))

	  (if (string= cleaned-associationrole-constraints "[")
	      (setf cleaned-associationrole-constraints "null")
	      (setf cleaned-associationrole-constraints 
		    (concatenate 'string (subseq cleaned-associationrole-constraints 0 (- (length cleaned-associationrole-constraints) 1)) "]")))
	  cleaned-associationrole-constraints)))))


;; -----------------------------------------------------------------------------
;; --- all topic constraints ---------------------------------------------------
;; -----------------------------------------------------------------------------
(defun get-constraints-of-topic (topic-instance &key(treat-as 'type))
    "Returns a constraint list with the constraints:
     subjectidentifier-constraints, subjectlocator-constraints,
     topicname-constraints, topicoccurrence-constraints and
     uniqueoccurrence-constraints."
  (let ((constraint-topics
	 (get-all-constraint-topics-of-topic topic-instance :treat-as treat-as)))
    (let ((exclusive-instance-constraints
	   (let ((value
		  (get-exclusive-instance-constraints (getf constraint-topics :exclusive-instance-constraints))))
	     (concatenate 'string "\"exclusiveInstances\":" value)))
	  (subjectidentifier-constraints
	   (let ((value
		  (get-simple-constraints (getf constraint-topics :subjectidentifier-constraints) :error-msg-constraint-name "subjectidentifier")))
	     (concatenate 'string "\"subjectIdentifierConstraints\":" value)))
	  (subjectlocator-constraints
	   (let ((value
		  (get-simple-constraints (getf constraint-topics :subjectlocator-constraints) :error-msg-constraint-name "subjectlocator")))
	     (concatenate 'string "\"subjectLocatorConstraints\":" value)))
	  (topicname-constraints
	   (let ((value
		  (get-topicname-constraints (getf constraint-topics :topicname-constraints))))
	     (concatenate 'string "\"topicNameConstraints\":" value)))
	  (topicoccurrence-constraints
	   (let ((value
		  (get-topicoccurrence-constraints (getf constraint-topics :topicoccurrence-constraints) 
						   (getf constraint-topics :uniqueoccurrence-constraints))))
	     (concatenate 'string "\"topicOccurrenceConstraints\":" value)))
	  (abstract-constraint
	   (concatenate 'string "\"abstractConstraint\":"
			(if (getf constraint-topics :abstract-constraint)
			    "true"
			    "false"))))
      (let ((json-string
	     (concatenate 'string "{" exclusive-instance-constraints "," subjectidentifier-constraints
			  "," subjectlocator-constraints "," topicname-constraints ","
			  topicoccurrence-constraints "," abstract-constraint "}")))
        json-string))))


(defun get-exclusive-instance-constraints(exclusive-instances-lists)
  "Returns a list of psis which represents some topics."
  (let ((constraint-role (get-item-by-psi *constraint-role-psi*))
	(applies-to (get-item-by-psi *applies-to-psi*))
	(topictype-role (get-item-by-psi *topictype-role-psi*)))
    (let ((topics
	   (remove-duplicates
	    (loop for exclusive-instances-list in exclusive-instances-lists
	       append (let ((owner (getf exclusive-instances-list :owner))
			    (exclusive-constraints (getf exclusive-instances-list :exclusive-constraints)))
			(loop for exclusive-constraint in exclusive-constraints
			   append (loop for role in (player-in-roles exclusive-constraint)
				     when (and (eq constraint-role (instance-of role))
					       (eq applies-to (instance-of (parent role))))
				     append (loop for other-role in (roles (parent role))
					       when (and (eq topictype-role (instance-of other-role))
							 (not (eq owner (player other-role))))
					       collect (player other-role)))))))))
      (json:encode-json-to-string (map 'list #'(lambda(y)
						 (map 'list #'uri y))
				       (map 'list #'psis topics))))))


(defun get-simple-constraints(constraint-topics &key (error-msg-constraint-name "uniqueoccurrence"))
  "Returns a list of the form
   ((:regexp <string> :card-min <string> :card-max <string>))
   which contains the subjectidentifier, subjectlocator or
   unique-occurrence constraints. This depends on the passed
   constraint-topics."
  (let ((all-values
	 (remove-duplicate-constraints
	  (loop for constraint-topic in constraint-topics
	     collect (get-constraint-topic-values constraint-topic)))))
    (let ((contrary-constraints (find-contrary-constraints all-values)))
      (when contrary-constraints
	(error "found contrary ~a-constraints: ~a~%" error-msg-constraint-name contrary-constraints)))
    (simple-constraints-to-json all-values)))


(defun simple-constraints-to-json(simple-constraints)
  "Transforms a list of simple constraint lists of the form
   ((:regexp <string> :card-min <string> :card-max <string>) <...>)
   to a valid json list of the form
   [{\"regexp\":\"expr\",\"cardMin\":\"123\",\"cardMax\":\"456\"}, <...>]."
  (let ((constraints "["))
    (loop for constraint in simple-constraints
       do (let ((constraint (concatenate 'string "{\"regexp\":"
					 (json:encode-json-to-string (getf constraint :regexp))
					 ",\"cardMin\":"
					 (json:encode-json-to-string (getf constraint :card-min))
					 ",\"cardMax\":"
					 (json:encode-json-to-string (getf constraint :card-max))
					 "}")))
	    (if (string= constraints "[")
		(setf constraints (concatenate 'string constraints constraint))
		(setf constraints (concatenate 'string constraints "," constraint)))))
    (if (string= constraints "[")
	(setf constraints "null")
	(setf constraints (concatenate 'string constraints "]")))
    constraints))


(defun get-topicname-constraints(constraint-topics)
  "Returns all topicname constraints as a list of the following form:
   ( ( :type <nametype-topic>
       :constraints ( ( :regexp <string> :card-min <string> :card-max <string>)
                      <...>)
               :scopes ( ( :scope <scope-topic> :regexp <string> :card-min <string> :card-max <string>)
                          <...>))
         <...>)."
  (let ((constraint-role (get-item-by-psi *constraint-role-psi*))
	(applies-to (get-item-by-psi *applies-to-psi*))
	(nametype-role (get-item-by-psi *nametype-role-psi*))
	(nametype (get-item-by-psi *nametype-psi*))
	(nametype-constraint (get-item-by-psi *nametype-constraint-psi*)))
    (let ((topicname-constraints
	   (remove-if #'null
		      (loop for constraint-topic in constraint-topics
			 append (loop for role in (player-in-roles constraint-topic)
				   when (and (eq constraint-role (instance-of role))
					     (eq applies-to (instance-of (parent role))))
				   append (loop for other-role in (roles (parent role))
					     when (eq nametype-role (instance-of other-role))
					     collect (let ((nametype-topic (player other-role))
							   (constraint-list (get-constraint-topic-values constraint-topic)))
						       (list :type nametype-topic :constraint constraint-list))))))))
      
      (let ((nametype-topics
	     (remove-duplicates (map 'list #'(lambda(x)
					       (let ((topicname-type
						      (getf x :type)))
						 (topictype-p topicname-type nametype nametype-constraint)
						 topicname-type))
				     topicname-constraints))))
	(let ((cleaned-topicname-constraints "["))
	  (loop for nametype-topic in nametype-topics
	     do (let ((constraint-lists
		       (remove-duplicate-constraints
			(loop for topicname-constraint in topicname-constraints
			   when (eq nametype-topic (getf topicname-constraint :type))
			   collect (getf topicname-constraint :constraint)))))
		  (let ((contrary-constraints
			 (find-contrary-constraints constraint-lists)))
		    (when contrary-constraints
		      (error "found contrary topicname-constraints: ~a~%" contrary-constraints)))
		  (let ((typescope-constraints
			 (let ((current-scopes
				(get-typescope-constraints nametype-topic :what 'topicname)))
			   (concatenate 'string "\"scopeConstraints\":" current-scopes)))
			(json-constraint-lists
			 (concatenate 'string "\"constraints\":" (simple-constraints-to-json constraint-lists)))
			(type-topic
			 (concatenate 'string "\"nameType\":"
				      (json-exporter::identifiers-to-json-string nametype-topic))))
		    (setf cleaned-topicname-constraints
			  (concatenate 'string cleaned-topicname-constraints "{" type-topic "," json-constraint-lists "," typescope-constraints "},")))))
	  (if (string= cleaned-topicname-constraints "[")
	      (setf cleaned-topicname-constraints "null")
	      (setf cleaned-topicname-constraints
		    (concatenate 'string (subseq cleaned-topicname-constraints 0 (- (length cleaned-topicname-constraints) 1)) "]")))
	  cleaned-topicname-constraints)))))


(defun get-topicoccurrence-constraints(constraint-topics unique-constraint-topics)
  "Returns all topicoccurrence constraints as a list of the following form:
   ( ( :type <occurrencetype-topic>
       :constraints ( ( :regexp <string> :card-min <string> :card-max <string>)
                      <...>)
       :scopes ( ( :scope <scope-topic> :regexp <string> :card-min <string> :card-max <string>)
                 <...>)
       :datatype <string>
       :uniqe ( ( :regexp <string> :dard-min <string> :card-max <string> ) )
     <...>)."
  (let ((constraint-role (get-item-by-psi *constraint-role-psi*))
	(applies-to (get-item-by-psi *applies-to-psi*))
	(occurrencetype-role (get-item-by-psi *occurrencetype-role-psi*))
	(occurrencetype (get-item-by-psi *occurrencetype-psi*))
	(occurrencetype-constraint (get-item-by-psi *occurrencetype-constraint-psi*)))
    (let ((topicoccurrence-constraints
	   (remove-if #'null
		      (loop for constraint-topic in constraint-topics
			 append (loop for role in (player-in-roles constraint-topic)
				   when (and (eq constraint-role (instance-of role))
					     (eq applies-to (instance-of (parent role))))
				   append (loop for other-role in (roles (parent role))
					     when (eq occurrencetype-role (instance-of other-role))
					     collect (let ((occurrencetype-topic (player other-role))
							   (constraint-list (get-constraint-topic-values constraint-topic)))
						       (list :type occurrencetype-topic :constraint constraint-list))))))))
      (let ((occurrencetype-topics
	     (remove-duplicates (map 'list #'(lambda(x)
					       (let ((occurrence-type (getf x :type)))
						 (topictype-p occurrence-type occurrencetype occurrencetype-constraint)
						 occurrence-type))
				     topicoccurrence-constraints))))
	(let ((cleaned-topicoccurrence-constraints "["))
	  (loop for occurrencetype-topic in occurrencetype-topics
	     do (let ((constraint-lists
		       (remove-duplicate-constraints
			(loop for topicoccurrence-constraint in topicoccurrence-constraints
			   when (eq occurrencetype-topic (getf topicoccurrence-constraint :type))
			   collect (getf topicoccurrence-constraint :constraint)))))
		  (let ((contrary-constraints
			 (find-contrary-constraints constraint-lists)))
		    (when contrary-constraints
		      (error "found contrary topicname-constraints: ~a~%" contrary-constraints)))
		  (let ((type-topic
			 (concatenate 'string "\"occurrenceType\":"
				      (json-exporter::identifiers-to-json-string occurrencetype-topic)))
			(typescope-constraints
			 (let ((current-scopes
				(get-typescope-constraints occurrencetype-topic :what 'topicoccurrence)))
			   (concatenate 'string "\"scopeConstraints\":" current-scopes)))
			(datatype-constraint
			 (concatenate 'string "\"datatypeConstraint\":"
				      (get-occurrence-datatype-constraint occurrencetype-topic)))
			(unique-constraints
			 (concatenate 'string "\"uniqueConstraints\":"
				      (get-simple-constraints unique-constraint-topics)))
			(json-constraint-lists
			 (concatenate 'string "\"constraints\":" (simple-constraints-to-json constraint-lists))))
		    (setf cleaned-topicoccurrence-constraints
			  (concatenate 'string cleaned-topicoccurrence-constraints
				       "{" type-topic "," json-constraint-lists "," typescope-constraints "," datatype-constraint "," unique-constraints "},")))))
	  (if (string= cleaned-topicoccurrence-constraints "[")
	      (setf cleaned-topicoccurrence-constraints "null")
	      (setf cleaned-topicoccurrence-constraints
		    (concatenate 'string (subseq cleaned-topicoccurrence-constraints 0 (- (length cleaned-topicoccurrence-constraints) 1)) "]")))
	  cleaned-topicoccurrence-constraints)))))


(defun get-occurrence-datatype-constraint(occurrencetype-topic)
  "Return a datatype qualifier as a string."
  (let ((constraint-role (get-item-by-psi *constraint-role-psi*))
	(applies-to (get-item-by-psi *applies-to-psi*))
	(occurrencetype-role (get-item-by-psi *occurrencetype-role-psi*))
	(datatype (get-item-by-psi *datatype-psi*))
	(occurrencedatatype-constraint (get-item-by-psi *occurrencedatatype-constraint-psi*)))
    (let ((datatype-constraints
	   (remove-duplicates
	    (loop for role in (player-in-roles occurrencetype-topic)
	       when (and (eq occurrencetype-role (instance-of role))
			 (eq applies-to (instance-of (parent role))))
	       append (loop for other-role in (roles (parent role))
			 when (and (eq constraint-role (instance-of other-role))
				   (topictype-of-p (player other-role) occurrencedatatype-constraint))
			 collect (player other-role))))))
      (let ((datatype-constraint
	     (remove-duplicates
	      (map 'list #'(lambda(constraint-topic)
			     (loop for occurrence in (occurrences constraint-topic)
				when (and (eq (instance-of occurrence) datatype)
					  (slot-boundp occurrence 'charvalue))
				return (charvalue occurrence)))
		   datatype-constraints))))
	(when (> (length datatype-constraint) 1)
	  (error "found contrary occurrence-datatype-constraints: ~a~%" datatype-constraints))
	(if datatype-constraint
	    (json:encode-json-to-string (first datatype-constraint))
	    nil)))))


(defun get-typescope-constraints(element-type-topic &key(what 'topicname))
  "Returns a list of scopes for the element-typetopic which is the type topic of
   a topicname, a topicoccurrence or an association. To specifiy of what kind
   of element the scopes should be there is the key-variable what.
   It can be set to 'topicname, 'topicoccurrence or 'association.
   The return value is of the form
   ( :scope <scope-topic>
     :constraint (:card-min <string> :card-max <string> ))."
  (let ((element-type-role-and-scope-constraint
	 (cond
	   ((eq what 'topicname)
	    (list (get-item-by-psi *nametype-role-psi*)
		  (get-item-by-psi *nametypescope-constraint-psi*)))
	   ((eq what 'topicoccurrence)
	    (list
	     (get-item-by-psi *occurrencetype-role-psi*)
	     (get-item-by-psi *occurrencetypescope-constraint-psi*)))
	   ((eq what 'association)
	    (list
	     (get-item-by-psi *associationtype-role-psi*)
	     (get-item-by-psi *associationtypescope-constraint-psi*)))))
	(scopetype-role (get-item-by-psi *scopetype-role-psi*))
	(constraint-role (get-item-by-psi *constraint-role-psi*))
	(applies-to (get-item-by-psi *applies-to-psi*)))
    (when (and (= (length element-type-role-and-scope-constraint) 2)
	       (first element-type-role-and-scope-constraint)
	       (second element-type-role-and-scope-constraint))
      (let ((type-role (first element-type-role-and-scope-constraint))
	    (typescope-constraint (second element-type-role-and-scope-constraint)))
	(let ((typescope-constraints
	       (loop for role in (player-in-roles element-type-topic)
		  when (and (eq type-role (instance-of role))
			    (eq applies-to (instance-of (parent role))))
		  append (loop for other-role in (roles (parent role))
			    when (and (eq constraint-role (instance-of other-role))
				      (topictype-of-p (player other-role) typescope-constraint))
			    collect (let ((scopes nil)
					  (constraint nil))
				      (loop for c-role in (player-in-roles (player other-role))
					 when (and (eq constraint-role (instance-of c-role))
						   (eq applies-to (instance-of (parent c-role))))
					 do (progn
					      (setf constraint (get-constraint-topic-values (player c-role)))
					      (loop for c-other-role in (roles (parent c-role))
						 when (eq scopetype-role (instance-of c-other-role))
						 do (push (player c-other-role) scopes))))
				      (list :scopes scopes :constraint constraint))))))
	  (let ((scopetype-groups
		 (remove-duplicates (map 'list #'(lambda(x)
						   (let ((scopes (getf x :scopes)))
						     (when scopes
						       scopes)))
					 typescope-constraints)
				    :test #'(lambda(x y)
					      (when (and (= (length x) (length y))
							 (= (length x) (length (intersection x y))))
						t)))))
	    (let ((cleaned-typescope-constraints "["))
	      (loop for scopetype-group in scopetype-groups
		 do (let ((constraint-lists
			   (remove-duplicate-constraints
			    (loop for typescope-constraint in typescope-constraints
			       when (and (= (length (getf typescope-constraint :scopes))
					    (length scopetype-group))
					 (= (length (getf typescope-constraint :scopes))
					    (length (intersection (getf typescope-constraint :scopes) scopetype-group))))
			       collect (getf typescope-constraint :constraint)))))
		      (when (> (length constraint-lists) 1)
			(error "found contrary scopetype-constraints for ~a: ~a~%"
			       (map 'list #'(lambda(x)(uri (first (psis x)))) scopetype-group)
			       constraint-lists))
		      (let ((card-min (getf (first constraint-lists) :card-min))
			    (card-max (getf (first constraint-lists) :card-max)))
			(let ((json-scopes "\"scopeTypes\":["))
			  (dolist (item scopetype-group)
			    (let ((json-list (json-exporter::identifiers-to-json-string item)))
			      (setf json-scopes (concatenate 'string json-scopes json-list ","))))
			  (setf json-scopes (subseq json-scopes 0 (- (length json-scopes) 1)))
			  (let ((current-json-string
				 (concatenate 'string "{" json-scopes "],\"cardMin\":\"" card-min "\",\"cardMax\":\"" card-max "\"}")))
			    (setf cleaned-typescope-constraints 
				  (concatenate 'string cleaned-typescope-constraints current-json-string ",")))))))
	      (if (string= cleaned-typescope-constraints "[")
		  (setf cleaned-typescope-constraints "null")
		  (setf cleaned-typescope-constraints 
			(concatenate 'string (subseq cleaned-typescope-constraints 0 (- (length cleaned-typescope-constraints) 1)) "]")))
	      cleaned-typescope-constraints)))))))
    

;; -----------------------------------------------------------------------------
;; --- some basic helpers ------------------------------------------------------
;; -----------------------------------------------------------------------------
(defun get-constraint-topic-values(topic)
  "Returns all constraint values of the passed topic in the
   following form (list :regexp regexp :card-min card-min :card-max card-max)"
  (let ((regexp
	 (get-constraint-occurrence-value topic))
	(card-min
	 (get-constraint-occurrence-value topic :what 'card-min))
	(card-max
	 (get-constraint-occurrence-value topic :what 'card-max)))
    (when (and (string/= "MAX_INT" card-max)
	       (> (parse-integer card-min) (parse-integer card-max)))
      (error "card-min (~a) must be < card-max (~a)" card-min card-max))
    (list :regexp regexp :card-min card-min :card-max card-max)))


(defun get-constraint-occurrence-value(topic &key (what 'regexp))
  "Checks the occurrence-value of a regexp, card-min or card-max
   constriant-occurrence.
   If what = 'regexp and the occurrence-value is empty there will be returned
   the value '.*!'.
   If what = 'card-min and the occurrence-value is empty there will be returned
   the value '0'.
   If what = 'card-max and the occurrence-value is empty there will be returned
   the value 'MAX_INT'"
  (let ((occurrence-type
	 (get-item-by-psi
	  (cond 
	    ((eq what 'regexp)
	     *regexp-psi*)
	    ((eq what 'card-min)
	     *card-min-psi*)
	    ((eq what 'card-max)
	     *card-max-psi*)
	    (t
	     "")))))
    (when occurrence-type
      (let ((occurrence-value
	     (let ((occurrence
		    (find occurrence-type (occurrences topic) :key #'instance-of)))
	       (if (and occurrence
			(slot-boundp occurrence 'charvalue)
			(> (length  (charvalue occurrence)) 0))
		   (charvalue occurrence)
		   (cond
		     ((eq what 'regexp)
		      ".*")
		     ((eq what 'card-min)
		      "0")
		     ((eq what 'card-max)
		      "MAX_INT"))))))
	(cond
	  ((eq what 'card-min)
	   (let ((is-valid
		  (handler-case (let ((card-min
				       (parse-integer occurrence-value)))
				  (when (>= card-min 0)
				    t))
		    (condition () nil))))
	     (unless is-valid
	       (error "card-min in ~a is \"~a\" but should be >= 0"
		      (uri (first (psis topic)))
		      occurrence-value))))
	  ((eq what 'card-max)
	   (let ((is-valid
		  (handler-case (let ((card-max
				       (parse-integer occurrence-value)))
				  (when (>= card-max 0)
				    t))
		    (condition () (when (string= occurrence-value "MAX_INT")
				    t)))))
	     (unless is-valid
	       (error "card-max in ~a is \"~a\" but should be >= 0 or \"MAX_INT\""
		      (uri (first (psis topic)))
		      occurrence-value)))))
	occurrence-value))))
	  

(defun find-contrary-constraints(constraint-lists)
  "Returns a list which contains a list of minimum two contrary constraints
   or nil if there are no contrary constraints.
   The list is of the form
   (list (list :regexp <regexp> :card-min <card-min> :card-max <card-max>) (list ...))."
  (let ((current-constraint nil))
    (loop for constraint-list in constraint-lists
       do (progn
	    (when (> (length current-constraint) 0)
	      (return-from find-contrary-constraints current-constraint))
	    (setf current-constraint (remove-if #'null (map 'list #'(lambda(x)
								      (contrary-constraint-list x constraint-list))
							    constraint-lists)))))))

(defun contrary-constraint-list (lst-1 lst-2)
  "Returns both passed lists when they have the same
   regular expression but different card-min or card-max values."
  (when (and (typep lst-1 'list) (typep lst-2 'list)
	     (= 6 (length lst-1) (length lst-2)))
    (when (and (string= (getf lst-1 :regexp) (getf lst-2 :regexp))
	       (or (string/= (getf lst-1 :card-min) (getf lst-2 :card-min))
		   (string/= (getf lst-1 :card-max) (getf lst-2 :card-max))))
      (list lst-1 lst-2))))
  

(defun remove-duplicate-constraints(constraint-lists)
  "Removes duplicate constraints of the passed constraint list.
   This list should have the form
   (list (list :regexp <regexp> :card-min <card-min> :card-max <card-max>) (list ...)).
   A constraint is defined as equal whan all three value (regexp, card-min and card-max
   are equal."
  (remove-duplicates constraint-lists :test #'eql-constraint-list))



(defun eql-constraint-list (lst-1 lst-2)
  "Compares two constraint lists of the form (list <string> <string> string>)
   or (list <topic> <string> <string> <string>."
  (when (and (typep lst-1 'list) (typep lst-2 'list)
	     (= 6 (length lst-1) (length lst-2)))
    (and (string= (getf lst-1 :regexp) (getf lst-2 :regexp))
	 (string= (getf lst-1 :card-min) (getf lst-2 :card-min))
	 (string= (getf lst-1 :card-max) (getf lst-2 :card-max)))))


;; --- checks if the given topic is a valid topictype --------------------------
(defun get-direct-types-of-topic(topic-instance)
  "Returns the direct types of the topic as a list passed to this function.
   This function only returns the types of the type-instance-relationship -> TMDM 7.2"
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
   This function only returns the types of the supertype-subtype-relationship -> TMDM 7.3"
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


(defun subtype-p (topic-instance &optional (topictype (get-item-by-psi *topictype-psi*)) (checked-topics nil))
  "Returns a list of all supertypes of the passed topic if the passed topic
   is not an instanceOf any other topic but a subtype of some supertypes
   of topictype or it is the topictype-topic itself."
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


(defun topictype-of-p (topic-instance type-instance &optional checked-topics)
  "Returns a list of all types and supertypes of this topic if this topic is a
   valid instance-topic of the type-topic called type-instance. TMCL 4.4.2"
  (let ((current-checked-topics (append checked-topics (list topic-instance)))
	(topictype (get-item-by-psi *topictype-psi*))
	(isas-of-this (get-direct-types-of-topic topic-instance))
	(akos-of-this (get-direct-supertypes-of-topic topic-instance)))

    (when (eq topic-instance topictype)
      t)

    (when (and (not isas-of-this)
	       (not akos-of-this))
      (return-from topictype-of-p nil))

    (loop for isa-of-this in isas-of-this
       do (let ((found-topics (topictype-p isa-of-this)))
	    (when (not found-topics)
	      (return-from topictype-of-p nil))
	    (dolist (item found-topics)
	      (pushnew item current-checked-topics))))

    (loop for ako-of-this in akos-of-this
       when (not (find ako-of-this current-checked-topics :test #'eq))
       do (let ((found-topics (topictype-of-p ako-of-this type-instance current-checked-topics)))
	    (when (not found-topics)
	      (return-from topictype-of-p nil))
	    (dolist (item found-topics)
	      (pushnew item current-checked-topics))))

      (when (find type-instance current-checked-topics)
	current-checked-topics)))


;; --- gets all constraint topics ----------------------------------------------
(defun get-direct-constraint-topics-of-topic (topic-instance)
  "Returns all constraint topics defined for the passed topic-instance"
  (let ((constraint-role (get-item-by-psi *constraint-role-psi*))
	(topictype-role (get-item-by-psi *topictype-role-psi*))
	(applies-to (get-item-by-psi *applies-to-psi*))
	(abstract-topictype-constraint (get-item-by-psi *abstract-topictype-constraint-psi*))
	(exclusive-instance-constraint (get-item-by-psi *exclusive-instance-psi*))
	(subjectidentifier-constraint (get-item-by-psi *subjectidentifier-constraint-psi*))
	(subjectlocator-constraint (get-item-by-psi *subjectlocator-constraint-psi*))
	(topicname-constraint (get-item-by-psi *topicname-constraint-psi*))
	(topicoccurrence-constraint (get-item-by-psi *topicoccurrence-constraint-psi*))
	(uniqueoccurrence-constraint (get-item-by-psi *uniqueoccurrence-constraint-psi*))
	(roleplayer-constraint (get-item-by-psi *roleplayer-constraint-psi*))
	(otherrole-constraint (get-item-by-psi *otherrole-constraint-psi*))
	(abstract-topictype-constraints nil)
	(exclusive-instance-constraints nil)
	(subjectidentifier-constraints nil)
	(subjectlocator-constraints nil)
	(topicname-constraints nil)
	(topicoccurrence-constraints nil)
	(uniqueoccurrence-constraints nil))

    (loop for role in (player-in-roles topic-instance)
       when (and (eq topictype-role (instance-of role))
		 (eq applies-to (instance-of (parent role))))
       do (loop for other-role in (roles (parent role))
	     when (eq constraint-role (instance-of other-role))
	     do (let ((constraint-topic (player other-role)))
		  (cond
		    ((topictype-of-p constraint-topic abstract-topictype-constraint)
		     (pushnew constraint-topic abstract-topictype-constraints))
		    ((topictype-of-p constraint-topic exclusive-instance-constraint)
		     (pushnew constraint-topic exclusive-instance-constraints))
		    ((topictype-of-p constraint-topic subjectidentifier-constraint)
		     (pushnew constraint-topic subjectidentifier-constraints))
		    ((topictype-of-p constraint-topic subjectlocator-constraint)
		     (pushnew constraint-topic subjectlocator-constraints))
		    ((topictype-of-p constraint-topic topicname-constraint)
		     (pushnew constraint-topic topicname-constraints))
		    ((topictype-of-p constraint-topic topicoccurrence-constraint)
		     (pushnew constraint-topic topicoccurrence-constraints))
		    ((topictype-of-p constraint-topic uniqueoccurrence-constraint)
		     (pushnew constraint-topic uniqueoccurrence-constraints))
		    (t
		     (unless (or (topictype-of-p constraint-topic roleplayer-constraint)
				 (topictype-of-p constraint-topic otherrole-constraint))
		      (error "Constraint-Topic \"~a\" could not be handled" (uri (first (psis constraint-topic))))))))))
    (list :abstract-topictype-constraints abstract-topictype-constraints
	  :exclusive-instance-constraints (list :exclusive-constraints exclusive-instance-constraints
						:owner topic-instance)
	  :subjectidentifier-constraints subjectidentifier-constraints
	  :subjectlocator-constraints subjectlocator-constraints
	  :topicname-constraints topicname-constraints
	  :topicoccurrence-constraints topicoccurrence-constraints
	  :uniqueoccurrence-constraints uniqueoccurrence-constraints)))


(defmethod get-all-constraint-topics-of-topic (topic-instance &key (treat-as 'type))
  "Returns a list of constraint-topics of the topics-instance's base type(s).
   If topic c is instanceOf a and b, there will be returned all
   constraint-topics of the topic types a and b.
   If treat-as is set to instance there will be only the constraints collected
   defined for the supertypes or the types of the passed topic - all constraints
   defined directly for the passed topic are ignored, unless the passed topic is
   an instance of itself."
  (let ((akos-and-isas-of-this
	 (remove-duplicates
	  (if (eql treat-as 'type)
	      (topictype-p topic-instance)
	      (loop for topic in (union (get-direct-types-of-topic topic-instance) (get-direct-supertypes-of-topic topic-instance))
		   append (topictype-p topic))))))
    (let ((all-abstract-topictype-constraints nil)
	  (all-exclusive-instance-constraints nil)
	  (all-subjectidentifier-constraints nil)
	  (all-subjectlocator-constraints nil)
	  (all-topicname-constraints nil)
	  (all-topicoccurrence-constraints nil)
	  (all-uniqueoccurrence-constraints nil))
      (loop for topic in akos-and-isas-of-this
	 do (let ((constraint-topics-of-topic (get-direct-constraint-topics-of-topic topic)))
	      (when (eq topic topic-instance)
		(dolist (item (getf constraint-topics-of-topic :abstract-topictype-constraints))
		  (pushnew item all-abstract-topictype-constraints)))
	      (let ((exclusive-instance-constraints
		     (getf constraint-topics-of-topic :exclusive-instance-constraints)))
		(when (getf exclusive-instance-constraints :exclusive-constraints)
		  (push exclusive-instance-constraints all-exclusive-instance-constraints)))
	      (dolist (item (getf constraint-topics-of-topic :subjectidentifier-constraints))
		(pushnew item all-subjectidentifier-constraints))
	      (dolist (item (getf constraint-topics-of-topic :subjectlocator-constraints))
		(pushnew item all-subjectlocator-constraints))
	      (dolist (item (getf constraint-topics-of-topic :topicname-constraints))
		(pushnew item all-topicname-constraints))
	      (dolist (item (getf constraint-topics-of-topic :topicoccurrence-constraints))
		(pushnew item all-topicoccurrence-constraints))
	      (dolist (item (getf constraint-topics-of-topic :uniqueoccurrence-constraints))
		(pushnew item all-uniqueoccurrence-constraints))))
      (list :abstract-topictype-constraints all-abstract-topictype-constraints
	    :exclusive-instance-constraints all-exclusive-instance-constraints
	    :subjectidentifier-constraints all-subjectidentifier-constraints
	    :subjectlocator-constraints all-subjectlocator-constraints
	    :topicname-constraints all-topicname-constraints
	    :topicoccurrence-constraints all-topicoccurrence-constraints
	    :uniqueoccurrence-constraints all-uniqueoccurrence-constraints))))


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


(defun get-direct-constraint-topics-of-association(associationtype-topic)
  "Returns all direct constraint topics defined for associations if
   the passed associationtype-topic"
  (let ((constraint-role (get-item-by-psi *constraint-role-psi*))
	(associationtype-role (get-item-by-psi *associationtype-role-psi*))
	(applies-to (get-item-by-psi *applies-to-psi*))
	(associationtypescope-constraint (get-item-by-psi *associationtypescope-constraint-psi*))
	(associationrole-constraint (get-item-by-psi *associationrole-constraint-psi*))
	(roleplayer-constraint (get-item-by-psi *roleplayer-constraint-psi*))
	(otherrole-constraint (get-item-by-psi *otherrole-constraint-psi*))
	(associationrole-constraints nil)
	(roleplayer-constraints nil)
	(otherrole-constraints nil))

    (loop for role in (player-in-roles associationtype-topic)
       when (and (eq associationtype-role (instance-of role))
		 (eq applies-to (instance-of (parent role))))
       do (loop for other-role in (roles (parent role))
	     when (eq constraint-role (instance-of other-role))
	     do (let ((constraint-topic (player other-role)))
		  (cond
		    ((topictype-of-p constraint-topic associationtypescope-constraint)
		     t) ;do nothing
		    ((topictype-of-p constraint-topic associationrole-constraint)
		     (pushnew constraint-topic associationrole-constraints))
		    ((topictype-of-p constraint-topic roleplayer-constraint)
		     (pushnew constraint-topic roleplayer-constraints))
		    ((topictype-of-p constraint-topic otherrole-constraint)
		     (pushnew constraint-topic otherrole-constraints))
		    (t
		     (error "Constraint-Topic \"~a\" could not be handled" (uri (first (psis constraint-topic)))))))))

    (list :associationrole-constraints associationrole-constraints
	  :roleplayer-constraints roleplayer-constraints
	  :otherrole-constraints otherrole-constraints)))


(defun get-all-constraint-topics-of-association(associationtype-topic)
  "Returns all constraint topics defined for associations if
   the passed associationtype-topic."
  (let ((akos-and-isas-of-this
	 (topictype-p associationtype-topic (get-item-by-psi *associationtype-psi*) (get-item-by-psi *associationtype-constraint-psi*))))
    (let ((all-associationrole-constraints nil)
	  (all-roleplayer-constraints nil)
	  (all-otherrole-constraints nil))
      (loop for topic in akos-and-isas-of-this
	 do (let ((constraint-topics-of-topic
		   (get-direct-constraint-topics-of-association topic)))
	      (dolist (item (getf constraint-topics-of-topic :associationrole-constraints))
		(pushnew item all-associationrole-constraints))
	      (dolist (item (getf constraint-topics-of-topic :roleplayer-constraints))
		(pushnew item all-roleplayer-constraints))
	      (dolist (item (getf constraint-topics-of-topic :otherrole-constraints))
		(pushnew item all-otherrole-constraints))))
      (list :associationrole-constraints all-associationrole-constraints
	    :roleplayer-constraints all-roleplayer-constraints
	    :otherrole-constraints all-otherrole-constraints))))


(defun get-available-associations-of-topic(topic-instance &key (treat-as 'type))
  "Returns a list of topics decribing the available associationtype for the
   passed topic."
  (let ((applies-to (get-item-by-psi *applies-to-psi*))
	(topictype-role (get-item-by-psi *topictype-role-psi*))
	(constraint-role (get-item-by-psi *constraint-role-psi*))
	(othertopictype-role (get-item-by-psi *othertopictype-role-psi*))
	(associationtype-role (get-item-by-psi *associationtype-role-psi*))
	(roleplayer-constraint (get-item-by-psi *roleplayer-constraint-psi*))
	(otherrole-constraint (get-item-by-psi *otherrole-constraint-psi*))
	(all-possible-player-topics	 
	 (remove-duplicates
	  (if (eql treat-as 'type)
	      (topictype-p topic-instance)
	      (loop for topic in (union (get-direct-types-of-topic topic-instance) (get-direct-supertypes-of-topic topic-instance))
		 append (topictype-p topic))))))
    

    ;what's with associationrole-constraints without a player-constraint???
    (let ((all-available-associationtypes
	   (remove-duplicates
	    (loop for possible-player-topic in all-possible-player-topics
	       append (loop for role in (player-in-roles possible-player-topic)
			 when (and (or (eq topictype-role (instance-of role))
				       (eq othertopictype-role (instance-of role)))
				   (eq applies-to (instance-of (parent role))))
			 append (loop for other-role in (roles (parent role))
				   when (and (eq constraint-role (instance-of other-role))
					     (or (topictype-of-p (player other-role) roleplayer-constraint)
						 (topictype-of-p (player other-role) otherrole-constraint)))
				   append (loop for c-role in (player-in-roles (player other-role))
					     when (and (eq constraint-role (instance-of c-role))
						       (eq applies-to (instance-of (parent c-role))))
					     append (loop for type-role in (roles (parent c-role))
						       when (eq associationtype-role (instance-of type-role))
						       collect (player type-role)))))))))
      all-available-associationtypes)))