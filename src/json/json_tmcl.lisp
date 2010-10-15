;;+-----------------------------------------------------------------------------
;;+  Isidorus
;;+  (c) 2008-2010 Marc Kuester, Christoph Ludwig, Lukas Georgieff
;;+
;;+  Isidorus is freely distributable under the LLGPL license.
;;+  You can find a detailed description in trunk/docs/LLGPL-LICENSE.txt and
;;+  trunk/docs/LGPL-LICENSE.txt.
;;+-----------------------------------------------------------------------------

(in-package :json-tmcl)


;; =============================================================================
;; --- all fragment constraints ------------------------------------------------
;; =============================================================================
(defun get-constraints-of-fragment(topic-psis &key
				   (treat-as 'type) (revision *TM-REVISION*))
  "Returns a json string with all constraints of this topic-psis.
   topic-psis must contain one item if it is treated as instance otherwise#
   there can be more psis then the fragment will be treated as an instanceOf
   all passed psis."
  (declare (type (or integer null) revision)
	   (symbol treat-as)
	   (list topic-psis))
  (let ((associationtype (get-item-by-psi *associationtype-psi* :revision revision))
	(associationtype-constraint (is-type-constrained
				     :what *associationtype-constraint-psi*
				     :revision revision))
	(topics nil))
    (when (and (not (eql treat-as 'type))
	       (> (length topic-psis) 1))
      (error "From get-constraints-of-fragment: when treat-as is set ot instance there must be exactly one item in topic-psis!"))
    (loop for topic-psi in topic-psis
       do (let ((psi
		 (elephant:get-instance-by-value 'PersistentIdC 'uri topic-psi)))
	    (if psi
		(pushnew (identified-construct psi) topics)
		(error "Topic \"~a\" not found!" topic-psi))))
    (when topics
      (let ((topic-constraints
	     (let ((value
		    (get-constraints-of-topic topics :treat-as treat-as
					      :revision revision)))
	       (concatenate 'string "\"topicConstraints\":" value))))
	(let ((available-associations
	       (remove-duplicates
		(loop for topic in topics
		   append (get-available-associations-of-topic
			   topic :treat-as treat-as :revision revision)))))
	  (dolist (item available-associations)
	    (topictype-p item associationtype associationtype-constraint
			 nil revision))
	  (let ((associations-constraints
		 (concatenate
		  'string "\"associationsConstraints\":"
		  (let ((inner-associations-constraints "["))
		    (loop for available-association in available-associations
		       do (let ((value
				 (get-constraints-of-association
				  available-association :revision revision)))
			    (setf inner-associations-constraints
				  (concatenate 'string inner-associations-constraints
					       value ","))))
		    (if (string= inner-associations-constraints "[")
			(setf inner-associations-constraints "null")
			(setf inner-associations-constraints
			      (concatenate
			       'string
			       (subseq inner-associations-constraints 0
				       (- (length inner-associations-constraints) 1))
			       "]")))))))
	    (let ((json-string
		   (concatenate 'string
				"{" topic-constraints "," associations-constraints
				"}")))
	      json-string)))))))


;; =============================================================================
;; --- all association constraints ---------------------------------------------
;; =============================================================================
(defun get-constraints-of-association (associationtype-topic &key
				       (revision *TM-REVISION*))
  "Returns a list of constraints which are describing associations of the 
   passed associationtype-topic."
  (declare (TopicC associationtype-topic)
	   (type (or integer null) revision))
  (let ((constraint-topics
	 (get-all-constraint-topics-of-association associationtype-topic
						   :revision revision)))
    (let ((associationtype
	   (concatenate 'string "\"associationType\":"
			(json-exporter::identifiers-to-json-string
			 associationtype-topic :revision revision)))
	  (associationtypescope-constraints
	   (let ((value (get-typescope-constraints associationtype-topic
						   :what 'association
						   :revision revision)))
	     (concatenate 'string "\"scopeConstraints\":" value)))
	  (associationrole-constraints
	   (let ((value
		  (get-associationrole-constraints
		   (getf constraint-topics :associationrole-constraints)
		   :revision revision)))
	     (concatenate 'string "\"associationRoleConstraints\":" value)))
	  (roleplayer-constraints
	   (let ((value
		  (get-roleplayer-constraints
		   (getf constraint-topics :roleplayer-constraints)
		   :revision revision)))
	     (concatenate 'string "\"rolePlayerConstraints\":"  value)))
	  (otherrole-constraints
	   (let ((value
		  (get-otherrole-constraints
		   (getf constraint-topics :otherrole-constraints)
		   :revision revision)))
	     (concatenate 'string "\"otherRoleConstraints\":" value))))
      (let ((json-string
	     (concatenate 'string "{" associationtype "," associationrole-constraints
			  "," roleplayer-constraints ","
			  otherrole-constraints "," associationtypescope-constraints
			  "}")))
	json-string))))


(defun get-otherrole-constraints (constraint-topics &key (revision *TM-REVISION*))
  "Returns a list of the form
   ((::role <topic> :player <topic> :otherrole <topic> :othertopic <topic>
   :card-min <string> :card-max <string>) <...>)
   which describes an otherrole constraint for the parent-association of a give type."
  (declare (list constraint-topics)
	   (type (or integer null) revision))
  (let ((applies-to (get-item-by-psi *applies-to-psi* :revision revision))
	(constraint-role (get-item-by-psi *constraint-role-psi* :revision revision))
	(topictype-role (get-item-by-psi *topictype-role-psi* :revision revision))
	(roletype-role (get-item-by-psi *roletype-role-psi* :revision revision))
	(othertopictype-role (get-item-by-psi *othertopictype-role-psi*
					      :revision revision))
	(otherroletype-role (get-item-by-psi *otherroletype-role-psi*
					     :revision revision))
	(roletype (get-item-by-psi *roletype-psi* :revision revision))
	(roletype-constraint (is-type-constrained :what *roletype-constraint-psi*
						  :revision revision))
	(topictype (get-item-by-psi *topictype-psi* :revision revision))
	(topictype-constraint (is-type-constrained :revision revision)))
    (let ((otherrole-constraints
	   (loop for constraint-topic in constraint-topics
	      append (let ((players nil)
			   (roletypes nil)
			   (otherplayers nil)
			   (otherroletypes nil)
			   (constraint-list
			    (get-constraint-topic-values constraint-topic
							 :revision revision)))
		       (loop for role in (player-in-roles constraint-topic
							  :revision revision)
			  when (and (eq constraint-role
					(instance-of role :revision revision))
				    (eq applies-to (instance-of
						    (parent role :revision revision)
						    :revision revision)))
			  do (loop for other-role in (roles
						      (parent role :revision revision)
						      :revision revision)
				do (let ((current-player
					  (player other-role :revision revision))
					 (current-role
					  (instance-of other-role :revision revision)))
				     (cond
				       ((eq topictype-role current-role)
					(push current-player players))
				       ((eq roletype-role current-role)
					(push current-player roletypes))
				       ((eq othertopictype-role current-role)
					(push current-player otherplayers))
				       ((eq otherroletype-role current-role)
					(push current-player otherroletypes))))))
		       (when (and (append
				   players roletypes otherplayers otherroletypes)
				  (or (not players) (not roletypes)
				      (not otherplayers) (not otherroletypes)))
			 (error "otherroletype-constraint ~a is not complete:~%players: ~a~%roletypes: ~a~%otherplayers: ~a~%otherroletypes: ~a~%"
				(uri (first (psis constraint-topic)))
				(map 'list
				     #'(lambda(x)
					 (uri (first (psis x :revision revision))))
				     players)
				(map 'list 
				     #'(lambda(x)
					 (uri (first (psis x :revision revision))))
				     roletypes)
				(map 'list 
				     #'(lambda(x)
					 (uri (first (psis x :revision revision))))
				     otherplayers)
				(map 'list 
				     #'(lambda(x)
					 (uri (first (psis x :revision revision))))
				     otherroletypes)))
		       (let ((cross-product-1
			      (loop for player in players
				 append (loop for roletype in roletypes
					   collect (list :player player
							 :role roletype))))
			     (cross-product-2
			      (loop for otherplayer in otherplayers
				   append (loop for otherroletype in otherroletypes
					     collect
					       (list :otherplayer otherplayer
						     :otherrole otherroletype)))))
			 (let ((cross-product
				(loop for tupple-1 in cross-product-1
				     append
				     (loop for tupple-2 in cross-product-2
					collect
					  (append
					   tupple-1 tupple-2
					   (list :constraint constraint-list))))))
			   cross-product))))))
      (let ((involved-topic-tupples
	     (remove-duplicates
	      (loop for otherrole-constraint in otherrole-constraints
		 collect (let ((player (getf otherrole-constraint :player))
			       (role-type (getf otherrole-constraint :role))
			       (otherplayer (getf otherrole-constraint :otherplayer))
			       (otherrole-type (getf otherrole-constraint :otherrole)))
			   (topictype-p player topictype topictype-constraint
					nil revision)
			   (topictype-p role-type roletype roletype-constraint
					nil revision)
			   (topictype-p otherplayer topictype topictype-constraint
					nil revision)
			   (topictype-p otherrole-type roletype roletype-constraint
					nil revision)
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
			   when (and (eq (getf otherrole-constraint :player)
					 (getf involved-topic-tupple :player))
				     (eq (getf otherrole-constraint :role)
					 (getf involved-topic-tupple :role))
				     (eq (getf otherrole-constraint :otherplayer)
					 (getf involved-topic-tupple :otherplayer))
				     (eq (getf otherrole-constraint :otherrole)
					 (getf involved-topic-tupple :otherrole)))
			   collect (getf otherrole-constraint :constraint)))))
		  (when (> (length constraint-lists) 1)
		    (error "found contrary otherrole-constraints:~%player: ~a~%role: ~a~%otherplayer: ~a~%otherrole: ~a~% ~a~%"
			   (uri (first (psis (getf involved-topic-tupple :player)
					     :revision revision)))
			   (uri (first (psis (getf involved-topic-tupple :role)
					     :revision revision)))
			   (uri (first (psis (getf involved-topic-tupple :otherplayer)
					     :revision revision)))
			   (uri (first (psis (getf involved-topic-tupple :otherrole)
					     :revision revision)))
			   constraint-lists))

		  (let ((json-player-type
			 (concatenate
			  'string "\"playerType\":"
			  (topics-to-json-list
			   (getf (list-subtypes (getf involved-topic-tupple :player)
						nil nil nil nil revision)
				 :subtypes) :revision revision)))
			(json-player
			 (concatenate
			  'string "\"players\":"
			  (topics-to-json-list
			   (list-instances (getf involved-topic-tupple :player)
					   topictype topictype-constraint revision)
			   :revision revision)))
			(json-role
			 (concatenate
			  'string "\"roleType\":"
			  (topics-to-json-list
			   (getf (list-subtypes (getf involved-topic-tupple :role)
						roletype roletype-constraint nil
						nil revision)
				 :subtypes) :revision revision)))
			(json-otherplayer-type
			 (concatenate
			  'string "\"otherPlayerType\":"
			  (topics-to-json-list
			   (getf (list-subtypes
				  (getf involved-topic-tupple :otherplayer)
				  nil nil nil nil revision) :subtypes)
			   :revision revision)))
			(json-otherplayer
			 (concatenate
			  'string "\"otherPlayers\":"
			  (topics-to-json-list
			   (list-instances (getf involved-topic-tupple :otherplayer)
					   topictype topictype-constraint revision)
			   :revision revision)))
			(json-otherrole
			 (concatenate
			  'string "\"otherRoleType\":"
			  (topics-to-json-list
			   (getf (list-subtypes
				  (getf involved-topic-tupple :otherrole)
				  roletype roletype-constraint nil nil revision)
				 :subtypes) :revision revision)))
			(card-min
			 (concatenate 'string "\"cardMin\":"
				      (getf (first constraint-lists) :card-min)))
			(card-max
			 (concatenate 'string "\"cardMax\":"
				      (getf (first constraint-lists) :card-max))))
		    (setf cleaned-otherrole-constraints
			  (concatenate 'string cleaned-otherrole-constraints
				       "{" json-player-type "," json-player ","
				       json-role "," json-otherplayer-type ","
				       json-otherplayer "," json-otherrole ","
				       card-min "," card-max "},")))))
	  (if (string= cleaned-otherrole-constraints "[")
	      (setf cleaned-otherrole-constraints "null")
	      (setf cleaned-otherrole-constraints 
		    (concatenate
		     'string (subseq cleaned-otherrole-constraints 0
				     (- (length cleaned-otherrole-constraints) 1))
		     "]")))
	  cleaned-otherrole-constraints)))))


(defun get-roleplayer-constraints (constraint-topics &key (revision *TM-REVISION*))
  "Returns a list of the form
   ((:role <topic> :player <topic> :card-min <string> :card-max <string>) <...>)
   which describes the cardinality of topctypes used as players in roles of given
   types in an association of a given type which is also the parent if this list."
  (declare (type (or integer null) revision)
	   (list constraint-topics))
  (let ((applies-to (get-item-by-psi *applies-to-psi* :revision revision))
	(constraint-role (get-item-by-psi *constraint-role-psi* :revision revision))
	(topictype-role (get-item-by-psI *topictype-role-psi* :revision revision))
	(roletype-role (get-item-by-psi *roletype-role-psi* :revision revision))
	(roletype (get-item-by-psi *roletype-psi* :revision revision))
	(roletype-constraint (is-type-constrained :what *roletype-constraint-psi*
						  :revision revision))
	(topictype (get-item-by-psi *topictype-psi* :revision revision))
	(topictype-constraint (is-type-constrained :revision revision)))
    (let ((roleplayer-constraints
	   (loop for constraint-topic in constraint-topics
	      append (let ((constraint-list
			    (get-constraint-topic-values constraint-topic
							 :revision revision)))
		       (let ((players
			      (loop for role in (player-in-roles constraint-topic
								 :revision revision)
				 when (and (eq constraint-role
					       (instance-of role :revision revision))
					   (eq applies-to
					       (instance-of
						(parent role :revision revision)
						:revision revision)))
				 append (loop for other-role in
					     (roles (parent role :revision revision)
						    :revision revision)
					   when (eq topictype-role
						    (instance-of other-role
								 :revision revision))
					   collect (player other-role
							   :revision revision))))
			     (roles
			      (loop for role in (player-in-roles constraint-topic
								 :revision revision)
				 when (and (eq constraint-role
					       (instance-of role :revision revision))
					   (eq applies-to
					       (instance-of
						(parent role :revision revision)
						:revision revision)))
				 append (loop for other-role in (roles (parent role))
					   when (eq roletype-role
						    (instance-of other-role
								 :revision revision))
					   collect (player other-role)))))
			 (when (or (and players (not roles))
				   (and roles (not players)))
			   (error "roleplayer-constraint ~a is not complete:~%players: ~a~%roles: ~a~%"
				  (uri (first (psis constraint-topic
						    :revision revision)))
				  (map 'list
				       #'(lambda(x)
					   (uri (first (psis x :revision revision))))
				       players)
				  (map 'list
				       #'(lambda(x)
					   (uri (first (psis x :revision revision))))
				       roles)))
			 (let ((cross-product
				(loop for player in players
				   append (loop for role in roles
					     collect
					       (list :player player
						     :role role
						     :constraint constraint-list)))))
			   cross-product))))))
      (let ((role-player-tupples
	     (remove-duplicates
	      (loop for roleplayer-constraint in roleplayer-constraints
		 collect (let ((current-player (getf roleplayer-constraint :player))
			       (current-role (getf roleplayer-constraint :role)))
			   (topictype-p current-player topictype topictype-constraint
					nil revision)
			   (topictype-p current-role roletype roletype-constraint
					nil revision)
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
			   when (and (eq (getf roleplayer-constraint :player)
					 (getf role-player-tupple :player))
				     (eq (getf roleplayer-constraint :role)
					 (getf role-player-tupple :role)))
			   collect (getf roleplayer-constraint :constraint)))))
		  (when (> (length constraint-lists) 1)
		    (error "found contrary roleplayer-constraints:~%role: ~a~%player: ~a~% ~a ~%"
			   (uri (first (psis (getf role-player-tupple :role)
					     :revision revision)))
			   (uri (first (psis (getf role-player-tupple :player)
					     :revision revision)))
			   constraint-lists))
		  (let ((json-player-type
			 (concatenate
			  'string "\"playerType\":"
			  (topics-to-json-list
			   (getf (list-subtypes (getf role-player-tupple :player)
						nil nil nil nil revision) :subtypes)
			   :revision revision)))
			(json-players
			 (concatenate
			  'string "\"players\":"
			  (topics-to-json-list
			   (list-instances (getf role-player-tupple :player)
					   topictype topictype-constraint revision)
			   :revision revision)))
			(json-role
			 (concatenate
			  'string "\"roleType\":"
			  (topics-to-json-list
			   (getf (list-subtypes (getf role-player-tupple :role)
						roletype roletype-constraint nil
						nil revision)
				 :subtypes)
			   :revision revision)))
			(card-min
			 (concatenate
			  'string "\"cardMin\":"
			  (getf (first constraint-lists) :card-min)))
			(card-max
			 (concatenate
			  'string "\"cardMax\":"
			  (getf (first constraint-lists) :card-max))))
		    (setf cleaned-roleplayer-constraints
			  (concatenate 'string cleaned-roleplayer-constraints
				       "{" json-player-type "," json-players ","
				       json-role "," card-min "," card-max "},")))))
	  (if (string= cleaned-roleplayer-constraints "[")
	      (setf cleaned-roleplayer-constraints "null")
	      (setf cleaned-roleplayer-constraints 
		    (concatenate
		     'string (subseq cleaned-roleplayer-constraints 0
				     (- (length cleaned-roleplayer-constraints) 1))
		     "]")))
	  cleaned-roleplayer-constraints)))))


(defun get-associationrole-constraints (constraint-topics &key
					(revision *TM-REVISION*))
  "Returns a list of the form
   ((:associationroletype <topic> :card-min <string> :card-max <string>), <...>)
   which describes all associationrole-constraints of the passed
   constraint-topics.
   If as-json is set to t the return value of this function is a
   json-string otherwise a list of lists of the following form
   (:roletype <topic, topic, ...> :cardMin <min> :cardMax <max>)"
  (declare (type (or integer null) revision)
	   (list constraint-topics))
  (let ((applies-to (get-item-by-psi *applies-to-psi* :revision revision))
	(roletype-role (get-item-by-psi *roletype-role-psi* :revision revision))
	(constraint-role (get-item-by-psi *constraint-role-psi* :revision revision))
	(roletype (get-item-by-psi *roletype-psi* :revision revision))
	(roletype-constraint (is-type-constrained :what *roletype-constraint-psi*
						  :revision revision)))
    (let ((associationrole-constraints
	   (loop for constraint-topic in constraint-topics
	      append (let ((constraint-list
			    (get-constraint-topic-values constraint-topic
							 :revision revision)))
		       (loop for role in (player-in-roles constraint-topic
							  :revision revision)
			  when (and (eq constraint-role 
					(instance-of role :revision revision))
				    (eq applies-to 
					(instance-of (parent role :revision revision)
						     :revision revision)))
			  append (loop for other-role in
				      (roles (parent role :revision revision)
					     :revision revision)
				    when (eq roletype-role
					     (instance-of other-role
							  :revision revision))
				    collect 
				      (list :associationroletype
					    (player other-role :revision revision)
					    :constraint constraint-list)))))))
      (let ((associationroletype-topics
	     (remove-duplicates
	      (map 'list #'(lambda(x)
			     (let ((associationroletype (getf x :associationroletype)))
			       (topictype-p associationroletype roletype
					    roletype-constraint nil revision)
			       associationroletype))
		   associationrole-constraints))))
	(let ((cleaned-associationrole-constraints "["))
	  (loop for associationroletype-topic in associationroletype-topics
	     do
	       (let ((constraint-lists
		      (remove-duplicate-constraints
		       (loop for associationrole-constraint in
			  associationrole-constraints
			  when (eq associationroletype-topic
				   (getf associationrole-constraint
					 :associationroletype))
			  collect (getf associationrole-constraint :constraint)))))
		 (when (> (length constraint-lists) 1)
		   (error "found contrary associationrole-constraints: ~a ~a~%" (uri (first (psis associationroletype-topic :revision revision))) constraint-lists))
		  (let ((roletype-with-subtypes
			 (json:encode-json-to-string 
			  (map 'list #'(lambda(topic)
					 (map 'list #'uri
					      (psis topic :revision revision)))
			       (getf (list-subtypes associationroletype-topic
						    roletype roletype-constraint
						    nil nil revision) :subtypes)))))
		    (setf cleaned-associationrole-constraints
			  (concatenate 'string
				       cleaned-associationrole-constraints
				       "{\"roleType\":" roletype-with-subtypes
				       ",\"cardMin\":" (getf (first constraint-lists)
							     :card-min)
				       ",\"cardMax\":" (getf (first constraint-lists)
							     :card-max) "},")))))
	  (if (string= cleaned-associationrole-constraints "[")
	      (setf cleaned-associationrole-constraints "null")
	      (setf cleaned-associationrole-constraints 
		    (concatenate
		     'string (subseq cleaned-associationrole-constraints 0
				     (- (length cleaned-associationrole-constraints)
					1)) "]")))
	  cleaned-associationrole-constraints)))))


;; =============================================================================
;; --- all topic constraints ---------------------------------------------------
;; =============================================================================
(defun get-constraints-of-topic (topic-instances &key(treat-as 'type)
				 (revision *TM-REVISION*))
  "Returns a constraint list with the constraints:
   subjectidentifier-constraints, subjectlocator-constraints,
   topicname-constraints, topicoccurrence-constraints and
   uniqueoccurrence-constraints.
   topic-instances should be a list with exactly one item if trea-as is set to type
   otherwise it can constain more items."
  (declare (list topic-instances)
	   (symbol treat-as)
	   (type (or integer null) revision))
  (when (and (> (length topic-instances) 1)
	     (not (eql treat-as 'type)))
    (error "From get-constraints-of-topic: topic-instances must contain exactly one item when treated as instance!"))
  (let ((abstract-topictype-constraints nil)
	(exclusive-instance-constraints nil)
	(subjectidentifier-constraints nil)
	(subjectlocator-constraints nil)
	(topicname-constraints nil)
	(topicoccurrence-constraints nil)
	(uniqueoccurrence-constraints nil))
    (loop for topic-instance in topic-instances
       do (let ((current-constraints
		 (get-all-constraint-topics-of-topic topic-instance
						     :treat-as treat-as
						     :revision revision)))
	    (dolist (item (getf current-constraints :abstract-topictype-constraints))
	      (pushnew item abstract-topictype-constraints))
	    (dolist (item (getf current-constraints :exclusive-instance-constraints))
	      (let ((current-list
		     (list topic-instance (list item))))
		(let ((found-item
		       (find current-list exclusive-instance-constraints
			     :key #'first)))
		  (if found-item
		      (dolist (inner-item (second current-list))
			(pushnew inner-item (second found-item)))
		      (push current-list exclusive-instance-constraints)))))
	    (dolist (item (getf current-constraints :subjectidentifier-constraints))
	      (pushnew item subjectidentifier-constraints))
	    (dolist (item (getf current-constraints :subjectlocator-constraints))
	      (pushnew item subjectlocator-constraints))
	    (dolist (item (getf current-constraints :topicname-constraints))
	      (pushnew item topicname-constraints))
	    (dolist (item (getf current-constraints :topicoccurrence-constraints))
	      (pushnew item topicoccurrence-constraints))
	    (dolist (item (getf current-constraints :uniqueoccurrence-constraints))
	      (pushnew item uniqueoccurrence-constraints))))
    (let ((exclusive-instance-constraints
	   (let ((value "["))
	     (loop for exclusive-instance-constraint in exclusive-instance-constraints
		do (setf value
			 (concatenate 'string value
				      (get-exclusive-instance-constraints
				       (first exclusive-instance-constraint)
				       (second exclusive-instance-constraint)
				       :revision revision) ",")))
	     (if (string= value "[")
		 (setf value "null")
		 (setf value (concatenate 'string (subseq value 0
							  (- (length value) 1)) "]")))
	     (concatenate 'string "\"exclusiveInstances\":" value)))
	  (subjectidentifier-constraints
	   (let ((value
		  (get-simple-constraints
		   subjectidentifier-constraints
		   :error-msg-constraint-name "subjectidentifier"
		   :revision revision)))
	     (concatenate 'string "\"subjectIdentifierConstraints\":" value)))
	  (subjectlocator-constraints
	   (let ((value
		  (get-simple-constraints
		   subjectlocator-constraints
		   :error-msg-constraint-name "subjectlocator"
		   :revision revision)))
	     (concatenate 'string "\"subjectLocatorConstraints\":" value)))
	  (topicname-constraints
	   (let ((value
		  (get-topicname-constraints topicname-constraints
					     :revision revision)))
	     (concatenate 'string "\"topicNameConstraints\":" value)))
	  (topicoccurrence-constraints
	   (let ((value
		  (get-topicoccurrence-constraints topicoccurrence-constraints
						   uniqueoccurrence-constraints
						   :revision revision)))
	     (concatenate 'string "\"topicOccurrenceConstraints\":" value)))
	  (abstract-constraint
	   (concatenate 'string "\"abstractConstraint\":"
			(if abstract-topictype-constraints
			    "true"
			    "false"))))
      (let ((json-string
	     (concatenate 'string "{" exclusive-instance-constraints ","
			  subjectidentifier-constraints
			  "," subjectlocator-constraints "," topicname-constraints ","
			  topicoccurrence-constraints "," abstract-constraint "}")))
        json-string))))


(defun get-exclusive-instance-constraints(owner exclusive-instances-lists
					  &key (revision *TM-REVISION*))
  "Returns a JSON-obejct of the following form:
   {owner: [psi-1, psi-2], exclusives: [[psi-1-1, psi-1-2], [psi-2-1, <...>], <...>]}."
  (declare (type (or integer null) revision))
  (let ((constraint-role (get-item-by-psi *constraint-role-psi* :revision revision))
	(applies-to (get-item-by-psi *applies-to-psi* :revision revision))
	(topictype-role (get-item-by-psi *topictype-role-psi* :revision revision))
	(topictype (get-item-by-psi *topictype-psi* :revision revision))
	(topictype-constraint (is-type-constrained :revision revision)))
    (let ((topics
	   (remove-duplicates
	    (loop for exclusive-instances-list in exclusive-instances-lists
	       append
		 (let ((owner (getf exclusive-instances-list :owner))
		       (exclusive-constraints
			(getf exclusive-instances-list :exclusive-constraints)))
		   (loop for exclusive-constraint in exclusive-constraints
		      append
			(loop for role in
			     (player-in-roles exclusive-constraint
					      :revision revision)
			   when (and (eq constraint-role
					 (instance-of role
						      :revision revision))
				     (eq applies-to (instance-of
						     (parent role :revision revision)
						     :revision revision)))
			   append
			     (loop for other-role in
				  (roles
				   (parent role :revision revision)
				   :revision revision)
				when (and (eq topictype-role
					      (instance-of other-role
							   :revision revision))
					  (not
					   (eq owner (player other-role
							     :revision revision))))
				append
				  (getf
				   (list-subtypes
				    (player other-role :revision revision)
				    topictype topictype-constraint nil
				    nil revision) :subtypes)))))))))
      (concatenate 'string "{\"owner\":" (json-exporter::identifiers-to-json-string
					  owner :revision revision)
		   ",\"exclusives\":"
		   (json:encode-json-to-string
		    (map 'list #'(lambda(y)
				   (map 'list #'uri y))
			 (map 'list #'(lambda(z)
					(psis z :revision revision))
			      topics))) "}"))))


(defun get-simple-constraints(constraint-topics &key
			      (error-msg-constraint-name "uniqueoccurrence")
			      (revision *TM-REVISION*))
  "Returns a list of the form
   ((:regexp <string> :card-min <string> :card-max <string>))
   which contains the subjectidentifier, subjectlocator or
   unique-occurrence constraints. This depends on the passed
   constraint-topics."
  (declare (list constraint-topics)
	   (string error-msg-constraint-name)
	   (type (or integer null) revision))
  (let ((all-values
	 (remove-duplicate-constraints
	  (loop for constraint-topic in constraint-topics
	     collect (get-constraint-topic-values constraint-topic
						  :revision revision)))))
    (let ((contrary-constraints (find-contrary-constraints all-values)))
      (when contrary-constraints
	(error "found contrary ~a-constraints: ~a~%"
	       error-msg-constraint-name contrary-constraints)))
    (simple-constraints-to-json all-values)))


(defun simple-constraints-to-json(simple-constraints)
  "Transforms a list of simple constraint lists of the form
   ((:regexp <string> :card-min <string> :card-max <string>) <...>)
   to a valid json list of the form
   [{regexp: expr, cardMin: 123, cardMax: 456}, <...>]."
  (let ((constraints "["))
    (loop for constraint in simple-constraints
       do (let ((constraint
		 (concatenate
		  'string "{\"regexp\":"
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


(defun get-topicname-constraints(constraint-topics &key (revision *TM-REVISION*))
  "Returns all topicname constraints as a list of the following form:
  [{nametypescopes:[{nameType: [psi-1, psi-2], scopeConstraints: [<scopeConstraint>]},
                    {nameType: [subtype-1-psi-1], scopeConstraints: [<scopeConstraints>]},
    constraints: [<simpleConstraint>, <...>]},
    <...>]."
  (declare (type (or integer null) revision)
	   (list constraint-topics))
  (let ((constraint-role (get-item-by-psi *constraint-role-psi* :revision revision))
	(applies-to (get-item-by-psi *applies-to-psi* :revision revision))
	(nametype-role (get-item-by-psi *nametype-role-psi* :revision revision))
	(nametype (get-item-by-psi *nametype-psi* :revision revision))
	(nametype-constraint (is-type-constrained :what *nametype-constraint-psi*
						  :revision revision)))
    (let ((topicname-constraints
	   (remove-if
	    #'null
	    (loop for constraint-topic in constraint-topics
	       append
		 (loop for role in (player-in-roles constraint-topic
						    :revision revision)
		    when (and (eq constraint-role
				  (instance-of role :revision revision))
			      (eq applies-to
				  (instance-of (parent role :revision revision)
					       :revision revision)))
		    append
		      (loop for other-role in
			   (roles (parent role :revision revision)
				  :revision revision)
			 when (eq nametype-role 
				  (instance-of other-role :revision revision))
			 collect
			   (let ((nametype-topic
				  (player other-role :revision revision))
				 (constraint-list
				  (get-constraint-topic-values constraint-topic
							       :revision revision)))
			     (list :type nametype-topic
				   :constraint constraint-list))))))))
      (let ((nametype-topics
	     (remove-duplicates
	      (map 'list #'(lambda(x)
			     (let ((topicname-type
				    (getf x :type)))
			       (topictype-p topicname-type nametype
					    nametype-constraint nil revision)
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
		      (error "found contrary topicname-constraints: ~a~%"
			     contrary-constraints)))
		  (let ((nametype-with-subtypes
			 (remove-if
			  #'null
			  (getf (list-subtypes nametype-topic nametype
					       nametype-constraint nil nil revision)
				:subtypes))))
		    (let ((nametypescopes "\"nametypescopes\":["))
		      (loop for current-topic in nametype-with-subtypes
			 do (let ((current-json-string
				   (concatenate
				    'string "{\"nameType\":"
				    (json-exporter::identifiers-to-json-string
				     current-topic :revision revision)
				    ",\"scopeConstraints\":"
				    (get-typescope-constraints current-topic
							       :what 'topicname
							       :revision revision)
				    "}")))
			      (setf nametypescopes
				    (concatenate 'string nametypescopes
						 current-json-string ","))))
		      (if (string= nametypescopes "\"nametypescopes\"[")
			  (setf nametypescopes "null")
			  (setf nametypescopes
				(concatenate 
				 'string (subseq nametypescopes 0
						 (- (length nametypescopes) 1)) "]")))
		      (let ((json-constraint-lists
			     (concatenate
			      'string "\"constraints\":"
			      (simple-constraints-to-json constraint-lists))))
			(setf cleaned-topicname-constraints
			      (concatenate
			       'string cleaned-topicname-constraints "{"
			       nametypescopes "," json-constraint-lists "},")))))))
	  (if (string= cleaned-topicname-constraints "[")
	      (setf cleaned-topicname-constraints "null")
	      (setf cleaned-topicname-constraints
		    (concatenate
		     'string (subseq cleaned-topicname-constraints 0
				     (- (length cleaned-topicname-constraints) 1))
		     "]")))
	  cleaned-topicname-constraints)))))


(defun get-topicoccurrence-constraints(constraint-topics unique-constraint-topics
				       &key (revision *TM-REVISION*))
  "Returns all topicoccurrence constraints as a list of the following form:
   [{occurrenceTypes:[{occurrenceType:[psi-1,psi-2],
		       scopeConstraints:[<scopeConstraints>],
		       datatypeConstraint:datatype},
                       <...>],
   constraints:[<simpleConstraints>, <...>],
   uniqueConstraint:[<uniqueConstraints>, <...> ]}
   <...>]."
  (declare (type (or integer null) revision)
	   (list constraint-topics unique-constraint-topics))
  (let ((constraint-role (get-item-by-psi *constraint-role-psi* :revision revision))
	(applies-to (get-item-by-psi *applies-to-psi* :revision revision))
	(occurrencetype-role (get-item-by-psi *occurrencetype-role-psi*
					      :revision revision))
	(occurrencetype (get-item-by-psi *occurrencetype-psi*
					 :revision revision))
	(occurrencetype-constraint
	 (is-type-constrained :what *occurrencetype-constraint-psi*
			      :revision revision)))
    (let ((topicoccurrence-constraints
	   (remove-if
	    #'null
	    (loop for constraint-topic in constraint-topics
	       append
		 (loop for role in (player-in-roles constraint-topic
						    :revision revision)
		    when (and (eq constraint-role
				  (instance-of role :revision revision))
			      (eq applies-to 
				  (instance-of (parent role :revision revision)
					       :revision revision)))
		    append
		      (loop for other-role in (roles (parent role :revision revision)
						     :revision revision)
			 when (eq occurrencetype-role
				  (instance-of other-role :revision revision))
			 collect
			   (let ((occurrencetype-topic
				  (player other-role :revision revision))
				 (constraint-list
				  (get-constraint-topic-values constraint-topic
							       :revision revision)))
			     (list :type occurrencetype-topic
				   :constraint constraint-list))))))))
      (let ((occurrencetype-topics
	     (remove-duplicates
	      (map 'list
		   #'(lambda(x)
		       (let ((occurrence-type (getf x :type)))
			 (topictype-p occurrence-type occurrencetype
				      occurrencetype-constraint nil revision)
			 occurrence-type))
		   topicoccurrence-constraints))))
	(let ((cleaned-topicoccurrence-constraints "["))
	  (loop for occurrencetype-topic in occurrencetype-topics
	     do (let ((constraint-lists
		       (remove-duplicate-constraints
			(loop for topicoccurrence-constraint in
			   topicoccurrence-constraints
			   when (eq occurrencetype-topic
				    (getf topicoccurrence-constraint :type))
			   collect (getf topicoccurrence-constraint :constraint)))))
		  (let ((contrary-constraints
			 (find-contrary-constraints constraint-lists)))
		    (when contrary-constraints
		      (error "found contrary topicname-constraints: ~a~%"
			     contrary-constraints)))
		  (let ((occurrencetype-with-subtypes
			 (getf
			  (list-subtypes occurrencetype-topic
					 occurrencetype occurrencetype-constraint
					 nil nil revision) :subtypes)))
		    (let ((occurrencetypes-json-string "\"occurrenceTypes\":["))
		      (loop for current-topic in occurrencetype-with-subtypes
			 do (let ((current-json-string
				   (concatenate
				    'string "{\"occurrenceType\":"
				    (json-exporter::identifiers-to-json-string
				     current-topic :revision revision)
				    ",\"scopeConstraints\":"
				    (get-typescope-constraints
				     current-topic :what 'topicoccurrence
				     :revision revision)
				    ",\"datatypeConstraint\":"
				    (get-occurrence-datatype-constraint
				     current-topic :revision revision)
				    "}")))
			      (setf occurrencetypes-json-string
				    (concatenate 'string occurrencetypes-json-string
						 current-json-string ","))))
		      (if (string= occurrencetypes-json-string "\"occurrenceTypes\"[")
			  (setf occurrencetypes-json-string "null")
			  (setf occurrencetypes-json-string
				(concatenate
				 'string (subseq occurrencetypes-json-string 0
						 (- (length
						     occurrencetypes-json-string) 1))
				 "]")))
		      (let ((unique-constraints
			     (concatenate 'string "\"uniqueConstraints\":"
					  (get-simple-constraints
					   unique-constraint-topics
					   :revision revision)))
			    (json-constraint-lists
			     (concatenate
			      'string "\"constraints\":" 
			      (simple-constraints-to-json constraint-lists))))
			(let ((current-json-string
			       (concatenate
				'string "{" occurrencetypes-json-string ","
				json-constraint-lists "," unique-constraints "}")))
			  (setf cleaned-topicoccurrence-constraints
				(concatenate
				 'string cleaned-topicoccurrence-constraints
				 current-json-string ","))))))))
	  (if (string= cleaned-topicoccurrence-constraints "[")
	      (setf cleaned-topicoccurrence-constraints "null")
	      (setf cleaned-topicoccurrence-constraints
		    (concatenate
		     'string
		     (subseq
		      cleaned-topicoccurrence-constraints 0
		      (- (length cleaned-topicoccurrence-constraints) 1)) "]")))
	  cleaned-topicoccurrence-constraints)))))


(defun get-occurrence-datatype-constraint(occurrencetype-topic
					  &key (revision *TM-REVISION*))
  "Return a datatype qualifier as a string."
  (declare (TopicC occurrencetype-topic)
	   (type (or integer null) revision))
  (let ((constraint-role (get-item-by-psi *constraint-role-psi* :revision revision))
	(applies-to (get-item-by-psi *applies-to-psi* :revision revision))
	(occurrencetype-role (get-item-by-psi *occurrencetype-role-psi*
					      :revision revision))
	(datatype (get-item-by-psi *datatype-psi* :revision revision))
	(occurrencedatatype-constraint
	 (get-item-by-psi *occurrencedatatype-constraint-psi*
			  :revision revision))
	(topictype (get-item-by-psi *topictype-psi* :revision revision))
	(topictype-constraint (get-item-by-psi *topictype-constraint-psi*
					       :revision revision)))
    (let ((datatype-constraints
	   (remove-duplicates
	    (loop for role in (player-in-roles occurrencetype-topic :revision revision)
	       when (and (eq occurrencetype-role (instance-of role :revision revision))
			 (eq applies-to (instance-of (parent role :revision revision)
						     :revision revision)))
	       append (loop for other-role in (roles (parent role :revision revision)
						     :revision revision)
			 when (and (eq constraint-role
				       (instance-of other-role :revision revision))
				   (topictype-of-p
				    (player other-role :revision revision)
				    occurrencedatatype-constraint topictype
				    topictype-constraint nil revision))
			 collect (player other-role :revision revision))))))
      (let ((datatype-constraint
	     (remove-duplicates
	      (map
	       'list
	       #'(lambda(constraint-topic)
		   (loop for occurrence in
			(occurrences constraint-topic :revision revision)
		      when (and (eq (instance-of occurrence :revision revision)
				    datatype)
				(slot-boundp occurrence 'charvalue))
		      return (charvalue occurrence)))
	       datatype-constraints))))
	(when (> (length datatype-constraint) 1)
	  (error "found contrary occurrence-datatype-constraints: ~a~%"
		 datatype-constraints))
	(if datatype-constraint
	    (json:encode-json-to-string (first datatype-constraint))
	    "null")))))


(defun get-typescope-constraints(element-type-topic &key (what 'topicname)
				 (revision *TM-REVISION*))
  "Returns a list of scopes for the element-typetopic which is the type topic of
   a topicname, a topicoccurrence or an association. To specifiy of what kind
   of element the scopes should be there is the key-variable what.
   It can be set to 'topicname, 'topicoccurrence or 'association.
   The return value is of the form:
   [{scopeTypes:[[[psi-1-1, psi-1-2], [subtype-1-psi-1, subtype-1-psi-2]], [[psi-2-1],
   [subtype-1-psi-1], [subtype-2-psi-1]]], cardMin: <int-as-string>,
   cardMax <int-as-string | MAX_INT>}, <...>]."
  (declare (TopicC element-type-topic)
	   (symbol what)
	   (type (or integer null) revision))
  (let ((element-type-role-and-scope-constraint
	 (cond
	   ((eq what 'topicname)
	    (list (get-item-by-psi *nametype-role-psi* :revision revision)
		  (get-item-by-psi *nametypescope-constraint-psi*
				   :revision revision)))
	   ((eq what 'topicoccurrence)
	    (list
	     (get-item-by-psi *occurrencetype-role-psi* :revision revision)
	     (get-item-by-psi *occurrencetypescope-constraint-psi*
			      :revision revision)))
	   ((eq what 'association)
	    (list
	     (get-item-by-psi *associationtype-role-psi* :revision revision)
	     (get-item-by-psi *associationtypescope-constraint-psi*
			      :revision revision)))))
	(scopetype-role (get-item-by-psi *scopetype-role-psi* :revision revision))
	(constraint-role (get-item-by-psi *constraint-role-psi* :revision revision))
	(applies-to (get-item-by-psi *applies-to-psi* :revision revision))
	(scopetype (get-item-by-psi *scopetype-psi* :revision revision))
	(topictype (get-item-by-psi *topictype-psi* :revision revision))
	(topictype-constraint (get-item-by-psi *topictype-constraint-psi*
					       :revision revision)))
    (when (and (= (length element-type-role-and-scope-constraint) 2)
	       (first element-type-role-and-scope-constraint)
	       (second element-type-role-and-scope-constraint))
      (let ((type-role (first element-type-role-and-scope-constraint))
	    (typescope-constraint (second element-type-role-and-scope-constraint)))
	(let ((typescope-constraints
	       (loop for role in
		    (player-in-roles element-type-topic :revision revision)
		  when (and (eq type-role (instance-of role :revision revision))
			    (eq applies-to
				(instance-of (parent role :revision revision)
					     :revision revision)))
		  append
		    (loop for other-role in
			 (roles (parent role :revision revision)
				:revision revision)
		       when (and (eq constraint-role
				     (instance-of other-role :revision revision))
				 (topictype-of-p
				  (player other-role :revision revision)
				  typescope-constraint topictype
				  topictype-constraint nil revision))
		       collect
			 (let ((scopes nil)
			       (constraint nil))
			   (loop for c-role in
				(player-in-roles
				 (player other-role :revision revision)
				 :revision revision)
			      when (and (eq constraint-role
					    (instance-of c-role :revision revision))
					(eq applies-to
					    (instance-of
					     (parent c-role :revision revision)
					     :revision revision)))
			      do (progn
				   (setf constraint
					 (get-constraint-topic-values
					  (player c-role :revision revision)
					  :revision revision))
				   (loop for c-other-role in
					(roles (parent c-role :revision revision)
					       :revision revision)
				      when (eq scopetype-role
					       (instance-of c-other-role
							    :revision revision))
				      do (push
					  (player c-other-role :revision revision)
					  scopes))))
			   (list :scopes scopes :constraint constraint))))))
	  (let ((scopetype-groups
		 (remove-duplicates
		  (map 'list #'(lambda(x)
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
			       when
				 (and (= (length (getf typescope-constraint :scopes))
					 (length scopetype-group))
				      (= (length (getf typescope-constraint :scopes))
					 (length (intersection
						  (getf typescope-constraint :scopes)
						  scopetype-group))))
			       collect (getf typescope-constraint :constraint)))))
		      (when (> (length constraint-lists) 1)
			(error "found contrary scopetype-constraints for ~a: ~a~%"
			       (map 'list
				    #'(lambda(x)
					(uri (first (psis x :revision revision))))
				    scopetype-group)
			       constraint-lists))
		      (let ((card-min (getf (first constraint-lists) :card-min))
			    (card-max (getf (first constraint-lists) :card-max)))
			(let ((json-scopes
			       (concatenate
				'string "\"scopeTypes\":"
				(let ((scopetypes-with-subtypes
				       (remove-if
					#'null
					(loop for current-scopetype in scopetype-group
					   collect (getf
						    (list-subtypes current-scopetype
								   scopetype nil nil
								   nil revision)
						    :subtypes)))))
				  (json:encode-json-to-string
				   (map
				    'list
				    #'(lambda(topic-group)
					(map 'list
					     #'(lambda(topic)
						 (map 'list #'uri
						      (psis topic :revision revision)))
					     topic-group))
				    scopetypes-with-subtypes))))))
			  (let ((current-json-string
				 (concatenate 'string "{" json-scopes
					      ",\"cardMin\":\"" card-min
					      "\",\"cardMax\":\"" card-max "\"}")))
			    (setf cleaned-typescope-constraints 
				  (concatenate 'string cleaned-typescope-constraints
					       current-json-string ",")))))))
	      (if (string= cleaned-typescope-constraints "[")
		  (setf cleaned-typescope-constraints "null")
		  (setf cleaned-typescope-constraints 
			(concatenate
			 'string
			 (subseq cleaned-typescope-constraints 0
				 (- (length cleaned-typescope-constraints) 1)) "]")))
	      cleaned-typescope-constraints)))))))
    

;; =============================================================================
;; --- some basic helpers ------------------------------------------------------
;; =============================================================================
(defun get-constraint-topic-values(topic &key (revision *TM-REVISION*))
  "Returns all constraint values of the passed topic in the
   following form (list :regexp regexp :card-min card-min :card-max card-max)"
  (declare (type (or integer null) revision))
  (let ((regexp
	 (get-constraint-occurrence-value topic :revision revision))
	(card-min
	 (get-constraint-occurrence-value topic :what 'card-min :revision revision))
	(card-max
	 (get-constraint-occurrence-value topic :what 'card-max :revision revision)))
    (when (and (string/= "MAX_INT" card-max)
	       (> (parse-integer card-min) (parse-integer card-max)))
      (error "card-min (~a) must be < card-max (~a)" card-min card-max))
    (list :regexp regexp :card-min card-min :card-max card-max)))


(defun get-constraint-occurrence-value(topic &key (what 'regexp)
				       (revision *TM-REVISION*))
  "Checks the occurrence-value of a regexp, card-min or card-max
   constraint-occurrence.
   If what = 'regexp and the occurrence-value is empty there will be returned
   the value '.*!'.
   If what = 'card-min and the occurrence-value is empty there will be returned
   the value '0'.
   If what = 'card-max and the occurrence-value is empty there will be returned
   the value 'MAX_INT'"
  (declare (type (or integer null) revision)
	   (TopicC topic)
	   (symbol what))
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
	     ""))
	  :revision revision)))
    (when occurrence-type
      (let ((occurrence-value
	     (let ((occurrence
		    (find occurrence-type (occurrences topic :revision revision)
			  :key #'(lambda(occ)
				   (instance-of occ :revision revision)))))
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
		      (uri (first (psis topic :revision revision)))
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
	    (setf current-constraint
		  (remove-if
		   #'null
		   (map 'list
			#'(lambda(x)
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


;; --- gets all constraint topics ----------------------------------------------
(defun get-direct-constraint-topics-of-topic (topic-instance &key
					      (revision *TM-REVISION*))
  "Returns all constraint topics defined for the passed topic-instance"
  (declare (type (or integer null) revision)
	   (TopicC topic-instance))
  (let ((constraint-role (get-item-by-psi *constraint-role-psi* :revision revision))
	(topictype-role (get-item-by-psi *topictype-role-psi* :revision revision))
	(applies-to (get-item-by-psi *applies-to-psi* :revision revision))
	(abstract-topictype-constraint
	 (get-item-by-psi *abstract-topictype-constraint-psi* :revision revision))
	(exclusive-instance-constraint
	 (get-item-by-psi *exclusive-instance-psi* :revision revision))
	(subjectidentifier-constraint
	 (get-item-by-psi *subjectidentifier-constraint-psi* :revision revision))
	(subjectlocator-constraint
	 (get-item-by-psi *subjectlocator-constraint-psi* :revision revision))
	(topicname-constraint
	 (get-item-by-psi *topicname-constraint-psi* :revision revision))
	(topicoccurrence-constraint
	 (get-item-by-psi *topicoccurrence-constraint-psi* :revision revision))
	(uniqueoccurrence-constraint
	 (get-item-by-psi *uniqueoccurrence-constraint-psi* :revision revision))
	(roleplayer-constraint
	 (get-item-by-psi *roleplayer-constraint-psi* :revision revision))
	(otherrole-constraint
	 (get-item-by-psi *otherrole-constraint-psi* :revision revision))
	(topictype (get-item-by-psi *topictype-psi* :revision revision))
	(topictype-constraint (get-item-by-psi *topictype-constraint-psi*
					       :revision revision))
	(abstract-topictype-constraints nil)
	(exclusive-instance-constraints nil)
	(subjectidentifier-constraints nil)
	(subjectlocator-constraints nil)
	(topicname-constraints nil)
	(topicoccurrence-constraints nil)
	(uniqueoccurrence-constraints nil))
    (loop for role in (player-in-roles topic-instance :revision revision)
       when (and (eq topictype-role (instance-of role :revision revision))
		 (eq applies-to (instance-of (parent role :revision revision)
					     :revision revision)))
       do (loop for other-role in (roles (parent role :revision revision)
					 :revision revision)
	     when (eq constraint-role (instance-of other-role :revision revision))
	     do (let ((constraint-topic (player other-role :revision revision)))
		  (cond
		    ((topictype-of-p constraint-topic abstract-topictype-constraint
				     topictype topictype-constraint nil revision)
		     (pushnew constraint-topic abstract-topictype-constraints))
		    ((topictype-of-p constraint-topic exclusive-instance-constraint
				     topictype topictype-constraint nil revision)
		     (pushnew constraint-topic exclusive-instance-constraints))
		    ((topictype-of-p constraint-topic subjectidentifier-constraint
				     topictype topictype-constraint nil revision)
		     (pushnew constraint-topic subjectidentifier-constraints))
		    ((topictype-of-p constraint-topic subjectlocator-constraint
				     topictype topictype-constraint nil revision)
		     (pushnew constraint-topic subjectlocator-constraints))
		    ((topictype-of-p constraint-topic topicname-constraint
				     topictype topictype-constraint nil revision)
		     (pushnew constraint-topic topicname-constraints))
		    ((topictype-of-p constraint-topic topicoccurrence-constraint
				     topictype topictype-constraint nil revision)
		     (pushnew constraint-topic topicoccurrence-constraints))
		    ((topictype-of-p constraint-topic uniqueoccurrence-constraint
				     topictype topictype-constraint nil revision)
		     (pushnew constraint-topic uniqueoccurrence-constraints))
		    (t
		     (unless (or
			      (topictype-of-p constraint-topic roleplayer-constraint
					      topictype topictype-constraint
					      nil revision)
			      (topictype-of-p constraint-topic otherrole-constraint
					      topictype topictype-constraint
					      nil revision))
		       (error "Constraint-Topic \"~a\" could not be handled"
			      (uri (first (psis constraint-topic
						:revision revision))))))))))
    (list :abstract-topictype-constraints abstract-topictype-constraints
	  :exclusive-instance-constraints
	  (list :exclusive-constraints exclusive-instance-constraints
		:owner topic-instance)
	  :subjectidentifier-constraints subjectidentifier-constraints
	  :subjectlocator-constraints subjectlocator-constraints
	  :topicname-constraints topicname-constraints
	  :topicoccurrence-constraints topicoccurrence-constraints
	  :uniqueoccurrence-constraints uniqueoccurrence-constraints)))


(defun get-all-constraint-topics-of-topic (topic-instance &key (treat-as 'type)
					   (revision *TM-REVISION*))
  "Returns a list of constraint-topics of the topics-instance's base type(s).
   If topic c is instanceOf a and b, there will be returned all
   constraint-topics of the topic types a and b.
   If treat-as is set to instance there will be only the constraints collected
   defined for the supertypes or the types of the passed topic - all constraints
   defined directly for the passed topic are ignored, unless the passed topic is
   an instance of itself."
  (declare (type (or integer null) revision)
	   (TopicC topic-instance)
	   (symbol treat-as))
  (let ((topictype (get-item-by-psi *topictype-psi* :revision revision))
	(topictype-constraint (get-item-by-psi *topictype-constraint-psi*
					       :revision revision)))
    (let ((akos-and-isas-of-this
	   (remove-duplicates
	    (if (eql treat-as 'type)
		(progn
		  (topictype-p topic-instance topictype topictype-constraint
			       nil revision)
		  (get-all-upper-constrainted-topics topic-instance
						     :revision revision))
		(progn
		  (valid-instance-p topic-instance nil nil revision)
		  (let ((topictypes
			 (get-direct-types-of-topic topic-instance
						    :revision revision))
			(all-constraints nil))
		    (dolist (tt topictypes)
		      (let ((upts
			     (get-all-upper-constrainted-topics tt
								:revision revision)))
			(dolist (upt upts)
			  (pushnew upt all-constraints))))
		    (remove-if #'(lambda(x)
				   (when (eql x topic-instance)
				     t))
			       all-constraints)))))))
      (let ((all-abstract-topictype-constraints nil)
	    (all-exclusive-instance-constraints nil)
	    (all-subjectidentifier-constraints nil)
	    (all-subjectlocator-constraints nil)
	    (all-topicname-constraints nil)
	    (all-topicoccurrence-constraints nil)
	    (all-uniqueoccurrence-constraints nil))
	(loop for topic in akos-and-isas-of-this
	   do (let ((constraint-topics-of-topic
		     (get-direct-constraint-topics-of-topic topic
							    :revision revision)))
		(when (eq topic topic-instance)
		  (dolist (item (getf constraint-topics-of-topic
				      :abstract-topictype-constraints))
		    (pushnew item all-abstract-topictype-constraints)))
		(let ((exclusive-instance-constraints
		       (getf constraint-topics-of-topic
			     :exclusive-instance-constraints)))
		  (when (getf exclusive-instance-constraints :exclusive-constraints)
		    (push exclusive-instance-constraints
			  all-exclusive-instance-constraints)))
		(dolist (item (getf constraint-topics-of-topic
				    :subjectidentifier-constraints))
		  (pushnew item all-subjectidentifier-constraints))
		(dolist (item (getf constraint-topics-of-topic
				    :subjectlocator-constraints))
		  (pushnew item all-subjectlocator-constraints))
		(dolist (item (getf constraint-topics-of-topic
				    :topicname-constraints))
		  (pushnew item all-topicname-constraints))
		(dolist (item (getf constraint-topics-of-topic
				    :topicoccurrence-constraints))
		  (pushnew item all-topicoccurrence-constraints))
		(dolist (item (getf constraint-topics-of-topic
				    :uniqueoccurrence-constraints))
		  (pushnew item all-uniqueoccurrence-constraints))))
	(list :abstract-topictype-constraints all-abstract-topictype-constraints
	      :exclusive-instance-constraints all-exclusive-instance-constraints
	      :subjectidentifier-constraints all-subjectidentifier-constraints
	      :subjectlocator-constraints all-subjectlocator-constraints
	      :topicname-constraints all-topicname-constraints
	      :topicoccurrence-constraints all-topicoccurrence-constraints
	      :uniqueoccurrence-constraints all-uniqueoccurrence-constraints)))))


(defun get-direct-constraint-topics-of-association(associationtype-topic
						   &key (revision *TM-REVISION*))
  "Returns all direct constraint topics defined for associations if
   the passed associationtype-topic"
  (declare (type (or integer null) revision)
	   (TopicC associationtype-topic))
  (let ((constraint-role (get-item-by-psi *constraint-role-psi* :revision revision))
	(associationtype-role (get-item-by-psi *associationtype-role-psi*
					       :revision revision))
	(applies-to (get-item-by-psi *applies-to-psi* :revision revision))
	(associationtypescope-constraint
	 (get-item-by-psi *associationtypescope-constraint-psi* :revision revision))
	(associationrole-constraint (get-item-by-psi *associationrole-constraint-psi*
						     :revision revision))
	(roleplayer-constraint (get-item-by-psi *roleplayer-constraint-psi*
						:revision revision))
	(otherrole-constraint (get-item-by-psi *otherrole-constraint-psi*
					       :revision revision))
	(topictype (get-item-by-psi *topictype-psi* :revision revision))
	(topictype-constraint (get-item-by-psi *topictype-constraint-psi*
					       :revision revision))
	(associationrole-constraints nil)
	(roleplayer-constraints nil)
	(otherrole-constraints nil))
    (loop for role in (player-in-roles associationtype-topic :revision revision)
       when (and (eq associationtype-role (instance-of role :revision revision))
		 (eq applies-to (instance-of (parent role :revision revision)
					     :revision revision)))
       do (loop for other-role in (roles (parent role :revision revision)
					 :revision revision)
	     when (eq constraint-role (instance-of other-role :revision revision))
	     do (let ((constraint-topic (player other-role :revision revision)))
		  (cond
		    ((topictype-of-p constraint-topic associationtypescope-constraint
				     topictype topictype-constraint nil revision)
		     t) ;do nothing
		    ((topictype-of-p constraint-topic associationrole-constraint
				     topictype topictype-constraint nil revision)
		     (pushnew constraint-topic associationrole-constraints))
		    ((topictype-of-p constraint-topic roleplayer-constraint
				     topictype topictype-constraint nil revision)
		     (pushnew constraint-topic roleplayer-constraints))
		    ((topictype-of-p constraint-topic otherrole-constraint
				     topictype topictype-constraint nil revision)
		     (pushnew constraint-topic otherrole-constraints))
		    (t
		     (error "Constraint-Topic \"~a\" could not be handled"
			    (uri (first (psis constraint-topic
					      :revision revision)))))))))
    (list :associationrole-constraints associationrole-constraints
	  :roleplayer-constraints roleplayer-constraints
	  :otherrole-constraints otherrole-constraints)))


(defun get-all-constraint-topics-of-association(associationtype-topic &key
						(revision *TM-REVISION*))
  "Returns all constraint topics defined for associations if
   the passed associationtype-topic."
  (declare (type (or integer null) revision)
	   (TopicC associationtype-topic))
  (topictype-p associationtype-topic
	       (get-item-by-psi *associationtype-psi* :revision revision)
	       (is-type-constrained :what *associationtype-constraint-psi*
				    :revision revision) nil revision)
  (let ((akos-and-isas-of-this
	 (get-all-upper-constrainted-topics associationtype-topic
					    :revision revision)))
    (let ((all-associationrole-constraints nil)
	  (all-roleplayer-constraints nil)
	  (all-otherrole-constraints nil))
      (loop for topic in akos-and-isas-of-this
	 do (let ((constraint-topics-of-topic
		   (get-direct-constraint-topics-of-association topic
								:revision revision)))
	      (dolist (item (getf constraint-topics-of-topic
				  :associationrole-constraints))
		(pushnew item all-associationrole-constraints))
	      (dolist (item (getf constraint-topics-of-topic :roleplayer-constraints))
		(pushnew item all-roleplayer-constraints))
	      (dolist (item (getf constraint-topics-of-topic :otherrole-constraints))
		(pushnew item all-otherrole-constraints))))
      (list :associationrole-constraints all-associationrole-constraints
	    :roleplayer-constraints all-roleplayer-constraints
	    :otherrole-constraints all-otherrole-constraints))))


(defun get-available-associations-of-topic(topic-instance &key (treat-as 'type)
					   (revision *TM-REVISION*))
  "Returns a list of topics decribing the available associationtype for the
   passed topic."
  (declare (type (or integer null) revision)
	   (TopicC topic-instance)
	   (symbol treat-as))
  (let ((topictype (get-item-by-psi *topictype-psi* :revision revision))
	(topictype-constraint (get-item-by-psi *topictype-constraint-psi*
					       :revision revision)))
    (let ((applies-to (get-item-by-psi *applies-to-psi* :revision revision))
	  (topictype-role (get-item-by-psi *topictype-role-psi* :revision revision))
	  (constraint-role (get-item-by-psi *constraint-role-psi* :revision revision))
	  (othertopictype-role (get-item-by-psi *othertopictype-role-psi*
						:revision revision))
	  (associationtype-role (get-item-by-psi *associationtype-role-psi*
						 :revision revision))
	  (associationtype (get-item-by-psi *associationtype-psi* :revision revision))
	  (associationtype-constraint
	   (get-item-by-psi *associationtype-constraint-psi* :revision revision))
	  (roleplayer-constraint (get-item-by-psi *roleplayer-constraint-psi*
						  :revision revision))
	  (otherrole-constraint (get-item-by-psi *otherrole-constraint-psi*
						 :revision revision))
	  (all-possible-player-topics	 
	   (remove-duplicates
	    (if (eql treat-as 'type)
		(topictype-p topic-instance topictype topictype-constraint nil
			     revision)
		(valid-instance-p topic-instance nil nil revision)))))
      (let ((all-available-associationtypes
	     (remove-duplicates
	      (loop for possible-player-topic in all-possible-player-topics
		 append
		   (loop for role in (player-in-roles possible-player-topic
						      :revision revision)
		      when (and (or (eq topictype-role
					(instance-of role :revision revision))
				    (eq othertopictype-role
					(instance-of role :revision revision)))
				(eq applies-to
				    (instance-of (parent role :revision revision)
						 :revision revision)))
		      append
			(loop for other-role in
			     (roles (parent role :revision revision)
				    :revision revision)
			   when (and (eq constraint-role
					 (instance-of other-role :revision revision))
				     (or (topictype-of-p
					  (player other-role :revision revision)
					  roleplayer-constraint topictype
					  topictype-constraint nil revision)
					 (topictype-of-p
					  (player other-role :revision revision)
					  otherrole-constraint topictype
					  topictype-constraint nil revision)))
			   append
			     (loop for c-role in
				  (player-in-roles
				   (player other-role :revision revision)
				   :revision revision)
				when (and (eq constraint-role
					      (instance-of c-role :revision revision))
					  (eq applies-to
					      (instance-of (parent c-role
								   :revision revision)
							   :revision revision)))
				append
				  (loop for type-role in
				       (roles (parent c-role :revision revision)
					      :revision revision)
				     when (eq associationtype-role
					      (instance-of type-role
							   :revision revision))
				     append
				       (map 
					'list
					#'(lambda(x)
					    (topictype-p x associationtype
							 associationtype-constraint
							 nil revision)
					    x)
					(getf (list-subtypes
					       (player type-role :revision revision)
					       associationtype
					       associationtype-constraint nil
					       nil revision) :subtypes))))))))))
	all-available-associationtypes))))


(defun topics-to-json-list (topics &key (revision *TM-REVISION*))
  "Returns a json list of psi-lists."
  (declare (list topics)
	   (type (or integer null) revision))
  (json:encode-json-to-string 
   (map 'list #'(lambda(topic)
		  (map 'list #'uri (psis topic :revision revision)))
	topics)))


(defun tree-view-to-json-string (tree-views)
  "Returns a full tree-view as json-string."
  (let ((json-string 
	 (concatenate
	  'string "["
	  (if tree-views
	      (let ((inner-string ""))
		(loop for tree-view in tree-views
		   do (setf inner-string 
			    (concatenate 'string inner-string
					 (node-to-json-string tree-view) ",")))
		(concatenate 'string (subseq inner-string 0
					     (- (length inner-string) 1)) "]"))
	      "null"))))
    json-string))


(defun make-tree-view (&key (revision *TM-REVISION*))
  "Returns a list of the form:
   ((<topictype> (direct-instances) (direc-subtypes)) (<...>));
   -> direct-instances: (<any-topic> (direct-instances) (direct-subtypes))
   -> direct-subtypes: (<any-topic> (direct-instances) (direct-subtypes))"
  (declare (type (or integer null) revision))
  (let ((topictype
	 (get-item-by-psi *topictype-psi* :revision revision))
	(topictype-constraint (is-type-constrained :revision revision)))
    (if topictype-constraint
	(progn
	  (unless topictype
	    (error "From make-tree-view(): The topictype-constraint \"~a\" exists but the topictype \"~a\" is missing!"
		   *topictype-constraint-psi* 
		   *topictype-psi*))
	  (list (make-nodes topictype t t :revision revision)))
	(let ((tree-roots
	       (get-all-tree-roots :revision revision)))
	  (let ((tree-list
		 (loop for root in tree-roots
		    collect
		      (let ((l-is-type
			     (handler-case
				 (progn
				   (topictype-p root topictype topictype-constraint
						nil revision)
				   t)
			       (Condition () nil)))
			    (l-is-instance
			     (handler-case (progn
					     (valid-instance-p root nil nil revision)
					     t)
			       (Condition () nil))))
			(make-nodes root l-is-type l-is-instance
				    :revision revision)))))
	    tree-list)))))


(defun node-to-json-string(node &key (revision *TM-REVISION*))
  "Returns a json-object of the form
   {topic: [<psis>], isType: <bool>, isInstance: <bool>,
    instances: [<nodes>], subtypes: [<nodes>]}."
  (declare (type (or integer null) revision)
	   (list node))
  (let ((topic-psis
	 (concatenate
	  'string "\"topic\":"
	  (json:encode-json-to-string
	   (map 'list #'d:uri (d:psis (getf node :topic) :revision revision)))))
	(is-type
	 (concatenate 'string "\"isType\":"
		      (if (getf node :is-type)
			  "true"
			  "false")))
	(is-instance
	 (concatenate 'string "\"isInstance\":"
		      (if (getf node :is-instance)
			  "true"
			  "false")))
	(instances
	 (concatenate
	  'string "\"instances\":"
	  (if (getf node :instances)
	      (let ((inner-string "["))
		(loop for instance-node in (getf node :instances)
		   do (setf inner-string
			    (concatenate 
			     'string inner-string
			     (node-to-json-string instance-node :revision revision)
			     ",")))
		(concatenate 'string (subseq inner-string 0
					     (- (length inner-string) 1)) "]"))
	      "null")))
	(subtypes
	 (concatenate
	  'string "\"subtypes\":"
	  (if (getf node :subtypes)
	      (let ((inner-string "["))
		(loop for instance-node in (getf node :subtypes)
		   do (setf inner-string
			    (concatenate 'string inner-string
					 (node-to-json-string instance-node
							      :revision revision)
					 ",")))
		(concatenate 'string (subseq inner-string 0
					     (- (length inner-string) 1)) "]"))
	      "null"))))
    (concatenate 'string "{" topic-psis "," is-type "," is-instance "," instances
		 "," subtypes"}")))


(defun make-nodes (topic-instance is-type is-instance &key (revision *TM-REVISION*))
  "Creates a li of nodes.
   A node looks like
   (:topic <topic> :is-type <bool> :is-instance <bool> :instances <node>
    :subtypes <nodes>)."
  (declare (TopicC topic-instance)
	   (type (or integer null) revision))
  (let ((topictype (get-item-by-psi *topictype-psi* :revision revision))
	(topictype-constraint (is-type-constrained :revision revision)))
    (let ((isas-of-this
	   (map
	    'list
	    #'(lambda(z)
		(let ((l-is-type
		       (handler-case
			   (progn
			     (topictype-p z topictype topictype-constraint
					  nil revision)
			     t)
			 (Condition () nil)))
		      (l-is-instance
		       (handler-case (progn
				       (valid-instance-p z nil nil revision)
				       t)
			 (Condition () nil))))
		  (list :topic z :is-type l-is-type :is-instance l-is-instance)))
		(remove-duplicates
		 (remove-if #'null
			    (remove-if
			     #'(lambda(x) (when (eql topic-instance x)
					    t))
			     (get-direct-instances-of-topic topic-instance
							    :revision revision))))))
	  (akos-of-this
	   (map 'list
		#'(lambda(z)
		    (let ((l-is-type
			   (handler-case
			       (progn
				 (topictype-p z topictype topictype-constraint
					      nil revision)
				 t)
			     (Condition () nil)))
			  (l-is-instance
			   (handler-case (progn
					   (valid-instance-p z nil nil revision)
					   t)
			     (Condition () nil))))
		      (list :topic z :is-type l-is-type :is-instance l-is-instance)))
		(remove-duplicates
		 (remove-if
		  #'null
		  (remove-if #'(lambda(x) (when (eql topic-instance x)
					    t))
			     (get-direct-subtypes-of-topic topic-instance
							   :revision revision)))))))
      (let ((cleaned-isas ;;all constraint topics are removed
	     (clean-topic-entries isas-of-this :revision revision))
	    (cleaned-akos ;;all constraint topics are removed
	     (clean-topic-entries akos-of-this :revision revision)))
	(list :topic topic-instance
	      :is-type is-type
	      :is-instance is-instance
	      :instances (map 'list #'(lambda(x)
					(make-nodes (getf x :topic)
						    (getf x :is-type)
						    (getf x :is-instance)
						    :revision revision))
			      cleaned-isas)
	      :subtypes (map 'list #'(lambda(x)
				       (make-nodes (getf x :topic)
						   (getf x :is-type)
						   (getf x :is-instance)
						   :revision revision))
			     cleaned-akos))))))


(defun clean-topic-entries(isas-or-akos &key (revision *TM-REVISION*))
  "Removes all TMCL-topics from the passed topic-list."
  (remove-if
   #'null
   (map 'list
	#'(lambda(top-entry)
	    (when (and (d:find-item-by-revision (getf top-entry :topic) 0)
		       (find-if
			#'(lambda(psi)
			    (unless (or (string= (uri psi) *constraint-psi*)
					(string= (uri psi) *occurrencetype-psi*)
					(string= (uri psi) *nametype-psi*)
					(string= (uri psi) *associationtype-psi*)
					(string= (uri psi) *roletype-psi*)
					(string= (uri psi) *scopetype-psi*)
					(string= (uri psi) *schema-psi*))
			      top-entry))
			(psis (getf top-entry :topic) :revision revision)))
	      top-entry))
	isas-or-akos)))


(defun get-all-tree-roots (&key (revision *TM-REVISION*))
  "Returns all topics that are no instanceOf and no subtype 
   of any other topic."
  (declare (type (or integer null) revision))
  (let ((all-topics (get-all-topics revision)))
    (remove-if
     #'null
     (map 'list
	  #'(lambda(x)
	      (let ((isas-of-x
		     (remove-if #'(lambda(y)
				    (when (eql y x)
				      t))
				(get-direct-types-of-topic x :revision revision)))
		    (akos-of-x
		     (remove-if
		      #'(lambda(y)
			  (when (eql y x)
			    t))
		      (get-direct-supertypes-of-topic x :revision revision))))
		(unless (or isas-of-x akos-of-x)
		  x)))
	  all-topics))))