;;+-----------------------------------------------------------------------------
;;+  Isidorus
;;+  (c) 2008-2010 Marc Kuester, Christoph Ludwig, Lukas Georgieff
;;+
;;+  Isidorus is freely distributable under the LLGPL license.
;;+  You can find a detailed description in trunk/docs/LLGPL-LICENSE.txt and
;;+  trunk/docs/LGPL-LICENSE.txt.
;;+-----------------------------------------------------------------------------

(defpackage :TM-SPARQL
  (:use :cl :datamodel :base-tools :exceptions :constants)
  (:export :SPARQL-Query
	   :result))

(in-package :TM-SPARQL)

(defvar *empty-label* "_empty_label_symbol" "A label symobl for empyt prefix labels")

(defvar *equal-operators* nil "A Table taht contains tuples of 
                               classes and equality operators.")

(defun init-*equal-operators* ()
  (setf *equal-operators*
	(list (list :class 'Boolean :operator #'eql)
	      (list :class 'String :operator #'string=)
	      (list :class 'Number :operator #'=))))


(init-*equal-operators*)


(defun get-equal-operator (value)
  (let ((entry
	 (find-if #'(lambda(entry)
		      (typep value (getf entry :class)))
		  *equal-operators*)))
    (when entry
      (getf entry :operator))))


(defclass SPARQL-Triple-Elem()
  ((elem-type :initarg :elem-type
	      :reader elem-type
	      :type Symbol
	      :initform (error
			 (make-condition
			  'missing-argument-error
			  :message "From SPARQL-Triple-Elem(): elem-type must be set"))
	      :documentation "Contains information about the type of this element
                              possible values are 'IRI, 'VARIABLE, or 'LITERAL")
   (value :initarg :value
	  :accessor value
	  :type T
	  :initform nil
	  :documentation "Contains the actual value of any type.")
   (literal-lang :initarg :literal-lang
		 :accessor literal-lang
		 :initform nil
		 :type String
		 :documentation "Contains the @lang attribute of a literal")
   (literal-datatype :initarg :literal-datatype
		     :accessor literal-datatype
		     :type String
		     :initform nil
		     :documentation "Contains the datatype of the literal,
                                     e.g. xml:string"))
  (:documentation "Represents one element of an RDF-triple."))


(defclass SPARQL-Triple()
  ((subject :initarg :subject
	    :accessor subject
	    :type SPARQL-Triple-Elem
	    :initform (error
		       (make-condition
			'missing-argument-error
			:message "From SPARQL-Triple(): subject must be set"))
	    :documentation "Represents the subject of an RDF-triple.")
   (subject-result :initarg :subject-result
		   :accessor subject-result
		   :type T
		   :initform nil
		   :documentation "Contains the result of the subject triple elem.")
   (predicate :initarg :predicate
	      :accessor predicate
	      :type SPARQL-Triple-Elem
	      :initform (error
			 (make-condition
			  'missing-argument-error
			  :message "From SPARQL-Triple(): predicate must be set"))
	    :documentation "Represents the predicate of an RDF-triple.")
   (predicate-result :initarg :predicate-result
		     :accessor predicate-result
		     :type T
		     :initform nil
		     :documentation "Contains the result of the predicate
                                     triple elem.")
   (object :initarg :object
	   :accessor object
	   :type SPARQL-Triple-Elem
	   :initform (error
		      (make-condition
		       'missing-argument-error
		       :message "From SPARQL-Triple-(): object must be set"))
	   :documentation "Represents the subject of an RDF-triple.")
   (object-result :initarg :object-result
		  :accessor object-result
		  :type T
		  :initform nil
		  :documentation "Contains the result of the object triple elem."))
  (:documentation "Represents an entire RDF-triple."))


(defclass SPARQL-Query ()
  ((revision :initarg :revision
	     :accessor revision
	     :type Integer
	     :initform 0
	     :documentation "Represents the revision in which all the queries
                             are processed in the DB.")
   (original-query :initarg :query
		   :accessor original-query  ;this value is only for internal
					     ;purposes and mustn't be reset
		   :type String
		   :initform (error
			      (make-condition
			       'missing-argument-error
			       :message "From TM-Query(): original-query must be set"))
		   :documentation "Containst the original received querry as string")
   (variables :initarg :variables
	      :accessor variables ;this value is only for internal purposes
					;purposes and mustn't be reset
	      :type List
	      :initform nil
	      :documentation "A list of the form ((:variable var-name
                             :value value-object)), that contains tuples
                             for each selected variable and its result.")
   (prefixes :initarg :prefixes
	     :accessor prefixes ;this value is only for internal purposes
			        ;purposes and mustn't be reset
	     :type List
	     :initform nil
	     :documentation "A list of the form
                            ((:label 'id' :value 'prefix'))")
   (base-value :initarg :base ;initialy the requester's address
	       :accessor base-value ;this value is only for internal purposes
				    ;purposes and mustn't be reset
	       :type String
	       :initform nil
	       :documentation "Contains the last set base-value.")
   (select-group :initarg :select-group
		 :accessor select-group ;this value is only for
					;internal purposes purposes
					;and mustn't be reset
		 :type List
		 :initform nil
		 :documentation "Contains a SPARQL-Group that represents
                                 the entire inner select-where statement."))
  (:documentation "This class represents the entire request."))


(defmethod variables ((construct SPARQL-Triple-Elem))
  "Returns all variable names that are contained in the passed element."
  (remove-duplicates
   (remove-null
    (loop for triple in (select-group construct)
       collect (remove-null
		(list (when (variable-p (subject construct))
			(value (subject construct)))
		      (when (variable-p (predicate construct))
			(value (predicate construct)))
		      (when (variable-p (object construct))
		       (value (object construct)))))))
   :test #'string=))


(defgeneric add-triple (construct triple)
  (:documentation "Adds a triple object to the select-group list.")
  (:method ((construct SPARQL-Query) (triple SPARQL-Triple))
    (push triple (slot-value construct 'select-group))))


(defgeneric (setf elem-type) (construct elem-type)
  (:documentation "Sets the passed elem-type on the passed cosntruct.")
  (:method ((construct SPARQL-Triple-Elem) (elem-type Symbol))
    (unless (and (eql elem-type 'IRI)
		 (eql elem-type 'VARIABLE)
		 (eql elem-type 'LITERAL))
      (error (make-condition
	      'bad-argument-error
	      :message (format nil "Expected a one of the symbols ~a, but get ~a~%"
			       '('IRI 'VARIABLE 'LITERAL) elem-type))))
    (setf (slot-value construct 'elem-type) elem-type)))


(defgeneric add-prefix (construct prefix-label prefix-value)
  (:documentation "Adds the new prefix tuple to the list of all existing.
                   If there already exists a tuple with the same label
                   the label's value will be overwritten by the new value.")
  (:method ((construct SPARQL-Query) (prefix-label String) (prefix-value String))
    (let ((existing-tuple
	   (find-if #'(lambda(x)
			(string= (getf x :label) prefix-label))
		    (prefixes construct))))
      (if existing-tuple
	  (setf (getf existing-tuple :value) prefix-value)
	  (push (list :label prefix-label :value prefix-value)
		(prefixes construct))))))


(defgeneric get-prefix (construct string-with-prefix)
  (:documentation "Returns the URL corresponding to the found prefix-label
                   followed by : and the variable. Otherwise the return
                   value is nil.")
  (:method ((construct SPARQL-query) (string-with-prefix String))
    (loop for entry in (prefixes construct)
       when (string-starts-with string-with-prefix
				(concatenate 'string (getf entry :label) ":"))
       return (concatenate-uri
	       (getf entry :value)
	       (string-after string-with-prefix
			     (concatenate 'string (getf entry :label) ":"))))))


(defgeneric add-variable (construct variable-name variable-value)
  (:documentation "Adds a new variable-name with its value to the aexisting list.
                   If a variable-already exists the existing entry will be
                   overwritten. An entry is of the form
                   (:variable string :value any-type).")
  (:method ((construct SPARQL-Query) (variable-name String) variable-value)
    (let ((existing-tuple
	   (find-if #'(lambda(x)
			(string= (getf x :variable) variable-name))
		    (variables construct))))
      (if existing-tuple
	  (setf (getf existing-tuple :value) variable-value)
	  (push (list :variable variable-name :value variable-value)
		(variables construct))))))


(defgeneric set-results (construct &key revision)
  (:documentation "Calculates the result of a triple and set all the values in
                   the passed object.")
  (:method ((construct SPARQL-Triple) &key (revision d:*TM-REVISION*))
    (declare (Integer revision))
    (set-tm-constructs construct :revision revision)
    (when (not (iri-not-found-p construct)) ;there is only a result if all IRIs were found
      (let ((results (or (filter-by-given-subject construct :revision revision)
			 (filter-by-given-predicate construct :revision revision)
			 (filter-by-given-object construct :revision revision))))
	(map 'list #'(lambda(result)
		       (push (getf result :subject) (subject-result construct))
		       (push (getf result :predicate) (predicate-result construct))
		       (push (getf result :object) (object-result construct)))
	     ;;literal-datatype is not used and is not returned, since
	     ;;the values are returned as object of their specific type, e.g.
	     ;;integer, boolean, string, ...
	     results)))))


(defgeneric filter-by-given-object (construct &key revision)
  (:documentation "Returns a list representing a triple that is the result
                   of a given object.")
  (:method ((construct SPARQL-Triple) &key (revision *TM-REVISION*))
    (declare (Integer revision))
    (when (and (not (variable-p (object construct)))
	       (variable-p (predicate construct))
	       (variable-p (subject construct)))
      (cond ((literal-p (object construct))
	     (filter-by-characteristic-value (value (object construct))
					     (literal-datatype (object construct))
					     :revision revision))
	    ((iri-p (object construct))
	     (filter-by-otherplayer (value (object construct))
				    :revision revision))))))


(defun filter-by-characteristic-value (literal-value literal-datatype
				       &key (revision *TM-REVISION*))
  "Returns a triple where the passed value is a charvalue in a occurrence
   or name. The subject is the owner topic and the predicate is the
   characteristic's type."
  (declare (Integer revision)
	   (String literal-value literal-datatype))
  (let ((chars
	 (cond ((string= literal-datatype *xml-string*)
		(remove-if #'(lambda(elem)
			       (string/= (charvalue elem) literal-value))
			   (append
			    (elephant:get-instances-by-value
			     'OccurrenceC 'charvalue literal-value)
			    (elephant:get-instances-by-value
			     'NameC 'charvalue literal-value))))
	       ((and (string= literal-datatype *xml-boolean*)
		     (eql literal-value t))
		(remove-if #'(lambda(elem)
			       (string/= (charvalue elem) "true"))
			   (elephant:get-instances-by-value
			    'OccurrenceC 'charvalue "true")))
	       ((and (string= literal-datatype *xml-boolean*)
		     (eql literal-value nil))
		(remove-if #'(lambda(elem)
			       (string/= (charvalue elem) "false"))
			   (elephant:get-instances-by-value
			    'OccurrenceC 'charvalue "false")))
	       ((or (string= literal-datatype *xml-double*)
		    (string= literal-datatype *xml-decimal*)
		    (string= literal-datatype *xml-integer*))
		(let ((occs
		       (remove-if #'(lambda(occ)
				      (string/= (datatype occ) literal-datatype))
				  (elephant:get-instances-by-value
				   'OccurrenceC 'datatype literal-datatype))))
		  (remove-if #'(lambda(occ)
				 (not (literal= (charvalue occ) literal-value)))
			     occs))))))
    (remove-null
     (map 'list #'(lambda(char)
		    (let ((subj (when-do top (parent char :revision revision)
					 (any-id top :revision revision)))
			  (pred (when-do top (instance-of char :revision revision)
					 (any-id top :revision revision))))
		      (when (and subj pred)
			(list :subject subj
			      :predicate pred
			      :object (charvalue char)
			      :literal-datatyp literal-datatype))))
	  ;;elephant returns names, occurences, and variants if any string
	  ;;value matches, so all duplicates have to be removed, additionaly
	  ;;variants have to be remove completely
	  (remove-if #'(lambda(obj)
			 (typep obj 'VariantC))
		     (remove-duplicates chars))))))


(defgeneric filter-by-otherplayer (construct &key revision)
  (:documentation "Returns triples where the passed player is the object,
                   the other player is the subject and the type of the passed
                   player's role is the predicate.")
  (:method ((construct TopicC) &key (revision *TM-REVISION*))
    (declare (Integer revision))
    (let ((roles-by-oplayer (player-in-roles construct :revision revision))
	  (obj-uri (any-id construct :revision revision)))
      (remove-null
       (map 'list
	    #'(lambda(role)
		(let* ((orole
			(when-do assoc (parent role :revision revision)
				 (when (= (length (roles assoc :revision revision))
					  2)
				   (find-if #'(lambda(r) (not (eql r role)))
					    (roles assoc :revision revision)))))
		       (pred-uri
			(when-do type (instance-of role :revision revision)
				 (any-id type :revision revision)))
		       (subj-uri
			(when-do plr (player orole :revision revision)
				 (any-id plr :revision revision))))
		  (when (and obj-uri pred-uri subj-uri)
		    (list :subject subj-uri
			  :predicate pred-uri
			  :object obj-uri))))
	    roles-by-oplayer)))))


(defgeneric filter-by-given-predicate (construct &key revision)
  (:documentation "Returns all topics that owns a characteristic of the
                   given type or an associaiton with an otherrole of the
                   given type. The result is a plist representing a triple.")
  (:method ((construct SPARQL-Triple) &key (revision *TM-REVISION*))
    (declare (Integer revision))
    (when (and (variable-p (subject construct))
	       (iri-p (predicate construct)))
      (cond ((variable-p (object construct))
	     (append (filter-by-otherroletype construct :revision revision)
		     (filter-by-characteristictype construct :revision revision)))
	    ((literal-p (object construct))
	     (filter-by-characteristictype construct :revision revision))
	    ((iri-p (object construct))
	     (filter-by-otherroletype construct :revision revision))))))


(defgeneric filter-by-otherroletype (construct &key revision)
  (:documentation "Returns triple where the passed predicate is a
                   type of a role. The returned subject is the otherplayer,
                   the predicate is the passed predicate, the object is
                   the player of the role of the passed type.")
  (:method ((construct SPARQL-Triple) &key (revision *TM-REVISION*))
    (declare (Integer revision))
    (when (or (variable-p (object construct))
	      (iri-p (object construct)))
      (let* ((roles-by-type
	      (remove-null
	       (map 'list #'(lambda(typed-construct)
			      (when (typep typed-construct 'RoleC)
				typed-construct))
		    (used-as-type (value (predicate construct)) :revision revision))))
	     (roles-by-player
	      (if (iri-p (object construct))
		  (remove-null
		   (map 'list #'(lambda(role)
				  (when (eql (player role :revision revision)
					     (value (object construct)))
				    role))
			roles-by-type))
		  roles-by-type))
	     (pred-uri (any-id (value (predicate construct)) :revision revision)))
	(remove-null
	 (map 'list
	      #'(lambda(role)
		  (let* ((obj-uri
			  (when-do plr-top (player role :revision revision)
				   (any-id plr-top :revision revision)))
			 (assoc (parent role :revision revision))
			 (orole (when (and assoc
					   (= (length
					       (roles assoc :revision revision))
					      2))
				  (find-if #'(lambda(r)
					       (not (eql r role)))
					   (roles assoc :revision revision))))
			 (subj-uri
			  (when-do plr (player orole :revision revision)
				   (any-id plr :revision revision))))
		    (when (and subj-uri pred-uri obj-uri)
		      (list :subject subj-uri
			    :predicate pred-uri
			    :object obj-uri))))
	      roles-by-player))))))


(defgeneric filter-by-characteristictype (construct &key revision)
  (:documentation "Returns the results of filter-by-nametype and
                   filter-by-occurrencetype.")
  (:method ((construct SPARQL-Triple) &key (revision *TM-REVISION*))
    (declare (Integer revision))
    (append (filter-by-nametype construct :revision revision)
	    (filter-by-occurrencetype construct :revision revision))))


(defgeneric filter-by-nametype (construct &key revision)
  (:documentation "Returns all names that corresponds to the given parameters.")
  (:method ((construct SPARQL-Triple) &key (revision *TM-REVISION*))
    (declare (Integer revision))
    (when (and (not (iri-p (object construct)))
	       (or (not (literal-datatype (object construct)))
		   (string= (literal-datatype (object construct)) *xml-string*)))
      (let* ((names-by-type
	      (remove-null
	       (map 'list #'(lambda(typed-construct)
			      (when (typep typed-construct 'NameC)
				typed-construct))
		    (used-as-type (value (predicate construct))
				  :revision revision))))
	     (names-by-literal
	      (if (variable-p (object construct))
		  names-by-type
		  (remove-null
		   (map 'list #'(lambda(name)
				  (when (string= (charvalue name)
						 (value (object construct)))
				    name))
			names-by-type)))))
	(remove-null
	 (map 'list
	      #'(lambda(name)
		  (let ((subj
			 (when-do top (parent name :revision revision)
				  (any-id top :revision revision)))
			(pred
			 (when-do top (instance-of name :revision revision)
				  (any-id top :revision revision))))
		    (when (and subj pred)
		      (list :subject subj
			    :predicate pred
			    :object (charvalue name)
			    :literal-datatype *xml-string*))))
	      names-by-literal))))))


(defgeneric filter-by-occurrencetype (construct &key revision)
  (:documentation "Returns all occurrence that corresponds to the
                   given parameters.")
  (:method ((construct SPARQL-Triple) &key (revision *TM-REVISION*))
    (declare (Integer revision))
    (unless (iri-p (object construct))
      (let* ((occs-by-type
	      (remove-null
	       (map 'list #'(lambda(typed-construct)
			      (when (typep typed-construct 'OccurrenceC)
				typed-construct))
		    (used-as-type (value (predicate construct))
				  :revision revision))))
	     (all-occs
	      (let ((literal-value (if (variable-p (object construct))
				       nil
				       (value (object construct))))
		    (literal-datatype (literal-datatype (object construct))))
		(remove-null
		 (map 'list #'(lambda(occ)
				(filter-occ-by-value occ literal-value
						     literal-datatype))
		      occs-by-type)))))
	(remove-null
	 (map 'list
	      #'(lambda(occ)
		  (let ((subj
			 (when-do top (parent occ :revision revision)
				  (any-id top :revision revision)))
			(pred
			 (when-do top (instance-of occ :revision revision)
				  (any-id top :revision revision))))
		    (when (and subj pred)
		      (list :subject subj
			    :predicate pred
			    :object (charvalue occ)
			    :literal-datatype (datatype occ)))))
	      all-occs))))))


(defgeneric filter-by-given-subject (construct &key revision)
  (:documentation "Calls filter-characteristics and filter associations
                   for the topic that is set as a subject of the passed triple.")
  (:method ((construct SPARQL-Triple) &key (revision *TM-REVISION*))
    (declare (Integer revision))
    (when (iri-p (subject construct))
      (let* ((subj (value (subject construct)))
	     (pred (when (iri-p (predicate construct))
		     (value (predicate construct)))))
	(cond ((variable-p (object construct))
	       (append (filter-characteristics
			subj pred nil nil :revision revision)
		       (filter-associations
			subj pred nil :revision revision)))
	      ((literal-p (object construct))
	       (filter-characteristics
		subj pred (value (object construct))
		(literal-datatype (object construct)) :revision revision))
	      ((iri-p (object construct))
	       (filter-associations subj pred (value (object construct))
				    :revision revision)))))))


(defgeneric literal-p (construct)
  (:documentation "Returns t if the passed construct has an elem-type
                   set to 'LITERAL.")
  (:method ((construct SPARQL-Triple-Elem))
    (eql (elem-type construct) 'LITERAL)))


(defgeneric iri-p (construct)
  (:documentation "Returns t if the passed construct has an elem-type
                   set to 'IRI.")
  (:method ((construct SPARQL-Triple-Elem))
    (eql (elem-type construct) 'IRI)))


(defgeneric variable-p (construct)
  (:documentation "Returns t if the passed construct has an elem-type
                   set to 'VARIABLE.")
  (:method ((construct SPARQL-Triple-Elem))
    (eql (elem-type construct) 'VARIABLE)))


(defgeneric iri-not-found-p (construct)
  (:documentation "Must be called after a call of set-tm-constructs.
                   It returns t if a TM-construct was not found for a
                   given IRI, so the result value of a query is nil.")
  (:method ((construct SPARQL-Triple))
    (or (iri-not-found-p (subject construct))
	(iri-not-found-p (predicate construct))
	(iri-not-found-p (object construct)))))


(defmethod iri-not-found-p ((construct SPARQL-Triple-Elem))
  (and (eql (elem-type construct) 'IRI)
       (not (value construct))))


(defgeneric set-tm-constructs (construct &key revision)
  (:documentation "Calls the method set-tm-construct for every element
                   in a SPARQL-Triple object.")
  (:method ((construct SPARQL-Triple) &key (revision *TM-REVISION*))
    (when-do subj (subject construct)
	     (set-tm-construct subj :revision revision))
    (when-do pred (predicate construct)
	     (set-tm-construct pred :revision revision))
    (when-do obj (object construct) (set-tm-construct obj :revision revision))))


(defgeneric set-tm-construct (construct &key revision)
  (:documentation "Replaces the IRI in the given object by the corresponding
                   TM-construct.")
  (:method ((construct SPARQL-Triple-Elem) &key (revision *TM-REVISION*))
    (declare (Integer revision))
    (when (eql (elem-type construct) 'IRI)
      (setf (value construct)
	    (get-item-by-any-id (value construct) :revision revision)))))


(defun literal= (value-1 value-2)
  "Returns t if both arguments are equal. The equality function is searched in
   the table *equal-operators*."
  (when (or (and (numberp value-1) (numberp value-2))
	    (typep value-1 (type-of value-2))
	    (typep value-2 (type-of value-1)))
    (let ((operator (get-equal-operator value-1)))
      (funcall operator value-1 value-2))))


(defun filter-occ-by-value (occurrence literal-value literal-datatype)
  "A helper that compares the occurrence's charvalue with the passed
   literal value."
  (declare (OccurrenceC occurrence)
	   (type (or Null String) literal-value literal-datatype))
  (when (or (not literal-datatype)
	    (string= (datatype occurrence) literal-datatype))
    (if (not literal-value)
	occurrence
	(handler-case
	    (let ((occ-value (cast-literal (charvalue occurrence)
					   (datatype occurrence))))
	      (when (literal= occ-value literal-value)
		occurrence))
	  (condition () nil)))))	      
      

(defgeneric filter-occurrences(construct type-top literal-value
					 literal-datatype &key revision)
  (:documentation "Returns a list representing a triple.")
  (:method ((construct TopicC) type-top literal-value literal-datatype
	    &key (revision *TM-REVISION*))
    (declare (Integer revision)
	     (type (or Null String) literal-value literal-datatype)
	     (type (or Null TopicC) type-top))
    (let* ((occs-by-type
	    (if type-top
		(occurrences-by-type construct type-top :revision revision)
		(occurrences construct :revision revision)))
	   (all-occs
	    (remove-null
	     (map 'list
		  #'(lambda(occ)
		      (filter-occ-by-value occ literal-value literal-datatype))
		  occs-by-type)))
	   (subj-uri (any-id construct :revision revision)))
      (remove-null
       (map 'list #'(lambda(occ)
		      (let ((pred-uri
			     (when-do type-top (instance-of occ :revision revision)
				      (any-id type-top :revision revision))))
			(when pred-uri
			  (list :subject subj-uri
				:predicate pred-uri
				:object (charvalue occ)
				:literal-datatype (datatype occ)))))
	    all-occs)))))


(defgeneric filter-names(construct type-top literal-value
				   &key revision)
  (:documentation "Returns a list representing a triple.")
  (:method ((construct TopicC) type-top literal-value
	    &key (revision *TM-REVISION*))
    (declare (Integer revision)
	     (type (or Null String) literal-value)
	     (type (or Null TopicC) type-top))
    (let* ((by-type
	    (if type-top
		(names-by-type construct type-top :revision revision)
		(names construct :revision revision)))
	   (by-literal (if literal-value
			   (names-by-value
			    construct #'(lambda(name)
					  (string= name literal-value))
			    :revision revision)
			   (names construct :revision revision)))
	   (all-names (intersection by-type by-literal))
	   (subj-uri (any-id construct :revision revision)))
      (remove-null
       (map 'list #'(lambda(name)
		      (let ((pred-uri
			     (when-do type-top (instance-of name :revision revision)
				      (any-id type-top :revision revision))))
			(when pred-uri
			  (list :subject subj-uri
				:predicate pred-uri
				:object (charvalue name)
				:literal-datatype *xml-string*))))
	    all-names)))))


(defgeneric filter-characteristics (construct type-top literal-value
					      literal-datatype &key revision)
  (:documentation "Returns a list representing a triple.")
  (:method ((construct TopicC) type-top literal-value literal-datatype
	    &key (revision *TM-REVISION*))
    (declare (Integer revision)
	     (type (or Null String) literal-value literal-datatype)
	     (type (or Null TopicC) type-top))
    (let ((occs (filter-occurrences construct type-top literal-value
				    literal-datatype :revision revision))
	  (names (if (or (not literal-datatype)
			 (string= literal-datatype *xml-string*))
		     (filter-names construct type-top literal-value
				   :revision revision)
		     nil)))
      (append occs names))))


(defgeneric filter-associations(construct type-top player-top
					  &key revision)
  (:documentation "Returns a list of the form (:predicate <uri>
                   :object <uri> :subject <uri>).
                   predicate is the type of the otherrole and
                   object is the uri of the otherplayer.")
  (:method ((construct TopicC) type-top player-top
	    &key (revision *TM-REVISION*))
    (declare (Integer revision)
	     (type (or Null TopicC) type-top player-top))
    (let ((assocs
	   (associations-of construct nil nil type-top player-top
			    :revision revision))
	  (subj-uri (any-id construct :revision revision)))
      (remove-null ;only assocs with two roles can match!
       (map 'list
	    #'(lambda(assoc)
		(when (= (length (roles assoc :revision revision)) 2)
		  (let* ((other-role
			  (find-if #'(lambda(role)
				       (and
					(not (eql construct
						  (player role :revision revision)))
					(or (not type-top)
					    (eql type-top
						 (instance-of
						  role :revision revision)))))
				   (roles assoc :revision revision)))
			 (pred-uri
			  (when other-role
			    (when-do
			     type-top (instance-of other-role
						   :revision revision)
			     (any-id type-top :revision revision))))
			 
			 (obj-uri
			  (when other-role
			    (when-do player-top (player other-role
							:revision revision)
				     (any-id player-top :revision revision)))))
		    (when (and pred-uri obj-uri)
		      (list :subject subj-uri
			    :predicate pred-uri
			    :object obj-uri)))))
	    assocs)))))



(defgeneric result (construct)
  (:documentation "Returns the result of the entire query.")
  (:method ((construct SPARQL-Query))
    (let ((result-lists (make-result-lists construct)))
      (reduce-results construct result-lists)
      (let* ((response-variables (variables construct))
	     (cleaned-results (make-result-lists construct)))
	(map 'list #'(lambda(response-variable)
		       (variable-intersection response-variable
					      cleaned-results))
	     response-variables)))))


(defgeneric make-result-lists (construct)
  (:documentation "Returns a list of the form ((:variable 'var-name'
                   :result (<any-object>)).")
  (:method ((construct SPARQL-Query))
    (remove-null
     (loop for triple in (select-group construct)
	collect (remove-null
		 (list
		  (when (variable-p (subject construct))
		    (list :variable (value (subject construct))
			  :result (subject-result construct)))
		  (when (variable-p (predicate construct))
		    (list :variable (value (predicate construct))
			  :result (predicate-result construct)))
		  (when (variable-p (object construct))
		    (list :variable (value (object construct))
			  :result (object-result construct)))))))))


(defgeneric all-variables (result-lists)
  (:documentation "Returns a list of all variables that are contained in
                   the passed result-lists.")
  (:method ((result-lists List))
    (remove-duplicates
     (map 'list #'(lambda(entry)
		    (getf entry :variable))
	  result-lists)
     :test #'string=)))


(defgeneric variable-intersection (variable-name result-lists)
  (:documentation "Returns a list with all results of the passed variable
                   that are contained in the result-lists. All results is
                   an intersection of all paratial results.")
  (:method ((variable-name String) (result-lists List))
    (let* ((all-values (results-for-variable variable-name result-lists))
	   (list-1 (when (>= (length all-values) 1)
		     (first all-values)))
	   (list-2 (if (> (length all-values) 2)
		       (second all-values)
		       list-1))
	   (more-lists (rest (rest all-values))))
      (recursive-intersection list-1 list-2 more-lists))))


(defun recursive-intersection (list-1 list-2 &rest more-lists)
  "Returns an intersection of al the passed lists."
  (declare (List list-1 list-2))
  (let ((current-result
	 (intersection list-1 list-2
		       :test #'(lambda(val-1 val-2)
				 (if (and (stringp val-1) (stringp val-2))
				     (string= val-1 val-2)
				     (eql val-1 val-2))))))
    (if (= (length more-lists) 0)
	current-result
	(apply #'recursive-intersection current-result
	       (first more-lists) (rest more-lists)))))


(defgeneric reduce-results(construct result-lists)
  (:documentation "Reduces the select-group of the passed construct by processing
                   all triples with the intersection-results.")
  (:method ((construct SPARQL-Query) (result-lists List))
    (map 'list #'(lambda(triple)
		   (reduce-triple triple result-lists))
	 (select-group construct))))


(defgeneric reduce-triple(construct result-lists)
  (:documentation "Reduces the results of a triple by using only the
                   intersection values.")
  (:method ((construct SPARQL-Triple-Elem) (result-lists List))
    (let* ((triple-variables (variables construct))
	   (intersections
	    (map 'list #'(lambda(var)
			   (list :variable var
				 :result (variable-intersection
					  var result-lists)))
		 triple-variables)))
      (map 'list #'(lambda(entry)
		     (delete-rows construct (getf entry :variable)
				  (getf entry :result)))
	   intersections))))


(defgeneric delete-rows (construct variable-name dont-touch-values)
  (:documentation "Checks all results of the passed variable of the given
                   construct and deletes every result with the corresponding
                   row that is not contained in the dont-touch-values.")
  (:method ((construct SPARQL-Triple-Elem) (variable-name String)
	    (dont-touch-values List))
    (let ((var-elem
	   (cond ((and (variable-p (subject construct))
		       (string= (value (subject construct)) variable-name))
		  (subject-result construct))
		 ((and (variable-p (predicate construct))
		       (string= (value (predicate construct)) variable-name))
		  (predicate-result construct))
		 ((and (variable-p (object construct))
		       (string= (value (object construct)) variable-name))
		  (object-result construct)))))
      (if (not var-elem)
	  construct
	  (let* ((rows-to-hold
		  (remove-null
		   (map 'list #'(lambda(val)
				  (if (stringp val)
				      (position val var-elem :test #'string=)
				      (position val var-elem)))
			var-elem)))
		 (new-result-list
		  (dolist (row-idx rows-to-hold)
		    (list :subject (elt (subject-result construct) row-idx)
			  :predicate (elt (predicate-result construct) row-idx)
			  :object (elt (object-result construct) row-idx)))))
	    (setf (subject-result construct)
		  (map 'list #'(lambda(entry)
				 (getf entry :subject)) new-result-list))
	    (setf (predicate-result construct)
		  (map 'list #'(lambda(entry)
				 (getf entry :predicate)) new-result-list))
	    (setf (object-result construct)
		  (map 'list #'(lambda(entry)
				 (getf entry :object)) new-result-list)))))))


(defgeneric results-for-variable (variable-name result-lists)
  (:documentation "Returns a list with result-lists for the passed variable.")
  (:method ((variable-name String) (result-lists List))
    (let* ((cleaned-result-lists
	    (remove-if-not #'(lambda(entry)
			       (string= (getf entry :variable)
					variable-name))
			   result-lists))
	   (values
	    (map 'list #'(lambda(entry)
			   (getf entry :result))
		 cleaned-result-lists)))
      values)))


(defmethod initialize-instance :after ((construct SPARQL-Query) &rest args)
  (declare (ignorable args))
  (parser-start construct (original-query construct))
  (dolist (triple (select-group construct))
    (set-results triple :revision (revision construct)))
  construct)