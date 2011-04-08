;;+-----------------------------------------------------------------------------
;;+  Isidorus
;;+  (c) 2008-2010 Marc Kuester, Christoph Ludwig, Lukas Georgieff
;;+
;;+  Isidorus is freely distributable under the LLGPL license.
;;+  You can find a detailed description in trunk/docs/LLGPL-LICENSE.txt and
;;+  trunk/docs/LGPL-LICENSE.txt.
;;+-----------------------------------------------------------------------------

(defpackage :TM-SPARQL
  (:use :cl :datamodel :base-tools :exceptions :constants
	:TM-SPARQL-Constants :xml-importer :xml-constants
	:isidorus-threading :xml-tools)
  (:export :SPARQL-Query
	   :result
	   :init-tm-sparql))


(in-package :TM-SPARQL)

(defvar *empty-label* "_empty_label_symbol" "A label symbol for empyt prefix labels")

(defvar *equal-operators* nil "A Table taht contains tuples of 
                               classes and equality operators.")



(defgeneric sparql-node (construct &key revision)
  (:documentation "Returns a string of the form <uri> or _t123 that represents
                   a resource node or a blank node.")
  (:method ((construct TopicMapConstructC) &key (revision d:*TM-REVISION*))
    (declare (Integer revision))
    (let ((uri-string (any-id construct :revision revision)))
      (if uri-string
	  (concat "<" uri-string ">")
	  (let ((oid-string (write-to-string (elephant::oid construct)))
		(pref (subseq (symbol-name (type-of construct)) 0 1)))
	    (concat "_:" (string-downcase pref) oid-string))))))


(defun init-tm-sparql (&optional (revision (get-revision)))
  "Imports the file tmsparql_core_psis.xtm. core_psis.xtm has to be imported
   before."
  (with-writer-lock
    (with-tm (revision "tmsparql.xtm" (concat *tms* "topic-map"))
      (let ((core-dom (cxml:parse-file *tmsparql_core_psis.xtm*
				       (cxml-dom:make-dom-builder)))
	    (xtm-id (reverse
		     (base-tools:string-until
		      (reverse
		       (pathname-name
			xml-constants:*tmsparql_core_psis.xtm*)) "/"))))
	(elephant:ensure-transaction (:txn-nosync t)
	  (loop for top-elem across 
	       (xpath-child-elems-by-qname (dom:document-element core-dom)
					   *xtm2.0-ns* "topic")
	     do (let ((top
		       (from-topic-elem-to-stub top-elem revision
						:xtm-id xtm-id)))
		  (add-to-tm xml-importer::tm top))))))))



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
		  :type List
		  :initform nil
		  :documentation "Contains the result of the object triple elem.")
   (object-datatype :initarg :object-datatype
		    :accessor object-datatype
		    :type List
		    :initform nil
		    :documentation "Conations the corresponding value's datatype."))
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
	      :documentation "A list of that contains the variable
                              names as strings.")
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
                                 the entire inner select-where statement.")
   (filters :initarg filters
	    :accessor filters ;this value is only for internal purposes
			      ;purposes and mustn't be reset
	    :type List ;a list of strings
	    :initform nil
	    :documentation "Contains strings, each string represents a filter
                            that was transformed to lisp code and can be evoked
                            on each triple in the list select-group."))
  (:documentation "This class represents the entire request."))


(defgeneric *-p (construct)
  (:documentation "Returns t if the user selected all variables with *.")
  (:method ((construct SPARQL-Query))
    (loop for var in (variables construct)
       when (string= var "*")
       return t)))


(defgeneric add-filter (construct filter)
  (:documentation "Pushes the filter string to the corresponding list in
                   the construct.")
  (:method ((construct SPARQL-Query) (filter String))
    (push filter (filters construct))))


(defmethod variables ((construct SPARQL-Triple))
  "Returns all variable names that are contained in the passed element."
  (remove-duplicates
   (remove-null
    (list (when (variable-p (subject construct))
	    (value (subject construct)))
	  (when (variable-p (predicate construct))
	    (value (predicate construct)))
	  (when (variable-p (object construct))
	    (value (object construct)))))
   :test #'string=))


(defgeneric add-triple (construct triple)
  (:documentation "Adds a triple object to the select-group list.")
  (:method ((construct SPARQL-Query) (triple SPARQL-Triple))
    (push triple (slot-value construct 'select-group))))


(defgeneric (setf elem-type) (value construct)
  (:documentation "Sets the passed elem-type on the passed cosntruct.")
  (:method ((value Symbol) (construct SPARQL-Triple-Elem))
    (when (and (not (eql value 'IRI))
	       (not (eql value 'VARIABLE))
	       (not (eql value 'LITERAL)))
      (error (make-condition
	      'bad-argument-error
	      :message (format nil "Expected a one of the symbols ~a, but get ~a~%"
			       '('IRI 'VARIABLE 'LITERAL) value))))
    (setf (slot-value construct 'elem-type) value)))


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
       when (string-starts-with string-with-prefix (concat (getf entry :label) ":"))
       return (concatenate-uri
	       (getf entry :value)
	       (string-after string-with-prefix (concat (getf entry :label) ":"))))))


(defgeneric add-variable (construct variable-name)
  (:documentation "Adds a new variable-name with its value to the aexisting list.
                   If a variable-already exists the existing entry will be
                   overwritten. An entry is of the form
                   (:variable string :value any-type).")
  (:method ((construct SPARQL-Query) (variable-name String))
    (unless (find variable-name (variables construct) :test #'string=)
      (push variable-name (variables construct)))))



(defgeneric cast-variable-values(construct variable-value-list)
  (:documentation "Casts all values contained in the variable value list
                   to the corresponding type that is also stored in the
                   variable-value list.")
  (:method ((construct SPARQL-Query) (variable-value-list List))
    (map 'list
	 #'(lambda(item)
	     (map 'list
		  #'(lambda(inner-item)
		      (list :variable-name (getf inner-item :variable-name)
			    :variable-value
			    (if (and (getf inner-item :variable-value)
				     (getf inner-item :literal-datatype))
				(cast-literal (getf inner-item :variable-value)
					      (getf inner-item :literal-datatype)
					      :back-as-string-when-unsupported t)
				(getf inner-item :variable-value))))
		  item))
	 variable-value-list)))


(defgeneric make-variable-values(construct variable-name existing-results)
  (:documentation "Returns a list of values that are bound to the passed
                   variable. The first occurrence of the given variable
                   is evaluated, since all occurrences have the same values,
                   because reduce-results is called before and makes an
                   intersection over all triples.")
  (:method ((construct SPARQL-Query) (variable-name String) (existing-results List))
    (let* ((found-p nil)
	   (results
	    (loop for triple in (select-group construct)
	       when (and (variable-p (subject triple))
			 (string= (value (subject triple)) variable-name))
	       return (progn (setf found-p t)
			     (list :result (subject-result triple)))
	       when (and (variable-p (predicate triple))
			 (string= (value (predicate triple)) variable-name))
	       return (progn (setf found-p t)
			     (list :result (predicate-result triple)))
	       when (and (variable-p (object triple))
			 (string= (value (object triple))
				  variable-name))
	       return (progn (setf found-p t)
			     (list :result (object-result triple)
				   :literal-datatype (object-datatype triple)))))
	   (new-results nil))
      (if (not found-p)
	  existing-results
	  (if existing-results
	      (dotimes (idx (length (getf results :result)) new-results)
		(dolist (old-result existing-results)
		  (push (append old-result
				(list
				 (list :variable-name variable-name
				       :literal-datatype
				       (when (getf results :literal-datatype)
					 (elt (getf results :literal-datatype) idx))
				       :variable-value
				       (elt (getf results :result) idx))))
			new-results)))
	      (loop for idx to (1- (length (getf results :result)))
		 collect (list
			  (list :variable-name variable-name
				:literal-datatype
				(when (getf results :literal-datatype)
				  (elt (getf results :literal-datatype) idx))
				:variable-value
				(elt (getf results :result) idx)))))))))


(defun to-lisp-code (variable-values filter)
  "Concatenates all variable names and elements with the filter expression
   in a let statement and returns a string representing the corresponding
   lisp code."
  (declare (List variable-values))
  (let ((result "(let* ((true t)(false nil)"))
    (dolist (var-elem variable-values)
      (push-string (concat "(?" (getf var-elem :variable-name) " "
			   (write-to-string (getf var-elem :variable-value)) ")")
		   result)
      (push-string (concat "($" (getf var-elem :variable-name) " "
			   (write-to-string (getf var-elem :variable-value)) ")")
		   result))
    (push-string (concat "(result " filter "))") result)
    (push-string "(declare (Ignorable true false " result)
    (when variable-values
      (dolist (var-elem variable-values)
	(push-string (concat "?" (getf var-elem :variable-name) " ") result)
	(push-string (concat "$" (getf var-elem :variable-name) " ") result))
      (push-string ")" result))
    (when variable-values
      (push-string "(Special " result)
      (dolist (var-elem variable-values)
	(push-string (concat "?" (getf var-elem :variable-name) " ") result)
	(push-string (concat "$" (getf var-elem :variable-name) " ") result)))
    (push-string ")) result)" result)
    (concat "(handler-case " result " (condition () nil))")))


(defun return-false-values (all-values true-values)
  "Returns a list that contains all values from all-values that
   are not contained in true-values."
  (cond ((not all-values)
	 nil)
	((not true-values)
	 (let ((local-all-values
		(remove-duplicates (reduce #'(lambda(x y) (append x y)) all-values)
				   :test #'variable-list=)))
	   local-all-values))
	(t
	 (let ((local-all-values
		(remove-duplicates (reduce #'(lambda(x y) (append x y)) all-values)
				   :test #'variable-list=))
	       (results nil))
	   (dolist (value local-all-values)
	     (when (not (find value true-values :test #'variable-list=))
	       (push value results)))
	   results))))


(defun variable-list= (x y)
  (and (string= (getf x :variable-name)
		(getf y :variable-name))
       (literal= (getf x :variable-value)
		 (getf y :variable-value))))


(defgeneric process-filters (construct)
  (:documentation "Processes all filters by calling invoke-filter.")
  (:method ((construct SPARQL-Query))
    (dolist (filter (filters construct))
      (let ((filter-variable-names (get-variables-from-filter-string filter))
	    (filter-variable-values nil))
	(dolist (var-name filter-variable-names)
	  (setf filter-variable-values
		(make-variable-values construct var-name filter-variable-values)))
	(setf filter-variable-values
	      (remove-duplicates-from-variable-list construct filter-variable-values))
	(setf filter-variable-values
	      (cast-variable-values construct filter-variable-values))
	(let ((true-values nil))
	  (dolist (var-elem filter-variable-values)
	    (when (eval (read-from-string (to-lisp-code var-elem filter)))
	      (map 'list #'(lambda(list-elem)
			     (push list-elem true-values))
		   var-elem)))
	  (let ((values-to-remove
		 (return-false-values filter-variable-values
				      (remove-duplicates true-values
							 :test #'variable-list=))))
	    (dolist (to-del values-to-remove)
	      (delete-rows-by-value construct (getf to-del :variable-name)
				    (getf to-del :variable-value)))))))))


(defgeneric remove-duplicates-from-variable-list (construct variable-list)
  (:documentation "Removes all duplicates from the passed variable list")
  (:method ((construct SPARQL-QUERY) (variable-list LIST))
    (remove-duplicates
     variable-list
     :test #'(lambda(x y)
	       (when (= (length x) (length y))
		 (let ((result nil))
		   (dotimes (idx (length x) result)
		     (let ((cx (elt x idx))
			   (cy (elt y idx)))
		       (when (or (string/= (getf cx :variable-name)
					   (getf cy :variable-name))
				 (and (getf cx :literal-datatype)
				      (getf cy :literal-datatype)
				      (string/= (getf cx :literal-datatype)
						(getf cy :literal-datatype)))
				 (and (getf cx :literal-datatype)
				      (not (getf cy :literal-datatype)))
				 (and (not (getf cx :literal-datatype))
				      (getf cy :literal-datatype))
				 (and (getf cx :variable-value)
				      (getf cy :variable-value)
				      (string/= (getf cx :variable-value)
						(getf cy :variable-value)))
				 (and (getf cx :variable-value)
				      (not (getf cy :variable-value)))
				 (and (not (getf cx :variable-value))
				      (getf cy :variable-value)))
			 (setf idx (length x))))
		     (when (= idx (max 0 (1- (length x))))
		       (setf result t)))))))))


(defgeneric idx-of (construct variable-name variable-value &key what)
  (:documentation "Returns the idx of the variable with the name
                   variable-name and the value variable-value.")
  (:method ((construct SPARQL-Triple) (variable-name String)
	    variable-value &key (what :subject))
    (declare (Keyword what))
    (let ((result nil)
	  (local-results
	   (cond ((eql what :subject) (subject-result construct))
		 ((eql what :predicate) (predicate-result construct))
		 ((eql what :object)
		  (if (object-datatype construct)
		      (loop for idx to (1- (length (object-result construct)))
			 when (elt (object-datatype construct) idx)
			 collect (cast-literal
				  (elt (object-result construct) idx)
				  (elt (object-datatype construct) idx)
				  :back-as-string-when-unsupported t)
			 else
			 collect (elt (object-result construct) idx))
		      (object-result construct)))))
	  (variable-p
	   (cond ((eql what :subject)
		  (and (variable-p (subject construct))
		       (string= (value (subject construct)) variable-name)))
		 ((eql what :predicate)
		  (and (variable-p (predicate construct))
		       (string=  (value (predicate construct)) variable-name)))
		 ((eql what :object)
		  (and (variable-p (object construct))
		       (string= (value (object construct)) variable-name))))))
      (when variable-p
	(remove-null
	 (dotimes (idx (length local-results))
	   (when (literal= variable-value (elt local-results idx))
	     (push idx result)))))
      result)))


(defgeneric delete-rows-by-value (construct variable-name value-to-delete)
  (:documentation "Deletes all rows that owns a variable with the
                   given value.")
  (:method ((construct SPARQL-Query) (variable-name String) value-to-delete)
    (dolist (triple (select-group construct))
      (let* ((subj-delete-idx-lst
	      (idx-of triple variable-name value-to-delete))
	     (pred-delete-idx-lst
	      (idx-of triple variable-name value-to-delete :what :predicate))
	     (obj-delete-idx-lst
	      (idx-of triple variable-name value-to-delete :what :object))
	     (all-idxs (union (union subj-delete-idx-lst
				     pred-delete-idx-lst)
			      obj-delete-idx-lst)))
	(when all-idxs
	  (let ((new-values nil))
	    (dotimes (idx (length (subject-result triple)))
	      (when (not (find idx all-idxs))
		(push
		 (list :subject (elt (subject-result triple) idx)
		       :predicate (elt (predicate-result triple) idx)
		       :object (elt (object-result triple) idx)
		       :object-datatype (elt (object-datatype triple) idx))
		 new-values)))
	    (setf (subject-result triple)
		  (map 'list #'(lambda(elem) (getf elem :subject)) new-values))
	    (setf (predicate-result triple)
		  (map 'list #'(lambda(elem) (getf elem :predicate)) new-values))
	    (setf (object-result triple)
		  (map 'list #'(lambda(elem) (getf elem :object)) new-values))
	    (setf (object-datatype triple)
		  (map 'list #'(lambda(elem) (getf elem :object-datatype))
		       new-values))))))
    construct))


(defgeneric set-results (construct &key revision)
  (:documentation "Calculates the result of a triple and set all the values in
                   the passed object.")
  (:method ((construct SPARQL-Triple) &key (revision d:*TM-REVISION*))
    (declare (Integer revision))
    (set-tm-constructs construct :revision revision)
    (when (not (iri-not-found-p construct)) ;there is only a result if all IRIs were found
      (let ((results (append
		      (or (filter-by-given-subject construct :revision revision)
			  (filter-by-given-predicate construct :revision revision)
			  (filter-by-given-object construct :revision revision))
		      (filter-by-special-uris construct :revision revision))))
	(map 'list #'(lambda(result)
		       (push (getf result :subject) (subject-result construct))
		       (push (getf result :predicate) (predicate-result construct))
		       (push (getf result :object) (object-result construct))
		       (push (getf result :literal-datatype)
			     (object-datatype construct)))
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
	    ((and (iri-p (object construct))
		  (typep (value (object construct)) 'TopicC))
	     (filter-by-otherplayer (value (object construct))
				    :revision revision))))))


(defun return-characteristics (literal-value literal-datatype)
  "Returns all characteristica that own the specified value.
   Note the type xsd:date is not supported and so handled as a string."
  (declare (String literal-datatype))
  (let ((chars
	 (cond ((or (string= literal-datatype *xml-string*)
		    (string= literal-datatype *xml-date*))
		(remove-if #'(lambda(elem)
			       (string/= (charvalue elem) literal-value))
			   (append
			    (elephant:get-instances-by-value
			     'OccurrenceC 'charvalue literal-value)
			    (elephant:get-instances-by-value
			     'VariantC 'charvalue literal-value)
			    (elephant:get-instances-by-value
			     'NameC 'charvalue literal-value))))
	       ((and (string= literal-datatype *xml-boolean*)
		     (or (and (stringp literal-value) (string= literal-value "true"))
			 (and (typep literal-value 'Boolean) literal-value)))
		(remove-if #'(lambda(elem)
			       (string/= (charvalue elem) "true"))
			   (append (elephant:get-instances-by-value
				    'VariantC 'charvalue "true")
				   (elephant:get-instances-by-value
				    'OccurrenceC 'charvalue "true"))))
	       ((and (string= literal-datatype *xml-boolean*)
		     (or (and (stringp literal-value) (string= literal-value "false"))
			 (and (typep literal-value 'Boolean) (not literal-value))))
		(remove-if #'(lambda(elem)
			       (string/= (charvalue elem) "false"))
			   (append (elephant:get-instances-by-value
				    'VariantC 'charvalue "true")
				   (elephant:get-instances-by-value
				    'OccurrenceC 'charvalue "false"))))
	       ((or (string= literal-datatype *xml-double*)
		    (string= literal-datatype *xml-decimal*)
		    (string= literal-datatype *xml-integer*))
		(let ((constructs
		       (remove-if #'(lambda(con)
				      (string/= (datatype con) literal-datatype))
				  (append
				   (elephant:get-instances-by-value
				    'VariantC 'datatype literal-datatype)
				   (elephant:get-instances-by-value
				    'OccurrenceC 'datatype literal-datatype))))
		      (user-val (if (stringp literal-value)
				    (concat "\"\"\"" literal-value "\"\"\"^^"
					    literal-datatype)
				    literal-value)))
		  (remove-if #'(lambda(con)
				 (not (literal= (concat "\"\"\"" (charvalue con)
							"\"\"\"^^" (datatype con))
						user-val)))
			     constructs))))))
    ;;elephant returns names, occurences, and variants if any string
    ;;value matches, so all duplicates have to be removed
    (remove-duplicates chars)))


(defun filter-by-characteristic-value (literal-value literal-datatype
				       &key (revision *TM-REVISION*))
  "Returns a triple where the passed value is a charvalue in a occurrence
   or name. The subject is the owner topic and the predicate is the
   characteristic's type.
   (Variants are not considered because they are not typed, so they cannot
   be referenced via a predicate)."
  (declare (Integer revision)
	   (String literal-datatype))
    (remove-null
     (map 'list #'(lambda(char)
		    (let ((subj-uri
			   (when-do top (parent char :revision revision)
				    (sparql-node top :revision revision)))
			  (pred-uri
			   (when-do top (instance-of char :revision revision)
				    (sparql-node top :revision revision))))
		      (when (and subj-uri pred-uri)
			(list :subject subj-uri
			      :predicate pred-uri
			      :object (charvalue char)
			      :literal-datatype literal-datatype))))	  
	  (remove-if #'(lambda(char)
			 (typep char 'VariantC))
		     (return-characteristics literal-value literal-datatype)))))


(defgeneric filter-by-otherplayer (construct &key revision)
  (:documentation "Returns triples where the passed player is the object,
                   the other player is the subject and the type of the passed
                   player's role is the predicate.")
  (:method ((construct TopicC) &key (revision *TM-REVISION*))
    (declare (Integer revision))
    (let ((roles-by-oplayer (player-in-roles construct :revision revision))
	  (obj-uri (sparql-node construct :revision revision)))
      (remove-null
       (map 'list
	    #'(lambda(role)
		(let ((orole
		       (when-do assoc (parent role :revision revision)
				(when (= (length (roles assoc :revision revision))
					 2)
				  (find-if #'(lambda(r) (not (eql r role)))
					   (roles assoc :revision revision))))))
		  (when orole
		    (list :subject
			  (when-do plr (player orole :revision revision)
				   (sparql-node plr :revision revision))
			  :predicate
			  (when-do type (instance-of role :revision revision)
				   (sparql-node type :revision revision))
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
		  roles-by-type)))
	(remove-null
	 (map 'list
	      #'(lambda(role)
		  (let* ((assoc (parent role :revision revision))
			 (orole (when (and assoc
					   (= (length
					       (roles assoc :revision revision))
					      2))
				  (find-if #'(lambda(r)
					       (not (eql r role)))
					   (roles assoc :revision revision)))))
		    (when (and orole assoc)
		      (list :subject
			    (when-do plr (player orole :revision revision)
				     (sparql-node plr :revision revision))
			    :predicate
			    (sparql-node (value (predicate construct))
					 :revision revision)
			    :object
			    (when-do plr-top (player role :revision revision)
				     (sparql-node plr-top :revision revision))))))
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
		  (when (and (parent name :revision revision)
			     (instance-of name :revision revision))
		    (list :subject
			  (sparql-node (parent name :revision revision)
				       :revision revision)
			  :predicate
			  (sparql-node (instance-of name :revision revision)
				       :revision revision)
			  :object (charvalue name)
			  :literal-datatype *xml-string*)))
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
		  (when (and (parent occ :revision revision)
			     (instance-of occ :revision revision))
		    (list :subject
			  (sparql-node (parent occ :revision revision)
				       :revision revision)
			  :predicate
			  (sparql-node (instance-of occ :revision revision)
				       :revision revision)
			  :object (charvalue occ)
			  :literal-datatype (datatype occ))))
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
	       (when (typep subj 'TopicC)
		 (append (filter-characteristics
			  subj pred nil nil :revision revision)
			 (filter-associations
			  subj pred nil :revision revision))))
	      ((literal-p (object construct))
	       (when (typep subj 'TopicC)
		 (filter-characteristics
		  subj pred (value (object construct))
		  (literal-datatype (object construct)) :revision revision)))
	      ((and (iri-p (object construct))
		    (typep subj 'TopicC)
		    (or (variable-p (object construct))
			(typep (value (object construct)) 'TopicC)))
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


(defun split-literal-string (literal-string)
  "Returns a list of the form (:value literal-value :datatype literal-type)
   of a string literal-value^^literal-type."
  (when (stringp literal-string)
    (let ((str (cut-comment literal-string)))
      (when (string-starts-with-one-of literal-string (list "\"" "'"))
	(let* ((delimiter (cond ((string-starts-with str "'") "'")
				((string-starts-with str "\"\"\"") "\"\"\"")
				(t "\"")))
	       (l-end (find-literal-end (subseq str (length delimiter)) delimiter))
	       (l-value (subseq str (length delimiter) l-end))
	       (l-rest (subseq str (+ (length delimiter) l-end)))
	       (l-type (if (string-starts-with l-rest "^^")
			   (subseq l-rest 2)
			   *xml-string*)))
	  (list :value l-value :datatype l-type))))))


(defun literal= (value-1 value-2)
  "Returns t if both arguments are equal. The equality function is searched in
   the table *equal-operators*."
  (let ((real-value-1 (let ((result (split-literal-string value-1)))
			(if result
			    (cast-literal (getf result :value)
					  (getf result :datatype))
			    value-1)))
	(real-value-2 (let ((result (split-literal-string value-2)))
			(if result
			    (cast-literal (getf result :value)
					  (getf result :datatype))
			    value-2))))
    (when (or (and (numberp real-value-1) (numberp real-value-2))
	      (typep value-1 (type-of real-value-2))
	      (typep value-2 (type-of real-value-1)))
      (let ((operator (get-equal-operator real-value-1)))
	(funcall operator real-value-1 real-value-2)))))


(defun filter-datatypable-by-value (construct literal-value literal-datatype)
  "A helper that compares the datatypable's charvalue with the passed
   literal value."
  (declare (d::DatatypableC construct)
	   (type (or Null String) literal-datatype))
  (when (or (not literal-datatype)
	    (string= (datatype construct) literal-datatype))
    (if (and (not literal-value)
	     (string/= literal-datatype *xml-boolean*))
	construct
	(handler-case
	    (let ((occ-value
		   (cast-literal (charvalue construct)
				 (datatype construct)
				 :back-as-string-when-unsupported t)))
	      (when (literal= occ-value literal-value)
		construct))
	  (condition () nil)))))	      


(defun filter-variant-by-value (variant literal-value literal-datatype)
  "A helper that compares the occurrence's variant's with the passed
   literal value."
  (declare (VariantC variant)
	   (type (or Null String) literal-value literal-datatype))
  (filter-datatypable-by-value variant literal-value literal-datatype))


(defun filter-occ-by-value (occurrence literal-value literal-datatype)
  "A helper that compares the occurrence's charvalue with the passed
   literal value."
  (declare (OccurrenceC occurrence)
	   (type (or Null String) literal-datatype))
  (filter-datatypable-by-value occurrence literal-value literal-datatype))
      

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
	   (subj-uri (sparql-node construct :revision revision)))
      (remove-null
       (map 'list #'(lambda(occ)
		      (when (instance-of occ :revision revision)
			(list :subject subj-uri
			      :predicate (sparql-node
					  (instance-of occ :revision revision)
					  :revision revision)
			      :object (charvalue occ)
			      :literal-datatype (datatype occ))))
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
					  (literal= name literal-value))
					  ;(string= name literal-value))
			    :revision revision)
			   (names construct :revision revision)))
	   (all-names (intersection by-type by-literal))
	   (subj-uri (sparql-node construct :revision revision)))
      (map 'list #'(lambda(name)
		     (when (instance-of name :revision revision)
		       (list :subject subj-uri
			     :predicate (sparql-node
					 (instance-of name :revision revision)
					 :revision revision)
			     :object (charvalue name)
			     :literal-datatype *xml-string*)))
	   all-names))))


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
	  (subj-uri (sparql-node construct :revision revision)))
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
			     (sparql-node type-top :revision revision))))
			 
			 (obj-uri
			  (when other-role
			    (when-do player-top (player other-role
							:revision revision)
				     (sparql-node player-top :revision revision)))))
		    (when (and subj-uri pred-uri obj-uri)
		      (list :subject subj-uri
			    :predicate pred-uri
			    :object obj-uri)))))
	    assocs)))))


(defgeneric result (construct)
  (:documentation "Returns the result of the entire query.")
  (:method ((construct SPARQL-Query))
    (let* ((response-variables
	    (reverse (if (*-p construct)
			 (all-variables construct)
			 (variables construct))))
	   (cleaned-results (make-result-lists construct)))
      (let ((result
	     (map 'list #'(lambda(response-variable)
			    (let ((result
				   (variable-intersection response-variable
							  cleaned-results)))
			      (list :variable response-variable
				    :result (getf result :result)
				    :literal-datatype
				    (getf result :literal-datatype))))
		  response-variables)))
	(cast-result-values result)))))


(defun cast-result-values (result-values)
  "Casts all literal values that are represented as a string to
   the actual datatype."
  (declare (List result-values))
  (loop for set-idx to (1- (length result-values))
     collect (let ((value-set (getf (elt result-values set-idx) :result))
		   (type-set (getf (elt result-values set-idx) :literal-datatype))
		   (var-name (getf (elt result-values set-idx) :variable)))
	       (list :variable var-name
		     :result
		     (loop for value-idx to (1- (length value-set))
			when (elt type-set value-idx)
			collect (cast-literal (elt value-set value-idx)
					      (elt type-set value-idx))
			else
			collect (elt value-set value-idx))))))



(defgeneric make-result-lists (construct)
  (:documentation "Returns a list of the form ((:variable 'var-name'
                   :result (<any-object>)).")
  (:method ((construct SPARQL-Query))
    (remove-null
     (loop for triple in (select-group construct)
	append (remove-null
		(list
		 (when (variable-p (subject triple))
		   (list :variable (value (subject triple))
			 :result (subject-result triple)))
		 (when (variable-p (predicate triple))
		   (list :variable (value (predicate triple))
			 :result (predicate-result triple)))
		 (when (variable-p (object triple))
		   (list :variable (value (object triple))
			 :literal-datatype (object-datatype triple)
			 :result (object-result triple)))))))))


(defgeneric all-variables (result-lists)
  (:documentation "Returns a list of all variables that are contained in
                   the passed result-lists."))


(defmethod all-variables ((result-lists List))
  (remove-duplicates
   (map 'list #'(lambda(entry)
		  (getf entry :variable))
	result-lists)
   :test #'string=))


(defmethod all-variables ((construct SPARQL-Query))
  "Returns all variables that are contained in the select group memebers."
  (remove-duplicates
   (remove-null
    (loop for triple in (select-group construct)
       append (variables triple)))
   :test #'string=))


(defgeneric variable-intersection (variable-name result-lists)
  (:documentation "Returns a list with all results of the passed variable
                   that are contained in the result-lists. All results is
                   an intersection of all partial results.")
  (:method ((variable-name String) (result-lists List))
    (let* ((all-values (results-for-variable variable-name result-lists))
	   (list-1 (when (>= (length all-values) 1)
		     (first all-values)))
	   (list-2 (if (>= (length all-values) 2)
		       (second all-values)
		       list-1))
	   (more-lists (rest (rest all-values))))
      (recursive-intersection list-1 list-2 more-lists))))


(defun recursive-intersection (list-1 list-2 more-lists)
  "Returns an intersection of al the passed lists."
  (declare (List list-1 list-2))
  (let* ((current-result
	  (intersection (getf list-1 :result) (getf list-2 :result)
			:test #'(lambda(val-1 val-2)
				  (if (and (stringp val-1) (stringp val-2))
				      (string= val-1 val-2)
				      (eql val-1 val-2)))))
	 (current-datatypes
	  (map 'list #'(lambda(result-entry)
			 (let ((pos (position result-entry (getf list-1 :result)
					      :test #'string=)))
			   (when (getf list-1 :literal-datatype)
			     (elt (getf list-1 :literal-datatype) pos))))
	       current-result)))
    (if (not more-lists)
	(list :result current-result
	      :literal-datatype current-datatypes)
	(recursive-intersection (list :result current-result
				      :literal-datatype current-datatypes)
				(first more-lists)
				(rest more-lists)))))


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
  (:method ((construct SPARQL-Triple) (result-lists List))
    (let* ((triple-variables (variables construct))
	   (intersections
	    (map 'list
		 #'(lambda(var)
		     (let ((result (variable-intersection
				    var result-lists)))
		       (list :variable var
			     :result (getf result :result)
			     :literal-datatype (getf result :literal-datatype))))
		 triple-variables)))
      (map 'list #'(lambda(entry)
		     (delete-rows construct (getf entry :variable)
				  (getf entry :result)))
	   intersections))))



(defgeneric delete-rows (construct variable-name dont-touch-values)
  (:documentation "Checks all results of the passed variable of the given
                   construct and deletes every result with the corresponding
                   row that is not contained in the dont-touch-values.")
  (:method ((construct SPARQL-Triple) (variable-name String)
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
      (when var-elem
	(let* ((rows-to-hold
		(loop for idx to (max 0 (1- (length var-elem)))
		   when  (cond
			   ((stringp (elt var-elem idx))
			    (find (elt var-elem idx) dont-touch-values :test #'string=))
			   ((numberp (elt var-elem idx))
			    (find (elt var-elem idx) dont-touch-values :test #'=))
			   (t
			    (find (elt var-elem idx) dont-touch-values)))
		   collect idx))
	       (new-result-list
		(map
		 'list
		 #'(lambda(row-idx)
		     (list
		      :subject (elt (subject-result construct) row-idx)
		      :predicate (elt (predicate-result construct) row-idx)
		      :object (elt (object-result construct) row-idx)
		      :object-datatype (elt (object-datatype construct) row-idx)))
		     rows-to-hold)))
	  (setf (subject-result construct)
		(map 'list #'(lambda(entry)
			       (getf entry :subject)) new-result-list))
	  (setf (predicate-result construct)
		(map 'list #'(lambda(entry)
			       (getf entry :predicate)) new-result-list))
	  (setf (object-result construct)
		(map 'list #'(lambda(entry)
			       (getf entry :object)) new-result-list))
	  (setf (object-datatype construct)
		(map 'list #'(lambda(entry)
			       (getf entry :object-datatype)) new-result-list)))))))


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
			   (list :result (getf entry :result)
				 :literal-datatype (getf entry :literal-datatype)))
		 cleaned-result-lists)))
      values)))


(defun cast-literal (literal-value literal-type
		     &key (back-as-string-when-unsupported nil))
  "A helper function that casts the passed string value of the literal
   corresponding to the passed literal-type."
  (declare (String literal-value)
	   (type (or String null) literal-type)
	   (Boolean back-as-string-when-unsupported))
  (let ((local-literal-type (if literal-type literal-type *xml-string*)))
    (cond ((string= local-literal-type *xml-string*)
	   literal-value)
	  ((string= local-literal-type *xml-boolean*)
	   (cast-literal-to-boolean literal-value))
	  ((string= local-literal-type *xml-integer*)
	   (cast-literal-to-integer literal-value))
	  ((string= local-literal-type *xml-double*)
	   (cast-literal-to-double literal-value))
	  ((string= local-literal-type *xml-decimal*)
	   (cast-literal-to-decimal literal-value))
	  (t ; return the value as a string
	   (if back-as-string-when-unsupported
	       literal-value
	       (concat "\"\"\"" literal-value "\"\"\"^^" local-literal-type))))))


(defun cast-literal-to-decimal (literal-value)
  "A helper function that casts the passed string value of the literal
   value to an decimal value."
  (let ((bad-string
	 (loop for idx to (1- (length literal-value))
	    when (and (not (digit-char-p (elt literal-value idx)))
		      (not (eql (elt literal-value idx) #\.)))
	    return t)))
    (when bad-string
      (error (make-condition
	      'sparql-parser-error
	      :message (format nil "Could not cast from ~a to ~a"
			       literal-value *xml-decimal*)))))
  ;decimals are handled as single floats
  (if (find #\. literal-value)
      (read-from-string literal-value)
      (read-from-string (concat literal-value ".0"))))


(defun cast-literal-to-double (literal-value)
  "A helper function that casts the passed string value of the literal
   value to an decimal value."
  (let ((modified-str ""))
    (loop for idx to (1- (length literal-value))
       when (eql (char-downcase (elt literal-value idx)) #\e)
       do (push-string "d" modified-str)
       else
       do (push-string (string (elt literal-value idx)) modified-str))
    (let ((value
	   (cond ((or (string= "+INF" modified-str)
		      (string= "INF" modified-str))
		  sb-ext:double-float-positive-infinity)
		 ((string= "-INF" modified-str)
		  sb-ext:double-float-negative-infinity)
		 ((find #\d (string-downcase modified-str))
		  (read-from-string modified-str))
		 (t
		  (read-from-string (concat modified-str "d0"))))))
      (if (typep value 'double-float)
	  value
	  (error (make-condition
		  'sparql-parser-error
		  :message (format nil "Could not cast from ~a to ~a"
				   literal-value *xml-double*)))))))


(defun cast-literal-to-integer (literal-value)
  "A helper function that casts the passed string value of the literal
   value to an integer value."
  (handler-case (parse-integer literal-value)
    (condition ()
      (error (make-condition
	      'sparql-parser-error
	      :message (format nil "Could not cast from ~a to ~a"
			       literal-value *xml-integer*))))))
  

(defun cast-literal-to-boolean (literal-value)
  "A helper function that casts the passed string value of the literal
   value to t or nil."
  (when (and (string/= literal-value "false")
	     (string/= literal-value "true"))
    (error (make-condition
	    'sparql-parser-error
	    :message (format nil "Could not cast from ~a to ~a"
			     literal-value *xml-boolean*))))
  (if (string= literal-value "false")
      nil
      t))


(defmethod initialize-instance :after ((construct SPARQL-Query) &rest args)
  (declare (ignorable args))
  (parser-start construct (original-query construct))
  (dolist (triple (select-group construct))
    (set-results triple :revision (revision construct)))
  ;; filters all entries that are not important for the result
  ;; => an intersection is invoked
  (reduce-results construct (make-result-lists construct))
  (process-filters construct)
  construct)