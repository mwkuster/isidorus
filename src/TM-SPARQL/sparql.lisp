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
  (:export :SPARQL-Query))


(in-package :TM-SPARQL)

(defvar *empty-label* "_empty_label_symbol")


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
   (literal-type :initarg :literal-type
		 :accessor literal-type
		 :type String
		 :initform nil
		 :documentation "Contains the datatype of the literal, e.g. xml:string"))
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
  ((original-query :initarg :query
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




;;TODO:
;;
;; find-triples (subject predicate object)
;; * var var var => return the entire graph (all subjects)
;; * var var object
;; * var predicate var
;; * var predicate object
;; * subject var var
;; * subject var object
;; * subject predicate var
;; * subject predicate object => return subject predicate object if true otherweise nil
;; handle special URIs => http://www.networkedplanet.com/ontopic/2009/11/making_topic_maps_sparql.html

(defgeneric set-result (construct)
  (:documentation "Calculates the result of a triple and set all the values in
                   the passed object.")
  (:method ((construct SPARQL-Triple))
    ;;TODO: implement
    construct))


(defgeneric find-subject-var-var (construct)
  (:documentation "Finds a triple corresponding to the subject and sets
                   both variables.")
  (:method ((construct SPARQL-Triple))

    ))



(defmethod initialize-instance :after ((construct SPARQL-Query) &rest args)
  (declare (ignorable args))
  (parser-start construct (original-query construct))
  construct)