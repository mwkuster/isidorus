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

(defclass Variable-Container ()
  ((variables :initarg :variables
	      :accessor variables ;this value is only for internal purposes
				  ;purposes and mustn't be reset
	      :type List
	      :initform nil
	      :documentation "A list of the form ((:variable var-name
                             :value value-object)), that contains tuples
                             for each variable and its result."))
   (:documentation "This class is used to store all variable in a WHERE{}
                    statement"))


(defclass SPARQL-Query (Variable-Container)
  ((original-query :initarg :query
		   :accessor original-query  ;this value is only for internal
					     ;purposes and mustn't be reset
		   :type String
		   :initform (error
			      (make-condition
			       'missing-argument-error
			       :message "From TM-Query(): original-query must be set"))
		   :documentation "Containst the original received querry as string")
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
   (select-statements :initarg :select-statements
		      :accessor select-statements ;this value is only for
					          ;internal purposes purposes
 					          ;and mustn't be reset
		      :type List 
		      :initform nil
		      :documentation "A list of the form ((:statement 'statement'
                                      :value value-object))"))
  (:documentation "This class represents the entire request."))


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
  (:method ((construct Variable-Container) (variable-name String) variable-value)
    (let ((existing-tuple
	   (find-if #'(lambda(x)
			(string= (getf x :variable) variable-name))
		    (variables construct))))
      (if existing-tuple
	  (setf (getf existing-tuple :value) variable-value)
	  (push (list :variable variable-name :value variable-value)
		(variables construct))))))


(defmethod initialize-instance :after ((construct SPARQL-Query) &rest args)
  (declare (ignorable args))
  (parser-start construct (original-query construct))
  construct)