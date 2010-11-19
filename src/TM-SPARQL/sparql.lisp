;;+-----------------------------------------------------------------------------
;;+  Isidorus
;;+  (c) 2008-2010 Marc Kuester, Christoph Ludwig, Lukas Georgieff
;;+
;;+  Isidorus is freely distributable under the LLGPL license.
;;+  You can find a detailed description in trunk/docs/LLGPL-LICENSE.txt and
;;+  trunk/docs/LGPL-LICENSE.txt.
;;+-----------------------------------------------------------------------------

(defpackage :TM-SPARQL
  (:use :cl :datamodel :base-tools :exceptions)
  (:export :SPARQL-Query))


(in-package :TM-SPARQL)

(defvar *empty-label* "_empty_label_symbol")


(defclass SPARQL-Query ()
  ((original-query :initarg :query
		   :accessor original-query  ;this value is only for internal
					     ;purposes and mustn't be reset
		   :type String
		   :initform (error
			      (make-condition
			       'missing-query-string-error
			       :message "From TM-Query(): original-query must be set"))
		   :documentation "Containst the original received querry as string")
   (prefix-list :initarg :prefix-list
		:accessor prefix-list ;this value is only for internal purposes
				      ;purposes and mustn't be reset
		:type List
		:initform nil
		:documentation "A list of the form
                               ((:label 'id' :value 'prefix'))")
   (base-value :initarg :base-value ;initialy the requester's address
	       :accessor base-value ;this value is only for internal purposes
				    ;purposes and mustn't be reset
	       :type String
	       :initform nil
	       :documentation "Contains the last set base-value.")
   (variables :initarg :variables
	      :accessor variables ;this value is only for internal purposes
				  ;purposes and mustn't be reset
	      :type List
	      :documentation "A list of the form ((:variable var-symbol
                             :value value-object)), that contains tuples
                             for each variable and its result.")
   (select-statements :initarg :select-statements
		      :accessor select-statements ;this value is only for
					          ;internal purposes purposes
 					          ;and mustn't be reset
		      :type List
		      :documentation "A list of the form ((:statement 'statement'
                                      :value value-object))")))


(defgeneric add-prefix (construct prefix-label prefix-value)
  (:documentation "Adds the new prefix tuple to the list of all existing.
                   If there already exists a tuple with the same label
                   the label's value will be overwritten by the new value.")
  (:method ((construct SPARQL-Query) (prefix-label String) (prefix-value String))
    (let ((existing-tuple
	   (find-if #'(lambda(x)
			(string= (getf x :label) prefix-label))
		    (prefix-list construct))))
      (if existing-tuple
	  (setf (getf existing-tuple :value) prefix-value)
	  (push (list :label prefix-label :value prefix-value)
		(prefix-list construct))))))
		    


(defmethod initialize-instance :after ((construct SPARQL-Query) &rest args)
  (declare (ignorable args))
  (parser-start construct (original-query construct))
  construct)
