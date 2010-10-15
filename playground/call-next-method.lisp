;;+-----------------------------------------------------------------------------
;;+  Isidorus
;;+  (c) 2008-2010 Marc Kuester, Christoph Ludwig, Lukas Georgieff
;;+
;;+  Isidorus is freely distributable under the LLGPL license.
;;+  You can find a detailed description in trunk/docs/LLGPL-LICENSE.txt and
;;+  trunk/docs/LGPL-LICENSE.txt.
;;+-----------------------------------------------------------------------------


(defclass Class-1 ()
  ((value :initarg :value
	  :accessor value)))

(defmethod set-value :before ((inst Class-1) value)
  (format t ":before -> value is of type ~a~%" (type-of value)))

(defmethod set-value ((inst Class-1) value)
  (format t ": -> value is being set to ~a~%" value)
  (setf (slot-value inst 'value) value))

(defmethod set-value :after ((inst Class-1) value)
  (format t ":after -> value was set to ~a~%" value))

(defmethod set-value :around ((inst Class-1) value)
  (format t ":around -> ???~%")
  (call-next-method inst "123")) ;calls the :before method with the
                                 ;arguments inst and "123"
                                 ;if no arguments are passed the arguments
                                 ;of the :around method are passed

(defvar *inst* (make-instance 'Class-1))
(set-value *inst* "val")
;:around -> ??? 
;:before -> value is of type (SIMPLE-ARRAY CHARACTER (3))
;: -> value is being set to 123
;:after -> value was set to 123


(defclass Class-2 (Class-1)
  ())

(defmethod set-value ((inst Class-2) value)
  (call-next-method) ;calls set-value of Class-1
  (format t "(Class-2): -> value is being set to ~a~%" value)
  (setf (slot-value inst 'value) value))

(defvar *inst2* (make-instance 'Class-2))
(set-value *inst2* "val2")
;:around -> ??? 
;:before -> value is of type (SIMPLE-ARRAY CHARACTER (3))
;: -> value is being set to 123
;(Class-2): -> value is being set to 123
;:after -> value was set to 123