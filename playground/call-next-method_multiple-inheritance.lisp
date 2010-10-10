(defclass CharacteristicC()
	   ((value :accessor value
		   :initarg :value
		   :type string)))

(defclass DatatypableC()
	   ((datatype :accessor datatype
		      :initarg :datatype
		      :type string)))

(defclass OccurrenceC (CharacteristicC DatatypableC)
	   ())

(defgeneric equivalent-construct (construct &rest args))

(defmethod equivalent-construct ((construct OccurrenceC) &rest args)
	   (format t "equivalent-construct --> OccurrenceC: ~a~%" args)
	   (call-next-method construct args))

(defmethod equivalent-construct ((construct CharacteristicC) &rest args)
	   (format t "equivalent-construct --> CharacteristicC: ~a~%" args)
	   (call-next-method construct (first args))
	   (string= (value construct) (getf (first args) :value)))

(defmethod equivalent-construct ((construct DatatypableC) &rest args)
	   (format t "equivalent-construct --> DatatypableC: ~a~%" args)
	   (string= (datatype construct) (getf (first args) :datatype)))

(defvar *occ* (make-instance 'Occurrencec :value "value" :datatype "datatype"))

(equivalent-construct *occ* :value "value" :datatype "datatype")
