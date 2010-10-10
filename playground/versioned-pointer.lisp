(asdf:operate 'asdf:load-op 'elephant)
(elephant:open-store '(:BDB "data_base"))
(defpclass Relation()
	   ((to-a :associate NodeA
		  :accessor to-a
		  :initarg :to-a)
	    (to-b :associate NodeB
		  :accessor to-b
		  :initarg :to-b)
	    (version :initarg :version
		     :accessor version
		     :type integer
		     :index t))
	   (:index t))
(defpclass NodeA()
	   ((relation-to-b :associate (Relation to-a)
			   :accessor relation-to-b
			   :initarg :relation-to-b))
	   (:index t))
(defpclass NodeB()
	   ((relation-to-a :associate (Relation to-b)
			   :accessor relation-to-a
			   :initarg :relation-to-a))
	   (:index t))
(defvar *rel* (make-instance 'Relation
				      :to-a (make-instance 'NodeA)
				      :to-b (make-instance 'NodeB)
				      :version 1))
