;;+-----------------------------------------------------------------------------
;;+  Isidorus
;;+  (c) 2008-2010 Marc Kuester, Christoph Ludwig, Lukas Georgieff
;;+
;;+  Isidorus is freely distributable under the LLGPL license.
;;+  You can find a detailed description in trunk/docs/LLGPL-LICENSE.txt and
;;+  trunk/docs/LGPL-LICENSE.txt.
;;+-----------------------------------------------------------------------------


;;This code has to be executed by several sblc instances in paralell!!!


(asdf:operate 'asdf:load-op :elephant)
(asdf:operate 'asdf:load-op :pathnames)
(asdf:operate 'asdf:load-op :uuid)
(use-package :elephant)
(use-package :com.gigamonkeys.pathnames)
(use-package :uuid)
(ensure-directories-exist #p"./test_data_base/")
(defvar *db-path* (truename #p"./test_data_base/"))


(defpclass TestClassC ()
  ((slot-1 :accessor slot-1
	   :initarg :slot-1
	   :initform ""
	   :type String
	   :index t)
   (slot-2 :accessor slot-2
	   :initarg :slot-2
	   :initform 0.0
	   :type Single-Float
	   :index t)
   (slot-3 :accessor slot-3
	   :initarg :slot-3
	   :initform nil
	   :type List
	   :index t)))


(defun make-object ()
  (with-transaction ()
    (make-instance 'TestClassC
		   :slot-1 (make-v1-uuid)
		   :slot-2 (read-from-string
			    (concatenate
			     'string (write-to-string (random 1000000000))
			     ".0"))
		   :slot-3 (loop for i to 100
			      collect (random 100000)))))


(defun do-writing-test()
  (elephant:open-store (list :BDB *db-path*) :register t)
  (dotimes (i 1000)
    (make-object)
    (format t "writing => i: ~a~%" i))
  (elephant:close-store))



(defun do-reading-test()
  (elephant:open-store (list :BDB *db-path*) :register t)
  (dotimes (i 1000)
    (elephant:get-instances-by-class 'TestclassC)
    (format t "reading => i: ~a~%" i))
  (elephant:close-store))

(do-writing-test)
(do-reading-test)