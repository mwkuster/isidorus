;;+-----------------------------------------------------------------------------
;;+  Isidorus
;;+  (c) 2008-2010 Marc Kuester, Christoph Ludwig, Lukas Georgieff
;;+
;;+  Isidorus is freely distributable under the LLGPL license.
;;+  You can find a detailed description in trunk/docs/LLGPL-LICENSE.txt and
;;+  trunk/docs/LGPL-LICENSE.txt.
;;+-----------------------------------------------------------------------------

(in-package :datamodel)

(defgeneric equalT (construct1 construct2)
  (:documentation "Test for full equality of two elephant objects"))

(defmethod equalT ((x TopicMapConstructC) (y TopicMapConstructC))
  (when (equal (type-of x) (type-of y))
    (loop for slot in (sb-pcl:class-slots (find-class (type-of x)))

       ;; does not compare the OID and the DBCONNECTION-SPEC-PST slot
       when(and (not (equal (symbol-name (sb-pcl:slot-definition-name slot)) "OID"))
		(not (equal (symbol-name (sb-pcl:slot-definition-name slot)) "DBCONNECTION-SPEC-PST")))

       ;; iterates through all lists in the 'TopicMapConstructC
       do (if (equal 'CONS (type-of (slot-value x (sb-pcl:slot-definition-name slot))))
	      (loop for item-x in (slot-value x (sb-pcl:slot-definition-name slot))
		 for item-y in (slot-value y (sb-pcl:slot-definition-name slot))
		 do (if (typep item-x 'TopicMapConstructC)
			(when (not (equalT item-x item-y))
			  (return-from equalT nil))
			(when (not (equal item-x item-y))
			  (return-from equalT nil))))

	      ;; (else - not 'CONS) atomic values
	      (if (typep (sb-pcl:slot-definition-name slot) 'TopicMapConstructC)
		  (when (not (equalT (slot-value x (sb-pcl:slot-definition-name slot))
				     (slot-value y (sb-pcl:slot-definition-name slot))))
			     (return-from equalT nil))
		  (when (not (equal (slot-value x (sb-pcl:slot-definition-name slot))
				    (slot-value y (sb-pcl:slot-definition-name slot))))
		    (return-from equalT nil))))
	 (return-from equalT t)))
  nil)