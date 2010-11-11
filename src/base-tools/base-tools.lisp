;;+-----------------------------------------------------------------------------
;;+  Isidorus
;;+  (c) 2008-2010 Marc Kuester, Christoph Ludwig, Lukas Georgieff
;;+
;;+  Isidorus is freely distributable under the LLGPL license.
;;+  You can find a detailed description in trunk/docs/LLGPL-LICENSE.txt and
;;+  trunk/docs/LGPL-LICENSE.txt.
;;+-----------------------------------------------------------------------------

(defpackage :base-tools
  (:use :cl)
  (:nicknames :tools)
  (:export :push-string
	   :when-do
	   :remove-null
	   :full-path))

(in-package :base-tools)


(defmacro push-string (obj place)
  "Imitates the push macro but instead of pushing object in a list,
   there will be appended the given string to the main string object."
  `(setf ,place (concatenate 'string ,place ,obj)))


(defmacro when-do (result-bounding condition-statement do-with-result)
  "Executes the first statement and stores its result in the variable result.
   If result isn't nil the second statement is called.
   The second statement can use the variable tools:result as a parameter."
  `(let ((,result-bounding ,condition-statement))
     (if ,result-bounding
	 ,do-with-result
	 nil)))


(defun remove-null (lst)
  "Removes all null values from the passed list."
  (remove-if #'null lst))


(defun full-path (pathname)
  "Returns a string that represents the full path of the passed
   CL:Pathname construct."
  (declare (CL:Pathname pathname))
  (let ((segments
	 (remove-if #'null
		    (map 'list #'(lambda(item)
				   (when (stringp item)
				     (concatenate 'string "/" item)))
			 (pathname-directory pathname))))
	(full-path-string ""))
    (dolist (segment segments)
      (push-string segment full-path-string))
    (concatenate 'string full-path-string "/" (pathname-name pathname))))