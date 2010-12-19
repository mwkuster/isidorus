;;+-----------------------------------------------------------------------------
;;+  Isidorus
;;+  (c) 2008-2010 Marc Kuester, Christoph Ludwig, Lukas Georgieff
;;+
;;+  Isidorus is freely distributable under the LLGPL license.
;;+  You can find a detailed description in trunk/docs/LLGPL-LICENSE.txt and
;;+  trunk/docs/LGPL-LICENSE.txt.
;;+-----------------------------------------------------------------------------


(defpackage :filter-functions
  (:use :base-tools :constants :tm-sparql))


(defun filter-functions::not(x)
  (not x))


(defun filter-functions::one+(x)
  (1+ x))


(defun filter-functions::one-(x)
  (1- x))


(defun filter-functions::+(x y)
  (+ x y))


(defun filter-functions::-(x y)
  (- x y))


(defun filter-functions::*(x y)
  (* x y))


(defun filter-functions::/(x y)
  (/ x y))


(defun filter-functions::or(x y)
  (or x y))


(defun filter-functions::and(x y)
  (and x y))


(defun filter-functions::=(x y)
  (cond ((and (stringp x) (stringp y))
	 (string= x y))
	((and (numberp x)( numberp y))
	 (= x y))
	(t
	 (eql x y))))


(defun filter-functions::!=(x y)
  (filter-functions::not
   (filter-functions::= x y)))


(defun filter-functions::<(x y)
  (cond ((and (numberp x) (numberp y))
	 (< x y))
	((and (stringp x) (stringp y))
	 (string< x y))
	((and (typep x 'Boolean) (typep y 'Boolean))
	 (and (not x) y))
	(t
	 nil)))


(defun filter-functions::>(x y)
  (filter-functions::not
   (filter-functions::< x y)))


(defun filter-functions::<=(x y)
  (filter-functions::or
   (filter-functions::< x y)
   (filter-functions::= x y)))


(defun filter-functions::>=(x y)
  (filter-functions::or
   (filter-functions::> x y)
   (filter-functions::= x y)))
	   

(defun filter-functions::regex(str pattern &optional flags)
  (declare (Ignorable flags))
  (let* ((case-insensitive (when (find #\i flags) t))
	 (multi-line (when (find #\m flags) t))
	 (single-line (when (find #\s flags) t))
      	 (local-pattern
	  (if (find #\x flags)
	      (base-tools:string-replace
	       (base-tools:string-replace
		(base-tools:string-replace
		 (base-tools:string-replace pattern (string #\newline) "")
		 (string #\tab) "") (string #\cr) "") " " "")
	      pattern))
	 (scanner
	  (ppcre:create-scanner local-pattern
				:case-insensitive-mode case-insensitive
				:multi-line-mode multi-line
				:single-line-mode single-line)))
    (ppcre:scan scanner str)))


(defun filter-functions::bound(x)
  (boundp x))


(defun filter-functions::isLITERAL(x)
  (or (numberp x)
      (not (and (base-tools:string-starts-with x "<")
		(base-tools:string-ends-with x ">")
		(base-tools:absolute-uri-p x)))))


(defun filter-functions::datatype(x)
  (let ((type-suffix
	 (when (and (stringp x)
		    (or (base-tools:string-starts-with x "'")
			(base-tools:string-starts-with x "\"")))
	   (let* ((result (base-tools:get-literal x))
		  (literal-datatype
		   (when (base-tools:string-starts-with
			  (getf result :next-string) "^^")
		     (subseq (getf result :next-string) 2))))
	     literal-datatype))))
    (cond (type-suffix type-suffix)
	  ((integerp x) constants::*xml-integer*)
	  ((floatp x) constants::*xml-decimal*)
	  ((numberp x) constants::*xml-double*)
	  ((stringp x) constants::*xml-string*)
	  (t (type-of x)))))


(defun filter-functions::str(x)
  (if (stringp x)
      (if (and (base-tools:string-starts-with x "<")
	       (base-tools:string-ends-with x ">")
	       (base-tools:absolute-uri-p (subseq x 1 (1- (length x)))))
	  (subseq x 1 (1- (length x)))
	  x)
      (write-to-string x)))