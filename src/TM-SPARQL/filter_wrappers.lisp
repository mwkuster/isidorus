;;+-----------------------------------------------------------------------------
;;+  Isidorus
;;+  (c) 2008-2010 Marc Kuester, Christoph Ludwig, Lukas Georgieff
;;+
;;+  Isidorus is freely distributable under the LLGPL license.
;;+  You can find a detailed description in trunk/docs/LLGPL-LICENSE.txt and
;;+  trunk/docs/LGPL-LICENSE.txt.
;;+-----------------------------------------------------------------------------


(defpackage :filter-functions
  (:use :base-tools :constants :tm-sparql)
  (:import-from :cl progn handler-case let))


(defun filter-functions::normalize-value (value)
  "Returns the normalized value, i.e. if a literal
   is passed as '12'^^xsd:integer 12 is returned."
  (cond ((not (stringp value))
	 value)
	((or (base-tools:string-starts-with value "'")
	     (base-tools:string-starts-with value "\""))
	 (let* ((literal-result (tm-sparql::get-literal value))
		(literal-value
		 (cond ((or (base-tools:string-starts-with
			     (getf literal-result :literal) "\"\"\"")
			    (base-tools:string-starts-with
			     (getf literal-result :literal) "'''"))
			(subseq (getf literal-result :literal) 3
				(- (length (getf literal-result :literal)) 3)))
		       (t
			(subseq (getf literal-result :literal) 1
				(- (length (getf literal-result :literal)) 1)))))
		(given-datatype
		 (when (base-tools:string-starts-with
			(getf literal-result :next-string) "^^")
		   (subseq (getf literal-result :next-string) 2))))
	   (tm-sparql::cast-literal literal-value given-datatype)))
	(t
	 value)))


(defun filter-functions::not(x)
  (not (filter-functions::normalize-value x)))


(defun filter-functions::one+(x)
  (1+ (filter-functions::normalize-value x)))


(defun filter-functions::one-(x)
  (1- (filter-functions::normalize-value x)))


(defun filter-functions::+(x y)
  (+ (filter-functions::normalize-value x)
     (filter-functions::normalize-value y)))


(defun filter-functions::-(x y)
  (- (filter-functions::normalize-value x)
     (filter-functions::normalize-value y)))


(defun filter-functions::*(x y)
  (* (filter-functions::normalize-value x)
     (filter-functions::normalize-value y)))


(defun filter-functions::/(x y)
  (/ (filter-functions::normalize-value x)
     (filter-functions::normalize-value y)))


(defun filter-functions::or(x y)
  (or (filter-functions::normalize-value x)
      (filter-functions::normalize-value y)))


(defun filter-functions::and(x y)
  (and (filter-functions::normalize-value x)
       (filter-functions::normalize-value y)))


(defun filter-functions::=(x y)
  (let ((local-x (filter-functions::normalize-value x))
	(local-y (filter-functions::normalize-value y)))
    (cond ((and (stringp local-x) (stringp local-y))
	   (string= local-x local-y))
	  ((and (numberp local-x)( numberp local-y))
	   (= local-x local-y))
	  (t
	   (eql local-x local-y)))))


(defun filter-functions::!=(x y)
  (filter-functions::not
   (filter-functions::= x y)))


(defun filter-functions::<(x y)
  (let ((local-x (filter-functions::normalize-value x))
	(local-y (filter-functions::normalize-value y)))
    (cond ((and (numberp local-x) (numberp local-y))
	   (< local-x local-y))
	  ((and (stringp local-x) (stringp local-y))
	   (string< local-x local-y))
	  ((and (typep local-x 'Boolean) (typep local-y 'Boolean))
	   (and (not local-x) local-y))
	  (t
	   nil))))


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
  (let* ((local-flags (filter-functions::normalize-value flags))
	 (case-insensitive (when (find #\i local-flags) t))
	 (multi-line (when (find #\m local-flags) t))
	 (single-line (when (find #\s local-flags) t))
      	 (local-pattern
	  (if (find #\x local-flags)
	      (base-tools:string-replace
	       (base-tools:string-replace
		(base-tools:string-replace
		 (base-tools:string-replace
		  (filter-functions::normalize-value pattern)
		  (string #\newline) "")
		 (string #\tab) "") (string #\cr) "") " " "")
	      (filter-functions::normalize-value pattern)))
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