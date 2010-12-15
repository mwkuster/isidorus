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
	   :full-path
	   :trim-whitespace-left
	   :trim-whitespace-right
	   :trim-whitespace
	   :string-starts-with
	   :string-ends-with
	   :string-ends-with-one-of
	   :string-starts-with-char
	   :string-until
	   :string-after
	   :search-first
	   :concatenate-uri
	   :absolute-uri-p
	   :string-starts-with-digit
	   :string-after-number
	   :separate-leading-digits
	   :white-space))

(in-package :base-tools)


(defparameter *white-space*
  (list #\Space #\Tab #\Newline #\cr)
  "Contains all characters that are treated as white space.")


(defun white-space()
  "Returns a lit os string that represents a white space."
  (map 'list #'(lambda(char)
		 (string char))
       *white-space*))


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


(defun trim-whitespace-left (value)
  "Uses string-left-trim with a predefined character-list."
  (declare (String value))
  (string-left-trim *white-space* value))


(defun trim-whitespace-right (value)
  "Uses string-right-trim with a predefined character-list."
  (declare (String value))
  (string-right-trim *white-space* value))


(defun trim-whitespace (value)
  "Uses string-trim with a predefined character-list."
  (declare (String value))
  (string-trim *white-space* value))


(defun string-starts-with (str prefix &key (ignore-case nil))
  "Checks if string str starts with a given prefix."
  (declare (String str prefix)
	   (Boolean ignore-case))
  (let ((str-i (if ignore-case
		   (string-downcase str :start 0 :end (min (length str)
							   (length prefix)))
		   str))
	(prefix-i (if ignore-case
		      (string-downcase prefix)
		      prefix)))
    (string= str-i prefix-i :start1 0 :end1
	     (min (length prefix-i)
		  (length str-i)))))


(defun string-ends-with (str suffix &key (ignore-case nil))
  "Checks if string str ends with a given suffix."
  (declare (String str suffix)
	   (Boolean ignore-case))
  (let ((str-i (if ignore-case
		   (string-downcase str :start (max (- (length str)
						       (length suffix))
						    0)
				    :end (length str))
		   str))
	(suffix-i (if ignore-case
		      (string-downcase suffix)
		      suffix)))
    (string= str-i suffix-i :start1 (max (- (length str)
					    (length suffix))
					 0))))


(defun string-ends-with-one-of (str suffixes &key (ignore-case nil))
  "Returns t if str ends with one of the string contained in suffixes."
  (declare (String str)
	   (List suffixes)
	   (Boolean ignore-case))
  (loop for suffix in suffixes
     when (string-ends-with str suffix :ignore-case ignore-case)
     return t))


(defun string-starts-with-digit (str)
  "Checks whether the passed string starts with a digit."
  (declare (String str))
  (loop for item in (list 0 1 2 3 4 5 6 7 8 9)
     when (string-starts-with str (write-to-string item))
     return t))

(defun string-after-number (str)
  "If str starts with a digit, there is returned the first
   substring after a character that is a non-digit.
   If str does not start with a digit str is returned."
  (declare (String str))
  (if (and (string-starts-with-digit str)
	   (> (length str) 0))
      (string-after-number (subseq str 1))
      str))


(defun separate-leading-digits (str &optional digits)
  "If str starts with a number the number is returned."
  (declare (String str)
	   (type (or Null String) digits))
  (if (string-starts-with-digit str)
      (separate-leading-digits
       (subseq str 1) (concatenate 'string digits (subseq str 0 1)))
      digits))


(defun string-starts-with-char (begin str)
  (equal (char str 0) begin))


(defun string-until (str anchor)
  "Returns a substring until the position of the passed anchor."
  (declare (String str anchor))
  (let ((pos (search anchor str)))
    (if pos
	(subseq str 0 pos)
	str)))


(defun string-after (str prefix)
  "Returns the substring after the found prefix.
   If there is no substring equal to prefix nil is returned."
  (declare (String str prefix))
  (let ((pos (search prefix str)))
    (if pos
	(subseq str (+ pos (length prefix)))
	nil)))


(defun search-first (search-strings main-string &key from-end)
  "Returns the position of one of the search-strings. The returned position
   is the one closest to 0. If no search-string is found, nil is returned."
  (declare (String main-string)
	   (List search-strings)
	   (Boolean from-end))
  (let ((positions
	 (remove-null
	  (map 'list #'(lambda(search-str)
			 (search search-str main-string :from-end from-end))
	       search-strings))))
    (let ((sorted-positions (if from-end
				(sort positions #'>)
				(sort positions #'<))))
      (when sorted-positions
	(first sorted-positions)))))


(defun concatenate-uri (absolute-ns value)
  "Returns a string conctenated of the absolut namespace an the given value
   separated by either '#' or '/'."
  (declare (string absolute-ns value))
  (unless (and (> (length absolute-ns) 0)
	       (> (length value) 0))
    (error "From concatenate-uri(): absolute-ns and value must be of length > 0"))
  (unless (absolute-uri-p absolute-ns)
    (error "From concatenate-uri(): absolute-ns has to be an absolute URI: ~a" absolute-ns))
  (let ((last-char
	 (elt absolute-ns (- (length absolute-ns) 1)))
	(first-char
	 (elt value 0)))
    (let ((separator
	   (cond
	     ((or (eql first-char #\#)
		  (eql first-char #\/))
	      "")
	     ((or (eql last-char #\#)
		  (eql last-char #\/))
	      "")
	     (t
	      "/"))))
      (let ((prep-ns
	     (if (and (eql last-char first-char)
		      (or (eql last-char #\#)
			  (eql last-char #\/)))
		 (subseq absolute-ns 0 (- (length absolute-ns) 1))
		 (if (and (eql last-char #\#)
			  (find #\/ value))
		     (progn
		       (when (not (eql first-char #\/))
			 (setf separator "/"))
		       (subseq absolute-ns 0 (- (length absolute-ns) 1)))
		     absolute-ns))))
	(concatenate 'string prep-ns separator value)))))


(defun absolute-uri-p (uri)
  "Returns t if the passed uri is an absolute one. This
   is indicated by a ':' with no leadgin '/'."
  (when uri
    (let ((position-of-colon
	   (position #\: uri)))
      (declare (string uri))
      (and position-of-colon (> position-of-colon 0)
	   (not (find #\/ (subseq uri 0 position-of-colon)))))))