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
	   :string-starts-with-char
	   :string-until
	   :string-after
	   :search-first))

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


(defun trim-whitespace-left (value)
  "Uses string-left-trim with a predefined character-list."
  (declare (String value))
  (string-left-trim '(#\Space #\Tab #\Newline) value))


(defun trim-whitespace-right (value)
  "Uses string-right-trim with a predefined character-list."
  (declare (String value))
  (string-right-trim '(#\Space #\Tab #\Newline) value))


(defun trim-whitespace (value)
  "Uses string-trim with a predefined character-list."
  (declare (String value))
  (string-trim '(#\Space #\Tab #\Newline) value))


(defun string-starts-with (str prefix)
  "Checks if string str starts with a given prefix."
  (declare (string str prefix))
  (string= str prefix :start1 0 :end1
           (min (length prefix)
                (length str))))


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


(defun search-first (search-strings main-string)
  "Returns the position of one of the search-strings. The returned position
   is the one closest to 0. If no search-string is found, nil is returned."
  (declare (String main-string)
	   (List search-strings))
  (let ((positions
	 (remove-null (map 'list #'(lambda(search-str)
				     (search search-str main-string))
			   search-strings))))
    (let ((sorted-positions (sort positions #'<)))
      (when sorted-positions
	(first sorted-positions)))))