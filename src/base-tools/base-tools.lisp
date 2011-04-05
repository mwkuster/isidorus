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
	   :concat
	   :when-do
	   :string-replace
	   :remove-null
	   :full-path
	   :trim-whitespace-left
	   :trim-whitespace-right
	   :trim-whitespace
	   :string-starts-with
	   :string-ends-with
	   :string-ends-with-one-of
	   :string-starts-with-char
	   :string-starts-with-one-of
	   :string-until
	   :string-after
	   :search-first
	   :search-first-ignore-literals
	   :concatenate-uri
	   :absolute-uri-p
	   :string-starts-with-digit
	   :string-after-number
	   :separate-leading-digits
	   :white-space
	   :white-space-p
	   :escape-string
	   :search-first-unclosed-paranthesis 
	   :search-first-unopened-paranthesis
	   :in-literal-string-p
	   :find-literal-end
	   :get-literal-quotation
	   :get-literal
	   :return-if-starts-with))

(in-package :base-tools)


(defparameter *white-space*
  (list #\Space #\Tab #\Newline #\cr)
  "Contains all characters that are treated as white space.")


(defun white-space()
  "Returns a lit os string that represents a white space."
  (map 'list #'(lambda(char)
		 (string char))
       *white-space*))


(defmacro concat (&rest strings)
  `(concatenate 'string ,@strings))


(defmacro push-string (obj place)
  "Imitates the push macro but instead of pushing object in a list,
   there will be appended the given string to the main string object."
  `(setf ,place (concat ,place ,obj)))


(defmacro when-do (result-bounding condition-statement do-with-result)
  "Executes the first statement and stores its result in the variable result.
   If result isn't nil the second statement is called.
   The second statement can use the variable tools:result as a parameter."
  `(let ((,result-bounding ,condition-statement))
     (if ,result-bounding
	 ,do-with-result
	 nil)))


(defun white-space-p (str)
  "Returns t if the passed str contains only white space characters."
  (cond ((and (= (length str) 1)
	      (string-starts-with-one-of str (white-space)))
	 t)
	((string-starts-with-one-of str (white-space))
	 (white-space-p (subseq str 1)))
	(t
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
				     (concat "/" item)))
			 (pathname-directory pathname))))
	(full-path-string ""))
    (dolist (segment segments)
      (push-string segment full-path-string))
    (concat full-path-string "/" (pathname-name pathname))))


(defun trim-whitespace-left (value)
  "Uses string-left-trim with a predefined character-list."
  (declare (type (or Null String) value))
  (when value
    (string-left-trim *white-space* value)))


(defun trim-whitespace-right (value)
  "Uses string-right-trim with a predefined character-list."
  (declare (type (or Null String) value))
  (when value
    (string-right-trim *white-space* value)))


(defun trim-whitespace (value)
  "Uses string-trim with a predefined character-list."
  (declare (type (or Null String) value))
  (when value
    (string-trim *white-space* value)))


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


(defun string-starts-with-one-of (str prefixes &key (ignore-case nil))
  "Returns t if str ends with one of the string contained in suffixes."
  (declare (String str)
	   (List prefixes)
	   (Boolean ignore-case))
  (loop for prefix in prefixes
     when (string-starts-with str prefix :ignore-case ignore-case)
     return t))


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


(defun string-replace (main-string string-to-replace new-string)
  "Replaces every occurrence of string-to-replace by new-string
   in main-string."
  (declare (String main-string string-to-replace new-string))
  (if (string= string-to-replace new-string)
      main-string
      (let ((search-idx (search-first (list string-to-replace) main-string)))
	(if (not search-idx)
	    main-string
	    (let ((modified-string
		   (concat (subseq main-string 0 search-idx)
			   new-string
			   (subseq main-string
				   (+ search-idx (length string-to-replace))))))
	      (string-replace modified-string string-to-replace new-string))))))



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
       (subseq str 1) (concat digits (subseq str 0 1)))
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
	   (List search-strings))
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


(defun find-literal-end (query-string delimiter &optional (overall-pos 0))
  "Returns the end of the literal corresponding to the passed delimiter
   string. The query-string must start after the opening literal delimiter.
   The return value is an int that represents the start index of closing
   delimiter. delimiter must be either \", ', or '''.
   If the returns value is nil, there is no closing delimiter."
  (declare (String query-string delimiter)
	   (Integer overall-pos))
  (let ((current-pos (search delimiter query-string)))
    (if current-pos
	(if (string-ends-with (subseq query-string 0 current-pos) "\\")
	    (find-literal-end (subseq query-string (+ current-pos
						      (length delimiter)))
			      delimiter (+ overall-pos current-pos 1))
	    (+ overall-pos current-pos (length delimiter)))
	nil)))


(defun get-literal-quotation (str)
  "Returns ', ''', \" or \"\"\" when the string starts with a literal delimiter."
  (cond ((string-starts-with str "'''")
	 "'")
	((string-starts-with str "\"\"\"")
	 "\"\"\"")
	((string-starts-with str "'")
	 "'")
	((string-starts-with str "\"")
	 "\"")))


(defun get-literal (query-string &key (quotation nil))
  "Returns a list of the form (:next-string <string> :literal <string>
   where next-query is the query after the found literal and literal
   is the literal string."
  (declare (String query-string)
	   (type (or Null String) quotation))
  (let ((local-quotation quotation))
    (cond ((or (string-starts-with query-string "\"\"\"")
	       (string-starts-with query-string "'''"))
	   (unless local-quotation
	     (setf local-quotation (subseq query-string 0 3)))
	   (let ((literal-end
		  (find-literal-end (subseq query-string 3) (subseq query-string 0 3))))
	     (when literal-end
	       (list :next-string (subseq query-string (+ 3 literal-end))
		     :literal (concat quotation
				      (subseq query-string 3 literal-end)
				      quotation)))))
	  ((or (string-starts-with query-string "\"")
	       (string-starts-with query-string "'"))
	   (unless local-quotation
	     (setf local-quotation (subseq query-string 0 1)))
	   (let ((literal-end
		  (find-literal-end (subseq query-string 1)
				    (subseq query-string 0 1))))
	     (when literal-end
	       (let ((literal
		      (escape-string (subseq query-string 1 literal-end) "\"")))
		 (list :next-string (subseq query-string (+ 1 literal-end))
		       :literal (concat local-quotation literal
					local-quotation)))))))))


(defun search-first-ignore-literals (search-strings main-string &key from-end)
  (declare (String main-string)
	   (List search-strings)
	   (Boolean from-end))
  (let ((first-pos
	 (search-first search-strings main-string :from-end from-end)))
    (when first-pos
      (if (not (in-literal-string-p main-string first-pos))
	  first-pos
	  (let* ((literal-start
		  (search-first (list "\"" "'") (subseq main-string 0 first-pos)
				:from-end from-end))
		 (next-str
		  (if from-end
		      

		      (subseq main-string 0 literal-start)
		      
		      
		      (let* ((sub-str (subseq main-string literal-start))
			     (literal-result (get-literal sub-str)))
			(getf literal-result :next-string)))))
	    (let ((next-pos
		   (search-first-ignore-literals search-strings next-str
						 :from-end from-end)))
	      (when next-pos
		(+ (- (length main-string) (length next-str)) next-pos))))))))


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
	(concat prep-ns separator value)))))


(defun absolute-uri-p (uri)
  "Returns t if the passed uri is an absolute one. This
   is indicated by a ':' with no leadgin '/'."
  (when uri
    (let ((position-of-colon
	   (position #\: uri)))
      (declare (string uri))
      (and position-of-colon (> position-of-colon 0)
	   (not (find #\/ (subseq uri 0 position-of-colon)))))))


(defun escape-string (str char-to-escape)
  "Escapes every occurrence of char-to-escape in str, if it is
   not escaped."
  (declare (String str char-to-escape))
  (let ((result ""))
    (dotimes (idx (length str))
      (let ((current-char (subseq str idx (1+ idx)))
	    (previous-char (if (= idx 0) "" (subseq str (1- idx) idx))))
	(cond ((and (string= current-char char-to-escape)
		    (string/= previous-char "\\"))
	       (push-string "\\" result)
	       (push-string current-char result))
	      (t
	       (push-string current-char result)))))
    result))


(defun in-literal-string-p(filter-string pos)
  "Returns t if the passed pos is within a literal string value."
  (declare (String filter-string)
	   (Integer pos))
  (let ((result nil))
    (dotimes (idx (length filter-string) result)
      (let* ((current-str (subseq filter-string idx))
	     (delimiter (cond ((string-starts-with current-str "'''")
			       "'''")
			      ((string-starts-with current-str "'")
			       "'")
			      ((string-starts-with current-str "\"\"\"")
			       "\"\"\"")
			      ((string-starts-with current-str "\"")
			       "\""))))
	(when delimiter
	  (let* ((end-pos
		  (let ((result
			 (search-first (list delimiter) 
				       (subseq current-str (length delimiter)))))
		    (when result
		      (+ (length delimiter) result))))
		 (quoted-str (when end-pos
			       (subseq current-str (length delimiter) end-pos)))
		 (start-pos idx))
	    (incf idx (+ (* 2 (length delimiter)) (length quoted-str)))
	    (if (and (>= pos start-pos)
		     (<= pos (+ start-pos end-pos)))
		(progn
		  (setf result t)
		  (setf idx (length filter-string)))
		(incf idx (+ (* 2 (length delimiter)) (length quoted-str))))))))))


(defun search-first-unclosed-paranthesis (str &key (ignore-literals t))
  "Returns the idx of the first ( that is not closed, the search is
   started from the end of the string.
   If ignore-literals is set to t all paranthesis that are within
   \", \"\"\", ' and ''' are ignored."
  (declare (String str)
	   (Boolean ignore-literals))
  (let ((open-brackets 0)
	(result-idx nil))
    (do ((idx (1- (length str)))) ((< idx 0))
      (let ((current-char (subseq str idx (1+ idx))))
	(cond ((string= current-char ")")
	       (when (or (not ignore-literals)
			 (and ignore-literals
			      (not (in-literal-string-p str idx))))
		 (decf open-brackets)))
	      ((string= current-char "(")
	       (when (or (not ignore-literals)
			 (and ignore-literals
			      (not (in-literal-string-p str idx))))
		 (incf open-brackets)
		 (when (> open-brackets 0)
		   (setf result-idx idx)
		   (setf idx 0)))))
	(decf idx)))
    result-idx))


(defun search-first-unopened-paranthesis (str &key (ignore-literals t))
  "Returns the idx of the first paranthesis that is not opened in str.
   If ignore-literals is set to t all mparanthesis that are within
   \", \"\"\", ' and ''' are ignored."
  (declare (String str)
	   (Boolean ignore-literals))
  (let ((closed-brackets 0)
	(result-idx nil))
    (dotimes (idx (length str))
      (let ((current-char (subseq str idx (1+ idx))))
	(cond ((string= current-char "(")
	       (when (or (not ignore-literals)
			 (and ignore-literals
			      (not (in-literal-string-p str idx))))
		 (decf closed-brackets)
		 (setf result-idx nil)))
	      ((string= current-char ")")
	       (when (or (not ignore-literals)
			 (and ignore-literals
			      (not (in-literal-string-p str idx))))
		 (incf closed-brackets)
		 (when (> closed-brackets 0)
		   (setf result-idx idx)
		   (setf idx (length str))))))))
    result-idx))


(defun return-if-starts-with (str to-be-matched &key from-end ignore-case
			      ignore-leading-whitespace)
  "Returns the string that is contained in to-be-matched and that is the
   start of the string str."
  (declare (String str)
	   (List to-be-matched)
	   (Boolean from-end ignore-case ignore-leading-whitespace))
  (let ((cleaned-str (if ignore-leading-whitespace
			 (trim-whitespace-left str)
			 str)))
    (loop for try in to-be-matched
       when (if from-end
		(string-ends-with cleaned-str try :ignore-case ignore-case)
		(string-starts-with cleaned-str try :ignore-case ignore-case))
       return try)))