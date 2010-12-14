;;+-----------------------------------------------------------------------------
;;+  Isidorus
;;+  (c) 2008-2010 Marc Kuester, Christoph Ludwig, Lukas Georgieff
;;+
;;+  Isidorus is freely distributable under the LLGPL license.
;;+  You can find a detailed description in trunk/docs/LLGPL-LICENSE.txt and
;;+  trunk/docs/LGPL-LICENSE.txt.
;;+-----------------------------------------------------------------------------

(in-package :TM-SPARQL)


(defparameter *supported-functions*
  (list "BOUND" "isLITERAL" "STR" "DATATYPE" "REGEX")
  "Contains all supported SPARQL-functions")


(defparameter *supported-operators*
  (list "!" "||" "&&" "=" "!=" "<" "<=" ">" ">=" "+" "-" "*" "/")
  "Contains all supported operators, note some unary operators
   are handled as functions, e.g. + and -")


(defun make-sparql-parser-condition(rest-of-query entire-query expected)
  "Creates a spqrql-parser-error object."
  (declare (String rest-of-query entire-query expected))
  (let ((message
	 (format nil "The query:~%\"~a\"~%~%has a bad token at position ~a => ~a.~%Expected: ~a"
		 entire-query (- (length entire-query)
				 (length rest-of-query))
		 (subseq entire-query (- (length entire-query)
					 (length rest-of-query)))
		 expected)))
    (make-condition 'sparql-parser-error :message message)))


(defgeneric parse-filter (construct query-string)
  (:documentation "A helper functions that returns a filter and the next-query
                   string in the form (:next-query string :filter object).")
  (:method ((construct SPARQL-Query) (query-string String))
    (let* ((result-set-boundings (set-boundings construct query-string))
	   (filter-string (getf result-set-boundings :filter-string))
	   (next-query (getf result-set-boundings :next-query))
	   ))))
  ;;TODO: implement
  ;; **replace () by (progn )
  ;; **replace ', """, ''' by '''
  ;; *replace !x by (not x)
  ;; *replace +x by (1+ x)
  ;; *replace -x by (1- x)
  ;; *replace x operator y by (filter-operator x y)
  ;;   *=, !=, <, >, <=, >=, +, -, *, /, ||, &&
  ;; *replace function(x), function(x, y), function(x, y, z)
  ;;   by filter-function(x), (filter-function(x, y), filter-function(x, y, z)
  ;; *create and store this filter object


(defgeneric set-boundings (construct query-string)
  (:documentation "Returns a list of the form (:next-query <string>
                   :filter-string <string>). next-query is a string containing
                   the query after the filter and filter is a string
                   containing the actual filter. Additionally all free
                   '(' are transformed into '(progn' and all ', ''', \"\"\"
                   are transformed into \".")
  (:method ((construct SPARQL-Query) (query-string String))
    (let ((filter-string "")
	  (open-brackets 0)
	  (result nil))
      (dotimes (idx (length query-string))
	(let ((current-char (subseq query-string idx (1+ idx))))
	  (cond ((string= "(" current-char)
		 (setf open-brackets (1+ open-brackets))
		 (if (progn-p query-string idx)
		     (push-string "(progn " filter-string)
		     (push-string current-char filter-string)))
		((string= ")" current-char)
		 (setf open-brackets (1- open-brackets))
		 (when (< open-brackets 0)
		   (make-sparql-parser-condition
		    (subseq query-string idx)
		    (original-query construct)
		    "an opening bracket \"(\" is missing for the current closing one"))
		 (push-string current-char filter-string))
		((or (string= "'" current-char)
		     (string= "\"" current-char))
		 (let ((result (get-literal (subseq query-string idx))))
		   (unless result
		     (make-sparql-parser-condition
		      (subseq query-string idx)
		      (original-query construct)
		      "a closing character for the given literal"))
		   (setf idx (- (1- (length query-string))
				(length (getf result :next-query))))
		   (push-string (getf result :literal) filter-string)))
		((string= "#" current-char)
		 (let ((comment-string
			(string-until (subseq query-string idx)
				      (string #\newline))))
		   (setf idx (+ idx (length comment-string)))))
		((and (string= current-char (string #\newline))
		      (= 0 open-brackets))
		 (setf result
		       (list :next-query (subseq query-string idx)
			     :filter-string filter-string))
		 (setf idx (1- (length query-string))))
		((string= current-char "}")
		 (when (/= open-brackets 0)
		   (make-sparql-parser-condition
		    (subseq query-string idx)
		    (original-query construct)
		    "a valid filter, but the filter is not complete"))
		 (setf result
		       (list :next-query (subseq query-string idx)
			     :filter-string filter-string)))
		(t
		 (push-string current-char filter-string)))))
      result)))


(defun progn-p(query-string idx)
  "Returns t if the ( at position idx in the filter string
  represents a (progn) block."
  (declare (String query-string)
	   (Integer idx))
  (let* ((delimiters (append (list " " (string #\Space) (string #\Tab)
				   (string #\Newline) (string #\cr) "(" ")")
			     *supported-operators*))
	 (string-before (trim-whitespace-right (subseq query-string 0 idx)))
	 (fragment-before-idx
	  (search-first delimiters string-before :from-end t))
	 (fragment-before
	  (if (and (not fragment-before-idx)
		   (and (> (length string-before) 0)
			(not (find string-before *supported-functions*
				   :test #'string=))))
	      (error (make-condition
		      'SPARQL-PARSER-ERROR
		      :message (format nil "Invalid filter: \"~a\"~%"
				       query-string)))
	      (when fragment-before-idx
		(let ((inner-value
		       (subseq string-before fragment-before-idx)))
		  (if (and (> (length inner-value) 1)
			   (string-starts-with inner-value "("))
		      (subseq inner-value 1)
		      inner-value))))))
    (if fragment-before
	(progn
	  (when (or (string-starts-with fragment-before "?")
		    (string-starts-with fragment-before "$"))
	    (error
	     (make-condition
	      'SPARQL-PARSER-ERROR
	      :message (format nil "Invalid filter: found \"~a\" but expected ~a"
			       fragment-before *supported-functions*))))
	  (when (not (find fragment-before (append *supported-functions*
						   delimiters)
			   :test #'string=))
	    (error
	     (make-condition
	      'SPARQL-PARSER-ERROR
	      :message
	      (format nil "Invalid character: ~a, expected characters: ~a"
		      fragment-before (append *supported-functions* delimiters)))))
	  (if (find fragment-before *supported-functions* :test #'string=)
	      nil
	      t))
	(if (find string-before *supported-functions* :test #'string=)
	    nil
	    t))))


(defun get-literal (query-string)
  "Returns a list of the form (:next-query <string> :literal <string>
   where next-query is the query after the found literal and literal
   is the literal string."
  (declare (String query-string))
  (cond ((or (string-starts-with query-string "\"\"\"")
	     (string-starts-with query-string "'''"))
	 (let ((literal-end
		(find-literal-end (subseq query-string 3) (subseq query-string 0 3))))
	   (when literal-end
	     (list :next-query (subseq query-string (+ 3 literal-end))
		   :literal (concatenate 'string "'''"
					 (subseq query-string 3 literal-end)
					 "'''")))))
	((or (string-starts-with query-string "\"")
	     (string-starts-with query-string "'"))
	 (let ((literal-end
		(find-literal-end (subseq query-string 1)(subseq query-string 0 1))))
	   (when literal-end
	     (list :next-query (subseq query-string (+ 1 literal-end))
		   :literal (concatenate 'string "'''"
					 (subseq query-string 1 literal-end)
					 "'''")))))))


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