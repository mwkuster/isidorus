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

(defparameter *supported-brackets*
  (list "(" ")")
  "Contains all supported brackets in a list of strings.")


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
	   (filter-string-unary-ops (set-unary-operators construct filter-string))
	   ))))
  ;;TODO: implement
  ;; *replace #comment => in set boundings
  ;; **replace () by (progn )
  ;; **replace ', """, ''' by '''
  ;; **replace !x by (not x)
  ;; **replace +x by (1+ x)
  ;; **replace -x by (1- x)
  ;; *replace x operator y by (filter-operator x y)
  ;;   *=, !=, <, >, <=, >=, +, -, *, /, ||, &&
  ;; *replace function(x), function(x, y), function(x, y, z)
  ;;   by filter-function(x), (filter-function(x, y), filter-function(x, y, z)
  ;; check if all functions that will e invoked are allowed
  ;; *create and store this filter object


(defgeneric set-unary-operators (construct filter-string)
  (:documentation "Transforms the unary operators !, +, - to (not ),
                   (1+ ) and (1- ). The return value is a modified filter
                   string.")
  (:method ((construct SPARQL-Query) (filter-string String))
    (let ((result-string ""))
      (dotimes (idx (length filter-string))
	(let ((current-char (subseq filter-string idx (1+ idx))))
	  (cond ((string= current-char "!")
		 (if (and (< idx (1- (length filter-string)))
			  (string= (subseq filter-string (1+ idx) (+ 2 idx)) "="))
		     (push-string current-char result-string)
		     (let ((result (unary-operator-scope filter-string idx)))
		       (push-string "(not " result-string)
		       (push-string (set-unary-operators construct (getf result :scope))
				    result-string)
		       (push-string ")" result-string)
		       (setf idx (- (1- (length filter-string))
				    (length (getf result :next-query)))))))
		((or (string= current-char "-")
		     (string= current-char "+"))
		 (let ((string-before
			(trim-whitespace-right (subseq filter-string 0 idx))))
		   (if (or (string= string-before "")
			   (string-ends-with string-before "(progn")
			   (string-ends-with-one-of string-before
						    *supported-operators*))
		       (let ((result (unary-operator-scope filter-string idx)))
			 (push-string (concatenate 'string "(1" current-char " ")
				      result-string)
			 (push-string (set-unary-operators construct
							   (getf result :scope))
				      result-string)
			 (push-string ")" result-string)
			 (setf idx (- (1- (length filter-string))
				      (length (getf result :next-query)))))
		       (push-string current-char result-string))))
		(t
		 (push-string current-char result-string)))))
      result-string)))


(defun unary-operator-scope (filter-string idx)
  "Returns a list of the form (:next-query <string> :scope <string>).
   scope contains the statement that is in the scope of one of the following
   operators !, +, -."
  (declare (String filter-string)
	   (Integer idx))
  (let* ((string-after (subseq filter-string (1+ idx)))
	 (cleaned-str (cut-comment string-after)))
    (cond ((string-starts-with cleaned-str "(")
	   (let ((result (bracket-scope cleaned-str)))
	     (list :next-query (string-after cleaned-str result)
		   :scope result)))
	  ((or (string-starts-with cleaned-str "?")
	       (string-starts-with cleaned-str "$"))
	   (let ((result (get-filter-variable cleaned-str)))
	     (list :next-query (string-after cleaned-str result)
		   :scope result)))
	  ((string-starts-with cleaned-str "'''")
	   (let ((result (get-literal cleaned-str)))
	     (list :next-query (getf result :next-query)
		   :scope (getf result :literal))))
	  ((string-starts-with-digit cleaned-str)
	   (let ((result (separate-leading-digits cleaned-str)))
	     (list :next-query (string-after cleaned-str result)
		   :scope result)))
	  ((string-starts-with cleaned-str "true")
	   (list :next-query (string-after cleaned-str "true")
		 :scope "true"))
	  ((string-starts-with cleaned-str "false")
	   (list :next-query (string-after cleaned-str "false")
		 :scope "false"))
	  ((let ((pos (search-first *supported-functions* cleaned-str)))
	     (when pos
	       (= pos 0)))
	   (let ((result (function-scope cleaned-str)))
	     (list :next-query (string-after cleaned-str result)
		   :scope result)))
	  (t
	   (error
	    (make-condition
	     'sparql-parser-error
	     :message
	     (format
	      nil "Invalid filter: \"~a\". An unary operator must be followed by ~a"
	      filter-string
	      "a number, boolean, string, function or a variable")))))))


(defun function-scope (str)
  "If str starts with a supported function it there is given the entire substr
   that is the scope of the function, i.e. the function name and all its
   variable including the closing )."
  (declare (String str))
  (let* ((cleaned-str (cut-comment str))
	 (after-fun
	  (remove-null (map 'list #'(lambda(fun)
				      (when (string-starts-with cleaned-str fun)
					(string-after str fun)))
			    *supported-functions*)))
	 (fun-suffix (when after-fun
		       (cut-comment (first after-fun)))))
    (when fun-suffix
      (let* ((args (bracket-scope fun-suffix))
	     (fun-name (string-until cleaned-str args)))
	(concatenate 'string fun-name args)))))


(defun get-filter-variable (str)
  "Returns the substring of str if str starts with ? or $ until the variable ends,
   otherwise the return value is nil."
  (declare (String str))
  (when (or (string-starts-with str "?")
	    (string-starts-with str "$"))
    (let ((found-end (search-first (append (white-space) *supported-operators*
					   *supported-brackets* (list "?" "$"))
				   (subseq str 1))))
      (if found-end
	  (subseq str 0 (1+ found-end))
	  str))))


(defun bracket-scope (str &key (open-bracket "(") (close-bracket ")"))
  "If str starts with open-bracket there will be returned the substring until
   the matching close-bracket is found. Otherwise the return value is nil."
  (declare (String str open-bracket close-bracket))
  (when (string-starts-with str open-bracket)
    (let ((open-brackets 0)
	  (result ""))
      (dotimes (idx (length str))
	(let ((current-char (subseq str idx (1+ idx))))
	  (cond ((or (string= "'" current-char)
		     (string= "\"" current-char))
		 (let* ((sub-str (subseq str idx))
			(quotation
			 (cond ((string-starts-with sub-str "'''")
				"'''")
			       ((string-starts-with sub-str "\"\"\"")
				"\"\"\"")
			       ((string-starts-with sub-str "'")
				"'")
			       ((string-starts-with sub-str "\"")
				"\"")))
			(literal
			 (get-literal (subseq str idx) :quotation quotation)))
		   (if literal
		       (progn
			 (setf idx (- (1- (length str))
				      (length (getf literal :next-query))))
			 (push-string (getf literal :literal) str))
		       (progn
			 (setf result nil)
			 (setf idx (length str))))))
		((string= current-char close-bracket)
		 (decf open-brackets)
		 (push-string current-char result)
		 (when (= open-brackets 0)
		   (setf idx (length str))))
		((string= current-char open-bracket)
		 (incf open-brackets)
		 (push-string current-char result))
		(t
		 (push-string current-char result)))))
      result)))


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
		   (error
		    (make-sparql-parser-condition
		     (subseq query-string idx)
		     (original-query construct)
		     "an opening bracket \"(\" is missing for the current closing one")))
		 (push-string current-char filter-string))
		((or (string= "'" current-char)
		     (string= "\"" current-char))
		 (let ((result (get-literal (subseq query-string idx))))
		   (unless result
		     (error (make-sparql-parser-condition
			     (subseq query-string idx)
			     (original-query construct)
			     "a closing character for the given literal")))
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
		   (error (make-sparql-parser-condition
			   (subseq query-string idx)
			   (original-query construct)
			   "a valid filter, but the filter is not complete")))
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
	      (if fragment-before-idx
		  (subseq string-before fragment-before-idx)
		  nil))))
    (when fragment-before
      (mapcan #'(lambda(operator)
		  (when (and (string-starts-with fragment-before operator)
			     (> (length fragment-before) (length operator)))
		    (setf fragment-before
			  (string-after fragment-before operator))))
	      (append *supported-operators* *supported-brackets*)))
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
	      (format nil "Invalid character: \"~a\", expected characters: ~a"
		      fragment-before (append *supported-functions* delimiters)))))
	  (if (find fragment-before *supported-functions* :test #'string=)
	      nil
	      t))
	(if (find string-before *supported-functions* :test #'string=)
	    nil
	    t))))


(defun get-literal (query-string &key (quotation "'''"))
  "Returns a list of the form (:next-query <string> :literal <string>
   where next-query is the query after the found literal and literal
   is the literal string."
  (declare (String query-string)
	   (String quotation))
  (cond ((or (string-starts-with query-string "\"\"\"")
	     (string-starts-with query-string "'''"))
	 (let ((literal-end
		(find-literal-end (subseq query-string 3) (subseq query-string 0 3))))
	   (when literal-end
	     (list :next-query (subseq query-string (+ 3 literal-end))
		   :literal (concatenate 'string quotation
					 (subseq query-string 3 literal-end)
					 quotation)))))
	((or (string-starts-with query-string "\"")
	     (string-starts-with query-string "'"))
	 (let ((literal-end
		(find-literal-end (subseq query-string 1)(subseq query-string 0 1))))
	   (when literal-end
	     (list :next-query (subseq query-string (+ 1 literal-end))
		   :literal (concatenate 'string quotation
					 (subseq query-string 1 literal-end)
					 quotation)))))))


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