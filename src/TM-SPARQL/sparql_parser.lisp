;;+-----------------------------------------------------------------------------
;;+  Isidorus
;;+  (c) 2008-2010 Marc Kuester, Christoph Ludwig, Lukas Georgieff
;;+
;;+  Isidorus is freely distributable under the LLGPL license.
;;+  You can find a detailed description in trunk/docs/LLGPL-LICENSE.txt and
;;+  trunk/docs/LGPL-LICENSE.txt.
;;+-----------------------------------------------------------------------------

(in-package :TM-SPARQL)


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


(defun cut-comment (query-string)
  "Returns the given string back. If the query starts with a # or
   space # the characters until the nextline are removed."
  (declare (String query-string))
  (let ((trimmed-str (trim-whitespace-left query-string)))
    (if (string-starts-with trimmed-str "#")
        (let ((next-query (string-after trimmed-str (string #\newline))))
	  (if next-query
	      next-query
	      ""))
	trimmed-str)))


(defgeneric parser-start(construct query-string)
  (:documentation "The entry point of the SPARQL-parser.")
  (:method ((construct SPARQL-Query) (query-string String))
    (let ((trimmed-query-string (cut-comment query-string)))
      (cond ((string-starts-with trimmed-query-string "SELECT")
	     (parse-select
	      construct (string-after trimmed-query-string "SELECT")))
	    ((string-starts-with trimmed-query-string "PREFIX")
	     (parse-prefixes
	      construct (string-after trimmed-query-string "PREFIX")))
	    ((string-starts-with trimmed-query-string "BASE")
	     (parse-base construct (string-after trimmed-query-string "BASE")
			 #'parser-start))
	    ((= (length trimmed-query-string) 0)
	     ;; If there is only a BASE and/or PREFIX statement return an
	     ;; query-object with the result nil
	     construct)
	    (t
	     (error (make-sparql-parser-condition
		     trimmed-query-string (original-query construct)
		     "SELECT, PREFIX or BASE")))))))


(defgeneric parse-select (construct query-string)
  (:documentation "The entry-point of the parsing of the select - where
                   statement.")
  (:method ((construct SPARQL-Query) (query-string String))
    (let* ((trimmed-str (cut-comment query-string))
	   (next-query (if (string-starts-with trimmed-str "WHERE")
			   trimmed-str
			   (parse-variables construct trimmed-str))))
      (unless (string-starts-with next-query "WHERE")
	(error (make-sparql-parser-condition
		next-query (original-query construct) "WHERE")))
      (let* ((tripples (string-after next-query "WHERE"))
	     (query-tail (parse-where construct tripples)))
	(or query-tail) ;TODO: process tail-of query, e.g. order by, ...
	construct))))


(defgeneric parse-where (construct query-string)
  (:documentation "The entry-point for the parsing of the WHERE statement.")
  (:method ((construct SPARQL-Query) (query-string String))
    (let ((trimmed-str (cut-comment query-string)))
      (unless (string-starts-with trimmed-str "{")
	(error (make-sparql-parser-condition trimmed-str
					     (original-query construct) "{")))
      (parse-group construct (subseq trimmed-str 1) nil))))


(defgeneric parse-group (construct query-string values)
  (:documentation "The entry-point for the parsing of a {} statement.")
  (:method ((construct SPARQL-Query) (query-string String) (values List))
    (let ((trimmed-str (cut-comment query-string)))
      (cond ((string-starts-with trimmed-str "BASE")
	     (parse-base construct (string-after trimmed-str "BASE")
			 #'parse-where))
	    ((string-starts-with trimmed-str "{")
	     (error (make-sparql-parser-condition
		     trimmed-str (original-query construct)
		     "FILTER, BASE, or tripple. Grouping is currently no implemented.")))
	    ((string-starts-with trimmed-str "FILTER")
	     nil) ;TODO: implement => save the filters and call
	          ;it after invoking parse-tripples
	    ((string-starts-with trimmed-str "OPTIONAL")
	     (error (make-sparql-parser-condition
		     trimmed-str (original-query construct)
		     "FILTER, BASE, or tripple. Grouping is currently no implemented.")))
	    ((string-starts-with trimmed-str "UNION")
	     (error (make-sparql-parser-condition
		     trimmed-str (original-query construct)
		     "FILTER, BASE, or tripple. Grouping is currently no implemented.")))
	    ((string-starts-with trimmed-str "}") ;ending of this group
	     (subseq trimmed-str 1))
	    (t
	     (parse-tripple construct trimmed-str values))))))


(defun parse-tripple-elem (query-string query-object &key (literal-allowed nil))
  "A helper function to parse a subject or predicate of an RDF tripple.
   Returns an entry of the form (:value (:value string :type <'VAR|'IRI|'LITERAL>)
   :next-query string)."
  (declare (String query-string)
	   (SPARQL-Query query-object)
	   (Boolean literal-allowed))
  (let ((trimmed-str (cut-comment query-string)))
    (cond ((string-starts-with trimmed-str "<")
	   (parse-base-suffix-pair trimmed-str query-object))
	  ((or (string-starts-with trimmed-str "?")
	       (string-starts-with trimmed-str "$"))
	   (let ((result (parse-variable-name trimmed-str query-object)))
	     (list :next-query (getf result :next-query)
		   :value (list :value (getf result :value)
				:type 'VAR))))
	  (t
	   (if (or (string-starts-with-digit trimmed-str)
		   (string-starts-with trimmed-str "\"")
		   (string-starts-with trimmed-str "true")
		   (string-starts-with trimmed-str "false")
		   (string-starts-with trimmed-str "'"))
	       (progn
		 (unless literal-allowed
		   (error (make-sparql-parser-condition
			   trimmed-str (original-query query-object)
			   "an IRI of the form prefix:suffix or <iri> but found a literal.")))
		 (parse-literal-elem trimmed-str query-object))
	       (parse-prefix-suffix-pair trimmed-str query-object))))))


(defun parse-literal-elem (query-string query-object)
  "A helper-function that returns a literal vaue of the form
   (:value (:value object :literal-type string :literal-lang
   string :type <'LITERAL>) :next-query string)."
  (declare (String query-string)
	   (SPARQL-Query query-object))
  (let* ((trimmed-str (cut-comment query-string))
	 (value-type-lang-query
	  (cond ((or (string-starts-with trimmed-str "\"")
		     (string-starts-with trimmed-str "'"))
		 (parse-literal-string-value trimmed-str query-object))
		((string-starts-with trimmed-str "true")
		 (list :value t :type *xml-boolean*
		       :next-query (subseq trimmed-str (length "true"))))
		((string-starts-with trimmed-str "false")
		 (list :value nil :type *xml-boolean*
		       :next-query (subseq trimmed-str (length "false"))))
		((string-starts-with-digit trimmed-str)
		 (parse-literal-number-value trimmed-str query-object)))))
    (list :next-query (getf value-type-lang-query :next-query)
	  :value (list :value (getf value-type-lang-query :value)
		       :literal-type (getf value-type-lang-query :value)
		       :type 'LITERAL))))


(defun parse-literal-string-value (query-string query-object)
  "A helper function that parses a string that is a literal.
   The return value is of the form
   (list :value object :type string :lang string :next-query string)."
  (declare (String query-string)
	   (SPARQL-Query query-object))
  (let* ((trimmed-str (cut-comment query-string))
	 (result-1 (separate-literal-value trimmed-str query-object))
	 (after-literal-value (getf result-1 :next-query))
	 (l-value (getf result-1 :literal))
	 (result-2 (separate-literal-lang-or-type
		    after-literal-value query-object))
	 (l-type (getf result-2 :type))
	 (l-lang (if (getf result-2 :lang)
		     (getf result-2 :lang)
		     *xml-string*))
	 (next-query (getf result-2 :next-query)))
    (list :next-query next-query :lang l-lang :type l-lang
	  :value (cast-literal l-value l-type query-object))))


(defun cast-literal (literal-value literal-type)
  "A helper function that casts the passed string value of the literal
   corresponding to the passed literal-type."
  (declare (String literal-value literal-type))
  (cond ((string= literal-type *xml-string*)
	 literal-value)
	((string= literal-type *xml-boolean*)
	 (when (or (string/= literal-value "false")
		   (string/= literal-value "true"))
	   (error (make-condition
		   'sparql-parser-error
		   :message (format nil "Could not cast from ~a to ~a"
				    literal-value literal-type))))
	 (if (string= literal-value "false")
	     nil
	     t))
	((string= literal-type *xml-integer*)
	 (handler-case (parse-integer literal-value)
	   (condition ()
	     (error (make-condition
		   'sparql-parser-error
		   :message (format nil "Could not cast from ~a to ~a"
				    literal-value literal-type))))))
	((or (string= literal-type *xml-decimal*) ;;both types are
	     (string= literal-type *xml-double*)) ;;handled the same way
	 (let ((value (read-from-string literal-value)))
	   (unless (numberp value)
	     (error (make-condition
		   'sparql-parser-error
		   :message (format nil "Could not cast from ~a to ~a"
				    literal-value literal-type))))
	   value))))


(defun separate-literal-lang-or-type (query-string query-object)
  "A helper function that returns (:next-query string :lang string
   :type string). Only one of :lang and :type can be set, the other
   element is set to nil. The query string must be the string direct
   after the closing literal bounding."
  (declare (String query-string)
	   (SPARQL-Query query-object))
  (let ((delimiters (list " ." ". " ";" "}" " " (string #\tab)
			  (string #\newline))))
    (cond ((string-starts-with query-string "@")
	   (let ((end-pos (search-first (append delimiters (list "."))
					(subseq query-string 1))))
	     (unless end-pos
	       (error (make-sparql-parser-condition
		       query-string (original-query query-object)
		       "'.', ';', '}', ' ', '\t', or '\n'")))
	     (list :next-query (subseq (subseq query-string 1) end-pos)
		   :lang (subseq (subseq query-string 1) 0 end-pos)
		   :type nil)))
	  ((string-starts-with query-string "^^")
	   (let ((end-pos (search-first delimiters (subseq query-string 2))))
	     (unless end-pos
	       (error (make-sparql-parser-condition
		       query-string (original-query query-object)
		       "'. ', ,' .', ';', '}', ' ', '\t', or '\n'")))
	     (let* ((type-str (subseq (subseq query-string 2) 0 end-pos))
		    (next-query (subseq (subseq query-string 2) end-pos))
		    (final-type (if (get-prefix query-object type-str)
				    (get-prefix query-object type-str)
				    type-str)))
	       (list :next-query next-query :type final-type :lang nil))))
	  (t
	   (list :next-query query-string :type nil :lang nil)))))


(defun separate-literal-value (query-string query-object)
  "A helper function that returns (:next-query string :literal string).
   The literal string contains the pure literal value."
  (declare (String query-string)
	   (SPARQL-Query query-object))
  (let* ((trimmed-str (cut-comment query-string))
	 (delimiter (cond ((string-starts-with trimmed-str "\"")
			   "\"")
			  ((string-starts-with trimmed-str "'''")
			   "'''")
			  ((string-starts-with trimmed-str "'")
			   "'")
			  (t
			   (error (make-sparql-parser-condition
				   trimmed-str (original-query query-object)
				   "a literal starting with ', ''', or \"")))))
      	 (literal-end (find-literal-end (subseq trimmed-str (length delimiter))
					delimiter 0)))
    (list :next-query (subseq trimmed-str (+ literal-end (length delimiter)))
	  :literal (subseq trimmed-str (length delimiter) literal-end))))


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
	    (+ overall-pos current-pos 1))
	nil)))


(defun parse-literal-number-value (query-string query-object)
  "A helper function that parses any number that is a literal.
   The return value is of the form
   (list :value nil :type string :pos int)."
  (declare (String query-string)
	   (SPARQL-Query query-object))
  (let* ((trimmed-str (cut-comment query-string))
	 (triple-delimiters
	  (list ". " ". " ";" " " (string #\tab)
		(string #\newline) "}"))
	 (end-pos (search-first triple-delimiters
				trimmed-str)))
    (unless end-pos
      (error (make-sparql-parser-condition
	      trimmed-str (original-query query-object)
	      "'. ', ' .', ';' ' ', '\\t', '\\n' or '}'")))
    (let* ((literal-number
	    (read-from-string (subseq trimmed-str 0 end-pos)))
	   (number-type
	    (if (search "." (subseq trimmed-str 0 end-pos))
		*xml-double* ;could also be an xml:decimal, since the doucble has
		             ;a bigger range it shouldn't matter
		*xml-integer*)))
      (unless (numberp literal-number)
	(error (make-sparql-parser-condition
		trimmed-str (original-query query-object)
		"a valid number of the form '1', '1.3', 1.0e6'")))
      (list :value literal-number :type number-type
	    :next-query (subseq trimmed-str end-pos)))))


(defun parse-base-suffix-pair (query-string query-object)
  "A helper function that returns a list of the form
   (list :next-query string :value (:value uri :type 'IRI))."
  (declare (String query-string)
	   (SPARQL-Query query-object))
  (let* ((trimmed-str (cut-comment query-string))
	 (result (parse-closed-value trimmed-str query-object))
	 (result-uri
	  (if (or (absolute-uri-p (getf result :value))
		  (not (base-value query-object)))
	      (getf result :value)
	      (concatenate-uri (base-value query-object)
			       (getf result :value)))))
    (list :next-query (getf result :next-query)
	  :value (list :value result-uri :type 'IRI))))


(defun parse-prefix-suffix-pair(query-string query-object)
  "A helper function that returns a list of the form
   (list :next-query string :value (:value uri :type 'IRI))."
  (declare (String query-string)
	   (SPARQL-Query query-object))
  (let* ((trimmed-str (cut-comment query-string))
	(delimiters (list "." ";" "}" "<" " " (string #\newline)
			  (string #\tab) "#"))
	 (end-pos (search-first delimiters trimmed-str))
	 (elem-str (when end-pos
		     (subseq trimmed-str 0 end-pos)))
	 (prefix (when elem-str
		   (string-until elem-str ":")))
	 (suffix (when prefix
		   (string-after elem-str ":"))))
    (unless (and end-pos prefix suffix)
      (error (make-sparql-parser-condition
	      trimmed-str (original-query query-object)
	      "An IRI of the form prefix:suffix")))
    (list :next-query (string-after
		       trimmed-str
		       (concatenate 'string prefix ":" suffix))
	  :value (list :value (concatenate 'string prefix ":" suffix)
		       :type 'IRI))))


(defgeneric parse-tripple (construct query-string values)
  (:documentation "Parses a tripple within a trippel group and returns a
                   a list of the form (:next-query :subject (:type <'VAR|'IRI>
                   :value string) :predicate (:type <'VAR|'IRI> :value string)
                   :object (:type <'VAR|'IRI|'LITERAL> :value string)).")
  (:method ((construct SPARQL-Query) (query-string String) (values List))
    (let* ((trimmed-str (cut-comment query-string))
	   (subject
	    (let ((result (parse-tripple-elem trimmed-str construct)))
	      (setf trimmed-str (getf result :next-query))
	      (getf result :value)))
	   (predicate
	    (let ((result (parse-tripple-elem trimmed-str construct)))
	      (setf trimmed-str (getf result :next-query))
	      (getf result :value)))
	   (object
	    (let ((result (parse-tripple-elem trimmed-str construct
					      :literal-allowed t)))
	      (setf trimmed-str (getf result :next-query))
	      (getf result :value))))
      (or subject object predicate);;TODO: implement
    ;; 0) ; => use last subject
    ;; 1) search for <url> => if full-url use it otherwise set bse
    ;; 2) search for label:suffix
    ;; 3) varname => ?|$
    ;; 4) literal => only the object

    ;; => BASE is also allowed
    ;; => ;-shortcut

    ;; <full-url>
    ;; <base-suffix>
    ;; label:pref-suffix
    ;; ?var
    ;; $var
    ;; "literal"
    ;; 'literal'
    ;; "literal"@language
    ;; "literal"^^type
    ;; '''"literal"'''
    ;; 1, which is the same as "1"^^xsd:integer
    ;; 1.3, which is the same as "1.3"^^xsd:decimal
    ;; 1.300, which is the same as "1.300"^^xsd:decimal
    ;; 1.0e6, which is the same as "1.0e6"^^xsd:double
    ;; true, which is the same as "true"^^xsd:boolean
    ;; false, which is the same as "false"^^xsd:boolean
      )))


(defgeneric parse-variables (construct query-string)
  (:documentation "Parses the variables of the SELECT statement
                   and adds them to the passed construct.")
  (:method ((construct SPARQL-Query) (query-string String))
    (let ((trimmed-str (cut-comment query-string)))
      (if (string-starts-with trimmed-str "WHERE")
	  trimmed-str
	  (if (string-starts-with trimmed-str "*")
	      (progn (add-variable construct "*" nil)
		     (parse-variables construct (string-after trimmed-str "*")))
	      (let ((result (parse-variable-name trimmed-str construct)))
		(add-variable construct (getf result :value) nil)
		(parse-variables construct (getf result :next-query))))))))


(defun parse-variable-name (query-string query-object)
  "A helper function that parses the first non-whitespace character
   in the query. since it must be a variable, it must be prefixed
   by a ? or $. The return value is of the form
   (:next-query string :value string)."
  (declare (String query-string)
	   (SPARQL-Query query-object))
  (let ((trimmed-str (cut-comment query-string))
	(delimiters (list " " "?" "$" "." (string #\newline) (string #\tab))))
    (unless (or (string-starts-with trimmed-str "?")
		(string-starts-with trimmed-str "$"))
      (error (make-sparql-parser-condition
	      trimmed-str (original-query query-object) "? or $")))
    (let* ((var-name-end (search-first delimiters (subseq trimmed-str 1)))
	   (var-name
	    (if var-name-end
		(subseq trimmed-str 0 (+ 1 var-name-end))
		(error (make-sparql-parser-condition
			trimmed-str (original-query query-object)
			"space, newline, tab, ?, ., $ or WHERE"))))
	   (next-query (string-after trimmed-str var-name))
	   (normalized-var-name 
	    (if (<= (length var-name) 1)
		(error (make-sparql-parser-condition
			next-query (original-query query-object)
			"a variable name"))
		(subseq var-name 1))))
      (list :next-query next-query :value normalized-var-name))))


(defgeneric parse-base (construct query-string next-fun)
  (:documentation "Parses the Base statment and sets the corresponding
                   attribute in the query-construct. Since the BASE statement
                   may appear in different states the next-fun defines the next
                   call function that calls the next transitions and states.")
  (:method ((construct SPARQL-Query) (query-string String) (next-fun Function))
    (let* ((trimmed-str (cut-comment query-string))
	   (result (parse-closed-value trimmed-str construct)))
      (setf (base-value construct) (getf result :value))
      (funcall next-fun construct (getf result :next-query)))))


(defgeneric parse-prefixes (construct query-string)
  (:documentation "Sets the correponding prefix-tuples in the passed object.")
  (:method ((construct SPARQL-Query) (query-string String))
    (let ((trimmed-string (cut-comment query-string)))
      (if (string-starts-with trimmed-string ":")
	  (let ((results
		 (parse-closed-value (subseq trimmed-string 1) construct)))
	    (add-prefix construct *empty-label* (getf results :value))
	    (parser-start construct (getf results :next-query)))
	  (let* ((label-name
		  (trim-whitespace-right (string-until trimmed-string ":")))
		 (next-query-str
		  (trim-whitespace-left (string-after trimmed-string ":")))
		 (results (parse-closed-value next-query-str construct)))
	    (when (string= label-name trimmed-string)
	      (error (make-sparql-parser-condition
		      trimmed-string (original-query construct) ":")))
	    (add-prefix construct label-name (getf results :value))
	    (parser-start construct (getf results :next-query)))))))


(defun parse-closed-value(query-string query-object &key (open "<") (close ">"))
  "A helper function that checks the value of a statement within
   two brackets, i.e. <prefix-value>. A list of the
   form (:next-query string :value string) is returned."
  (declare (String query-string open close)
	   (SPARQL-Query query-object))
  (let ((trimmed-string (cut-comment query-string)))
    (if (string-starts-with trimmed-string open)
	(let* ((pref-url (string-until (string-after trimmed-string open) close))
	       (next-query-str (string-after trimmed-string close)))
	  (unless next-query-str
	    (error (make-sparql-parser-condition
		    trimmed-string (original-query query-object)
		    close)))
	  (list :next-query next-query-str
		:value pref-url))
	(error (make-sparql-parser-condition
		trimmed-string (original-query query-object)
		close)))))