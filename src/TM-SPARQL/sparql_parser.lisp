;;+-----------------------------------------------------------------------------
;;+  Isidorus
;;+  (c) 2008-2010 Marc Kuester, Christoph Ludwig, Lukas Georgieff
;;+
;;+  Isidorus is freely distributable under the LLGPL license.
;;+  You can find a detailed description in trunk/docs/LLGPL-LICENSE.txt and
;;+  trunk/docs/LGPL-LICENSE.txt.
;;+-----------------------------------------------------------------------------

(in-package :TM-SPARQL)

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
	     ;; If there is only a BASE and/or PREFIX statement return a
	     ;; query-object with the result nil
	     construct)
	    (t
	     (error (make-sparql-parser-condition
		     trimmed-query-string (original-query construct)
		     (format nil "SELECT, PREFIX or BASE, but found: ~a..."
			     (subseq trimmed-query-string 0 10)))))))))


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
      (let* ((triples (string-after next-query "WHERE"))
	     (query-tail (parse-where construct triples)))
	(when (> (length query-tail) 0)
	  (error (make-sparql-parser-condition
		  query-tail (original-query construct)
		  "The end of the query. Solution sequence modifiers are not supported yet.")))
	construct))))


(defgeneric parse-where (construct query-string)
  (:documentation "The entry-point for the parsing of the WHERE statement.")
  (:method ((construct SPARQL-Query) (query-string String))
    (let ((trimmed-str (cut-comment query-string)))
      (unless (string-starts-with trimmed-str "{")
	(error (make-sparql-parser-condition trimmed-str
					     (original-query construct) "{")))
      (let ((query-tail (parse-group construct (subseq trimmed-str 1))))
	(when (> (length (trim-whitespace query-tail)) 0)
	  (make-sparql-parser-condition
	   query-tail (original-query construct) "end of query, solution sequences and modifiers are not supported"))
	query-tail))))


(defgeneric parse-group (construct query-string &key last-subject)
  (:documentation "The entry-point for the parsing of a {} statement.")
  (:method ((construct SPARQL-Query) (query-string String)
	    &key (last-subject nil))
    (declare (type (or Null SPARQL-Triple-Elem) last-subject))
    (let ((trimmed-str (cut-comment query-string)))
      (cond ((string-starts-with trimmed-str "BASE")
	     (parse-base construct (string-after trimmed-str "BASE")
			 #'(lambda(constr query-str)
			     (parse-group constr query-str
					  :last-subject last-subject))))
	    ((string-starts-with trimmed-str "{")
	     (error (make-sparql-parser-condition
		     trimmed-str (original-query construct)
		     "FILTER, BASE, or triple. Grouping is currently no implemented.")))
	    ((string-starts-with trimmed-str "FILTER")
	     (parse-filter construct (string-after trimmed-str "FILTER")))
	    ((string-starts-with trimmed-str "OPTIONAL")
	     (error (make-sparql-parser-condition
		     trimmed-str (original-query construct)
		     "FILTER, BASE, or triple. Grouping is currently no implemented.")))
	    ((string-starts-with trimmed-str "UNION")
	     (error (make-sparql-parser-condition
		     trimmed-str (original-query construct)
		     "FILTER, BASE, or triple. Grouping is currently no implemented.")))
	    ((string-starts-with trimmed-str "}") ;ending of this group
	     (subseq trimmed-str 1))
	    (t
	     (parse-triple construct trimmed-str :last-subject last-subject))))))


(defgeneric parse-triple-elem (construct query-string &key literal-allowed)
  (:documentation "A helper function to parse a subject or predicate of an RDF triple.")
  (:method ((construct SPARQL-Query) (query-string String)
	    &key (literal-allowed nil))
    (declare (Boolean literal-allowed))
    (let ((trimmed-str (cut-comment query-string)))
      (cond ((string-starts-with trimmed-str "a ") ;;rdf:type
	     (list :next-query (cut-comment (subseq trimmed-str 1))
		   :value (make-instance 'SPARQL-Triple-Elem
					 :elem-type 'IRI
					 :value *type-psi*)))
	    ((string-starts-with trimmed-str "<")
	     (parse-base-suffix-pair construct trimmed-str))
	    ((or (string-starts-with trimmed-str "?")
		 (string-starts-with trimmed-str "$"))
	     (let ((result
		    (parse-variable-name construct trimmed-str
					 :additional-delimiters (list "}"))))
	       (list :next-query (cut-comment (getf result :next-query))
		     :value (make-instance 'SPARQL-Triple-Elem
					   :elem-type 'VARIABLE
					   :value (getf result :value)))))
	    (t
	     (if (or (string-starts-with-digit trimmed-str)
		     (string-starts-with trimmed-str "\"")
		     (string-starts-with trimmed-str "true")
		     (string-starts-with trimmed-str "false")
		     (string-starts-with trimmed-str "'"))
		 (progn
		   (unless literal-allowed
		     (error (make-sparql-parser-condition
			     trimmed-str (original-query construct)
			     "an IRI of the form prefix:suffix or <iri> but found a literal.")))
		   (parse-literal-elem construct trimmed-str))
		 (parse-prefix-suffix-pair construct trimmed-str)))))))


(defgeneric parse-literal-elem (construct query-string)
  (:documentation "A helper-function that returns a literal vaue of the form
                   (:value (:value object :literal-type string :literal-lang
                   string :type <'LITERAL>) :next-query string).")
  (:method ((construct SPARQL-Query) (query-string String))
    (let* ((trimmed-str (cut-comment query-string))
	   (value-type-lang-query
	    (cond ((or (string-starts-with trimmed-str "\"")
		       (string-starts-with trimmed-str "'"))
		   (parse-literal-string-value construct trimmed-str))
		  ((string-starts-with trimmed-str "true")
		   (list :value t :type *xml-boolean*
			 :next-query (subseq trimmed-str (length "true"))))
		  ((string-starts-with trimmed-str "false")
		   (list :value nil :type *xml-boolean*
			 :next-query (subseq trimmed-str (length "false"))))
		  ((string-starts-with-digit trimmed-str)
		   (parse-literal-number-value construct trimmed-str)))))
      (list :next-query (getf value-type-lang-query :next-query)
	    :value (make-instance
		    'SPARQL-Triple-Elem
		    :elem-type 'LITERAL
		    :value (getf value-type-lang-query :value)
		    :literal-lang (getf value-type-lang-query :lang)
		    :literal-datatype (getf value-type-lang-query :type))))))
  
  
(defgeneric parse-literal-string-value (construct query-string)
  (:documentation "A helper function that parses a string that is a literal.
                   The return value is of the form
                   (list :value object :type string :lang string
                   :next-query string).")
  (:method ((construct SPARQL-Query) (query-string String))
    (let* ((trimmed-str (cut-comment query-string))
	   (result-1 (separate-literal-value construct trimmed-str))
	   (after-literal-value (getf result-1 :next-query))
	   (l-value (getf result-1 :literal))
	   (result-2 (separate-literal-lang-or-type
		      construct after-literal-value))
	   (l-type (if (getf result-2 :type)
		       (getf result-2 :type)
		       *xml-string*))
	   (l-lang (getf result-2 :lang))
	   (next-query (getf result-2 :next-query)))
      (list :next-query next-query :lang l-lang :type l-type
	    :value (cast-literal l-value l-type)))))


(defgeneric separate-literal-lang-or-type (construct query-string)
  (:documentation "A helper function that returns (:next-query string
                   :lang string :type string). Only one of :lang and
                   :type can be set, the other element is set to nil.
                   The query string must be the string direct after
                   the closing literal bounding.")
  (:method ((construct SPARQL-Query) (query-string String))
    (let ((delimiters-1 (list "." ";" "}" " " (string #\tab)
			      (string #\newline)))
	  (delimiters-2 (list " ." ". " ";" "}" " " (string #\tab)
			      (string #\newline)
			      (concatenate 'string "." (string #\newline))
			      (concatenate 'string "." (string #\tab)))))
      (cond ((string-starts-with query-string "@")
	     (let ((end-pos (search-first delimiters-1
					  (subseq query-string 1))))
	       (unless end-pos
		 (error (make-sparql-parser-condition
			 query-string (original-query construct)
			 "'.', ';', '}', ' ', '\t', or '\n'")))
	       (list :next-query (subseq (subseq query-string 1) end-pos)
		     :lang (subseq (subseq query-string 1) 0 end-pos)
		     :type nil)))
	    ((string-starts-with query-string "^^")
	     (let ((end-pos (search-first delimiters-2 (subseq query-string 2))))
	       (unless end-pos
		 (error (make-sparql-parser-condition
			 query-string (original-query construct)
			 "'. ', ,' .', ';', '}', ' ', '\t', or '\n'")))
	       (let* ((type-str (subseq (subseq query-string 2) 0 end-pos))
		      (next-query (subseq (subseq query-string 2) end-pos))
		      (final-type (if (get-prefix construct type-str)
				      (get-prefix construct type-str)
				      type-str)))
		 (list :next-query (cut-comment next-query)
		       :type final-type :lang nil))))
	    (t
	     (list :next-query (cut-comment query-string) :type nil :lang nil))))))


(defgeneric separate-literal-value (construct query-string)
  (:documentation "A helper function that returns (:next-query string
                   :literal string). The literal string contains the
                   pure literal value.")
  (:method ((construct SPARQL-Query) (query-string String))
    (let* ((trimmed-str (cut-comment query-string))
	   (delimiter (cond ((string-starts-with trimmed-str "\"")
			     "\"")
			    ((string-starts-with trimmed-str "'''")
			     "'''")
			    ((string-starts-with trimmed-str "'")
			     "'")
			    (t
			     (error (make-sparql-parser-condition
				     trimmed-str (original-query construct)
				     "a literal starting with ', ''', or \"")))))
	   (literal-end (find-literal-end (subseq trimmed-str (length delimiter))
					  delimiter 0)))
      (list :next-query (subseq trimmed-str (+ literal-end (length delimiter)))
	    :literal (subseq trimmed-str (length delimiter) literal-end)))))


(defgeneric parse-literal-number-value (construct query-string)
  (:documentation "A helper function that parses any number that is a literal.
                   The return value is of the form
                  (list :value nil :type string :next-query string.")
  (:method ((construct SPARQL-Query) (query-string String))
    (let* ((trimmed-str (cut-comment query-string))
	   (triple-delimiters
	    (list ". " ";" " " (string #\tab)
		  (string #\newline) "}"))
	   (end-pos (search-first triple-delimiters
				  trimmed-str)))
      (unless end-pos
	(error (make-sparql-parser-condition
		trimmed-str (original-query construct)
		"'. ', , ';' ' ', '\\t', '\\n' or '}'")))
      (let* ((literal-number
	      (read-from-string (subseq trimmed-str 0 end-pos)))
	     (number-type
	      (if (search "." (subseq trimmed-str 0 end-pos))
		  *xml-double* ;could also be an xml:decimal, since the doucble has
			       ;a bigger range it shouldn't matter
		  *xml-integer*)))
	(unless (numberp literal-number)
	  (error (make-sparql-parser-condition
		  trimmed-str (original-query construct)
		  "a valid number of the form '1', '1.3', 1.0e6'")))
	(list :value literal-number :type number-type
	      :next-query (subseq trimmed-str end-pos))))))


(defgeneric parse-base-suffix-pair (construct query-string)
  (:documentation "A helper function that returns a list of the form
                  (list :next-query string :value (:value uri :type 'IRI)).")
  (:method ((construct SPARQL-Query) (query-string String))
    (let* ((trimmed-str (cut-comment query-string))
	   (result (parse-closed-value trimmed-str construct))
	   (result-uri
	    (if (or (absolute-uri-p (getf result :value))
		    (not (base-value construct)))
		(getf result :value)
		(concatenate-uri (base-value construct)
				 (getf result :value))))
	   (next-query (getf result :next-query)))
      (list :next-query (cut-comment next-query)
	    :value (make-instance 'SPARQL-Triple-Elem
				  :elem-type 'IRI
				  :value result-uri)))))


(defgeneric parse-prefix-suffix-pair(construct query-string)
  (:documentation "A helper function that returns a list of the form
                  (list :next-query string :value (:value uri :type 'IRI)).")
  (:method ((construct SPARQL-Query) (query-string String))
    (let* ((trimmed-str (cut-comment query-string))
	   (delimiters (list "." ";" "}" "<" " " (string #\newline)
			     (string #\tab) "#"))
	   (end-pos (search-first delimiters trimmed-str))
	   (elem-str (when end-pos
		       (subseq trimmed-str 0 end-pos)))
	   (prefix (when elem-str
		     (string-until elem-str ":")))
	   (suffix (when prefix
		     (string-after elem-str ":")))
	   (full-url
	    (when (and suffix prefix)
	      (get-prefix construct (concatenate 'string prefix ":" suffix)))))
      (unless (and end-pos prefix suffix)
	(error (make-sparql-parser-condition
		trimmed-str (original-query construct)
		"An IRI of the form prefix:suffix")))
      (unless full-url
	(error (make-condition
		'sparql-parser-error
		:message (format nil "The prefix in \"~a:~a\" is not registered"
				 prefix suffix))))
      (list :next-query (cut-comment
			 (string-after
			  trimmed-str
			  (concatenate 'string prefix ":" suffix)))
	    :value (make-instance 'SPARQL-Triple-Elem
				  :elem-type 'IRI
				  :value full-url)))))


(defgeneric parse-triple (construct query-string &key last-subject)
  (:documentation "Parses a triple within a trippel group.")
  (:method ((construct SPARQL-Query) (query-string String) &key (last-subject nil))
    (declare (type (or Null SPARQL-Triple-Elem) last-subject))
    (let* ((trimmed-str (cut-comment query-string))
	   (subject-result (if last-subject ;;is used after a ";"
			       last-subject
			       (parse-triple-elem construct trimmed-str)))
	   (predicate-result (parse-triple-elem
			      construct
			      (if last-subject
				  trimmed-str
				  (getf subject-result :next-query))))
	   (object-result (parse-triple-elem construct
					     (getf predicate-result :next-query)
					     :literal-allowed t)))
      (add-triple construct
		  (make-instance 'SPARQL-Triple
				 :subject (if last-subject
					      last-subject
					      (getf subject-result :value))
				 :predicate (getf predicate-result :value)
				 :object (getf object-result :value)))
      (let ((tr-str (cut-comment (getf object-result :next-query))))
	(cond ((string-starts-with tr-str ";")
	       (parse-group construct (subseq tr-str 1)
			    :last-subject (getf subject-result :value)))
	      ((string-starts-with tr-str ".")
	       (parse-group construct (subseq tr-str 1)))
	      ((string-starts-with tr-str "}")
	       (parse-group construct tr-str)))))))


(defgeneric parse-variables (construct query-string)
  (:documentation "Parses the variables of the SELECT statement
                   and adds them to the passed construct.")
  (:method ((construct SPARQL-Query) (query-string String))
    (let ((trimmed-str (cut-comment query-string)))
      (if (string-starts-with trimmed-str "WHERE")
	  trimmed-str
	  (if (string-starts-with trimmed-str "*")
	      (progn (add-variable construct "*")
		     (parse-variables construct (string-after trimmed-str "*")))
	      (let ((result (parse-variable-name construct trimmed-str)))
		(add-variable construct (getf result :value))
		(parse-variables construct (getf result :next-query))))))))


(defgeneric parse-variable-name (construct query-string &key additional-delimiters)
  (:documentation "A helper function that parses the first non-whitespace character
                   in the query. since it must be a variable, it must be prefixed
                   by a ? or $. The return value is of the form
                  (:next-query string :value string).")
  (:method ((construct SPARQL-Query) (query-string String)
	    &key (additional-delimiters))
    (declare (List additional-delimiters))
    (let ((trimmed-str (cut-comment query-string))
	  (delimiters (append
		       (list " " "?" "$" "." (string #\newline) (string #\tab))
		       additional-delimiters)))
      (unless (or (string-starts-with trimmed-str "?")
		  (string-starts-with trimmed-str "$"))
	(error (make-sparql-parser-condition
		trimmed-str (original-query construct) "? or $")))
      (let* ((var-name-end (search-first delimiters (subseq trimmed-str 1)))
	     (var-name
	      (if var-name-end
		  (subseq trimmed-str 0 (+ 1 var-name-end))
		  (error (make-sparql-parser-condition
			  trimmed-str (original-query construct)
			  "space, newline, tab, ?, ., $ or WHERE"))))
	     (next-query (string-after trimmed-str var-name))
	     (normalized-var-name 
	      (if (<= (length var-name) 1)
		  (error (make-sparql-parser-condition
			  next-query (original-query construct)
			  "a variable name"))
		  (subseq var-name 1))))
	(list :next-query next-query :value normalized-var-name)))))


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