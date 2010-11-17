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
	 (format nil "The query:~%~a bad token on position ~a. Expected: ~a"
		 entire-query (- (length entire-query)
				 (length rest-of-query))
		 expected)))
    (make-condition 'sparql-parser-error :message message)))



(defgeneric parser-start(construct query-string)
  (:documentation "The entry point of the SPARQL-parser.")
  (:method ((construct SPARQL-Query) (query-string String))
    (let ((trimmed-query-string (trim-whitespace-left query-string)))
      (cond ((string-starts-with trimmed-query-string "SELECT")
	     (parse-prefixes construct
			     (string-after trimmed-query-string "SELECT")))
	    ((string-starts-with trimmed-query-string "PREFIX")
	     nil) ;TODO: implement
	    ((string-starts-with trimmed-query-string "BASE")
	     nil) ;TODO: implement
	    (t
	     (error (make-sparql-parser-condition
		     trimmed-query-string (original-query construct)
		     "SELECT, PREFIX or BASE")))))))


(defgeneric parse-prefixes (construct query-string)
  (:documentation "Sets the correponding prefix-tuples in the passed object.")
  (:method ((construct SPARQL-Query) (query-string String))
    (let ((trimmed-string (trim-whitespace-left query-string)))
      (if (string-starts-with trimmed-string ":")
	  (let ((results
		 (parse-bracket-value (subseq trimmed-string 1) construct)))
	    (add-prefix construct *empty-label* (getf results :value))
	    (parser-start construct (getf results :query-string)))
	  (let* ((label-name
		  (trim-whitespace-right (string-until trimmed-string ":")))
		 (next-query-str
		  (trim-whitespace-left (string-after trimmed-string ":")))
		 (results (parse-bracket-value next-query-str construct)))
	    (add-prefix construct label-name (getf results :value))
	    (parser-start construct (getf results :query-string)))))))


(defun parse-bracket-value(query-string query-object &key (open "<") (close ">"))
  "A helper function that checks the value of a statement within
   two brackets, i.e. <prefix-value>. A list of the
   form (:query-string string :value string) is returned."
  (declare (String query-string open close)
	   (SPARQL-Query query-object))
  (let ((trimmed-string (trim-whitespace-left query-string)))
    (if (and (string-starts-with trimmed-string open)
	     (> (length (string-after trimmed-string close)) 0))
	(let* ((pref-url
		(string-until (string-after trimmed-string open) close))
	       (next-query-str
		(string-after pref-url close)))
	  (unless next-query-str
	    (error (make-sparql-parser-condition
		    trimmed-string (original-query query-object)
		    close)))
	  (list :query-string next-query-str
		:value pref-url))
	(error (make-sparql-parser-condition
		trimmed-string (original-query query-object)
		open)))))



;((PREFIX bounding: <uri-prefix>)|(PREFIX : <uri-prefix>)*
;(BASE <base-uri>)*)*
;SELECT ?varName+
;WHERE {
;(({?subjectOrVarName predicateOrVarName objectOrVarName}?)*
;({?FILTER (filterExpression)}?)*
;(BASE <base-uri>)*)*
;}
;Grouping
;{}
;Base
;BASE <uri>
;…
;<book>
;-> uri/book
;Literals
;(“anyCharacter*“)|(‘anyCharacter*‘)((anyUri)|(@languageTag)){0,1}
;
;Variables
;($anyChar*)|(?anyChar*)
;?var = $var
;Predicate object-lists
;?x foaf:name ?name ;
;foaf:mbox ?mbox .
;This is the same as writing the triple patterns:
;?x foaf:name ?name .
;?x foaf:mbox ?mbox .
;rdf:type
;rdf:type = a
;Empty Graph Pattern
;The group pattern:
;{ }
;matches any graph (including the empty graph) with one solution that does not bind any variables. For example:
;SELECT ?x
;WHERE {}
;matches with one solution in which variable x is not bound."