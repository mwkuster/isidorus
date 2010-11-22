;;+-----------------------------------------------------------------------------
;;+  Isidorus
;;+  (c) 2008-2010 Marc Kuester, Christoph Ludwig, Lukas Georgieff
;;+
;;+  Isidorus is freely distributable under the LLGPL license.
;;+  You can find a detailed description in trunk/docs/LLGPL-LICENSE.txt and
;;+  trunk/docs/LGPL-LICENSE.txt.
;;+-----------------------------------------------------------------------------

(defpackage :sparql-test
  (:use  :cl
	 :it.bese.FiveAM
	 :TM-SPARQL
	 :exceptions
	 :constants)
  (:export :run-sparql-tests
	   :sparql-tests
	   :test-prefix-and-base
	   :test-parse-literals
	   :test-parse-triple-elem))


(in-package :sparql-test)


(def-suite sparql-tests
     :description "tests  various key functions of the TM-SPARQL module")

(in-suite sparql-tests)


(test test-prefix-and-base
  "Tests the sparql parser when parsing PREFIX and BASE statements."
  (let* ((query-1 "PREFIX foaf  :   <http://xmlns.com/foaf/0.1/>
                   PREFIX org:    <http://example.com/ns#>
                   PREFIX isi:<http://isidor.us>
                   PREFIX :<http://some.where>
                   BASE      <http://base.one>
                   PREFIX foaf : <http://overwrite.foaf>
                   BASE<http://base.two>")
	 (query-2 "PREFIX foaf  :   <http://xmlns.com/foaf/0.1/>
                   PREFIX org:
<http://example.com/ns#>
                   PREFIX isi:<http://isidor.us>
                   PREFIX
:<http://some.where>
                   BASE      <http://base.one>
                   PREFIX foaf : <http://overwrite.foaf>
                   BASE<http://base.two>")
	 (query-object-1 (make-instance 'SPARQL-Query :query query-1))
	 (query-object-2 (make-instance 'SPARQL-Query :query query-2
					:base "http://any-base")))
    (signals missing-argument-error (make-instance 'SPARQL-Query))
    (is-true query-object-1)
    (is-true query-object-2)
    (is (string= (TM-SPARQL::base-value query-object-1) "http://base.two"))
    (is (string= (TM-SPARQL::base-value query-object-2) "http://base.two"))
    (is (= (length (TM-SPARQL::prefixes query-object-1)) 4))
    (is (= (length (TM-SPARQL::prefixes query-object-2)) 4))
    (is-true (find-if #'(lambda(elem)
			  (and (string= (getf elem :label) "foaf")
			       (string= (getf elem :value)
					"http://overwrite.foaf")))
		      (TM-SPARQL::prefixes query-object-1)))
    (is-true (find-if #'(lambda(elem)
			  (and (string= (getf elem :label) "org")
			       (string= (getf elem :value)
					"http://example.com/ns#")))
		      (TM-SPARQL::prefixes query-object-1)))
    (is-true (find-if #'(lambda(elem)
			  (and (string= (getf elem :label) "isi")
			       (string= (getf elem :value)
					"http://isidor.us")))
		      (TM-SPARQL::prefixes query-object-1)))
    (is-true (find-if #'(lambda(elem)
			  (and (string= (getf elem :label)
					TM-SPARQL::*empty-label*)
			       (string= (getf elem :value)
					"http://some.where")))
		      (TM-SPARQL::prefixes query-object-1)))
    (is-true (find-if #'(lambda(elem)
			  (and (string= (getf elem :label) "foaf")
			       (string= (getf elem :value)
					"http://overwrite.foaf")))
		      (TM-SPARQL::prefixes query-object-2)))
    (is-true (find-if #'(lambda(elem)
			  (and (string= (getf elem :label) "org")
			       (string= (getf elem :value)
					"http://example.com/ns#")))
		      (TM-SPARQL::prefixes query-object-2)))
    (is-true (find-if #'(lambda(elem)
			  (and (string= (getf elem :label) "isi")
			       (string= (getf elem :value)
					"http://isidor.us")))
		      (TM-SPARQL::prefixes query-object-2)))
    (is-true (find-if #'(lambda(elem)
			  (and (string= (getf elem :label)
					TM-SPARQL::*empty-label*)
			       (string= (getf elem :value)
					"http://some.where")))
		      (TM-SPARQL::prefixes query-object-2)))))


(test test-variable-names
  "Tests the sparql parser when parsing variables in the SELECT statement."
  (let* ((query-1 "PREFIX foaf  :   <http://xmlns.com/foaf/0.1/>
                   PREFIX org:    <http://example.com/ns#>
                   PREFIX isi:<http://isidor.us>
                   PREFIX :<http://some.where>
                   BASE      <http://base.one>
                   PREFIX foaf : <http://overwrite.foaf>
                   BASE<http://base.two>
                   SELECT ?var1$var2
$var3 ?var3 WHERE{}")
	 (query-2 "SELECT ?var1$var2 $var3 ?var3 WHERE{}")
	 (query-3 "SELECT ?var1$var2 $var3 ?var3WHERE{}")
	 (query-4 "SELECT * WHERE{}")
	 (query-object-1 (make-instance 'SPARQL-Query :query query-1))
	 (query-object-2 (make-instance 'SPARQL-Query :query query-2))
	 (query-object-3 (make-instance 'SPARQL-QUERY :query query-4)))
    (is-true query-object-1)
    (is-true query-object-2)
    (is-true query-object-3)
    (signals sparql-parser-error (make-instance 'SPARQL-Query :query query-3))
    (is (= (length (TM-SPARQL::variables query-object-1)) 3))
    (is-true (find-if #'(lambda(elem)
			  (and (string= (getf elem :variable) "var1")
			       (null (getf elem :value))))
		      (TM-SPARQL::variables query-object-1)))
    (is-true (find-if #'(lambda(elem)
			  (and (string= (getf elem :variable) "var2")
			       (null (getf elem :value))))
		      (TM-SPARQL::variables query-object-1)))
    (is-true (find-if #'(lambda(elem)
			  (and (string= (getf elem :variable) "var3")
			       (null (getf elem :value))))
		      (TM-SPARQL::variables query-object-1)))
    (is (= (length (TM-SPARQL::variables query-object-2)) 3))
    (is-true (find-if #'(lambda(elem)
			  (and (string= (getf elem :variable) "var1")
			       (null (getf elem :value))))
		      (TM-SPARQL::variables query-object-2)))
    (is-true (find-if #'(lambda(elem)
			  (and (string= (getf elem :variable) "var2")
			       (null (getf elem :value))))
		      (TM-SPARQL::variables query-object-2)))
    (is-true (find-if #'(lambda(elem)
			  (and (string= (getf elem :variable) "var3")
			       (null (getf elem :value))))
		      (TM-SPARQL::variables query-object-2)))
    (is-true (find-if #'(lambda(elem)
			  (and (string= (getf elem :variable) "*")
			       (null (getf elem :value))))
		      (TM-SPARQL::variables query-object-3)))))


(test test-parse-literals
  "Tests the helper functions for parsing literals."
  (let ((query-1 "   \"literal-value\"@de.")
	(query-2 "true.")
	(query-3 "false}")
	(query-4 (concatenate 'string "1234.43e10" (string #\tab)))
	(query-5 (concatenate 'string "'''true'''^^" *xml-boolean* " ;"))
	(query-6 (concatenate 'string "'123.4'^^" *xml-double*
			      "." (string #\newline)))
	(query-7 "\"Just a test

literal with some \\\"quoted\\\" words!\"@en.")
	(query-8 (concatenate 'string "'''12.4'''^^" *xml-integer* ". "))
	(query-9 (concatenate 'string "\"13e4\"^^" *xml-boolean* " ."))
	(dummy-object (make-instance 'SPARQL-Query :query "")))
    (is-true dummy-object)
    (let ((result (tm-sparql::parse-literal-elem query-1 dummy-object)))
      (is (string= (getf result :next-query) "."))
      (is (string= (getf (getf result :value) :value)
		   "literal-value"))
      (is (string= (getf (getf result :value) :literal-lang)
		   "de"))
      (is (string= (getf (getf result :value) :literal-type)
		   *xml-string*))
      (is (eql (getf (getf result :value) :type) 'TM-SPARQL::LITERAL)))
    (let ((result (tm-sparql::parse-literal-elem query-2 dummy-object)))
      (is (string= (getf result :next-query) "."))
      (is (eql (getf (getf result :value) :value) t))
      (is-false (getf (getf result :value) :literal-lang))
      (is (string= (getf (getf result :value) :literal-type)
		   *xml-boolean*))
      (is (eql (getf (getf result :value) :type) 'TM-SPARQL::LITERAL)))
    (let ((result (tm-sparql::parse-literal-elem query-3 dummy-object)))
      (is (string= (getf result :next-query) "}"))
      (is (eql (getf (getf result :value) :value) nil))
      (is-false (getf (getf result :value) :literal-lang))
      (is (string= (getf (getf result :value) :literal-type)
		   *xml-boolean*))
      (is (eql (getf (getf result :value) :type) 'TM-SPARQL::LITERAL)))
    (let ((result (tm-sparql::parse-literal-elem query-4 dummy-object)))
      (is (string= (getf result :next-query) (string #\tab)))
      (is (= (getf (getf result :value) :value) 1234.43e10))
      (is-false (getf (getf result :value) :literal-lang))
      (is (string= (getf (getf result :value) :literal-type)
		   *xml-double*))
      (is (eql (getf (getf result :value) :type) 'TM-SPARQL::LITERAL)))
    (let ((result (tm-sparql::parse-literal-elem query-5 dummy-object)))
      (is (string= (getf result :next-query) ";"))
      (is (eql (getf (getf result :value) :value) t))
      (is-false (getf (getf result :value) :literal-lang))
      (is (string= (getf (getf result :value) :literal-type)
		   *xml-boolean*))
      (is (eql (getf (getf result :value) :type) 'TM-SPARQL::LITERAL)))
    (let ((result (tm-sparql::parse-literal-elem query-6 dummy-object)))
      (is (string= (getf result :next-query)
		   (concatenate 'string "." (string #\newline))))
      (is (= (getf (getf result :value) :value) 123.4))
      (is-false (getf (getf result :value) :literal-lang))
      (is (string= (getf (getf result :value) :literal-type)
		   *xml-double*))
      (is (eql (getf (getf result :value) :type) 'TM-SPARQL::LITERAL)))
    (let ((result (tm-sparql::parse-literal-elem query-7 dummy-object)))
      (is (string= (getf result :next-query) "."))
      (is (string= (getf (getf result :value) :value)
		   "Just a test

literal with some \\\"quoted\\\" words!"))
      (is (string= (getf (getf result :value) :literal-lang)
		   "en"))
      (is (string= (getf (getf result :value) :literal-type)
		   *xml-string*))
      (is (eql (getf (getf result :value) :type) 'TM-SPARQL::LITERAL)))
    (signals sparql-parser-error
      (tm-sparql::parse-literal-elem query-8 dummy-object))
    (signals sparql-parser-error
      (tm-sparql::parse-literal-elem query-9 dummy-object))))


(test test-parse-triple-elem
  "Tests various functionality of the parse-triple-elem function."
  (let ((query-1 "?var1   .")
	(query-2 "$var2 ;")
	(query-3 "$var3 }")
	(query-4 "<http://full.url>.")
	(query-5 "<url-suffix>  }")
	(query-6 "pref:suffix  .")
	(query-7 "pref:suffix}")
	(query-8 "preff:suffix}")
	(dummy-object (make-instance 'SPARQL-Query :query ""
				     :base "http://base.value")))
    (tm-sparql::add-prefix dummy-object "pref" "http://prefix.value")
    (let ((result (tm-sparql::parse-triple-elem query-1 dummy-object)))
      (is (string= (getf result :next-query) "."))
      (is (string= (getf (getf result :value) :value) "var1"))
      (is (eql (getf (getf result :value) :type) 'TM-SPARQL::VAR)))
    (let ((result (tm-sparql::parse-triple-elem query-2 dummy-object)))
      (is (string= (getf result :next-query) ";"))
      (is (string= (getf (getf result :value) :value) "var2"))
      (is (eql (getf (getf result :value) :type) 'TM-SPARQL::VAR)))
    (let ((result (tm-sparql::parse-triple-elem query-3 dummy-object)))
      (is (string= (getf result :next-query) "}"))
      (is (string= (getf (getf result :value) :value) "var3"))
      (is (eql (getf (getf result :value) :type) 'TM-SPARQL::VAR)))
    (let ((result (tm-sparql::parse-triple-elem query-4 dummy-object)))
      (is (string= (getf result :next-query) "."))
      (is (string= (getf (getf result :value) :value) "http://full.url"))
      (is (eql (getf (getf result :value) :type) 'TM-SPARQL::IRI)))
    (let ((result (tm-sparql::parse-triple-elem query-5 dummy-object)))
      (is (string= (getf result :next-query) "}"))
      (is (string= (getf (getf result :value) :value) "http://base.value/url-suffix"))
      (is (eql (getf (getf result :value) :type) 'TM-SPARQL::IRI)))
    (let ((result (tm-sparql::parse-triple-elem query-6 dummy-object)))
      (is (string= (getf result :next-query) "."))
      (is (string= (getf (getf result :value) :value) "http://prefix.value/suffix"))
      (is (eql (getf (getf result :value) :type) 'TM-SPARQL::IRI)))
    (let ((result (tm-sparql::parse-triple-elem query-7 dummy-object)))
      (is (string= (getf result :next-query) "}"))
      (is (string= (getf (getf result :value) :value) "http://prefix.value/suffix"))
      (is (eql (getf (getf result :value) :type) 'TM-SPARQL::IRI)))
    (signals sparql-parser-error 
      (tm-sparql::parse-triple-elem query-8 dummy-object))))


(test test-parse-group-1
  "Test various functionality of several functions responsible for parsing
   the SELECT-WHERE-statement."
  (let ((query-1 "?subject ?predicate $object }")
	(query-2 "<subject> pref:predicate 1234.5e12}")
	(query-3 "pref:subject ?predicate 'literal'@en}")
	(dummy-object (make-instance 'SPARQL-Query :query ""
				     :base "http://base.value/")))
    (is-true dummy-object)
    (tm-sparql::add-prefix dummy-object "pref" "http://prefix.value/")
    (let ((result (tm-sparql::parse-triple dummy-object query-1 nil)))
      (is (string= (getf result :next-query) "}"))
      (is (= (length (getf result :values)) 1))
      (is (eql (getf (getf (first (getf result :values)) :subject) :type)
	       'TM-SPARQL::VAR))
      (is (string= (getf (getf (first (getf result :values)) :subject) :value)
		   "subject"))
      (is (eql (getf (getf (first (getf result :values)) :predicate) :type)
	       'TM-SPARQL::VAR))
      (is (string= (getf (getf (first (getf result :values)) :predicate) :value)
		   "predicate"))
      (is (eql (getf (getf (first (getf result :values)) :object) :type)
	       'TM-SPARQL::VAR))
      (is (string= (getf (getf (first (getf result :values)) :object) :value)
		   "object")))
    (let ((result (tm-sparql::parse-triple dummy-object query-2 nil)))
      (is (string= (getf result :next-query) "}"))
      (is (eql (getf (getf (first (getf result :values)) :subject) :type)
	       'TM-SPARQL::IRI))
      (is (string= (getf (getf (first (getf result :values)) :subject) :value)
		   "http://base.value/subject"))
      (is (eql (getf (getf (first (getf result :values)) :predicate) :type)
	       'TM-SPARQL::IRI))
      (is (string= (getf (getf (first (getf result :values)) :predicate) :value)
		   "http://prefix.value/predicate"))
      (is (eql (getf (getf (first (getf result :values)) :object) :type)
	       'TM-SPARQL::LITERAL))
      (is (= (getf (getf (first (getf result :values)) :object) :value)
	     1234.5e12))
      (is (string= (getf (getf (first (getf result :values)) :object)
			 :literal-type)
		   *xml-double*)))
    (let ((result (tm-sparql::parse-triple dummy-object query-3 nil)))
      (is (string= (getf result :next-query) "}"))
      (is (eql (getf (getf (first (getf result :values)) :subject) :type)
	       'TM-SPARQL::IRI))
      (is (string= (getf (getf (first (getf result :values)) :subject) :value)
		   "http://prefix.value/subject"))
      (is (eql (getf (getf (first (getf result :values)) :predicate) :type)
	       'TM-SPARQL::VAR))
      (is (string= (getf (getf (first (getf result :values)) :predicate) :value)
		   "predicate"))
      (is (eql (getf (getf (first (getf result :values)) :object) :type)
	       'TM-SPARQL::LITERAL))
      (is (string= (getf (getf (first (getf result :values)) :object) :value)
		   "literal"))
      (is (string= (getf (getf (first (getf result :values)) :object)
			 :literal-lang)
		   "en")))))


(test test-parse-group-2
  "Test various functionality of several functions responsible for parsing
   the SELECT-WHERE-statement."
  (let ((query-4 (concatenate 'string "<subject> <predicate> '''true'''^^"
			      *xml-boolean* "; pref:predicate-2 \"12\"^^"
			      *xml-integer* "}"))
	(query-5 (concatenate 'string "<subject> <predicate> '''false'''^^"
			      *xml-boolean* "; pref:predicate-2 \"abc\"^^"
			      *xml-string* "}"))
	(dummy-object (make-instance 'SPARQL-Query :query ""
				     :base "http://base.value/")))
    (is-true dummy-object)
    (tm-sparql::add-prefix dummy-object "pref" "http://prefix.value/")
    (let ((result (tm-sparql::parse-triple dummy-object query-4 nil)))
      (is (string= (getf result :next-query) "}"))
      (is (= (length (getf result :values)) 2))
      (is (eql (getf (getf (first (getf result :values)) :subject) :type)
	       'TM-SPARQL::IRI))
      (is (string= (getf (getf (first (getf result :values)) :subject) :value)
		   "http://base.value/subject"))
      (is (eql (getf (getf (first (getf result :values)) :predicate) :type)
	       'TM-SPARQL::IRI))
      (is (string= (getf (getf (first (getf result :values)) :predicate) :value)
		   "http://base.value/predicate"))
      (is (eql (getf (getf (first (getf result :values)) :object) :type)
	       'TM-SPARQL::LITERAL))
      (is (eql (getf (getf (first (getf result :values)) :object) :value) t))
      (is (string= (getf (getf (first (getf result :values)) :object)
			 :literal-type)
		   *xml-boolean*))
      (is (string= (getf result :next-query) "}"))
      (is (= (length (getf result :values)) 2))
      (is (eql (getf (getf (second (getf result :values)) :subject) :type)
	       'TM-SPARQL::IRI))
      (is (string= (getf (getf (second (getf result :values)) :subject) :value)
		   "http://base.value/subject"))
      (is (eql (getf (getf (second (getf result :values)) :predicate) :type)
	       'TM-SPARQL::IRI))
      (is (string= (getf (getf (second (getf result :values)) :predicate) :value)
		   "http://prefix.value/predicate-2"))
      (is (eql (getf (getf (second (getf result :values)) :object) :type)
	       'TM-SPARQL::LITERAL))
      (is (= (getf (getf (second (getf result :values)) :object) :value) 12))
      (is (string= (getf (getf (second (getf result :values)) :object)
			 :literal-type)
		   *xml-integer*)))
    (let ((result (tm-sparql::parse-triple dummy-object query-5 nil)))
      (is (string= (getf result :next-query) "}"))
      (is (= (length (getf result :values)) 2))
      (is (eql (getf (getf (first (getf result :values)) :subject) :type)
	       'TM-SPARQL::IRI))
      (is (string= (getf (getf (first (getf result :values)) :subject) :value)
		   "http://base.value/subject"))
      (is (eql (getf (getf (first (getf result :values)) :predicate) :type)
	       'TM-SPARQL::IRI))
      (is (string= (getf (getf (first (getf result :values)) :predicate) :value)
		   "http://base.value/predicate"))
      (is (eql (getf (getf (first (getf result :values)) :object) :type)
	       'TM-SPARQL::LITERAL))
      (is (eql (getf (getf (first (getf result :values)) :object) :value) nil))
      (is (string= (getf (getf (first (getf result :values)) :object)
			 :literal-type)
		   *xml-boolean*))
      (is (string= (getf result :next-query) "}"))
      (is (= (length (getf result :values)) 2))
      (is (eql (getf (getf (second (getf result :values)) :subject) :type)
	       'TM-SPARQL::IRI))
      (is (string= (getf (getf (second (getf result :values)) :subject) :value)
		   "http://base.value/subject"))
      (is (eql (getf (getf (second (getf result :values)) :predicate) :type)
	       'TM-SPARQL::IRI))
      (is (string= (getf (getf (second (getf result :values)) :predicate) :value)
		   "http://prefix.value/predicate-2"))
      (is (eql (getf (getf (second (getf result :values)) :object) :type)
	       'TM-SPARQL::LITERAL))
      (is (string= (getf (getf (second (getf result :values)) :object) :value)
		   "abc"))
      (is (string= (getf (getf (second (getf result :values)) :object)
			 :literal-type)
		   *xml-string*)))))



(defun run-sparql-tests ()
  (it.bese.fiveam:run! 'sparql-test:sparql-tests))
