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
	 :unittests-constants
	 :fixtures
	 :d
	 :constants)
  (:export :run-sparql-tests
	   :sparql-tests
	   :test-prefix-and-base
	   :test-parse-literals
	   :test-parse-triple-elem
	   :test-parse-group-1
	   :test-parse-group-2
	   :test-set-result-1
	   :test-set-result-2))


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
      (is (string= (tm-sparql::value (getf result :value))
		   "literal-value"))
      (is (string= (tm-sparql::literal-lang (getf result :value))
		   "de"))
      (is (string= (tm-sparql::literal-type (getf result :value))
		   *xml-string*))
      (is (eql (tm-sparql::elem-type (getf result :value)) 'TM-SPARQL::LITERAL)))
    (let ((result (tm-sparql::parse-literal-elem query-2 dummy-object)))
      (is (string= (getf result :next-query) "."))
      (is (eql (tm-sparql::value (getf result :value)) t))
      (is-false (tm-sparql::literal-lang (getf result :value)))
      (is (string= (tm-sparql::literal-type (getf result :value))
		   *xml-boolean*))
      (is (eql (tm-sparql::elem-type (getf result :value)) 'TM-SPARQL::LITERAL)))
    (let ((result (tm-sparql::parse-literal-elem query-3 dummy-object)))
      (is (string= (getf result :next-query) "}"))
      (is (eql (tm-sparql::value (getf result :value)) nil))
      (is-false (tm-sparql::literal-lang (getf result :value)))
      (is (string= (tm-sparql::literal-type (getf result :value))
		   *xml-boolean*))
      (is (eql (tm-sparql::elem-type (getf result :value)) 'TM-SPARQL::LITERAL)))
    (let ((result (tm-sparql::parse-literal-elem query-4 dummy-object)))
      (is (string= (getf result :next-query) (string #\tab)))
      (is (= (tm-sparql::value (getf result :value)) 1234.43e10))
      (is-false (tm-sparql::literal-lang (getf result :value)))
      (is (string= (tm-sparql::literal-type (getf result :value))
		   *xml-double*))
      (is (eql (tm-sparql::elem-type (getf result :value)) 'TM-SPARQL::LITERAL)))
    (let ((result (tm-sparql::parse-literal-elem query-5 dummy-object)))
      (is (string= (getf result :next-query) ";"))
      (is (eql (tm-sparql::value (getf result :value)) t))
      (is-false (tm-sparql::literal-lang (getf result :value)))
      (is (string= (tm-sparql::literal-type (getf result :value))
		   *xml-boolean*))
      (is (eql (tm-sparql::elem-type (getf result :value)) 'TM-SPARQL::LITERAL)))
    (let ((result (tm-sparql::parse-literal-elem query-6 dummy-object)))
      (is (string= (getf result :next-query)
		   (concatenate 'string "." (string #\newline))))
      (is (eql (tm-sparql::value (getf result :value)) 123.4))
      (is-false (tm-sparql::literal-lang (getf result :value)))
      (is (string= (tm-sparql::literal-type (getf result :value))
		   *xml-double*))
      (is (eql (tm-sparql::elem-type (getf result :value)) 'TM-SPARQL::LITERAL)))
    (let ((result (tm-sparql::parse-literal-elem query-7 dummy-object)))
      (is (string= (getf result :next-query) "."))
      (is (string= (tm-sparql::value (getf result :value))
		   "Just a test

literal with some \\\"quoted\\\" words!"))
      (is (string= (tm-sparql::literal-lang (getf result :value)) "en"))
      (is (string= (tm-sparql::literal-type (getf result :value))
		   *xml-string*))
      (is (eql (tm-sparql::elem-type (getf result :value)) 'TM-SPARQL::LITERAL)))
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
				     :base "http://base.value"))
	(var 'TM-SPARQL::VARIABLE)
	(iri 'TM-SPARQL::IRI))
    (tm-sparql::add-prefix dummy-object "pref" "http://prefix.value")
    (let ((result (tm-sparql::parse-triple-elem query-1 dummy-object)))
      (is (string= (getf result :next-query) "."))
      (is (string= (tm-sparql::value (getf result :value)) "var1"))
      (is (eql (tm-sparql::elem-type (getf result :value)) var)))
    (let ((result (tm-sparql::parse-triple-elem query-2 dummy-object)))
      (is (string= (getf result :next-query) ";"))
      (is (string= (tm-sparql::value (getf result :value)) "var2"))
      (is (eql (tm-sparql::elem-type (getf result :value)) var)))
    (let ((result (tm-sparql::parse-triple-elem query-3 dummy-object)))
      (is (string= (getf result :next-query) "}"))
      (is (string= (tm-sparql::value (getf result :value)) "var3"))
      (is (eql (tm-sparql::elem-type (getf result :value)) var)))
    (let ((result (tm-sparql::parse-triple-elem query-4 dummy-object)))
      (is (string= (getf result :next-query) "."))
      (is (string= (tm-sparql::value (getf result :value))
		   "http://full.url"))
      (is (eql (tm-sparql::elem-type (getf result :value)) iri)))
    (let ((result (tm-sparql::parse-triple-elem query-5 dummy-object)))
      (is (string= (getf result :next-query) "}"))
      (is (string= (tm-sparql::value (getf result :value))
		   "http://base.value/url-suffix"))
      (is (eql (tm-sparql::elem-type (getf result :value)) iri)))
    (let ((result (tm-sparql::parse-triple-elem query-6 dummy-object)))
      (is (string= (getf result :next-query) "."))
      (is (string= (tm-sparql::value (getf result :value))
		   "http://prefix.value/suffix"))
      (is (eql (tm-sparql::elem-type (getf result :value)) iri)))
    (let ((result (tm-sparql::parse-triple-elem query-7 dummy-object)))
      (is (string= (getf result :next-query) "}"))
      (is (string= (tm-sparql::value (getf result :value))
		   "http://prefix.value/suffix"))
      (is (eql (tm-sparql::elem-type (getf result :value)) iri)))
    (signals sparql-parser-error 
      (tm-sparql::parse-triple-elem query-8 dummy-object))))


(test test-parse-group-1
  "Test various functionality of several functions responsible for parsing
   the SELECT-WHERE-statement."
  (let ((query-1 "?subject ?predicate $object }")
	(query-2 "<subject> pref:predicate 1234.5e12}")
	(query-3 "pref:subject ?predicate 'literal'@en}")
	(dummy-object (make-instance 'SPARQL-Query :query ""
				     :base "http://base.value/"))
	(var 'TM-SPARQL::VARIABLE)
	(lit 'TM-SPARQL::LITERAL)
	(iri 'TM-SPARQL::IRI))
    (is-true dummy-object)
    (tm-sparql::add-prefix dummy-object "pref" "http://prefix.value/")
    (is (string= (tm-sparql::parse-triple dummy-object query-1) ""))
    (is (= (length (tm-sparql::select-group dummy-object)) 1))
    (let ((elem (first (tm-sparql::select-group dummy-object))))
      (is (eql (tm-sparql::elem-type (tm-sparql::subject elem)) var))
      (is (string= (tm-sparql::value (tm-sparql::subject elem)) "subject"))
      (is (eql (tm-sparql::elem-type (tm-sparql::predicate elem)) var))
      (is (string= (tm-sparql::value (tm-sparql::predicate elem)) "predicate"))
      (is (eql (tm-sparql::elem-type (tm-sparql::object elem)) var))
      (is (string= (tm-sparql::value (tm-sparql::object elem)) "object")))
    (is (string= (tm-sparql::parse-triple dummy-object query-2) ""))
    (is (= (length (tm-sparql::select-group dummy-object)) 2))
    (let ((elem (first (tm-sparql::select-group dummy-object))))
      (is (eql (tm-sparql::elem-type (tm-sparql::subject elem)) iri))
      (is (string= (tm-sparql::value (tm-sparql::subject elem))
		   "http://base.value/subject"))
      (is (eql (tm-sparql::elem-type (tm-sparql::predicate elem)) iri))
      (is (string= (tm-sparql::value (tm-sparql::predicate elem))
		   "http://prefix.value/predicate"))
      (is (eql (tm-sparql::elem-type (tm-sparql::object elem)) lit))
      (is (= (tm-sparql::value (tm-sparql::object elem)) 1234.5e12))
      (is (string= (tm-sparql::literal-type (tm-sparql::object elem))
		   *xml-double*))
      (is-false (tm-sparql::literal-lang (tm-sparql::object elem))))
    (is (string= (tm-sparql::parse-triple dummy-object query-3) ""))
    (is (= (length (tm-sparql::select-group dummy-object)) 3))
    (let ((elem (first (tm-sparql::select-group dummy-object))))
      (is (eql (tm-sparql::elem-type (tm-sparql::subject elem)) iri))
      (is (string= (tm-sparql::value (tm-sparql::subject elem))
		   "http://prefix.value/subject"))
      (is (eql (tm-sparql::elem-type (tm-sparql::predicate elem)) var))
      (is (string= (tm-sparql::value (tm-sparql::predicate elem))
		   "predicate"))
      (is (eql (tm-sparql::elem-type (tm-sparql::object elem)) lit))
      (is (string= (tm-sparql::value (tm-sparql::object elem)) "literal"))
      (is (string= (tm-sparql::literal-type (tm-sparql::object elem))
		   *xml-string*))
      (is (string= (tm-sparql::literal-lang (tm-sparql::object elem)) "en")))))


(test test-parse-group-2
  "Test various functionality of several functions responsible for parsing
   the SELECT-WHERE-statement."
  (let ((query-4 (concatenate 'string "<subject> <predicate> '''true'''^^"
			      *xml-boolean* "; pref:predicate-2 \"12\"^^"
			      *xml-integer* "}"))
	(query-5 (concatenate 'string "<subject> <predicate> '''false'''^^"
			      *xml-boolean* "; BASE <http://new.base/>"
			      "<predicate-2> \"abc\"^^"
			      *xml-string* "}"))
	(dummy-object (make-instance 'SPARQL-Query :query ""
				     :base "http://base.value/"))
	(lit 'TM-SPARQL::LITERAL)
	(iri 'TM-SPARQL::IRI))
    (is-true dummy-object)
    (tm-sparql::add-prefix dummy-object "pref" "http://prefix.value/")
    (is (string= (tm-sparql::parse-group dummy-object query-4) ""))
    (is (= (length (tm-sparql::select-group dummy-object)) 2))
    (let ((elem (second (tm-sparql::select-group dummy-object))))
      (is (eql (tm-sparql::elem-type (tm-sparql::subject elem)) iri))
      (is (string= (tm-sparql::value (tm-sparql::subject elem))
		   "http://base.value/subject"))
      (is (eql (tm-sparql::elem-type (tm-sparql::predicate elem)) iri))
      (is (string= (tm-sparql::value (tm-sparql::predicate elem))
		   "http://base.value/predicate"))
      (is (eql (tm-sparql::elem-type (tm-sparql::object elem)) lit))
      (is (eql (tm-sparql::value (tm-sparql::object elem)) t))
      (is (string= (tm-sparql::literal-type (tm-sparql::object elem))
		   *xml-boolean*))
      (is-false (tm-sparql::literal-lang (tm-sparql::object elem))))
    (let ((elem (first (tm-sparql::select-group dummy-object))))
      (is (eql (tm-sparql::elem-type (tm-sparql::subject elem)) iri))
      (is (string= (tm-sparql::value (tm-sparql::subject elem))
		   "http://base.value/subject"))
      (is (eql (tm-sparql::elem-type (tm-sparql::predicate elem)) iri))
      (is (string= (tm-sparql::value (tm-sparql::predicate elem))
		   "http://prefix.value/predicate-2"))
      (is (eql (tm-sparql::elem-type (tm-sparql::object elem)) lit))
      (is (= (tm-sparql::value (tm-sparql::object elem)) 12))
      (is (string= (tm-sparql::literal-type (tm-sparql::object elem))
		   *xml-integer*))
      (is-false (tm-sparql::literal-lang (tm-sparql::object elem))))
    (is (string= "http://base.value/" (tm-sparql::base-value dummy-object)))
    (is (string= (tm-sparql::parse-group dummy-object query-5) ""))
    (is (= (length (tm-sparql::select-group dummy-object)) 4))
    (is (string= "http://new.base/" (tm-sparql::base-value dummy-object)))
    (let ((elem (second (tm-sparql::select-group dummy-object))))
      (is (eql (tm-sparql::elem-type (tm-sparql::subject elem)) iri))
      (is (string= (tm-sparql::value (tm-sparql::subject elem))
		   "http://base.value/subject"))
      (is (eql (tm-sparql::elem-type (tm-sparql::predicate elem)) iri))
      (is (string= (tm-sparql::value (tm-sparql::predicate elem))
		   "http://base.value/predicate"))
      (is (eql (tm-sparql::elem-type (tm-sparql::object elem)) lit))
      (is (eql (tm-sparql::value (tm-sparql::object elem)) nil))
      (is (string= (tm-sparql::literal-type (tm-sparql::object elem))
		   *xml-boolean*))
      (is-false (tm-sparql::literal-lang (tm-sparql::object elem))))
    (let ((elem (first (tm-sparql::select-group dummy-object))))
      (is (eql (tm-sparql::elem-type (tm-sparql::subject elem)) iri))
      (is (string= (tm-sparql::value (tm-sparql::subject elem))
		   "http://base.value/subject"))
      (is (eql (tm-sparql::elem-type (tm-sparql::predicate elem)) iri))
      (is (string= (tm-sparql::value (tm-sparql::predicate elem))
		   "http://new.base/predicate-2"))
      (is (eql (tm-sparql::elem-type (tm-sparql::object elem)) lit))
      (is (string= (tm-sparql::value (tm-sparql::object elem)) "abc"))
      (is (string= (tm-sparql::literal-type (tm-sparql::object elem))
		   *xml-string*))
      (is-false (tm-sparql::literal-lang (tm-sparql::object elem))))))


(test test-set-result-1
  (with-fixture with-tm-filled-db ("data_base" *poems.xtm*)
    (with-revision 0
      (let* ((query-1 "BASE <http://some.where/>
                       SELECT ?subject ?predicate ?object WHERE {
                         ?subject ?predicate ?object }")
	     (query-2 "BASE <http://some.where/psis/poem/>
                       SELECT $subject ?predicate WHERE{
                         ?subject $predicate <zauberlehrling> }")
	     (query-3 "SELECT ?predicate ?subject WHERE
                         {?subject ?predicate \"Johann Wolfgang\" }")
	     (q-obj-1 (make-instance 'TM-SPARQL:SPARQL-Query :query query-1))
	     (q-obj-2 (make-instance 'TM-SPARQL:SPARQL-Query :query query-2))
	     (q-obj-3 (make-instance 'TM-SPARQL:SPARQL-Query :query query-3)))
	(is-true q-obj-1)
	(is (= (length (tm-sparql::select-group q-obj-1)) 1))
	(is-true q-obj-2)
	(is (= (length (tm-sparql::select-group q-obj-2)) 1))
	(is-true q-obj-3)
	(is (= (length (tm-sparql::select-group q-obj-3)) 1))
	(is-false (tm-sparql::subject-result
		   (first (tm-sparql::select-group q-obj-1))))
	(is-false (tm-sparql::predicate-result
		   (first (tm-sparql::select-group q-obj-1))))
	(is-false (tm-sparql::object-result
		   (first (tm-sparql::select-group q-obj-1))))
	(is (= (length (tm-sparql::subject-result
			(first (tm-sparql::select-group q-obj-2)))) 2))
	(is (= (length (tm-sparql::predicate-result
			(first (tm-sparql::select-group q-obj-2)))) 2))
	(is (= (length (tm-sparql::object-result
			(first (tm-sparql::select-group q-obj-2)))) 2))
	(let ((subj-1 (first (tm-sparql::subject-result
			      (first (tm-sparql::select-group q-obj-2)))))
	      (subj-2 (second (tm-sparql::subject-result
			       (first (tm-sparql::select-group q-obj-2)))))
	      (pred-1 (first (tm-sparql::predicate-result
			      (first (tm-sparql::select-group q-obj-2)))))
	      (pred-2 (second (tm-sparql::predicate-result
			       (first (tm-sparql::select-group q-obj-2)))))
	      (obj-1 (first (tm-sparql::object-result
			      (first (tm-sparql::select-group q-obj-2)))))
	      (obj-2 (second (tm-sparql::object-result
			       (first (tm-sparql::select-group q-obj-2))))))
	  (cond ((or (string= subj-1 "http://some.where/psis/author/goethe")
		     (string= subj-1 "http://some.where/psis/persons/goethe"))
		 (is (string= pred-1 "http://some.where/base-psis/written"))
		 (is (or (string= obj-1 "http://some.where/psis/poem/zauberlehrling")
			 (string= obj-1 "http://some.where/psis/der_zauberlehrling")))
		 (is (string= subj-2 "http://some.where/base-psis/poem"))
		 (is (string= pred-2 "http://psi.topicmaps.org/iso13250/model/instance"))
		 (is (or (string= obj-2 "http://some.where/psis/poem/zauberlehrling")
			 (string= obj-2 "http://some.where/psis/der_zauberlehrling"))))
		((string= subj-1 "http://some.where/base-psis/poem")
		 (is (string= pred-2 "http://some.where/base-psis/written"))
		 (is (or (string= obj-1 "http://some.where/psis/poem/zauberlehrling")
			 (string= obj-1 "http://some.where/psis/der_zauberlehrling")))
		 (is (or (string= subj-2 "http://some.where/psis/author/goethe")
			 (string= subj-2 "http://some.where/psis/persons/goethe")))
		 (is (string= pred-1 "http://psi.topicmaps.org/iso13250/model/type"))
		 (is (or (string= obj-2 "http://some.where/psis/poem/zauberlehrling")
			 (string= obj-2 "http://some.where/psis/der_zauberlehrling"))))
		(t
		 (is-true nil))))
	(is (= (length (tm-sparql::subject-result
			(first (tm-sparql::select-group q-obj-3)))) 1))
	(is (= (length (tm-sparql::predicate-result
			(first (tm-sparql::select-group q-obj-3)))) 1))
	(is (= (length (tm-sparql::object-result
			(first (tm-sparql::select-group q-obj-3)))) 1))
	(is (or (string= (first (tm-sparql::subject-result
				 (first (tm-sparql::select-group q-obj-3))))
			 "http://some.where/psis/author/goethe")
		(string= (first (tm-sparql::subject-result
				 (first (tm-sparql::select-group q-obj-3))))
			 "http://some.where/psis/persons/goethe")))
	(is (string= (first (tm-sparql::predicate-result
			     (first (tm-sparql::select-group q-obj-3))))
		     "http://some.where/base-psis/first-name"))
	(is (string= (first (tm-sparql::object-result
			     (first (tm-sparql::select-group q-obj-3))))
		     "Johann Wolfgang"))))))


(test test-set-result-2
  (with-fixture with-tm-filled-db ("data_base" *poems.xtm*)
    (with-revision 0
      (let* ((query-1 "PREFIX pref:<http://some.where/base-psis/>
                       SELECT $subject $object WHERE {
                         ?subject pref:written ?object }")
	     (query-2 "BASE <http://some.where/base-psis/>
                       SELECT $subject $object WHERE {
                         ?subject <first-name> ?object }")
	     (query-3 "BASE <http://some.where/psis/>
                       SELECT ?subject WHERE{
                         ?subject <http://some.where/base-psis/written>
                           <poem/zauberlehrling>}")
	     (q-obj-1 (make-instance 'TM-SPARQL:SPARQL-Query :query query-1))
	     (q-obj-2 (make-instance 'TM-SPARQL:SPARQL-Query :query query-2))
	     (q-obj-3 (make-instance 'TM-SPARQL:SPARQL-Query :query query-3)))
	(is-true q-obj-1)
	(is (= (length (tm-sparql::select-group q-obj-1)) 1))
	(is (= (length (tm-sparql::subject-result
			(first (tm-sparql::select-group q-obj-1)))) 4))
	(is (= (length (tm-sparql::predicate-result
			(first (tm-sparql::select-group q-obj-1)))) 4))
	(is (= (length (tm-sparql::object-result
			(first (tm-sparql::select-group q-obj-1)))) 4))
	(let* ((s-1 (first (tm-sparql::subject-result
			    (first (tm-sparql::select-group q-obj-1)))))
	       (s-2 (second (tm-sparql::subject-result
			     (first (tm-sparql::select-group q-obj-1)))))
	       (s-3 (third (tm-sparql::subject-result
			    (first (tm-sparql::select-group q-obj-1)))))
	       (s-4 (fourth (tm-sparql::subject-result
			     (first (tm-sparql::select-group q-obj-1)))))
	       (p-1 (first (tm-sparql::predicate-result
			    (first (tm-sparql::select-group q-obj-1)))))
	       (p-2 (second (tm-sparql::predicate-result
			     (first (tm-sparql::select-group q-obj-1)))))
	       (p-3 (third (tm-sparql::predicate-result
			    (first (tm-sparql::select-group q-obj-1)))))
	       (p-4 (fourth (tm-sparql::predicate-result
			     (first (tm-sparql::select-group q-obj-1)))))
	       (o-1 (first (tm-sparql::object-result
			    (first (tm-sparql::select-group q-obj-1)))))
	       (o-2 (second (tm-sparql::object-result
			    (first (tm-sparql::select-group q-obj-1)))))
	       (o-3 (third (tm-sparql::object-result
			    (first (tm-sparql::select-group q-obj-1)))))
	       (o-4 (fourth (tm-sparql::object-result
			     (first (tm-sparql::select-group q-obj-1))))))
	  (is (string= p-1 "http://some.where/base-psis/written"))
	  (is (string= p-2 "http://some.where/base-psis/written"))
	  (is (string= p-3 "http://some.where/base-psis/written"))
	  (is (string= p-4 "http://some.where/base-psis/written"))
	  (is (or (not (set-exclusive-or
			(list "http://some.where/psis/author/eichendorff"
			      "http://some.where/psis/author/schiller"
			      "http://some.where/psis/author/goethe")
			(list s-1 s-2 s-3 s-4)
			:test #'string=))
		  (not (set-exclusive-or
			(list "http://some.where/psis/author/eichendorff"
			      "http://some.where/psis/author/schiller"
			      "http://some.where/psis/persons/goethe")
			(list s-1 s-2 s-3 s-4)
			:test #'string=))))
	  (is-false (set-exclusive-or
		     (list "http://some.where/psis/poem/mondnacht"
			   "http://some.where/psis/poem/resignation"
			   "http://some.where/psis/poem/erlkoenig"
			   "http://some.where/psis/poem/zauberlehrling")
		     (list o-1 o-2 o-3 o-4)
		     :test #'string=)))
	(is-true q-obj-2)
	(is (= (length (tm-sparql::subject-result
			(first (tm-sparql::select-group q-obj-2)))) 3))
	(is (= (length (tm-sparql::predicate-result
			(first (tm-sparql::select-group q-obj-2)))) 3))
	(is (= (length (tm-sparql::object-result
			(first (tm-sparql::select-group q-obj-2)))) 3))
	(let* ((s-1 (first (tm-sparql::subject-result
			    (first (tm-sparql::select-group q-obj-2)))))
	       (s-2 (second (tm-sparql::subject-result
			     (first (tm-sparql::select-group q-obj-2)))))
	       (s-3 (third (tm-sparql::subject-result
			    (first (tm-sparql::select-group q-obj-2)))))
	       (p-1 (first (tm-sparql::predicate-result
			    (first (tm-sparql::select-group q-obj-2)))))
	       (p-2 (second (tm-sparql::predicate-result
			     (first (tm-sparql::select-group q-obj-2)))))
	       (p-3 (third (tm-sparql::predicate-result
			    (first (tm-sparql::select-group q-obj-2)))))
	       (o-1 (first (tm-sparql::object-result
			    (first (tm-sparql::select-group q-obj-2)))))
	       (o-2 (second (tm-sparql::object-result
			    (first (tm-sparql::select-group q-obj-2)))))
	       (o-3 (third (tm-sparql::object-result
			    (first (tm-sparql::select-group q-obj-2))))))
	  (string= p-1 "http://some.where/base-psis/first-name")
	  (string= p-2 "http://some.where/base-psis/first-name")
	  (string= p-3 "http://some.where/base-psis/first-name")
	  (cond ((string= o-1 "Johann Christoph Friedrich")
		 (is (string= s-1 "http://some.where/psis/author/schiller"))
		 (cond ((string= o-2 "Johann Wolfgang")
			(is (or (string= s-2 "http://some.where/psis/author/goethe")
				(string= s-2 "http://some.where/psis/persons/goethe")))
			(is (string= s-3 "http://some.where/psis/author/eichendorff"))
			(is (string= o-3 "Joseph Karl Benedikt")))
		       ((string= o-2 "Joseph Karl Benedikt")
			(is (string= s-2 "http://some.where/psis/author/eichendorff"))
			(is (or (string= s-3 "http://some.where/psis/author/goethe")
				(string= s-3 "http://some.where/psis/persons/goethe")))
			(is (string= o-3 "Johann Wolfgang")))
		       (t
			(is-true nil))))
		((string= o-1 "Johann Wolfgang")
		 (is (or (string= s-1 "http://some.where/psis/author/goethe")
			 (string= s-1 "http://some.where/psis/persons/goethe")))
		 (cond ((string= o-2 "Johann Christoph Friedrich")
			(is (string= s-2 "http://some.where/psis/author/schiller"))
			(is (string= s-3 "http://some.where/psis/author/eichendorff"))
			(is (string= o-3 "Joseph Karl Benedikt")))
		       ((string= o-2 "Joseph Karl Benedikt")
			(is (string= s-2 "http://some.where/psis/author/eichendorff"))
			(is (string= s-3 "http://some.where/psis/author/schiller"))
			(is (string= o-3 "Johann Christoph Friedrich")))
		       (t
			(is-true nil))))
		((string= o-1 "Joseph Karl Benedikt")
		 (is (string= s-1 "http://some.where/psis/author/eichendorff"))
		 (cond ((string= o-2 "Johann Wolfgang")
			(is (or (string= s-2 "http://some.where/psis/author/goethe")
				(string= s-2 "http://some.where/psis/persons/goethe")))
			(is (string= s-3 "http://some.where/psis/author/schiller"))
			(is (string= o-3 "Johann Christoph Friedrich")))
		       ((string= o-2 "Johann Christoph Friedrich")
			(is (string= s-2 "http://some.where/psis/author/schiller"))
			(is (or (string= s-3 "http://some.where/psis/author/goethe")
				(string= s-3 "http://some.where/psis/persons/goethe")))
			(is (string= o-3 "Johann Wolfgang")))
		       (t
			(is-true nil))))
		(t
		 (is-true nil))))
	(is-true q-obj-3)
	(is (= (length (tm-sparql::select-group q-obj-3)) 1))
	(is (= (length (tm-sparql::subject-result
			(first (tm-sparql::select-group q-obj-3)))) 1))
	(is (= (length (tm-sparql::predicate-result
			(first (tm-sparql::select-group q-obj-3)))) 1))
	(is (= (length (tm-sparql::object-result
			(first (tm-sparql::select-group q-obj-3)))) 1))
	(is (or (string= (first (tm-sparql::subject-result
				 (first (tm-sparql::select-group q-obj-3))))
			 "http://some.where/psis/author/goethe")
		(string= (first (tm-sparql::subject-result
				 (first (tm-sparql::select-group q-obj-3))))
			 "http://some.where/psis/persons/goethe")))
	(is (string= (first (tm-sparql::predicate-result
			     (first (tm-sparql::select-group q-obj-3))))
		     "http://some.where/base-psis/written"))
	(is (string= (first (tm-sparql::object-result
			     (first (tm-sparql::select-group q-obj-3))))
		     "http://some.where/psis/poem/zauberlehrling"))))))
      


(defun run-sparql-tests ()
  (it.bese.fiveam:run! 'sparql-test:sparql-tests))
