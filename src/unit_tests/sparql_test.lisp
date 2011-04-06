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
	 :base-tools
	 :it.bese.FiveAM
	 :TM-SPARQL
	 :exceptions
	 :unittests-constants
	 :fixtures
	 :d
	 :constants
	 :tm-sparql-constants)
  (:export :run-sparql-tests
	   :sparql-tests
	   :test-prefix-and-base
	   :test-variable-names
	   :test-parse-literals
	   :test-parse-triple-elem
	   :test-parse-group-1
	   :test-parse-group-2
	   :test-set-result-1
	   :test-set-result-2
	   :test-set-result-3
	   :test-set-result-4
	   :test-set-result-5
	   :test-result
	   :test-set-boundings
	   :test-set-unary-operators
	   :test-set-or-and-operators
	   :test-set-*-and-/-operators
	   :test-set-+-and---operators
	   :test-set-compare-operators
	   :test-set-functions
	   :test-module-1
	   :test-module-2
	   :test-module-3
	   :test-module-4
	   :test-module-5
	   :test-module-6
	   :test-module-7
	   :test-module-8
	   :test-module-9
	   :test-module-10
	   :test-module-11
	   :test-module-12
	   :test-module-13
	   :test-module-14
	   :test-module-15
	   :test-module-16))


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
    (is-true (find "var1" (TM-SPARQL::variables query-object-1)
		   :test #'string=))
    (is-true (find "var2" (TM-SPARQL::variables query-object-1)
		   :test #'string=))
    (is-true (find "var3" (TM-SPARQL::variables query-object-1)
		   :test #'string=))
    (is (= (length (TM-SPARQL::variables query-object-2)) 3))
    (is-true (find "var1" (TM-SPARQL::variables query-object-2)
		   :test #'string=))
    (is-true (find "var2" (TM-SPARQL::variables query-object-2)
		   :test #'string=))
    (is-true (find "var3" (TM-SPARQL::variables query-object-2)
		   :test #'string=))
    (is-true (find "*" (TM-SPARQL::variables query-object-3)
		   :test #'string=))
    (is-true (tm-sparql::*-p query-object-3))))


(test test-parse-literals
  "Tests the helper functions for parsing literals."
  (let ((query-1 "   \"literal-value\"@de.")
	(query-2 "true.")
	(query-3 "false}")
	(query-4 (concat "1234.43e10" (string #\tab)))
	(query-5 (concat "'''true'''^^" *xml-boolean* " ;"))
	(query-6 (concat "'123.4'^^" *xml-double* "." (string #\newline)))
	(query-7 "\"Just a test

literal with some \\\"quoted\\\" words!\"@en.")
	(query-8 (concat "'''12.4'''^^" *xml-integer* ". "))
	(query-9 (concat "\"13e4\"^^" *xml-boolean* " ."))
	(dummy-object (make-instance 'SPARQL-Query :query "")))
    (is-true dummy-object)
    (let ((res (tm-sparql::parse-literal-elem dummy-object query-1)))
      (is (string= (getf res :next-query) "."))
      (is (string= (tm-sparql::value (getf res :value))
		   "literal-value"))
      (is (string= (tm-sparql::literal-lang (getf res :value))
		   "de"))
      (is (string= (tm-sparql::literal-datatype (getf res :value))
		   *xml-string*))
      (is (eql (tm-sparql::elem-type (getf res :value)) 'TM-SPARQL::LITERAL)))
    (let ((res (tm-sparql::parse-literal-elem dummy-object query-2)))
      (is (string= (getf res :next-query) "."))
      (is (eql (tm-sparql::value (getf res :value)) t))
      (is-false (tm-sparql::literal-lang (getf res :value)))
      (is (string= (tm-sparql::literal-datatype (getf res :value))
		   *xml-boolean*))
      (is (eql (tm-sparql::elem-type (getf res :value)) 'TM-SPARQL::LITERAL)))
    (let ((res (tm-sparql::parse-literal-elem dummy-object query-3)))
      (is (string= (getf res :next-query) "}"))
      (is (eql (tm-sparql::value (getf res :value)) nil))
      (is-false (tm-sparql::literal-lang (getf res :value)))
      (is (string= (tm-sparql::literal-datatype (getf res :value))
		   *xml-boolean*))
      (is (eql (tm-sparql::elem-type (getf res :value)) 'TM-SPARQL::LITERAL)))
    (let ((res (tm-sparql::parse-literal-elem dummy-object query-4)))
      (is (string= (getf res :next-query) (string #\tab)))
      (is (= (tm-sparql::value (getf res :value)) 1234.43e10))
      (is-false (tm-sparql::literal-lang (getf res :value)))
      (is (string= (tm-sparql::literal-datatype (getf res :value))
		   *xml-double*))
      (is (eql (tm-sparql::elem-type (getf res :value)) 'TM-SPARQL::LITERAL)))
    (let ((res (tm-sparql::parse-literal-elem dummy-object query-5)))
      (is (string= (getf res :next-query) ";"))
      (is (eql (tm-sparql::value (getf res :value)) t))
      (is-false (tm-sparql::literal-lang (getf res :value)))
      (is (string= (tm-sparql::literal-datatype (getf res :value))
		   *xml-boolean*))
      (is (eql (tm-sparql::elem-type (getf res :value)) 'TM-SPARQL::LITERAL)))
    (let ((res (tm-sparql::parse-literal-elem dummy-object query-6)))
      (is (string= (getf res :next-query)
		   (concat "." (string #\newline))))
      (is (eql (tm-sparql::value (getf res :value)) 123.4d0))
      (is-false (tm-sparql::literal-lang (getf res :value)))
      (is (string= (tm-sparql::literal-datatype (getf res :value))
		   *xml-double*))
      (is (eql (tm-sparql::elem-type (getf res :value)) 'TM-SPARQL::LITERAL)))
    (let ((res (tm-sparql::parse-literal-elem dummy-object query-7)))
      (is (string= (getf res :next-query) "."))
      (is (string= (tm-sparql::value (getf res :value))
		   "Just a test

literal with some \\\"quoted\\\" words!"))
      (is (string= (tm-sparql::literal-lang (getf res :value)) "en"))
      (is (string= (tm-sparql::literal-datatype (getf res :value))
		   *xml-string*))
      (is (eql (tm-sparql::elem-type (getf res :value)) 'TM-SPARQL::LITERAL)))
    (signals sparql-parser-error
      (tm-sparql::parse-literal-elem dummy-object query-8))
    (signals sparql-parser-error
      (tm-sparql::parse-literal-elem dummy-object query-9))))


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
    (let ((res (tm-sparql::parse-triple-elem dummy-object query-1)))
      (is (string= (getf res :next-query) "."))
      (is (string= (tm-sparql::value (getf res :value)) "var1"))
      (is (eql (tm-sparql::elem-type (getf res :value)) var)))
    (let ((res (tm-sparql::parse-triple-elem dummy-object query-2)))
      (is (string= (getf res :next-query) ";"))
      (is (string= (tm-sparql::value (getf res :value)) "var2"))
      (is (eql (tm-sparql::elem-type (getf res :value)) var)))
    (let ((res (tm-sparql::parse-triple-elem dummy-object query-3)))
      (is (string= (getf res :next-query) "}"))
      (is (string= (tm-sparql::value (getf res :value)) "var3"))
      (is (eql (tm-sparql::elem-type (getf res :value)) var)))
    (let ((res (tm-sparql::parse-triple-elem dummy-object query-4)))
      (is (string= (getf res :next-query) "."))
      (is (string= (tm-sparql::value (getf res :value))
		   "http://full.url"))
      (is (eql (tm-sparql::elem-type (getf res :value)) iri)))
    (let ((res (tm-sparql::parse-triple-elem dummy-object query-5)))
      (is (string= (getf res :next-query) "}"))
      (is (string= (tm-sparql::value (getf res :value))
		   "http://base.value/url-suffix"))
      (is (eql (tm-sparql::elem-type (getf res :value)) iri)))
    (let ((res (tm-sparql::parse-triple-elem dummy-object query-6)))
      (is (string= (getf res :next-query) "."))
      (is (string= (tm-sparql::value (getf res :value))
		   "http://prefix.value/suffix"))
      (is (eql (tm-sparql::elem-type (getf res :value)) iri)))
    (let ((res (tm-sparql::parse-triple-elem dummy-object query-7)))
      (is (string= (getf res :next-query) "}"))
      (is (string= (tm-sparql::value (getf res :value))
		   "http://prefix.value/suffix"))
      (is (eql (tm-sparql::elem-type (getf res :value)) iri)))
    (signals sparql-parser-error 
      (tm-sparql::parse-triple-elem dummy-object query-8))))


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
      (is (string= (tm-sparql::literal-datatype (tm-sparql::object elem))
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
      (is (string= (tm-sparql::literal-datatype (tm-sparql::object elem))
		   *xml-string*))
      (is (string= (tm-sparql::literal-lang (tm-sparql::object elem)) "en")))))


(test test-parse-group-2
  "Test various functionality of several functions responsible for parsing
   the SELECT-WHERE-statement."
  (let ((query-4 (concat "<subject> <predicate> '''true'''^^"
			 *xml-boolean* "; pref:predicate-2 \"12\"^^"
			 *xml-integer* "}"))
	(query-5 (concat "<subject> <predicate> '''false'''^^"
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
      (is (string= (tm-sparql::literal-datatype (tm-sparql::object elem))
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
      (is (string= (tm-sparql::literal-datatype (tm-sparql::object elem))
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
      (is (string= (tm-sparql::literal-datatype (tm-sparql::object elem))
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
      (is (string= (tm-sparql::literal-datatype (tm-sparql::object elem))
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
	  (cond ((or (string= subj-1 "<http://some.where/psis/author/goethe>")
		     (string= subj-1 "<http://some.where/psis/persons/goethe>"))
		 (is (string= pred-1 "<http://some.where/base-psis/written>"))
		 (is (or (string= obj-1 "<http://some.where/psis/poem/zauberlehrling>")
			 (string= obj-1 "<http://some.where/psis/der_zauberlehrling>")))
		 (is (string= subj-2 "<http://some.where/base-psis/poem>"))
		 (is (string= pred-2 "<http://psi.topicmaps.org/iso13250/model/instance>"))
		 (is (or (string= obj-2 "<http://some.where/psis/poem/zauberlehrling>")
			 (string= obj-2 "<http://some.where/psis/der_zauberlehrling>"))))
		((string= subj-1 "<http://some.where/base-psis/poem>")
		 (is (string= pred-2 "<http://some.where/base-psis/written>"))
		 (is (or (string= obj-1 "<http://some.where/psis/poem/zauberlehrling>")
			 (string= obj-1 "<http://some.where/psis/der_zauberlehrling>")))
		 (is (or (string= subj-2 "<http://some.where/psis/author/goethe>")
			 (string= subj-2 "<http://some.where/psis/persons/goethe>")))
		 (is (string= pred-1 "<http://psi.topicmaps.org/iso13250/model/instance>"))
		 (is (or (string= obj-2 "<http://some.where/psis/poem/zauberlehrling>")
			 (string= obj-2 "<http://some.where/psis/der_zauberlehrling>"))))
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
			 "<http://some.where/psis/author/goethe>")
		(string= (first (tm-sparql::subject-result
				 (first (tm-sparql::select-group q-obj-3))))
			 "<http://some.where/psis/persons/goethe>")))
	(is (string= (first (tm-sparql::predicate-result
			     (first (tm-sparql::select-group q-obj-3))))
		     "<http://some.where/base-psis/first-name>"))
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
	(is-true q-obj-2)
	(is-true q-obj-3)
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
	  (is (string= p-1 "<http://some.where/base-psis/written>"))
	  (is (string= p-2 "<http://some.where/base-psis/written>"))
	  (is (string= p-3 "<http://some.where/base-psis/written>"))
	  (is (string= p-4 "<http://some.where/base-psis/written>"))
	  (is (or (not (set-exclusive-or
			(list "<http://some.where/psis/author/eichendorff>"
			      "<http://some.where/psis/author/schiller>"
			      "<http://some.where/psis/author/goethe>")
			(list s-1 s-2 s-3 s-4)
			:test #'string=))
		  (not (set-exclusive-or
			(list "<http://some.where/psis/author/eichendorff>"
			      "<http://some.where/psis/author/schiller>"
			      "<http://some.where/psis/persons/goethe>")
			(list s-1 s-2 s-3 s-4)
			:test #'string=))))
	  (is-false (set-exclusive-or
		     (list "<http://some.where/psis/poem/mondnacht>"
			   "<http://some.where/psis/poem/resignation>"
			   "<http://some.where/psis/poem/erlkoenig>"
			   "<http://some.where/psis/poem/zauberlehrling>")
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
	  (string= p-1 "<http://some.where/base-psis/first-name>")
	  (string= p-2 "<http://some.where/base-psis/first-name>")
	  (string= p-3 "<http://some.where/base-psis/first-name>")
	  (cond ((string= o-1 "Johann Christoph Friedrich")
		 (is (string= s-1 "<http://some.where/psis/author/schiller>"))
		 (cond ((string= o-2 "Johann Wolfgang")
			(is (or (string= s-2 "<http://some.where/psis/author/goethe>")
				(string= s-2 "<http://some.where/psis/persons/goethe>")))
			(is (string= s-3 "<http://some.where/psis/author/eichendorff>"))
			(is (string= o-3 "Joseph Karl Benedikt")))
		       ((string= o-2 "Joseph Karl Benedikt")
			(is (string= s-2 "<http://some.where/psis/author/eichendorff>"))
			(is (or (string= s-3 "<http://some.where/psis/author/goethe>")
				(string= s-3 "<http://some.where/psis/persons/goethe>")))
			(is (string= o-3 "Johann Wolfgang")))
		       (t
			(is-true nil))))
		((string= o-1 "Johann Wolfgang")
		 (is (or (string= s-1 "<http://some.where/psis/author/goethe>")
			 (string= s-1 "<http://some.where/psis/persons/goethe>")))
		 (cond ((string= o-2 "Johann Christoph Friedrich")
			(is (string= s-2 "<http://some.where/psis/author/schiller>"))
			(is (string= s-3 "<http://some.where/psis/author/eichendorff>"))
			(is (string= o-3 "Joseph Karl Benedikt")))
		       ((string= o-2 "Joseph Karl Benedikt")
			(is (string= s-2 "<http://some.where/psis/author/eichendorff>"))
			(is (string= s-3 "<http://some.where/psis/author/schiller>"))
			(is (string= o-3 "Johann Christoph Friedrich")))
		       (t
			(is-true nil))))
		((string= o-1 "Joseph Karl Benedikt")
		 (is (string= s-1 "<http://some.where/psis/author/eichendorff>"))
		 (cond ((string= o-2 "Johann Wolfgang")
			(is (or (string= s-2 "<http://some.where/psis/author/goethe>")
				(string= s-2 "<http://some.where/psis/persons/goethe>")))
			(is (string= s-3 "<http://some.where/psis/author/schiller>"))
			(is (string= o-3 "Johann Christoph Friedrich")))
		       ((string= o-2 "Johann Christoph Friedrich")
			(is (string= s-2 "<http://some.where/psis/author/schiller>"))
			(is (or (string= s-3 "<http://some.where/psis/author/goethe>")
				(string= s-3 "<http://some.where/psis/persons/goethe>")))
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
			 "<http://some.where/psis/author/goethe>")
		(string= (first (tm-sparql::subject-result
				 (first (tm-sparql::select-group q-obj-3))))
			 "<http://some.where/psis/persons/goethe>")))
	(is (string= (first (tm-sparql::predicate-result
			     (first (tm-sparql::select-group q-obj-3))))
		     "<http://some.where/base-psis/written>"))
	(is (string= (first (tm-sparql::object-result
			     (first (tm-sparql::select-group q-obj-3))))
		     "<http://some.where/psis/poem/zauberlehrling>"))))))


(test test-set-result-3
  (with-fixture with-tm-filled-db ("data_base" *poems.xtm*)
    (with-revision 0
      (let* ((query-1 "PREFIX pref:<http://some.where/base-psis/>
                       SELECT $subject WHERE {
                         ?subject pref:author-info \"http://de.wikipedia.org/wiki/Johann_Wolfgang_von_Goethe\"^^http://www.w3.org/2001/XMLSchema#anyURI }")
	     (query-2 "BASE <http://some.where/base-psis/>
                       SELECT $subject WHERE {
                         ?subject <last-name> 'von Goethe'^^anyType }")
	     (query-3 "BASE <http://some.where/base-psis/>
                       SELECT ?subject WHERE{
                         ?subject <http://some.where/base-psis/last-name>
                           'Johann Wolfgang' }")
	     (query-4 "PREFIX pref-1:<http://some.where/base-psis/>
                       PREFIX pref-2:<http://some.where/psis/>
                       SELECT ?subject WHERE {
                         ?subject pref-1:written pref-2:poem/resignation }")
	     (q-obj-1 (make-instance 'TM-SPARQL:SPARQL-Query :query query-1))
	     (q-obj-2 (make-instance 'TM-SPARQL:SPARQL-Query :query query-2))
	     (q-obj-3 (make-instance 'TM-SPARQL:SPARQL-Query :query query-3))
	     (q-obj-4 (make-instance 'TM-SPARQL:SPARQL-Query :query query-4)))
	(is-true q-obj-1)
	(is-true q-obj-2)
	(is-true q-obj-3)
	(is-true q-obj-4)
	(is (= (length (tm-sparql::select-group q-obj-1)) 1))
	(is (= (length (tm-sparql::select-group q-obj-2)) 1))
	(is (= (length (tm-sparql::select-group q-obj-3)) 1))
	(is (= (length (tm-sparql::select-group q-obj-4)) 1))
	(is (= (length (tm-sparql::predicate-result
			(first (tm-sparql::select-group q-obj-1)))) 1))
	(is (= (length (tm-sparql::predicate-result
			(first (tm-sparql::select-group q-obj-2)))) 0))
	(is (= (length (tm-sparql::predicate-result
			(first (tm-sparql::select-group q-obj-3)))) 0))
	(is (or (string= (first (tm-sparql::subject-result
				 (first (tm-sparql::select-group q-obj-1))))
			 "<http://some.where/psis/author/goethe>")
		(string= (first (tm-sparql::subject-result
				 (first (tm-sparql::select-group q-obj-1))))
			 "<http://some.where/psis/persons/goethe>")))
	(is (string= (first (tm-sparql::predicate-result
			     (first (tm-sparql::select-group q-obj-1))))
		     "<http://some.where/base-psis/author-info>"))
	(is (string= (first (tm-sparql::object-result
			     (first (tm-sparql::select-group q-obj-1))))
		     "http://de.wikipedia.org/wiki/Johann_Wolfgang_von_Goethe"))
	(is (string= (first (tm-sparql::subject-result
			     (first (tm-sparql::select-group q-obj-4))))
		     "<http://some.where/psis/author/schiller>"))
	(is (string= (first (tm-sparql::predicate-result
			     (first (tm-sparql::select-group q-obj-4))))
		     "<http://some.where/base-psis/written>"))
	(is (string= (first (tm-sparql::object-result
			     (first (tm-sparql::select-group q-obj-4))))
		     "<http://some.where/psis/poem/resignation>"))))))


(test test-set-result-4
  (with-fixture with-tm-filled-db ("data_base" *poems.xtm*)
    (with-revision 0
      (let* ((query-1 "BASE <http://some.where/>
                       SELECT ?predicate ?object WHERE {
                         <psis/author/goethe> ?predicate ?object}")
	     (query-2 "BASE <http://some.where/>
                       SELECT ?predicate ?object WHERE {
                         <psis/poem/zauberlehrling> ?predicate ?object}")
	     (query-3 "BASE <http://some.where/>
                       SELECT ?predicate WHERE {
                         <psis/persons/goethe> ?predicate <psis/poem/zauberlehrling>}")
	     (q-obj-1 (make-instance 'TM-SPARQL:SPARQL-Query :query query-1))
	     (q-obj-2 (make-instance 'TM-SPARQL:SPARQL-Query :query query-2))
	     (q-obj-3 (make-instance 'TM-SPARQL:SPARQL-Query :query query-3)))
	(is-true q-obj-1)
	(is-true q-obj-2)
	(is-true q-obj-3)
	(is (= (length (tm-sparql::select-group q-obj-1)) 1))
	(is (= (length (tm-sparql::select-group q-obj-2)) 1))
	(is (= (length (tm-sparql::select-group q-obj-3)) 1))
	(is (= (length (tm-sparql::subject-result
			(first (tm-sparql::select-group q-obj-1)))) 7))
	(is (= (length (tm-sparql::subject-result
			(first (tm-sparql::select-group q-obj-2)))) 4))
	(is (= (length (tm-sparql::subject-result
			(first (tm-sparql::select-group q-obj-3)))) 1))
	(is-true (or (null (set-exclusive-or
			    (list "<http://some.where/psis/author/goethe>")
			    (tm-sparql::subject-result
			     (first (tm-sparql::select-group q-obj-1)))
			    :test #'string=))
		     (null (set-exclusive-or
			    (list "<http://some.where/psis/persons/goethe>")
			    (tm-sparql::subject-result
			     (first (tm-sparql::select-group q-obj-1)))
			    :test #'string=))))
	(let ((predicates (tm-sparql::predicate-result
			   (first (tm-sparql::select-group q-obj-1)))))
	  (is (= (count "<http://some.where/base-psis/written>" predicates
			:test #'string=) 2))
	  (is (= (count "<http://some.where/base-psis/place>" predicates
			:test #'string=) 1))
	  (is (= (count "<http://some.where/base-psis/first-name>" predicates
			:test #'string=) 1))
	  (is (= (count "<http://some.where/base-psis/last-name>" predicates
			:test #'string=) 1))
	  (is (= (count "<http://some.where/base-psis/author-info>" predicates
			:test #'string=) 1))
	  (is (= (count "<http://psi.topicmaps.org/iso13250/model/type>" predicates
			:test #'string=) 1)))
	(let ((objects (tm-sparql::object-result
			(first (tm-sparql::select-group q-obj-1)))))
	  (is (= (count "<http://some.where/psis/poem/erlkoenig>" objects
			:test #'string=) 1))
	  (is (or (= (count "<http://some.where/psis/poem/der_zauberlehrling>"
			    objects :test #'string=) 1)
		  (= (count "<http://some.where/psis/poem/zauberlehrling>" objects
			    :test #'string=) 1)))
	  (is (or (= (count "<http://some.where/base-psis/author>" objects
			    :test #'string=) 1)
		  (= (count "<http://some.where/base-psis/author-psi>" objects
			    :test #'string=) 1)))
	  (is (= (count "http://de.wikipedia.org/wiki/Johann_Wolfgang_von_Goethe"
			objects :test #'string=) 1))
	  (is (= (count "von Goethe" objects :test #'string=) 1))
	  (is (= (count "Johann Wolfgang" objects :test #'string=) 1))
	  (is (= (count "<http://some.where/psis/region/frankfurt_am_main>"
			objects :test #'string=) 1)))
	(is-true (or (null (set-exclusive-or
			    (list "<http://some.where/psis/poem/der_zauberlehrling>")
			    (tm-sparql::subject-result
			     (first (tm-sparql::select-group q-obj-2)))
			    :test #'string=))
		     (null (set-exclusive-or
			    (list "<http://some.where/psis/poem/zauberlehrling>")
			    (tm-sparql::subject-result
			     (first (tm-sparql::select-group q-obj-2)))
			    :test #'string=))))
	(let ((predicates (tm-sparql::predicate-result
			   (first (tm-sparql::select-group q-obj-2)))))
	  (is (= (count "<http://some.where/base-psis/writer>" predicates
			:test #'string=) 1))
	  (is (= (count "<http://some.where/base-psis/title>" predicates
			:test #'string=) 1))
	  (is (= (count "<http://some.where/base-psis/poem-content>" predicates
			:test #'string=) 1))
	  (is (= (count "<http://psi.topicmaps.org/iso13250/model/type>" predicates
			:test #'string=) 1)))
	(let ((objects (tm-sparql::object-result
			(first (tm-sparql::select-group q-obj-2)))))
	  (is (or (= (count "<http://some.where/psis/author/goethe>" objects
			    :test #'string=) 1)
		  (= (count "<http://some.where/psis/persons/goethe>" objects
			    :test #'string=) 1)))
	  (is (= (count "Der Zauberlehrling" objects :test #'string=) 1))
	  (is (= (count "<http://some.where/base-psis/poem>"
			objects :test #'string=) 1))
	  ;do not check the entire poem content => too long
	  )
	(is (or (string= "<http://some.where/psis/author/goethe>"
			 (first (tm-sparql::subject-result
				 (first (tm-sparql::select-group q-obj-3)))))
		(string= "<http://some.where/psis/persons/goethe>"
			 (first (tm-sparql::subject-result
				 (first (tm-sparql::select-group q-obj-3)))))))
	(is (string= "<http://some.where/base-psis/written>"
		     (first (tm-sparql::predicate-result
			     (first (tm-sparql::select-group q-obj-3))))))
	(is (or (string= "<http://some.where/psis/poem/der_zauberlehrling>"
			 (first (tm-sparql::object-result
				 (first (tm-sparql::select-group q-obj-3)))))
		(string= "<http://some.where/psis/poem/zauberlehrling>"
			 (first (tm-sparql::object-result
				 (first (tm-sparql::select-group q-obj-3)))))))))))


(test test-set-result-5
  (with-fixture with-tm-filled-db ("data_base" *poems.xtm*)
    (with-revision 0
      (let* ((query-1 "BASE <http://some.where/>
                       SELECT ?predicate WHERE {
                         <psis/author/goethe> ?predicate 'Johann Wolfgang'}")
	     (query-2 "BASE <http://some.where/>
                       SELECT ?object WHERE {
                         <psis/author/goethe> <base-psis/written> ?object}")
	     (query-3 "BASE <http://some.where/>
                       SELECT ?object WHERE {
                         <psis/persons/goethe> <base-psis/last-name> ?object.
                         <does/not/exist> <any-predicate> ?object}")
	     (q-obj-1 (make-instance 'TM-SPARQL:SPARQL-Query :query query-1))
	     (q-obj-2 (make-instance 'TM-SPARQL:SPARQL-Query :query query-2))
	     (q-obj-3 (make-instance 'TM-SPARQL:SPARQL-Query :query query-3)))
	(is-true q-obj-1)
	(is-true q-obj-2)
	(is-true q-obj-3)
	(is (= (length (tm-sparql::select-group q-obj-1)) 1))
	(is (= (length (tm-sparql::select-group q-obj-2)) 1))
	(is (= (length (tm-sparql::select-group q-obj-3)) 2))
	(is (= (length (tm-sparql::subject-result
			(first (tm-sparql::select-group q-obj-1)))) 1))
	(is (= (length (tm-sparql::subject-result
			(first (tm-sparql::select-group q-obj-2)))) 2))
	(is (= (length (tm-sparql::subject-result
			(first (tm-sparql::select-group q-obj-3)))) 0))
	(is (= (length (tm-sparql::subject-result
			(second (tm-sparql::select-group q-obj-3)))) 0))
	(is (or (string= "<http://some.where/psis/author/goethe>"
			 (first (tm-sparql::subject-result
				 (first (tm-sparql::select-group q-obj-1)))))
		(string= "<http://some.where/psis/persons/goethe>"
			 (first (tm-sparql::subject-result
				 (first (tm-sparql::select-group q-obj-1)))))))
	(is (string= "<http://some.where/base-psis/first-name>"
		     (first (tm-sparql::predicate-result
			     (first (tm-sparql::select-group q-obj-1))))))
	(is (string= "Johann Wolfgang"
		     (first (tm-sparql::object-result
			     (first (tm-sparql::select-group q-obj-1))))))
	(is (or (string= "<http://some.where/psis/author/goethe>"
			 (first (tm-sparql::subject-result
				 (first (tm-sparql::select-group q-obj-2)))))
		(string= "<http://some.where/psis/persons/goethe>"
			 (first (tm-sparql::subject-result
				 (first (tm-sparql::select-group q-obj-2)))))))
	(is (string= "<http://some.where/base-psis/written>"
		     (first (tm-sparql::predicate-result
			     (first (tm-sparql::select-group q-obj-2))))))
	(is (or (string= "<http://some.where/psis/poem/zauberlehrling>"
			 (first (tm-sparql::object-result
				 (first (tm-sparql::select-group q-obj-2)))))
		(string= "<http://some.where/psis/poem/der_zauberlehrling>"
			 (first (tm-sparql::object-result
				 (first (tm-sparql::select-group q-obj-2)))))
		(string= "<http://some.where/psis/poem/erlkoenig>"
			 (first (tm-sparql::object-result
				 (first (tm-sparql::select-group q-obj-2)))))))
	(is (or (string= "<http://some.where/psis/author/goethe>"
			 (second (tm-sparql::subject-result
				  (first (tm-sparql::select-group q-obj-2)))))
		(string= "<http://some.where/psis/persons/goethe>"
			 (second (tm-sparql::subject-result
				  (first (tm-sparql::select-group q-obj-2)))))))
	(is (string= "<http://some.where/base-psis/written>"
		     (second (tm-sparql::predicate-result
			      (first (tm-sparql::select-group q-obj-2))))))
	(is (or (string= "<http://some.where/psis/poem/zauberlehrling>"
			 (second (tm-sparql::object-result
				  (first (tm-sparql::select-group q-obj-2)))))
		(string= "<http://some.where/psis/poem/der_zauberlehrling>"
			 (second (tm-sparql::object-result
				  (first (tm-sparql::select-group q-obj-2)))))
		(string= "<http://some.where/psis/poem/erlkoenig>"
			 (second (tm-sparql::object-result
				  (first (tm-sparql::select-group q-obj-2)))))))
	(is-false (first (tm-sparql::subject-result
			  (first (tm-sparql::select-group q-obj-3)))))
	(is-false (first (tm-sparql::predicate-result
			  (first (tm-sparql::select-group q-obj-3)))))
	(is-false (first (tm-sparql::object-result
			  (first (tm-sparql::select-group q-obj-3)))))
	(is-false (first (tm-sparql::subject-result
			  (second (tm-sparql::select-group q-obj-3)))))
	(is-false (first (tm-sparql::predicate-result
			  (second (tm-sparql::select-group q-obj-3)))))
	(is-false (first (tm-sparql::object-result
			  (second (tm-sparql::select-group q-obj-3)))))))))


(test test-result
  (with-fixture with-tm-filled-db ("data_base" *poems.xtm*)
    (with-revision 0
      (let* ((query-1 "PREFIX author:<http://some.where/psis/author/>
                     PREFIX poem:<http://some.where/psis/poem/>
                     PREFIX basePSIs:<http://some.where/base-psis/>
                     SELECT ?poems ?poets WHERE {
                         ?poets a basePSIs:author .
                         ?poets basePSIs:written ?poems.
                         ?poems basePSIs:title 'Der Erlkönig' .
                         ?poems a basePSIs:poem}")
	     (q-obj-1 (make-instance 'TM-SPARQL:SPARQL-Query :query query-1))
	     (query-2 "PREFIX author:<http://some.where/psis/author/>
                     PREFIX poem:<http://some.where/psis/poem/>
                     PREFIX basePSIs:<http://some.where/base-psis/>
                     SELECT * WHERE {
                         ?poems a basePSIs:poem.
                         <goethe> <last-name> 'von Goethe' .
                         ?poems basePSIs:title ?titles}")
	     (q-obj-2 (make-instance 'TM-SPARQL:SPARQL-Query :query query-2)))
	(is-true q-obj-1)
	(is-true q-obj-2)
	(is (= (length (tm-sparql::select-group q-obj-1)) 4))
	(is (= (length (tm-sparql::select-group q-obj-2)) 3))
	(is (= (length (result q-obj-1)) 2))
	(if (string= (getf (first (result q-obj-1)) :variable) "poets")
	    (progn
	      (is (= (length (getf (first (result q-obj-1)) :result)) 1))
	      (is (or (string= (first (getf (first (result q-obj-1)) :result))
			       "<http://some.where/psis/author/goethe>")
		      (string= (first (getf (first (result q-obj-1)) :result))
			       "<http://some.where/psis/persons/goethe>")))
	      (is (= (length (getf (second (result q-obj-1)) :result)) 1))
	      (is (string= (first (getf (second (result q-obj-1)) :result))
			   "<http://some.where/psis/poem/erlkoenig>"))
	      (is (string= (getf (second (result q-obj-1)) :variable) "<poems")))
	    (progn
	      (is (= (length (getf (second (result q-obj-1)) :result)) 1))
	      (is (or (string= (first (getf (second (result q-obj-1)) :result))
			       "<http://some.where/psis/author/goethe>")
		      (string= (first (getf (second (result q-obj-1)) :result))
			       "<http://some.where/psis/persons/goethe>")))
	      (is (= (length (getf (first (result q-obj-1)) :result)) 1))
	      (is (string= (first (getf (first (result q-obj-1)) :result))
			   "<http://some.where/psis/poem/erlkoenig>"))
	      (is (string= (getf (first (result q-obj-1)) :variable) "poems"))))
	(is (= (length (result q-obj-2)) 2))
	(if (string= (getf (first (result q-obj-2)) :variable) "titles")
	    (progn
	      (is (= (length (getf (first (result q-obj-2)) :result)) 4))
	      (is-true
	       (find "Mondnacht"
		     (getf (first (result q-obj-2)) :result) :test #'string=))
	      (is-true
	       (find "Der Erlkönig"
		     (getf (first (result q-obj-2)) :result) :test #'string=))
	      (is-true
	       (find "Der Zauberlehrling"
		     (getf (first (result q-obj-2)) :result) :test #'string=))
	      (is-true
	       (find "Resignation - Eine Phantasie"
		     (getf (first (result q-obj-2)) :result) :test #'string=))
	      (string= (getf (second (result q-obj-2)) :variable) "poems")
	      (is-true
	       (find "<http://some.where/psis/poem/mondnacht>"
		     (getf (second (result q-obj-2)) :result) :test #'string=))
	      (is-true
	       (find "<http://some.where/psis/poem/resignation>"
		     (getf (second (result q-obj-2)) :result) :test #'string=))
	      (is-true
	       (find "<http://some.where/psis/poem/erlkoenig>"
		     (getf (second (result q-obj-2)) :result) :test #'string=))
	      (is-true
	       (or
		(find "<http://some.where/psis/poem/zauberlehrling>"
		      (getf (second (result q-obj-2)) :result) :test #'string=)
		(find "<http://some.where/psis/poem/der_zauberlehrling>"
		      (getf (second (result q-obj-2)) :result) :test #'string=))))
	    (progn
	      (is (= (length (getf (second (result q-obj-2)) :result)) 4))
	      (is-true
	       (find "Mondnacht"
		     (getf (second (result q-obj-2)) :result) :test #'string=))
	      (is-true
	       (find "Der Erlkönig"
		     (getf (second (result q-obj-2)) :result) :test #'string=))
	      (is-true
	       (find "Der Zauberlehrling"
		     (getf (second (result q-obj-2)) :result) :test #'string=))
	      (is-true
	       (find "Resignation - Eine Phantasie"
		     (getf (second (result q-obj-2)) :result) :test #'string=))
	      (string= (getf (first (result q-obj-2)) :variable) "poems")
	      (is-true
	       (find "<http://some.where/psis/poem/mondnacht>"
		     (getf (first (result q-obj-2)) :result) :test #'string=))
	      (is-true
	       (find "<http://some.where/psis/poem/resignation>"
		     (getf (first (result q-obj-2)) :result) :test #'string=))
	      (is-true
	       (find "<http://some.where/psis/poem/erlkoenig>"
		     (getf (first (result q-obj-2)) :result) :test #'string=))
	      (is-true
	       (or
		(find "<http://some.where/psis/poem/zauberlehrling>"
		      (getf (first (result q-obj-2)) :result) :test #'string=)
		(find "<http://some.where/psis/poem/der_zauberlehrling>"
		      (getf (first (result q-obj-2)) :result) :test #'string=)))))))))


(test test-set-boundings
  "Tests various cases of the function set-boundings"
  (let* ((dummy-object (make-instance 'TM-SPARQL::SPARQL-Query :query "  "))
	 (str-1 "BOUND((  (?var)  )) || (isLITERAL($var) && ?var = 'abc')}")
	 (result-1 (tm-sparql::set-boundings dummy-object str-1))
	 (str-2
	  "(REGEX(?var1, '''''', ?var3) || (?var1 > ?var3 && (STR( ?var) = \"abc\")))}")
	 (result-2 (tm-sparql::set-boundings dummy-object str-2))
	 (str-3
	  "DATATYPE(?var3) || +?var1 = -?var2
           ?var1 ?var2 ?var3}")
	 (result-3 (tm-sparql::set-boundings dummy-object str-3))
	 (str-4 "DATATYPE(?var3) ||isLITERAL(+?var1 = -?var2)}")
	 (result-4 (tm-sparql::set-boundings dummy-object str-4))
	 (str-5 "DATATYPE(?var3) ||(isLITERAL  (+?var1 = -?var2))}")
	 (result-5 (tm-sparql::set-boundings dummy-object str-5)))
    (is-true result-1)
    (is-true result-2)
    (is (string= (getf result-1 :filter-string)
		 "BOUND((progn   (progn ?var)  )) || (progn isLITERAL($var) && ?var = \"abc\")"))
    (is (string= (getf result-1 :next-query) "}"))
    (is (string= (getf result-2 :filter-string)
		 "(progn REGEX(?var1, \"\", ?var3) || (progn ?var1 > ?var3 && (progn STR( ?var) = \"abc\")))"))
    (is (string= (getf result-2 :next-query) "}"))
    (is (string= (getf result-3 :filter-string)
		 "DATATYPE(?var3) || +?var1 = -?var2"))
    (is (string= (getf result-3 :next-query) (subseq str-3 34)))
    (is (string= (getf result-4 :filter-string)
		 "DATATYPE(?var3) ||isLITERAL(+?var1 = -?var2)"))
    (is (string= (getf result-4 :next-query) "}"))
    (is (string= (getf result-5 :filter-string)
		 "DATATYPE(?var3) ||(progn isLITERAL  (+?var1 = -?var2))"))
    (is (string= (getf result-5 :next-query) "}"))))


(test test-set-unary-operators
  "Tests various cases of the function set-unary-operators."
  (let* ((dummy-object (make-instance 'TM-SPARQL::SPARQL-Query :query "  "))
	 (str-1 "BOUND(?var1)||(!(+(-(?var1))))}")
	 (str-2 "!BOUND(?var1) = false}")
	 (str-3 "+?var1=-$var2}")
	 (str-4 "!'a\"b\"c' && (+12 = - 14)}")
	 (str-5 "!'a(+c)' && (+12 = - 14)}")
	 (str-6 "!'abc)def'}")
	 (result-1
	  (getf (tm-sparql::set-boundings dummy-object str-1) :filter-string))
	 (result-1-1 (tm-sparql::set-unary-operators dummy-object result-1))
	 (result-2
	  (getf (tm-sparql::set-boundings dummy-object str-2) :filter-string))
	 (result-2-1 (tm-sparql::set-unary-operators dummy-object result-2))
	 (result-3
	  (getf (tm-sparql::set-boundings dummy-object str-3) :filter-string))
	 (result-3-1
	  (tm-sparql::set-unary-operators dummy-object result-3))
	 (result-4
	  (getf (tm-sparql::set-boundings dummy-object str-4) :filter-string))
	 (result-4-1
	  (tm-sparql::set-unary-operators dummy-object result-4))
	 (result-5
	  (getf (tm-sparql::set-boundings dummy-object str-5) :filter-string))
	 (result-5-1
	  (tm-sparql::set-unary-operators dummy-object result-5))
	 (result-6
	  (getf (tm-sparql::set-boundings dummy-object str-6) :filter-string))
	 (result-6-1
	  (tm-sparql::set-unary-operators dummy-object result-6)))
    (is-true result-1)
    (is-true result-1-1)
    (is-true result-2)
    (is-true result-2-1)
    (is-true result-3)
    (is-true result-3-1)
    (is-true result-4)
    (is-true result-4-1)
    (is-true result-5)
    (is-true result-5-1)
    (is-true result-6)
    (is-true result-6-1)
    (is (string=
	 result-1-1
	 "BOUND(?var1)||(progn (not (progn (one+ (progn (one- (progn ?var1)))))))"))
    (is (string= result-2-1 "(not BOUND(?var1)) = false"))
    (is (string= result-3-1 "(one+ ?var1)=(one- $var2)"))
    (is (string= result-4-1 "(not \"a\\\"b\\\"c\") && (progn (one+ 12) = (one- 14))"))
    (is (string= result-5-1 "(not \"a(+c)\") && (progn (one+ 12) = (one- 14))"))
    (is (string= result-6-1 "(not \"abc)def\")"))))
	 

(test test-set-or-and-operators
  "Tests various cases of the function set-or-and-operators."
  (let* ((dummy-object (make-instance 'TM-SPARQL::SPARQL-Query :query "  "))
	 (str-1 "isLITERAL(STR(?var))||?var = 12 && true}")
	 (str-2 "(true != false || !false ) && 12 < 14 || !isLITERAL(?var)}")
	 (str-3 "isLITERAL('a(bc||def') && 'abc)def'}")
	 (str-4 "(a && (b || c))}")
	 (str-5 "(b || c) && a}")
	 (result-1
	  (getf (tm-sparql::set-boundings dummy-object str-1) :filter-string))
	 (result-1-1 (tm-sparql::set-or-and-operators dummy-object result-1 result-1))
	 (result-2
	  (getf (tm-sparql::set-boundings dummy-object str-2) :filter-string))
	 (result-2-1 (tm-sparql::set-or-and-operators dummy-object result-2 result-2))
	 (result-3
	  (getf (tm-sparql::set-boundings dummy-object str-3) :filter-string))
	 (result-3-1 (tm-sparql::set-or-and-operators dummy-object result-3 result-3))
	 (result-4
	  (getf (tm-sparql::set-boundings dummy-object str-4) :filter-string))
	 (result-4-1 (tm-sparql::set-or-and-operators dummy-object result-4 result-4))
	 (result-5
	  (getf (tm-sparql::set-boundings dummy-object str-5) :filter-string))
	 (result-5-1 (tm-sparql::set-or-and-operators dummy-object result-5 result-5)))
    (is-true result-1)
    (is-true result-1-1)
    (is-true result-2)
    (is-true result-2-1)
    (is-true result-3)
    (is-true result-3-1)
    (is-true result-4)
    (is-true result-4-1)
    (is-true result-5)
    (is-true result-5-1)
    (is (string= (string-replace result-1-1 " " "")
		 "(and(progn(or(prognisLITERAL(STR(?var)))(progn?var=12)))(progntrue))"))
    (is (string= (string-replace result-2-1 " " "")
		 "(or(progn(and(progn(progn(or(progntrue!=false)(progn!false))))(progn12<14)))(progn!isLITERAL(?var)))"))
    (is (string= (string-replace result-3-1 " " "")
		 "(and(prognisLITERAL(\"a(bc||def\"))(progn\"abc)def\"))"))
    (is (string= (string-replace result-4-1 " " "")
		"(progn(and(progna)(progn(progn(or(prognb)(prognc))))))"))
    (is (string= (string-replace result-5-1 " " "")
		 "(and(progn(progn(or(prognb)(prognc))))(progna))"))))


(test test-set-*-and-/-operators
  "Tests various cases of the function set-*-and-/-operators."
  (let* ((dummy-object (make-instance 'TM-SPARQL::SPARQL-Query :query "  "))
	 (str-1 "x = a + b * c && y = a / 3 + b * 2 || 0 = 12 - 14 + 2 * 3 / 3}")
	 (str-2 "x = 2 && (2 + 2) * 2 + 12 * 4 / 2 - 10 + 2 * (12 - 3) + (12 * 3)}")
	 (str-3 "(x < a || ( a = 4 && 4 = x / y + (+1)) && -1)}")
	 (str-4 "isLITERAL(((1 + '(13+4*5))') * 3) / 4) && (12 = 13 + 14 * 15 || 2 * 3 = 1)}")
	 (result-1
	  (getf (tm-sparql::set-boundings dummy-object str-1) :filter-string))
	 (result-1-1
	  (tm-sparql::set-unary-operators dummy-object result-1))
	 (result-1-2
	  (tm-sparql::set-or-and-operators dummy-object result-1-1 result-1))
	 (result-1-3
	  (tm-sparql::set-*-and-/-operators dummy-object result-1-2))
	 (result-2
	  (getf (tm-sparql::set-boundings dummy-object str-2) :filter-string))
	 (result-2-1
	  (tm-sparql::set-unary-operators dummy-object result-2))
	 (result-2-2
	  (tm-sparql::set-or-and-operators dummy-object result-2-1 result-2))
	 (result-2-3
	  (tm-sparql::set-*-and-/-operators dummy-object result-2-2))
	 (result-3
	  (getf (tm-sparql::set-boundings dummy-object str-3) :filter-string))
	 (result-3-1
	  (tm-sparql::set-unary-operators dummy-object result-3))
	 (result-3-2
	  (tm-sparql::set-or-and-operators dummy-object result-3-1 result-3))
	 (result-3-3
	  (tm-sparql::set-*-and-/-operators dummy-object result-3-2))
	 (result-4
	  (getf (tm-sparql::set-boundings dummy-object str-4) :filter-string))
	 (result-4-1
	  (tm-sparql::set-unary-operators dummy-object result-4))
	 (result-4-2
	  (tm-sparql::set-or-and-operators dummy-object result-4-1 result-4))
	 (result-4-3
	  (tm-sparql::set-*-and-/-operators dummy-object result-4-2)))
    (is-true result-1) (is-true result-1-1)
    (is-true result-1-2) (is-true result-1-3)
    (is-true result-2) (is-true result-2-1)
    (is-true result-2-2) (is-true result-2-3)
    (is-true result-3) (is-true result-3-1)
    (is-true result-3-2) (is-true result-3-3)
    (is-true result-4) (is-true result-4-1)
    (is-true result-4-2) (is-true result-4-3)
    (is (string= (string-replace result-1-3 " " "")
		 "(or(progn(and(prognx=a+(*bc))(progny=(/a3)+(*b2))))(progn0=12-14+(/(*23)3)))"))
    (is (string= (string-replace result-2-3 " " "")
		 "(and(prognx=2)(progn(*(progn2+2)2)+(/(*124)2)-10+(*2(progn12-3))+(progn(*123))))"))
    (is (string= (string-replace result-3-3 " " "")
		 "(progn(and(progn(or(prognx<a)(progn(progn(and(progna=4)(progn4=(/xy)+(progn(one+1))))))))(progn(one-1))))"))
    (is (string= (string-replace result-4-3 " " "")
		 "(and(prognisLITERAL((/(progn(*(progn1+\"(13+4*5))\")3))4)))(progn(progn(or(progn12=13+(*1415))(progn(*23)=1)))))"))))


(test test-set-+-and---operators
  "Tests various cases of the function set-+-and---operators."
  (let* ((dummy-object (make-instance 'TM-SPARQL::SPARQL-Query :query "  "))
	 (str-1 "x = a + b * c && y = a / 3 + b * 2 || 0 = 12 - 14 + 2 * 3 / 3}")
	 (str-2 "x = 2 && (2 + 2) * 2 + 12 * 4 / 2 - 10 + 2 * (12 - 3) + (12 * 3)}")
	 (str-3 "(x < a || ( a = 4 && 4 = x / y + (+1)) && -1)}")
	 (str-4 "isLITERAL(((1 + '(13+4*5))') * 3) / 4) && (12 = 13 + 14 * 15 || 2 * 3 = 1)}")
	 (str-5 "(1 + 2 >= 3) || ((2 - 4) + 5 + 6) = 3}")
	 (result-1
	  (getf (tm-sparql::set-boundings dummy-object str-1) :filter-string))
	 (result-1-1
	  (tm-sparql::set-unary-operators dummy-object result-1))
	 (result-1-2
	  (tm-sparql::set-or-and-operators dummy-object result-1-1 result-1))
	 (result-1-3
	  (tm-sparql::set-*-and-/-operators dummy-object result-1-2))
	 (result-1-4
	  (tm-sparql::set-+-and---operators dummy-object result-1-3))
	 (result-2
	  (getf (tm-sparql::set-boundings dummy-object str-2) :filter-string))
	 (result-2-1
	  (tm-sparql::set-unary-operators dummy-object result-2))
	 (result-2-2
	  (tm-sparql::set-or-and-operators dummy-object result-2-1 result-2))
	 (result-2-3
	  (tm-sparql::set-*-and-/-operators dummy-object result-2-2))
	 (result-2-4
	  (tm-sparql::set-+-and---operators dummy-object result-2-3))
	 (result-3
	  (getf (tm-sparql::set-boundings dummy-object str-3) :filter-string))
	 (result-3-1
	  (tm-sparql::set-unary-operators dummy-object result-3))
	 (result-3-2
	  (tm-sparql::set-or-and-operators dummy-object result-3-1 result-3))
	 (result-3-3
	  (tm-sparql::set-*-and-/-operators dummy-object result-3-2))
	 (result-3-4
	  (tm-sparql::set-+-and---operators dummy-object result-3-3))
	 (result-4
	  (getf (tm-sparql::set-boundings dummy-object str-4) :filter-string))
	 (result-4-1
	  (tm-sparql::set-unary-operators dummy-object result-4))
	 (result-4-2
	  (tm-sparql::set-or-and-operators dummy-object result-4-1 result-4))
	 (result-4-3
	  (tm-sparql::set-*-and-/-operators dummy-object result-4-2))
	 (result-4-4
	  (tm-sparql::set-+-and---operators dummy-object result-4-3))
	 (result-5
	  (getf (tm-sparql::set-boundings dummy-object str-5) :filter-string))
	 (result-5-1
	  (tm-sparql::set-unary-operators dummy-object result-5))
	 (result-5-2
	  (tm-sparql::set-or-and-operators dummy-object result-5-1 result-5))
	 (result-5-3
	  (tm-sparql::set-*-and-/-operators dummy-object result-5-2))
	 (result-5-4
	  (tm-sparql::set-+-and---operators dummy-object result-5-3)))
    (is-true result-1) (is-true result-1-1)
    (is-true result-1-2) (is-true result-1-3)
    (is-true result-2) (is-true result-2-1)
    (is-true result-2-2) (is-true result-2-3)
    (is-true result-3) (is-true result-3-1)
    (is-true result-3-2) (is-true result-3-3)
    (is-true result-4) (is-true result-4-1)
    (is-true result-4-2) (is-true result-4-3)
    (is-true result-1-4) (is-true result-2-4)
    (is-true result-3-4) (is-true result-4-4)
    (is-true result-5) (is-true result-5-1)
    (is-true result-5-2) (is-true result-5-3)
    (is-true result-5-4)
    (is (string= (string-replace result-1-4 " " "")
		 "(or(progn(and(prognx=(+a(*bc)))(progny=(+(/a3)(*b2)))))(progn0=(+(-1214)(/(*23)3))))"))
    (is (string= (string-replace result-2-4 " " "")
		 "(and(prognx=2)(progn(+(+(-(+(*(progn(+22))2)(/(*124)2))10)(*2(progn(-123))))(progn(*123)))))"))
    (is (string= (string-replace result-3-4 " " "")
		 "(progn(and(progn(or(prognx<a)(progn(progn(and(progna=4)(progn4=(+(/xy)(progn(one+1)))))))))(progn(one-1))))"))
    (is (string= (string-replace result-4-4 " " "")
		 "(and(prognisLITERAL((/(progn(*(progn(+1\"(13+4*5))\"))3))4)))(progn(progn(or(progn12=(+13(*1415)))(progn(*23)=1)))))"))
    (is (string= (string-replace result-5-4 " " "")
		 "(or(progn(progn(+12)>=3))(progn(progn(+(+(progn(-24))5)6))=3))"))))


(test test-set-compare-operators
  "Tests various cases of the function set-compare-operators."
  (let* ((dummy-object (make-instance 'TM-SPARQL::SPARQL-Query :query "  "))
	 (str-1 "x = a + b * c && y = a / 3 + b * 2 || 0 = 12 - 14 + 2 * 3 / 3}")
	 (str-2 "x = 2 && (2 + 2) * 2 + 12 * 4 / 2 - 10 + 2 * (12 - 3) + (12 * 3)}")
	 (str-3 "(x < a || ( a = 4 && 4 = x / y + (+1)) && -1)}")
	 (str-4 "isLITERAL(((1 + '(13+4*5))') * 3) / 4) && (12 = 13 + 14 * 15 || 2 * 3 = 1)}")
	 (str-5 "(1 + 2 >= 3) || ((2 - 4) + 5 + 6) = 3}")
	 (str-6 "2 > 1 <= 0 != 99 || true}")
	 (result-1
	  (getf (tm-sparql::set-boundings dummy-object str-1) :filter-string))
	 (result-1-1
	  (tm-sparql::set-unary-operators dummy-object result-1))
	 (result-1-2
	  (tm-sparql::set-or-and-operators dummy-object result-1-1 result-1))
	 (result-1-3
	  (tm-sparql::set-*-and-/-operators dummy-object result-1-2))
	 (result-1-4
	  (tm-sparql::set-+-and---operators dummy-object result-1-3))
	 (result-1-5
	  (tm-sparql::set-compare-operators dummy-object result-1-4))
	 (result-2
	  (getf (tm-sparql::set-boundings dummy-object str-2) :filter-string))
	 (result-2-1
	  (tm-sparql::set-unary-operators dummy-object result-2))
	 (result-2-2
	  (tm-sparql::set-or-and-operators dummy-object result-2-1 result-2))
	 (result-2-3
	  (tm-sparql::set-*-and-/-operators dummy-object result-2-2))
	 (result-2-4
	  (tm-sparql::set-+-and---operators dummy-object result-2-3))
	 (result-2-5
	  (tm-sparql::set-compare-operators dummy-object result-2-4))
	 (result-3
	  (getf (tm-sparql::set-boundings dummy-object str-3) :filter-string))
	 (result-3-1
	  (tm-sparql::set-unary-operators dummy-object result-3))
	 (result-3-2
	  (tm-sparql::set-or-and-operators dummy-object result-3-1 result-3))
	 (result-3-3
	  (tm-sparql::set-*-and-/-operators dummy-object result-3-2))
	 (result-3-4
	  (tm-sparql::set-+-and---operators dummy-object result-3-3))
	 (result-3-5
	  (tm-sparql::set-compare-operators dummy-object result-3-4))
	 (result-4
	  (getf (tm-sparql::set-boundings dummy-object str-4) :filter-string))
	 (result-4-1
	  (tm-sparql::set-unary-operators dummy-object result-4))
	 (result-4-2
	  (tm-sparql::set-or-and-operators dummy-object result-4-1 result-4))
	 (result-4-3
	  (tm-sparql::set-*-and-/-operators dummy-object result-4-2))
	 (result-4-4
	  (tm-sparql::set-+-and---operators dummy-object result-4-3))
	 (result-4-5
	  (tm-sparql::set-compare-operators dummy-object result-4-4))
	 (result-5
	  (getf (tm-sparql::set-boundings dummy-object str-5) :filter-string))
	 (result-5-1
	  (tm-sparql::set-unary-operators dummy-object result-5))
	 (result-5-2
	  (tm-sparql::set-or-and-operators dummy-object result-5-1 result-5))
	 (result-5-3
	  (tm-sparql::set-*-and-/-operators dummy-object result-5-2))
	 (result-5-4
	  (tm-sparql::set-+-and---operators dummy-object result-5-3))
	 (result-5-5
	  (tm-sparql::set-compare-operators dummy-object result-5-4))
	 (result-6
	  (getf (tm-sparql::set-boundings dummy-object str-6) :filter-string))
	 (result-6-1
	  (tm-sparql::set-unary-operators dummy-object result-6))
	 (result-6-2
	  (tm-sparql::set-or-and-operators dummy-object result-6-1 result-6))
	 (result-6-3
	  (tm-sparql::set-*-and-/-operators dummy-object result-6-2))
	 (result-6-4
	  (tm-sparql::set-+-and---operators dummy-object result-6-3))
	 (result-6-5
	  (tm-sparql::set-compare-operators dummy-object result-6-4)))
    (is-true result-1) (is-true result-1-1)
    (is-true result-1-2) (is-true result-1-3)
    (is-true result-2) (is-true result-2-1)
    (is-true result-2-2) (is-true result-2-3)
    (is-true result-3) (is-true result-3-1)
    (is-true result-3-2) (is-true result-3-3)
    (is-true result-4) (is-true result-4-1)
    (is-true result-4-2) (is-true result-4-3)
    (is-true result-1-4) (is-true result-2-4)
    (is-true result-3-4) (is-true result-4-4)
    (is-true result-5) (is-true result-5-1)
    (is-true result-5-2) (is-true result-5-3)
    (is-true result-5-4) (is-true result-1-5)
    (is-true result-2-5) (is-true result-3-5)
    (is-true result-4-5) (is-true result-5-5)
    (is-true result-6-1) (is-true result-6-2)
    (is-true result-6-3) (is-true result-6-4)
    (is-true result-6-5)
    (is (string= (string-replace result-1-5 " " "")
		 "(or(progn(and(progn(=x(+a(*bc))))(progn(=y(+(/a3)(*b2))))))(progn(=0(+(-1214)(/(*23)3)))))"))
    (is (string= (string-replace result-2-5 " " "")
		 "(and(progn(=x2))(progn(+(+(-(+(*(progn(+22))2)(/(*124)2))10)(*2(progn(-123))))(progn(*123)))))"))
    (is (string= (string-replace result-3-4 " " "")
		 "(progn(and(progn(or(prognx<a)(progn(progn(and(progna=4)(progn4=(+(/xy)(progn(one+1)))))))))(progn(one-1))))"))
    (is (string= (string-replace result-4-5 " " "")
		 "(and(prognisLITERAL((/(progn(*(progn(+1\"(13+4*5))\"))3))4)))(progn(progn(or(progn(=12(+13(*1415))))(progn(=(*23)1))))))"))
    (is (string= (string-replace result-5-5 " " "")
		 "(or(progn(progn(>=(+12)3)))(progn(=(progn(+(+(progn(-24))5)6))3)))"))
    (is (string= (string-replace result-6-5 " " "")
		 "(or(progn(!=(<=(>21)0)99))(progntrue))"))))


(test test-set-functions
  "Tests various cases of the function set-functions"
  (let* ((dummy-object (make-instance 'TM-SPARQL::SPARQL-Query :query "  "))
	 (str-1 "BOUND((  (?var)  )) || (isLITERAL($var) && ?var = 'abc')}")
	 (str-2
	  "(REGEX(?var1, '''''', ?var3) || (?var1 > ?var3 && (STR( ?var) = \"abc\")))}")
	 (str-3
	  "STR(DATATYPE(?var3,isLITERAL(x, y))) || +?var1 = -?var2 + ?var2 * ?var3}")
	 (str-4 "DATATYPE(?var3) ||isLITERAL(+?var1 = -?var2)}")
	 (str-5 "DATATYPE(?var3) ||(isLITERAL  (+?var1 = -?var2))}")
	 (result-1
	  (getf (tm-sparql::set-boundings dummy-object str-1) :filter-string))
	 (result-1-2
	  (tm-sparql::set-or-and-operators dummy-object result-1 result-1))
	 (result-1-3
	  (tm-sparql::set-*-and-/-operators dummy-object result-1-2))
	 (result-1-4
	  (tm-sparql::set-+-and---operators dummy-object result-1-3))
	 (result-1-5
	  (tm-sparql::set-compare-operators dummy-object result-1-4))
	 (result-1-6
	  (tm-sparql::set-functions dummy-object result-1-5))
	 (result-2
	  (getf (tm-sparql::set-boundings dummy-object str-2) :filter-string))
	 (result-2-2
	  (tm-sparql::set-or-and-operators dummy-object result-2 result-2))
	 (result-2-3
	  (tm-sparql::set-*-and-/-operators dummy-object result-2-2))
	 (result-2-4
	  (tm-sparql::set-+-and---operators dummy-object result-2-3))
	 (result-2-5
	  (tm-sparql::set-compare-operators dummy-object result-2-4))
	 (result-2-6
	  (tm-sparql::set-functions dummy-object result-2-5))
	 (result-3
	      (getf (tm-sparql::set-boundings dummy-object str-3) :filter-string))
	 (result-3-2-1
	  (tm-sparql::set-unary-operators dummy-object result-3))
	 (result-3-2
	  (tm-sparql::set-or-and-operators dummy-object result-3-2-1 result-3))
	 (result-3-3
	  (tm-sparql::set-*-and-/-operators dummy-object result-3-2))
	 (result-3-4
	  (tm-sparql::set-+-and---operators dummy-object result-3-3))
	 (result-3-5
	  (tm-sparql::set-compare-operators dummy-object result-3-4))
	 (result-3-6
	  (tm-sparql::set-functions dummy-object result-3-5))
	 (result-4
	  (getf (tm-sparql::set-boundings dummy-object str-4) :filter-string))
	 (result-4-2-1
	  (tm-sparql::set-unary-operators dummy-object result-4))
	 (result-4-2
	  (tm-sparql::set-or-and-operators dummy-object result-4-2-1 result-4-2-1))
	 (result-4-3
	  (tm-sparql::set-*-and-/-operators dummy-object result-4-2))
	 (result-4-4
	  (tm-sparql::set-+-and---operators dummy-object result-4-3))
	 (result-4-5
	  (tm-sparql::set-compare-operators dummy-object result-4-4))
	 (result-4-6
	  (tm-sparql::set-functions dummy-object result-4-5))
	 (result-5
	  (getf (tm-sparql::set-boundings dummy-object str-5) :filter-string))
	 (result-5-2-1
	  (tm-sparql::set-unary-operators dummy-object result-5))
	 (result-5-2
	  (tm-sparql::set-or-and-operators dummy-object result-5-2-1 result-5-2-1))
	 (result-5-3
	  (tm-sparql::set-*-and-/-operators dummy-object result-5-2))
	 (result-5-4
	  (tm-sparql::set-+-and---operators dummy-object result-5-3))
	 (result-5-5
	  (tm-sparql::set-compare-operators dummy-object result-5-4))
	 (result-5-6
	  (tm-sparql::set-functions dummy-object result-5-5)))
    (is-true result-1) (is-true result-1-2) (is-true result-1-3)
    (is-true result-1-4) (is-true result-1-5) (is-true result-1-6)
    (is-true result-2) (is-true result-2-2) (is-true result-2-3)
    (is-true result-2-4) (is-true result-2-5) (is-true result-2-6)
    (is-true result-3) (is-true result-3-2) (is-true result-3-3)
    (is-true result-3-4) (is-true result-3-5) (is-true result-3-6)
    (is-true result-4) (is-true result-4-2) (is-true result-4-3)
    (is-true result-4-4) (is-true result-4-5) (is-true result-4-6)
    (is-true result-5) (is-true result-5-2) (is-true result-5-3)
    (is-true result-5-4) (is-true result-5-5) (is-true result-5-6)
    (is (string= (string-replace result-1-6 " " "")
		 "(or(progn(BOUND(progn(progn?var))))(progn(progn(and(progn(isLITERAL$var))(progn(=?var\"abc\"))))))"))
    (is (string= (string-replace result-2-6 " " "")
		 "(progn(or(progn(REGEX?var1\"\"?var3))(progn(progn(and(progn(>?var1?var3))(progn(progn(=(STR?var)\"abc\"))))))))"))
    (is (string= (string-replace result-3-6 " " "")
		 "(or(progn(STR(DATATYPE?var3(isLITERALxy))))(progn(=(one+?var1)(+(one-?var2)(*?var2?var3)))))"))
    (is (string= (string-replace result-4-6 " " "")
		 "(or(progn(DATATYPE?var3))(progn(isLITERAL(=(one+?var1)(one-?var2)))))"))
    (is (string= (string-replace result-5-6 " " "")
		 "(or(progn(DATATYPE?var3))(progn(progn(isLITERAL(=(one+?var1)(one-?var2))))))"))))


(test test-module-1
  "Tests the entire module."
  (with-fixture with-tm-filled-db ("data_base" *poems.xtm*)
    (with-revision 0
      (let* ((query-1
	      "BASE <http://some.where/psis/poem/>
               SELECT $subject ?predicate WHERE{
                ?subject $predicate <zauberlehrling> .
                FILTER (STR(?predicate) = 'http://some.where/base-psis/written')}")
	     (query-2 "SELECT ?object ?subject WHERE{
                        <http://some.where/psis/author/goethe> ?predicate ?object .
                        FILTER (isLITERAL(?object) &&
                                DATATYPE(?object) =
                                 'http://www.w3.org/2001/XMLSchema#string')}")
	     (query-3 "SELECT ?object ?subject WHERE{
                        <http://some.where/psis/author/goethe> ?predicate ?object .
                        FILTER (notAllowed(?subject)}")
	     (query-4 "SELECT ?object ?predicate WHERE{
                        <http://some.where/psis/author/goethe> ?predicate ?object .
                        FILTER ((notAllowed( ?predicate)))}")
	     (query-5 "SELECT ?object ?subject WHERE{
                        <http://some.where/psis/author/goethe> ?predicate ?object .
                        FILTER(?a && (?b || ?c)}")
	     (result-1
	      (tm-sparql:result
	       (make-instance 'TM-SPARQL:SPARQL-Query :query query-1)))
	     (result-2 
	      (tm-sparql:result
	       (make-instance 'TM-SPARQL:SPARQL-Query :query query-2))))
	(is-true result-1)
	(is-true result-2)
	(signals exceptions:sparql-parser-error
	  (tm-sparql:result (make-instance 'TM-SPARQL:SPARQL-Query :query query-3)))
	(signals exceptions:sparql-parser-error
	  (tm-sparql:result (make-instance 'TM-SPARQL:SPARQL-Query :query query-4)))
	(signals exceptions:sparql-parser-error
	  (tm-sparql:result (make-instance 'TM-SPARQL:SPARQL-Query :query query-5)))
	(is (= (length result-1) 2))
	(if (string= (getf (first result-1) :variable) "subject")
	    (progn
	      (is (= (length (getf (first result-1) :result)) 1))
	      (is (string= (first (getf (first result-1) :result))
			   "<http://some.where/psis/author/goethe>"))
	      (is (string= (getf (second result-1) :variable) "predicate"))
	      (is (= (length (getf (second result-1) :result)) 1))
	      (is (string= (first (getf (second result-1) :result))
			   "<http://some.where/base-psis/written>")))
	    (progn
	      (is (= (length (getf (second result-1) :result)) 1))
	      (is (string= (first (getf (second result-1) :result))
			   "<http://some.where/psis/author/goethe>"))
	      (is (string= (getf (first result-1) :variable) "predicate"))
	      (is (= (length (getf (first result-1) :result)) 1))
	      (is (string= (first (getf (first result-1) :result))
			   "<http://some.where/base-psis/written>"))))
	(if (string= (getf (first result-2) :variable) "subject")
	    (progn 
	      (is (= (length (getf (first result-2) :result)) 0))
	      (is (string= (getf (second result-2) :variable) "object"))
	      (is (= (length (getf (second result-2) :result)) 3))
	      (is-false (set-exclusive-or
			 (getf (second result-2) :result)
			 (list "Johann Wolfgang" "von Goethe"
			       (concat "\"\"\"http://de.wikipedia.org/wiki/Johann_Wolfgang_von_Goethe\"\"\"^^" *xml-uri*))
			 :test #'string=)))
	    (progn 
	      (is (= (length (getf (second result-2) :result)) 0))
	      (is (string= (getf (first result-2) :variable) "object"))
	      (is (= (length (getf (first result-2) :result)) 3))
	      (is-false (set-exclusive-or
			 (getf (first result-2) :result)
			 (list "Johann Wolfgang" "von Goethe"
			       (concat "\"\"\"http://de.wikipedia.org/wiki/Johann_Wolfgang_von_Goethe\"\"\"^^" *xml-uri*))
			 :test #'string=))))))))


(test test-module-2
  "Tests the entire module."
  (with-fixture with-tm-filled-db ("data_base" *poems.xtm*)
    (with-revision 0
      (let* ((query-1
	      "PREFIX poem:<http://some.where/psis/poem/>
               PREFIX author:<http://some.where/psis/author/>
               PREFIX main:<http://some.where/base-psis/>
               PREFIX tmdm:<http://psi.topicmaps.org/iso13250/model/>
               SELECT ?poems WHERE{
                ?poems tmdm:type main:poem . #self as ?x a <y>
	        ?poems main:title ?titles .
  	        FILTER (REGEX(?titles, '[a-zA-Z]+ [a-zA-Z]+')) }")
	     (result-1
	      (tm-sparql:result (make-instance 'TM-SPARQL:SPARQL-Query :query query-1))))
	(is-true result-1)
	(is (= (length result-1) 1))
	(is (string= (getf (first result-1) :variable) "poems"))
	(is-false (set-exclusive-or
		   (getf (first result-1) :result)
		   (list "<http://some.where/psis/poem/resignation>"
			 "<http://some.where/psis/poem/erlkoenig>"
			 "<http://some.where/psis/poem/zauberlehrling>")
		   :test #'string=))))))


(test test-module-3
  "Tests the entire module with the file sparql_test.xtm"
  (with-fixture with-tm-filled-db ("data_base" *sparql_test.xtm*)
    (tm-sparql:init-tm-sparql)
    (let* ((q-1 (concat
		 "SELECT * WHERE {
                  ?subj1 <http://some.where/tmsparql/first-name> \"Johann Wolfgang\".
                  ?subj2 <http://some.where/tmsparql/last-name> 'von Goethe'^^"
	                                   *xml-string* ".
                  ?subj3 <http://some.where/tmsparql/date-of-birth> '28.08.1749'^^"
                                           *xml-date* ".
	          ?subj4 <http://some.where/tmsparql/date-of-death> '22.03.1832'^^"
                                           *xml-date* ".
                  ?subj5 <http://some.where/tmsparql/years> 82.0.
                  ?subj6 <http://some.where/tmsparql/years> 82.
                  ?subj7 <http://some.where/tmsparql/years> '82'^^" *xml-integer* ".
	          ?subj8 <http://some.where/tmsparql/isDead> true.
	          ?subj9 <http://some.where/tmsparql/isDead> 'true'^^" *xml-boolean* ".
                  ?subj10 <http://some.where/tmsparql/isDead> 'false'^^" *xml-boolean* ".
		  ?subj11 <http://some.where/tmsparql/isDead> false"
                 "}"))
	   (r-1 (tm-sparql:result (make-instance 'TM-SPARQL:SPARQL-Query :query q-1))))
      (is-true (= (length r-1) 11))
      (map 'list #'(lambda(item)
		     (cond ((or (string= (getf item :variable) "subj1")
				(string= (getf item :variable) "subj2")
				(string= (getf item :variable) "subj3")
				(string= (getf item :variable) "subj4")
				(string= (getf item :variable) "subj6")
				(string= (getf item :variable) "subj7")
				(string= (getf item :variable) "subj8")
				(string= (getf item :variable) "subj9"))
			    (is (string= (first (getf item :result))
					 "<http://some.where/tmsparql/author/goethe>")))
			   ((or (string= (getf item :variable) "subj5")
				(string= (getf item :variable) "subj10")
				(string= (getf item :variable) "subj11"))
			    (is-false (getf item :result)))
			   (t
			    (is-true (format t "bad variable-name found")))))
	   r-1))
    (is-true (d:get-item-by-psi "http://www.networkedplanet.com/tmsparql/topicProperty"
				:revision 0))
    (is-true (d:get-item-by-psi "http://www.networkedplanet.com/tmsparql/reifier"
				:revision 0))
    (is-true (d:get-item-by-psi "http://www.networkedplanet.com/tmsparql/role"
				:revision 0))
    (is-true (d:get-item-by-psi "http://www.networkedplanet.com/tmsparql/player"
				:revision 0))
    (is-true (d:get-item-by-psi "http://www.networkedplanet.com/tmsparql/scope"
				:revision 0))
    (is-true (d:get-item-by-psi "http://www.networkedplanet.com/tmsparql/value"
				:revision 0))
    (is-true (d:get-item-by-psi *rdf-type* :revision 0))))


(test test-module-4
  "Tests the entire module with the file sparql_test.xtm"
  (with-fixture with-tm-filled-db ("data_base" *sparql_test.xtm*)
    (tm-sparql:init-tm-sparql)
    (let* ((q-1 (concat
		 "PREFIX pref:<http://www.w3.org/1999/02/>
                  SELECT * WHERE {
                  ?subj1 a <http://some.where/tmsparql/author> .
                  ?subj2 <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://some.where/tmsparql/author> .
                  ?subj3 <http://psi.topicmaps.org/iso13250/model/type> <http://some.where/tmsparql/author> .
	          ?subj4 pref:22-rdf-syntax-ns#type <http://some.where/tmsparql/author>"
                 "}"))
	   (r-1 (tm-sparql:result (make-instance 'TM-SPARQL:SPARQL-Query :query q-1))))
      (is-true (= (length r-1) 4))
      (map 'list #'(lambda(item)
		     (cond ((or (string= (getf item :variable) "subj1")
				(string= (getf item :variable) "subj2")
				(string= (getf item :variable) "subj3")
				(string= (getf item :variable) "subj4"))
			    (is (string= (first (getf item :result))
					 "<http://some.where/tmsparql/author/goethe>")))
			   (t
			    (is-true (format t "bad variable-name found")))))
	   r-1))))


(test test-module-5
  "Tests the entire module with the file sparql_test.xtm"
  (with-fixture with-tm-filled-db ("data_base" *sparql_test.xtm*)
    (tm-sparql:init-tm-sparql)
    (let* ((q-1 (concat
		 "PREFIX tms:<http://www.networkedplanet.com/tmsparql/>
                  SELECT * WHERE {
                   <http://some.where/ii/goethe-occ> tms:reifier ?obj1.
                   ?subj1 tms:reifier <http://some.where/ii/goethe-name-reifier>"
                 "}"))
	   (r-1 (tm-sparql:result (make-instance 'TM-SPARQL:SPARQL-Query :query q-1))))
      (is-true (= (length r-1) 2))
      (map 'list #'(lambda(item)
		     (cond ((string= (getf item :variable) "subj1")
			    (is (string=
				 (first (getf item :result))
				 (concat "_:n"
					 (write-to-string
					  (elephant::oid
					   (d:get-item-by-content "von Goethe")))))))
			   ((string= (getf item :variable) "obj1")
			    (is (string= (first (getf item :result))
					 "<http://some.where/ii/goethe-occ-reifier>")))
			   (t
			    (is-true (format t "bad variable-name found")))))
	   r-1))))


(test test-module-6
  "Tests the entire module with the file sparql_test.xtm"
  (with-fixture with-tm-filled-db ("data_base" *sparql_test.xtm*)
    (tm-sparql:init-tm-sparql)
    (let* ((q-1 (concat
		 "PREFIX tms:<http://www.networkedplanet.com/tmsparql/>
                  SELECT * WHERE {
                   ?assoc tms:reifier <http://some.where/ii/association-reifier>.
                   <http://some.where/ii/association> tms:role ?roles.
                   ?assoc2 tms:role <http://some.where/ii/role-2>"
                 "}"))
	   (r-1 (tm-sparql:result (make-instance 'TM-SPARQL:SPARQL-Query :query q-1)))
	   (role-1 (concat "_:r" (write-to-string
				  (elephant::oid
				   (first (roles
					   (get-item-by-item-identifier 
					    "http://some.where/ii/association"
					    :revision 0)))))))
	   (role-2 (concat "_:r" (write-to-string
				  (elephant::oid
				   (second (roles
					    (get-item-by-item-identifier 
					     "http://some.where/ii/association"
					     :revision 0))))))))
      (is-true (= (length r-1) 3))
      (map 'list #'(lambda(item)
		     (cond ((string= (getf item :variable) "assoc")
			    (is (string= (first (getf item :result))
					 "<http://some.where/ii/association>")))
			    ((string= (getf item :variable) "roles")
			    (is (or (string= (first (getf item :result))
					     role-1)
				    (string= (first (getf item :result))
					     role-2)
				    (string= (first (getf item :result))
					     "<http://some.where/ii/role-2>")))
			     (is (or (string= (second (getf item :result))
					      role-1)
				     (string= (second (getf item :result))
					      role-2)
				     (string= (second (getf item :result))
					      "<http://some.where/ii/role-2>"))))
			    ((string= (getf item :variable) "assoc2")
			     (is (string= (first (getf item :result))
					  "<http://some.where/ii/association>")))
			    (t
			     (is-true (format t "bad variable-name found")))))
	   r-1))))


(test test-module-7
  "Tests the entire module with the file sparql_test.xtm"
  (with-fixture with-tm-filled-db ("data_base" *sparql_test.xtm*)
    (tm-sparql:init-tm-sparql)
    (let* ((q-1 (concat
		 "PREFIX tms:<http://www.networkedplanet.com/tmsparql/>
                  SELECT * WHERE {
                   <http://some.where/ii/role-2> tms:player ?player.
                   ?role tms:player <http://some.where/psis/poem/zauberlehrling>"
                 "}"))
	   (r-1 (tm-sparql:result (make-instance 'TM-SPARQL:SPARQL-Query :query q-1))))
      (is-true (= (length r-1) 2))
      (map 'list #'(lambda(item)
		     (cond ((string= (getf item :variable) "player")
			    (is (string=
				 (first (getf item :result))
				 "<http://some.where/psis/poem/zauberlehrling>")))
			   ((string= (getf item :variable) "role")
			    (is (= (length (getf item :result)) 2))
			    ;one role is the type-instance role
			    (is (or (string= (first (getf item :result))
					     "<http://some.where/ii/role-2>")
				    (string= (second (getf item :result))
					     "<http://some.where/ii/role-2>"))))
			   (t
			    (is-true (format t "bad variable-name found")))))
	   r-1))))


(test test-module-8
  "Tests the entire module with the file sparql_test.xtm"
  (with-fixture with-tm-filled-db ("data_base" *sparql_test.xtm*)
    (tm-sparql:init-tm-sparql)
    (let* ((q-1 (concat
		 "PREFIX tms:<http://www.networkedplanet.com/tmsparql/>
                  SELECT * WHERE {
                   <http://some.where/tmsparql/author/goethe> tms:topicProperty ?props.
                   ?subj1 tms:topicProperty <http://some.where/ii/goethe-untyped-name>.
                   ?subj2 tms:topicProperty <http://some.where/ii/goethe-occ>"
                 "}"))
	   (r-1 (tm-sparql:result (make-instance 'TM-SPARQL:SPARQL-Query :query q-1)))
	   (prop-ids
	    (map 'list
		 #'(lambda(prop)
		     (if (item-identifiers prop :revision 0)
			 (concat "<" (d:uri (first (item-identifiers
						    prop :revision 0))) ">")
			 (if (typep prop 'OccurrenceC)
			     (concat "_:o" (write-to-string (elephant::oid prop)))
			     (concat "_:n" (write-to-string (elephant::oid prop))))))
		 (append (names (get-item-by-psi
				 "http://some.where/tmsparql/author/goethe"
				 :revision 0))
			 (occurrences (get-item-by-psi
				       "http://some.where/tmsparql/author/goethe"
				       :revision 0))))))
      (is-true (= (length r-1) 3))
      (map 'list #'(lambda(item)
		     (cond ((or (string= (getf item :variable) "subj1")
				(string= (getf item :variable) "subj2"))
			    (is (string=
				 (first (getf item :result))
				 "<http://some.where/tmsparql/author/goethe>")))
			   ((string= (getf item :variable) "props")
			    (is (= (length (getf item :result)) 8))
			    (is-false (set-exclusive-or prop-ids (getf item :result)
							:test #'string=)))
			   (t
			    (is-true (format t "bad variable-name found")))))
	   r-1))))


(test test-module-9
  "Tests the entire module with the file sparql_test.xtm"
  (with-fixture with-tm-filled-db ("data_base" *sparql_test.xtm*)
    (tm-sparql:init-tm-sparql)
    (let* ((q-1 (concat
		 "PREFIX tms:<http://www.networkedplanet.com/tmsparql/>
                  SELECT * WHERE {
                   <http://some.where/ii/zb/occurrence> tms:scope ?scope.
                   ?owner tms:scope <http://some.where/tmsparql/de>"
                 "}"))
	   (r-1 (tm-sparql:result (make-instance 'TM-SPARQL:SPARQL-Query :query q-1))))
      (is-true (= (length r-1) 2))
      (map 'list #'(lambda(item)
		     (cond ((string= (getf item :variable) "scope")
			    (is (string= (first (getf item :result))
					 "<http://some.where/tmsparql/de>")))
			   ((string= (getf item :variable) "owner")
			    (is (string= (first (getf item :result))
					 "<http://some.where/ii/zb/occurrence>")))
			   (t
			    (is-true (format t "bad variable-name found")))))
	   r-1))))


(test test-module-10
  "Tests the entire module with the file sparql_test.xtm"
  (with-fixture with-tm-filled-db ("data_base" *sparql_test.xtm*)
    (tm-sparql:init-tm-sparql)
    (let* ((q-1 (concat
		 "PREFIX tms:<http://www.networkedplanet.com/tmsparql/>
                  SELECT * WHERE {
                   <http://some.where/ii/goethe-untyped-name> tms:value ?obj1.
                   <http://some.where/ii/goethe-occ> tms:value ?obj2.
                   <http://some.where/ii/goethe-variant> tms:value ?obj3.
                   ?subj1 tms:value 'Goethe'.
                   ?subj2 tms:value '28.08.1749'^^http://www.w3.org/2001/XMLSchema#date.
                   ?subj3 tms:value 'Johann Wolfgang von Goethe'.
                   ?subj4 tms:value 82"
                 "}"))
	   (r-1 (tm-sparql:result (make-instance 'TM-SPARQL:SPARQL-Query :query q-1))))
      (is-true (= (length r-1) 7))
      (map 'list #'(lambda(item)
		     (cond ((string= (getf item :variable) "obj1")
			    (is (string= (first (getf item :result))
					 "Johann Wolfgang von Goethe")))
			   ((string= (getf item :variable) "obj2")
			    (is (string= (first (getf item :result))
					 (concat "\"\"\"28.08.1749\"\"\"^^"
						 *xml-date*))))
			   ((string= (getf item :variable) "obj3")
			    (is (string= (first (getf item :result))
					 "Goethe")))
			   ((string= (getf item :variable) "subj1")
			    (is (string= (first (getf item :result))
					 "<http://some.where/ii/goethe-variant>")))
			   ((string= (getf item :variable) "subj2")
			    (is (string= (first (getf item :result))
					 "<http://some.where/ii/goethe-occ>")))
			   ((string= (getf item :variable) "subj3")
			    (is (string= (first (getf item :result))
					 "<http://some.where/ii/goethe-untyped-name>")))
			   ((string= (getf item :variable) "subj4")
			    (is (string= (first (getf item :result))
					 "<http://some.where/ii/goethe-years-occ>")))
			   (t
			    (is-true (format t "bad variable-name found")))))
	   r-1))))


(test test-module-11
  "Tests the entire module with the file sparql_test.xtm"
  (with-fixture with-tm-filled-db ("data_base" *sparql_test.xtm*)
    (tm-sparql:init-tm-sparql)
    (let* ((q-1 (concat
		 "PREFIX tms:<http://www.networkedplanet.com/tmsparql/>
                  SELECT * WHERE {
                   <http://some.where/tmsparql/author/goethe> a <http://some.where/tmsparql/author>.
	           <http://some.where/ii/goethe-occ> tms:reifier <http://some.where/ii/goethe-occ-reifier>.
                   <http://some.where/ii/association> tms:role <http://some.where/ii/role-2>.
                   <http://some.where/ii/role-2> tms:player <http://some.where/psis/poem/zauberlehrling>.
                   <http://some.where/tmsparql/author/goethe> tms:topicProperty <http://some.where/ii/goethe-untyped-name>.
                   <http://some.where/ii/goethe-variant> tms:scope <http://some.where/tmsparql/display-name>.
                   <http://some.where/ii/goethe-untyped-name> tms:value 'Johann Wolfgang von Goethe'"
                 "}"))
	   (r-1 (tm-sparql:result (make-instance 'TM-SPARQL:SPARQL-Query :query q-1))))
      (is-false r-1))))


(test test-module-12
  "Tests the entire module with the file sparql_test.xtm"
  (with-fixture with-tm-filled-db ("data_base" *sparql_test.xtm*)
    (tm-sparql:init-tm-sparql)
    (let* ((q-1 (concat
		 "PREFIX tms:<http://www.networkedplanet.com/tmsparql/>
                  SELECT * WHERE {
                   ?subj1 a ?obj1.
                   ?subj2 tms:reifier ?obj2.
                   ?subj3 tms:role ?obj3.
                   ?subj4 tms:player ?obj4.
                   ?subj5 tms:topicProperty ?obj5.
                   ?subj6 tms:scope ?obj6.
                   ?subj7 tms:value ?obj7"
                 "}"))
	   (r-1 (tm-sparql:result (make-instance 'TM-SPARQL:SPARQL-Query :query q-1))))
      (is (= (length r-1) 14))
      (map 'list #'(lambda(item)
		     (cond ((string= (getf item :variable) "subj1")
			    (is (= (length (getf item :result)) 29)))
			   ((string= (getf item :variable) "obj2")
			    (is (= (length (getf item :result)) 4))
			    (is-false (set-exclusive-or
				       (getf item :result)
				       (list "<http://some.where/ii/goethe-name-reifier>"
					     "<http://some.where/ii/goethe-occ-reifier>"
					     "<http://some.where/ii/association-reifier>"
					     "<http://some.where/ii/role-reifier>")
				       :test #'string=)))
			   ((string= (getf item :variable) "subj3")
			    (is (= (length (getf item :result)) 60))
			    (is (find "<http://some.where/ii/association>"
				      (getf item :result) :test #'string=)))
			   ((string= (getf item :variable) "subj4")
			    (is (= (length (getf item :result)) 60)))
			   ((string= (getf item :variable) "subj5")
			    (is (= (length (getf item :result)) 10)))
			   ((string= (getf item :variable) "subj6")
			    (is (= (length (getf item :result)) 2))
			    (set-exclusive-or
			     (getf item :result)
			     (list "<http://some.where/ii/zb/occurrence>"
				   "<http://some.where/ii/goethe-variant>")
			     :test #'string=))
			   ((string= (getf item :variable) "subj7")
			    (is (= (length (getf item :result)) 11)))
			   ((string= (getf item :variable) "obj1")
			    (is (= (length (getf item :result)) 29)))
			   ((string= (getf item :variable) "subj2")
			    (is (= (length (getf item :result)) 4))
			    (is-false
			     (set-exclusive-or
			      (getf item :result)
			      (list
			       "<http://some.where/ii/goethe-occ>"
			       "<http://some.where/ii/association>"
			       (concat
				"_:r"
				(write-to-string
				 (elephant::oid
				  (loop for role in
				       (roles (get-item-by-item-identifier
					       "http://some.where/ii/association"
					       :revision 0) :revision 0)
				     when (string=
					   (uri (first
						 (psis (player role :revision 0)
						       :revision 0)))
					   "http://some.where/tmsparql/author/goethe")
				     return role))))
			       (concat
				"_:n"
				(write-to-string
				 (elephant::oid
				  (loop for name in
				       (names
					(get-item-by-psi
					 "http://some.where/tmsparql/author/goethe"
					 :revision 0) :revision 0)
				     when (string= (charvalue name) "von Goethe")
				     return name)))))
			      :test #'string=)))
			   ((string= (getf item :variable) "obj3")
			    (is (= (length (getf item :result)) 60))
			    (is (find "<http://some.where/ii/role-2>"
				      (getf item :result) :test #'string=)))
			   ((string= (getf item :variable) "obj4")
			    (is (= (length (getf item :result)) 60)))
			   ((string= (getf item :variable) "obj5")
			    (is (= (length (getf item :result)) 10)))
			   ((string= (getf item :variable) "obj6")
			    (is (= (length (getf item :result)) 2))
			    (set-exclusive-or
			     (getf item :result)
			     (list "<http://some.where/tmsparql/display-name>"
				   "<http://some.where/tmsparql/de>")))
			   ((string= (getf item :variable) "obj7")
			    (is (= (length (getf item :result)) 11))
			    (set-exclusive-or
			     (getf item :result)
			     (list "Johann Wolfgang" "von Goethe"
				   "Johann Wolfgang von Goethe" "Der Zauberlehrling"
				   "28.08.1749" "22.03.1832" 82 t nil
				   "Hat der alte Hexenmeister
	sich doch einmal wegbegeben!
	...
      ")
			     :test #'tm-sparql::literal=))
			    (t
			     (is-true (format t "bad variable-name found")))))
	   r-1))))


(test test-module-13
  "Tests the entire module with the file sparql_test.xtm"
  (with-fixture with-tm-filled-db ("data_base" *sparql_test.xtm*)
    (tm-sparql:init-tm-sparql)
    (let* ((q-1 (concat
		 "PREFIX tms:<http://www.networkedplanet.com/tmsparql/>
                  SELECT * WHERE {
                   <http://some.where/tmsparql/author/goethe> ?pred1 ?obj1.
		   <http://some.where/ii/association> ?pred2 ?obj2.
                   <http://some.where/ii/role-2> ?pred3 ?obj3.
                   <http://some.where/ii/goethe-untyped-name> ?pred4 ?obj4.
                   <http://some.where/ii/goethe-occ> ?pred5 ?obj5.
                   <http://some.where/ii/goethe-variant> ?pred6 ?obj6"
                 "}"))
	   (r-1 (tm-sparql:result (make-instance 'TM-SPARQL:SPARQL-Query :query q-1))))
      (is-true (= (length r-1) 12))
      (map 'list #'(lambda(item)
		     (cond ((string= (getf item :variable) "pred1")
			    ;one name without a type so it is not listed
			    ;as regular triple but as tms:topicProperty
			    (is (= (length (getf item :result)) 17)))
			   ((string= (getf item :variable) "pred2")
			    (is (= (length (getf item :result)) 3))
			    (is-false (set-exclusive-or
				       (getf item :result)
				       (list (concat "<" *tms-role* ">")
					     (concat "<" *tms-reifier* ">"))
				       :test #'string=)))
			   ((string= (getf item :variable) "pred3")
			    (is (= (length (getf item :result)) 1))
			    (is (string= (first (getf item :result))
					 (concat "<" *tms-player* ">"))))
			   ((string= (getf item :variable) "pred4")
			    (is (= (length (getf item :result)) 1))
			    (is (string= (first (getf item :result))
					 (concat "<" *tms-value* ">"))))
			   ((string= (getf item :variable) "pred5")
			    (is (= (length (getf item :result)) 2))
			    (is-false (set-exclusive-or
				       (getf item :result)
				       (list (concat "<" *tms-value* ">")
					     (concat "<" *tms-reifier* ">"))
				       :test #'string=)))
			   ((string= (getf item :variable) "pred6")
			    (is (= (length (getf item :result)) 2))
			    (is-false (set-exclusive-or
				       (getf item :result)
				       (list (concat "<" *tms-value* ">")
					     (concat "<" *tms-scope* ">"))
				       :test #'string=)))
			   ((string= (getf item :variable) "obj1")
			    (is (= (length (getf item :result)) 17))
			    (is-true (find "Johann Wolfgang" (getf item :result)
					   :test #'tm-sparql::literal=))
			    (is-true (find "von Goethe" (getf item :result)
					   :test #'tm-sparql::literal=))
			    (is-true (find t (getf item :result)
					   :test #'tm-sparql::literal=))
			    (is-true (position nil (getf item :result)
					       :test #'tm-sparql::literal=))
			    (is-true (find (concat "'28.08.1749'^^" *xml-date*)
					   (getf item :result)
					   :test #'tm-sparql::literal=))
			    (is-true (find (concat "'22.03.1832'^^" *xml-date*)
					   (getf item :result)
					   :test #'tm-sparql::literal=))
			    (is-true (find 82 (getf item :result)
					   :test #'tm-sparql::literal=))
			    (is-true (find "<http://some.where/tmsparql/author>"
					   (getf item :result)
					   :test #'tm-sparql::literal=))
			    (is-true
			     (find "<http://some.where/psis/poem/zauberlehrling>"
				   (getf item :result) :test #'tm-sparql::literal=)))
			   ((string= (getf item :variable) "obj2")
			    (is (= (length (getf item :result)) 3))
			    (is-false
			     (set-exclusive-or
			      (getf item :result)
			      (list
			       "<http://some.where/ii/association-reifier>"
			       "<http://some.where/ii/role-2>"
			       (concat
				"_:r"
				(write-to-string
				 (elephant::oid
				  (loop for role in
				       (roles
					(get-item-by-item-identifier
					 "http://some.where/ii/association"
					 :revision 0))
				     when (string=
					   (uri (first (psis (player role
								     :revision 0))))
					   "http://some.where/tmsparql/author/goethe")
				     return role)))))
			      :test #'string=)))
			   ((string= (getf item :variable) "obj3")
			    (is (= (length (getf item :result)) 1))
			    (is (string=
				 (first (getf item :result))
				 "<http://some.where/psis/poem/zauberlehrling>")))
			   ((string= (getf item :variable) "obj4")
			    (is (= (length (getf item :result)) 1))
			    (is (string= (first (getf item :result))
					 "Johann Wolfgang von Goethe")))
			   ((string= (getf item :variable) "obj5")
			    (is (= (length (getf item :result)) 2))
			    (is-false
			     (set-exclusive-or
			      (getf item :result)
			      (list (concat "'28.08.1749'^^" *xml-date*)
				    "<http://some.where/ii/goethe-occ-reifier>")
			      :test #'tm-sparql::literal=)))
			   ((string= (getf item :variable) "obj6")
			   (is (= (length (getf item :result)) 2))
			    (is-false
			     (set-exclusive-or
			      (getf item :result)
			      (list "Goethe"
				    "<http://some.where/tmsparql/display-name>")
			      :test #'string=)))
			   (t
			    (is-true (format t "bad variable-name found")))))
	   r-1))))


(test test-module-14
  "Tests the entire module with the file sparql_test.xtm"
  (with-fixture with-tm-filled-db ("data_base" *sparql_test.xtm*)
    (tm-sparql:init-tm-sparql)
    (let* ((q-1 (concat
		 "PREFIX tms:<http://www.networkedplanet.com/tmsparql/>
                  SELECT * WHERE {
                   ?subj1 ?pred1 <http://some.where/tmsparql/author/goethe>.
                   ?subj2 ?pred2 <http://some.where/ii/goethe-variant>.
                   ?subj3 ?pred3 <http://some.where/ii/goethe-untyped-name>.
                   ?subj4 ?pred4 <http://some.where/ii/goethe-occ>.
                   ?subj5 ?pred5 <http://some.where/ii/association>.
                   ?subj6 ?pred6 <http://some.where/ii/role-2>.
                   ?subj7 ?pred7 <http://some.where/tmsparql/display-name>.
                   ?subj8 ?pred8 <http://some.where/ii/role-reifier>"
                 "}"))
	   (r-1 (tm-sparql:result (make-instance 'TM-SPARQL:SPARQL-Query :query q-1))))
      (is-true (= (length r-1) 16))
      (map 'list #'(lambda(item)
		     (cond ((string= (getf item :variable) "pred1")
			    (is (= (length (getf item :result)) 4))
			    (is-false
			     (set-exclusive-or
			      (list (concat "<" *instance-psi* ">")
				    "<http://some.where/tmsparql/writer>"
				    (concat "<" *tms-player* ">"))
			      (getf item :result) :test #'string=)))
			   ((string= (getf item :variable) "subj1")
			    (is (= (length (getf item :result)) 4))
			    (is-false
			     (set-exclusive-or
			      (list "<http://some.where/tmsparql/author>"
				    "<http://some.where/psis/poem/zauberlehrling>"
				    (concat
				     "_:r"
				     (write-to-string
				      (elephant::oid
				       (first
					(player-in-roles
					 (get-item-by-psi
					  "http://some.where/tmsparql/author/goethe"
					  :revision 0) :revision 0)))))
				    (concat
				     "_:r"
				     (write-to-string
				      (elephant::oid
				       (second
					(player-in-roles
					 (get-item-by-psi
					  "http://some.where/tmsparql/author/goethe"
					  :revision 0) :revision 0))))))
			      (getf item :result) :test #'string=)))
			    ((or (string= (getf item :variable) "pred2")
				 (string= (getf item :variable) "pred5"))
			     (is-false (getf item :result)))
			    ((or (string= (getf item :variable) "subj2")
				 (string= (getf item :variable) "subj5"))
			     (is-false (getf item :result)))
			    ((or (string= (getf item :variable) "pred3")
				 (string= (getf item :variable) "pred4"))
			     (is (= (length (getf item :result)) 1))
			     (is (string= (first (getf item :result))
					  (concat "<" *tms-topicProperty* ">"))))
			    ((or (string= (getf item :variable) "subj3")
				 (string= (getf item :variable) "subj4"))
			     (is (= (length (getf item :result)) 1))
			     (is (string= (first (getf item :result))
					  "<http://some.where/tmsparql/author/goethe>")))
			    ((string= (getf item :variable) "pred6")
			     (is (= (length (getf item :result)) 1))
			     (is (string= (first (getf item :result))
					  (concat "<" *tms-role* ">"))))
			    ((string= (getf item :variable) "subj6")
			     (is (= (length (getf item :result)) 1))
			     (is (string= (first (getf item :result))
					  "<http://some.where/ii/association>")))
			    ((string= (getf item :variable) "pred7")
			     (is (= (length (getf item :result)) 3))
			     (is-false (set-exclusive-or
					(list (concat "<" *tms-player* ">")
					      (concat "<" *tms-scope* ">")
					      (concat "<" *instance-psi* ">"))
					(getf item :result) :test #'string=)))
			    ((string= (getf item :variable) "subj7")
			     (is (= (length (getf item :result)) 3))
			     (is (find "<http://psi.topicmaps.org/tmcl/scope-type>"
				       (getf item :result) :test #'string=))
			     (is (find "<http://some.where/ii/goethe-variant>"
				       (getf item :result) :test #'string=)))
			    ((string= (getf item :variable) "pred8")
			     (is (= (length (getf item :result)) 3))
			     (is-false (set-exclusive-or
					(list (concat "<" *tms-player* ">")
					      (concat "<" *tms-reifier* ">")
					      (concat "<" *instance-psi* ">"))
					(getf item :result) :test #'string=)))
			    ((string= (getf item :variable) "subj8")
			     (is (= (length (getf item :result)) 3))
			     (set-exclusive-or
			      (list "http://some.where/tmsparql/reifier-type"
				    (concat
				     "_:r"
				     (write-to-string
				      (elephant::oid
				       (first
					(player-in-roles
					 (get-item-by-item-identifier
					  "http://some.where/ii/role-reifier"
					  :revision 0) :revision 0))))))
			      (getf item :result) :test #'string=))
			    (t
			     (is-true (format t "bad variable-name found ~a"
					      (getf item :variable))))))
	   r-1))))


(test test-module-15
  "Tests the entire module with the file sparql_test.xtm"
  (with-fixture with-tm-filled-db ("data_base" *sparql_test.xtm*)
    (tm-sparql:init-tm-sparql)
    (let* ((q-1 (concat
		 "PREFIX tms:<" *tms* ">
                  SELECT * WHERE {
                   ?assoc tms:reifier <http://some.where/ii/association-reifier>.
                   ?assoc tms:role ?roles.
                   ?roles tms:reifier <http://some.where/ii/role-reifier>"
                 "}"))
	   (r-1 (tm-sparql:result (make-instance 'TM-SPARQL:SPARQL-Query :query q-1))))
      (is-true (= (length r-1) 2))
      (map 'list #'(lambda(item)
		     (cond
		       ((string= (getf item :variable) "assoc")
			(is (= (length (getf item :result)) 1))
			(is (string= (first (getf item :result))
				     "<http://some.where/ii/association>")))
		       ((string= (getf item :variable) "roles")
			(is (= (length (getf item :result)) 1))
			(is
			 (string=
			  (first (getf item :result))
			  (concat
			   "_:r"
			   (write-to-string
			    (elephant::oid
			     (loop for role in
				  (roles
				   (get-item-by-item-identifier
				    "http://some.where/ii/association"
				    :revision 0) :revision 0)
				when (string=
				      (uri (first (psis (player role :revision 0)
							:revision 0)))
				      "http://some.where/tmsparql/author/goethe")
				return role)))))))
		       (t
			(is-true (format t "bad variable-name found ~a"
					 (getf item :variable))))))
	   r-1))))


(test test-module-16
  "Tests the entire module with the file sparql_test.xtm"
  (with-fixture with-tm-filled-db ("data_base" *sparql_test.xtm*)
    (tm-sparql:init-tm-sparql)
    (let* ((q-1 (concat
		 "SELECT * WHERE {
                   <http://some.where/tmsparql/author/goethe> ?pred1 ?obj1.
                   FILTER ?obj1 = 'von Goethe' || ?obj1 = 82
                   FILTER ?obj1 = 'von Goethe' || ?obj1 = '82'^^" *xml-integer* "
		   FILTER (?obj1 = 'von Goethe'^^" *xml-string* " || 82 = ?obj1)
                   FILTER (?obj1 = 'von Goethe') || (82 = ?obj1)
		   FILTER ((?obj1 = 'von Goethe') || (82 = ?obj1))"
		 "}"))
	   (r-1 (tm-sparql:result (make-instance 'TM-SPARQL:SPARQL-Query :query q-1))))
      (is-true (= (length r-1) 2))
      (map 'list #'(lambda(item)
		     (cond
		       ((string= (getf item :variable) "pred1")
			(is (= (length (getf item :result)) 2))
			(is (find "<http://some.where/tmsparql/last-name>"
				  (getf item :result) :test #'string=))
			(is (find "<http://some.where/tmsparql/years>"
				  (getf item :result) :test #'string=)))
		       ((string= (getf item :variable) "obj1")
			(is (= (length (getf item :result)) 2))
			(is (find 82 (getf item :result) :test #'tm-sparql::literal=))
			(is (find "von Goethe" (getf item :result)
				  :test #'tm-sparql::literal=)))
		       (t
			(is-true (format t "bad variable-name found ~a"
					 (getf item :variable))))))
			
	   r-1))))





;TODO: test complex filters

(defun run-sparql-tests ()
  (it.bese.fiveam:run! 'sparql-test:sparql-tests))
