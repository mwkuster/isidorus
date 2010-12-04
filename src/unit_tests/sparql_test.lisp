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
	   :test-result))


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
    (let ((res (tm-sparql::parse-literal-elem query-1 dummy-object)))
      (is (string= (getf res :next-query) "."))
      (is (string= (tm-sparql::value (getf res :value))
		   "literal-value"))
      (is (string= (tm-sparql::literal-lang (getf res :value))
		   "de"))
      (is (string= (tm-sparql::literal-datatype (getf res :value))
		   *xml-string*))
      (is (eql (tm-sparql::elem-type (getf res :value)) 'TM-SPARQL::LITERAL)))
    (let ((res (tm-sparql::parse-literal-elem query-2 dummy-object)))
      (is (string= (getf res :next-query) "."))
      (is (eql (tm-sparql::value (getf res :value)) t))
      (is-false (tm-sparql::literal-lang (getf res :value)))
      (is (string= (tm-sparql::literal-datatype (getf res :value))
		   *xml-boolean*))
      (is (eql (tm-sparql::elem-type (getf res :value)) 'TM-SPARQL::LITERAL)))
    (let ((res (tm-sparql::parse-literal-elem query-3 dummy-object)))
      (is (string= (getf res :next-query) "}"))
      (is (eql (tm-sparql::value (getf res :value)) nil))
      (is-false (tm-sparql::literal-lang (getf res :value)))
      (is (string= (tm-sparql::literal-datatype (getf res :value))
		   *xml-boolean*))
      (is (eql (tm-sparql::elem-type (getf res :value)) 'TM-SPARQL::LITERAL)))
    (let ((res (tm-sparql::parse-literal-elem query-4 dummy-object)))
      (is (string= (getf res :next-query) (string #\tab)))
      (is (= (tm-sparql::value (getf res :value)) 1234.43e10))
      (is-false (tm-sparql::literal-lang (getf res :value)))
      (is (string= (tm-sparql::literal-datatype (getf res :value))
		   *xml-double*))
      (is (eql (tm-sparql::elem-type (getf res :value)) 'TM-SPARQL::LITERAL)))
    (let ((res (tm-sparql::parse-literal-elem query-5 dummy-object)))
      (is (string= (getf res :next-query) ";"))
      (is (eql (tm-sparql::value (getf res :value)) t))
      (is-false (tm-sparql::literal-lang (getf res :value)))
      (is (string= (tm-sparql::literal-datatype (getf res :value))
		   *xml-boolean*))
      (is (eql (tm-sparql::elem-type (getf res :value)) 'TM-SPARQL::LITERAL)))
    (let ((res (tm-sparql::parse-literal-elem query-6 dummy-object)))
      (is (string= (getf res :next-query)
		   (concatenate 'string "." (string #\newline))))
      (is (eql (tm-sparql::value (getf res :value)) 123.4))
      (is-false (tm-sparql::literal-lang (getf res :value)))
      (is (string= (tm-sparql::literal-datatype (getf res :value))
		   *xml-double*))
      (is (eql (tm-sparql::elem-type (getf res :value)) 'TM-SPARQL::LITERAL)))
    (let ((res (tm-sparql::parse-literal-elem query-7 dummy-object)))
      (is (string= (getf res :next-query) "."))
      (is (string= (tm-sparql::value (getf res :value))
		   "Just a test

literal with some \\\"quoted\\\" words!"))
      (is (string= (tm-sparql::literal-lang (getf res :value)) "en"))
      (is (string= (tm-sparql::literal-datatype (getf res :value))
		   *xml-string*))
      (is (eql (tm-sparql::elem-type (getf res :value)) 'TM-SPARQL::LITERAL)))
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
    (let ((res (tm-sparql::parse-triple-elem query-1 dummy-object)))
      (is (string= (getf res :next-query) "."))
      (is (string= (tm-sparql::value (getf res :value)) "var1"))
      (is (eql (tm-sparql::elem-type (getf res :value)) var)))
    (let ((res (tm-sparql::parse-triple-elem query-2 dummy-object)))
      (is (string= (getf res :next-query) ";"))
      (is (string= (tm-sparql::value (getf res :value)) "var2"))
      (is (eql (tm-sparql::elem-type (getf res :value)) var)))
    (let ((res (tm-sparql::parse-triple-elem query-3 dummy-object)))
      (is (string= (getf res :next-query) "}"))
      (is (string= (tm-sparql::value (getf res :value)) "var3"))
      (is (eql (tm-sparql::elem-type (getf res :value)) var)))
    (let ((res (tm-sparql::parse-triple-elem query-4 dummy-object)))
      (is (string= (getf res :next-query) "."))
      (is (string= (tm-sparql::value (getf res :value))
		   "http://full.url"))
      (is (eql (tm-sparql::elem-type (getf res :value)) iri)))
    (let ((res (tm-sparql::parse-triple-elem query-5 dummy-object)))
      (is (string= (getf res :next-query) "}"))
      (is (string= (tm-sparql::value (getf res :value))
		   "http://base.value/url-suffix"))
      (is (eql (tm-sparql::elem-type (getf res :value)) iri)))
    (let ((res (tm-sparql::parse-triple-elem query-6 dummy-object)))
      (is (string= (getf res :next-query) "."))
      (is (string= (tm-sparql::value (getf res :value))
		   "http://prefix.value/suffix"))
      (is (eql (tm-sparql::elem-type (getf res :value)) iri)))
    (let ((res (tm-sparql::parse-triple-elem query-7 dummy-object)))
      (is (string= (getf res :next-query) "}"))
      (is (string= (tm-sparql::value (getf res :value))
		   "http://prefix.value/suffix"))
      (is (eql (tm-sparql::elem-type (getf res :value)) iri)))
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
			 "http://some.where/psis/author/goethe")
		(string= (first (tm-sparql::subject-result
				 (first (tm-sparql::select-group q-obj-1))))
			 "http://some.where/psis/persons/goethe")))
	(is (string= (first (tm-sparql::predicate-result
			     (first (tm-sparql::select-group q-obj-1))))
		     "http://some.where/base-psis/author-info"))
	(is (string= (first (tm-sparql::object-result
			     (first (tm-sparql::select-group q-obj-1))))
		     "http://de.wikipedia.org/wiki/Johann_Wolfgang_von_Goethe"))
	(is (string= (first (tm-sparql::subject-result
			     (first (tm-sparql::select-group q-obj-4))))
		     "http://some.where/psis/author/schiller"))
	(is (string= (first (tm-sparql::predicate-result
			     (first (tm-sparql::select-group q-obj-4))))
		     "http://some.where/base-psis/written"))
	(is (string= (first (tm-sparql::object-result
			     (first (tm-sparql::select-group q-obj-4))))
		     "http://some.where/psis/poem/resignation"))))))


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
			    (list "http://some.where/psis/author/goethe")
			    (tm-sparql::subject-result
			     (first (tm-sparql::select-group q-obj-1)))
			    :test #'string=))
		     (null (set-exclusive-or
			    (list "http://some.where/psis/persons/goethe")
			    (tm-sparql::subject-result
			     (first (tm-sparql::select-group q-obj-1)))
			    :test #'string=))))
	(let ((predicates (tm-sparql::predicate-result
			   (first (tm-sparql::select-group q-obj-1)))))
	  (is (= (count "http://some.where/base-psis/written" predicates
			:test #'string=) 2))
	  (is (= (count "http://some.where/base-psis/place" predicates
			:test #'string=) 1))
	  (is (= (count "http://some.where/base-psis/first-name" predicates
			:test #'string=) 1))
	  (is (= (count "http://some.where/base-psis/last-name" predicates
			:test #'string=) 1))
	  (is (= (count "http://some.where/base-psis/author-info" predicates
			:test #'string=) 1))
	  (is (= (count "http://psi.topicmaps.org/iso13250/model/type" predicates
			:test #'string=) 1)))
	(let ((objects (tm-sparql::object-result
			(first (tm-sparql::select-group q-obj-1)))))
	  (is (= (count "http://some.where/psis/poem/erlkoenig" objects
			:test #'string=) 1))
	  (is (or (= (count "http://some.where/psis/poem/der_zauberlehrling"
			    objects :test #'string=) 1)
		  (= (count "http://some.where/psis/poem/zauberlehrling" objects
			    :test #'string=) 1)))
	  (is (or (= (count "http://some.where/base-psis/author" objects
			    :test #'string=) 1)
		  (= (count "http://some.where/base-psis/author-psi" objects
			    :test #'string=) 1)))
	  (is (= (count "http://de.wikipedia.org/wiki/Johann_Wolfgang_von_Goethe"
			objects :test #'string=) 1))
	  (is (= (count "von Goethe" objects :test #'string=) 1))
	  (is (= (count "Johann Wolfgang" objects :test #'string=) 1))
	  (is (= (count "http://some.where/psis/region/frankfurt_am_main"
			objects :test #'string=) 1)))
	(is-true (or (null (set-exclusive-or
			    (list "http://some.where/psis/poem/der_zauberlehrling")
			    (tm-sparql::subject-result
			     (first (tm-sparql::select-group q-obj-2)))
			    :test #'string=))
		     (null (set-exclusive-or
			    (list "http://some.where/psis/poem/zauberlehrling")
			    (tm-sparql::subject-result
			     (first (tm-sparql::select-group q-obj-2)))
			    :test #'string=))))
	(let ((predicates (tm-sparql::predicate-result
			   (first (tm-sparql::select-group q-obj-2)))))
	  (is (= (count "http://some.where/base-psis/writer" predicates
			:test #'string=) 1))
	  (is (= (count "http://some.where/base-psis/title" predicates
			:test #'string=) 1))
	  (is (= (count "http://some.where/base-psis/poem-content" predicates
			:test #'string=) 1))
	  (is (= (count "http://psi.topicmaps.org/iso13250/model/type" predicates
			:test #'string=) 1)))
	(let ((objects (tm-sparql::object-result
			(first (tm-sparql::select-group q-obj-2)))))
	  (is (or (= (count "http://some.where/psis/author/goethe" objects
			    :test #'string=) 1)
		  (= (count "http://some.where/psis/persons/goethe" objects
			    :test #'string=) 1)))
	  (is (= (count "Der Zauberlehrling" objects :test #'string=) 1))
	  (is (= (count "http://some.where/base-psis/poem"
			objects :test #'string=) 1))
	  ;do not check the entire poem content => too long
	  )
	(is (or (string= "http://some.where/psis/author/goethe"
			 (first (tm-sparql::subject-result
				 (first (tm-sparql::select-group q-obj-3)))))
		(string= "http://some.where/psis/persons/goethe"
			 (first (tm-sparql::subject-result
				 (first (tm-sparql::select-group q-obj-3)))))))
	(is (string= "http://some.where/base-psis/written"
		     (first (tm-sparql::predicate-result
			     (first (tm-sparql::select-group q-obj-3))))))
	(is (or (string= "http://some.where/psis/poem/der_zauberlehrling"
			 (first (tm-sparql::object-result
				 (first (tm-sparql::select-group q-obj-3)))))
		(string= "http://some.where/psis/poem/zauberlehrling"
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
			(second (tm-sparql::select-group q-obj-3)))) 1))
	(is (or (string= "http://some.where/psis/author/goethe"
			 (first (tm-sparql::subject-result
				 (first (tm-sparql::select-group q-obj-1)))))
		(string= "http://some.where/psis/persons/goethe"
			 (first (tm-sparql::subject-result
				 (first (tm-sparql::select-group q-obj-1)))))))
	(is (string= "http://some.where/base-psis/first-name"
		     (first (tm-sparql::predicate-result
			     (first (tm-sparql::select-group q-obj-1))))))
	(is (string= "Johann Wolfgang"
		     (first (tm-sparql::object-result
			     (first (tm-sparql::select-group q-obj-1))))))
	(is (or (string= "http://some.where/psis/author/goethe"
			 (first (tm-sparql::subject-result
				 (first (tm-sparql::select-group q-obj-2)))))
		(string= "http://some.where/psis/persons/goethe"
			 (first (tm-sparql::subject-result
				 (first (tm-sparql::select-group q-obj-2)))))))
	(is (string= "http://some.where/base-psis/written"
		     (first (tm-sparql::predicate-result
			     (first (tm-sparql::select-group q-obj-2))))))
	(is (or (string= "http://some.where/psis/poem/zauberlehrling"
			 (first (tm-sparql::object-result
				 (first (tm-sparql::select-group q-obj-2)))))
		(string= "http://some.where/psis/poem/der_zauberlehrling"
			 (first (tm-sparql::object-result
				 (first (tm-sparql::select-group q-obj-2)))))
		(string= "http://some.where/psis/poem/erlkoenig"
			 (first (tm-sparql::object-result
				 (first (tm-sparql::select-group q-obj-2)))))))
	(is (or (string= "http://some.where/psis/author/goethe"
			 (second (tm-sparql::subject-result
				  (first (tm-sparql::select-group q-obj-2)))))
		(string= "http://some.where/psis/persons/goethe"
			 (second (tm-sparql::subject-result
				  (first (tm-sparql::select-group q-obj-2)))))))
	(is (string= "http://some.where/base-psis/written"
		     (second (tm-sparql::predicate-result
			      (first (tm-sparql::select-group q-obj-2))))))
	(is (or (string= "http://some.where/psis/poem/zauberlehrling"
			 (second (tm-sparql::object-result
				  (first (tm-sparql::select-group q-obj-2)))))
		(string= "http://some.where/psis/poem/der_zauberlehrling"
			 (second (tm-sparql::object-result
				  (first (tm-sparql::select-group q-obj-2)))))
		(string= "http://some.where/psis/poem/erlkoenig"
			 (second (tm-sparql::object-result
				  (first (tm-sparql::select-group q-obj-2)))))))
	(is-false (first (tm-sparql::subject-result
			  (first (tm-sparql::select-group q-obj-3)))))
	(is-false (first (tm-sparql::predicate-result
			  (first (tm-sparql::select-group q-obj-3)))))
	(is-false (first (tm-sparql::object-result
			  (first (tm-sparql::select-group q-obj-3)))))
	(is (or (string= "http://some.where/psis/author/goethe"
			 (first (tm-sparql::subject-result
				 (second (tm-sparql::select-group q-obj-3)))))
		(string= "http://some.where/psis/persons/goethe"
			 (first (tm-sparql::subject-result
				 (second (tm-sparql::select-group q-obj-3)))))))
	(is (string= "http://some.where/base-psis/last-name"
		     (first (tm-sparql::predicate-result
			     (second (tm-sparql::select-group q-obj-3))))))
	(is (string= "von Goethe"
		     (first (tm-sparql::object-result
			     (second (tm-sparql::select-group q-obj-3))))))))))


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
			       "http://some.where/psis/author/goethe")
		      (string= (first (getf (first (result q-obj-1)) :result))
			       "http://some.where/psis/persons/goethe")))
	      (is (= (length (getf (second (result q-obj-1)) :result)) 1))
	      (is (string= (first (getf (second (result q-obj-1)) :result))
			   "http://some.where/psis/poem/erlkoenig"))
	      (is (string= (getf (second (result q-obj-1)) :variable) "poems")))
	    (progn
	      (is (= (length (getf (second (result q-obj-1)) :result)) 1))
	      (is (or (string= (first (getf (second (result q-obj-1)) :result))
			       "http://some.where/psis/author/goethe")
		      (string= (first (getf (second (result q-obj-1)) :result))
			       "http://some.where/psis/persons/goethe")))
	      (is (= (length (getf (first (result q-obj-1)) :result)) 1))
	      (is (string= (first (getf (first (result q-obj-1)) :result))
			   "http://some.where/psis/poem/erlkoenig"))
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
	       (find "http://some.where/psis/poem/mondnacht"
		     (getf (second (result q-obj-2)) :result) :test #'string=))
	      (is-true
	       (find "http://some.where/psis/poem/resignation"
		     (getf (second (result q-obj-2)) :result) :test #'string=))
	      (is-true
	       (find "http://some.where/psis/poem/erlkoenig"
		     (getf (second (result q-obj-2)) :result) :test #'string=))
	      (is-true
	       (or
		(find "http://some.where/psis/poem/zauberlehrling"
		      (getf (second (result q-obj-2)) :result) :test #'string=)
		(find "http://some.where/psis/poem/der_zauberlehrling"
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
	       (find "http://some.where/psis/poem/mondnacht"
		     (getf (first (result q-obj-2)) :result) :test #'string=))
	      (is-true
	       (find "http://some.where/psis/poem/resignation"
		     (getf (first (result q-obj-2)) :result) :test #'string=))
	      (is-true
	       (find "http://some.where/psis/poem/erlkoenig"
		     (getf (first (result q-obj-2)) :result) :test #'string=))
	      (is-true
	       (or
		(find "http://some.where/psis/poem/zauberlehrling"
		      (getf (first (result q-obj-2)) :result) :test #'string=)
		(find "http://some.where/psis/poem/der_zauberlehrling"
		      (getf (first (result q-obj-2)) :result) :test #'string=)))))))))




(defun run-sparql-tests ()
  (it.bese.fiveam:run! 'sparql-test:sparql-tests))
