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
	 :exceptions)
  (:export :run-sparql-tests
	   :sparql-tests
	   :test-prefix-and-base))


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


(defun run-sparql-tests ()
  (it.bese.fiveam:run! 'sparql-test:sparql-tests))