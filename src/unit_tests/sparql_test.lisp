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
	 :TM-SPARQL)
  (:export :run-sparql-tests
	   :sparql-tests))


(in-package :sparql-test)


(def-suite sparql-test
     :description "tests  various key functions of the TM-SPARQL module")

(in-suite sparql-test)


;TODO: prefix tests
;PREFIX foaf  :   <http://xmlns.com/foaf/0.1/>
;PREFIX org:    <http://example.com/ns#>
;PREFIX isi:<http://isidor.us>
;PREFIX :<http://some.where>
;PREFIX foaf : <http://overwrite.foaf>"


(defun run-sparql-tests ()
  (it.bese.fiveam:run! 'sparql-test:sparql-tests))