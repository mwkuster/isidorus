;;+-----------------------------------------------------------------------------
;;+  Isidorus
;;+  (c) 2008-2010 Marc Kuester, Christoph Ludwig, Lukas Georgieff
;;+
;;+  Isidorus is freely distributable under the LLGPL license.
;;+  You can find a detailed description in trunk/docs/LLGPL-LICENSE.txt and
;;+  trunk/docs/LGPL-LICENSE.txt.
;;+-----------------------------------------------------------------------------

(defpackage :trivial-queries-test
  (:use  :cl
	 :it.bese.FiveAM
	 :datamodel)
  (:export :run-trivial-queries-tests
	   :trivial-queries-tests))


(in-package :trivial-queries-test)


(def-suite trivial-queries-test
     :description "tests various key functions of the trivial-query-test of
                   the datamodel module")

(in-suite trivial-queries-test)


(defun run-trivial-queries-tests ()
  (it.bese.fiveam:run! 'trivial-queries-test:trivial-queries-tests))