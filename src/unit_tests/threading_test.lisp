;;+-----------------------------------------------------------------------------
;;+  Isidorus
;;+  (c) 2008-2009 Marc Kuester, Christoph Ludwig, Lukas Giessmann
;;+
;;+  Isidorus is freely distributable under the LGPL license.
;;+  You can find a detailed description in trunk/docs/LGPL-LICENSE.txt.
;;+-----------------------------------------------------------------------------


(defpackage :threading-test
  (:use  :cl
	 :it.bese.FiveAM
	 :isidorus-threading
	 :bordeaux-threads)
  (:export :run-threading-tests
	   :test-helpers
	   :test-with-reader-lock
	   :test-with-writer-lock
	   :threading-test))


(in-package :threading-test)


(def-suite threading-test
     :description "tests  various key functions of the threading module")

(in-suite threading-test)

(test test-helpers
  "Tests the helper functions current-readers, add-thread-to-reader-list
   and remove-thread-from-reader-list"
  (is-true isidorus-threading::*readerlist-lock*)
  (is-true isidorus-threading::*writer-lock*)
  (is-false isidorus-threading::*current-readers*)
  (is-false (progn
	      (isidorus-threading::remove-thread-from-reader-list)
	      (current-readers)))
  (is (= 1 (length (progn
		       (isidorus-threading::add-thread-to-reader-list)
		       (current-readers)))))
  (is (eql (first (current-readers)) (current-thread)))
  (is (= 1 (length isidorus-threading::*current-readers*))) 
  (is-true (let ((copy-of-readers
		  (current-readers)))
	     (setf copy-of-readers nil)
	     isidorus-threading::*current-readers*))
  (setf isidorus-threading::*current-readers* nil)
  (is-false (current-readers))
  (is (= 2 (length (progn
		     (isidorus-threading::add-thread-to-reader-list)
		     (isidorus-threading::add-thread-to-reader-list)
		     (isidorus-threading::current-readers)))))
  (is (= 1 (progn
	     (isidorus-threading::remove-thread-from-reader-list)
	     (push t isidorus-threading::*current-readers*)
	     (length (current-readers)))))
  (setf isidorus-threading::*current-readers* nil))


(test test-with-reader-lock
  "Tests the macro with-reader-lock"
  (is-true isidorus-threading::*readerlist-lock*)
  (is-true isidorus-threading::*writer-lock*)
  (is-false isidorus-threading::*current-readers*)
  (let ((thread-1
	 (make-thread #'(lambda()
			  (with-reader-lock (sleep 3)))))
	(thread-2
	 (make-thread #'(lambda()
			  (with-reader-lock (sleep 3)))))
	(thread-3
	 (make-thread #'(lambda()
			  (with-reader-lock (sleep 3))))))
    (is (= 3 (length (current-readers))))
    (is-true (find thread-1 (current-readers)))
    (is-true (find thread-2 (current-readers)))
    (is-true (find thread-3 (current-readers)))
    (sleep 4)
    (is-false (current-readers)))
  (setf isidorus-threading::*current-readers* nil)
  (make-thread #'(lambda()
		   (with-lock-held (isidorus-threading::*readerlist-lock*)
		     (sleep 3))))
  (let ((start-time
	 (get-universal-time)))
    (isidorus-threading::add-thread-to-reader-list)
    (is (<= (+ 2 start-time) (get-universal-time))))
  (setf isidorus-threading::*current-readers* nil)
  (let ((start-time
	 (get-universal-time)))
    (make-thread #'(lambda() (with-reader-lock (sleep 3))))
    (make-thread #'(lambda() (with-reader-lock (sleep 3))))
    (is (> (+ start-time 3) (get-universal-time)))
    (is (= 2 (length (current-readers))))
    (sleep 4))
  (is-false (current-readers)))


(test test-with-writer-lock
  "Tests the macro with-writer-lock"
  (is-true isidorus-threading::*readerlist-lock*)
  (is-true isidorus-threading::*writer-lock*)
  (is-false isidorus-threading::*current-readers*)
  (let ((start-time
	 (get-universal-time)))
    (with-writer-lock nil)
    (is (>= (+ 1 start-time) (get-universal-time))))
  (make-thread #'(lambda()
		   (with-reader-lock #'(lambda()
					 (sleep 3)))))
  (let ((start-time
	 (get-universal-time)))
    (make-thread #'(lambda() (with-writer-lock (sleep 3))))
    (make-thread #'(lambda() (with-reader-lock (sleep 3))))
    (is-false (current-readers))
    (with-writer-lock nil)
    (is (<= (+ 3 start-time) (get-universal-time))))
  (let ((start-time
	 (get-universal-time)))
    (make-thread #'(lambda() (with-reader-lock (sleep 3))))
    (make-thread #'(lambda() (with-reader-lock (sleep 3))))
    (make-thread #'(lambda() (with-reader-lock (sleep 3))))
    (with-writer-lock nil)
    (is (<= (+ start-time 3) (get-universal-time)))))


(defun run-threading-tests ()
  "Runs all defined tests in this package"
  (it.bese.fiveam:run! 'test-helpers)
  (it.bese.fiveam:run! 'test-with-reader-lock)
  (it.bese.fiveam:run! 'test-with-writer-lock))