;;+-----------------------------------------------------------------------------
;;+  Isidorus
;;+  (c) 2008-2010 Marc Kuester, Christoph Ludwig, Lukas Georgieff
;;+
;;+  Isidorus is freely distributable under the LLGPL license.
;;+  You can find a detailed description in trunk/docs/LLGPL-LICENSE.txt and
;;+  trunk/docs/LGPL-LICENSE.txt.
;;+-----------------------------------------------------------------------------

(defpackage :isidorus-threading
  (:use :cl :sb-thread)
  (:export :current-readers
	   :with-reader-lock
	   :with-writer-lock))

(in-package :isidorus-threading)

(defvar *readerlist-lock* (make-mutex :name "isidorus-threading: current readers lock"))
(defvar *writer-lock* (make-mutex :name "isidorus-threading: writer lock"))
(defvar *current-readers* nil)


(defun current-readers ()
  "Returns a copy of the list which contains all current reader
   threads, *current-readers*"
  (with-mutex (*readerlist-lock*)
    (copy-list *current-readers*)))
 


(defun add-thread-to-reader-list ()
  "Adds the current thread to the reader list"
  (with-mutex (*writer-lock*)
    (with-mutex (*readerlist-lock*)
      (push *current-thread* *current-readers*))))


(defun remove-thread-from-reader-list ()
  "Removes the current threads from the reader list"
  (with-mutex (*readerlist-lock*)
    (setf *current-readers*
	  (delete *current-thread* *current-readers*))))


(defmacro with-reader-lock (&body body)
  "Executes the passed 'body' with the reader lock"
  `(progn
     (add-thread-to-reader-list)
     (let ((result nil))
       (handler-case
	   (setf result ,@body)
	 (condition (c)
	   (progn
	     (remove-thread-from-reader-list)
	     (error c))))
       (remove-thread-from-reader-list)
       result)))


(defmacro with-writer-lock (&body body)
  "Executes the passed body when the reader list is empty otherwise
   the do macor loops in 500 ms time interval for a next chance."
  `(with-mutex (*writer-lock*)
     (do
      ((remaining-readers (current-readers) (current-readers)))
      ((null remaining-readers))
       (sleep 0.05))
     ,@body))