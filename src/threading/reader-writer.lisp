(defpackage :isidorus-reader-writer
  (:use :cl :hunchentoot-mp) ;hunchentoot 0.15.7
  (:export :current-readers
	   :with-reader-lock
	   :with-writer-lock))

(in-package :isidorus-reader-writer)

(defvar *readerlist-mutex* (make-lock "isidorus current-readers lock")) ;hunchentoot 0.15.7
(defvar *writer-mutex* (make-lock "isidorus writer lock")) ;hunchentoot 0.15.7
;;(defvar *readerlist-mutex* (hunchentoot::make-lock "isidorus current-readers lock")) ;hunchentoot 1.0.0
;;(defvar *writer-mutex* (hunchentoot::make-lock "isidorus writer lock")) ;hunchentoot 1.0.0

(defvar *current-readers* nil)

(defun current-readers ()
  (let
      ((result nil))
    ;;(with-lock (*readerlist-mutex*) ;hunchentoot 0.15.7
    (hunchentoot::with-lock-held (*readerlist-mutex*) ;hunchentoot 1.0.0
      (setf result (copy-list *current-readers*)))
    result))

(defun add-current-to-reader-list ()
  (with-lock (*writer-mutex*) ;hunchentoot 0.15.7
    (with-lock (*readerlist-mutex*) ;hunchentoot 0.15.7
  ;;(hunchentoot::with-lock-held (*writer-mutex*) ;hunchentoot 1.0.0
    ;;(hunchentoot::with-lock-held (*readerlist-mutex*) ;hunchentoot 1.0.0
      (push *current-process* *current-readers*))))

(defun remove-current-from-reader-list ()
  (with-lock (*readerlist-mutex*) ;hunchentoot 0.15.7
  ;;(hunchentoot::with-lock-held (*readerlist-mutex*) ;hunchentoot 1.0.0
    (setf *current-readers*
	  (delete *current-process* *current-readers*))))

(defmacro with-reader-lock (&body body)
  `(progn
     (add-current-to-reader-list)
     (handler-case
	 (progn ,@body)
       (condition (c)
	 (progn
	   (remove-current-from-reader-list)
	   (error c))))
     (remove-current-from-reader-list)))
	 

(defmacro with-writer-lock (&body body)
  `(with-lock (*writer-mutex*) ;hunchentoot 0.15.7
  ;;`(hunchentoot::with-lock-held (*writer-mutex*) ;hunchetoot 1.0.0
     (do
      ((remaining-readers (current-readers) (current-readers)))
      ((nullp remaining-raeders) nil)
       ;; TODO: replace hunchentoot's internal function by
       ;; something we are officially allowed to use.
       ;; make sure the current thread sleeps for, say, 500ms.
       (hunchentoot::process-allow-scheduling()))
     ,@body))


     
    