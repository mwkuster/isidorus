;;+-----------------------------------------------------------------------------
;;+  Isidorus
;;+  (c) 2008-2009 Marc Kuester, Christoph Ludwig, Lukas Giessmann
;;+
;;+  Isidorus is freely distributable under the LGPL license.
;;+  You can find a detailed description in trunk/docs/LGPL-LICENSE.txt.
;;+-----------------------------------------------------------------------------


(defpackage :exceptions
  (:use :common-lisp)
  (:export :inconsistent-file-error
           :missing-reference-error
	   :no-identifier-error
           :duplicate-identifier-error
           :object-not-found-error
	   :not-mergable-error
	   :missing-argument-error
	   :tm-reference-error))

(in-package :exceptions)

(define-condition inconsistent-file-error(error)
  ((message
    :initarg :message
    :accessor message)))


(define-condition missing-reference-error(error)
  ((message
    :initarg :message
    :accessor message)
   (reference 
    :accessor reference
    :initarg :reference))
  (:documentation "thrown is a reference is missing"))


(define-condition duplicate-identifier-error(error)
  ((message
    :initarg :message
    :accessor message)
   (uri
    :accessor reference
    :initarg :reference))
  (:documentation "thrown if the same identifier is already in use"))


(define-condition object-not-found-error(error)
  ((message
    :initarg :message
    :accessor message))
  (:documentation "thrown if the object could not be found"))


(define-condition no-identifier-error(error)
  ((message
    :initarg :message
    :accessor message)
   (internal-id 
    :initarg :internal-id
    :accessor internal-id))
  (:documentation "thrown if the topic has no identifier"))


(define-condition not-mergable-error (error)
  ((message
    :initarg :message
    :accessor message)
   (construc-1
    :initarg :construct-1
    :accessor construct-1)
   (construc-2
    :initarg :construct-2
    :accessor construct-2))
  (:documentation "Thrown if two constructs are not mergable since
                   they have e.g. difference types."))


(define-condition missing-argument-error (error)
  ((message
    :initarg :message
    :accessor message)
   (argument-symbol
    :initarg :argument-symbol
    :accessor argument-symbol)
   (function-symbol
    :initarg :function-symbol
    :accessor function-symbol))
  (:documentation "Thrown if a argument is missing in a function."))


(define-condition tm-reference-error (error)
  ((message
    :initarg :message
    :accessor message)
   (referenced-construct
    :initarg :referenced-construct
    :accessor referenced-construct)
   (existing-reference
    :initarg :existing-reference
    :accessor existing-reference)
   (new-reference
    :initarg :new-reference
    :accessor new-reference))
  (:documentation "Thrown of the referenced-construct is already owned by another
                   TM-construct (existing-reference) and is going to be referenced
                   by a second TM-construct (new-reference) at the same time."))