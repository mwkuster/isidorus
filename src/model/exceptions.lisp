(defpackage :exceptions
  (:use :common-lisp)
  (:export :inconsistent-file-error
           :missing-reference-error
	   :no-identifier-error
           :duplicate-identifier-error
           :object-not-found-error))

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
