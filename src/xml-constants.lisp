(defpackage :xml-constants
  (:use :common-lisp
	:asdf)
  (:import-from :constants
		*isidorus-system*)
  (:export :*xml-component*
	   :*core_psis.xtm*))

(in-package :xml-constants)

(defparameter *xml-component*
  (asdf:find-component *isidorus-system* "xml"))

(defparameter *core_psis.xtm*
  (asdf:component-pathname
   (asdf:find-component *isidorus-system* "xml/core_psis.xtm")))

