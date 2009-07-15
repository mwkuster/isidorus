;;+-----------------------------------------------------------------------------
;;+  Isidorus
;;+  (c) 2008-2009 Marc Kuester, Christoph Ludwig, Lukas Giessmann
;;+
;;+  Isidorus is freely distributable under the LGPL license.
;;+  You can find a detailed description in trunk/docs/LGPL-LICENSE.txt.
;;+-----------------------------------------------------------------------------


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
   (asdf:find-component *isidorus-system* "xml/xtm/core_psis.xtm")))

