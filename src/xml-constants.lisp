;;+-----------------------------------------------------------------------------
;;+  Isidorus
;;+  (c) 2008-2010 Marc Kuester, Christoph Ludwig, Lukas Georgieff
;;+
;;+  Isidorus is freely distributable under the LLGPL license.
;;+  You can find a detailed description in trunk/docs/LLGPL-LICENSE.txt and
;;+  trunk/docs/LGPL-LICENSE.txt.
;;+-----------------------------------------------------------------------------

(defpackage :xml-constants
  (:use :common-lisp
	:asdf)
  (:import-from :constants
		*isidorus-system*)
  (:export :*xml-component*
	   :*core_psis.xtm*
	   :*rdf_core_psis.xtm*
	   :*tmsparql_core_psis.xtm*))

(in-package :xml-constants)

(defparameter *xml-component*
  (asdf:find-component *isidorus-system* "xml"))

(defparameter *core_psis.xtm*
  (asdf:component-pathname
   (asdf:find-component *isidorus-system* "xml/xtm/core_psis.xtm")))

(defparameter *rdf_core_psis.xtm*
  (asdf:component-pathname
   (asdf:find-component *isidorus-system* "xml/rdf/rdf_core_psis.xtm")))

(defparameter *tmsparql_core_psis.xtm*
  (asdf:component-pathname
   (asdf:find-component *isidorus-system* "TM-SPARQL/tmsparql_core_psis.xtm")))