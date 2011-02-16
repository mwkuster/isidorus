;;+-----------------------------------------------------------------------------
;;+  Isidorus
;;+  (c) 2008-2010 Marc Kuester, Christoph Ludwig, Lukas Georgieff
;;+
;;+  Isidorus is freely distributable under the LLGPL license.
;;+  You can find a detailed description in trunk/docs/LLGPL-LICENSE.txt and
;;+  trunk/docs/LGPL-LICENSE.txt.
;;+-----------------------------------------------------------------------------

(defpackage :TM-SPARQL-Constants
  (:use :cl :base-tools)
  (:nicknames tms)
  (:export :*tms*
	   :*tms-reifier*
	   :*tms-role*
	   :*tms-player*
	   :*tms-topicProperty*
	   :*tms-scope*
	   :*tms-value*))

(in-package :TM-SPARQL-Constants)

(defvar *tms* "http://www.networkedplanet.com/tmsparql/")

(defvar *tms-reifier* (concat *tms* "reifier"))

(defvar *tms-role* (concat *tms* "role"))

(defvar *tms-player* (concat *tms* "player"))

(defvar *tms-topicProperty* (concat *tms* "topicProperty"))

(defvar *tms-scope* (concat *tms* "scope"))

(defvar *tms-value* (concat *tms* "value"))