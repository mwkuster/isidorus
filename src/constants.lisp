;;+-----------------------------------------------------------------------------
;;+  Isidorus
;;+  (c) 2008-2009 Marc Kuester, Christoph Ludwig, Lukas Giessmann
;;+
;;+  Isidorus is freely distributable under the LGPL license.
;;+  You can find a detailed description in trunk/docs/LGPL-LICENSE.txt.
;;+-----------------------------------------------------------------------------


(defpackage :constants
  (:use :cl)
  (:export :*atom-ns*
           :*egovpt-ns*
	   :*instance-psi*
	   :*isidorus-system*
	   :*type-instance-psi*
	   :*type-psi*
	   :*supertype-subtype-psi*
	   :*supertype-psi*
	   :*subtype-psi*
	   :*xtm2.0-ns*
	   :*xtm1.0-ns*
	   :*xtm1.0-xlink*))

(in-package :constants)
(defparameter *xtm2.0-ns* "http://www.topicmaps.org/xtm/")

(defparameter *xtm1.0-ns* "http://www.topicmaps.org/xtm/1.0/")

(defparameter *xtm1.0-xlink* "http://www.w3.org/1999/xlink")

(defparameter *atom-ns* "http://www.w3.org/2005/Atom")

(defparameter *egovpt-ns* "http://www.egovpt.org/sdshare/")

(defparameter *type-instance-psi* "http://psi.topicmaps.org/iso13250/model/type-instance")

(defparameter *type-psi* "http://psi.topicmaps.org/iso13250/model/type")

(defparameter *instance-psi* "http://psi.topicmaps.org/iso13250/model/instance")

(defparameter *supertype-subtype-psi* "http://psi.topicmaps.org/iso13250/model/supertype-subtype")

(defparameter *supertype-psi* "http://psi.topicmaps.org/iso13250/model/supertype")

(defparameter *subtype-psi* "http://psi.topicmaps.org/iso13250/model/subtype")

(defparameter *isidorus-system* (asdf:find-system "isidorus"))
