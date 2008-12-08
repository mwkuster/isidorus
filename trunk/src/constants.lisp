(defpackage :constants
  (:use :cl)
  (:export :*atom-ns*
           :*egovpt-ns*
	   :*instance-psi*
	   :*isidorus-system*
	   :*type-instance-psi*
	   :*type-psi*
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

(defparameter *isidorus-system* (asdf:find-system "isidorus"))
