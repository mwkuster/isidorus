
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
	   :*xtm1.0-xlink*
	   :*rdf-ns*
	   :*rdfs-ns*
	   :*xml-ns*
	   :*xmlns-ns*
	   :*xml-string*
	   :*rdf2tm-ns*
	   :*rdf-statement*
	   :*rdf-object*
	   :*rdf-subject*
	   :*rdf-predicate*
	   :*rdf-nil*
	   :*rdf-first*
	   :*rdf-rest*
	   :*rdf2tm-object*
	   :*rdf2tm-subject*
	   :*rdf2tm-scope-prefix*))

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

(defparameter *rdf-ns* "http://www.w3.org/1999/02/22-rdf-syntax-ns#")

(defparameter *rdfs-ns* "http://www.w3.org/2000/01/rdf-schema#")

(defparameter *xml-ns* "http://www.w3.org/XML/1998/namespace")

(defparameter *xmlns-ns* "http://www.w3.org/2000/xmlns/")

(defparameter *xml-string* "http://www.w3.org/2001/XMLSchema#string")

(defparameter *rdf2tm-ns* "http://isidorus/rdf2tm_mapping#")

(defparameter *rdf-statement* "http://www.w3.org/1999/02/22-rdf-syntax-ns#Statement")

(defparameter *rdf-object* "http://www.w3.org/1999/02/22-rdf-syntax-ns#object")

(defparameter *rdf-subject* "http://www.w3.org/1999/02/22-rdf-syntax-ns#subject")

(defparameter *rdf-predicate* "http://www.w3.org/1999/02/22-rdf-syntax-ns#predicate")

(defparameter *rdf-nil* "http://www.w3.org/1999/02/22-rdf-syntax-ns#nil")

(defparameter *rdf-first* "http://www.w3.org/1999/02/22-rdf-syntax-ns#first")

(defparameter *rdf-rest* "http://www.w3.org/1999/02/22-rdf-syntax-ns#rest")

(defparameter *rdf2tm-object* "http://isidorus/rdf2tm_mapping#object")

(defparameter *rdf2tm-subject* "http://isidorus/rdf2tm_mapping#subject")

(defparameter *rdf2tm-scope-prefix* "http://isidorus/rdf2tm_mapping/scope#")