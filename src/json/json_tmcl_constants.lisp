;;+-----------------------------------------------------------------------------
;;+  Isidorus
;;+  (c) 2008-2009 Marc Kuester, Christoph Ludwig, Lukas Giessmann
;;+
;;+  Isidorus is freely distributable under the LGPL license.
;;+  You can find a detailed description in trunk/docs/LGPL-LICENSE.txt.
;;+-----------------------------------------------------------------------------


(defpackage :json-tmcl-constants
  (:use :cl)
  (:export :*topictype-psi*
	   :*topictype-constraint-psi*
	   :*associationtype-psi*
	   :*associationtype-constraint-psi*
	   :*roletype-psi*
	   :*roletype-constraint-psi*
	   :*occurrencetype-psi*
	   :*occurrencetype-constraint-psi*
	   :*nametype-psi*
	   :*nametype-constraint-psi*
	   :*scopetype-psi*
	   :*topictype-role-psi*
	   :*applies-to-psi*
	   :*constraint-role-psi*
	   :*regexp-psi*
	   :*card-min-psi*
	   :*card-max-psi*
	   :*datatype-psi*
	   :*subjectidentifier-constraint-psi*
	   :*subjectlocator-constraint-psi*
	   :*abstract-topictype-constraint-psi*
	   :*exclusive-instance-psi*
	   :*topicname-constraint-psi*
	   :*topicoccurrence-constraint-psi*
	   :*occurrencedatatype-constraint-psi*
	   :*uniqueoccurrence-constraint-psi*
	   :*roleplayer-constraint-psi*
	   :*otherrole-constraint-psi*
	   :*nametype-role-psi*
	   :*scopetype-role-psi*
	   :*occurrencetype-role-psi*
	   :*othertopictype-role-psi*
	   :*otherroletype-role-psi*
	   :*associationtype-role-psi*
	   :*nametypescope-constraint-psi*
	   :*occurrencetypescope-constraint-psi*
	   :*associationtypescope-constraint-psi*
	   :*associationrole-constraint-psi*
	   :*roletype-role-psi*))

(in-package :json-tmcl-constants)

(defparameter *topictype-psi* "http://psi.topicmaps.org/tmcl/topic-type")
(defparameter *topictype-constraint-psi* "http://psi.topicmaps.org/tmcl/topic-type-constraint")
(defparameter *associationtype-psi* "http://psi.topicmaps.org/tmcl/association-type")
(defparameter *associationtype-constraint-psi* "http://psi.topicmaps.org/tmcl/association-type-constraint")
(defparameter *roletype-psi* "http://psi.topicmaps.org/tmcl/role-type")
(defparameter *roletype-constraint-psi* "http://psi.topicmaps.org/tmcl/role-type-constraint")
(defparameter *occurrencetype-psi* "http://psi.topicmaps.org/tmcl/occurrence-type")
(defparameter *occurrencetype-constraint-psi* "http://psi.topicmaps.org/tmcl/occurrence-type-constraint")
(defparameter *nametype-psi* "http://psi.topicmaps.org/tmcl/name-type")
(defparameter *nametype-constraint-psi* "http://psi.topicmaps.org/tmcl/name-type-constraint")
(defparameter *scopetype-psi* "http://psi.topicmaps.org/tmcl/scope-type")
(defparameter *topictype-role-psi* "http://psi.topicmaps.org/tmcl/topic-type-role")
(defparameter *applies-to-psi* "http://psi.topicmaps.org/tmcl/applies-to")
(defparameter *constraint-role-psi* "http://psi.topicmaps.org/tmcl/constraint-role")
(defparameter *regexp-psi* "http://psi.topicmaps.org/tmcl/reg-exp")
(defparameter *card-min-psi* "http://psi.topicmaps.org/tmcl/card-min")
(defparameter *card-max-psi* "http://psi.topicmaps.org/tmcl/card-max")
(defparameter *datatype-psi* "http://psi.topicmaps.org/tmcl/datatype")
(defparameter *subjectidentifier-constraint-psi* "http://psi.topicmaps.org/tmcl/subject-identifier-constraint")
(defparameter *subjectlocator-constraint-psi* "http://psi.topicmaps.org/tmcl/subject-locator-constraint")
(defparameter *abstract-topictype-constraint-psi* "http://psi.topicmaps.org/tmcl/abstract-topic-type-constraint")
(defparameter *exclusive-instance-psi* "http://psi.topicmaps.org/tmcl/exclusive-instance")
(defparameter *topicname-constraint-psi* "http://psi.topicmaps.org/tmcl/topic-name-constraint")
(defparameter *topicoccurrence-constraint-psi* "http://psi.topicmaps.org/tmcl/topic-occurrence-constraint")
(defparameter *occurrencedatatype-constraint-psi* "http://psi.topicmaps.org/tmcl/occurrence-datatype-constraint")
(defparameter *uniqueoccurrence-constraint-psi* "http://psi.topicmaps.org/tmcl/unique-occurrence-constraint")
(defparameter *roleplayer-constraint-psi* "http://psi.topicmaps.org/tmcl/role-player-constraint")
(defparameter *otherrole-constraint-psi* "http://psi.topicmaps.org/tmcl/other-role-constraint")
(defparameter *nametypescope-constraint-psi* "http://psi.topicmaps.org/tmcl/name-type-scope-constraint")
(defparameter *occurrencetypescope-constraint-psi* "http://psi.topicmaps.org/tmcl/occurrence-type-scope-constraint")
(defparameter *associationtypescope-constraint-psi* "http://psi.topicmaps.org/tmcl/association-type-scope-constraint")
(defparameter *nametype-role-psi* "http://psi.topicmaps.org/tmcl/name-type-role")
(defparameter *scopetype-role-psi* "http://psi.topicmaps.org/tmcl/scope-type-role")
(defparameter *occurrencetype-role-psi* "http://psi.topicmaps.org/tmcl/occurrence-type-role")
(defparameter *othertopictype-role-psi* "http://psi.topicmaps.org/tmcl/other-topic-type-role")
(defparameter *otherroletype-role-psi* "http://psi.topicmaps.org/tmcl/other-role-type-role")
(defparameter *associationtype-role-psi* "http://psi.topicmaps.org/tmcl/association-type-role")
(defparameter *associationrole-constraint-psi* "http://psi.topicmaps.org/tmcl/association-role-constraint")
(defparameter *roletype-role-psi* "http://psi.topicmaps.org/tmcl/role-type-role")