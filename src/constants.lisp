;;+-----------------------------------------------------------------------------
;;+  Isidorus
;;+  (c) 2008-2010 Marc Kuester, Christoph Ludwig, Lukas Georgieff
;;+
;;+  Isidorus is freely distributable under the LLGPL license.
;;+  You can find a detailed description in trunk/docs/LLGPL-LICENSE.txt and
;;+  trunk/docs/LGPL-LICENSE.txt.
;;+-----------------------------------------------------------------------------

(defpackage :constants
  (:use :cl :base-tools)
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
	   :*xml-boolean*
	   :*xml-decimal*
	   :*xml-double*
	   :*xml-integer*
	   :*xml-date*
	   :*xml-uri*
	   :*rdf2tm-ns*
	   :*rdf-statement*
	   :*rdf-object*
	   :*rdf-subject*
	   :*rdf-predicate*
	   :*rdf-nil*
	   :*rdf-first*
	   :*rdf-rest*
	   :*rdf-type*
	   :*rdf2tm-object*
	   :*rdf2tm-subject*
	   :*rdf2tm-scope-prefix*
	   :*tm2rdf-ns*
	   :*tm2rdf-topic-type-uri*
	   :*tm2rdf-name-type-uri*
	   :*tm2rdf-name-property*
	   :*tm2rdf-variant-type-uri*
	   :*tm2rdf-variant-property*
	   :*tm2rdf-occurrence-type-uri*
	   :*tm2rdf-occurrence-property*
	   :*tm2rdf-role-type-uri*
	   :*tm2rdf-role-property*
	   :*tm2rdf-association-type-uri*
	   :*tm2rdf-associaiton-property*
	   :*tm2rdf-subjectIdentifier-property*
	   :*tm2rdf-itemIdentity-property*
	   :*tm2rdf-subjectLocator-property*
	   :*tm2rdf-value-property*
	   :*tm2rdf-nametype-property*
	   :*tm2rdf-scope-property*
	   :*tm2rdf-varianttype-property*
	   :*tm2rdf-occurrencetype-property*
	   :*tm2rdf-roletype-property*
	   :*tm2rdf-associationtype-property*
	   :*tm2rdf-player-property*
	   :*rdf2tm-blank-node-prefix*
	   :*tm2rdf-reifier-property*
	   :*xsd-ns*
	   :*topic-name-psi*))
	   

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

(defparameter *xml-boolean* "http://www.w3.org/2001/XMLSchema#boolean")

(defparameter *xml-integer* "http://www.w3.org/2001/XMLSchema#integer")

(defparameter *xml-date* "http://www.w3.org/2001/XMLSchema#date")

(defparameter *xml-decimal* "http://www.w3.org/2001/XMLSchema#decimal")

(defparameter *xml-double* "http://www.w3.org/2001/XMLSchema#double")

(defparameter *xml-uri* "http://www.w3.org/2001/XMLSchema#anyURI")

(defparameter *rdf2tm-ns* "http://isidorus/rdf2tm_mapping/")

(defparameter *rdf-statement* (concat *rdf-ns* "Statement"))

(defparameter *rdf-object* (concat *rdf-ns* "object"))

(defparameter *rdf-subject* (concat *rdf-ns* "subject"))

(defparameter *rdf-predicate* (concat *rdf-ns* "predicate"))

(defparameter *rdf-nil* (concat *rdf-ns* "nil"))

(defparameter *rdf-type* (concat *rdf-ns* "type"))

(defparameter *rdf-first* (concat *rdf-ns* "first"))

(defparameter *rdf-rest* (concat *rdf-ns* "rest"))

(defparameter *rdf2tm-object* (concat *rdf2tm-ns* "object"))

(defparameter *rdf2tm-subject* (concat *rdf2tm-ns* "subject"))

(defparameter *rdf2tm-scope-prefix* (concat *rdf2tm-ns* "scope/"))

(defparameter *rdf2tm-blank-node-prefix* (concat *rdf2tm-ns* "blank_node/"))

(defparameter *tm2rdf-ns* "http://isidorus/tm2rdf_mapping/")

(defparameter *tm2rdf-topic-type-uri* (concat *tm2rdf-ns* "types/Topic"))

(defparameter *tm2rdf-name-type-uri* (concat *tm2rdf-ns* "types/Name"))

(defparameter *tm2rdf-name-property* (concat *tm2rdf-ns* "name"))

(defparameter *tm2rdf-variant-type-uri* (concat *tm2rdf-ns* "types/Variant"))

(defparameter *tm2rdf-variant-property* (concat *tm2rdf-ns* "variant"))

(defparameter *tm2rdf-occurrence-type-uri* (concat *tm2rdf-ns* "types/Occurrence"))

(defparameter *tm2rdf-occurrence-property* (concat *tm2rdf-ns* "occurrence"))

(defparameter *tm2rdf-role-type-uri* (concat *tm2rdf-ns* "types/Role"))

(defparameter *tm2rdf-role-property* (concat *tm2rdf-ns* "role"))

(defparameter *tm2rdf-association-type-uri* (concat *tm2rdf-ns* "types/Association"))

(defparameter *tm2rdf-association-property* (concat *tm2rdf-ns* "association"))

(defparameter *tm2rdf-subjectIdentifier-property* (concat *tm2rdf-ns* "subjectIdentifier"))

(defparameter *tm2rdf-subjectLocator-property* (concat *tm2rdf-ns* "subjectLocator"))

(defparameter *tm2rdf-itemIdentity-property* (concat *tm2rdf-ns* "itemIdentity"))

(defparameter *tm2rdf-value-property* (concat *tm2rdf-ns* "value"))

(defparameter *tm2rdf-nametype-property* (concat *tm2rdf-ns* "nametype"))

(defparameter *tm2rdf-scope-property* (concat *tm2rdf-ns* "scope"))

(defparameter *tm2rdf-varianttype-property* (concat *tm2rdf-ns* "varianttype"))

(defparameter *tm2rdf-occurrencetype-property* (concat *tm2rdf-ns* "occurrencetype"))

(defparameter *tm2rdf-roletype-property* (concat *tm2rdf-ns* "roletype"))

(defparameter *tm2rdf-associationtype-property* (concat *tm2rdf-ns* "associationtype"))

(defparameter *tm2rdf-player-property* (concat *tm2rdf-ns* "player"))

(defparameter *tm2rdf-reifier-property* (concat *tm2rdf-ns* "reifier"))

(defparameter *xsd-ns* "http://www.w3.org/2001/XMLSchema#")

(defparameter *topic-name-psi* "http://psi.topicmaps.org/iso13250/model/topic-name")