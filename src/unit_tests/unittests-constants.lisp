;;+-----------------------------------------------------------------------------
;;+  Isidorus
;;+  (c) 2008-2010 Marc Kuester, Christoph Ludwig, Lukas Georgieff
;;+
;;+  Isidorus is freely distributable under the LLGPL license.
;;+  You can find a detailed description in trunk/docs/LLGPL-LICENSE.txt and
;;+  trunk/docs/LGPL-LICENSE.txt.
;;+-----------------------------------------------------------------------------

(defpackage :unittests-constants
  (:use :common-lisp)
  (:import-from :constants
		*isidorus-system*)
  (:import-from :asdf
		component-pathname
		find-component)
  (:export :*unit-tests-component*
	   :*unit-tests-path*
	   :*dangling_topicref.xtm*
	   :*inconsistent.xtm*               
	   :*notificationbase.xtm*           
	   :*notification_merge1.xtm*           	   
	   :*notification_merge2.xtm*           
	   :*sample_objects_2_0.xtm*
	   :*dangling_instanceof.xtm*        
	   :*duplicate_identifier.xtm*       
	   :*inconsistent_2_0.xtm*           
	   :*sample_objects.xtm*             
	   :*t100.xtm*
	   :*atom_test.xtm*
	   :*atom-conf.lisp*
	   :*poems.xtm*
	   :*sparql_test.xtm*
	   :*poems_light.rdf*
	   :*poems_light.xtm*
	   :*poems_light.xtm.txt*
	   :*full_mapping.rdf*
	   :*reification_xtm1.0.xtm*
	   :*reification_xtm2.0.xtm*
	   :*reification.rdf*
	   :*poems_light_tm_ii.xtm*
	   :*poems_light_tm_ii_merge.xtm*
	   :*poems_light_tm_reification_xtm1.0.xtm*))

(in-package :unittests-constants)

(defparameter *unit-tests-component*
  (asdf:find-component *isidorus-system* "unit_tests"))

(defparameter *unit-tests-path*
  (asdf:component-pathname *unit-tests-component*))

(defparameter *dangling_topicref.xtm*
  (asdf:component-pathname
   (asdf:find-component *unit-tests-component* "dangling_topicref.xtm")))

(defparameter *inconsistent.xtm*
  (asdf:component-pathname
   (asdf:find-component *unit-tests-component* "inconsistent.xtm")))               

(defparameter *notificationbase.xtm*
  (asdf:component-pathname
   (asdf:find-component *unit-tests-component* "notificationbase.xtm")))           

(defparameter *notification_merge1.xtm*
  (asdf:component-pathname
   (asdf:find-component *unit-tests-component* "notification_merge1.xtm")))           

(defparameter *notification_merge2.xtm*
  (asdf:component-pathname
   (asdf:find-component *unit-tests-component* "notification_merge2.xtm")))           

(defparameter *sample_objects_2_0.xtm*
  (asdf:component-pathname
   (asdf:find-component *unit-tests-component* "sample_objects_2_0.xtm")))

(defparameter *dangling_instanceof.xtm*
  (asdf:component-pathname
   (asdf:find-component *unit-tests-component* "dangling_instanceof.xtm")))        

(defparameter *duplicate_identifier.xtm*
  (asdf:component-pathname
   (asdf:find-component *unit-tests-component* "duplicate_identifier.xtm")))       

(defparameter *inconsistent_2_0.xtm*
  (asdf:component-pathname
   (asdf:find-component *unit-tests-component* "inconsistent_2_0.xtm")))           

(defparameter *sample_objects.xtm*
  (asdf:component-pathname
   (asdf:find-component *unit-tests-component* "sample_objects.xtm")))             

(defparameter *t100.xtm*
  (asdf:component-pathname
   (asdf:find-component *unit-tests-component* "t100.xtm")))

(defparameter *atom_test.xtm*
  (asdf:component-pathname
   (asdf:find-component *unit-tests-component* "atom_test.xtm")))

(defparameter *atom-conf.lisp*
  (asdf:component-pathname
   (asdf:find-component *unit-tests-component* "atom-conf")))

(defparameter *poems.xtm*
  (asdf:component-pathname
   (asdf:find-component *unit-tests-component* "poems.xtm")))

(defparameter *sparql_test.xtm*
  (asdf:component-pathname
   (asdf:find-component *unit-tests-component* "sparql_test.xtm")))

(defparameter *poems_light.rdf*
  (asdf:component-pathname
   (asdf:find-component *unit-tests-component* "poems_light.rdf")))

(defparameter *poems_light.xtm*
  (asdf:component-pathname
   (asdf:find-component *unit-tests-component* "poems_light.xtm")))

(defparameter *poems_light.xtm.txt*
  (asdf:component-pathname
   (asdf:find-component *unit-tests-component* "poems_light.xtm.txt")))

(defparameter *full_mapping.rdf*
  (asdf:component-pathname
   (asdf:find-component *unit-tests-component* "full_mapping.rdf")))

(defparameter *reification_xtm1.0.xtm*
  (asdf:component-pathname
   (asdf:find-component *unit-tests-component* "reification_xtm1.0.xtm")))

(defparameter *reification_xtm2.0.xtm*
  (asdf:component-pathname
   (asdf:find-component *unit-tests-component* "reification_xtm2.0.xtm")))

(defparameter *reification.rdf*
  (asdf:component-pathname
   (asdf:find-component *unit-tests-component* "reification.rdf")))

(defparameter *poems_light_tm_ii.xtm*
  (asdf:component-pathname
   (asdf:find-component *unit-tests-component* "poems_light_tm_ii.xtm")))

(defparameter *poems_light_tm_ii_merge.xtm*
  (asdf:component-pathname
   (asdf:find-component *unit-tests-component* "poems_light_tm_ii_merge.xtm")))

(defparameter *poems_light_tm_reification_xtm1.0.xtm*
  (asdf:component-pathname
   (asdf:find-component *unit-tests-component* "poems_light_tm_reification_xtm1.0.xtm")))
