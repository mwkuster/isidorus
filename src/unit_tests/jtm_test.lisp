;;+-----------------------------------------------------------------------------
;;+  Isidorus
;;+  (c) 2008-2010 Marc Kuester, Christoph Ludwig, Lukas Georgieff
;;+
;;+  Isidorus is freely distributable under the LLGPL license.
;;+  You can find a detailed description in trunk/docs/LLGPL-LICENSE.txt and
;;+  trunk/docs/LGPL-LICENSE.txt.
;;+-----------------------------------------------------------------------------

(defpackage :jtm-test
  (:use
   :common-lisp
   :jtm
   :constants
   :base-tools
   :xml-importer
   :datamodel
   :it.bese.FiveAM
   :unittests-constants
   :fixtures)
  (:export :test-create-prefixes
	   :test-topic-reference))


(in-package :jtm-test)


(def-suite jtm-tests
     :description "tests various functions of the jtm module")

(in-suite jtm-tests)


(test test-create-prefixes
  "Tests the functions that are responsible for generating xml-prefix-suffix
   pairs."
  (with-fixture with-tm-filled-db ("data_base" *sparql_test.xtm*)
    (let* ((goethe
	    (get-item-by-psi "http://some.where/tmsparql/author/goethe"
			     :revision 0))
	   (goethe-tm (first (in-topicmaps goethe :revision 0)))
	   (goethe-assocs (map 'list #'(lambda(role)
					  (parent role :revision 0))
				(player-in-roles goethe :revision 0)))
	   (prefixes (jtm::create-prefix-list (list goethe) goethe-assocs
					      goethe-tm :revision 0)))
      (is (= (length prefixes) 4))
      (is-false
       (set-exclusive-or (list "pref_1" "pref_2" "pref_3" "xsd")
			 (map 'list #'(lambda(item) (getf item :pref)) prefixes)
			 :test #'string=))
      (is-false
       (set-exclusive-or (list "http://some.where/tmsparql/author/"
			       "http://some.where/ii/"
			       "http://www.isidor.us/unittests/"
			       *xsd-ns*)
			 (map 'list #'(lambda(item) (getf item :value)) prefixes)
			 :test #'string=)))))


(test test-topic-reference
  "Tests all functions that are corresponding for topic references."
  (with-fixture with-empty-db ("data_base")
    (let ((top-1 (make-construct 'TopicC
				 :start-revision 100
				 :psis
				 (list
				  (make-construct 'PersistentIdC
						  :uri "http://some.where/example/psi-1"))))
	  (top-2 (make-construct 'TopicC
				 :start-revision 100
				 :item-identifiers
				 (list
				  (make-construct 'ItemIdentifierC
						  :uri "http://some.where/example/ii-1"))))
	  (top-3 (make-construct 'TopicC
				 :start-revision 100
				 :locators
				 (list
				  (make-construct 'SubjectLocatorC
						  :uri "http://some.where/example/sl-1"))))
	  (top-4 (make-construct 'TopicC :start-revision 100))
	  (top-5 (make-construct 'TopicC
				 :start-revision 100
				 :psis
				 (list
				  (make-instance 'PersistentIdC
						 :uri "http://some.where/example#psi-2")
				  (make-instance 'PersistentIdC
						 :uri "http://some.wherepexample#psi-3"))
				 :item-idenitfiers
				 (list
				  (make-instance 'ItemIdentifierC
						 :uri "http://some.where/example#ii-2")
				  (make-instance 'ItemIdentifierC
						 :uri "http://some.where/example#ii-3"))
				 :locators
				 (list
				  (make-instance 'SubjectLocatorC
						 :uri "http://some.where/example#sl-2")
				  (make-instance 'SubjectLocatorC
						 :uri "http://some.where/example#sl-3"))))
	  (prefixes (list (list :pref "pref_1" :value "http://some.where/example#")
			  (list :pref "pref_2" :value "http://some.where/")
			  (list :pref "pref_3" :value "http://any.prefix/"))))
      (is (string= (jtm::export-topic-reference-to-jtm top-1 :revision 0)
		   "\"si:http:\\/\\/some.where\\/example\\/psi-1\""))
      (is (string= (jtm::export-topic-reference-to-jtm top-2 :revision 0)
		   "\"ii:http:\\/\\/some.where\\/example\\/ii-1\""))
      (is (string= (jtm::export-topic-reference-to-jtm top-3 :revision 0)
		   "\"sl:http:\\/\\/some.where\\/example\\/sl-1\""))
      (signals exceptions::JTM-error
	(jtm::export-topic-reference-to-jtm top-4 :revision 0))
      (is (string= (jtm::export-topic-reference-to-jtm top-5 :revision 0)
		   "\"si:http:\\/\\/some.where\\/example#psi-2\""))
      (is (string= (jtm::export-topic-reference-to-jtm top-1 :revision 0
						       :prefixes prefixes)
		   "\"si:[pref_2:example\\/psi-1]\""))
      (is (string= (jtm::export-topic-reference-to-jtm top-2 :revision 0
						       :prefixes prefixes)
		   "\"ii:[pref_2:example\\/ii-1]\""))
      (is (string= (jtm::export-topic-reference-to-jtm top-3 :revision 0
						       :prefixes prefixes)
		   "\"sl:[pref_2:example\\/sl-1]\""))
      (is (string= (jtm::export-topic-reference-to-jtm top-5 :revision 0
						       :prefixes prefixes)
		   "\"si:[pref_1:psi-2]\""))
      (is (string= (jtm::export-topic-reference-to-jtm
		    top-5 :revision 0
		    :prefixes (list (list :pref "pref_1"
					  :value "http://some.where/example#psi-2")))
		   "\"si:http:\\/\\/some.where\\/example#psi-2\""))
      (is (string= (jtm::export-topic-reference-to-jtm
		    top-5 :revision 0
		    :prefixes (list (list :pref "pref_1"
					  :value "http://any.pref/example#psi-2")))
		   "\"si:http:\\/\\/some.where\\/example#psi-2\"")))))
				 