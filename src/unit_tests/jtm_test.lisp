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
	   :test-identifiers-to-jtm
	   :test-topic-reference
	   :test-type-scopes-reifier-to-jtm
	   :test-parent-references-to-jtm
	   :run-jtm-tests
	   :test-instance-ofs-to-jtm
	   :test-export-to-jtm-variant
	   :test-export-to-jtm-name
	   :test-export-to-jtm-occurrence))


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
	   (prefixes (jtm::create-prefix-list-for-tm (list goethe) goethe-assocs
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
						 :uri "http://some.where/example#psi-3"))
				 :item-identifiers
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


(test test-identifiers-to-jtm
  "Tests the function export-identifiers-to-jtm."
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
	  (top-5 (make-construct 'TopicC
				 :start-revision 100
				 :psis
				 (list
				  (make-instance 'PersistentIdC
						 :uri "http://some.where/example#psi-2")
				  (make-instance 'PersistentIdC
						 :uri "http://some.where/example#psi-3"))
				 :item-identifiers
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
	  (name-1 (make-construct 'NameC
				  :start-revision 100
				  :item-identifiers
				  (list
				   (make-construct
				    'ItemIdentifierC
				    :uri "http://some.where/example/ii-5"))))
	  (prefixes (list (list :pref "pref_1" :value "http://some.where/example#")
			  (list :pref "pref_2" :value "http://some.where/")
			  (list :pref "pref_3" :value "http://any.prefix/"))))
      (is (string= (jtm::export-identifiers-to-jtm top-1 :revision 0
						   :identifier-type 'PersistentIdC)
		   "[\"http:\\/\\/some.where\\/example\\/psi-1\"]"))
      (is (string= (jtm::export-identifiers-to-jtm top-1 :revision 0
						   :identifier-type 'PersistentIdC
						   :prefixes prefixes)
		   "[\"[pref_2:example\\/psi-1]\"]"))
      (is (string= (jtm::export-identifiers-to-jtm top-1 :revision 0
						   :identifier-type 'SubjectLocatorC)
		   "null"))
      (is (string= (jtm::export-identifiers-to-jtm top-1 :revision 0)
		   "null"))
      (is (string= (jtm::export-identifiers-to-jtm top-2 :revision 0)
		   "[\"http:\\/\\/some.where\\/example\\/ii-1\"]"))
      (is (string= (jtm::export-identifiers-to-jtm top-2 :revision 0
						   :prefixes prefixes)
		   "[\"[pref_2:example\\/ii-1]\"]"))
      (is (string= (jtm::export-identifiers-to-jtm top-2 :revision 0
						   :identifier-type 'SubjectLocatorC)
		   "null"))
      (is (string= (jtm::export-identifiers-to-jtm top-2 :revision 0
						   :identifier-type 'PersistentIdC)
		   "null"))
      (is (string= (jtm::export-identifiers-to-jtm top-3 :revision 0
						   :identifier-type 'SubjectLocatorC)
		   "[\"http:\\/\\/some.where\\/example\\/sl-1\"]"))
      (is (string= (jtm::export-identifiers-to-jtm top-3 :revision 0
						   :identifier-type 'SubjectLocatorC
						   :prefixes prefixes)
		   "[\"[pref_2:example\\/sl-1]\"]"))
      (is (string= (jtm::export-identifiers-to-jtm top-3 :revision 0
						   :identifier-type 'PersistentIdC)
		   "null"))
      (is (string= (jtm::export-identifiers-to-jtm top-3 :revision 0)
		   "null"))
      (signals exceptions:JTM-error
	(jtm::export-identifiers-to-jtm top-3 :revision 0 :identifier-type 'AnyType))
      (is (string= (jtm::export-identifiers-to-jtm top-5 :revision 0
						   :identifier-type 'PersistentIdC)
		   "[\"http:\\/\\/some.where\\/example#psi-2\",\"http:\\/\\/some.where\\/example#psi-3\"]"))
      (is (string= (jtm::export-identifiers-to-jtm top-5 :revision 0
						   :identifier-type 'PersistentIdC
						   :prefixes prefixes)
		   "[\"[pref_1:psi-2]\",\"[pref_1:psi-3]\"]"))
      (is (string= (jtm::export-identifiers-to-jtm top-5 :revision 0
						   :identifier-type 'SubjectLocatorC)
		   "[\"http:\\/\\/some.where\\/example#sl-2\",\"http:\\/\\/some.where\\/example#sl-3\"]"))
      (is (string= (jtm::export-identifiers-to-jtm top-5 :revision 0
						   :identifier-type 'SubjectLocatorC
						   :prefixes prefixes)
		   "[\"[pref_1:sl-2]\",\"[pref_1:sl-3]\"]"))
      (is (string= (jtm::export-identifiers-to-jtm top-5 :revision 0)
		   "[\"http:\\/\\/some.where\\/example#ii-2\",\"http:\\/\\/some.where\\/example#ii-3\"]"))
      (is (string= (jtm::export-identifiers-to-jtm top-5 :revision 0
						   :prefixes prefixes)
		   "[\"[pref_1:ii-2]\",\"[pref_1:ii-3]\"]"))
      
      (is (string= (jtm::export-identifiers-to-jtm
		    top-5 :revision 0
		    :prefixes (list (list :pref "pref_1"
					  :value "http://some.where/example#psi-2")))
		   "[\"http:\\/\\/some.where\\/example#ii-2\",\"http:\\/\\/some.where\\/example#ii-3\"]"))
      (is (string= (jtm::export-identifiers-to-jtm
		    top-5 :revision 0
		    :prefixes (list (list :pref "pref_1"
					  :value "http://any.pref/example#psi-2")))
		   "[\"http:\\/\\/some.where\\/example#ii-2\",\"http:\\/\\/some.where\\/example#ii-3\"]"))
      (is (string= (jtm::export-identifiers-to-jtm name-1 :revision 0)
		   "[\"http:\\/\\/some.where\\/example\\/ii-5\"]"))
      (is (string= (jtm::export-identifiers-to-jtm name-1 :revision 0
						   :prefixes prefixes)
		   "[\"[pref_2:example\\/ii-5]\"]")))))



(test test-type-scopes-reifier-to-jtm
  "Tests the functions export-type-to-jtm, export-scopes-to-jtm,
   and export-reifier-to-jtm."
  (with-fixture with-empty-db ("data_base")
    (let* ((top-1 (make-construct 'TopicC
				  :start-revision 100
				  :psis
				  (list
				   (make-construct 'PersistentIdC
						   :uri "http://some.where/example/psi-1"))))
	   (top-2 (make-construct 'TopicC
				  :start-revision 100
				  :locators
				  (list
				   (make-construct 'SubjectLocatorC
						   :uri "http://some.where/example/sl-1"))))
	   (top-3 (make-construct 'TopicC :start-revision 100))
	   (name-1 (make-construct 'NameC :start-revision 100
				   :charvalue "name-1"
				   :instance-of top-1))
	   (name-2 (make-construct 'NameC :start-revision 100
				   :charvalue "name-2"))
	   (name-3 (make-construct 'NameC :start-revision 100
				   :charvalue "name-3"
				   :instance-of top-3))
	   (occ-1 (make-construct 'OccurrenceC :start-revision 100
				  :charvalue "occ-1"
				  :themes (list top-1 top-2)))
	   (occ-2 (make-construct 'OccurrenceC :start-revision 100
				  :charvalue (list top-1 top-2)))
	   (occ-3 (make-construct 'OccurrenceC :start-revision 100
				  :charvalue "occ-3"
				  :themes (list top-1 top-3 top-2)))
	   (assoc-1 (make-construct 'AssociationC :start-revision 100
				    :reifier top-1))
	   (name-4 (make-construct 'NameC :start-revision 100
				   :charvalue "name-4"))
	   (occ-4 (make-construct 'OccurrenceC :start-revision 100
				  :charvalue "occ-4"
				  :reifier top-3))
	   (prefixes (list (list :pref "pref_1" :value "http://some.where/example/"))))
      (is (string= (jtm::export-type-to-jtm name-1 :revision 0)
		   "\"si:http:\\/\\/some.where\\/example\\/psi-1\""))
      (is (string= (jtm::export-type-to-jtm name-1 :revision 0 :prefixes prefixes)
		   "\"si:[pref_1:psi-1]\""))
      (is (string= (jtm::export-type-to-jtm name-2 :revision 0 :prefixes prefixes
					    :error-if-nil nil)
		   "null"))
      (signals exceptions:JTM-error (jtm::export-type-to-jtm name-3 :revision 0))
      (is (or (string= (jtm::export-scopes-to-jtm occ-1 :revision 0)
		       "[\"si:http:\\/\\/some.where\\/example\\/psi-1\",\"sl:http:\\/\\/some.where\\/example\\/sl-1\"]")
	      (string= (jtm::export-scopes-to-jtm occ-1 :revision 0)
		       "[\"sl:http:\\/\\/some.where\\/example\\/sl-1\",\"si:http:\\/\\/some.where\\/example\\/psi-1\"]")))
      (is (or (string= (jtm::export-scopes-to-jtm occ-1 :revision 0
						  :prefixes prefixes)
		       "[\"si:[pref_1:psi-1]\",\"sl:[pref_1:sl-1]\"]")
	      (string= (jtm::export-scopes-to-jtm occ-1 :revision 0
						  :prefixes prefixes)
		       "[\"sl:[pref_1:sl-1]\",\"si:[pref_1:psi-1]\"]")))
      (is (string= (jtm::export-scopes-to-jtm occ-2 :revision 0)
		   "null"))
      (signals exceptions:JTM-error (jtm::export-scopes-to-jtm occ-3 :revision 0))
      (is (string= (jtm::export-reifier-to-jtm assoc-1 :revision 0)
		   "\"si:http:\\/\\/some.where\\/example\\/psi-1\""))
      (is (string= (jtm::export-reifier-to-jtm name-4 :revision 0)
		   "null"))
      (signals exceptions::JTM-error (jtm::export-reifier-to-jtm occ-4 :revision 0)))))




(test test-parent-references-to-jtm
  "Tests the function export-parent-references-to-jtm."
  (with-fixture with-empty-db ("data_base")
    (let* ((var-1 (make-construct 'VariantC :start-revision 100
				  :charvalue "var-1"))
	   (var-2 (make-construct 'VariantC :start-revision 100))
	   (name-1 (make-construct 'NameC :start-revision 100
				   :item-identifiers
				   (list (make-construct
					  'ItemIdentifierC
					  :uri "http://some.where/example/ii-1"))
				    :charvalue "name-1"
				    :variants (list var-1)))
	   (name-2 (make-construct 'NameC :start-revision 100
				    :charvalue "name-2"))
	   (occ-1 (make-construct 'OccurrenceC :start-revision 100
				  :charvalue "occ-1"))
	   (top-1 (make-construct 'TopicC :start-revision 100
				  :names (list name-1)
				  :psis
				  (list
				   (make-construct 'PersistentIdC
						   :uri "http://some.where/example/psi-1"))))
	   (top-2 (make-construct 'TopicC :start-revision 100
				  :occurrences (list occ-1)
				  :locators
				  (list
				   (make-construct 'SubjectLocatorC
						   :uri "http://some.where/example/sl-1"))))
	   (top-3 (make-construct 'TopicC :start-revision 100
				  :names (list name-2)))
	   (assoc-1 (make-construct 'AssociationC :start-revision 100
				    :item-identifiers
				    (list (make-construct
					   'ItemIdentifierC
					   :uri "http://some.where/example/ii-3"))
				    :roles (list (list :player top-1
						       :start-revision 100))))
	   (tm (make-construct 'TopicMapC :start-revision 100
			       :topics (list top-3)
			       :associations (list assoc-1)
			       :item-identifiers
			       (list (make-construct
				      'ItemIdentifierC
				      :uri "http://some.where/example/ii-2"))))
	   (assoc-2 (make-construct 'AssociationC :start-revision 100
				    :roles (list (list :player top-2
						       :start-revision 100))))
	   (role-1 (first (roles assoc-1 :revision 0)))
	   (role-2 (first (roles assoc-2 :revision 0)))
	   (prefixes (list (list :pref "pref_1" :value "http://some.where/example/"))))
      (setf *TM-REVISION* 0)
      (is (string= (jtm::export-parent-references-to-jtm top-3)
		   "[\"ii:http:\\/\\/some.where\\/example\\/ii-2\"]"))
      (is (string= (jtm::export-parent-references-to-jtm top-3 :prefixes prefixes)
		   "[\"ii:[pref_1:ii-2]\"]"))
      (signals exceptions:JTM-error (jtm::export-parent-references-to-jtm top-1))
      (signals exceptions:JTM-error (jtm::export-parent-references-to-jtm assoc-2))
      (signals exceptions:JTM-error (jtm::export-parent-references-to-jtm tm))
      (signals exceptions:JTM-error (jtm::export-parent-references-to-jtm name-2))
      (signals exceptions:JTM-error (jtm::export-parent-references-to-jtm var-2))
      (signals exceptions:JTM-error (jtm::export-parent-references-to-jtm role-2))
      (is-true role-1)
      (is-true role-2)
      (is (string= (jtm::export-parent-references-to-jtm var-1)
		   "[\"ii:http:\\/\\/some.where\\/example\\/ii-1\"]"))
      (is (string= (jtm::export-parent-references-to-jtm var-1 :prefixes prefixes)
		   "[\"ii:[pref_1:ii-1]\"]"))
      (is (string= (jtm::export-parent-references-to-jtm name-1)
		   "[\"si:http:\\/\\/some.where\\/example\\/psi-1\"]"))
      (is (string= (jtm::export-parent-references-to-jtm name-1 :prefixes prefixes)
		   "[\"si:[pref_1:psi-1]\"]"))
      (is (string= (jtm::export-parent-references-to-jtm occ-1)
		   "[\"sl:http:\\/\\/some.where\\/example\\/sl-1\"]"))
      (is (string= (jtm::export-parent-references-to-jtm occ-1 :prefixes prefixes)
		   "[\"sl:[pref_1:sl-1]\"]"))
      (is (string= (jtm::export-parent-references-to-jtm assoc-1)
		   "[\"ii:http:\\/\\/some.where\\/example\\/ii-2\"]"))
      (is (string= (jtm::export-parent-references-to-jtm assoc-1 :prefixes prefixes)
		   "[\"ii:[pref_1:ii-2]\"]"))
      (is (string= (jtm::export-parent-references-to-jtm role-1)
		   "[\"ii:http:\\/\\/some.where\\/example\\/ii-3\"]"))
      (is (string= (jtm::export-parent-references-to-jtm role-1 :prefixes prefixes)
		   "[\"ii:[pref_1:ii-3]\"]")))))


(test test-instance-ofs-to-jtm
  "Tests the function export-instance-ofs-to-jtm."
  (with-fixture with-empty-db ("data_base")
    (let* ((top-1 (make-construct 'TopicC :start-revision 100
				  :psis
				  (list
				   (make-construct 'PersistentIdC
						   :uri "http://some.where/example#psi-1"))))
	   (top-2 (make-construct 'TopicC :start-revision 100
				  :locators
				  (list
				   (make-construct 'SubjectLocatorC
						   :uri "http://some.where/example#sl-1"))))
	   (top-3 (make-construct 'TopicC :start-revision 100
				  :item-identifiers
				  (list
				   (make-construct 'ItemIdentifierC
						   :uri "http://some.where/example#ii-1"))))
	   (top-4 (make-construct 'TopicC :start-revision 100))
	   (top-5 (make-construct 'TopicC :start-revision 100))
       	   (tit (make-construct 'TopicC :start-revision 100
				:psis (list (make-construct
					     'PersistentIdC
					     :uri *type-instance-psi*))))
	   (it (make-construct 'TopicC :start-revision 100
			       :psis (list (make-construct 'PersistentIdC
							   :uri *instance-psi*))))
	   (tt (make-construct 'TopicC :start-revision 100
			       :psis (list (make-construct 'PersistentIdC
							   :uri *type-psi*))))
	   (prefixes (list (list :pref "pref_1" :value "http://some.where/"))))
      (make-construct 'AssociationC :start-revision 100
		      :roles (list (list :player top-1 :instance-of tt
					 :start-revision 100)
				   (list :player top-4 :instance-of it
					 :start-revision 100))
		      :instance-of tit)
      (make-construct 'AssociationC :start-revision 100
		      :roles (list (list :player top-2 :instance-of tt
					 :start-revision 100)
				   (list :player top-4 :instance-of it
					 :start-revision 100))
		      :instance-of tit)
      (make-construct 'AssociationC :start-revision 100
		      :roles (list (list :player top-3 :instance-of tt
					 :start-revision 100)
				   (list :player top-5 :instance-of it
					 :start-revision 100))
		      :instance-of tit)
      (setf *TM-REVISION* 0)
      (is (string= (jtm::export-instance-ofs-to-jtm top-5)
		   "[\"ii:http:\\/\\/some.where\\/example#ii-1\"]"))
      (is (string= (jtm::export-instance-ofs-to-jtm top-5 :prefixes prefixes)
		   "[\"ii:[pref_1:example#ii-1]\"]"))
      (is (string= (jtm::export-instance-ofs-to-jtm top-1)
		   "null"))
      (is (or (string= (jtm::export-instance-ofs-to-jtm top-4)
		       "[\"si:http:\\/\\/some.where\\/example#psi-1\",\"sl:http:\\/\\/some.where\\/example#sl-1\"]")
	      (string= (jtm::export-instance-ofs-to-jtm top-4)
		       "[\"sl:http:\\/\\/some.where\\/example#sl-1\",\"si:http:\\/\\/some.where\\/example#psi-1\"]")))
      (is (or (string= (jtm::export-instance-ofs-to-jtm top-4 :prefixes prefixes)
		       "[\"si:[pref_1:example#psi-1]\",\"sl:[pref_1:example#sl-1]\"]")
	      (string= (jtm::export-instance-ofs-to-jtm top-4 :prefixes prefixes)
		       "[\"sl:[pref_1:example#sl-1]\",\"si:[pref_1:example#psi-1]\"]")))
      (make-construct 'AssociationC :start-revision 100
		      :roles (list (list :player top-4 :instance-of tt
					 :start-revision 100)
				   (list :player top-5 :instance-of it
					 :start-revision 100))
		      :instance-of tit)
      (signals exceptions:JTM-error (jtm::export-instance-ofs-to-jtm top-5)))))


(test test-export-to-jtm-variant
  "Tests the function export-to-jtm bound to VariantC and the function
   export-construct-as-jtm-string also bound to VariantC."
  (with-fixture with-empty-db ("data_base")
    (let* ((name-1 (make-construct 'NameC :start-revision 100
				   :item-identifiers
				   (list
				    (make-construct 'ItemIdentifierC
						    :uri "http://some.where/ii-1"))
				   :charvalue "name-1"))
	   (top-1
	    (make-construct 'TopicC :start-revision 100
			    :psis
			    (list (make-construct 'PersistentIdC
						  :uri "http://some.where/psi-1"))))
	   (top-2
	    (make-construct 'TopicC :start-revision 100
			    :locators
			    (list (make-construct 'SubjectLocatorC
						  :uri "http://some.where/sl-1"))))
	   (top-3
	    (make-construct 'TopicC :start-revision 100
			    :item-identifiers
			    (list (make-construct 'ItemIdentifierC
						  :uri "http://some.where/ii-2"))))
	   (var-1 (make-construct 'VariantC :start-revision 100
				  :charvalue "var-1"
				  :datatype *xml-string*
				  :themes (list top-1)
				  :parent name-1))
	   (var-2 (make-construct 'VariantC :start-revision 100
				  :item-identifiers
				  (list
				   (make-construct 'ItemIdentifierC
						   :uri "http://some.where/ii-3"))
				  :charvalue "http://any.uri"
				  :themes (list top-2)
				  :reifier top-3
				  :datatype *xml-uri*))
	   (jtm-1 (jtm::export-to-jtm var-1 :item-type-p nil :revision 0))
	   (jtm-2 (jtm::export-to-jtm var-2 :item-type-p nil :revision 0))
	   (jtm-str-1 (export-construct-as-jtm-string var-1 :revision 0))
	   (jtm-str-2 (export-construct-as-jtm-string
		       var-2 :jtm-format :1.0 :parent-p nil :revision 0)))

      (is (string= jtm-1
		   (concat "{\"item_identifiers\":null,\"datatype\":" (json:encode-json-to-string *xml-string*) ",\"value\":\"var-1\",\"scope\":[\"si:http:\\/\\/some.where\\/psi-1\"],\"reifier\":null}")))
      (is (string= jtm-2
		   (concat "{\"item_identifiers\":[\"http:\\/\\/some.where\\/ii-3\"],\"datatype\":" (json:encode-json-to-string *xml-uri*) ",\"value\":\"http:\\/\\/any.uri\",\"scope\":[\"sl:http:\\/\\/some.where\\/sl-1\"],\"reifier\":\"ii:http:\\/\\/some.where\\/ii-2\"}")))
      (is (string= jtm-str-1
		   (concat "{\"version\":\"1.1\",\"prefixes\":{\"xsd\":\"http:\\/\\/www.w3.org\\/2001\\/XMLSchema#\",\"pref_1\":\"http:\\/\\/some.where\\/\"},\"item_identifiers\":null,\"datatype\":" (json:encode-json-to-string *xml-string*) ",\"value\":\"var-1\",\"item_type\":\"variant\",\"parent\":[\"ii:[pref_1:ii-1]\"],\"scope\":[\"si:[pref_1:psi-1]\"],\"reifier\":null}")))
      (is (string= jtm-str-2
		   (concat "{\"version\":\"1.0\",\"item_identifiers\":[\"http:\\/\\/some.where\\/ii-3\"],\"datatype\":" (json:encode-json-to-string *xml-uri*) ",\"value\":\"http:\\/\\/any.uri\",\"item_type\":\"variant\",\"scope\":[\"sl:http:\\/\\/some.where\\/sl-1\"],\"reifier\":\"ii:http:\\/\\/some.where\\/ii-2\"}"))))))
    

(test test-export-to-jtm-name
  "Tests the function export-to-jtm bound to NameC and the function
   export-construct-as-jtm-string also bound to NameC."
  (with-fixture with-empty-db ("data_base")
    (let* ((top-1 (make-construct 'TopicC :start-revision 100
				  :psis
				  (list
				   (make-construct 'PersistentIdC
						   :uri "http://some.where/psi-1"))))
	   (top-2 (make-construct 'TopicC :start-revision 100
				  :locators
				  (list
				   (make-construct 'SubjectLocatorC
						   :uri "http://some.where/sl-1"))))
	   (top-3 (make-construct 'TopicC :start-revision 100
				  :item-identifiers
				  (list
				   (make-construct 'ItemIdentifierC
						   :uri "http://some.where/ii-1"))))
	   (var-1 (make-construct 'VariantC :start-revision 100
				  :themes (list top-2)
				  :charvalue "var-1"))
	   (var-2 (make-construct 'VariantC :start-revision 100
				  :themes (list top-2)
				  :charvalue "var-2"))
	   (name-1 (make-construct 'NameC :start-revision 100
				   :item-identifiers
				   (list
				   (make-construct 'ItemIdentifierC
						   :uri "http://some.where/ii-2"))
				   :themes (list top-1)
				   :instance-of top-2
				   :reifier top-3
				   :charvalue "name-1"
				   :parent top-1
				   :variants (list var-1 var-2)))
	   (name-2 (make-construct 'NameC :start-revision 100
				   :charvalue "name-2"))
	   (jtm-1 (jtm::export-to-jtm name-1 :item-type-p nil :revision 0))
	   (jtm-2 (jtm::export-to-jtm name-2 :item-type-p nil :revision 0))
	   (jtm-str-1 (export-construct-as-jtm-string name-1 :revision 0))
	   (jtm-str-2 (export-construct-as-jtm-string
		       name-2 :jtm-format :1.0 :parent-p nil :revision 0))
	   (prefixes (list (list :pref "pref_1" :value *xsd-ns*)
			   (list :pref "xsd" :value *xsd-ns*)
			   (list :pref "pref_2" :value "http://some.where/"))))
      (is (string= jtm-1
		   (concat "{\"item_identifiers\":[\"http:\\/\\/some.where\\/ii-2\"],\"value\":\"name-1\",\"type\":\"sl:http:\\/\\/some.where\\/sl-1\",\"scope\":[\"si:http:\\/\\/some.where\\/psi-1\"],\"variants\":[" (jtm::export-to-jtm var-1 :item-type-p nil :revision 0) "," (jtm::export-to-jtm var-2 :item-type-p nil :revision 0) "],\"reifier\":\"ii:http:\\/\\/some.where\\/ii-1\"}")))
      (is (string= jtm-2
		   "{\"item_identifiers\":null,\"value\":\"name-2\",\"type\":null,\"scope\":null,\"variants\":null,\"reifier\":null}"))
      (is (string= jtm-str-1
		   (concat "{\"version\":\"1.1\",\"prefixes\":{\"pref_1\":\"http:\\/\\/www.w3.org\\/2001\\/XMLSchema#\",\"xsd\":\"http:\\/\\/www.w3.org\\/2001\\/XMLSchema#\",\"pref_2\":\"http:\\/\\/some.where\\/\"},\"item_identifiers\":[\"[pref_2:ii-2]\"],\"value\":\"name-1\",\"type\":\"sl:[pref_2:sl-1]\",\"item_type\":\"name\",\"parent\":[\"si:[pref_2:psi-1]\"],\"scope\":[\"si:[pref_2:psi-1]\"],\"variants\":[" (jtm::export-to-jtm var-1 :item-type-p nil :revision 0 :prefixes prefixes) "," (jtm::export-to-jtm var-2 :item-type-p nil :revision 0 :prefixes prefixes) "],\"reifier\":\"ii:[pref_2:ii-1]\"}")))
      (is (string= jtm-str-2
		   "{\"version\":\"1.0\",\"item_identifiers\":null,\"value\":\"name-2\",\"type\":null,\"item_type\":\"name\",\"scope\":null,\"variants\":null,\"reifier\":null}")))))



(test test-export-to-jtm-occurrence
  "Tests the function export-to-jtm bound to NameC and the function
   export-construct-as-jtm-string also bound to NameC."
  (with-fixture with-empty-db ("data_base")
    (let* ((top-1 (make-construct 'TopicC :start-revision 100
				  :psis
				  (list
				   (make-construct 'PersistentIdC
						   :uri "http://some.where/psi-1"))))
	   (top-2 (make-construct 'TopicC :start-revision 100
				  :locators
				  (list
				   (make-construct 'SubjectLocatorC
						   :uri "http://some.where/sl-1"))))
	   (top-3 (make-construct 'TopicC :start-revision 100
				  :item-identifiers
				  (list
				   (make-construct 'ItemIdentifierC
						   :uri "http://some.where/ii-1"))))
	   (occ-1 (make-construct 'OccurrenceC :start-revision 100
				   :item-identifiers
				   (list
				   (make-construct 'ItemIdentifierC
						   :uri "http://some.where/ii-2"))
				   :themes (list top-1)
				   :instance-of top-2
				   :reifier top-3
				   :charvalue "occ-1"
				   :parent top-1))
	   (occ-2 (make-construct 'OccurrenceC :start-revision 100
				   :charvalue "http://any.uri"
				   :datatype *xml-uri*
				   :instance-of top-1))
	   (jtm-1 (jtm::export-to-jtm occ-1 :item-type-p nil :revision 0))
	   (jtm-2 (jtm::export-to-jtm occ-2 :item-type-p nil :revision 0))
	   (jtm-str-1 (export-construct-as-jtm-string occ-1 :revision 0))
	   (jtm-str-2 (export-construct-as-jtm-string
		       occ-2 :jtm-format :1.0 :parent-p nil :revision 0)))
      (is (string= jtm-1
		   (concat "{\"item_identifiers\":[\"http:\\/\\/some.where\\/ii-2\"],\"datatype\":" (json:encode-json-to-string *xml-string* ) ",\"type\":\"sl:http:\\/\\/some.where\\/sl-1\",\"value\":\"occ-1\",\"scope\":[\"si:http:\\/\\/some.where\\/psi-1\"],\"reifier\":\"ii:http:\\/\\/some.where\\/ii-1\"}")))
      (is (string= jtm-2
		   (concat "{\"item_identifiers\":null,\"datatype\":" (json:encode-json-to-string *xml-uri* ) ",\"type\":\"si:http:\\/\\/some.where\\/psi-1\",\"value\":\"http:\\/\\/any.uri\",\"scope\":null,\"reifier\":null}")))
      (is (string= jtm-str-1
		   (concat "{\"version\":\"1.1\",\"prefixes\":{\"xsd\":\"http:\\/\\/www.w3.org\\/2001\\/XMLSchema#\",\"pref_1\":\"http:\\/\\/some.where\\/\"},\"item_identifiers\":[\"[pref_1:ii-2]\"],\"datatype\":" (json:encode-json-to-string *xml-string* ) ",\"type\":\"sl:[pref_1:sl-1]\",\"value\":\"occ-1\",\"item_type\":\"occurrence\",\"parent\":[\"si:[pref_1:psi-1]\"],\"scope\":[\"si:[pref_1:psi-1]\"],\"reifier\":\"ii:[pref_1:ii-1]\"}")))
      (is (string= jtm-str-2
		   (concat "{\"version\":\"1.0\",\"item_identifiers\":null,\"datatype\":" (json:encode-json-to-string *xml-uri* ) ",\"type\":\"si:http:\\/\\/some.where\\/psi-1\",\"value\":\"http:\\/\\/any.uri\",\"item_type\":\"occurrence\",\"scope\":null,\"reifier\":null}"))))))


(defun run-jtm-tests()
  "Runs all tests of this test-suite."
  (it.bese.fiveam:run! 'jtm-tests))