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
   :xtm-importer
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
	   :test-export-to-jtm-occurrence
	   :test-export-to-jtm-topic
	   :test-export-to-jtm-role
	   :test-export-to-jtm-association
	   :test-export-to-jtm-fragment
	   :test-export-as-jtm
	   :test-import-jtm-references-1
	   :test-import-jtm-references-2
	   :test-get-item
	   :test-import-identifiers
	   :test-import-variants
	   :test-import-occurrences
	   :test-import-names
	   :test-make-instance-of-association
	   :test-import-topics))


(in-package :jtm-test)


(defun read-file (file-path)
  "A helper function that reads a file and returns the content as a string."
  (with-open-file (stream file-path)
    (let ((file-string ""))
      (do ((l (read-line stream) (read-line stream nil 'eof)))
	  ((eq l 'eof))
	(base-tools:push-string (base-tools::concat l (string #\newline)) file-string))
      (subseq file-string 0 (max 0 (1- (length file-string)))))))
  

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
  "Tests the function export-to-jtm bound to OccurrenceC and the function
   export-construct-as-jtm-string also bound to OccurrenceC."
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


(test test-export-to-jtm-topic
  "Tests the function export-to-jtm bound to TopicC and the function
   export-construct-as-jtm-string also bound to TopicC."
  (with-fixture with-empty-db ("data_base")
    (let* ((top-1 (make-construct 'TopicC :start-revision 100
				  :psis
				  (list
				   (make-construct 'PersistentIdC
						   :uri "http://some.where/psi-1")
				   (make-construct 'PersistentIdC
						   :uri "http://some.where/psi-2"))
				  :item-identifiers
				  (list
				   (make-construct 'ItemIdentifierC
						   :uri "http://some.where/ii-4"))
				  :locators
				  (list
				   (make-construct 'SubjectLocatorC
						   :uri "http://some.where/sl-2"))))
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
				   :instance-of top-1
				   :parent top-1))
	   (name-1 (make-construct 'NameC :start-revision 100
				   :charvalue "name-1"
				   :parent top-1))
	   (var-1 (make-construct 'VariantC :start-revision 100
				  :themes (list top-2 top-3)
				  :charvalue "var-1"))
	   (name-2 (make-construct 'NameC :start-revision 100
				   :charvalue  "name-2"
				   :themes (list top-2)
				   :variants (list var-1)
				   :parent top-1))
	   (tm (make-construct 'TopicMapC :start-revision 100
			       :item-identifiers
			       (list
				(make-construct 'ItemIdentifierC
						:uri "http://some.where/ii-3"))))
	   (tt (make-construct 'TopicC :start-revision 100
			       :psis
			       (list
				(make-construct 'PersistentIdC :start-revision 100
						:uri *type-psi*))))
	   (it (make-construct 'TopicC :start-revision 100
			       :psis
			       (list
				(make-construct 'PersistentIdC :start-revision 100
						:uri *instance-psi*))))
	   (tit (make-construct 'TopicC :start-revision 100
				:psis
				(list
				 (make-construct 'PersistentIdC :start-revision 100
						 :uri *type-instance-psi*))))
	   (jtm-1 (jtm::export-to-jtm top-1 :item-type-p nil :revision 0))
	   (jtm-str-1 (export-construct-as-jtm-string
			   top-1 :revision 0 :parent-p nil))
	   (jtm-2 (progn
		    (add-to-tm tm top-1)
		    (make-construct 'AssociationC :start-revision 100
				    :instance-of tit
				    :roles (list (list :player top-1
						       :start-revision 100
						       :instance-of it)
						 (list :player top-2
						       :start-revision 100
						       :instance-of tt)))
		    (make-construct 'AssociationC :start-revision 100
				    :instance-of tit
				    :roles (list (list :player top-1
						       :start-revision 100
						       :instance-of it)
						 (list :player top-3
						       :start-revision 100
						       :instance-of tt)))
		    (jtm::export-to-jtm top-1 :item-type-p nil :revision 0)))
	   (jtm-str-2 (export-construct-as-jtm-string
		       top-1 :jtm-format :1.0 :revision 0))
	   (prefixes (list (list :pref "pref_1" :value *xsd-ns*)
			   (list :pref "xsd" :value *xsd-ns*)
			   (list :pref "pref_2" :value "http://some.where/"))))
      (or occ-1 occ-2 name-1 name-2) ;only to avoid compilation warnings
      (is (string= jtm-1
		   (concat "{\"subject_identifiers\":[\"http:\\/\\/some.where\\/psi-1\",\"http:\\/\\/some.where\\/psi-2\"],\"subject_locators\":[\"http:\\/\\/some.where\\/sl-2\"],\"item_identifiers\":[\"http:\\/\\/some.where\\/ii-4\"],\"instance_of\":null,\"names\":[" (jtm::export-to-jtm (first (names top-1 :revision 0)) :item-type-p nil :revision 0) "," (jtm::export-to-jtm (second (names top-1 :revision 0)) :item-type-p nil :revision 0) "],\"occurrences\":[" (jtm::export-to-jtm (first (occurrences top-1 :revision 0)) :item-type-p nil :revision 0) "," (jtm::export-to-jtm (second (occurrences top-1 :revision 0)) :item-type-p nil :revision 0) "]}")))
      (is (string= jtm-2
		   (concat "{\"subject_identifiers\":[\"http:\\/\\/some.where\\/psi-1\",\"http:\\/\\/some.where\\/psi-2\"],\"subject_locators\":[\"http:\\/\\/some.where\\/sl-2\"],\"item_identifiers\":[\"http:\\/\\/some.where\\/ii-4\"],\"instance_of\":[\"sl:http:\\/\\/some.where\\/sl-1\",\"ii:http:\\/\\/some.where\\/ii-1\"],\"names\":[" (jtm::export-to-jtm (first (names top-1 :revision 0)) :item-type-p nil :revision 0) "," (jtm::export-to-jtm (second (names top-1 :revision 0)) :item-type-p nil :revision 0) "],\"occurrences\":[" (jtm::export-to-jtm (first (occurrences top-1 :revision 0)) :item-type-p nil :revision 0) "," (jtm::export-to-jtm (second (occurrences top-1 :revision 0)) :item-type-p nil :revision 0) "]}")))
      (is (string= jtm-str-1
		   (concat "{\"version\":\"1.1\",\"prefixes\":{\"pref_1\":\"http:\\/\\/www.w3.org\\/2001\\/XMLSchema#\",\"xsd\":\"http:\\/\\/www.w3.org\\/2001\\/XMLSchema#\",\"pref_2\":\"http:\\/\\/some.where\\/\"},\"subject_identifiers\":[\"[pref_2:psi-1]\",\"[pref_2:psi-2]\"],\"subject_locators\":[\"[pref_2:sl-2]\"],\"item_identifiers\":[\"[pref_2:ii-4]\"],\"instance_of\":null,\"item_type\":\"topic\",\"names\":[" (jtm::export-to-jtm (first (names top-1 :revision 0)) :item-type-p nil :revision 0 :prefixes prefixes :prefixes-p nil) "," (jtm::export-to-jtm (second (names top-1 :revision 0)) :item-type-p nil :revision 0 :prefixes prefixes :prefixes-p nil) "],\"occurrences\":[" (jtm::export-to-jtm (first (occurrences top-1 :revision 0)) :item-type-p nil :revision 0 :prefixes prefixes :prefixes-p nil) "," (jtm::export-to-jtm (second (occurrences top-1 :revision 0)) :item-type-p nil :revision 0 :prefixes prefixes :prefixes-p nil) "]}")))
      (is (string= jtm-str-2
		   (concat "{\"version\":\"1.0\",\"subject_identifiers\":[\"http:\\/\\/some.where\\/psi-1\",\"http:\\/\\/some.where\\/psi-2\"],\"subject_locators\":[\"http:\\/\\/some.where\\/sl-2\"],\"item_identifiers\":[\"http:\\/\\/some.where\\/ii-4\"],\"item_type\":\"topic\",\"parent\":[\"ii:http:\\/\\/some.where\\/ii-3\"],\"names\":[" (jtm::export-to-jtm (first (names top-1 :revision 0)) :item-type-p nil :revision 0) "," (jtm::export-to-jtm (second (names top-1 :revision 0)) :item-type-p nil :revision 0) "],\"occurrences\":[" (jtm::export-to-jtm (first (occurrences top-1 :revision 0)) :item-type-p nil :revision 0) "," (jtm::export-to-jtm (second (occurrences top-1 :revision 0)) :item-type-p nil :revision 0) "]}"))))))


(test test-export-to-jtm-role
  "Tests the function export-to-jtm bound to RoleC and the function
   export-construct-as-jtm-string also bound to RoleC."
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
	   (top-4 (make-construct 'TopicC :start-revision 100
				  :locators
				  (list
				   (make-construct 'SubjectLocatorC
						   :uri "http://some.where/sl-2"))))
	   (assoc-1 (make-construct 'AssociationC :start-revision 100
				    :item-identifiers
				    (list
				     (make-construct 'ItemIdentifierC
						     :uri "http://some.where/ii-2"))))
	   (role-1 (make-construct 'RoleC :start-revision 100
				   :player top-1
				   :instance-of top-2
				   :item-identifiers
				   (list 
				    (make-construct 'ItemIdentifierC
						    :uri "http://some.where/ii-3")
				    (make-construct 'ItemIdentifierC
						    :uri "http://some.where/ii-4"))
				   :reifier top-4
				   :parent assoc-1))
	   (role-2 (make-construct 'RoleC :start-revision 100
				   :player top-2
				   :instance-of top-3))
	   (jtm-1 (jtm::export-to-jtm role-1 :item-type-p nil :revision 0))
	   (jtm-2 (jtm::export-to-jtm role-2 :item-type-p nil :revision 0))
	   (jtm-str-1 (export-construct-as-jtm-string role-1 :revision 0))
	   (jtm-str-2 (export-construct-as-jtm-string
		       role-2 :jtm-format :1.0 :parent-p nil :revision 0)))
      (is (string= jtm-1
		   "{\"item_identifiers\":[\"http:\\/\\/some.where\\/ii-3\",\"http:\\/\\/some.where\\/ii-4\"],\"type\":\"sl:http:\\/\\/some.where\\/sl-1\",\"reifier\":\"sl:http:\\/\\/some.where\\/sl-2\",\"player\":\"si:http:\\/\\/some.where\\/psi-1\"}"))
      (is (string= jtm-2
		   "{\"item_identifiers\":null,\"type\":\"ii:http:\\/\\/some.where\\/ii-1\",\"reifier\":null,\"player\":\"sl:http:\\/\\/some.where\\/sl-1\"}"))
      (is (string= jtm-str-1
		   "{\"version\":\"1.1\",\"prefixes\":{\"xsd\":\"http:\\/\\/www.w3.org\\/2001\\/XMLSchema#\",\"pref_1\":\"http:\\/\\/some.where\\/\"},\"item_identifiers\":[\"[pref_1:ii-3]\",\"[pref_1:ii-4]\"],\"type\":\"sl:[pref_1:sl-1]\",\"item_type\":\"role\",\"parent\":[\"ii:[pref_1:ii-2]\"],\"reifier\":\"sl:[pref_1:sl-2]\",\"player\":\"si:[pref_1:psi-1]\"}"))
      (is (string= jtm-str-2
		   "{\"version\":\"1.0\",\"item_identifiers\":null,\"type\":\"ii:http:\\/\\/some.where\\/ii-1\",\"item_type\":\"role\",\"reifier\":null,\"player\":\"sl:http:\\/\\/some.where\\/sl-1\"}")))))


(test test-export-to-jtm-association
  "Tests the function export-to-jtm bound to AssociationC and the function
   export-construct-as-jtm-string also bound to AssociationC."
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
	   (assoc-1 (make-construct 'AssociationC :start-revision 100
				    :item-identifiers
				    (list
				     (make-construct 'ItemIdentifierC
						     :uri "http://some.where/ii-2")
				     (make-construct 'ItemIdentifierC
						     :uri "http://some.where/ii-3"))
				    :instance-of top-1
				    :themes (list top-1)
				    :roles
				    (list (list :player top-1
						:instance-of top-2
						:start-revision 100)
					  (list :player top-2
						:instance-of top-3
						:start-revision 100))))
	   (assoc-2 (make-construct 'AssociationC :start-revision 100
				    :instance-of top-1
				    :reifier top-2
				    :roles
				    (list (list :player top-2
						:instance-of top-3
						:start-revision 100))))
	   (tm (make-construct 'TopicMapC :start-revision 100
			       :item-identifiers
			       (list
				(make-construct 'ItemIdentifierC
						:uri "http://some.where/ii-4"))))
	   (jtm-1 (jtm::export-to-jtm assoc-1 :item-type-p nil :revision 0))
	   (jtm-2 (jtm::export-to-jtm assoc-2 :item-type-p nil :revision 0))
	   (jtm-str-1 (progn
			(add-to-tm tm assoc-1)
			(export-construct-as-jtm-string assoc-1 :revision 0)))
	   (jtm-str-2 (export-construct-as-jtm-string
		       assoc-2 :jtm-format :1.0 :parent-p nil :revision 0))
	   (prefixes (list (list :pref "pref_1" :value *xsd-ns*)
			   (list :pref "xsd" :value *xsd-ns*)
			   (list :pref "pref_2" :value "http://some.where/"))))
      (is (string= jtm-1
		   (concat "{\"item_identifiers\":[\"http:\\/\\/some.where\\/ii-2\",\"http:\\/\\/some.where\\/ii-3\"],\"type\":\"si:http:\\/\\/some.where\\/psi-1\",\"reifier\":null,\"scope\":[\"si:http:\\/\\/some.where\\/psi-1\"],\"roles\":[" (jtm::export-to-jtm (first (roles assoc-1 :revision 0)) :item-type-p nil :revision 0) "," (jtm::export-to-jtm (second (roles assoc-1 :revision 0)) :item-type-p nil :revision 0) "]}")))
      (is (string= jtm-2
		   (concat "{\"item_identifiers\":null,\"type\":\"si:http:\\/\\/some.where\\/psi-1\",\"reifier\":\"sl:http:\\/\\/some.where\\/sl-1\",\"scope\":null,\"roles\":[" (jtm::export-to-jtm (first (roles assoc-2 :revision 0)) :item-type-p nil :revision 0)"]}")))
      (is (string= jtm-str-1
		   (concat "{\"version\":\"1.1\",\"prefixes\":{\"pref_1\":\"http:\\/\\/www.w3.org\\/2001\\/XMLSchema#\",\"xsd\":\"http:\\/\\/www.w3.org\\/2001\\/XMLSchema#\",\"pref_2\":\"http:\\/\\/some.where\\/\"},\"item_identifiers\":[\"[pref_2:ii-2]\",\"[pref_2:ii-3]\"],\"type\":\"si:[pref_2:psi-1]\",\"item_type\":\"association\",\"parent\":[\"ii:[pref_2:ii-4]\"],\"reifier\":null,\"scope\":[\"si:[pref_2:psi-1]\"],\"roles\":[" (jtm::export-to-jtm (first (roles assoc-1 :revision 0)) :item-type-p nil :revision 0 :prefixes prefixes) "," (jtm::export-to-jtm (second (roles assoc-1 :revision 0)) :item-type-p nil :revision 0 :prefixes prefixes) "]}")))
      (is (string= jtm-str-2
		   (concat "{\"version\":\"1.0\",\"item_identifiers\":null,\"type\":\"si:http:\\/\\/some.where\\/psi-1\",\"item_type\":\"association\",\"reifier\":\"sl:http:\\/\\/some.where\\/sl-1\",\"scope\":null,\"roles\":[" (jtm::export-to-jtm (first (roles assoc-2 :revision 0)) :item-type-p nil :revision 0)"]}"))))))


(test test-export-to-jtm-fragment
  "Tests the function export-to-jtm bound to FragmentC and the function
   export-construct-as-jtm-string also bound to FragmentC."
  (with-fixture with-tm-filled-db ("data_base" *sparql_test.xtm*)
    (let* ((fragment
	    (d::create-latest-fragment-of-topic
	     "http://some.where/tmsparql/author/goethe"))
	   (jtm-1 (jtm::export-to-jtm fragment :item-type-p nil :revision 0))
	   (jtm-str-1 (export-construct-as-jtm-string fragment :revision 0))
	   (jtm-str-2 (export-construct-as-jtm-string
		       fragment :jtm-format :1.0 :parent-p nil :revision 0))
	   (prefixes (list
		      (list :pref "pref_1"
			    :value "http://some.where/tmsparql/author/")
		      (list :pref "xsd" :value *xsd-ns*)
		      (list :pref "pref_3" :value "http://some.where/psis/poem/")
		      (list :pref "pref_2" :value "http://some.where/tmsparql/")
		      (list :pref "pref_4" :value "http://some.where/ii/zb/")
		      (list :pref "pref_5" :value "http://some.where/ii/"))))
      (is (string= jtm-1
		   (concat "{\"topics\":"
			   (jtm::export-topics-to-jtm
			    (append (d:referenced-topics fragment)
				    (list (d:topic fragment))
				    (list
				     (d:get-item-by-psi *type-instance-psi*
							:revision 0)
				     (d:get-item-by-psi *instance-psi*
							:revision 0)
				     (d:get-item-by-psi *type-psi*
							:revision 0)))
			    :item-type-p nil :parent-p nil :prefixes nil
			    :revision 0 :instance-of-p nil)
			   ",\"associations\":"
			   (jtm::export-associations-to-jtm
			    (append
			     (d:associations fragment)
			     (instance-of-associations (topic fragment) :revision 0))
			    :item-type-p nil :parent-p nil :prefixes nil :revision 0)
			   ",\"item_identifiers\":null,\"reifier\":null}")))
      (is (string= jtm-str-1
		   (concat "{\"version\":\"1.1\",\"prefixes\":"
			   (jtm::export-prefix-list-to-jtm prefixes)
			   ",\"topics\":"
			   (jtm::export-topics-to-jtm
			    (append (d:referenced-topics fragment)
				    (list (d:topic fragment)))
			    :item-type-p nil :parent-p nil :prefixes prefixes
			    :revision 0 :instance-of-p t)
			   ",\"associations\":"
			   (jtm::export-associations-to-jtm
			    (d:associations fragment)
			    :item-type-p nil :parent-p nil
			    :prefixes prefixes :revision 0)
			   ",\"item_type\":\"topicmap\",\"item_identifiers\":null,\"reifier\":null}")))
      (is (string= jtm-str-2
		   (concat "{\"version\":\"1.0\",\"topics\":"
			   (jtm::export-topics-to-jtm
			    (append (d:referenced-topics fragment)
				    (list (d:topic fragment))
				    (list
				     (d:get-item-by-psi *type-instance-psi*
							:revision 0)
				     (d:get-item-by-psi *instance-psi*
							:revision 0)
				     (d:get-item-by-psi *type-psi*
							:revision 0)))
			    :item-type-p nil :parent-p nil :prefixes nil
			    :revision 0 :instance-of-p nil)
			   ",\"associations\":"
			   (jtm::export-associations-to-jtm
			    (append
			     (d:associations fragment)
			     (instance-of-associations (topic fragment) :revision 0))
			    :item-type-p nil :parent-p nil :prefixes nil :revision 0)
			   ",\"item_type\":\"topicmap\",\"item_identifiers\":null,\"reifier\":null}"))))))


(test test-export-as-jtm
  "Tests the function export-as-jtm."
  (with-fixture with-tm-filled-db ("data_base" *sparql_test.xtm*)
    (let ((jtm-path-1 
	   (merge-pathnames
	    (asdf:component-pathname
	     (asdf:find-component constants:*isidorus-system* "unit_tests"))
	    "out_sparql_xtm_1.jtm"))
	  (jtm-path-2
	   (merge-pathnames
	    (asdf:component-pathname
	     (asdf:find-component constants:*isidorus-system* "unit_tests"))
	    "out_sparql_xtm_2.jtm"))
	  (jtm-path-3
	   (merge-pathnames
	    (asdf:component-pathname
	     (asdf:find-component constants:*isidorus-system* "unit_tests"))
	    "out_sparql_xtm_3.jtm"))
	  (jtm-path-4
	   (merge-pathnames
	    (asdf:component-pathname
	     (asdf:find-component constants:*isidorus-system* "unit_tests"))
	    "out_sparql_xtm_4.jtm")))
      (handler-case (delete-file jtm-path-1) (condition () nil))
      (handler-case (delete-file jtm-path-2) (condition () nil))
      (handler-case (delete-file jtm-path-3) (condition () nil))
      (handler-case (delete-file jtm-path-4) (condition () nil))
      (export-as-jtm jtm-path-1 :tm-id nil :revision 0 :jtm-format :1.1)
      (export-as-jtm jtm-path-2 :tm-id nil :revision 0 :jtm-format :1.0)
      (export-as-jtm jtm-path-3 :tm-id fixtures::tm-id :revision 0 :jtm-format :1.1)
      (export-as-jtm jtm-path-4 :tm-id fixtures::tm-id :revision 0 :jtm-format :1.0)
      (let ((jtm-str-1 (read-file jtm-path-1))
	    (jtm-str-2 (read-file jtm-path-2))
	    (jtm-str-3 (read-file jtm-path-3))
	    (jtm-str-4 (read-file jtm-path-4))
	    (prefixes (list
		       (list :pref "pref_1"
			     :value "http://www.topicmaps.org/xtm/1.0/core.xtm#")
		       (list :pref "pref_2"
			     :value "http://psi.topicmaps.org/iso13250/model/")
		       (list :pref "pref_5"
			     :value "http://some.where/tmsparql/author/")
		       (list :pref "xsd" :value *xsd-ns*)
		       (list :pref "pref_3"
			     :value "http://psi.topicmaps.org/tmcl/")
		       (list :pref "pref_6"
			     :value "http://some.where/psis/poem/")
		       (list :pref "pref_4"
			     :value "http://some.where/tmsparql/")
		       (list :pref "pref_7"
			     :value "http://some.where/ii/zb/")
		       (list :pref "pref_8"
			     :value "http://some.where/ii/")))
	    (prefixes-2 (list
			 (list :pref "pref_3"
			       :value "http://some.where/tmsparql/author/")
			 (list :pref "xsd" :value *xsd-ns*)
			 (list :pref "pref_7"
			       :value "http://www.isidor.us/unittests/")
			 (list :pref "pref_5"
			       :value "http://psi.topicmaps.org/tmcl/")
			 (list :pref "pref_1"
			       :value "http://some.where/psis/poem/")
			 (list :pref "pref_4"
			       :value "http://some.where/tmsparql/")
			 (list :pref "pref_2"
			       :value "http://some.where/ii/zb/")
			 (list :pref "pref_6"
			       :value "http://some.where/ii/"))))
	(is (string= jtm-str-1
		     (concat "{\"version\":\"1.1\",\"prefixes\":"
			     (jtm::export-prefix-list-to-jtm prefixes)
			     ",\"item_identifiers\":null,\"topics\":"
			     (jtm::export-topics-to-jtm
			      (elephant:get-instances-by-class 'd:TopicC)
			      :item-type-p nil :parent-p nil :prefixes prefixes
			      :instance-of-p t :revision 0)
			     ",\"associations\":"
			     (jtm::export-associations-to-jtm
			      (remove-null
			       (map 'list
				    #'(lambda(assoc)
					(unless (eql
						 (d:instance-of assoc :revision 0)
						 (d:get-item-by-psi *type-instance-psi*
								    :revision 0))
					  assoc))
				    (elephant:get-instances-by-class 'd:AssociationC)))
			      :item-type-p nil :parent-p nil :prefixes prefixes
			      :revision 0)
			     ",\"item_type\":\"topicmap\",\"reifier\":null}")))
	(is (string=
	     jtm-str-2
	     (concat "{\"version\":\"1.0\","
		     "\"item_identifiers\":null,\"topics\":"
		     (jtm::export-topics-to-jtm
		      (elephant:get-instances-by-class 'd:TopicC)
		      :item-type-p nil :parent-p nil
		      :instance-of-p nil :revision 0)
		     ",\"associations\":"
		     (jtm::export-associations-to-jtm
		      (elephant:get-instances-by-class 'd:AssociationC)
		      :item-type-p nil :parent-p nil :revision 0)
		     ",\"item_type\":\"topicmap\",\"reifier\":null}")))
	(is (string= jtm-str-3
		     (concat "{\"version\":\"1.1\",\"prefixes\":"
			     (jtm::export-prefix-list-to-jtm prefixes-2)
			     ",\"item_identifiers\":[\"[pref_7:testtm]\"],\"topics\":"
			     (jtm::export-topics-to-jtm
			      (reverse
			       (remove
				(d:get-item-by-psi *type-instance-psi* :revision 0)
				(remove
				 (d:get-item-by-psi *instance-psi* :revision 0)
				 (remove
				  (d:get-item-by-psi *type-psi* :revision 0)
				  (d:topics
				   (d:identified-construct
				    (elephant:get-instance-by-value
				     'd:ItemIdentifierC 'd:uri
				     "http://www.isidor.us/unittests/testtm")
				    :revision 0))))))
			      :item-type-p nil :parent-p nil :prefixes prefixes-2
			      :instance-of-p t :revision 0)
			     ",\"associations\":"
			     (jtm::export-associations-to-jtm
			       (remove-null
				(map 'list
				     #'(lambda(assoc)
					 (unless
					     (eql
					      (d:instance-of assoc :revision 0)
					      (d:get-item-by-psi *type-instance-psi*
								 :revision 0))
					   assoc))
				     (d:associations
				      (d:identified-construct
				       (elephant:get-instance-by-value
					'd:ItemIdentifierC 'd:uri
					"http://www.isidor.us/unittests/testtm")
				       :revision 0))))
			      :item-type-p nil :parent-p nil :prefixes prefixes-2
			      :revision 0)
			     ",\"item_type\":\"topicmap\",\"reifier\":null}")))
	(is (string=
	     jtm-str-4
	     (concat "{\"version\":\"1.0\",\"item_identifiers\":"
		     "[\"http:\\/\\/www.isidor.us\\/unittests\\/testtm\"]"
		     ",\"topics\":"
		     (jtm::export-topics-to-jtm
		      (d:topics
		       (d:identified-construct
			(elephant:get-instance-by-value
			 'd:ItemIdentifierC 'd:uri
			 "http://www.isidor.us/unittests/testtm")
			:revision 0))
		      :item-type-p nil :parent-p nil :instance-of-p nil :revision 0)
		     ",\"associations\":"
		     (jtm::export-associations-to-jtm
		      (d:associations
		       (d:identified-construct
			(elephant:get-instance-by-value
			 'd:ItemIdentifierC 'd:uri
			 "http://www.isidor.us/unittests/testtm")
			:revision 0))
		      :item-type-p nil :parent-p nil :revision 0)
		     ",\"item_type\":\"topicmap\",\"reifier\":null}")))))))


(test test-import-jtm-references-1
  "Tests all functions that are responsible for processing and searching
   constructs by jtm-references."
  (with-fixture with-empty-db ("data_base")
    (let ((prefixes (list (list :pref "pref_1" :value "http://pref.org/")
			  (list :pref "pref_3" :value "http://pref.org/app/")
			  (list :pref "pref_2" :value "http://pref.org/app#"))))
      (is (string= (jtm::compute-full-uri prefixes "pref_1" "suffix-1")
		   "http://pref.org/suffix-1"))
      (is (string= (jtm::compute-full-uri prefixes "pref_3" "suffix-2")
		   "http://pref.org/app/suffix-2"))
      (is (string= (jtm::compute-full-uri prefixes "pref_2" "suffix-3")
		   "http://pref.org/app#suffix-3"))
      (signals exceptions:jtm-error
	(jtm::compute-full-uri prefixes "pref_4" "suffix-3"))
      (signals exceptions:jtm-error
	(jtm::compute-full-uri prefixes "pref_1" ""))
      (is (eql (jtm::get-identifier-type-from-jtm-reference "ii:[pref:suff]")
	       'ItemIdentifierC))
      (is (eql (jtm::get-identifier-type-from-jtm-reference "si:http://pref.suf")
	       'PersistentIdC))
      (is (eql (jtm::get-identifier-type-from-jtm-reference "sl:")
	       'SubjectLocatorC))
      (signals exceptions::JTM-error
	(jtm::get-identifier-type-from-jtm-reference "xy:[pref:suff]"))
      (signals exceptions::JTM-error
	(jtm::get-identifier-type-from-jtm-reference "ii[pref:suff]"))
      (signals exceptions::JTM-error
	(jtm::get-identifier-type-from-jtm-reference ""))
      (is (string= (jtm::compute-uri-from-jtm-identifier "http://any.uri" nil)
		   "http://any.uri"))
      (is (string=
	   (jtm::compute-uri-from-jtm-identifier "http://any.uri" prefixes)
	   "http://any.uri"))
      (is (string=
	   (jtm::compute-uri-from-jtm-identifier "pref_1:any.uri" prefixes)
	   "pref_1:any.uri"))
      (is (string=
	   (jtm::compute-uri-from-jtm-identifier "[pref_1:any.uri]" prefixes)
	   "http://pref.org/any.uri"))
      (signals exceptions::JTM-error 
	       (jtm::compute-uri-from-jtm-identifier "[pref_5:any.uri]" prefixes))
      (signals exceptions::JTM-error 
	(jtm::compute-uri-from-jtm-identifier "" prefixes))
      (signals exceptions::JTM-error 
	(jtm::compute-uri-from-jtm-identifier "[]" prefixes))
      (signals exceptions::JTM-error 
	(jtm::compute-uri-from-jtm-identifier "[any.uri]" prefixes))
      (signals exceptions::JTM-error 
	(jtm::compute-uri-from-jtm-identifier "[pref:]" prefixes))
      (signals exceptions::JTM-error 
	(jtm::compute-uri-from-jtm-identifier "[:suffix]" prefixes))
      (is (string=
	   (jtm::compute-uri-from-jtm-identifier "[http://any.uri" prefixes)
	   "[http://any.uri"))
      (is (string=
	   (jtm::compute-uri-from-jtm-identifier "http://any.uri]" prefixes)
	   "http://any.uri]")))))


(test test-import-jtm-references-2
  "Tests all functions that are responsible for processing and searching
   constructs by jtm-references."
  (with-fixture with-empty-db ("data_base")
    (let ((prefixes (list (list :pref "pref_1" :value "http://pref.org/")
			  (list :pref "pref_3" :value "http://pref.org/app/")
			  (list :pref "pref_2" :value "http://pref.org/app#")))
	  (top-1 (make-construct 'TopicC :start-revision 100
				 :psis
				 (list (make-construct
					'PersistentIdC
					:uri "http://pref.org/app#psi-1")
				       (make-construct
					'PersistentIdC
					:uri "http://pref.org/app/psi-1"))
				 :item-identifiers
				 (list (make-construct
					'ItemIdentifierC
					:uri "http://pref.org/iis/ii-1"))
				 :locators
				 (list (make-construct
					'SubjectLocatorC
					:uri "http://some.where/app/sl-1"))))
	  (assoc-1 (make-construct 'AssociationC :start-revision 100
				   :item-identifiers
				   (list (make-construct
					  'ItemIdentifierC
					  :uri "http://pref.org/app#ii-2")))))
      (is (eql (jtm::get-item-from-jtm-reference
		"si:http://pref.org/app#psi-1" :revision 0 :prefixes prefixes)
	       top-1))
      (is (eql (jtm::get-item-from-jtm-reference
		"si:[pref_2:psi-1]" :revision 0 :prefixes prefixes)
	       top-1))
      (is (eql (jtm::get-item-from-jtm-reference
		"si:[pref_3:psi-1]" :revision 0 :prefixes prefixes)
	       top-1))
      (is (eql (jtm::get-item-from-jtm-reference
		"si:[pref_1:app#psi-1]" :revision 0 :prefixes prefixes)
	       top-1))
      (signals exceptions::missing-reference-error
	(jtm::get-item-from-jtm-reference
	 "sl:http://pref.org/app/sl-1" :revision 0))
      (is (eql (jtm::get-item-from-jtm-reference
		"sl:http://some.where/app/sl-1" :revision 0)
	       top-1))
      (is (eql (jtm::get-item-from-jtm-reference
		"ii:http://pref.org/iis/ii-1" :revision 0 :prefixes prefixes)
	       top-1))
      (is (eql (jtm::get-item-from-jtm-reference
		"ii:[pref_1:iis/ii-1]" :revision 0 :prefixes prefixes)
	       top-1))
      (signals exceptions::jtm-error
	(jtm::get-item-from-jtm-reference
	 "ii:[pref_1:iis/ii-1]" :revision 0))
      (signals exceptions::missing-reference-error
	(jtm::get-item-from-jtm-reference
	 "si:[pref_1:iis/ii-1]" :revision 0 :prefixes prefixes))
      (is (eql (jtm::get-item-from-jtm-reference
		"ii:http://pref.org/app#ii-2" :revision 0 :prefixes prefixes)
	       assoc-1))
      (is (eql (jtm::get-item-from-jtm-reference
		"ii:[pref_2:ii-2]" :revision 0 :prefixes prefixes)
	       assoc-1))
      (let ((refs (jtm::get-items-from-jtm-references
		   (list "si:http://pref.org/app#psi-1"
			 "si:[pref_2:psi-1]"
			 "sl:http://some.where/app/sl-1"
			 "ii:http://pref.org/iis/ii-1"
			 "ii:http://pref.org/app#ii-2"
			 "ii:[pref_2:ii-2]")
		   :revision 0 :prefixes prefixes)))
	(dotimes (idx 3)
	  (is (eql (elt refs idx) top-1)))
	(dotimes (idx 2)
	  (is (eql (elt refs (+ idx 4)) assoc-1)))))))
      

(test test-get-item
  "Tests the function get-item."
  (let* ((jtm-variant "{\"version\":\"1.1\",\"prefixes\":{\"xsd\":\"http:\\/\\/www.w3.org\\/2001\\/XMLSchema#\",\"pref_1\":\"http:\\/\\/some.where\\/\"},\"item_identifiers\":[\"http://some.where/ii-1\",\"[pref_1:ii-2]\"],\"datatype\":\"http:\\/\\/www.w3.org\\/2001\\/XMLSchema#string\",\"value\":\"var-1\",\"item_type\":\"variant\",\"parent\":[\"ii:[pref_1:ii-1]\"],\"scope\":[\"si:[pref_1:psi-1]\"],\"reifier\":null}")
	 (jtm-lst (json:decode-json-from-string jtm-variant)))
    (is (string= (jtm::get-item :VERSION jtm-lst) "1.1"))
    (is-false (set-exclusive-or (jtm::get-item :ITEM--IDENTIFIERS jtm-lst)
				(list "http://some.where/ii-1"
				      "[pref_1:ii-2]") :test #'string=))
    (is (eql (first (first (jtm::get-item :PREFIXES jtm-lst))) :XSD))
    (is (string= (rest (first (jtm::get-item :PREFIXES jtm-lst)))
		 "http://www.w3.org/2001/XMLSchema#"))
    (is (eql (first (second (jtm::get-item :PREFIXES jtm-lst))) :PREF--1))
    (is (string= (rest (second (jtm::get-item :PREFIXES jtm-lst)))
		 "http://some.where/"))))


(test test-import-identifiers
  "Tests the functions import-identifier-from-jtm-string and
   import-identifiers-from-jtm-strings."
  (with-fixture with-empty-db ("data_base")
    (let* ((prefixes (list (list :pref "pref_1" :value "http://pref.org/")
			   (list :pref "pref_2" :value "http://pref.org#")
			   (list :pref "pref_3" :value "http://pref.org/app/")))
	   (j-ii-1 "http://pref.org/ii-1")
	   (j-ii-2 "[pref_1:ii-2]")
	   (j-sl-1 "[pref_2:sl-1]")
	   (j-sl-2 "[pref_3:app_2/sl-2]")
	   (j-psi-1 "[pref_3:psi-1]")
	   (j-psi-2 "http://pref.org/psi-2")
	   (ii-1 (jtm::import-identifier-from-jtm-string j-ii-1 :prefixes prefixes))
	   (sl-1 (jtm::import-identifier-from-jtm-string
		  j-sl-1 :prefixes prefixes :identifier-type-symbol 'SubjectLocatorC))
	   (psi-1 (jtm::import-identifier-from-jtm-string
		   j-psi-1 :prefixes prefixes :identifier-type-symbol 'PersistentIdC))
	   (psi-2 (jtm::import-identifier-from-jtm-string
		   j-psi-2 :prefixes prefixes :identifier-type-symbol 'PersistentIdC))
	   (psis (jtm::import-identifiers-from-jtm-strings
		  (list j-psi-1 j-psi-2) :prefixes prefixes
		  :identifier-type-symbol 'PersistentIdC))
	   (iis (jtm::import-identifiers-from-jtm-strings (list j-ii-1 j-ii-2)
							  :prefixes prefixes))
	   (ii-2 (elephant:get-instance-by-value
		 'd:ItemIdentifierC 'd:uri "http://pref.org/ii-2"))
	   (sls (jtm::import-identifiers-from-jtm-strings
		 (list j-sl-1 j-sl-2) :prefixes prefixes
		 :identifier-type-symbol 'SubjectLocatorC))
	   (sl-2 (elephant:get-instance-by-value
		  'd:SubjectLocatorC 'd:uri "http://pref.org/app/app_2/sl-2")))
      (signals exceptions:JTM-error
	(jtm::import-identifier-from-jtm-string j-ii-2))
      (signals exceptions:duplicate-identifier-error
	(jtm::import-identifier-from-jtm-string
	 j-ii-1 :identifier-type-symbol 'PersistentIdC))
      (signals exceptions:JTM-error
	(jtm::import-identifiers-from-jtm-strings (list j-ii-2)))
      (signals exceptions:duplicate-identifier-error
	(jtm::import-identifiers-from-jtm-strings
	 (list j-ii-1) :identifier-type-symbol 'PersistentIdC))
      (is (eql (elephant:get-instance-by-value 'd:ItemIdentifierC 'd:uri j-ii-1)
	       ii-1))
      (is (find ii-2 iis))
      (is (eql (elephant:get-instance-by-value
		'd:SubjectLocatorC 'd:uri "http://pref.org#sl-1")
	       sl-1))
      (is (find sl-2 sls))
      (is (eql (elephant:get-instance-by-value
		'd:PersistentIdC 'd:uri "http://pref.org/app/psi-1")
	       psi-1))
      (is (eql (elephant:get-instance-by-value 'd:PersistentIdC 'd:uri j-psi-2)
	       psi-2))
      (is-false (set-exclusive-or psis (list psi-1 psi-2)))
      (is-false (set-exclusive-or iis (list ii-1 ii-2)))
      (is-false (set-exclusive-or sls (list sl-1 sl-2))))))


(test test-import-variants
  "Tests the functions import-variant-from-jtm-string and
   import-constructs-from-jtm-strings."
  (with-fixture with-empty-db ("data_base")
    (let* ((prefixes (list (list :pref "xsd" :value *xsd-ns*)
			   (list :pref "pref_1" :value "http://some.where/")))
	   (jtm-var-1 (concat "{\"version\":\"1.1\",\"prefixes\":{\"xsd\":\"http:\\/\\/www.w3.org\\/2001\\/XMLSchema#\",\"pref_1\":\"http:\\/\\/some.where\\/\"},\"item_identifiers\":null,\"datatype\":" (json:encode-json-to-string *xml-string*) ",\"value\":\"var-1\",\"item_type\":\"variant\",\"parent\":[\"ii:[pref_1:ii-1]\"],\"scope\":[\"si:[pref_1:psi-1]\"],\"reifier\":null}"))
	   (jtm-var-2 (concat "{\"version\":\"1.0\",\"item_identifiers\":[\"http:\\/\\/some.where\\/ii-3\"],\"datatype\":" (json:encode-json-to-string *xml-uri*) ",\"value\":\"http:\\/\\/any.uri\",\"item_type\":\"variant\",\"scope\":[\"sl:http:\\/\\/some.where\\/sl-1\"],\"reifier\":\"ii:http:\\/\\/some.where\\/ii-2\"}"))
	   (jtm-var-3 (concat "{\"version\":\"1.1\",\"prefixes\":{\"xsd\":\"http:\\/\\/www.w3.org\\/2001\\/XMLSchema#\",\"pref_1\":\"http:\\/\\/some.where\\/\"},\"item_identifiers\":null,\"datatype\":" (json:encode-json-to-string *xml-string*) ",\"value\":\"var-1\",\"item_type\":\"variant\",\"parent\":[\"ii:[pref_1:ii-10]\"],\"scope\":[\"si:[pref_1:psi-1]\"],\"reifier\":null}"))
	   (name-1 (make-construct
		    'NameC :start-revision 100
		    :item-identifiers
		    (list (make-construct 'ItemIdentifierC
					  :uri "http://some.where/ii-1"))))
	   (scope-1 (make-construct
		     'TopicC :start-revision 100
		     :psis
		     (list (make-construct 'PersistentIdC
					   :uri "http://some.where/psi-1"))))
	   (var-1 (jtm::import-variant-from-jtm-list
		   (json:decode-json-from-string jtm-var-1) nil :revision 100
		   :prefixes prefixes))
	   (scope-2 (make-construct
		     'TopicC :start-revision 100
		     :locators
		     (list (make-construct 'SubjectLocatorC
					   :uri "http://some.where/sl-1"))))
	   (reifier-2 (make-construct
		       'TopicC :start-revision 100
		       :item-identifiers
		       (list (make-construct 'ItemIdentifierC
					     :uri "http://some.where/ii-2"))))
	   (var-2 (jtm::import-variant-from-jtm-list
		   (json:decode-json-from-string jtm-var-2) name-1 :revision 100
		   :prefixes prefixes))
	   (vars (jtm::import-constructs-from-jtm-lists
		  (list (json:decode-json-from-string jtm-var-1)
			(json:decode-json-from-string jtm-var-2)) name-1
			#'jtm::import-variant-from-jtm-list :revision 100
			:prefixes prefixes)))
      (is-true (d:find-item-by-revision var-1 100 name-1))
      (is-false (d:find-item-by-revision var-1 50 name-1))
      (is (eql (parent var-1 :revision 0) name-1))
      (is (eql (parent var-2 :revision 0) name-1))
      (is (string= (datatype var-1) *xml-string*))
      (is (string= (datatype var-2) *xml-uri*))
      (is (string= (charvalue var-1) "var-1"))
      (is (string= (charvalue var-2) "http://any.uri"))
      (is-false (d:item-identifiers var-1 :revision 0))
      (is-false (set-exclusive-or
		 (map 'list #'d:uri (d:item-identifiers var-2 :revision 0))
		 (list "http://some.where/ii-3") :test #'string=))
      (is-false (reifier var-1 :revision 0))
      (is (eql (reifier var-2 :revision 0) reifier-2))
      (is-false (set-exclusive-or (themes var-1 :revision 0) (list scope-1)))
      (is-false (set-exclusive-or (themes var-2 :revision 0) (list scope-2)))
      (is-false (set-exclusive-or vars (list var-1 var-2)))
      (signals exceptions:missing-reference-error
	(jtm::import-variant-from-jtm-list
	 (json:decode-json-from-string jtm-var-3) nil :revision 100
	 :prefixes prefixes))
      (signals exceptions:JTM-error
	(jtm::import-variant-from-jtm-list
	 (json:decode-json-from-string jtm-var-1) name-1 :revision 100))
      (signals exceptions:JTM-error
	(jtm::import-variant-from-jtm-list
	 (json:decode-json-from-string jtm-var-2) nil :revision 100))
      (signals exceptions:missing-reference-error
	(jtm::import-constructs-from-jtm-lists
	 (list (json:decode-json-from-string jtm-var-3)) nil
	 #'jtm::import-variant-from-jtm-list :revision 100
	 :prefixes prefixes))
      (signals exceptions:JTM-error
	(jtm::import-constructs-from-jtm-lists
	 (list (json:decode-json-from-string jtm-var-1)) name-1
	 #'jtm::import-variant-from-jtm-list :revision 100))
      (signals exceptions:JTM-error
	(jtm::import-constructs-from-jtm-lists
	 (list (json:decode-json-from-string jtm-var-2)) nil
	 #'jtm::import-variant-from-jtm-list :revision 100)))))


(test test-import-occurrences
  "Tests the functions import-occurrence-from-jtm-string and
   import-constructs-from-jtm-strings."
  (with-fixture with-empty-db ("data_base")
    (let* ((prefixes (list (list :pref "xsd" :value *xsd-ns*)
			   (list :pref "pref_1" :value "http://some.where/")))
	   (jtm-occ-1 (concat "{\"version\":\"1.1\",\"prefixes\":{\"xsd\":\"http:\\/\\/www.w3.org\\/2001\\/XMLSchema#\",\"pref_1\":\"http:\\/\\/some.where\\/\"},\"item_identifiers\":[\"[pref_1:ii-2]\"],\"datatype\":" (json:encode-json-to-string *xml-string* ) ",\"type\":\"sl:[pref_1:sl-1]\",\"value\":\"occ-1\",\"item_type\":\"occurrence\",\"parent\":[\"si:[pref_1:psi-1]\"],\"scope\":[\"si:[pref_1:psi-1]\"],\"reifier\":\"ii:[pref_1:ii-1]\"}"))
	   (jtm-occ-2 (concat "{\"version\":\"1.0\",\"item_identifiers\":null,\"datatype\":" (json:encode-json-to-string *xml-uri* ) ",\"type\":\"si:http:\\/\\/some.where\\/psi-1\",\"value\":\"http:\\/\\/any.uri\",\"item_type\":\"occurrence\",\"scope\":null,\"reifier\":null}"))
	   (jtm-occ-3 (concat "{\"version\":\"1.1\",\"prefixes\":{\"xsd\":\"http:\\/\\/www.w3.org\\/2001\\/XMLSchema#\",\"pref_1\":\"http:\\/\\/some.where\\/\"},\"item_identifiers\":[\"[pref_1:ii-2]\"],\"datatype\":" (json:encode-json-to-string *xml-string* ) ",\"type\":\"sl:[pref_1:sl-1]\",\"value\":\"occ-1\",\"item_type\":\"occurrence\",\"parent\":[\"si:[pref_1:psi-6]\"],\"scope\":[\"si:[pref_1:psi-1]\"],\"reifier\":\"ii:[pref_1:ii-1]\"}"))
	   (jtm-occ-4 (concat "{\"version\":\"1.0\",\"item_identifiers\":null,\"datatype\":" (json:encode-json-to-string *xml-uri* ) ",\"type\":null,\"value\":\"http:\\/\\/any.uri\",\"item_type\":\"occurrence\",\"scope\":null,\"reifier\":null}"))
	   (jtm-occ-5 (concat "{\"version\":\"1.0\",\"item_identifiers\":null,\"datatype\":" (json:encode-json-to-string *xml-uri* ) ",\"type\":\"si:http://any-uri/psi-10\",\"value\":\"http:\\/\\/any.uri\",\"item_type\":\"occurrence\",\"scope\":null,\"reifier\":null}"))
	   (type-1 (make-construct
		    'TopicC :start-revision 0
		    :locators
		    (list (make-construct 'SubjectLocatorC
					  :uri "http://some.where/sl-1"))))
	   (scope-1 (make-construct
		    'TopicC :start-revision 0
		    :psis
		    (list (make-construct 'PersistentIdC
					  :uri "http://some.where/psi-1"))))
	   (reifier-1 (make-construct
		       'TopicC :start-revision 0
		       :item-identifiers
		       (list (make-construct 'ItemIdentifierC
					     :uri "http://some.where/ii-1"))))
	   (parent-1 scope-1)
	   (type-2 scope-1)
	   (occ-1 (jtm::import-occurrence-from-jtm-list
		   (json:decode-json-from-string jtm-occ-1) nil :revision 100
		   :prefixes prefixes))
	   (occ-2 (jtm::import-occurrence-from-jtm-list
		   (json:decode-json-from-string jtm-occ-2) parent-1 :revision 100
		   :prefixes prefixes))
	   (occs (jtm::import-constructs-from-jtm-lists
		  (list (json:decode-json-from-string jtm-occ-1)
			(json:decode-json-from-string jtm-occ-2)) parent-1
			#'jtm::import-occurrence-from-jtm-list :revision 100
			:prefixes prefixes)))
      (is-true (d:find-item-by-revision occ-1 100 parent-1))
      (is-false (d:find-item-by-revision occ-1 50 parent-1))
      (is (eql (parent occ-1 :revision 0) parent-1))
      (is (eql (parent occ-2 :revision 0) parent-1))
      (is (string= (datatype occ-1) *xml-string*))
      (is (string= (datatype occ-2) *xml-uri*))
      (is (string= (charvalue occ-1) "occ-1"))
      (is (string= (charvalue occ-2) "http://any.uri"))
      (is-false (set-exclusive-or
		 (map 'list #'d:uri (d:item-identifiers occ-1 :revision 0))
		 (list "http://some.where/ii-2") :test #'string=))
      (is-false (d:item-identifiers occ-2 :revision 0))
      (is (eql (reifier occ-1 :revision 0) reifier-1))
      (is-false (reifier occ-2 :revision 0))
      (is-false (set-exclusive-or (themes occ-1 :revision 0) (list scope-1)))
      (is-false (themes occ-2 :revision 0))
      (is (eql (instance-of occ-1 :revision 0) type-1))
      (is (eql (instance-of occ-2 :revision 0) type-2))
      (is-false (set-exclusive-or (list occ-1 occ-2) occs))
      (signals exceptions:missing-reference-error
	(jtm::import-occurrence-from-jtm-list
	 (json:decode-json-from-string jtm-occ-5) parent-1 :revision 100
	 :prefixes prefixes))
      (signals exceptions:JTM-error
	(jtm::import-occurrence-from-jtm-list
	 (json:decode-json-from-string jtm-occ-4) parent-1 :revision 100
	 :prefixes prefixes))
      (signals exceptions:missing-reference-error
	(jtm::import-occurrence-from-jtm-list
	 (json:decode-json-from-string jtm-occ-3) nil :revision 100
	 :prefixes prefixes))
      (signals exceptions:JTM-error
	(jtm::import-occurrence-from-jtm-list
	 (json:decode-json-from-string jtm-occ-1) parent-1 :revision 100))
      (signals exceptions:JTM-error
	(jtm::import-occurrence-from-jtm-list
	 (json:decode-json-from-string jtm-occ-2) nil :revision 100))
      (signals exceptions:missing-reference-error
	(jtm::import-constructs-from-jtm-lists
	 (list (json:decode-json-from-string jtm-occ-3)) nil
	 #'jtm::import-occurrence-from-jtm-list :revision 100
	 :prefixes prefixes))
      (signals exceptions:JTM-error
	(jtm::import-constructs-from-jtm-lists
	 (list (json:decode-json-from-string jtm-occ-1)) parent-1
	 #'jtm::import-occurrence-from-jtm-list :revision 100))
      (signals exceptions:JTM-error
	(jtm::import-constructs-from-jtm-lists
	 (list (json:decode-json-from-string jtm-occ-2)) nil
	 #'jtm::import-occurrence-from-jtm-list :revision 100)))))


(test test-import-names
  "Tests the functions import-name-from-jtm-list and
   import-constructs-from-jtm-lists."
  (with-fixture with-empty-db ("data_base")
    (let* ((prefixes (list (list :pref "xsd" :value *xsd-ns*)
			   (list :pref "pref_1" :value *xsd-ns*)
			   (list :pref "pref_2" :value "http://some.where/")))
	   (jtm-name-1 (concat "{\"version\":\"1.1\",\"prefixes\":{\"pref_1\":\"http:\\/\\/www.w3.org\\/2001\\/XMLSchema#\",\"xsd\":\"http:\\/\\/www.w3.org\\/2001\\/XMLSchema#\",\"pref_2\":\"http:\\/\\/some.where\\/\"},\"item_identifiers\":[\"[pref_2:ii-2]\"],\"value\":\"name-1\",\"type\":\"sl:[pref_2:sl-1]\",\"item_type\":\"name\",\"parent\":[\"si:[pref_2:psi-1]\"],\"scope\":[\"si:[pref_2:psi-1]\"],\"variants\":[{\"item_identifiers\":null,\"datatype\":\"http:\\/\\/www.w3.org\\/2001\\/XMLSchema#string\",\"value\":\"var-1\",\"scope\":[\"sl:[pref_2:sl-1]\"],\"reifier\":null},{\"item_identifiers\":null,\"datatype\":\"http:\\/\\/www.w3.org\\/2001\\/XMLSchema#string\",\"value\":\"var-2\",\"scope\":[\"sl:[pref_2:sl-1]\"],\"reifier\":null}],\"reifier\":\"ii:[pref_2:ii-1]\"}"))
	   (jtm-name-2 "{\"version\":\"1.0\",\"item_identifiers\":null,\"value\":\"name-2\",\"type\":null,\"item_type\":\"name\",\"scope\":null,\"variants\":null,\"reifier\":null}")
	   (jtm-name-3 "{\"version\":\"1.0\",\"item_identifiers\":null,\"value\":\"name-2\",\"type\":null,\"item_type\":\"name\",\"parent\":[\"si:[pref_2:psi-10]\"],\"scope\":null,\"variants\":null,\"reifier\":null}")
	   (type-1 (make-construct
		    'TopicC :start-revision 100
		    :locators
		    (list (make-construct 'SubjectLocatorC
					  :uri "http://some.where/sl-1"))))
	   (parent-1 (make-construct
		      'TopicC :start-revision 100
		      :psis
		      (list (make-construct 'PersistentIdC
					    :uri "http://some.where/psi-1"))))
	   (scope-1 parent-1)
	   (reifier-1 (make-construct
		      'TopicC :start-revision 100
		      :item-identifiers
		      (list (make-construct 'ItemIdentifierC
					    :uri "http://some.where/ii-1"))))
	   (name-1 (jtm::import-name-from-jtm-list
		    (json:decode-json-from-string jtm-name-1) nil :revision 100
		    :prefixes prefixes))
	   (name-2 (jtm::import-name-from-jtm-list
		    (json:decode-json-from-string jtm-name-2) parent-1 :revision 100
		    :prefixes prefixes))
	   (names (jtm::import-constructs-from-jtm-lists
		   (list (json:decode-json-from-string jtm-name-1)
			 (json:decode-json-from-string jtm-name-2)) parent-1
			 #'jtm::import-name-from-jtm-list :revision 100
			 :prefixes prefixes)))
      (is-true (d:find-item-by-revision name-1 100 parent-1))
      (is-false (d:find-item-by-revision name-1 50 parent-1))
      (is (eql (parent name-1 :revision 0) parent-1))
      (is (eql (parent name-2 :revision 0) parent-1))
      (is (string= (charvalue name-1) "name-1"))
      (is (string= (charvalue name-2) "name-2"))
      (is-false (set-exclusive-or
		 (map 'list #'d:uri (d:item-identifiers name-1 :revision 0))
		 (list "http://some.where/ii-2") :test #'string=))
      (is-false (d:item-identifiers name-2 :revision 0))
      (is (eql (reifier name-1 :revision 0) reifier-1))
      (is-false (reifier name-2 :revision 0))
      (is-false (set-exclusive-or (themes name-1 :revision 0) (list scope-1)))
      (is-false (themes name-2 :revision 0))
      (is (eql (instance-of name-1 :revision 0) type-1))
      (is-false (instance-of name-2 :revision 0))
      (is-false (set-exclusive-or
		 (map 'list #'d:charvalue (variants name-1 :revision 0))
		 (list "var-1" "var-2") :test #'string=))
      (is-false (variants name-2 :revision 0))
      (is-false (set-exclusive-or names (list name-1 name-2)))
      (signals exceptions:missing-reference-error
	(jtm::import-name-from-jtm-list
	 (json:decode-json-from-string jtm-name-3) nil :revision 100
	 :prefixes prefixes))
      (signals exceptions:JTM-error
	(jtm::import-name-from-jtm-list
	 (json:decode-json-from-string jtm-name-1) parent-1 :revision 100))
      (signals exceptions:JTM-error
	(jtm::import-name-from-jtm-list
	 (json:decode-json-from-string jtm-name-2) nil :revision 100))
      (signals exceptions:missing-reference-error
	(jtm::import-constructs-from-jtm-lists
	 (list (json:decode-json-from-string jtm-name-3)) nil
	 #'jtm::import-name-from-jtm-list :revision 100
	 :prefixes prefixes))
      (signals exceptions:JTM-error
	(jtm::import-constructs-from-jtm-lists
	 (list (json:decode-json-from-string jtm-name-1)) parent-1
	 #'jtm::import-name-from-jtm-list :revision 100))
      (signals exceptions:JTM-error
	(jtm::import-constructs-from-jtm-lists
	 (list (json:decode-json-from-string jtm-name-2)) nil
	 #'jtm::import-name-from-jtm-list :revision 100)))))



(test test-make-instance-of-association
  "Tests the function make-instance-of-association."
  (with-fixture with-empty-db ("data_base")
    (let* ((tt (make-construct 'TopicC :start-revision 100
			       :psis
			       (list (make-construct 'PersistentIdC
						     :uri *type-psi*))))
	   (it (make-construct 'TopicC :start-revision 100
			       :psis
			       (list (make-construct 'PersistentIdC
						     :uri *instance-psi*))))
	   (tit (make-construct 'TopicC :start-revision 100
				:psis
				(list (make-construct 'PersistentIdC
						     :uri *type-instance-psi*))))
	   (top-1 (make-construct
		   'TopicC :start-revision 100
		   :psis
		   (list (make-construct 'PersistentIdC
					 :uri "http://some.where/psi-1"))))
	   (top-2 (make-construct
		   'TopicC :start-revision 100
		   :locators
		   (list (make-construct 'SubjectLocatorC
					 :uri "http://some.where/sl-1"))))
	   (top-3 (make-construct
		   'TopicC :start-revision 100
		   :item-identifiers
		   (list (make-construct 'ItemIdentifierC
					 :uri "http://some.where/ii-1"))))
	   (tm (make-construct
		'TopicMapC :start-revision 100
		:item-identifiers
		(list (make-construct 'ItemIdentifierC
				      :uri "http://some.where/tm-ii")))))
      (jtm::make-instance-of-association top-1 top-2 (list tm) :revision 100)
      (is (= (length (player-in-roles top-1 :revision 0)) 1))
      (is (eql (instance-of (first (player-in-roles top-1 :revision 0)) :revision 0)
	       it))
      (let ((assoc (parent (first (player-in-roles top-1 :revision 0)) :revision 0)))
	(is-true assoc)
	(is (= (length (roles assoc :revision 0)) 2))
	(is (eql (instance-of assoc :revision 0) tit))
	(is-true (find tm (in-topicmaps assoc :revision 0)))
	(is-true (find-if #'(lambda(role)
			      (and (eql (instance-of role :revision 0) tt)
				   (eql (player role :revision 0) top-2)))
			  (roles assoc :revision 0))))
      (is (= (length (player-in-roles top-2 :revision 0)) 1))
      (is-true (find tm (in-topicmaps tt :revision 0)))
      (is-false (find tm (in-topicmaps tt :revision 50)))
      (is-true (find tm (in-topicmaps it :revision 0)))
      (is-true (find tm (in-topicmaps tit :revision 0)))
      (jtm::make-instance-of-association top-2 top-3 (list tm) :revision 100)
      (is (= (length (player-in-roles top-2 :revision 0)) 2))
      (is (= (length (player-in-roles top-3 :revision 0)) 1))
      (is (eql (instance-of (first (player-in-roles top-3 :revision 0)) :revision 0)
	       tt))
      (let ((assoc (parent (first (player-in-roles top-3 :revision 0)) :revision 0)))
	(is-true assoc)
	(is (= (length (roles assoc :revision 0)) 2))
	(is (eql (instance-of assoc :revision 0) tit))
	(is-true (find tm (in-topicmaps assoc :revision 0)))
	(is-true (find-if #'(lambda(role)
			      (and (eql (instance-of role :revision 0) it)
				   (eql (player role :revision 0) top-2)))
			  (roles assoc :revision 0))))
      (signals exceptions:JTM-error
	(jtm::make-instance-of-association top-1 top-3 nil :revision 100))
      (delete-psi
       tt (elephant:get-instance-by-value 'PersistentIdc 'd:uri *type-psi*)
       :revision 200)
      (signals exceptions:missing-reference-error
	(jtm::make-instance-of-association top-1 top-3 (list tm) :revision 200)))))


(test test-import-topics
  "Tests the functions import-topic-stub-from-jtm-list,
   and import-topic-stubs-from-jtm-lists."
  (with-fixture with-empty-db ("data_base")
    (let* ((prefixes (list (list :pref "xsd" :value *xsd-ns*)
			   (list :pref "pref_1" :value *xsd-ns*)
			   (list :pref "pref_2" :value "http://some.where/")))
	   (j-top-1 "{\"version\":\"1.1\",\"prefixes\":{\"pref_1\":\"http:\\/\\/www.w3.org\\/2001\\/XMLSchema#\",\"xsd\":\"http:\\/\\/www.w3.org\\/2001\\/XMLSchema#\",\"pref_2\":\"http:\\/\\/some.where\\/\"},\"subject_identifiers\":[\"[pref_2:psi-1]\",\"[pref_2:psi-2]\"],\"subject_locators\":[\"[pref_2:sl-2]\"],\"item_identifiers\":[\"[pref_2:ii-4]\"],\"instance_of\":null,\"item_type\":\"topic\",\"names\":[{\"item_identifiers\":null,\"value\":\"name-1\",\"type\":null,\"scope\":null,\"variants\":null,\"reifier\":null},{\"item_identifiers\":null,\"value\":\"name-2\",\"type\":null,\"scope\":[\"sl:[pref_2:sl-1]\"],\"variants\":[{\"item_identifiers\":null,\"datatype\":\"http:\\/\\/www.w3.org\\/2001\\/XMLSchema#string\",\"value\":\"var-1\",\"scope\":[\"ii:[pref_2:ii-1]\"],\"reifier\":null}],\"reifier\":null}],\"occurrences\":[{\"item_identifiers\":[\"[pref_2:ii-2]\"],\"datatype\":\"http:\\/\\/www.w3.org\\/2001\\/XMLSchema#string\",\"type\":\"sl:[pref_2:sl-1]\",\"value\":\"occ-1\",\"scope\":[\"si:[pref_2:psi-1]\"],\"reifier\":\"ii:[pref_2:ii-1]\"},{\"item_identifiers\":null,\"datatype\":\"http:\\/\\/www.w3.org\\/2001\\/XMLSchema#anyURI\",\"type\":\"si:[pref_2:psi-1]\",\"value\":\"http:\\/\\/any.uri\",\"scope\":null,\"reifier\":null}]}")
	   (j-top-2 "{\"version\":\"1.0\",\"subject_identifiers\":[\"http:\\/\\/some.where\\/psi-1\",\"http:\\/\\/some.where\\/psi-2\"],\"subject_locators\":[\"http:\\/\\/some.where\\/sl-2\"],\"item_identifiers\":[\"http:\\/\\/some.where\\/ii-4\"],\"item_type\":\"topic\",\"parent\":[\"ii:http:\\/\\/some.where\\/ii-3\"],\"names\":[{\"item_identifiers\":null,\"value\":\"name-1\",\"type\":null,\"scope\":null,\"variants\":null,\"reifier\":null},{\"item_identifiers\":null,\"value\":\"name-2\",\"type\":null,\"scope\":[\"sl:http:\\/\\/some.where\\/sl-1\"],\"variants\":[{\"item_identifiers\":null,\"datatype\":\"http:\\/\\/www.w3.org\\/2001\\/XMLSchema#string\",\"value\":\"var-1\",\"scope\":[\"ii:http:\\/\\/some.where\\/ii-1\"],\"reifier\":null}],\"reifier\":null}],\"occurrences\":[{\"item_identifiers\":[\"http:\\/\\/some.where\\/ii-2\"],\"datatype\":\"http:\\/\\/www.w3.org\\/2001\\/XMLSchema#string\",\"type\":\"sl:http:\\/\\/some.where\\/sl-1\",\"value\":\"occ-1\",\"scope\":[\"si:http:\\/\\/some.where\\/psi-1\"],\"reifier\":\"ii:http:\\/\\/some.where\\/ii-1\"},{\"item_identifiers\":null,\"datatype\":\"http:\\/\\/www.w3.org\\/2001\\/XMLSchema#anyURI\",\"type\":\"si:http:\\/\\/some.where\\/psi-1\",\"value\":\"http:\\/\\/any.uri\",\"scope\":null,\"reifier\":null}]}")
	   (j-top-3 "{\"subject_identifiers\":[\"http:\\/\\/some.where\\/tmsparql\\/author\"],\"subject_locators\":null,\"item_identifiers\":null,\"names\":null,\"occurrences\":null}")
	   (j-top-4 "{\"subject_identifiers\":[\"http:\\/\\/some.where\\/tmsparql\\/first-name\"],\"subject_locators\":null,\"item_identifiers\":null,\"names\":null,\"occurrences\":null}")
	   (j-top-5 "{\"subject_identifiers\":null,\"subject_locators\":null,\"item_identifiers\":[\"http:\\/\\/some.where\\/ii\\/goethe-name-reifier\"],\"names\":null,\"occurrences\":null}")
	   (tm-1 (make-construct
		  'TopicMapC :start-revision 100
		  :item-identifiers
		  (list (make-construct 'ItemIdentifierC
					:uri "http://some.where/tm-1"))))
	   (tm-2 (make-construct
		  'TopicMapC :start-revision 100
		  :item-identifiers
		  (list (make-construct 'ItemIdentifierC
					:uri "http://some.where/tm-2")))))
      (is-false (elephant:get-instances-by-class 'd:TopicC))
      (is (= (length (elephant:get-instances-by-class 'ItemIdentifierC)) 2))
      (let ((top-1 (jtm::import-topic-stub-from-jtm-list
		    (json:decode-json-from-string j-top-1)
		    (list tm-1 tm-2) :revision 100 :prefixes prefixes)))
	(is (= (length (elephant:get-instances-by-class 'TopicC)) 1))
	(is-false (elephant:get-instances-by-class 'NameC))
	(is-false (elephant:get-instances-by-class 'VariantC))
	(is-false (elephant:get-instances-by-class 'RoleC))
	(is-false (elephant:get-instances-by-class 'AssociationC))
	(is-false (elephant:get-instances-by-class 'OccurrenceC))
	(is (= (length (elephant:get-instances-by-class 'PersistentIdC)) 2))
	(is (= (length (elephant:get-instances-by-class 'ItemIdentifierC)) 3))
	(is (= (length (elephant:get-instances-by-class 'SubjectLocatorC)) 1))
	(is-false (set-exclusive-or (list "http://some.where/psi-1"
					  "http://some.where/psi-2")
				    (map 'list #'d:uri (psis top-1 :revision 0))
				    :test #'string=))
	(is-false (set-exclusive-or
		   (list "http://some.where/sl-2")
		   (map 'list #'d:uri (locators top-1 :revision 0))
		   :test #'string=))
	(is-false (set-exclusive-or
		   (list "http://some.where/ii-4")
		   (map 'list #'d:uri (item-identifiers top-1 :revision 0))
		   :test #'string=))
	(is-true (find tm-1 (in-topicmaps top-1 :revision 0)))
	(is-true (find tm-2 (in-topicmaps top-1 :revision 0))))
      (let ((top-2 (jtm::import-topic-stub-from-jtm-list
		    (json:decode-json-from-string j-top-2)
		    (list tm-1 tm-2) :revision 200)))
	(is (= (length (elephant:get-instances-by-class 'TopicC)) 1))
      	(is-false (elephant:get-instances-by-class 'NameC))
	(is-false (elephant:get-instances-by-class 'VariantC))
	(is-false (elephant:get-instances-by-class 'RoleC))
	(is-false (elephant:get-instances-by-class 'AssociationC))
	(is-false (elephant:get-instances-by-class 'OccurrenceC))
	(is (= (length (elephant:get-instances-by-class 'PersistentIdC)) 2))
	(is (= (length (elephant:get-instances-by-class 'ItemIdentifierC)) 3))
	(is (= (length (elephant:get-instances-by-class 'SubjectLocatorC)) 1))
	(is-false (set-exclusive-or (list "http://some.where/psi-1"
					  "http://some.where/psi-2")
				    (map 'list #'d:uri (psis top-2 :revision 200))
				    :test #'string=))
	(is-false (set-exclusive-or
		   (list "http://some.where/sl-2")
		   (map 'list #'d:uri (locators top-2 :revision 200))
		   :test #'string=))
	(is-false (set-exclusive-or
		   (list "http://some.where/ii-4")
		   (map 'list #'d:uri (item-identifiers top-2 :revision 200))
		   :test #'string=))
	(is-true (find tm-1 (in-topicmaps top-2 :revision 200)))
	(is-true (find tm-2 (in-topicmaps top-2 :revision 200))))
      (let ((tops-3-4-5
	     (jtm::import-topic-stubs-from-jtm-lists
	      (list (json:decode-json-from-string j-top-3)
		    (json:decode-json-from-string j-top-4)
		    (json:decode-json-from-string j-top-5))
	      (list tm-1 tm-2) :revision 200)))
	(is (= (length tops-3-4-5) 3))
	(is (= (length (elephant:get-instances-by-class 'TopicC)) 4))
	(is (= (length (elephant:get-instances-by-class 'PersistentIdC)) 4))
	(is (= (length (elephant:get-instances-by-class 'ItemIdentifierC)) 4))
	(is (= (length (elephant:get-instances-by-class 'SubjectLocatorC)) 1))
	(is-false (elephant:get-instances-by-class 'NameC))
	(is-false (elephant:get-instances-by-class 'VariantC))
	(is-false (elephant:get-instances-by-class 'RoleC))
	(is-false (elephant:get-instances-by-class 'AssociationC))
	(is-false (elephant:get-instances-by-class 'OccurrenceC))
	(is-true (find-if #'(lambda(top)
			      (and (= (length (psis top :revision 0)) 1)
				   (not (item-identifiers top :revision 0))
				   (not (locators top :revision 0))
				   (string= (uri (first (psis top :revision 0)))
					    "http://some.where/tmsparql/author")))
			  tops-3-4-5))
	(is-true
	 (find-if #'(lambda(top)
		      (and (= (length (psis top :revision 0)) 1)
			   (not (item-identifiers top :revision 0))
			   (not (locators top :revision 0))
			   (string= (uri (first (psis top :revision 0)))
				    "http://some.where/tmsparql/first-name")))
		  tops-3-4-5))
	(is-true
	 (find-if #'(lambda(top)
		      (and (= (length (item-identifiers top :revision 0)) 1)
			   (not (psis top :revision 0))
			   (not (locators top :revision 0))
			   (string= (uri (first (item-identifiers top :revision 0)))
				    "http://some.where/ii/goethe-name-reifier")))
		  tops-3-4-5))
	(signals exceptions:jtm-error
	  (jtm::import-topic-stub-from-jtm-list
	   (json:decode-json-from-string j-top-1)
	   (list tm-1 tm-2) :revision 200))
	(signals exceptions:missing-reference-error
	  (jtm::import-topic-stub-from-jtm-list
	   (json:decode-json-from-string j-top-2)
	   nil :revision 200))
	(signals exceptions:jtm-error
	  (jtm::import-topic-stubs-from-jtm-lists
	   (list (json:decode-json-from-string j-top-1))
	   (list tm-1 tm-2) :revision 200))
	(signals exceptions:missing-reference-error
	  (jtm::import-topic-stubs-from-jtm-lists
	   (list (json:decode-json-from-string j-top-2))
	   nil :revision 200))))))


;TODO:
; *merge-topics-from-jtm-lists
; *merge-topic-from-jtm-list


(defun run-jtm-tests()
  "Runs all tests of this test-suite."
  (it.bese.fiveam:run! 'jtm-tests))