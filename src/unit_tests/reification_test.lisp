;;+-----------------------------------------------------------------------------
;;+  Isidorus
;;+  (c) 2008-2009 Marc Kuester, Christoph Ludwig, Lukas Giessmann
;;+
;;+  Isidorus is freely distributable under the LGPL license.
;;+  You can find a detailed description in trunk/docs/LGPL-LICENSE.txt.
;;+-----------------------------------------------------------------------------


(defpackage :reification-test
  (:use 
   :common-lisp
   :datamodel
   :it.bese.FiveAM
   :unittests-constants
   :fixtures)
  (:export
   :reification-test
   :run-reification-tests
   :test-merge-reifier-topics))


(in-package :reification-test)


(def-suite reification-test
     :description "tests various functions of the reification functions")

(in-suite reification-test)


(test test-merge-reifier-topics
  "Tests the function merge-reifier-topics."
  (let ((db-dir "data_base")
	(revision-1 100)
	(revision-2 200))
    (clean-out-db db-dir)
    (elephant:open-store (xml-importer:get-store-spec db-dir))
    (let ((ii-1-1 (make-instance 'ItemIdentifierC
				 :uri "ii-1-1"
				 :start-revision revision-1))
	  (ii-1-2 (make-instance 'ItemIdentifierC
				 :uri "ii-1-2"
				 :start-revision revision-1))
	  (ii-2-1 (make-instance 'ItemIdentifierC
				 :uri "ii-2-1"
				 :start-revision revision-2))
	  (ii-2-2 (make-instance 'ItemIdentifierC
				 :uri "ii-2-2"
				 :start-revision revision-2))
	  (psi-1-1 (make-instance 'PersistentIdC
				  :uri "psi-1-1"
				  :start-revision revision-1))
	  (psi-1-2 (make-instance 'PersistentIdC
				  :uri "psi-1-2"
				  :start-revision revision-1))
	  (locator-2-1 (make-instance 'SubjectLocatorC
				      :uri "locator-2-1"
				      :start-revision revision-2))
	  (xtm-id-1 "xtm-id-1")
	  (xtm-id-2 "xtm-id-2")
	  (topic-id-1 "topic-id-1")
	  (topic-id-2 "topic-id-1")) ;should no be merged, since the xtm-id differs
      (let ((topic-1 (make-construct 'TopicC
				     :item-identifiers (list ii-1-1 ii-1-2)
				     :locators nil
				     :psis (list psi-1-1 psi-1-2)
				     :topicid topic-id-1
				     :xtm-id xtm-id-1
				     :start-revision revision-1))
	    (topic-2 (make-construct 'TopicC
				     :item-identifiers (list ii-2-1 ii-2-2)
				     :locators (list locator-2-1)
				     :psis nil
				     :topicid topic-id-2
				     :xtm-id xtm-id-2
				     :start-revision revision-2))
	    (scope-1 (make-construct 'TopicC
				     :psis (list (make-instance 'PersistentIdC
								:uri "psi-scope-1"
								:start-revision revision-1))
				     :topicid "scope-1"
				     :xtm-id xtm-id-1
				     :start-revision revision-1))
	    (scope-2 (make-construct 'TopicC
				     :psis (list (make-instance 'PersistentIdC
								:uri "psi-scope-2"
								:start-revision revision-1))
				     :topicid "scope-2"
				     :xtm-id xtm-id-1
				     :start-revision revision-1))
	    (name-type (make-construct 'TopicC
				       :psis (list (make-instance 'PersistentIdC
								  :uri "psi-name-type"
								  :start-revision revision-1))
				       :topicid "name-type"
				       :xtm-id xtm-id-1
				       :start-revision revision-1))
	    (occurrence-type (make-construct 'TopicC
				       :psis (list (make-instance 'PersistentIdC
								  :uri "psi-occurrence-type"
								  :start-revision revision-1))
				       :topicid "occurrence-type"
				       :xtm-id xtm-id-1
				       :start-revision revision-1)))
	(let ((name-1-1 (make-construct 'NameC
					:item-identifiers nil
					:topic topic-1
					:themes (list scope-1)
					:instance-of name-type
					:charvalue "name-1-1"
					:start-revision revision-1))
	      (name-2-1 (make-construct 'NameC
					:item-identifiers (list (make-instance 'ItemIdentifierC
									       :uri "name-2-1-ii-1"
									       :start-revision revision-1))
					:topic topic-2
					:themes (list scope-2)
					:instance-of nil
					:charvalue "name-2-1"
					:start-revision revision-2))
	      (occurrence-2-1 (make-construct 'OccurrenceC
					      :item-identifiers (list (make-instance 'ItemIdentifierC
										     :uri "occurrence-1-1-ii-1"
										     :start-revision revision-1))
					      :topic topic-2
					      :themes (list scope-1 scope-2)
					      :instance-of occurrence-type
					      :charvalue "occurrence-2-1"
					      :datatype "datatype"
					      :start-revision revision-2))
	      (occurrence-2-2 (make-construct 'OccurrenceC
					      :item-identifiers nil
					      :topic topic-2
					      :themes nil
					      :instance-of occurrence-type
					      :charvalue "occurrence-2-2"
					      :datatype "datatype"
					      :start-revision revision-2))
	      (test-name (make-construct 'NameC
					 :item-identifiers nil
					 :topic scope-2
					 :themes (list scope-1 topic-2)
					 :instance-of topic-2
					 :charvalue "test-name"
					 :start-revision revision-2)))
	  (is (= (length (elephant:get-instances-by-class 'TopicC)) 6))
	  (datamodel::merge-reifier-topics topic-1 topic-2)
	  (is (= (length (elephant:get-instances-by-class 'TopicC)) 5))
	  (is (= (length (union (list ii-1-1 ii-1-2 ii-2-1 ii-2-2)
				(item-identifiers topic-1)))
		 (length (list ii-1-1 ii-1-2 ii-2-1 ii-2-2))))
	  (is (= (length (union (list psi-1-1 psi-1-2)
				(psis topic-1)))
		 (length (list psi-1-1 psi-1-2))))
	  (is (= (length (union (list locator-2-1)
				(locators topic-1)))
		 (length (list locator-2-1))))
	  (is (= (length (union (names topic-1)
				(list name-1-1 name-2-1)))
		 (length (list name-1-1 name-2-1))))
	  (is (= (length (union (occurrences topic-1)
				(list occurrence-2-1 occurrence-2-2)))
		 (length (list occurrence-2-1 occurrence-2-2))))
	  (is (= (length (union (d:used-as-type topic-1)
				(list test-name)))
		 (length (list test-name))))
	  (is (= (length (union (d:used-as-theme topic-1)
				(list test-name)))
		 (length (list test-name))))
	  ;;TODO: roleplayer, topicmap
	  ;;TODO: check all objects and their version-infos
	  (elephant:close-store))))))


(defun run-reification-tests ()
  (it.bese.fiveam:run! 'test-merge-reifier-topics)
  )