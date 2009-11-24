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
   :test-merge-reifier-topics
   :test-xtm1.0-reification
   :test-xtm2.0-reification))


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
	    (assoc-type (make-construct 'TopicC
					:psis (list (make-instance 'PersistentIdC
								   :uri "psi-assoc-type"
								   :start-revision revision-1))
				       :topicid "assoc-type"
				       :xtm-id xtm-id-1
				       :start-revision revision-1))
	    (role-type (make-construct 'TopicC
				       :psis (list (make-instance 'PersistentIdC
								  :uri "psi-role-type"
								  :start-revision revision-1))
				       :topicid "assoc-type"
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
					 :start-revision revision-2))
	      (assoc (make-construct 'AssociationC
				     :item-identifiers nil
				     :instance-of assoc-type
				     :themes nil
				     :roles
				     (list 
				      (list :instance-of role-type
					    :player topic-1
					    :item-identifiers
					    (list (make-instance 'ItemIdentifierC
								 :uri "role-1"
								 :start-revision revision-1)))
				      (list :instance-of role-type
					    :player topic-2
					    :item-identifiers
					    (list (make-instance 'ItemIdentifierC
								 :uri "role-2"
								 :start-revision revision-1))))
				     :start-revision revision-1)))
	  (is (= (length (elephant:get-instances-by-class 'TopicC)) 8))
	  (datamodel::merge-reifier-topics topic-1 topic-2)
	  (is (= (length (elephant:get-instances-by-class 'TopicC)) 7))
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
	  (is (eql (player (first (roles assoc))) topic-1))
	  (is (eql (player (second (roles assoc))) topic-1))
	  ;;TODO: check all objects and their version-infos
	  (elephant:close-store))))))


(test test-xtm1.0-reification
  "Tests the reification in the xtm1.0-importer."
  (let
      ((dir "data_base"))
    (with-fixture initialize-destination-db (dir)
      (xml-importer:import-xtm *reification_xtm1.0.xtm* dir
       :tm-id "http://www.isidor.us/unittests/reification-xtm1.0-tests"
       :xtm-id "reification-xtm"
       :xtm-format '1.0)
      (is (= (length (elephant:get-instances-by-class 'TopicC)) 12))
      (is (= (length (elephant:get-instances-by-class 'AssociationC)) 1))
      (let ((homer
	     (identified-construct
	      (elephant:get-instance-by-value 'PersistentIdC 'uri "http://simpsons.tv/homer")))
	    (married-assoc
	     (first (elephant:get-instances-by-class 'AssociationC))))
	(let ((homer-occurrence (first (occurrences homer)))
	      (homer-name (first (names homer)))
	      (homer-variant (first (variants (first (names homer)))))
	      (husband-role (find-if #'(lambda(x)
					 (eql (instance-of x)
					      (identified-construct
					       (elephant:get-instance-by-value
						'PersistentIdC 'uri "http://simpsons.tv/husband"))))
				     (roles married-assoc)))
	      (reifier-occurrence
	       (identified-construct (elephant:get-instance-by-value 'PersistentIdC 'uri "#homer-occurrence")))
	      (reifier-name
	       (identified-construct (elephant:get-instance-by-value 'PersistentIdC 'uri "#homer-name")))
	      (reifier-variant
	       (identified-construct (elephant:get-instance-by-value 'PersistentIdC 'uri "#homer-name-variant")))
	      (reifier-married-assoc
	       (identified-construct (elephant:get-instance-by-value 'PersistentIdC 'uri "#a-married")))
	      (reifier-husband-role
	       (identified-construct (elephant:get-instance-by-value 'PersistentIdC 'uri "#married-husband-role"))))
      (is-true homer)
      (is-true homer-occurrence)
      (is-true homer-name)
      (is-true homer-variant)
      (is-true married-assoc)
      (is-true husband-role)
      (is-true reifier-occurrence)
      (is-true reifier-name)
      (is-true reifier-variant)
      (is-true reifier-married-assoc)
      (is-true reifier-husband-role)
      (is (eql (reifier homer-occurrence) reifier-occurrence))
      (is (eql (reified reifier-occurrence) homer-occurrence))
      (is (eql (reifier homer-name) reifier-name))
      (is (eql (reified reifier-name) homer-name))
      (is (eql (reifier homer-variant) reifier-variant))
      (is (eql (reified reifier-variant) homer-variant))
      (is (eql (reifier married-assoc) reifier-married-assoc))
      (is (eql (reified reifier-married-assoc) married-assoc))
      (is (eql (reifier husband-role) reifier-husband-role))
      (is (eql (reified reifier-husband-role) husband-role))
      (is-true (handler-case 
		   (progn (d::delete-construct homer-occurrence)
			  t)
		 (condition () nil)))
      (is-false (occurrences homer))
      (is (= (length (elephant:get-instances-by-class 'd:TopicC)) 12))
      (is-true (handler-case 
		   (progn (d::delete-construct reifier-occurrence)
			  t)
		 (condition () nil)))))
      (is (= (length (elephant:get-instances-by-class 'd:TopicC)) 11))
      (elephant:close-store))))


(test test-xtm2.0-reification
  "Tests the reification in the xtm2.0-importer."
  (let
      ((dir "data_base"))
    (with-fixture initialize-destination-db (dir)
      (xml-importer:import-xtm *reification_xtm2.0.xtm* dir
       :tm-id "http://www.isidor.us/unittests/reification-xtm2.0-tests"
       :xtm-id "reification-xtm")
      (is (= (length (elephant:get-instances-by-class 'TopicC)) 12))
      (is (= (length (elephant:get-instances-by-class 'AssociationC)) 1))
      (let ((homer
	     (identified-construct
	      (elephant:get-instance-by-value 'PersistentIdC 'uri "http://simpsons.tv/homer")))
	    (married-assoc
	     (first (elephant:get-instances-by-class 'AssociationC))))
	(let ((homer-occurrence (first (occurrences homer)))
	      (homer-name (first (names homer)))
	      (homer-variant (first (variants (first (names homer)))))
	      (husband-role (find-if #'(lambda(x)
					 (eql (instance-of x)
					      (identified-construct
					       (elephant:get-instance-by-value
						'PersistentIdC 'uri "http://simpsons.tv/husband"))))
				     (roles married-assoc)))
	      (reifier-occurrence
	       (identified-construct (elephant:get-instance-by-value 'ItemIdentifierC 'uri "http://simpsons.tv/homer-occurrence")))
	      (reifier-name
	       (identified-construct (elephant:get-instance-by-value 'ItemIdentifierC 'uri "http://simpsons.tv/homer-name")))
	      (reifier-variant
	       (identified-construct (elephant:get-instance-by-value 'ItemIdentifierC 'uri "http://simpsons.tv/homer-name-variant")))
	      (reifier-married-assoc
	       (identified-construct (elephant:get-instance-by-value 'ItemIdentifierC 'uri "http://simpsons.tv/married-association")))
	      (reifier-husband-role
	       (identified-construct (elephant:get-instance-by-value 'ItemIdentifierC 'uri "http://simpsons.tv/married-husband-role"))))
      (is-true homer)
      (is-true homer-occurrence)
      (is-true homer-name)
      (is-true homer-variant)
      (is-true married-assoc)
      (is-true husband-role)
      (is-true reifier-occurrence)
      (is-true reifier-name)
      (is-true reifier-variant)
      (is-true reifier-married-assoc)
      (is-true reifier-husband-role)
      (is (eql (reifier homer-occurrence) reifier-occurrence))
      (is (eql (reified reifier-occurrence) homer-occurrence))
      (is (eql (reifier homer-name) reifier-name))
      (is (eql (reified reifier-name) homer-name))
      (is (eql (reifier homer-variant) reifier-variant))
      (is (eql (reified reifier-variant) homer-variant))
      (is (eql (reifier married-assoc) reifier-married-assoc))
      (is (eql (reified reifier-married-assoc) married-assoc))
      (is (eql (reifier husband-role) reifier-husband-role))
      (is (eql (reified reifier-husband-role) husband-role))
      (is-true (handler-case 
		   (progn (d::delete-construct homer-occurrence)
			  t)
		 (condition () nil)))
      (is-false (occurrences homer))
      (is (= (length (elephant:get-instances-by-class 'd:TopicC)) 12))
      (is-true (handler-case 
		   (progn (d::delete-construct reifier-occurrence)
			  t)
		 (condition () nil)))))
      (is (= (length (elephant:get-instances-by-class 'd:TopicC)) 11))
      (elephant:close-store))))


;;TODO: check rdf importer
;;TODO: check xtm1.0 exporter
;;TODO: check xtm2.0 exporter
;;TODO: check fragment exporter
;;TODO: check merge-reifier-topics (--> versioning)


(defun run-reification-tests ()
  (it.bese.fiveam:run! 'test-merge-reifier-topics)
  (it.bese.fiveam:run! 'test-xtm1.0-reification)
  (it.bese.fiveam:run! 'test-xtm2.0-reification)
  )