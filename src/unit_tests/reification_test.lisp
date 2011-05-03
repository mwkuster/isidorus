;;+-----------------------------------------------------------------------------
;;+  Isidorus
;;+  (c) 2008-2010 Marc Kuester, Christoph Ludwig, Lukas Georgieff
;;+
;;+  Isidorus is freely distributable under the LLGPL license.
;;+  You can find a detailed description in trunk/docs/LLGPL-LICENSE.txt and
;;+  trunk/docs/LGPL-LICENSE.txt.
;;+-----------------------------------------------------------------------------

(defpackage :reification-test
  (:use 
   :common-lisp
   :datamodel
   :it.bese.FiveAM
   :unittests-constants
   :fixtures
   :base-tools
   :xtm-exporter)
  (:import-from :constants
                *xtm2.0-ns*
		*xtm1.0-ns*
		*xtm1.0-xlink*
		*rdf-ns*
		*rdfs-ns*
		*type-psi*
		*instance-psi*
		*type-instance-psi*
		*rdf2tm-subject*
		*rdf2tm-object*)
  (:import-from :xml-tools
                xpath-child-elems-by-qname xpath-single-child-elem-by-qname
		xpath-fn-string)
  (:export
   :reification-test
   :run-reification-tests
   :test-merge-reifier-topics
   :test-xtm1.0-reification
   :test-xtm2.0-reification
   :test-xtm1.0-reification-exporter
   :test-xtm2.0-reification-exporter
   :test-rdf-importer-reification
   :test-rdf-importer-reification-2
   :test-rdf-importer-reification-3
   :test-rdf-importer-reification-4
   :test-rdf-exporter-reification
   :test-rdf-exporter-reification-2
   :test-rdf-exporter-reification-3
   :test-rdf-exporter-reification-4
   :test-fragment-reification))


(in-package :reification-test)


(def-suite reification-test
     :description "tests various functions of the reification functions")

(in-suite reification-test)


(test test-merge-reifier-topics
  "Tests the function merge-constructs."
  (let ((db-dir "data_base")
	(revision-1 100)
	(revision-2 200))
    (clean-out-db db-dir)
    (elephant:open-store (xtm-importer:get-store-spec db-dir))
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
					:parent topic-1
					:themes (list scope-1)
					:instance-of name-type
					:charvalue "name-1-1"
					:start-revision revision-1))
	      (name-2-1 (make-construct 'NameC
					:item-identifiers (list (make-instance 'ItemIdentifierC
									       :uri "name-2-1-ii-1"
									       :start-revision revision-1))
					:parent topic-2
					:themes (list scope-2)
					:instance-of nil
					:charvalue "name-2-1"
					:start-revision revision-2))
	      (occurrence-2-1 (make-construct 'OccurrenceC
					      :item-identifiers (list (make-instance 'ItemIdentifierC
										     :uri "occurrence-1-1-ii-1"
										     :start-revision revision-1))
					      :parent topic-2
					      :themes (list scope-1 scope-2)
					      :instance-of occurrence-type
					      :charvalue "occurrence-2-1"
					      :datatype "datatype"
					      :start-revision revision-2))
	      (occurrence-2-2 (make-construct 'OccurrenceC
					      :item-identifiers nil
					      :parent topic-2
					      :themes nil
					      :instance-of occurrence-type
					      :charvalue "occurrence-2-2"
					      :datatype "datatype"
					      :start-revision revision-2))
	      (test-name (make-construct 'NameC
					 :item-identifiers nil
					 :parent scope-2
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
					    :start-revision revision-2
					    :item-identifiers
					    (list (make-instance 'ItemIdentifierC
								 :uri "role-1"
								 :start-revision revision-2)))
				      (list :instance-of role-type
					    :player topic-2
					    :start-revision revision-2
					    :item-identifiers
					    (list (make-instance 'ItemIdentifierC
								 :uri "role-2"
								 :start-revision revision-2))))
				     :start-revision revision-2)))
	  (is (= (length (elephant:get-instances-by-class 'TopicC)) 8))
	  (d::merge-constructs topic-1 topic-2 :revision revision-2)
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
	  (is (= (length (union (occurrences topic-1 :revision 0)
				(list occurrence-2-1 occurrence-2-2)))
		 (length (list occurrence-2-1 occurrence-2-2))))
	  (is (= (length (union (d:used-as-type topic-1)
				(list test-name)))
		 (length (list test-name))))
	  (is (= (length (union (d:used-as-theme topic-1)
				(list test-name)))
		 (length (list test-name))))
	  (is (= (length (roles assoc :revision 0)) 1))
	  (is (= (length (d::slot-p assoc 'd::roles)) 2))
	  (is (eql (player (first (roles assoc :revision 0)) :revision 0) topic-1))
	  (elephant:close-store))))))


(test test-xtm1.0-reification
  "Tests the reification in the xtm1.0-importer."
  (let
      ((dir "data_base"))
    (with-fixture initialize-destination-db (dir)
      (xtm-importer:import-from-xtm
       *reification_xtm1.0.xtm* dir
       :tm-id "http://www.isidor.us/unittests/reification-xtm1.0-tests"
       :xtm-id "reification-xtm"
       :xtm-format :1.0)
      (setf *TM-REVISION* 0)
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
      (is (eql (reified-construct reifier-occurrence) homer-occurrence))
      (is (eql (reifier homer-name) reifier-name))
      (is (eql (reified-construct reifier-name) homer-name))
      (is (eql (reifier homer-variant) reifier-variant))
      (is (eql (reified-construct reifier-variant) homer-variant))
      (is (eql (reifier married-assoc) reifier-married-assoc))
      (is (eql (reified-construct reifier-married-assoc) married-assoc))
      (is (eql (reifier husband-role) reifier-husband-role))
      (is (eql (reified-construct reifier-husband-role) husband-role))
      (is-true (handler-case 
		   (progn (d::delete-construct homer-occurrence)
			  t)
		 (condition () nil)))
      (is-false (occurrences homer))
      (is (= (length (elephant:get-instances-by-class 'd:TopicC)) 12))
      (elephant:close-store))))))


(test test-xtm2.0-reification
  "Tests the reification in the xtm2.0-importer."
  (let
      ((dir "data_base"))
    (with-fixture initialize-destination-db (dir)
      (xtm-importer:import-from-xtm
       *reification_xtm2.0.xtm* dir
       :tm-id "http://www.isidor.us/unittests/reification-xtm2.0-tests"
       :xtm-id "reification-xtm")
      (is (= (length (elephant:get-instances-by-class 'TopicC)) 12))
      (is (= (length (elephant:get-instances-by-class 'AssociationC)) 1))
      (setf *TM-REVISION* 0)
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
      (is (eql (reified-construct reifier-occurrence) homer-occurrence))
      (is (eql (reifier homer-name) reifier-name))
      (is (eql (reified-construct reifier-name) homer-name))
      (is (eql (reifier homer-variant) reifier-variant))
      (is (eql (reified-construct reifier-variant) homer-variant))
      (is (eql (reifier married-assoc) reifier-married-assoc))
      (is (eql (reified-construct reifier-married-assoc) married-assoc))
      (is (eql (reifier husband-role) reifier-husband-role))
      (is (eql (reified-construct reifier-husband-role) husband-role))
      (is-true (handler-case 
		   (progn (d::delete-construct homer-occurrence)
			  t)
		 (condition () nil)))
      (is-false (occurrences homer))
      (is (= (length (elephant:get-instances-by-class 'd:TopicC)) 12))
      (elephant:close-store))))))


(test test-xtm1.0-reification-exporter
  "Tests the reification in the xtm1.0-exporter."
  (let
      ((dir "data_base")
       (output-file "__out__.xtm")
       (tm-id "http://www.isidor.us/unittests/reification-xtm1.0-tests"))
    (with-fixture initialize-destination-db (dir)
      (handler-case (delete-file output-file)
	(error () )) ;do nothing
      (setf *TM-REVISION* 0)
      (xtm-importer:import-from-xtm
       *reification_xtm1.0.xtm* dir
       :tm-id tm-id
       :xtm-id "reification-xtm"
       :xtm-format :1.0)
      (export-as-xtm output-file :xtm-format :1.0 :tm-id tm-id)
      (let ((document
	     (dom:document-element
	      (cxml:parse-file output-file (cxml-dom:make-dom-builder)))))
	(let ((homer-topic
	       (loop for topic across (xpath-child-elems-by-qname document *xtm1.0-ns* "topic")
		  when (loop for subjectIndicatorRef across (xpath-child-elems-by-qname
							     (xpath-single-child-elem-by-qname
							      topic *xtm1.0-ns* "subjectIdentity")
							     *xtm1.0-ns* "subjectIndicatorRef")
			    when (string= (dom:get-attribute-ns subjectIndicatorRef *xtm1.0-xlink* "href")
					  "http://simpsons.tv/homer")
			    return t)
		  return topic))
	      (married-assoc (xpath-single-child-elem-by-qname document *xtm1.0-ns* "association")))
	  (is-true homer-topic)
	  (is-true married-assoc)
	  (loop for occurrence across (xpath-child-elems-by-qname homer-topic *xtm1.0-ns* "occurrence")
	     do (is (string= (dom:get-attribute occurrence "id") "homer-occurrence")))
	  (loop for name across (xpath-child-elems-by-qname homer-topic *xtm1.0-ns* "baseName")
	     do (progn (is (string= (dom:get-attribute name "id") "homer-name"))
		       (loop  for variant across (xpath-child-elems-by-qname name *xtm1.0-ns* "variant")
			  do (is (string= (dom:get-attribute variant "id") "homer-name-variant")))))
	  (is (string= (dom:get-attribute married-assoc "id") "a-married"))
	  (is-true (loop for role across (xpath-child-elems-by-qname married-assoc *xtm1.0-ns* "member")
		      when (string= (dom:get-attribute role "id")
				    "married-husband-role")
		      return t)))
	(is-true (loop for topic across (xpath-child-elems-by-qname document *xtm1.0-ns* "topic")
		  when (loop for subjectIndicatorRef across (xpath-child-elems-by-qname
							     (xpath-single-child-elem-by-qname
							      topic *xtm1.0-ns* "subjectIdentity")
							     *xtm1.0-ns* "subjectIndicatorRef")
			    when (string= (dom:get-attribute-ns subjectIndicatorRef *xtm1.0-xlink* "href")
					  "#homer-occurrence")
			  return t)
		    return t))
	(is-true (loop for topic across (xpath-child-elems-by-qname document *xtm1.0-ns* "topic")
		    when (loop for subjectIndicatorRef across (xpath-child-elems-by-qname
							       (xpath-single-child-elem-by-qname
								topic *xtm1.0-ns* "subjectIdentity")
							       *xtm1.0-ns* "subjectIndicatorRef")
			    when (string= (dom:get-attribute-ns subjectIndicatorRef *xtm1.0-xlink* "href")
					  "#homer-name")
			    return t)
		    return t))
	(is-true (loop for topic across (xpath-child-elems-by-qname document *xtm1.0-ns* "topic")
		    when (loop for subjectIndicatorRef across (xpath-child-elems-by-qname
							       (xpath-single-child-elem-by-qname
								topic *xtm1.0-ns* "subjectIdentity")
							       *xtm1.0-ns* "subjectIndicatorRef")
			    when (string= (dom:get-attribute-ns subjectIndicatorRef *xtm1.0-xlink* "href")
					  "#homer-name-variant")
			    return t)
		    return t))
	(is-true (loop for topic across (xpath-child-elems-by-qname document *xtm1.0-ns* "topic")
		    when (loop for subjectIndicatorRef across (xpath-child-elems-by-qname
							       (xpath-single-child-elem-by-qname
								topic *xtm1.0-ns* "subjectIdentity")
							       *xtm1.0-ns* "subjectIndicatorRef")
			    when (string= (dom:get-attribute-ns subjectIndicatorRef *xtm1.0-xlink* "href")
					  "#a-married")
			    return t)
		    return t))
	(is-true (loop for topic across (xpath-child-elems-by-qname document *xtm1.0-ns* "topic")
		    when (loop for subjectIndicatorRef across (xpath-child-elems-by-qname
							       (xpath-single-child-elem-by-qname
								topic *xtm1.0-ns* "subjectIdentity")
							       *xtm1.0-ns* "subjectIndicatorRef")
			    when (string= (dom:get-attribute-ns subjectIndicatorRef *xtm1.0-xlink* "href")
					  "#married-husband-role")
			    return t)
		    return t)))
      (handler-case (delete-file output-file)
	(error () )) ;do nothing
      (elephant:close-store))))


(test test-xtm2.0-reification-exporter
  "Tests the reification in the xtm2.0-exporter."
  (let
      ((dir "data_base")
       (output-file "__out__.xtm")
       (tm-id "http://www.isidor.us/unittests/reification-xtm2.0-tests"))
    (with-fixture initialize-destination-db (dir)
      (handler-case (delete-file output-file)
	(error () )) ;do nothing
      (setf *TM-REVISION* 0)
      (xtm-importer:import-from-xtm
       *reification_xtm2.0.xtm* dir
       :tm-id tm-id
       :xtm-id "reification-xtm")
      (export-as-xtm output-file :tm-id tm-id)
      (let ((document
	     (dom:document-element
	      (cxml:parse-file output-file (cxml-dom:make-dom-builder)))))
	(let ((homer-topic
	       (loop for topic across (xpath-child-elems-by-qname document *xtm2.0-ns* "topic")
		  when (loop for psi across (xpath-child-elems-by-qname topic *xtm2.0-ns* "subjectIdentifier")
			  when (string= (dom:get-attribute psi "href") "http://simpsons.tv/homer")
			  return t)
		  return topic))
	      (married-assoc (xpath-single-child-elem-by-qname document *xtm2.0-ns* "association")))
	  (is-true homer-topic)
	  (is-true married-assoc)
	  (loop for occurrence across (xpath-child-elems-by-qname homer-topic *xtm2.0-ns* "occurrence")
	     do (is (string= (dom:get-attribute occurrence "reifier") "http://simpsons.tv/homer-occurrence")))
	  (loop for name across (xpath-child-elems-by-qname homer-topic *xtm2.0-ns* "name")
	     do (is (string= (dom:get-attribute name "reifier") "http://simpsons.tv/homer-name")))
	  (loop for name across (xpath-child-elems-by-qname homer-topic *xtm2.0-ns* "name")
	     do (loop for variant across (xpath-child-elems-by-qname name *xtm2.0-ns* "variant")
		   do (is (string= (dom:get-attribute variant "reifier") "http://simpsons.tv/homer-name-variant"))))
	  (is (string= (dom:get-attribute married-assoc "reifier") "http://simpsons.tv/married-association"))
	  (is-true (loop for role across (xpath-child-elems-by-qname married-assoc *xtm2.0-ns* "role")
		      when (string= (dom:get-attribute role "reifier") "http://simpsons.tv/married-husband-role")
		      return t))
	  (is-true (loop for topic across (xpath-child-elems-by-qname document *xtm2.0-ns* "topic")
		      when (loop for ii across (xpath-child-elems-by-qname topic *xtm2.0-ns* "itemIdentity")
			      when (string= (dom:get-attribute ii "href") "http://simpsons.tv/homer-occurrence")
			      return t)
		      return t))
	  (is-true (loop for topic across (xpath-child-elems-by-qname document *xtm2.0-ns* "topic")
		      when (loop for ii across (xpath-child-elems-by-qname topic *xtm2.0-ns* "itemIdentity")
			      when (string= (dom:get-attribute ii "href") "http://simpsons.tv/homer-name")
			      return t)
		      return t))
	  (is-true (loop for topic across (xpath-child-elems-by-qname document *xtm2.0-ns* "topic")
		      when (loop for ii across (xpath-child-elems-by-qname topic *xtm2.0-ns* "itemIdentity")
			      when (string= (dom:get-attribute ii "href") "http://simpsons.tv/homer-name-variant")
			      return t)
		      return t))
	  (is-true (loop for topic across (xpath-child-elems-by-qname document *xtm2.0-ns* "topic")
		      when (loop for ii across (xpath-child-elems-by-qname topic *xtm2.0-ns* "itemIdentity")
			      when (string= (dom:get-attribute ii "href") "http://simpsons.tv/married-association")
			      return t)
		      return t))
	  (is-true (loop for topic across (xpath-child-elems-by-qname document *xtm2.0-ns* "topic")
		      when (loop for ii across (xpath-child-elems-by-qname topic *xtm2.0-ns* "itemIdentity")
			      when (string= (dom:get-attribute ii "href") "http://simpsons.tv/married-husband-role")
			      return t)
		      return t)))))
    (handler-case (delete-file output-file)
      (error () )) ;do nothing
    (elephant:close-store)))


(test test-rdf-importer-reification
  "Tests the function import-node non-recursively. Especially the reification
   of association- and occurrence-arcs."
  (let ((db-dir "data_base")
	(tm-id "http://test-tm/")
	(revision-1 100)
	(document-id "doc-id")
	(doc-1
	 (concat "<rdf:RDF xmlns:rdf=\"" *rdf-ns* "\" "
		 "xmlns:arcs=\"http://test/arcs/\" "
		 "xmlns:rdfs=\"" *rdfs-ns* "\">"
		 "<rdf:Description rdf:about=\"first-node\">"
		 "<arcs:arc1 rdf:ID=\"reification-1\">"
		 "<rdf:Description rdf:about=\"second-node\" />"
		 "</arcs:arc1>"
		 "</rdf:Description>"
		 "<rdf:Description rdf:ID=\"#reification-1\">"
		 "<arcs:arc2 rdf:resource=\"third-node\"/>"
		 "</rdf:Description>"
		 "<rdf:Description rdf:nodeID=\"fourth-node\">"
		 "<arcs:arc3 rdf:ID=\"reification-2\" rdf:datatype=\"dt\">"
		 "occurrence data"
		 "</arcs:arc3>"
		 "</rdf:Description>"
		 "<rdf:Description rdf:ID=\"#reification-2\">"
		 "<arcs:arc4 rdf:resource=\"fifth-node\" />"
		 "</rdf:Description>"
		 "</rdf:RDF>")))
    (setf *TM-REVISION* 0)
    (clean-out-db db-dir)
    (let ((dom-1 (cxml:parse doc-1 (cxml-dom:make-dom-builder))))
      (is-true dom-1)
      (is (= (length (dom:child-nodes dom-1)) 1))
      (let ((rdf-node (elt (dom:child-nodes dom-1) 0)))
	(is (= (length (dom:child-nodes rdf-node)) 4))
	(rdf-init-db :db-dir db-dir :start-revision revision-1)
	(dotimes (iter (length (dom:child-nodes rdf-node)))
	  (rdf-importer::import-node (elt (dom:child-nodes rdf-node) iter)
				     tm-id revision-1
				     :document-id document-id))
	(is (= (length (dom:child-nodes rdf-node)) 4))
	(rdf-init-db :db-dir db-dir :start-revision revision-1)
	(dotimes (iter (length (dom:child-nodes rdf-node)))
	  (rdf-importer::import-node (elt (dom:child-nodes rdf-node) iter)
				     tm-id revision-1
				     :document-id document-id))
	(let ((reification-1 (d:get-item-by-id "http://test-tm#reification-1"
					     :xtm-id document-id))
	      (reification-2 (d:get-item-by-id "http://test-tm#reification-2"
					       :xtm-id document-id))
	      (first-node (d:get-item-by-id "http://test-tm/first-node"
					  :xtm-id document-id))
	      (second-node (d:get-item-by-id "http://test-tm/second-node"
					   :xtm-id document-id))
	      (third-node (d:get-item-by-id "http://test-tm/third-node"
					  :xtm-id document-id))
	      (fourth-node (d:get-item-by-id "fourth-node"
					     :xtm-id document-id))
	      (fifth-node (d:get-item-by-id "http://test-tm/fifth-node"
					    :xtm-id document-id))
	      (arc1 (d:get-item-by-id "http://test/arcs/arc1"
				    :xtm-id document-id))
	      (arc2 (d:get-item-by-id "http://test/arcs/arc2"
				    :xtm-id document-id))
	      (arc3 (d:get-item-by-id "http://test/arcs/arc3"
				      :xtm-id document-id))
	      (arc4 (d:get-item-by-id "http://test/arcs/arc4"
				      :xtm-id document-id)))
	  (is (= (length (d:psis reification-1)) 1))
	  (is (string= (d:uri (first (d:psis reification-1)))
		       "http://test-tm#reification-1"))
	  (is (= (length (d:psis reification-2)) 1))
	  (is (string= (d:uri (first (d:psis reification-2)))
		       "http://test-tm#reification-2"))
	  (is (= (length (d:psis first-node)) 1))
	  (is (string= (d:uri (first (d:psis first-node)))
		       "http://test-tm/first-node"))
	  (is (= (length (d:psis second-node)) 1))
	  (is (string= (d:uri (first (d:psis second-node)))
		       "http://test-tm/second-node"))
	  (is (= (length (d:psis third-node)) 1))
	  (is (string= (d:uri (first (d:psis third-node)))
		       "http://test-tm/third-node"))
	  (is (= (length (d:psis fourth-node)) 0))
	  (is (= (length (d:psis fifth-node)) 1))
	  (is (string= (d:uri (first (d:psis fifth-node)))
		       "http://test-tm/fifth-node"))
	  (is (= (length (d:psis arc1)) 1))
	  (is (string= (d:uri (first (d:psis arc1)))
		       "http://test/arcs/arc1"))
	  (is (= (length (d:psis arc2))))
	  (is (string= (d:uri (first (d:psis arc2)))
		       "http://test/arcs/arc2"))
	  (is (= (length (d:psis arc3))))
	  (is (string= (d:uri (first (d:psis arc3)))
		       "http://test/arcs/arc3"))
	  (is (= (length (d:psis arc4))))
	  (is (string= (d:uri (first (d:psis arc4)))
		       "http://test/arcs/arc4"))
	  (is (= (length (d:used-as-type arc1)) 1))
	  (is (eql (reifier (first (d:used-as-type arc1))) reification-1))
	  (is (eql (reified-construct reification-1) (first (d:used-as-type arc1))))
	  (is (eql (reifier (first (d:used-as-type arc3))) reification-2))
	  (is (eql (reified-construct reification-2) (first (d:used-as-type arc3))))))))
  (elephant:close-store))


(test test-rdf-importer-reification-2
  "Tests the rdf-importer, especially some reification cases of
   the tm2rdf mapping."
  (let ((db-dir "data_base")
	(tm-id "http://test-tm/")
	(revision-1 100)
	(document-id "doc-id"))
    (setf *TM-REVISION* 0)
    (clean-out-db db-dir)
    (rdf-importer:import-from-rdf
     *reification.rdf* db-dir :tm-id tm-id
     :document-id document-id :start-revision revision-1)
    (elephant:open-store (xtm-importer:get-store-spec db-dir))
    (let ((homer (get-item-by-id "http://simpsons.tv/homer" :xtm-id document-id))
	  (bart (get-item-by-id "http://simpsons.tv/bart" :xtm-id document-id))
	  (married (get-item-by-id "http://simpsons.tv/arcs/married" :xtm-id document-id)))
      (is-true homer)
      (is-true bart)
      (is-true married)
      (is (= (length (used-as-type married)) 1))
      (is-true (reifier (first (used-as-type married))))
      (is-true (reified-construct (reifier (first (used-as-type married)))))
      (is (= (length (psis (reifier (first (used-as-type married))))) 1))
      (is (string= (uri (first (psis (reifier (first (used-as-type married))))))
		   "http://test-tm#married-arc"))
      (is (= (length (occurrences bart)) 1))
      (is-true (reifier (first (occurrences bart))))
      (is-true (reified-construct (reifier (first (occurrences bart)))))
      (is (string= (uri (first (psis (reifier (first (occurrences bart))))))
		   "http://test-tm#lastName-arc"))))
  (elephant:close-store))


(test test-rdf-importer-reification-3
  "Tests the rdf-importer, especially some reification cases of
   the tm2rdf mapping."
  (let ((db-dir "data_base")
	(tm-id "http://test-tm/")
	(revision-1 100)
	(document-id "doc-id"))
    (setf *TM-REVISION* 0)
    (clean-out-db db-dir)
    (rdf-importer:import-from-rdf
     *reification.rdf* db-dir :tm-id tm-id
     :document-id document-id :start-revision revision-1)
    (elephant:open-store (xtm-importer:get-store-spec db-dir))
    (let ((lisa (get-item-by-id "http://simpsons.tv/lisa" :xtm-id document-id)))
      (is-true lisa)
      (is (= (length (names lisa)) 1))
      (is (= (length (occurrences lisa)) 1))
      (let ((name (first (names lisa)))
	    (occurrence (first (occurrences lisa))))
	(is (= (length (variants name)) 1))
	(let ((variant (first (variants name))))
	  (is-true (reifier name))
	  (is-true (reified-construct (reifier name)))
	  (is (= (length (psis (reifier name))) 1))
	  (is (string= (uri (first (psis (reifier name))))
		       (concat tm-id "lisa-name")))
	  (is-true (reifier variant))
	  (is-true (reified-construct (reifier variant)))
	  (is (= (length (psis (reifier variant))) 1))
	  (is (string= (uri (first (psis (reifier variant))))
		       (concat tm-id "lisa-name-variant")))
	  (is-true (reifier occurrence))
	  (is-true (reified-construct (reifier occurrence)))
	  (is (= (length (psis (reifier occurrence))) 1))
	  (is (string= (uri (first (psis (reifier occurrence))))
		       (concat tm-id "lisa-occurrence")))))))
  (elephant:close-store))


(test test-rdf-importer-reification-4
  "Tests the rdf-importer, especially some reification cases of
   the tm2rdf mapping."
  (let ((db-dir "data_base")
	(tm-id "http://test-tm/")
	(revision-1 100)
	(document-id "doc-id"))
    (setf *TM-REVISION* 0)
    (clean-out-db db-dir)
    (rdf-importer:import-from-rdf
     *reification.rdf* db-dir :tm-id tm-id
     :document-id document-id :start-revision revision-1)
    (elephant:open-store (xtm-importer:get-store-spec db-dir))
    (let ((friendship (get-item-by-id "http://simpsons.tv/friendship" :xtm-id document-id))
	  (carl (get-item-by-id "http://simpsons.tv/carl" :xtm-id document-id)))
      (is-true friendship)
      (is-true carl)
      (is (= (length (used-as-type friendship)) 1))
      (is (typep (first (used-as-type friendship)) 'd:AssociationC))
      (let ((friendship-association (first (used-as-type friendship))))
	(is-true (reifier friendship-association))
	(is-true (reified-construct (reifier friendship-association)))
	(is (= (length (psis (reifier friendship-association))) 1))
	(is (string= (uri (first (psis (reifier friendship-association))))
		     (concat tm-id "friendship-association")))
	(is (= (length (roles friendship-association)) 2))
	(let ((carl-role
	       (find-if #'(lambda(role)
			    (eql (player role) carl))
			(roles friendship-association))))
	  (is-true carl-role)
	  (is-true (reifier carl-role))
	  (is-true (reified-construct (reifier carl-role)))
	  (is (= (length (psis (reifier carl-role))) 1))
	  (is (string= (uri (first (psis (reifier carl-role))))
		       (concat tm-id "friend-role")))))))
  (elephant:close-store))


(test test-rdf-exporter-reification
  "Tests the reification in the rdf-exporter."
  (let
      ((dir "data_base")
       (output-file "__out__.rdf")
       (tm-id "http://simpsons.tv"))
    (setf *TM-REVISION* 0)
    (handler-case (delete-file output-file)
      (error () )) ;do nothing
    (clean-out-db dir)
    (rdf-importer:import-from-rdf *reification.rdf* dir
				  :tm-id tm-id
				  :document-id "reification-xtm")
    (elephant:open-store (xtm-importer:get-store-spec dir))
    (rdf-exporter:export-as-rdf output-file :tm-id tm-id)
    (let ((document
	   (dom:document-element
	    (cxml:parse-file output-file (cxml-dom:make-dom-builder)))))
      (let ((married-arc
	     (loop for reifier-node across (xpath-child-elems-by-qname document *rdf-ns* "Description")
		when (let ((about (dom:get-attribute-ns reifier-node *rdf-ns* "about")))
		       (and (stringp about) (string= about "#married-arc")))
		return reifier-node))
	    (lastName-arc
	     (loop for reifier-node across (xpath-child-elems-by-qname document *rdf-ns* "Description")
		when (let ((about (dom:get-attribute-ns reifier-node *rdf-ns* "about")))
		       (and (stringp about) (string= about "#lastName-arc")))
		return reifier-node))
	    (lisa-name
	     (loop for reifier-node across (xpath-child-elems-by-qname document *rdf-ns* "Description")
		when (let ((about (dom:get-attribute-ns reifier-node *rdf-ns* "about")))
		       (and (stringp about) (string= about "#lisa-name")))
		return reifier-node))
	    (lisa-name-variant
	     (loop for reifier-node across (xpath-child-elems-by-qname document *rdf-ns* "Description")
		when (let ((about (dom:get-attribute-ns reifier-node *rdf-ns* "about")))
		       (and (stringp about) (string= about "#lisa-name-variant")))
		return reifier-node))
	    (lisa-occurrence
	     (loop for reifier-node across (xpath-child-elems-by-qname document *rdf-ns* "Description")
		when (let ((about (dom:get-attribute-ns reifier-node *rdf-ns* "about")))
		       (and (stringp about) (string= about "#lisa-occurrence")))
		return reifier-node))
	    (friendship-association
	     (loop for reifier-node across (xpath-child-elems-by-qname document *rdf-ns* "Description")
		when (let ((about (dom:get-attribute-ns reifier-node *rdf-ns* "about")))
		       (and (stringp about) (string= about "#friendship-association")))
		return reifier-node))
	    (friend-role
	     (loop for reifier-node across (xpath-child-elems-by-qname document *rdf-ns* "Description")
		when (let ((about (dom:get-attribute-ns reifier-node *rdf-ns* "about")))
		       (and (stringp about) (string= about "#friend-role")))
		return reifier-node)))
	(is-true married-arc)
	(is-true lastName-arc)
	(is-true lisa-name)
	(is-true lisa-name-variant)
	(is-true lisa-occurrence)
	(is-true friendship-association)
	(is-true friend-role)
	(dolist (reifier-node (list married-arc lastName-arc lisa-name
				    lisa-name-variant lisa-occurrence
				    friendship-association friend-role))
	  (let ((author-arc
		 (xpath-single-child-elem-by-qname reifier-node "http://simpsons.tv/arcs/" "author")))
	    (is-true author-arc)
	    (let ((resource (dom:get-attribute-ns author-arc *rdf-ns* "resource")))
	      (is (and (stringp resource) (string= resource "http://some.where/me"))))))))
    (handler-case (delete-file output-file)
      (error () ))) ;do nothing
  (elephant:close-store))


(test test-rdf-exporter-reification-2
  "Tests the reification in the rdf-exporter."
  (let
      ((dir "data_base")
       (output-file "__out__.rdf")
       (tm-id "http://simpsons.tv"))
    (setf *TM-REVISION* 0)
    (handler-case (delete-file output-file)
      (error () )) ;do nothing
    (clean-out-db dir)
    (rdf-importer:import-from-rdf *reification.rdf* dir
				  :tm-id tm-id
				  :document-id "reification-xtm")
    (elephant:open-store (xtm-importer:get-store-spec dir))
    (rdf-exporter:export-as-rdf output-file :tm-id tm-id)
    (let ((document
	   (dom:document-element
	    (cxml:parse-file output-file (cxml-dom:make-dom-builder)))))
      (let ((lisa
	     (loop for resource across (xpath-child-elems-by-qname document *rdf-ns* "Description")
		when (let ((about (dom:get-attribute-ns resource *rdf-ns* "about")))
		       (and (stringp about) (string= about "http://simpsons.tv/lisa")))
		return resource)))
	(is-true lisa)
	(let ((lisa-name
	       (let ((arc
		      (xpath-single-child-elem-by-qname lisa "http://isidorus/tm2rdf_mapping/" "name")))
		 (when arc
		   (xpath-single-child-elem-by-qname arc *rdf-ns* "Description"))))
	      (lisa-occurrence
	       (xpath-single-child-elem-by-qname lisa "http://simpsons.tv/" "profession")))
	  (is-true lisa-name)
	  (is-true lisa-occurrence)
	  (let ((lisa-name-variant
		 (let ((arc
			(xpath-single-child-elem-by-qname lisa-name "http://isidorus/tm2rdf_mapping/" "variant")))
		   (when arc
		     (xpath-single-child-elem-by-qname arc *rdf-ns* "Description")))))
	    (is-true lisa-name-variant)
	    (let ((name-reifier
		   (let ((elem
			  (xpath-single-child-elem-by-qname
			   lisa-name "http://isidorus/tm2rdf_mapping/" "reifier")))
		     (when elem
		       (dom:get-attribute-ns elem *rdf-ns* "resource"))))
		  (variant-reifier
		   (let ((elem
			  (xpath-single-child-elem-by-qname
			   lisa-name-variant "http://isidorus/tm2rdf_mapping/" "reifier")))
		     (when elem
		       (dom:get-attribute-ns elem *rdf-ns* "resource"))))
		  (occurrence-reifier (dom:get-attribute-ns lisa-occurrence *rdf-ns* "ID")))
	      (is (and (stringp name-reifier)
		       (string= name-reifier "lisa-name")))
	      (is (and (stringp variant-reifier)
		       (string= variant-reifier "lisa-name-variant")))
	      (is (and (stringp occurrence-reifier)
		       (string= occurrence-reifier "lisa-occurrence"))))))))
    (handler-case (delete-file output-file)
      (error () ))) ;do nothing
  (elephant:close-store))


(test test-rdf-exporter-reification-3
  "Tests the reification in the rdf-exporter."
  (let
      ((dir "data_base")
       (output-file "__out__.rdf")
       (tm-id "http://simpsons.tv"))
    (setf *TM-REVISION* 0)
    (handler-case (delete-file output-file)
      (error () )) ;do nothing
    (clean-out-db dir)
    (rdf-importer:import-from-rdf *reification.rdf* dir
				  :tm-id tm-id
				  :document-id "reification-xtm")
    (elephant:open-store (xtm-importer:get-store-spec dir))
    (rdf-exporter:export-as-rdf output-file :tm-id tm-id)
    (let ((document
	   (dom:document-element
	    (cxml:parse-file output-file (cxml-dom:make-dom-builder)))))
      (let ((homer
	     (loop for resource across (xpath-child-elems-by-qname document *rdf-ns* "Description")
		when (let ((about (dom:get-attribute-ns resource *rdf-ns* "about")))
		       (and (stringp about) (string= about "http://simpsons.tv/homer")))
		return resource)))
	(is-true homer)
	(let ((married-arc
	       (xpath-single-child-elem-by-qname homer "http://simpsons.tv/arcs/" "married")))
	  (is-true married-arc)
	  (let ((reifier-id (dom:get-attribute-ns married-arc *rdf-ns* "ID")))
	    (is (and (stringp reifier-id)
		     (string= reifier-id "married-arc")))))))
    (handler-case (delete-file output-file)
      (error () ))) ;do nothing
  (elephant:close-store))


(test test-rdf-exporter-reification-4
  "Tests the reification in the rdf-exporter."
  (let
      ((dir "data_base")
       (output-file "__out__.rdf")
       (tm-id "http://simpsons.tv"))
    (setf *TM-REVISION* 0)
    (handler-case (delete-file output-file)
      (error () )) ;do nothing
    (clean-out-db dir)
    (rdf-importer:import-from-rdf *reification.rdf* dir
				  :tm-id tm-id
				  :document-id "reification-xtm")
    (elephant:open-store (xtm-importer:get-store-spec dir))
    (rdf-exporter:export-as-rdf output-file :tm-id tm-id)
    (let ((document
	   (dom:document-element
	    (cxml:parse-file output-file (cxml-dom:make-dom-builder)))))
      (let ((association
	     (loop for resource across (xpath-child-elems-by-qname document *rdf-ns* "Description")
		when (let ((type (xpath-single-child-elem-by-qname resource *rdf-ns* "type")))
		       (when type
			 (let ((type-uri
				(dom:get-attribute-ns type *rdf-ns* "resource")))
			   (and (stringp type-uri)
				(string= type-uri "http://isidorus/tm2rdf_mapping/types/Association")))))
		return resource)))
	(is-true association)
	(let ((role
	       (loop for resource across
		    (xpath-child-elems-by-qname association "http://isidorus/tm2rdf_mapping/" "role")
		  when (let ((description (xpath-single-child-elem-by-qname resource *rdf-ns* "Description")))
			 (when description
			   (xpath-single-child-elem-by-qname
			    description "http://isidorus/tm2rdf_mapping/" "reifier")))
		  return (xpath-single-child-elem-by-qname resource *rdf-ns* "Description"))))
	  (is-true role)
	  (let ((association-reifier
		 (let ((elem (xpath-single-child-elem-by-qname
			      association "http://isidorus/tm2rdf_mapping/" "reifier")))
		   (when elem
		     (dom:get-attribute-ns elem *rdf-ns* "resource"))))
		(role-reifier
		 (let ((elem (xpath-single-child-elem-by-qname
			      role "http://isidorus/tm2rdf_mapping/" "reifier")))
		   (when elem
		     (dom:get-attribute-ns elem *rdf-ns* "resource")))))
	    (is-true association-reifier)
	    (is-true role-reifier)
	    (is (and (stringp association-reifier)
		     (string= association-reifier "friendship-association")))
	    (is (and (stringp role-reifier)
		     (string= role-reifier "friend-role")))))))
    (handler-case (delete-file output-file)
      (error () ))) ;do nothing
  (elephant:close-store))


(test test-fragment-reification
  "Tests the reification in the rdf-exporter."
  (let
      ((dir "data_base")
       (output-file "__out__.rdf")
       (tm-id "http://simpsons.tv"))
    (setf *TM-REVISION* 0)
    (handler-case (delete-file output-file)
      (error () )) ;do nothing
    (clean-out-db dir)
    (rdf-importer:import-from-rdf *reification.rdf* dir
       :tm-id tm-id
       :document-id "reification-xtm")
    (elephant:open-store (xtm-importer:get-store-spec dir))
    (let ((fragment (d:create-latest-fragment-of-topic "http://simpsons.tv/lisa")))
      (is-true fragment)
      (is (= (length (union (referenced-topics fragment)
			    (list (d:get-item-by-psi "http://simpsons.tv/lastName")
				  (d:get-item-by-psi "http://simpsons.tv/sortName")
				  (d:get-item-by-psi "http://simpsons.tv/profession")
				  (d:get-item-by-psi "http://simpsons.tv/lisa-name")
				  (d:get-item-by-psi "http://simpsons.tv/lisa-name-variant")
				  (d:get-item-by-psi "http://simpsons.tv/lisa-occurrence"))))
	     6)))
    (let ((fragment (d:create-latest-fragment-of-topic "http://simpsons.tv/carl")))
      (is-true fragment)
      (is (= (length (union (referenced-topics fragment)
			    (list (d:get-item-by-psi "http://simpsons.tv/friendship")
				  (d:get-item-by-psi "http://simpsons.tv/friendship-association")
				  (d:get-item-by-psi "http://simpsons.tv/friend")
				  (d:get-item-by-psi "http://simpsons.tv/lenny")
				  (d:get-item-by-psi "http://simpsons.tv/friend-role"))))
	     5))))
      (elephant:close-store))


(defun run-reification-tests ()
  (it.bese.fiveam:run! 'test-merge-reifier-topics)
  (it.bese.fiveam:run! 'test-xtm1.0-reification)
  (it.bese.fiveam:run! 'test-xtm2.0-reification)
  (it.bese.fiveam:run! 'test-xtm1.0-reification-exporter)
  (it.bese.fiveam:run! 'test-xtm2.0-reification-exporter)
  (it.bese.fiveam:run! 'test-rdf-importer-reification)
  (it.bese.fiveam:run! 'test-rdf-importer-reification-2)
  (it.bese.fiveam:run! 'test-rdf-importer-reification-3)
  (it.bese.fiveam:run! 'test-rdf-importer-reification-4)
  (it.bese.fiveam:run! 'test-rdf-exporter-reification)
  (it.bese.fiveam:run! 'test-rdf-exporter-reification-2)
  (it.bese.fiveam:run! 'test-rdf-exporter-reification-3)
  (it.bese.fiveam:run! 'test-rdf-exporter-reification-4)
  (it.bese.fiveam:run! 'test-fragment-reification))