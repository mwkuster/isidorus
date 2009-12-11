;;+-----------------------------------------------------------------------------
;;+  Isidorus
;;+  (c) 2008-2009 Marc Kuester, Christoph Ludwig, Lukas Giessmann
;;+
;;+  Isidorus is freely distributable under the LGPL license.
;;+  You can find a detailed description in trunk/docs/LGPL-LICENSE.txt.
;;+-----------------------------------------------------------------------------


(defpackage :exporter-test
  (:use 
   :common-lisp
   :xml-importer
   :exporter
   :it.bese.FiveAM
   :xml-tools)
  (:import-from :constants
                *xtm2.0-ns*
		*xtm1.0-ns*
		*xtm1.0-xlink*)
  (:import-from :datamodel
		identified-construct
		topic
		uri
		FragmentC
		PersistentIdC)
  (:import-from :unittests-constants
		*unit-tests-path*
                *sample_objects_2_0.xtm*)
  (:import-from :xml-tools
                xpath-child-elems-by-qname xpath-single-child-elem-by-qname
		xpath-fn-string)
  (:import-from :fixtures
		clean-out-db tear-down-test-db merge-test-db)
  (:export :check-document-structure :get-subjectIdentifier-by-ref
	   :check-single-name-value :check-single-name-itemIdentity
	   :check-topic-id :check-itemIdentities :check-psis
	   :check-single-instanceOf :get-subjectIndicatorRef-by-ref
	   :check-baseNameStrings :check-occurrences-instanceOf
	   :check-occurrences-itemIdentity :check-occurrences-type
	   :check-occurrences-resourceRef :check-occurrences-resourceData
	   :check-variant
	   :test-std-topics :test-sample-topics-t1-t10
	   :test-sample-topics-t50-t59 :test-sample-topics-t60-t100
	   :test-sample-topics-t101-t301 :test-sample-associations
	   :run-exporter-tests :test-fragments
	   :test-std-topics-xtm1.0 :test-sample-topics-t1-t10-xtm1.0
	   :test-sample-topics-t50-t59-xtm1.0 :test-sample-topics-t60-t100-xtm1.0
	   :test-sample-topics-t101-t301-xtm1.0 :test-sample-associations-xtm1.0
	   :test-fragments-xtm1.0
	   :test-exporter-xtm2.0-versions-1 :test-exporter-xtm2.0-versions-2
	   :test-exporter-xtm2.0-versions-3 :test-fragments-versions
	   :test-exporter-xtm1.0-versions-1 :test-exporter-xtm1.0-versions-2
	   :test-exporter-xtm1.0-versions-3 :test-fragments-xtm1.0-versions))

(in-package :exporter-test)
(def-suite exporter-tests)
(in-suite exporter-tests)


(defvar *out-xtm2.0-file* (merge-pathnames "out_sample_objects_2_0.xtm" *unit-tests-path*))
(defvar *out-xtm1.0-file* (merge-pathnames "out_sample_objects_1_0.xtm" *unit-tests-path*))

(defun set-up-exporter-test-db ()
  "clears out the database and parses the test file"
  (clean-out-db "data_base")

  (handler-case (delete-file *out-xtm2.0-file*)
    (error () )) ;do nothing
  (handler-case (delete-file *out-xtm1.0-file*)
    (error () )) ;do nothing
  (setup-repository *sample_objects_2_0.xtm* "data_base" :xtm-id "test-tm")
  (elephant:open-store (get-store-spec "data_base")))


(def-fixture refill-test-db ()
  (set-up-exporter-test-db)
  (&body)
  (tear-down-test-db))


;; === some helpers ============================================================
(defun check-document-structure (document number-topics number-associations &key (ns-uri *xtm2.0-ns*))
  (is-true document)
  (is (= number-topics (length (xpath-child-elems-by-qname document ns-uri "topic"))))
  (is (= number-associations (length (xpath-child-elems-by-qname document ns-uri "association")))))


(defun get-subjectIdentifier-by-ref (document topic-ref)
  "returns the subjectIdentfier as a string value of the referenced topic"
  (declare (dom:element document))
  (declare (string topic-ref))
  (when (not (equal (elt topic-ref 0) #\#))
    (error "topic-ref must start with a \"#\""))
  (let ((topic-id (subseq topic-ref 1)))
    (loop for topic across (xpath-child-elems-by-qname document *xtm2.0-ns* "topic")
       when (string= topic-id (dom:node-value (dom:get-attribute-node topic "id")))
       return (dom:node-value (dom:get-attribute-node
			       (xpath-single-child-elem-by-qname topic *xtm2.0-ns* "subjectIdentifier")
			       "href")))))

(defun get-subjectIndicatorRef-by-ref (document topic-ref)
  "returns the subjectIndicatorRef as a string value of the referenced topic"
  (declare (dom:element document))
  (declare (string topic-ref))
  (when (not (equal (elt topic-ref 0) #\#))
    (error "topic-ref must start with a \"#\""))
  (let ((topic-id (subseq topic-ref 1)))
    (loop for topic across (xpath-child-elems-by-qname document *xtm1.0-ns* "topic")
       when (string= topic-id (dom:node-value (dom:get-attribute-node topic "id")))
       return (dom:get-attribute-ns
	       (xpath-single-child-elem-by-qname
		(xpath-single-child-elem-by-qname topic *xtm1.0-ns* "subjectIdentity")
		*xtm1.0-ns* "subjectIndicatorRef")
	       *xtm1.0-xlink* "href"))))


(defun check-single-name-value (topic value)
  (declare (dom:element topic))
  (declare (string value))
  (let ((name-nodes (xpath-child-elems-by-qname topic *xtm2.0-ns* "name")))
    (is (= (length name-nodes) 1))
    (is (string= value 
		 (xpath-fn-string
		  (xpath-single-child-elem-by-qname (elt name-nodes 0) *xtm2.0-ns* "value"))))))


(defun check-psis (topic values)
  "checks all psi-values of the passed topci element"
  (declare (dom:element topic))
  (declare (list values))
  (let ((psi-nodes (xpath-child-elems-by-qname topic *xtm2.0-ns* "subjectIdentifier")))
    (is (= (length psi-nodes)))
    (let ((psis (loop for psi-node across psi-nodes
		   collect (dom:get-attribute psi-node "href"))))
      (loop for value in values
	 do (is-true (find value psis :test #'string=))))))


(defun check-baseNameStrings (topic values)
  "checks all values of the passed topic's baseNameStrings"
  (declare (dom:element topic))
  (declare (list values))
  (let ((baseName-nodes (xpath-child-elems-by-qname topic *xtm1.0-ns* "baseName")))
    (let ((baseNameStrings (map 'list #'(lambda(x)
					  (let ((baseNameString-nodes
						 (xpath-child-elems-by-qname x *xtm1.0-ns* "baseNameString")))
					    (is (= (length baseNameString-nodes) 1))
					    (xpath-fn-string (elt baseNameString-nodes 0))))
				baseName-nodes)))
      (is (= (length baseNameStrings) (length values)))
      (loop for value in values
	 do (is-true (find value baseNameStrings :test #'string=))))))
  


(defun check-single-name-itemIdentity (topic value)
  (declare (dom:element topic))
  (declare (string value))
  (let ((name-nodes (xpath-child-elems-by-qname topic *xtm2.0-ns* "name")))
    (is (= (length name-nodes) 1))
    (let ((itemIdentity-nodes (xpath-child-elems-by-qname (elt name-nodes 0) *xtm2.0-ns* "itemIdentity")))
      (is (= (length itemIdentity-nodes)))
      (is (string= value (dom:node-value (dom:get-attribute-node (elt itemIdentity-nodes 0) "href")))))))


(defun check-itemIdentities (dom-elem values)
  "checks the passed itemIdentity-elements of the passed dom-element"
  (declare (dom:element dom-elem))
  (declare (list values))
  (let ((itemIdentity-nodes (xpath-child-elems-by-qname dom-elem *xtm2.0-ns* "itemIdentity")))
    (is (= (length itemIdentity-nodes) (length values)))
    (let ((itemIdentities (loop for node across itemIdentity-nodes
			     collect (dom:get-attribute node "href"))))
      (loop for value in values
	 do (is-true (find value itemIdentities :test #'string=))))))


(defun check-topic-id (topic)
  "checks the id attribute (only if it exists)"
  (declare (dom:element topic))
  (is (> (length (handler-case (dom:node-value (dom:get-attribute-node topic "id")) (error () nil))) 0)))
  

(defun check-single-instanceOf (document topic instanceOf &key (xtm-format '2.0))
  "checks the instanceOf node"
  (declare (dom:element document topic))
  (declare (string instanceOf))
  (when (eql xtm-format '2.0)
    (let ((instanceOfs (xpath-child-elems-by-qname topic *xtm2.0-ns* "instanceOf")))
      (is (= (length instanceOfs) 1))
      (let ((topicRefs (xpath-child-elems-by-qname (elt instanceOfs 0) *xtm2.0-ns* "topicRef")))
	(is (= (length topicRefs) 1))
	(let ((href (dom:node-value (dom:get-attribute-node (elt topicRefs 0) "href"))))
	  (is (string= (get-subjectIdentifier-by-ref document href) instanceOf))))))
  (when (eql xtm-format '1.0)
    (let ((instanceOfs (xpath-child-elems-by-qname topic *xtm1.0-ns* "instanceOf")))
      (is (= (length instanceOfs) 1))
      (let ((subjectIndicatorRefs (xpath-child-elems-by-qname (elt instanceOfs 0) *xtm1.0-ns* "topicRef")))
	(is (= (length subjectIndicatorRefs) 1))
	(let ((href (dom:get-attribute-ns (elt subjectIndicatorRefs 0) *xtm1.0-xlink* "href")))
	  (is (string= (get-subjectIndicatorRef-by-ref document href) instanceOf)))))))


(defun check-occurrences-itemIdentity (topic values)
  (declare (dom:element topic))
  (declare (list values))
  (let ((itemIdentities nil))
    (loop for item across (xpath-child-elems-by-qname topic *xtm2.0-ns* "occurrence")
       do (handler-case (loop for ii across (xpath-child-elems-by-qname item *xtm2.0-ns* "itemIdentity")
			   do (push (dom:node-value (dom:get-attribute-node ii "href")) itemIdentities))
	    (error () )))
    (is (= (length itemIdentities) (length values)))
    (loop for item in values
       do (is-true (find item itemIdentities :test #'string=)))))


(defun check-occurrences-type (document topic values)
  (declare (dom:element document topic))
  (declare (list values))
  (let ((types
	 (loop for item across (xpath-child-elems-by-qname topic *xtm2.0-ns* "occurrence")
	    collect (get-subjectIdentifier-by-ref document
						   (dom:node-value
						    (dom:get-attribute-node
						     (xpath-single-child-elem-by-qname 
						      (xpath-single-child-elem-by-qname item *xtm2.0-ns* "type")
						      *xtm2.0-ns* "topicRef")
						     "href"))))))
    (is (= (length types) (length values)))
    (loop for item in values
       do (is-true (find item types :test #'string=)))))


(defun check-occurrences-instanceOf (document topic values)
  (declare (dom:element document topic))
  (declare (list values))
  (let ((types
	 (loop for item across (xpath-child-elems-by-qname topic *xtm1.0-ns* "occurrence")
	    collect (get-subjectIndicatorRef-by-ref document (dom:get-attribute-ns 
							      (xpath-single-child-elem-by-qname 
							       (xpath-single-child-elem-by-qname
								item
								*xtm1.0-ns*
								"instanceOf")
							       *xtm1.0-ns*
							       "topicRef")
							      *xtm1.0-xlink*
							      "href")))))
    (is (= (length types) (length values)))
    (loop for item in values
       do (is-true (find item types :test #'string=)))))


(defun check-occurrences-resourceRef (topic values &key (xtm-format '2.0))
  (declare (dom:element topic))
  (declare (list values))
  (let ((resourceRefs nil))
    (when (eql xtm-format '1.0)
      (loop for item across (xpath-child-elems-by-qname topic *xtm1.0-ns* "occurrence")
	 when (xpath-single-child-elem-by-qname item *xtm1.0-ns* "resourceRef")
	 do (push (dom:get-attribute-ns
		   (xpath-single-child-elem-by-qname item *xtm1.0-ns* "resourceRef")
					*xtm1.0-xlink* "href")
		  resourceRefs)))
    (when (eql xtm-format '2.0)
      (loop for item across (xpath-child-elems-by-qname topic *xtm2.0-ns* "occurrence")
	 when (xpath-single-child-elem-by-qname item *xtm2.0-ns* "resourceRef")
	 do (push (dom:node-value
		   (dom:get-attribute-node
		    (xpath-single-child-elem-by-qname item *xtm2.0-ns* "resourceRef")
		    "href")) resourceRefs)))
    (is (= (length values) (length resourceRefs)))
    (loop for item in resourceRefs
       do (is-true (find item values :test #'string=)))))


(defun check-occurrences-resourceData (topic values &key(xtm-format '2.0))
  (declare (dom:element topic))
  (declare (list values)) ;xtm2.0 -> (list (list :type value1 :data value2) ...); xtm1.0 -> (list data1 ...)
  (when (eql xtm-format '2.0)
    (let ((resourceData nil)
	  (resourceData-types nil))
      (loop for item across (xpath-child-elems-by-qname topic *xtm2.0-ns* "occurrence")
	 when (xpath-single-child-elem-by-qname item *xtm2.0-ns* "resourceData")
	 do (progn
	      (push (dom:node-value
		     (dom:get-attribute-node
		      (xpath-single-child-elem-by-qname item *xtm2.0-ns* "resourceData")
		      "datatype")) resourceData-types)
	      (push (xpath-fn-string
		     (xpath-single-child-elem-by-qname item *xtm2.0-ns* "resourceData")) resourceData)))
      (is (= (length resourceData) (length resourceData-types) (length values)))
      (loop for item in values
	 do (progn
	      (is-true (find (getf item :type) resourceData-types :test #'string=))
	      (is-true (find (getf item :data) resourceData :test #'string=))))))
  (when (eql xtm-format '1.0)
    (let ((resourceData nil))
      (loop for item across (xpath-child-elems-by-qname topic *xtm1.0-ns* "occurrence")
	 when (xpath-single-child-elem-by-qname item *xtm1.0-ns* "resourceData")
	 do (push (xpath-fn-string (xpath-single-child-elem-by-qname item *xtm1.0-ns* "resourceData")) resourceData))
      (is (= (length resourceData) (length values)))
      (loop for item in values
	 do (is-true (find item resourceData :test #'string=))))))


(defun check-variant (document variant-elem itemIdentities scopes resourceData resourceRef)
  "checks the varaint element in xtm2.0 format"
  (declare (dom:element document variant-elem))
  (declare (list itemIdentities resourceData scopes));resourceData -> :data "data" :type "type"
  (check-itemIdentities variant-elem itemIdentities)
  (let ((scope-values
	 (let ((scope-node
		(let ((scope-nodes (xpath-child-elems-by-qname variant-elem *xtm2.0-ns* "scope")))
		  (is (= (length scope-nodes) 1))
		  (elt scope-nodes 0))))
	   (let ((topicRef-nodes (xpath-child-elems-by-qname scope-node *xtm2.0-ns* "topicRef")))
	     (loop for topicRef across topicRef-nodes
		collect (get-subjectIdentifier-by-ref document (dom:get-attribute topicRef "href")))))))
    (is (= (length scope-values) (length scopes)))
    (loop for scope in scopes
       do (is-true (find scope scope-values :test #'string=))))
  (when resourceData
    (let ((resourceData-node
	   (let ((resourceData-nodes (xpath-child-elems-by-qname variant-elem *xtm2.0-ns* "resourceData")))
	     (is (= (length resourceData-nodes) 1))
	     (elt resourceData-nodes 0))))
      (let ((data (xpath-fn-string resourceData-node))
	    (type (dom:get-attribute resourceData-node "datatype")))
	(is (string= (getf resourceData :data) data))
	(is (string= (getf resourceData :type) type)))))
  (when resourceRef
    (let ((resourceRef-value
	   (let ((resourceRef-nodes (xpath-child-elems-by-qname variant-elem *xtm2.0-ns* "resourceRef")))
	     (declare (string resourceRef))
	     (is (= (length resourceRef-nodes) 1))
	     (get-subjectIdentifier-by-ref document (dom:get-attribute (elt resourceRef-nodes 0) "href")))))
      (is (string= resourceRef-value resourceRef)))))


(defun check-variant-xtm1.0 (document variant-elem scopes resourceData resourceRef)
  (declare (dom:element document variant-elem))
  (declare (list scopes))
  (let ((scope-values
	 (let ((parameters-nodes
		(xpath-child-elems-by-qname variant-elem *xtm1.0-ns* "parameters")))
	   (is (= (length parameters-nodes) 1))
	   (let ((topicRefs ;we export only topicRefs, so subjectIndicatorRefs can be ignored
		  (loop for topicRef across
		       (xpath-child-elems-by-qname (elt parameters-nodes 0) *xtm1.0-ns* "topicRef")
		     collect (get-subjectIndicatorRef-by-ref
			      document
			      (dom:get-attribute-ns topicRef *xtm1.0-xlink* "href")))))
	     topicRefs))))
    (is (= (length scope-values) (length scopes)))
    (loop for scope in scopes
       do (is-true (find scope scope-values :test #'string=))))

  (let ((variantName-node
	 (let ((variantName-nodes (xpath-child-elems-by-qname variant-elem *xtm1.0-ns* "variantName")))
	   (is (= (length variantName-nodes) 1))
	   (elt variantName-nodes 0))))
    (when resourceData
      (let ((resourceData-node
	     (let ((resourceData-nodes
		    (xpath-child-elems-by-qname variantName-node *xtm1.0-ns* "resourceData")))
	       (declare (string resourceData))
	       (is (= (length resourceData-nodes) 1))
	       (elt resourceData-nodes 0))))
	(is (string= resourceData (xpath-fn-string resourceData-node)))))
    (when resourceRef
      (let ((resourceRef-node
	     (let ((resourceRef-nodes
		    (xpath-child-elems-by-qname variantName-node *xtm1.0-ns* "resourceRef")))
	       (declare (string resourceRef))
	       (is (= (length resourceRef-nodes) 1))
	       (elt resourceRef-nodes 0))))
	(is (string= resourceRef
		     (dom:get-attribute-ns resourceRef-node *xtm1.0-xlink* "href")))))))


;; === variables: psis, itemIdentities, names, ... =============================
(defvar core-topic-psi "http://www.topicmaps.org/xtm/1.0/core.xtm#topic")
(defvar core-association-psi "http://www.topicmaps.org/xtm/1.0/core.xtm#association")
(defvar core-occurrence-psi "http://www.topicmaps.org/xtm/1.0/core.xtm#occurrence")
(defvar core-class-instance-psi "http://www.topicmaps.org/xtm/1.0/core.xtm#class-instance")
(defvar core-class-psi "http://www.topicmaps.org/xtm/1.0/core.xtm#class")
(defvar core-superclass-subclass-psi "http://www.topicmaps.org/xtm/1.0/core.xtm#supertype-subtype")
(defvar core-superclass-psi "http://www.topicmaps.org/xtm/1.0/core.xtm#supertype")
(defvar core-subclass-psi "http://www.topicmaps.org/xtm/1.0/core.xtm#subtype")
(defvar core-sort-psi "http://www.topicmaps.org/xtm/1.0/core.xtm#sort")
(defvar core-display-psi "http://www.topicmaps.org/xtm/1.0/core.xtm#display")
(defvar core-type-instance-psi "http://psi.topicmaps.org/iso13250/model/type-instance")
(defvar core-type-psi "http://psi.topicmaps.org/iso13250/model/type")
(defvar core-instance-psi "http://psi.topicmaps.org/iso13250/model/instance")
(defvar t1-psi "http://www.networkedplanet.com/psi/npcl/meta-types/topic-type")
(defvar t1-name "Topic Type")
(defvar t1-itemIdentity "http://psi.egovpt.org/itemIdentifiers#t1")
(defvar t1-itemIdentity-merge-1 "http://www.egovpt.org/itemIdentifiers#t1")
(defvar t2-psi "http://psi.egovpt.org/types/service")
(defvar t2-itemIdentity "http://psi.egovpt.org/itemIdentifiers#t2")
(defvar t2-itemIdentity-merge-1 "http://www.egovpt.org/itemIdentifiers#t2")
(defvar t2-name "Service")
(defvar t3-psi "http://psi.egovpt.org/types/standard")
(defvar t3-name "Standard")
(defvar t3-itemIdentity "http://psi.egovpt.org/itemIdentifiers#t3")
(defvar t3-itemIdentity-merge-1 "http://www.egovpt.org/itemIdentifiers#t3")
(defvar t3a-psi "http://psi.egovpt.org/types/semanticstandard")
(defvar t3a-psi-merge-1 "http://psi.egovpt.org/types/knowledgestandard")
(defvar t3a-psi-merge-2 "http://psi.egovpt.org/types/greatstandard")
(defvar t3a-name "Semantic Standard")
(defvar t3a-itemIdentity "http://psi.egovpt.org/itemIdentifiers#t3a")
(defvar t3a-itemIdentity-merge-1 "http://www.egovpt.org/itemIdentifiers#t3a")
(defvar t4-psi "http://psi.egovpt.org/types/subject")
(defvar t4-name "Subject")
(defvar t4-itemIdentity "http://psi.egovpt.org/itemIdentifiers#t4")
(defvar t6-psi "http://www.networkedplanet.com/psi/npcl/meta-types/occurrence-type")
(defvar t6-name "Occurrence Type")
(defvar t6-itemIdentity "http://psi.egovpt.org/itemIdentifiers#t6")
(defvar t7-psi "http://www.networkedplanet.com/psi/npcl/meta-types/association-type")
(defvar t7-name "Association Type")
(defvar t7-itemIdentity "http://psi.egovpt.org/itemIdentifiers#t7")
(defvar t8-psi "http://www.networkedplanet.com/psi/npcl/meta-types/association-role-type")
(defvar t8-name "Association Role Type")
(defvar t8-itemIdentity "http://psi.egovpt.org/itemIdentifiers#t8")
(defvar t50-psi "http://psi.egovpt.org/types/topicInTaxonomy")
(defvar t50-name "Topic used in this rudimentary taxonomy")
(defvar t50-itemIdentity "http://psi.egovpt.org/itemIdentifiers#t50")
(defvar t50a-psi "http://psi.egovpt.org/types/long-name")
(defvar t50a-name "long version of a name")
(defvar t50a-variant-name "Long-Version")
(defvar t50a-itemIdentity "http://psi.egovpt.org/itemIdentifiers#t50a")
(defvar t51-psi "http://psi.egovpt.org/types/standardHasStatus")
(defvar t51-name "Standard has a given status")
(defvar t51-itemIdentity "http://psi.egovpt.org/itemIdentifiers#t51")
(defvar t52-psi "http://psi.egovpt.org/status/InternationalStandard")
(defvar t52-name "International Standard")
(defvar t52-itemIdentity "http://psi.egovpt.org/itemIdentifiers#t52")
(defvar t53-psi "http://psi.egovpt.org/types/description")
(defvar t53-name "Topic has a given description")
(defvar t53-itemIdentity "http://psi.egovpt.org/itemIdentifiers#t53")
(defvar t54-psi "http://psi.egovpt.org/types/standardValidFromDate")
(defvar t54-name "Standard is valid from a given date")
(defvar t54-itemIdentity "http://psi.egovpt.org/itemIdentifiers#t54")
(defvar t55-psi "http://psi.egovpt.org/types/links")
(defvar t55-name "Links pertinent to this URL")
(defvar t55-itemIdentity "http://psi.egovpt.org/itemIdentifiers#t55")
(defvar t55-itemIdentity-merge-1 "http://psi.egovpt.org/itemIdentifiers#t55_1")
(defvar t56-psi "http://psi.egovpt.org/types/topicIsAboutSubject")
(defvar t56-name "topic is about subject")
(defvar t56-itemIdentity "http://psi.egovpt.org/itemIdentifiers#t56")
(defvar t57-psi "http://psi.egovpt.org/types/isNarrowerSubject")
(defvar t57-name "subject is narrower than the other one")
(defvar t57-itemIdentity "http://psi.egovpt.org/itemIdentifiers#t57")
(defvar t58-psi "http://psi.egovpt.org/types/narrowerSubject")
(defvar t58-name "subject is narrower than the other one")
(defvar t58-itemIdentity "http://psi.egovpt.org/itemIdentifiers#t58")
(defvar t59-psi "http://psi.egovpt.org/types/broaderSubject")
(defvar t59-name "subject is broader than the other one")
(defvar t59-itemIdentity "http://psi.egovpt.org/itemIdentifiers#t59")
(defvar t62-psi "http://psi.egovpt.org/types/StandardRoleType")
(defvar t62-name "role is standard")
(defvar t62-itemIdentity "http://psi.egovpt.org/itemIdentifiers#t62")
(defvar t63-psi "http://psi.egovpt.org/types/ServiceRoleType")
(defvar t63-name "role is service")
(defvar t63-itemIdentity "http://psi.egovpt.org/itemIdentifiers#t63")
(defvar t64-psi "http://psi.egovpt.org/types/serviceUsesStandard")
(defvar t64-name "service uses standard")
(defvar t64-itemIdentity "http://psi.egovpt.org/itemIdentifiers#t64")
(defvar t100-psi "http://psi.egovpt.org/standard/ISO+19115%3A+Geographic+Information+-+Metadata")
(defvar t100-name "ISO 19115")
(defvar t100-name-merge-1 "Geo Data")
(defvar t100-itemIdentity "http://psi.egovpt.org/itemIdentifiers#t100")
(defvar t100-itemIdentity-merge-1 "http://www.egovpt.org/itemIdentifiers#t99")
(defvar t100-name-itemIdentity "http://psi.egovpt.org/itemIdentifiers#t100_n1")
(defvar t100-name-itemIdentity "http://psi.egovpt.org/itemIdentifiers#t100_n1")
(defvar t100-variant-1-name "Geographic Information - Metadata")
(defvar t100-variant-1-itemIdentity "http://psi.egovpt.org/itemIdentifiers#t100_n1_v1")
(defvar t100-variant-2-name "ISO-19115")
(defvar t100-variant-2-itemIdentity "http://psi.egovpt.org/itemIdentifiers#t100_n1_v2")
(defvar t100-occurrence-itemIdentities (list "http://psi.egovpt.org/itemIdentifiers#t100_o1"
					     "http://psi.egovpt.org/itemIdentifiers#t100_o2"
					     "http://psi.egovpt.org/itemIdentifiers#t100_o3"
					     "http://psi.egovpt.org/itemIdentifiers#t100_o4"))
(defvar t100-occurrence-itemIdentities-merge-1 (list "http://psi.egovpt.org/itemIdentifiers#t100_o1"
					      "http://psi.egovpt.org/itemIdentifiers#t100_o2"
					      "http://psi.egovpt.org/itemIdentifiers#t100_o3"
					      "http://psi.egovpt.org/itemIdentifiers#t100_o4"
					      "http://www.egovpt.org/itemIdentifiers#t99_o1"))
(defvar t100-occurrences-resourceRef (list "http://www.budabe.de/"
					   "http://www.editeur.org/standards/ISO19115.pdf"))
(defvar t100-occurrences-resourceRef-merge-1 (list "http://www.budabe.de/"
					    "http://www.editeur.org/standards/ISO19115.pdf"
					    "http://www.budabe.eu/"))
(defvar t100-occurrences-resourceData (list (list :type "http://www.w3.org/2001/XMLSchema#string"
						  :data "The ISO 19115 standard ...")
					    (list :type "http://www.w3.org/2001/XMLSchema#date"
						  :data "2003-01-01")))
(defvar t101-psi "http://psi.egovpt.org/standard/Topic+Maps+2002")
(defvar t101-name-1 "Topic Maps 2002")
(defvar t101-name-2 "ISO/IEC 13250:2002: Topic Maps")
(defvar t101-name-2-itemIdentity "http://psi.egovpt.org/itemIdentifiers#t101_n2")
(defvar t101-variant-name "ISO/IEC-13250:2002")
(defvar t101-variant-itemIdentities (list "http://psi.egovpt.org/itemIdentifiers#t101_n2_v1"
					  "http://psi.egovpt.org/itemIdentifiers#t101_n2_v2"))
(defvar t101-occurrence-3-resourceData (list :type "//www.w3.org/2001/XMLSchema#date" :data "2002-05-19"))
(defvar t101-occurrence-4-resourceRef
  "http://www1.y12.doe.gov/capabilities/sgml/sc34/document/0322_files/iso13250-2nd-ed-v2.pdf")
(defvar t202-psi "http://psi.egovpt.org/subject/Data")
(defvar t202-name "Types of data")
(defvar t203-psi "http://psi.egovpt.org/subject/GeoData")
(defvar t203-name "Geographic Data")
(defvar t204-psi "http://psi.egovpt.org/subject/Legal+Data")
(defvar t204-name "Legal data")
(defvar t301a-psi-1 "http://psi.egovpt.org/service/Google+Maps")
(defvar t301a-psi-2 "http://maps.google.com")
(defvar t301a-name-value-1 "Google Maps")
(defvar t301a-name-value-2 "Google Maps Application")
(defvar t301a-name-itemIdentity "http://psi.egovpt.org/topic/t301a_n1")
(defvar t301a-occurrence-resourceRef "http://maps.google.de")
(defvar t300-psi "http://psi.egovpt.org/service/Norwegian+National+Curriculum")
(defvar t300-name "Norwegian National Curriculum")
(defvar t300-occurrence-1-resourceData "Normative version of the national Norwegian curriculum")
(defvar t300-occurrence-2-resourceRef "http://www.udir.no")
(defvar new-t100-psi "http://psi.egovpt.org/standard/Common+Lisp")
(defvar new-t100-itemIdentity "http://www.egovpt.org/itemIdentifiers#t100")
(defvar new-t100-itemIdentity-merge-2 "http://www.egovpt.org/itemIdentifiers#t100_new")
(defvar new-t100-name "Common Lisp")
(defvar new-t100-name-itemIdentities (list "http://www.egovpt.org/itemIdentifiers#t100_n1"
				    "http://www.egovpt.org/itemIdentifiers#t100_n1a"))
(defvar new-t100-variant-name "Common-Lisp")
(defvar new-t100-variant-itemIdentity "http://www.egovpt.org/itemIdentifiers#t100_n_v1")
(defvar new-t100-variant-name-merge-2 "CL")
(defvar new-t100-variant-itemIdentity-merge-2 "http://www.egovpt.org/itemIdentifiers#t100_n_v2")
(defvar new-t100-occurrence-resourceRef  "http://www.common-lisp.net/")
(defvar new-t100-occurrence-resourceRef-merge-2 (list "http://www.common-lisp.net/"
						      "http://www.cliki.net/"))
(defvar new-t100-occurrence-1-itemIdentity "http://www.egovpt.org/itemIdentifiers#t100_o1")
(defvar new-t100-occurrence-2-itemIdentity "http://www.egovpt.org/itemIdentifiers#t100_o2")
(defvar assoc-2-itemIdentity "http://psi.egovpt.org/itemIdentifiers#assoc_7")
(defvar assoc-6-itemIdentity "http://psi.egovpt.org/itemIdentifiers#assoc_6")
(defvar assoc-6-role-1-itemIdentity "http://psi.egovpt.org/itemIdentifiers#role_6_1")
(defvar assoc-6-role-2-itemIdentities (list "http://psi.egovpt.org/itemIdentifiers#role_6_2a"
					    "http://psi.egovpt.org/itemIdentifiers#role_6_2b"
					    "http://psi.egovpt.org/itemIdentifiers#role_6_2c"))
(defvar string-type "http://www.w3.org/2001/XMLSchema#string")


;; === checks all topics from core_psis.xtm ====================================
(test test-std-topics
  (with-fixture refill-test-db ()
    (export-xtm *out-xtm2.0-file*)
    (let ((document (dom:document-element (cxml:parse-file *out-xtm2.0-file* (cxml-dom:make-dom-builder))))
	  (topic-counter 0))
      (check-document-structure document 38 2)
      (loop for topic across (xpath-child-elems-by-qname document *xtm2.0-ns* "topic")
	 do (loop for subjectIdentifier across (xpath-child-elems-by-qname topic *xtm2.0-ns* "subjectIdentifier")
	       do (let ((href (dom:node-value (dom:get-attribute-node subjectIdentifier "href"))))	
		    (cond
		      ((string= core-topic-psi href)
		       (incf topic-counter)
		       (format t "name: ~A~%" (xpath-single-child-elem-by-qname topic *xtm2.0-ns* "name")))
		      ((string= core-association-psi href)
		       (incf topic-counter)
		       (format t "name: ~A~%" (xpath-single-child-elem-by-qname topic *xtm2.0-ns* "name")))
		      ((string= core-occurrence-psi href)
		       (incf topic-counter)
		       (format t "name: ~A~%" (xpath-single-child-elem-by-qname topic *xtm2.0-ns* "name")))
		      ((string= core-class-instance-psi href)
		       (incf topic-counter)
		       (format t "name: ~A~%" (xpath-single-child-elem-by-qname topic *xtm2.0-ns* "name")))
		      ((string= core-class-psi href)
		       (incf topic-counter)
		       (format t "name: ~A~%" (xpath-single-child-elem-by-qname topic *xtm2.0-ns* "name")))
		      ((string= core-superclass-subclass-psi href)
		       (incf topic-counter)
		       (format t "name: ~A~%" (xpath-single-child-elem-by-qname topic *xtm2.0-ns* "name")))
		      ((string= core-superclass-psi href)
		       (incf topic-counter)
		       (format t "name: ~A~%" (xpath-single-child-elem-by-qname topic *xtm2.0-ns* "name")))
		      ((string= core-subclass-psi href)
		       (incf topic-counter)
		       (format t "name: ~A~%" (xpath-single-child-elem-by-qname topic *xtm2.0-ns* "name")))
		      ((string= core-sort-psi href)
		       (incf topic-counter)
		       (format t "name: ~A~%" (xpath-single-child-elem-by-qname topic *xtm2.0-ns* "name")))
		      ((string= core-display-psi href)
		       (incf topic-counter)
		       (format t "name: ~A~%" (xpath-single-child-elem-by-qname topic *xtm2.0-ns* "name")))
		      ((string= core-type-instance-psi href)
		       (incf topic-counter)
		       (format t "name: ~A~%" (xpath-single-child-elem-by-qname topic *xtm2.0-ns* "name")))
		      ((string= core-type-psi href)
		       (incf topic-counter)
		       (format t "name: ~A~%" (xpath-single-child-elem-by-qname topic *xtm2.0-ns* "name")))
		      ((string= core-instance-psi href)
		       (incf topic-counter)
		       (format t "name: ~A~%" (xpath-single-child-elem-by-qname topic *xtm2.0-ns* "name")))))))
      (is (= topic-counter 13)))))


;; === checks the topics t1-t10 from sample_objects_2_0.xtm ====================
(test test-sample-topics-t1-t10
  (with-fixture refill-test-db()
    (export-xtm *out-xtm2.0-file*)
    (let ((document (dom:document-element (cxml:parse-file *out-xtm2.0-file* (cxml-dom:make-dom-builder)))))
      (check-document-structure document 38 2)
      (loop for topic across (xpath-child-elems-by-qname document *xtm2.0-ns* "topic")
	 do (loop for subjectIdentifier across (xpath-child-elems-by-qname topic *xtm2.0-ns* "subjectIdentifier")
	       do (let ((href (dom:node-value (dom:get-attribute-node subjectIdentifier "href"))))
		    (cond
		      ((string= href t1-psi)
		       (check-topic-id topic)
		       (check-single-name-value topic t1-name)
		       (check-itemIdentities topic (list t1-itemIdentity)))
		      ((string= href t3-psi)
		       (check-topic-id topic)
		       (check-single-name-value topic t3-name)
		       (check-itemIdentities topic (list t3-itemIdentity))
		       (check-single-instanceOf document topic t1-psi))
		      ((string= href t3a-psi)
		       (check-topic-id topic)
		       (check-single-name-value topic t3a-name)
		       (check-itemIdentities topic (list t3a-itemIdentity))
		       (check-single-instanceOf document topic t3-psi))
		      ((string= href t4-psi)
		       (check-topic-id topic)
		       (check-single-name-value topic t4-name)
		       (check-itemIdentities topic (list t4-itemIdentity))
		       (check-single-instanceOf document topic t1-psi))
		      ((string= href t6-psi)
		       (check-topic-id topic)
		       (check-single-name-value topic t6-name)
		       (check-itemIdentities topic (list t6-itemIdentity))
		       (check-single-instanceOf document topic t1-psi))
		      ((string= href t7-psi)
		       (check-topic-id topic)
		       (check-single-name-value topic t7-name)
		       (check-itemIdentities topic (list t7-itemIdentity))
		       (check-single-instanceOf document topic t1-psi))
		      ((string= href t8-psi)
		       (check-topic-id topic)
		       (check-single-name-value topic t8-name)
		       (check-itemIdentities topic (list t8-itemIdentity))
		       (check-single-instanceOf document topic t1-psi)))))))))


;; === checks the topics t50-t59 from sample_objects_2_0.xtm ===================
(test test-sample-topics-t50-t59
  (with-fixture refill-test-db()
    (export-xtm *out-xtm2.0-file*)
    (let ((document (dom:document-element (cxml:parse-file *out-xtm2.0-file* (cxml-dom:make-dom-builder)))))
      (check-document-structure document 38 2)      
      (loop for topic across (xpath-child-elems-by-qname document *xtm2.0-ns* "topic")
	 do (loop for subjectIdentifier across (xpath-child-elems-by-qname topic *xtm2.0-ns* "subjectIdentifier")
	       do (let ((href (dom:node-value (dom:get-attribute-node subjectIdentifier "href"))))
		    (cond
		      ((string= href t50-psi)
		       (check-topic-id topic)
		       (check-single-name-value topic t50-name)
		       (check-itemIdentities topic (list t50-itemIdentity))
		       (check-single-instanceOf document topic t1-psi))
		      ((string= href t50a-psi)
		       (check-topic-id topic)
		       (check-single-name-value topic t50a-name)
		       (check-itemIdentities topic (list t50a-itemIdentity))
		       (check-single-instanceOf document topic t6-psi))
		      ((string= href t51-psi)
		       (check-topic-id topic)
		       (check-single-name-value topic t51-name)
		       (check-itemIdentities topic (list t51-itemIdentity))
		       (check-single-instanceOf document topic t6-psi))
		      ((string= href t52-psi)
		       (check-topic-id topic)
		       (check-single-name-value topic t52-name)
		       (check-itemIdentities topic (list t52-itemIdentity))
		       (check-single-instanceOf document topic t50-psi))
		      ((string= href t53-psi)
		       (check-topic-id topic)
		       (check-single-name-value topic t53-name)
		       (check-itemIdentities topic (list t53-itemIdentity))
		       (check-single-instanceOf document topic t6-psi))
		      ((string= href t54-psi)
		       (check-topic-id topic)
		       (check-single-name-value topic t54-name)
		       (check-itemIdentities topic (list t54-itemIdentity))
		       (check-single-instanceOf document topic t6-psi))
		      ((string= href t55-psi)
		       (check-topic-id topic)
		       (check-single-name-value topic t55-name)
		       (check-itemIdentities topic (list t55-itemIdentity))
		       (check-single-instanceOf document topic t6-psi))
		      ((string= href t56-psi)
		       (check-topic-id topic)
		       (check-single-name-value topic t56-name)
		       (check-itemIdentities topic (list t56-itemIdentity))
		       (check-single-instanceOf document topic t7-psi))
		      ((string= href t57-psi)
		       (check-topic-id topic)
		       (check-single-name-value topic t57-name)
		       (check-itemIdentities topic (list t57-itemIdentity))
		       (check-single-instanceOf document topic t7-psi))
		      ((string= href t58-psi)
		       (check-topic-id topic)
		       (check-single-name-value topic t58-name)
		       (check-itemIdentities topic (list t58-itemIdentity))
		       (check-single-instanceOf document topic t8-psi))
		      ((string= href t59-psi)
		       (check-topic-id topic)
		       (check-single-name-value topic t59-name)
		       (check-itemIdentities topic (list t59-itemIdentity))
		       (check-single-instanceOf document topic t8-psi)))))))))


;; === checks the topics t60-t100 from sample_objects_2_0.xtm ==================
(test test-sample-topics-t60-t100
  (with-fixture refill-test-db()
    (export-xtm *out-xtm2.0-file*)
    (let ((document (dom:document-element (cxml:parse-file *out-xtm2.0-file* (cxml-dom:make-dom-builder)))))
      (check-document-structure document 38 2)
      (loop for topic across (xpath-child-elems-by-qname document *xtm2.0-ns* "topic")
	 do (loop for subjectIdentifier across (xpath-child-elems-by-qname topic *xtm2.0-ns* "subjectIdentifier")
	       do (let ((href (dom:node-value (dom:get-attribute-node subjectIdentifier "href"))))
		    (cond
		      ((string= href t62-psi)
		       (check-topic-id topic)
		       (check-single-name-value topic t62-name)
		       (check-itemIdentities topic (list t62-itemIdentity))
		       (check-single-instanceOf document topic t8-psi))
		      ((string= href t63-psi)
		       (check-topic-id topic)
		       (check-single-name-value topic t63-name)
		       (check-itemIdentities topic (list t63-itemIdentity))
		       (check-single-instanceOf document topic t8-psi))
		      ((string= href t64-psi)
		       (check-topic-id topic)
		       (check-single-name-value topic t64-name)
		       (check-itemIdentities topic (list t64-itemIdentity))
		       (check-single-instanceOf document topic t7-psi))
		      ((string= href t100-psi)
		       (check-topic-id topic)
		       (check-single-name-value topic t100-name)
		       (check-single-name-itemIdentity topic t100-name-itemIdentity)
		       (check-itemIdentities topic (list t100-itemIdentity))
		       (check-single-instanceOf document topic t3a-psi)
		       (check-occurrences-itemIdentity topic t100-occurrence-itemIdentities)
		       (check-occurrences-type document topic (list t51-psi t53-psi t54-psi t55-psi))
		       (check-occurrences-resourceRef topic t100-occurrences-resourceRef)
		       (check-occurrences-resourceData topic t100-occurrences-resourceData)))))))))


;; === checks the topics t101-t301 from sample_objects_2_0.xtm =================
(test test-sample-topics-t101-t301
  (with-fixture refill-test-db ()
    (export-xtm *out-xtm2.0-file*)
    (let ((document (dom:document-element (cxml:parse-file *out-xtm2.0-file* (cxml-dom:make-dom-builder)))))
      (check-document-structure document 38 2)
      (loop for topic across (xpath-child-elems-by-qname document *xtm2.0-ns* "topic")
	 do (loop for subjectIdentifier across (xpath-child-elems-by-qname topic *xtm2.0-ns* "subjectIdentifier")
	       do (let ((href (dom:node-value (dom:get-attribute-node subjectIdentifier "href"))))
		    (cond
		      ((string= href t202-psi)
		       (check-topic-id topic)
		       (check-single-name-value topic t202-name)
		       (check-single-instanceOf document topic t4-psi))
		      ((string= href t204-psi)
		       (check-topic-id topic)
		       (check-single-name-value topic t204-name)
		       (check-single-instanceOf document topic t4-psi))
		      ((or (string= href t301a-psi-1)
			   (string= href t301a-psi-2))
		       (check-topic-id topic)
		       ;; names
		       (loop for name across (xpath-child-elems-by-qname topic *xtm2.0-ns* "name")
			  do (let ((name-value (xpath-fn-string
						(xpath-single-child-elem-by-qname name *xtm2.0-ns* "value")))
				   (name-itemIdentity
				    (when (xpath-single-child-elem-by-qname name *xtm2.0-ns* "itemIdentity")
				      (dom:node-value (dom:get-attribute-node
						 (xpath-single-child-elem-by-qname name *xtm2.0-ns* "itemIdentity")
						 "href"))))
				   (name-scope
				    (when (xpath-single-child-elem-by-qname name *xtm2.0-ns* "scope")
				      (get-subjectIdentifier-by-ref
				       document
				       (dom:node-value (dom:get-attribute-node
							(xpath-single-child-elem-by-qname
							 (xpath-single-child-elem-by-qname name *xtm2.0-ns* "scope")
							 *xtm2.0-ns* "topicRef") "href"))))))
				      
			       (cond
				 ((string= t301a-name-value-1 name-value)
				  (is (string= t301a-name-itemIdentity name-itemIdentity)))		       
				 ((string= t301a-name-value-2 name-value)
				  (is (string= t50a-psi name-scope)))
				 (t
				(is (string= t301a-name-value-1 name-value))
				(is (string= t301a-name-value-2 name-value))))))
		       ;; occurrences
		       (let ((occurrence-nodes (xpath-child-elems-by-qname topic *xtm2.0-ns* "occurrence")))
			 (is (= (length occurrence-nodes) 1))
			 (let ((occurrence-type (get-subjectIdentifier-by-ref
						 document
						 (dom:node-value
						  (dom:get-attribute-node
						   (xpath-single-child-elem-by-qname
						    (xpath-single-child-elem-by-qname
						     (elt occurrence-nodes 0) *xtm2.0-ns* "type")
						    *xtm2.0-ns* "topicRef")
						   "href"))))
			       (occurrence-resourceRef (dom:node-value
							(dom:get-attribute-node
							 (xpath-single-child-elem-by-qname
							  (elt occurrence-nodes 0) *xtm2.0-ns* "resourceRef")
							 "href"))))
			   (is (string= occurrence-type t55-psi))
			   (is (string= occurrence-resourceRef t301a-occurrence-resourceRef))))))))))))


		      
;; === checks the associations from sample_objects_2_0.xtm =====================
(test test-sample-associations
  (with-fixture refill-test-db()
    (export-xtm *out-xtm2.0-file*)
    (let ((document (dom:document-element (cxml:parse-file *out-xtm2.0-file* (cxml-dom:make-dom-builder)))))
      (check-document-structure document 38 2)
      (let ((assoc-1 (elt (xpath-child-elems-by-qname document *xtm2.0-ns* "association") 0))
	    (assoc-2 (elt (xpath-child-elems-by-qname document *xtm2.0-ns* "association") 1)))
	(let ((assoc-1-type (get-subjectIdentifier-by-ref
			     document
			     (dom:node-value (dom:get-attribute-node
					      (xpath-single-child-elem-by-qname
					       (xpath-single-child-elem-by-qname assoc-1 *xtm2.0-ns* "type")
					       *xtm2.0-ns* "topicRef")
					      "href"))))
	      (assoc-2-type (get-subjectIdentifier-by-ref
			     document
			     (dom:node-value (dom:get-attribute-node
					      (xpath-single-child-elem-by-qname
					       (xpath-single-child-elem-by-qname assoc-2 *xtm2.0-ns* "type")
					       *xtm2.0-ns* "topicRef")
					      "href"))))
	      (assoc-1-role-1-type (get-subjectIdentifier-by-ref
				    document
				    (dom:node-value
				     (dom:get-attribute-node
				      (xpath-single-child-elem-by-qname
				       (xpath-single-child-elem-by-qname
					(elt (xpath-child-elems-by-qname assoc-1 *xtm2.0-ns* "role") 0)
					*xtm2.0-ns* "type")
				       *xtm2.0-ns* "topicRef")
				      "href"))))
	      (assoc-1-role-2-type (get-subjectIdentifier-by-ref
				    document
				    (dom:node-value
				     (dom:get-attribute-node
				      (xpath-single-child-elem-by-qname
				       (xpath-single-child-elem-by-qname
					(elt (xpath-child-elems-by-qname assoc-1 *xtm2.0-ns* "role") 1)
					*xtm2.0-ns* "type")
				       *xtm2.0-ns* "topicRef")
				      "href"))))
	      (assoc-2-role-1-type (get-subjectIdentifier-by-ref
				    document
				    (dom:node-value
				     (dom:get-attribute-node
				      (xpath-single-child-elem-by-qname
				       (xpath-single-child-elem-by-qname
					(elt (xpath-child-elems-by-qname assoc-2 *xtm2.0-ns* "role") 0)
					*xtm2.0-ns* "type")
				       *xtm2.0-ns* "topicRef")
				      "href"))))
	      (assoc-2-role-2-type (get-subjectIdentifier-by-ref
				    document
				    (dom:node-value
				     (dom:get-attribute-node
				      (xpath-single-child-elem-by-qname
				       (xpath-single-child-elem-by-qname
					(elt (xpath-child-elems-by-qname assoc-2 *xtm2.0-ns* "role") 1)
					*xtm2.0-ns* "type")
				       *xtm2.0-ns* "topicRef")
				      "href"))))
	      (assoc-1-role-1-topicRef (get-subjectIdentifier-by-ref
					document
					(dom:node-value
					 (dom:get-attribute-node
					  (xpath-single-child-elem-by-qname
					   (elt (xpath-child-elems-by-qname assoc-1 *xtm2.0-ns* "role") 0)
					   *xtm2.0-ns* "topicRef")
					  "href"))))
	      (assoc-1-role-2-topicRef (get-subjectIdentifier-by-ref
					document
					(dom:node-value
					 (dom:get-attribute-node
					  (xpath-single-child-elem-by-qname
					   (elt (xpath-child-elems-by-qname assoc-1 *xtm2.0-ns* "role") 1)
					   *xtm2.0-ns* "topicRef")
					  "href"))))
	      (assoc-2-role-1-topicRef (get-subjectIdentifier-by-ref
					document
					(dom:node-value
					 (dom:get-attribute-node
					  (xpath-single-child-elem-by-qname
					   (elt (xpath-child-elems-by-qname assoc-2 *xtm2.0-ns* "role") 0)
					   *xtm2.0-ns* "topicRef")
					  "href"))))
	      (assoc-2-role-2-topicRef (get-subjectIdentifier-by-ref
					document
					(dom:node-value
					 (dom:get-attribute-node
					  (xpath-single-child-elem-by-qname
					   (elt (xpath-child-elems-by-qname assoc-2 *xtm2.0-ns* "role") 1)
					   *xtm2.0-ns* "topicRef")
					  "href")))))	  
	  (when (string= assoc-1-type t57-psi)
	    (is (string= assoc-1-role-1-type t59-psi))
	    (is (string= assoc-1-role-1-topicRef t202-psi))
	    (is (string= assoc-1-role-2-type t58-psi))
	    (is (string= assoc-1-role-2-topicRef t204-psi))
	    (check-itemIdentities assoc-2 (list assoc-2-itemIdentity))
	    (is (string= assoc-2-type t64-psi))
	    (is (string= assoc-2-role-1-type t63-psi))
	    (is (or (string= assoc-2-role-1-topicRef t301a-psi-1)
		    (string= assoc-2-role-1-topicRef t301a-psi-2)))
	    (is (string= assoc-2-role-2-type t62-psi))
	    (is (string= assoc-2-role-2-topicRef t100-psi)))
	  (when (string= assoc-1-type t64-psi)
	    (check-itemIdentities assoc-1 (list assoc-2-itemIdentity))
	    (is (string= assoc-1-role-1-type t63-psi))
	    (is (or (string= assoc-1-role-1-topicRef t301a-psi-1)
		    (string= assoc-1-role-1-topicRef t301a-psi-2)))
	    (is (string= assoc-1-role-2-type t62-psi))
	    (is (string= assoc-1-role-2-topicRef t100-psi))
	    (is (string= assoc-2-type t57-psi))
	    (is (string= assoc-2-role-1-type t59-psi))
	    (is (string= assoc-2-role-1-topicRef t202-psi))
	    (is (string= assoc-2-role-2-type t58-psi))
	    (is (string= assoc-2-role-2-topicRef t204-psi))))))))


;; === checks the fragment of the topic t100 from sample_objects_2_0.xtm =======
(test test-fragments
  (with-fixture refill-test-db()
    (let* ((t100 (loop for item in (elephant:get-instances-by-class 'PersistentIdC)
		    when (string= (uri item) t100-psi)
		    return (identified-construct item)))
	   (t100-start-revision (d::start-revision (first (d::versions t100)))))

      (d:get-fragments t100-start-revision)
      (let ((t100-fragment (loop for item in (elephant:get-instances-by-class 'FragmentC)
			      when (eq (topic item) t100)
			      return item)))

	(with-open-file (stream *out-xtm2.0-file* :direction :output)
	  (write-string (export-xtm-fragment t100-fragment) stream))))

    (let ((document (dom:document-element (cxml:parse-file *out-xtm2.0-file* (cxml-dom:make-dom-builder)))))
      (check-document-structure document 10 1)
      (loop for topic across (xpath-child-elems-by-qname document *xtm2.0-ns* "topic")
	 do (loop for subjectIdentifier across (xpath-child-elems-by-qname topic *xtm2.0-ns* "subjectIdentifier")
	       do (let ((href (dom:node-value (dom:get-attribute-node subjectIdentifier "href"))))
		    (cond
		      ((string= href t3a-psi)
		       (check-topic-id topic)
		       (check-itemIdentities topic (list t3a-itemIdentity)))
		      ((string= href t51-psi)
		       (check-topic-id topic)
		       (check-itemIdentities topic (list t51-itemIdentity)))
		      ((string= href t53-psi)
		       (check-topic-id topic)
		       (check-itemIdentities topic (list t53-itemIdentity)))
		      ((string= href t54-psi)
		       (check-topic-id topic)
		       (check-itemIdentities topic (list t54-itemIdentity)))
		      ((string= href t55-psi)
		       (check-topic-id topic)
		       (check-itemIdentities topic (list t55-itemIdentity)))
		      ((string= href t62-psi)
		       (check-topic-id topic)
		       (check-itemIdentities topic (list t62-itemIdentity)))
		      ((string= href t63-psi)
		       (check-topic-id topic)
		       (check-itemIdentities topic (list t63-itemIdentity)))
		      ((string= href t64-psi)
		       (check-topic-id topic)
		       (check-itemIdentities topic (list t64-itemIdentity)))
		      ((string= href t100-psi)
		       (check-topic-id topic)
		       (check-single-name-value topic t100-name)
		       (check-single-name-itemIdentity topic t100-name-itemIdentity)
		       (check-itemIdentities topic (list t100-itemIdentity))
		       (check-single-instanceOf document topic t3a-psi)
		       (check-occurrences-itemIdentity topic t100-occurrence-itemIdentities)
		       (check-occurrences-type document topic (list t51-psi t53-psi t54-psi t55-psi))
		       (check-occurrences-resourceRef topic t100-occurrences-resourceRef)
		       (check-occurrences-resourceData topic t100-occurrences-resourceData))
		      ((or (string= href t301a-psi-1)
			   (string= href t301a-psi-2))
		       (check-topic-id topic))
		      (t
			(is-true (format t "unknown topic subjectIdentifier")))))))

      (let ((assoc (xpath-single-child-elem-by-qname document *xtm2.0-ns* "association")))
	(let ((assoc-type (get-subjectIdentifier-by-ref
			   document
			   (dom:node-value (dom:get-attribute-node
					    (xpath-single-child-elem-by-qname
					     (xpath-single-child-elem-by-qname assoc *xtm2.0-ns* "type")
					     *xtm2.0-ns* "topicRef")
					    "href"))))
	      (assoc-role-1-type (get-subjectIdentifier-by-ref
				  document
				  (dom:node-value
				   (dom:get-attribute-node
				    (xpath-single-child-elem-by-qname
				     (xpath-single-child-elem-by-qname
				      (elt (xpath-child-elems-by-qname assoc *xtm2.0-ns* "role") 0)
				      *xtm2.0-ns* "type")
				     *xtm2.0-ns* "topicRef")
				    "href"))))
	      (assoc-role-2-type (get-subjectIdentifier-by-ref
				  document
				  (dom:node-value
				   (dom:get-attribute-node
				    (xpath-single-child-elem-by-qname
				     (xpath-single-child-elem-by-qname
				      (elt (xpath-child-elems-by-qname assoc *xtm2.0-ns* "role") 1)
				      *xtm2.0-ns* "type")
				     *xtm2.0-ns* "topicRef")
				    "href"))))
	      (assoc-role-1-topicRef (get-subjectIdentifier-by-ref
				      document
				      (dom:node-value
				       (dom:get-attribute-node
					(xpath-single-child-elem-by-qname
					 (elt (xpath-child-elems-by-qname assoc *xtm2.0-ns* "role") 0)
					 *xtm2.0-ns* "topicRef")
					"href"))))
	      (assoc-role-2-topicRef (get-subjectIdentifier-by-ref
				      document
				      (dom:node-value
				       (dom:get-attribute-node
					(xpath-single-child-elem-by-qname
					 (elt (xpath-child-elems-by-qname assoc *xtm2.0-ns* "role") 1)
					 *xtm2.0-ns* "topicRef")
					"href")))))	  
	  (when (string= assoc-type t64-psi)
	    (is (string= assoc-role-1-type t63-psi))
	    (is (or (string= assoc-role-1-topicRef t301a-psi-1)
		    (string= assoc-role-1-topicRef t301a-psi-2)))
	    (is (string= assoc-role-2-type t62-psi))
	    (is (string= assoc-role-2-topicRef t100-psi))
	    (check-itemIdentities assoc (list assoc-2-itemIdentity))))))))


;; === tests the export to a xtm2.0 file from a certain revision ===============
(test test-exporter-xtm2.0-versions-1
  (with-fixture merge-test-db ()
    (handler-case (delete-file *out-xtm2.0-file*)(error () )) ;deletes file - if exist
    (export-xtm *out-xtm2.0-file* :revision fixtures::revision1)
    (let ((document (dom:document-element (cxml:parse-file *out-xtm2.0-file* (cxml-dom:make-dom-builder)))))
      (check-document-structure document 47 7)
      (loop for topic across (xpath-child-elems-by-qname document *xtm2.0-ns* "topic")
	 do (loop for subjectIdentifier across (xpath-child-elems-by-qname topic *xtm2.0-ns* "subjectIdentifier")
	       do (let ((href (dom:get-attribute subjectIdentifier "href")))
		    (cond
		      ((string= href t1-psi)
		       (check-topic-id topic)
		       (check-itemIdentities topic (list t1-itemIdentity))
		       (check-single-name-value topic t1-name))
		      ((string= href t2-psi)
		       (check-topic-id topic)
		       (check-itemIdentities topic (list t2-itemIdentity))
		       (check-single-name-value topic t2-name)
		       (check-single-instanceOf document topic t1-psi))
		      ((string= href t3-psi)
		       (check-topic-id topic)
		       (check-itemIdentities topic (list t3-itemIdentity))
		       (check-single-name-value topic t3-name)
		       (check-single-instanceOf document topic t1-psi))
		      ((string= href t3a-psi)
		       (check-topic-id topic)
		       (check-itemIdentities topic (list t3a-itemIdentity))
		       (check-single-name-value topic t3a-name)
		       (check-single-instanceOf document topic t3-psi))
		      ((string= href t50-psi)
		       (check-topic-id topic)
		       (check-itemIdentities topic (list t50-itemIdentity))
		       (check-single-name-value topic t50-name)
		       (check-single-instanceOf document topic t1-psi))
		      ((string= href t50a-psi)
		       (check-topic-id topic)
		       (check-itemIdentities topic (list t50a-itemIdentity))
		       (check-single-name-value topic t50a-name)
		       (check-single-instanceOf document topic t6-psi)
		       (let ((variant-node
			      (let ((variant-nodes
				     (xpath-child-elems-by-qname 
				      (xpath-single-child-elem-by-qname topic *xtm2.0-ns* "name")
				      *xtm2.0-ns* "variant")))
				(is (= (length variant-nodes) 1))
				(elt variant-nodes 0))))
			 (check-variant document variant-node nil (list core-sort-psi)
					(list :data t50a-variant-name :type string-type) nil)))
		      ((string= href t55-psi)
		       (check-topic-id topic)
		       (check-itemIdentities topic (list t55-itemIdentity))
		       (check-single-name-value topic t55-name)
		       (check-single-instanceOf document topic t6-psi))
		      ((string= href t57-psi)
		       (check-topic-id topic)
		       (check-itemIdentities topic (list t57-itemIdentity))
		       (check-single-name-value topic t57-name)
		       (check-single-instanceOf document topic t7-psi))
		      ((string= href t58-psi)
		       (check-topic-id topic)
		       (check-itemIdentities topic (list t58-itemIdentity))
		       (check-single-name-value topic t58-name)
		       (check-single-instanceOf document topic t8-psi))
		      ((string= href t59-psi)
		       (check-topic-id topic)
		       (check-itemIdentities topic (list t59-itemIdentity))
		       (check-single-name-value topic t59-name)
		       (check-single-instanceOf document topic t8-psi))
		      ((string= href t62-psi)
		       (check-topic-id topic)
		       (check-itemIdentities topic (list t62-itemIdentity))
		       (check-single-name-value topic t62-name)
		       (check-single-instanceOf document topic t8-psi))
		      ((string= href t64-psi)
		       (check-topic-id topic)
		       (check-itemIdentities topic (list t64-itemIdentity))
		       (check-single-name-value topic t64-name)
		       (check-single-instanceOf document topic t7-psi))
		      ((string= href t100-psi)
		       (check-topic-id topic)
		       (check-single-name-value topic t100-name)
		       (check-single-name-itemIdentity topic t100-name-itemIdentity)
		       (check-itemIdentities topic (list t100-itemIdentity))
		       (check-single-instanceOf document topic t3a-psi)
		       (check-occurrences-itemIdentity topic t100-occurrence-itemIdentities)
		       (check-occurrences-type document topic (list t51-psi t53-psi t54-psi t55-psi))
		       (check-occurrences-resourceRef topic t100-occurrences-resourceRef)
		       (check-occurrences-resourceData topic t100-occurrences-resourceData)
		       (let ((variant-nodes (xpath-child-elems-by-qname 
					     (xpath-single-child-elem-by-qname topic *xtm2.0-ns* "name")
					     *xtm2.0-ns* "variant")))
			 (is (= (length variant-nodes) 2))
			 (loop for variant across variant-nodes
			    do (let ((resourceRef
				      (xpath-fn-string
				       (xpath-single-child-elem-by-qname variant *xtm2.0-ns* "resourceData"))))
				 (cond
				   ((string= resourceRef t100-variant-1-name)
				    (check-variant document variant (list t100-variant-1-itemIdentity)
						   (list core-display-psi)
						   (list :data t100-variant-1-name :type string-type) nil))
				   ((string= resourceRef t100-variant-2-name)
				    (check-variant document variant (list t100-variant-2-itemIdentity)
						   (list core-sort-psi)
						   (list :data t100-variant-2-name :type string-type) nil))
				   (t
				    (is-true
				     (format t "bad resourceRef oin variant of t100 found: ~A~%" resourceRef))))))))
		      ((string= href t101-psi)
		       (check-topic-id topic)
		       (is (= (length (xpath-child-elems-by-qname topic *xtm2.0-ns* "itemIdentity")) 0))
		       (loop for name across (xpath-child-elems-by-qname topic *xtm2.0-ns* "name")
			  when (string= t101-name-1 (xpath-fn-string
						     (xpath-single-child-elem-by-qname name *xtm2.0-ns* "value")))
			  do (is-true t)
			  when (string= t101-name-2 (xpath-fn-string
						     (xpath-single-child-elem-by-qname name *xtm2.0-ns* "value")))
			  do (let ((ii-nodes (xpath-child-elems-by-qname name *xtm2.0-ns* "itemIdentity"))
				   (scope-nodes (xpath-child-elems-by-qname name *xtm2.0-ns* "scope"))
				   (type-nodes (xpath-child-elems-by-qname name *xtm2.0-ns* "type")))
			       (is (= (length ii-nodes) 1))
			       (is (string= t101-name-2-itemIdentity (dom:get-attribute (elt ii-nodes 0) "href")))
			       (is (= (length scope-nodes) 1))
			       (is (string= t50a-psi (get-subjectIdentifier-by-ref
						      document
						      (dom:get-attribute
						       (xpath-single-child-elem-by-qname
							(elt scope-nodes 0) *xtm2.0-ns* "topicRef")
						        "href"))))
			       (is (= (length type-nodes) 1))
			       (is (string= t50a-psi (get-subjectIdentifier-by-ref
						      document
						      (dom:get-attribute
						       (xpath-single-child-elem-by-qname
							(elt type-nodes 0) *xtm2.0-ns* "topicRef")
						       "href"))))
			       (let ((variant-node
				      (let ((variant-nodes (xpath-child-elems-by-qname name *xtm2.0-ns* "variant")))
					(is (= (length variant-nodes) 1))
					(elt variant-nodes 0))))
				 (check-variant document variant-node t101-variant-itemIdentities
						(list core-sort-psi t50a-psi)
						(list :data t101-variant-name :type string-type) nil)))
			  when (not (or (string= t101-name-1
						 (xpath-fn-string
						  (xpath-single-child-elem-by-qname name *xtm2.0-ns* "value")))
					(string= t101-name-2
						 (xpath-fn-string
						  (xpath-single-child-elem-by-qname name *xtm2.0-ns* "value")))))
			  do (is-true (format t "bad name value in topic found: ~A~%"
					      (xpath-single-child-elem-by-qname name *xtm2.0-ns* "value"))))
		       (check-single-instanceOf document topic t3a-psi)
		       (check-occurrences-type document topic (list t51-psi t53-psi t54-psi t55-psi))
		       (loop for occurrence across (xpath-child-elems-by-qname topic *xtm2.0-ns* "occurrence")
			  do (let ((type-nodes (xpath-child-elems-by-qname occurrence *xtm2.0-ns* "type")))
			       (is (= (length type-nodes) 1))
			       (let ((topicRef-nodes
				      (xpath-child-elems-by-qname (elt type-nodes 0) *xtm2.0-ns* "topicRef")))
				 (is (= (length topicRef-nodes) 1))
				 (let ((t-href (get-subjectIdentifier-by-ref
						document
						(dom:get-attribute (elt topicRef-nodes 0) "href"))))
				   (cond
				     ((string= t-href t51-psi)
				      (let ((resourceRef-nodes
					     (xpath-child-elems-by-qname occurrence *xtm2.0-ns* "resourceRef")))
					(is (= (length resourceRef-nodes) 1))
					(is (string= t52-psi
					     (get-subjectIdentifier-by-ref
					      document
					      (dom:get-attribute (elt resourceRef-nodes 0) "href"))))))
				     ((string= t-href t53-psi)
				      (let ((resourceData-nodes
					     (xpath-child-elems-by-qname occurrence *xtm2.0-ns* "resourceData")))
					(is (= (length resourceData-nodes) 1))))				
				     ((string= t-href t54-psi)
				      (let ((resourceData-nodes
					     (xpath-child-elems-by-qname occurrence *xtm2.0-ns* "resourceData")))
					(is (= (length resourceData-nodes) 1))
					(is (string= (getf t101-occurrence-3-resourceData :data)
						     (xpath-fn-string (elt resourceData-nodes 0))))
					(is (string= (getf t101-occurrence-3-resourceData :type)
						     (dom:get-attribute (elt resourceData-nodes 0) "datatype")))))
				     ((string= t-href t55-psi)
				      (let ((resourceRef-nodes
					     (xpath-child-elems-by-qname occurrence *xtm2.0-ns* "resourceRef")))
					(is (= (length resourceRef-nodes) 1))
					(is (string= (dom:get-attribute (elt resourceRef-nodes 0) "href")
						     t101-occurrence-4-resourceRef))))
				     (t
				      (is-true (format t "bad occurrence tpye found: ~A" t-href)))))))))
		      ((string= href t202-psi)
		       (check-topic-id topic)
		       (is (= (length (xpath-child-elems-by-qname topic *xtm2.0-ns* "itemIdentity")) 0))
		       (check-single-name-value topic t202-name)
		       (check-single-instanceOf document topic t4-psi))
		      ((string= href t203-psi)
		       (check-topic-id topic)
		       (is (= (length (xpath-child-elems-by-qname topic *xtm2.0-ns* "itemIdentity")) 0))
		       (check-single-name-value topic t203-name)
		       (check-single-instanceOf document topic t4-psi))
		      ((string= href t300-psi)
		       (check-topic-id topic)
		       (is (= (length (xpath-child-elems-by-qname topic *xtm2.0-ns* "itemIdentity")) 0))
		       (check-single-name-value topic t300-name)
		       (check-single-instanceOf document topic t2-psi)
		       (loop for occurrence across (xpath-child-elems-by-qname topic *xtm2.0-ns* "occurrence")
			  do (let ((type-nodes (xpath-child-elems-by-qname occurrence *xtm2.0-ns* "type")))
			       (is (= (length type-nodes) 1))
			       (let ((topicRef-nodes
				      (xpath-child-elems-by-qname (elt type-nodes 0) *xtm2.0-ns* "topicRef")))
				 (is (= (length topicRef-nodes) 1))
				 (let ((t-href (get-subjectIdentifier-by-ref
						document
						(dom:get-attribute (elt topicRef-nodes 0) "href"))))
				   (cond
				     ((string= t-href t53-psi)
				      (let ((resourceData-nodes
					     (xpath-child-elems-by-qname occurrence *xtm2.0-ns* "resourceData")))
					(is (= (length resourceData-nodes) 1))
					(is (string= t300-occurrence-1-resourceData
						     (xpath-fn-string (elt resourceData-nodes 0))))
					(is (string= (dom:get-attribute (elt resourceData-nodes 0) "datatype")
						     string-type))))
				     ((string= t-href t55-psi)
				      (let ((resourceRef-nodes
					     (xpath-child-elems-by-qname occurrence *xtm2.0-ns* "resourceRef")))
					(is (= (length resourceRef-nodes) 1))
					(is (string= (dom:get-attribute (elt resourceRef-nodes 0) "href")
						     t300-occurrence-2-resourceRef))))
				     (t
				      (is-true (format t "bad occurrence tpye found: ~A" t-href))))))))))))))))


;; === tests the export to a xtm2.0 file from a certain revision ===============
(test test-exporter-xtm2.0-versions-2
  (with-fixture merge-test-db ()
    (handler-case (delete-file *out-xtm2.0-file*)(error () )) ;deletes file - if exist
    (export-xtm *out-xtm2.0-file* :revision fixtures::revision2)
    (let ((document (dom:document-element (cxml:parse-file *out-xtm2.0-file* (cxml-dom:make-dom-builder)))))
      (check-document-structure document 48 7)
      (loop for topic across (xpath-child-elems-by-qname document *xtm2.0-ns* "topic")
	 do (loop for subjectIdentifier across (xpath-child-elems-by-qname topic *xtm2.0-ns* "subjectIdentifier")
	       do (let ((href (dom:get-attribute subjectIdentifier "href")))
		    (cond
		      ((string= href t1-psi)
		       (check-topic-id topic)
		       (check-itemIdentities topic (list t1-itemIdentity t1-itemIdentity-merge-1))
		       (check-single-name-value topic t1-name))
		      ((string= href t2-psi)
		       (check-topic-id topic)
		       (check-itemIdentities topic (list t2-itemIdentity t2-itemIdentity-merge-1))
		       (check-single-name-value topic t2-name)
		       (check-single-instanceOf document topic t1-psi))
		      ((string= href t3-psi)
		       (check-topic-id topic)
		       (check-itemIdentities topic (list t3-itemIdentity t3-itemIdentity-merge-1))
		       (check-single-name-value topic t3-name)
		       (check-single-instanceOf document topic t1-psi))
		      ((string= href t3a-psi)
		       (check-topic-id topic)
		       (check-itemIdentities topic (list t3a-itemIdentity t3a-itemIdentity-merge-1))
		       (check-single-name-value topic t3a-name)
		       (check-psis topic (list t3a-psi t3a-psi-merge-1))
		       (check-single-instanceOf document topic t3-psi))
		      ((string= href t50-psi)
		       (check-topic-id topic)
		       (check-itemIdentities topic (list t50-itemIdentity))
		       (check-single-name-value topic t50-name)
		       (check-single-instanceOf document topic t1-psi))
		      ((string= href t50a-psi)
		       (check-topic-id topic)
		       (check-itemIdentities topic (list t50a-itemIdentity))
		       (check-single-name-value topic t50a-name)
		       (check-single-instanceOf document topic t6-psi)
		       (let ((variant-node
			      (let ((variant-nodes
				     (xpath-child-elems-by-qname 
				      (xpath-single-child-elem-by-qname topic *xtm2.0-ns* "name")
				      *xtm2.0-ns* "variant")))
				(is (= (length variant-nodes) 1))
				(elt variant-nodes 0))))
			 (check-variant document variant-node nil (list core-sort-psi)
					(list :data t50a-variant-name :type string-type) nil)))
		      ((string= href t55-psi)
		       (check-topic-id topic)
		       (check-itemIdentities topic (list t55-itemIdentity t55-itemIdentity-merge-1))
		       (check-single-name-value topic t55-name)
		       (check-single-instanceOf document topic t6-psi))
		      ((string= href t57-psi)
		       (check-topic-id topic)
		       (check-itemIdentities topic (list t57-itemIdentity))
		       (check-single-name-value topic t57-name)
		       (check-single-instanceOf document topic t7-psi))
		      ((string= href t58-psi)
		       (check-topic-id topic)
		       (check-itemIdentities topic (list t58-itemIdentity))
		       (check-single-name-value topic t58-name)
		       (check-single-instanceOf document topic t8-psi))
		      ((string= href t59-psi)
		       (check-topic-id topic)
		       (check-itemIdentities topic (list t59-itemIdentity))
		       (check-single-name-value topic t59-name)
		       (check-single-instanceOf document topic t8-psi))
		      ((string= href t62-psi)
		       (check-topic-id topic)
		       (check-itemIdentities topic (list t62-itemIdentity))
		       (check-single-name-value topic t62-name)
		       (check-single-instanceOf document topic t8-psi))
		      ((string= href t64-psi)
		       (check-topic-id topic)
		       (check-itemIdentities topic (list t64-itemIdentity))
		       (check-single-name-value topic t64-name)
		       (check-single-instanceOf document topic t7-psi))
		      ((string= href t100-psi)
		       (check-topic-id topic)
		       (let ((name-nodes (xpath-child-elems-by-qname topic *xtm2.0-ns* "name")))
			 (is (= (length name-nodes) 2))
			 (loop for name-node across name-nodes
			    do (let ((name (xpath-fn-string
					    (xpath-single-child-elem-by-qname name-node *xtm2.0-ns* "value"))))
				 (cond
				   ((string= name t100-name)
				    (let ((itemIdentity-nodes
					   (xpath-child-elems-by-qname name-node *xtm2.0-ns* "itemIdentity")))
				      (is (= (length itemIdentity-nodes) 1))
				      (is (string= (dom:get-attribute (elt itemIdentity-nodes 0) "href")
						   t100-name-itemIdentity)))
				    (let ((variant-nodes (xpath-child-elems-by-qname 
							  (xpath-single-child-elem-by-qname topic *xtm2.0-ns* "name")
							  *xtm2.0-ns* "variant")))
				      (is (= (length variant-nodes) 2))
				      (loop for variant across variant-nodes
					 do (let ((resourceRef
						   (xpath-fn-string
						    (xpath-single-child-elem-by-qname
						     variant *xtm2.0-ns* "resourceData"))))
					      (cond
						((string= resourceRef t100-variant-1-name)
						 (check-variant document variant (list t100-variant-1-itemIdentity)
								(list core-display-psi)
								(list :data t100-variant-1-name :type string-type) nil))
						((string= resourceRef t100-variant-2-name)
						 (check-variant document variant (list t100-variant-2-itemIdentity)
								(list core-sort-psi)
								(list :data t100-variant-2-name :type string-type) nil))
						(t
						 (is-true
						  (format t "bad resourceRef oin variant of t100 found: ~A~%"
							  resourceRef))))))))
				   ((string= name t100-name-merge-1)
				    (is-true t))
				   (t
				    (is-true (format t "bad name value found in t100: ~A~%" name)))))))
		       (check-itemIdentities topic (list t100-itemIdentity t100-itemIdentity-merge-1))
		       (check-single-instanceOf document topic t3a-psi)
		       (check-occurrences-itemIdentity topic t100-occurrence-itemIdentities-merge-1)
		       (check-occurrences-type document topic (list t51-psi t53-psi t54-psi t55-psi t55-psi))
		       (check-occurrences-resourceRef topic t100-occurrences-resourceRef-merge-1)
		       (check-occurrences-resourceData topic t100-occurrences-resourceData))
		      ((string= href new-t100-psi)
		       (check-topic-id topic)
		       (check-itemIdentities topic (list new-t100-itemIdentity))
		       (check-single-name-value topic new-t100-name)
		       (check-itemIdentities
			(xpath-single-child-elem-by-qname topic *xtm2.0-ns* "name")
			new-t100-name-itemIdentities)
		       (let ((occurrence-nodes (xpath-child-elems-by-qname topic *xtm2.0-ns* "occurrence")))
			 (is (= (length occurrence-nodes) 1))
			 (check-itemIdentities (elt occurrence-nodes 0) (list new-t100-occurrence-1-itemIdentity))
			 (let ((type-nodes (xpath-child-elems-by-qname (elt occurrence-nodes 0) *xtm2.0-ns* "type")))
			   (is (= (length type-nodes) 1))
			   (let ((topicRef-nodes
				  (xpath-child-elems-by-qname (elt type-nodes 0) *xtm2.0-ns* "topicRef")))
			     (is (= (length topicRef-nodes) 1))
			     (is (string= t55-psi
					  (get-subjectIdentifier-by-ref
					   document
					   (dom:get-attribute (elt topicRef-nodes 0) "href")))))))
		       (check-occurrences-resourceRef topic (list new-t100-occurrence-resourceRef))
		       (let ((variant-node
			      (let ((variant-nodes
				     (xpath-child-elems-by-qname
				      (xpath-single-child-elem-by-qname topic *xtm2.0-ns* "name")
				      *xtm2.0-ns* "variant")))					
				(is (= (length variant-nodes) 1))
				(elt variant-nodes 0))))
			 (check-variant document variant-node (list new-t100-variant-itemIdentity)
					(list core-sort-psi t50a-psi)
					(list :data new-t100-variant-name :type string-type)
					nil)))
		      ((string= href t101-psi)
		       (check-topic-id topic)
		       (is (= (length (xpath-child-elems-by-qname topic *xtm2.0-ns* "itemIdentity")) 0))
		       (loop for name across (xpath-child-elems-by-qname topic *xtm2.0-ns* "name")
			  when (string= t101-name-1 (xpath-fn-string
						     (xpath-single-child-elem-by-qname name *xtm2.0-ns* "value")))
			  do (is-true t)
			  when (string= t101-name-2 (xpath-fn-string
						     (xpath-single-child-elem-by-qname name *xtm2.0-ns* "value")))
			  do (let ((ii-nodes (xpath-child-elems-by-qname name *xtm2.0-ns* "itemIdentity"))
				   (scope-nodes (xpath-child-elems-by-qname name *xtm2.0-ns* "scope"))
				   (type-nodes (xpath-child-elems-by-qname name *xtm2.0-ns* "type")))
			       (is (= (length ii-nodes) 1))
			       (is (string= t101-name-2-itemIdentity (dom:get-attribute (elt ii-nodes 0) "href")))
			       (is (= (length scope-nodes) 1))
			       (is (string= t50a-psi (get-subjectIdentifier-by-ref
						      document
						      (dom:get-attribute
						       (xpath-single-child-elem-by-qname
							(elt scope-nodes 0) *xtm2.0-ns* "topicRef")
						        "href"))))
			       (is (= (length type-nodes) 1))
			       (is (string= t50a-psi (get-subjectIdentifier-by-ref
						      document
						      (dom:get-attribute
						       (xpath-single-child-elem-by-qname
							(elt type-nodes 0) *xtm2.0-ns* "topicRef")
						       "href"))))
			       (let ((variant-node
				      (let ((variant-nodes (xpath-child-elems-by-qname name *xtm2.0-ns* "variant")))
					(is (= (length variant-nodes) 1))
					(elt variant-nodes 0))))
				 (check-variant document variant-node t101-variant-itemIdentities
						(list core-sort-psi t50a-psi)
						(list :data t101-variant-name :type string-type) nil)))
			  when (not (or (string= t101-name-1
						 (xpath-fn-string
						  (xpath-single-child-elem-by-qname name *xtm2.0-ns* "value")))
					(string= t101-name-2
						 (xpath-fn-string
						  (xpath-single-child-elem-by-qname name *xtm2.0-ns* "value")))))
			  do (is-true (format t "bad name value in topic found: ~A~%"
					      (xpath-single-child-elem-by-qname name *xtm2.0-ns* "value"))))
		       (check-single-instanceOf document topic t3a-psi)
		       (check-occurrences-type document topic (list t51-psi t53-psi t54-psi t55-psi))
		       (loop for occurrence across (xpath-child-elems-by-qname topic *xtm2.0-ns* "occurrence")
			  do (let ((type-nodes (xpath-child-elems-by-qname occurrence *xtm2.0-ns* "type")))
			       (is (= (length type-nodes) 1))
			       (let ((topicRef-nodes
				      (xpath-child-elems-by-qname (elt type-nodes 0) *xtm2.0-ns* "topicRef")))
				 (is (= (length topicRef-nodes) 1))
				 (let ((t-href (get-subjectIdentifier-by-ref
						document
						(dom:get-attribute (elt topicRef-nodes 0) "href"))))
				   (cond
				     ((string= t-href t51-psi)
				      (let ((resourceRef-nodes
					     (xpath-child-elems-by-qname occurrence *xtm2.0-ns* "resourceRef")))
					(is (= (length resourceRef-nodes) 1))
					(is (string= t52-psi
					     (get-subjectIdentifier-by-ref
					      document
					      (dom:get-attribute (elt resourceRef-nodes 0) "href"))))))
				     ((string= t-href t53-psi)
				      (let ((resourceData-nodes
					     (xpath-child-elems-by-qname occurrence *xtm2.0-ns* "resourceData")))
					(is (= (length resourceData-nodes) 1))))				
				     ((string= t-href t54-psi)
				      (let ((resourceData-nodes
					     (xpath-child-elems-by-qname occurrence *xtm2.0-ns* "resourceData")))
					(is (= (length resourceData-nodes) 1))
					(is (string= (getf t101-occurrence-3-resourceData :data)
						     (xpath-fn-string (elt resourceData-nodes 0))))
					(is (string= (getf t101-occurrence-3-resourceData :type)
						     (dom:get-attribute (elt resourceData-nodes 0) "datatype")))))
				     ((string= t-href t55-psi)
				      (let ((resourceRef-nodes
					     (xpath-child-elems-by-qname occurrence *xtm2.0-ns* "resourceRef")))
					(is (= (length resourceRef-nodes) 1))
					(is (string= (dom:get-attribute (elt resourceRef-nodes 0) "href")
						     t101-occurrence-4-resourceRef))))
				     (t
				      (is-true (format t "bad occurrence tpye found: ~A" t-href)))))))))
		      ((string= href t202-psi)
		       (check-topic-id topic)
		       (is (= (length (xpath-child-elems-by-qname topic *xtm2.0-ns* "itemIdentity")) 0))
		       (check-single-name-value topic t202-name)
		       (check-single-instanceOf document topic t4-psi))
		      ((string= href t203-psi)
		       (check-topic-id topic)
		       (is (= (length (xpath-child-elems-by-qname topic *xtm2.0-ns* "itemIdentity")) 0))
		       (check-single-name-value topic t203-name)
		       (check-single-instanceOf document topic t4-psi))
		      ((string= href t300-psi)
		       (check-topic-id topic)
		       (is (= (length (xpath-child-elems-by-qname topic *xtm2.0-ns* "itemIdentity")) 0))
		       (check-single-name-value topic t300-name)
		       (check-single-instanceOf document topic t2-psi)
		       (loop for occurrence across (xpath-child-elems-by-qname topic *xtm2.0-ns* "occurrence")
			  do (let ((type-nodes (xpath-child-elems-by-qname occurrence *xtm2.0-ns* "type")))
			       (is (= (length type-nodes) 1))
			       (let ((topicRef-nodes
				      (xpath-child-elems-by-qname (elt type-nodes 0) *xtm2.0-ns* "topicRef")))
				 (is (= (length topicRef-nodes) 1))
				 (let ((t-href (get-subjectIdentifier-by-ref
						document
						(dom:get-attribute (elt topicRef-nodes 0) "href"))))
				   (cond
				     ((string= t-href t53-psi)
				      (let ((resourceData-nodes
					     (xpath-child-elems-by-qname occurrence *xtm2.0-ns* "resourceData")))
					(is (= (length resourceData-nodes) 1))
					(is (string= t300-occurrence-1-resourceData
						     (xpath-fn-string (elt resourceData-nodes 0))))
					(is (string= (dom:get-attribute (elt resourceData-nodes 0) "datatype")
						     string-type))))
				     ((string= t-href t55-psi)
				      (let ((resourceRef-nodes
					     (xpath-child-elems-by-qname occurrence *xtm2.0-ns* "resourceRef")))
					(is (= (length resourceRef-nodes) 1))
					(is (string= (dom:get-attribute (elt resourceRef-nodes 0) "href")
						     t300-occurrence-2-resourceRef))))
				     (t
				      (is-true (format t "bad occurrence tpye found: ~A" t-href))))))))))))))))


;; === tests the export to a xtm2.0 file from a certain revision ===============
(test test-exporter-xtm2.0-versions-3
  (with-fixture merge-test-db ()
    (handler-case (delete-file *out-xtm2.0-file*)(error () )) ;deletes file - if exist
    (export-xtm *out-xtm2.0-file* :revision fixtures::revision3)
    (let ((document (dom:document-element (cxml:parse-file *out-xtm2.0-file* (cxml-dom:make-dom-builder)))))
      (check-document-structure document 48 8)
      (loop for topic across (xpath-child-elems-by-qname document *xtm2.0-ns* "topic")
	 do (loop for subjectIdentifier across (xpath-child-elems-by-qname topic *xtm2.0-ns* "subjectIdentifier")
	       do (let ((href (dom:get-attribute subjectIdentifier "href")))
		    (cond
		      ((string= href t1-psi)
		       (check-topic-id topic)
		       (check-itemIdentities topic (list t1-itemIdentity t1-itemIdentity-merge-1))
		       (check-single-name-value topic t1-name))
		      ((string= href t2-psi)
		       (check-topic-id topic)
		       (check-itemIdentities topic (list t2-itemIdentity t2-itemIdentity-merge-1))
		       (check-single-name-value topic t2-name)
		       (check-single-instanceOf document topic t1-psi))
		      ((string= href t3-psi)
		       (check-topic-id topic)
		       (check-itemIdentities topic (list t3-itemIdentity t3-itemIdentity-merge-1))
		       (check-single-name-value topic t3-name)
		       (check-single-instanceOf document topic t1-psi))
		      ((string= href t3a-psi)
		       (check-topic-id topic)
		       (check-itemIdentities topic (list t3a-itemIdentity t3a-itemIdentity-merge-1))
		       (check-single-name-value topic t3a-name)
		       (check-psis topic (list t3a-psi t3a-psi-merge-1 t3a-psi-merge-2))
		       (check-single-instanceOf document topic t3-psi))
		      ((string= href t50-psi)
		       (check-topic-id topic)
		       (check-itemIdentities topic (list t50-itemIdentity))
		       (check-single-name-value topic t50-name)
		       (check-single-instanceOf document topic t1-psi))
		      ((string= href t50a-psi)
		       (check-topic-id topic)
		       (check-itemIdentities topic (list t50a-itemIdentity))
		       (check-single-name-value topic t50a-name)
		       (check-single-instanceOf document topic t6-psi)
		       (let ((variant-node
			      (let ((variant-nodes
				     (xpath-child-elems-by-qname 
				      (xpath-single-child-elem-by-qname topic *xtm2.0-ns* "name")
				      *xtm2.0-ns* "variant")))
				(is (= (length variant-nodes) 1))
				(elt variant-nodes 0))))
			 (check-variant document variant-node nil (list core-sort-psi)
					(list :data t50a-variant-name :type string-type) nil)))
		      ((string= href t55-psi)
		       (check-topic-id topic)
		       (check-itemIdentities topic (list t55-itemIdentity t55-itemIdentity-merge-1))
		       (check-single-name-value topic t55-name)
		       (check-single-instanceOf document topic t6-psi))
		      ((string= href t57-psi)
		       (check-topic-id topic)
		       (check-itemIdentities topic (list t57-itemIdentity))
		       (check-single-name-value topic t57-name)
		       (check-single-instanceOf document topic t7-psi))
		      ((string= href t58-psi)
		       (check-topic-id topic)
		       (check-itemIdentities topic (list t58-itemIdentity))
		       (check-single-name-value topic t58-name)
		       (check-single-instanceOf document topic t8-psi))
		      ((string= href t59-psi)
		       (check-topic-id topic)
		       (check-itemIdentities topic (list t59-itemIdentity))
		       (check-single-name-value topic t59-name)
		       (check-single-instanceOf document topic t8-psi))
		      ((string= href t62-psi)
		       (check-topic-id topic)
		       (check-itemIdentities topic (list t62-itemIdentity))
		       (check-single-name-value topic t62-name)
		       (check-single-instanceOf document topic t8-psi))
		      ((string= href t64-psi)
		       (check-topic-id topic)
		       (check-itemIdentities topic (list t64-itemIdentity))
		       (check-single-name-value topic t64-name)
		       (check-single-instanceOf document topic t7-psi))
		      ((string= href t100-psi)
		       (check-topic-id topic)
		       (let ((name-nodes (xpath-child-elems-by-qname topic *xtm2.0-ns* "name")))
			 (is (= (length name-nodes) 2))
			 (loop for name-node across name-nodes
			    do (let ((name
				      (xpath-fn-string
				       (xpath-single-child-elem-by-qname name-node *xtm2.0-ns* "value"))))
				 (cond
				   ((string= name t100-name)
				    (let ((itemIdentity-nodes
					   (xpath-child-elems-by-qname name-node *xtm2.0-ns* "itemIdentity")))
				      (is (= (length itemIdentity-nodes) 1))
				      (is (string= (dom:get-attribute (elt itemIdentity-nodes 0) "href")
						   t100-name-itemIdentity)))
				    (let ((variant-nodes (xpath-child-elems-by-qname 
							  (xpath-single-child-elem-by-qname topic *xtm2.0-ns* "name")
							  *xtm2.0-ns* "variant")))
				      (is (= (length variant-nodes) 2))
				      (loop for variant across variant-nodes
					 do (let ((resourceRef
						   (xpath-fn-string
						    (xpath-single-child-elem-by-qname
						     variant *xtm2.0-ns* "resourceData"))))
					      (cond
						((string= resourceRef t100-variant-1-name)
						 (check-variant document variant (list t100-variant-1-itemIdentity)
								(list core-display-psi)
								(list :data t100-variant-1-name :type string-type) nil))
						((string= resourceRef t100-variant-2-name)
						 (check-variant document variant (list t100-variant-2-itemIdentity)
								(list core-sort-psi)
								(list :data t100-variant-2-name :type string-type) nil))
						(t
						 (is-true
						  (format t "bad resourceRef oin variant of t100 found: ~A~%"
							  resourceRef))))))))
				   ((string= name t100-name-merge-1)
				    (is-true t))
				   (t
				    (is-true (format t "bad name value found in t100: ~A~%" name)))))))
		       (check-itemIdentities topic (list t100-itemIdentity t100-itemIdentity-merge-1))
		       (check-single-instanceOf document topic t3a-psi)
		       (check-occurrences-itemIdentity topic t100-occurrence-itemIdentities-merge-1)
		       (check-occurrences-type document topic (list t51-psi t53-psi t54-psi t55-psi t55-psi))
		       (check-occurrences-resourceRef topic t100-occurrences-resourceRef-merge-1)
		       (check-occurrences-resourceData topic t100-occurrences-resourceData))
		      ((string= href new-t100-psi)
		       (check-topic-id topic)
		       (check-itemIdentities topic (list new-t100-itemIdentity new-t100-itemIdentity-merge-2))
		       (check-single-name-value topic new-t100-name)
		       (check-itemIdentities
			(xpath-single-child-elem-by-qname topic *xtm2.0-ns* "name")
			new-t100-name-itemIdentities)
		       (loop for occurrence across (xpath-child-elems-by-qname topic *xtm2.0-ns* "occurrence")
			  do (let ((resourceRef
				    (let ((resourceRef-nodes
					   (xpath-child-elems-by-qname occurrence *xtm2.0-ns* "resourceRef")))
				      (is (= (length resourceRef-nodes) 1))
				      (dom:get-attribute (elt resourceRef-nodes 0) "href"))))
			       (let ((occurrence-type
				      (let ((type-nodes
					     (xpath-child-elems-by-qname occurrence *xtm2.0-ns* "type")))
					(is (= (length type-nodes) 1))
					(let ((topicRef-nodes
					       (xpath-child-elems-by-qname (elt type-nodes 0)
									   *xtm2.0-ns* "topicRef")))
					  (is (= (length topicRef-nodes) 1))
					  (get-subjectIdentifier-by-ref document
									(dom:get-attribute
									 (elt topicRef-nodes 0) "href"))))))
				 (is (string= occurrence-type t55-psi)))
			       (cond
				 ((string= resourceRef (first new-t100-occurrence-resourceRef-merge-2))
				  (check-itemIdentities occurrence (list new-t100-occurrence-1-itemIdentity)))
				 ((string= resourceRef (second new-t100-occurrence-resourceRef-merge-2))
				  (check-itemIdentities occurrence (list new-t100-occurrence-2-itemIdentity)))
				 (t
				  (is-true
				   (format t "bad resourceRef found in new-t00 occurrence: ~A~%" resourceRef))))))
		       (let ((variant-nodes
			      (xpath-child-elems-by-qname
			       (xpath-single-child-elem-by-qname topic *xtm2.0-ns* "name")
			       *xtm2.0-ns* "variant")))
			 (is (= (length variant-nodes) 2))
			 (loop for variant across variant-nodes
			    do (let ((resourceData
				      (xpath-fn-string
				       (xpath-single-child-elem-by-qname variant *xtm2.0-ns* "resourceData"))))
				 (cond
				   ((string= resourceData new-t100-variant-name)
				    (check-variant document variant
						   (list new-t100-variant-itemIdentity)
						   (list core-sort-psi t50a-psi)
						   (list :data new-t100-variant-name :type string-type)
						   nil))
				   ((string= resourceData new-t100-variant-name-merge-2)
				    (check-variant document variant 
						   (list new-t100-variant-itemIdentity-merge-2)
						   (list core-display-psi)
						   (list :data new-t100-variant-name-merge-2 :type string-type)
						   nil))
				   (t
				    (is-true
				     (format t "bad resourceData found in variant of new t100: ~A~%"
					     resourceData))))))))
		      ((string= href t101-psi)
		       (check-topic-id topic)
		       (is (= (length (xpath-child-elems-by-qname topic *xtm2.0-ns* "itemIdentity")) 0))
		       (loop for name across (xpath-child-elems-by-qname topic *xtm2.0-ns* "name")
			  when (string= t101-name-1 (xpath-fn-string
						     (xpath-single-child-elem-by-qname name *xtm2.0-ns* "value")))
			  do (is-true t)
			  when (string= t101-name-2 (xpath-fn-string
						     (xpath-single-child-elem-by-qname name *xtm2.0-ns* "value")))
			  do (let ((ii-nodes (xpath-child-elems-by-qname name *xtm2.0-ns* "itemIdentity"))
				   (scope-nodes (xpath-child-elems-by-qname name *xtm2.0-ns* "scope"))
				   (type-nodes (xpath-child-elems-by-qname name *xtm2.0-ns* "type")))
			       (is (= (length ii-nodes) 1))
			       (is (string= t101-name-2-itemIdentity (dom:get-attribute (elt ii-nodes 0) "href")))
			       (is (= (length scope-nodes) 1))
			       (is (string= t50a-psi (get-subjectIdentifier-by-ref
						      document
						      (dom:get-attribute
						       (xpath-single-child-elem-by-qname
							(elt scope-nodes 0) *xtm2.0-ns* "topicRef")
						        "href"))))
			       (is (= (length type-nodes) 1))
			       (is (string= t50a-psi (get-subjectIdentifier-by-ref
						      document
						      (dom:get-attribute
						       (xpath-single-child-elem-by-qname
							(elt type-nodes 0) *xtm2.0-ns* "topicRef")
						       "href"))))
			       (let ((variant-node
				      (let ((variant-nodes (xpath-child-elems-by-qname name *xtm2.0-ns* "variant")))
					(is (= (length variant-nodes) 1))
					(elt variant-nodes 0))))
				 (check-variant document variant-node t101-variant-itemIdentities
						(list core-sort-psi t50a-psi)
						(list :data t101-variant-name :type string-type) nil)))
			  when (not (or (string= t101-name-1
						 (xpath-fn-string
						  (xpath-single-child-elem-by-qname name *xtm2.0-ns* "value")))
					(string= t101-name-2
						 (xpath-fn-string
						  (xpath-single-child-elem-by-qname name *xtm2.0-ns* "value")))))
			  do (is-true (format t "bad name value in topic found: ~A~%"
					      (xpath-single-child-elem-by-qname name *xtm2.0-ns* "value"))))
		       (check-single-instanceOf document topic t3a-psi)
		       (check-occurrences-type document topic (list t51-psi t53-psi t54-psi t55-psi))
		       (loop for occurrence across (xpath-child-elems-by-qname topic *xtm2.0-ns* "occurrence")
			  do (let ((type-nodes (xpath-child-elems-by-qname occurrence *xtm2.0-ns* "type")))
			       (is (= (length type-nodes) 1))
			       (let ((topicRef-nodes
				      (xpath-child-elems-by-qname (elt type-nodes 0) *xtm2.0-ns* "topicRef")))
				 (is (= (length topicRef-nodes) 1))
				 (let ((t-href (get-subjectIdentifier-by-ref
						document
						(dom:get-attribute (elt topicRef-nodes 0) "href"))))
				   (cond
				     ((string= t-href t51-psi)
				      (let ((resourceRef-nodes
					     (xpath-child-elems-by-qname occurrence *xtm2.0-ns* "resourceRef")))
					(is (= (length resourceRef-nodes) 1))
					(is (string= t52-psi
					     (get-subjectIdentifier-by-ref
					      document
					      (dom:get-attribute (elt resourceRef-nodes 0) "href"))))))
				     ((string= t-href t53-psi)
				      (let ((resourceData-nodes
					     (xpath-child-elems-by-qname occurrence *xtm2.0-ns* "resourceData")))
					(is (= (length resourceData-nodes) 1))))				
				     ((string= t-href t54-psi)
				      (let ((resourceData-nodes
					     (xpath-child-elems-by-qname occurrence *xtm2.0-ns* "resourceData")))
					(is (= (length resourceData-nodes) 1))
					(is (string= (getf t101-occurrence-3-resourceData :data)
						     (xpath-fn-string (elt resourceData-nodes 0))))
					(is (string= (getf t101-occurrence-3-resourceData :type)
						     (dom:get-attribute (elt resourceData-nodes 0) "datatype")))))
				     ((string= t-href t55-psi)
				      (let ((resourceRef-nodes
					     (xpath-child-elems-by-qname occurrence *xtm2.0-ns* "resourceRef")))
					(is (= (length resourceRef-nodes) 1))
					(is (string= (dom:get-attribute (elt resourceRef-nodes 0) "href")
						     t101-occurrence-4-resourceRef))))
				     (t
				      (is-true (format t "bad occurrence tpye found: ~A" t-href)))))))))
		      ((string= href t202-psi)
		       (check-topic-id topic)
		       (is (= (length (xpath-child-elems-by-qname topic *xtm2.0-ns* "itemIdentity")) 0))
		       (check-single-name-value topic t202-name)
		       (check-single-instanceOf document topic t4-psi))
		      ((string= href t203-psi)
		       (check-topic-id topic)
		       (is (= (length (xpath-child-elems-by-qname topic *xtm2.0-ns* "itemIdentity")) 0))
		       (check-single-name-value topic t203-name)
		       (check-single-instanceOf document topic t4-psi))
		      ((string= href t300-psi)
		       (check-topic-id topic)
		       (is (= (length (xpath-child-elems-by-qname topic *xtm2.0-ns* "itemIdentity")) 0))
		       (check-single-name-value topic t300-name)
		       (check-single-instanceOf document topic t2-psi)
		       (loop for occurrence across (xpath-child-elems-by-qname topic *xtm2.0-ns* "occurrence")
			  do (let ((type-nodes (xpath-child-elems-by-qname occurrence *xtm2.0-ns* "type")))
			       (is (= (length type-nodes) 1))
			       (let ((topicRef-nodes
				      (xpath-child-elems-by-qname (elt type-nodes 0) *xtm2.0-ns* "topicRef")))
				 (is (= (length topicRef-nodes) 1))
				 (let ((t-href (get-subjectIdentifier-by-ref
						document
						(dom:get-attribute (elt topicRef-nodes 0) "href"))))
				   (cond
				     ((string= t-href t53-psi)
				      (let ((resourceData-nodes
					     (xpath-child-elems-by-qname occurrence *xtm2.0-ns* "resourceData")))
					(is (= (length resourceData-nodes) 1))
					(is (string= t300-occurrence-1-resourceData
						     (xpath-fn-string (elt resourceData-nodes 0))))
					(is (string= (dom:get-attribute (elt resourceData-nodes 0) "datatype")
						     string-type))))
				     ((string= t-href t55-psi)
				      (let ((resourceRef-nodes
					     (xpath-child-elems-by-qname occurrence *xtm2.0-ns* "resourceRef")))
					(is (= (length resourceRef-nodes) 1))
					(is (string= (dom:get-attribute (elt resourceRef-nodes 0) "href")
						     t300-occurrence-2-resourceRef))))
				     (t
				      (is-true (format t "bad occurrence tpye found: ~A" t-href)))))))))))))
      (let ((matched-associations 0))
	(loop for association across (xpath-child-elems-by-qname document *xtm2.0-ns* "association")
	   when (handler-case
		    (string= assoc-6-itemIdentity
			     (dom:get-attribute
			      (xpath-single-child-elem-by-qname association *xtm2.0-ns* "itemIdentity")
			      "href"))
		  (error () nil))	     
	   do (let ((itemIdentity (dom:get-attribute
				   (xpath-single-child-elem-by-qname association *xtm2.0-ns* "itemIdentity")
				   "href"))
		    (association-type
		     (let ((type-nodes (xpath-child-elems-by-qname association *xtm2.0-ns* "type")))
		       (is (= (length type-nodes) 1))
		       (let ((topicRef-nodes
			      (xpath-child-elems-by-qname (elt type-nodes 0) *xtm2.0-ns* "topicRef")))
			 (is (= (length topicRef-nodes) 1))
			 (get-subjectIdentifier-by-ref document
						       (dom:get-attribute (elt topicRef-nodes 0) "href"))))))
		(loop for role across (xpath-child-elems-by-qname association *xtm2.0-ns* "role")
		   do (let ((role-type
			     (let ((role-type-nodes
				    (xpath-child-elems-by-qname role *xtm2.0-ns* "type")))
			       (is (= (length role-type-nodes) 1))
			       (let ((topicRef-nodes
				      (xpath-child-elems-by-qname (elt role-type-nodes 0) *xtm2.0-ns* "topicRef")))
				 (get-subjectIdentifier-by-ref document
							       (dom:get-attribute (elt topicRef-nodes 0) "href")))))
			    (role-player
			     (let ((topicRef-nodes
				    (xpath-child-elems-by-qname role *xtm2.0-ns* "topicRef")))
			       (is (= (length topicRef-nodes) 1))
			       (get-subjectIdentifier-by-ref document 
							     (dom:get-attribute (elt topicRef-nodes 0) "href")))))
			(cond
			  ((string= role-type t63-psi)
			   (check-itemIdentities role (list assoc-6-role-1-itemIdentity))
			   (is (string= role-player t300-psi)))
			  ((string= role-type t62-psi)
			   (check-itemIdentities role assoc-6-role-2-itemIdentities)
			   (is (string= role-player t101-psi)))
			  (t
			   (is-true (format t "bad role-type found in association 6: ~A~%" role-type))))))
		(is (string= association-type t64-psi))
		(is (string= itemIdentity assoc-6-itemIdentity))
		(incf matched-associations)))
	(is (= matched-associations 1))))))



;; === tests the fragment of the topic new-t100 from notificationbase.xtm, =====
;; === notification_merge1.xtm and notification_merge2.xtm in the latest version
(test test-fragments-versions
  (with-fixture merge-test-db ()
    (handler-case (delete-file *out-xtm2.0-file*)(error () )) ;deletes file - if exist

    (let ((new-t100 (loop for item in (elephant:get-instances-by-class 'PersistentIdC)
		       when (string= (uri item) new-t100-psi)
		       return (identified-construct item))))

      (d:get-fragments fixtures::revision3)
      (let ((fragment (loop for item in (elephant:get-instances-by-class 'FragmentC)
			 when (eq (topic item) new-t100)
			 return item)))

	(with-open-file (stream *out-xtm2.0-file* :direction :output)
	  (write-string (export-xtm-fragment fragment) stream))))

    (let ((document (dom:document-element (cxml:parse-file *out-xtm2.0-file* (cxml-dom:make-dom-builder)))))
      (check-document-structure document 6 0)
      (loop for topic across (xpath-child-elems-by-qname document *xtm2.0-ns* "topic")
	 do (loop for subjectIdentifier across (xpath-child-elems-by-qname topic *xtm2.0-ns* "subjectIdentifier")
	       do (let ((href (dom:node-value (dom:get-attribute-node subjectIdentifier "href"))))
		    (cond
		      ((string= href core-sort-psi)
		       (check-topic-id topic)
		       (check-itemIdentities topic nil))
		      ((string= href core-display-psi)
		       (check-topic-id topic)
		       (check-itemIdentities topic nil))
		      ((string= href t3-psi)
		       (check-topic-id topic)
		       (check-itemIdentities topic (list t3-itemIdentity t3-itemIdentity-merge-1)))
		      ((string= href t50a-psi)
		       (check-topic-id topic)
		       (check-itemIdentities topic (list t50a-itemIdentity)))
		      ((string= href t55-psi)
		       (check-topic-id topic)
		       (check-itemIdentities topic (list t55-itemIdentity t55-itemIdentity-merge-1)))
		      ((string= href new-t100-psi)
		       (check-topic-id topic)
		       (check-itemIdentities topic (list new-t100-itemIdentity new-t100-itemIdentity-merge-2))
		       (check-single-instanceOf document topic t3-psi)
		       (let ((name-nodes (xpath-child-elems-by-qname topic *xtm2.0-ns* "name")))
			 (is (= (length name-nodes) 1))
			 (check-itemIdentities (elt name-nodes 0) new-t100-name-itemIdentities)
			 (is (string= new-t100-name
				      (xpath-fn-string (xpath-single-child-elem-by-qname (elt name-nodes 0)
											 *xtm2.0-ns* "value")))))
		       (loop for occurrence across (xpath-child-elems-by-qname topic *xtm2.0-ns* "occurrence")
			  do (let ((resourceRef
				    (let ((resourceRef-nodes
					   (xpath-child-elems-by-qname occurrence *xtm2.0-ns* "resourceRef")))
				      (is (= (length resourceRef-nodes) 1))
				      (dom:get-attribute (elt resourceRef-nodes 0) "href"))))
			       (let ((occurrence-type
				      (let ((type-nodes
					     (xpath-child-elems-by-qname occurrence *xtm2.0-ns* "type")))
					(is (= (length type-nodes) 1))
					(let ((topicRef-nodes
					       (xpath-child-elems-by-qname (elt type-nodes 0)
									   *xtm2.0-ns* "topicRef")))
					  (is (= (length topicRef-nodes) 1))
					  (get-subjectIdentifier-by-ref document
									(dom:get-attribute
									 (elt topicRef-nodes 0) "href"))))))
				 (is (string= occurrence-type t55-psi)))
			       (cond
				 ((string= resourceRef (first new-t100-occurrence-resourceRef-merge-2))
				  (check-itemIdentities occurrence (list new-t100-occurrence-1-itemIdentity)))
				 ((string= resourceRef (second new-t100-occurrence-resourceRef-merge-2))
				  (check-itemIdentities occurrence (list new-t100-occurrence-2-itemIdentity)))
				 (t
				  (is-true
				   (format t "bad resourceRef found in new-t100 fragment: ~A~%" resourceRef))))))
		       (let ((variant-nodes
			      (xpath-child-elems-by-qname
			       (xpath-single-child-elem-by-qname topic *xtm2.0-ns* "name")
			       *xtm2.0-ns* "variant")))
			 (is (= (length variant-nodes) 2))
			 (loop for variant across variant-nodes
			    do (let ((resourceData
				      (xpath-fn-string
				       (xpath-single-child-elem-by-qname variant *xtm2.0-ns* "resourceData"))))
				 (cond
				   ((string= resourceData new-t100-variant-name)
				    (check-variant document variant
						   (list new-t100-variant-itemIdentity)
						   (list core-sort-psi t50a-psi)
						   (list :data new-t100-variant-name :type string-type)
						   nil))
				   ((string= resourceData new-t100-variant-name-merge-2)
				    (check-variant document variant 
						   (list new-t100-variant-itemIdentity-merge-2)
						   (list core-display-psi)
						   (list :data new-t100-variant-name-merge-2 :type string-type)
						   nil))
				   (t
				    (is-true
				     (format t "bad resourceData found in variant of new t100: ~A~%"
					     resourceData))))))))
		      (t
		       (is-true (format t "unknown subjectIndentifier found in fragments: ~A~%" href))))))))))



(defun run-exporter-tests()
  (tear-down-test-db) ; if a previous test fails and the store is open -> avoids elephant problems
  (run! 'exporter-tests))