;;+-----------------------------------------------------------------------------
;;+  Isidorus
;;+  (c) 2008-2009 Marc Kuester, Christoph Ludwig, Lukas Giessmann
;;+
;;+  Isidorus is freely distributable under the LGPL license.
;;+  You can find a detailed description in trunk/docs/LGPL-LICENSE.txt.
;;+-----------------------------------------------------------------------------


(in-package :exporter-test)
(in-suite exporter-tests)

;; === checks all topics from core_psis.xtm ====================================
(test test-std-topics-xtm1.0
  (with-fixture refill-test-db ()
    (export-xtm *out-xtm1.0-file* :xtm-format '1.0)
    (let ((document (dom:document-element
		     (cxml:parse-file *out-xtm1.0-file* (cxml-dom:make-dom-builder))))
	  (topic-counter 0))
      (check-document-structure document 38 2 :ns-uri *xtm1.0-ns*)
      (loop for topic across (xpath-child-elems-by-qname document *xtm1.0-ns* "topic")
	 do (loop for subjectIndicatorRef across (xpath-child-elems-by-qname
						  (xpath-single-child-elem-by-qname
						   topic *xtm1.0-ns* "subjectIdentity")
						  *xtm1.0-ns* "subjectIndicatorRef")
	       do (let ((href (dom:get-attribute-ns subjectIndicatorRef
						    *xtm1.0-xlink* "href")))
		    (cond
		      ((string= core-topic-psi href)
		       (incf topic-counter)
		       (format t "name: ~A~%"
			       (xpath-single-child-elem-by-qname topic *xtm1.0-ns*
								 "name")))
		      ((string= core-association-psi href)
		       (incf topic-counter)
		       (format t "name: ~A~%"
			       (xpath-single-child-elem-by-qname topic *xtm1.0-ns*
								 "name")))
		      ((string= core-occurrence-psi href)
		       (incf topic-counter)
		       (format t "name: ~A~%"
			       (xpath-single-child-elem-by-qname topic *xtm1.0-ns*
								 "name")))
		      ((string= core-class-instance-psi href)
		       (incf topic-counter)
		       (format t "name: ~A~%"
			       (xpath-single-child-elem-by-qname topic *xtm1.0-ns*
								 "name")))
		      ((string= core-class-psi href)
		       (incf topic-counter)
		       (format t "name: ~A~%"
			       (xpath-single-child-elem-by-qname topic *xtm1.0-ns*
								 "name")))
		      ((string= core-superclass-subclass-psi href)
		       (incf topic-counter)
		       (format t "name: ~A~%"
			       (xpath-single-child-elem-by-qname topic *xtm1.0-ns*
								 "name")))
		      ((string= core-superclass-psi href)
		       (incf topic-counter)
		       (format t "name: ~A~%"
			       (xpath-single-child-elem-by-qname topic *xtm1.0-ns*
								 "name")))
		      ((string= core-subclass-psi href)
		       (incf topic-counter)
		       (format t "name: ~A~%"
			       (xpath-single-child-elem-by-qname topic *xtm1.0-ns*
								 "name")))
		      ((string= core-sort-psi href)
		       (incf topic-counter)
		       (format t "name: ~A~%"
			       (xpath-single-child-elem-by-qname topic *xtm1.0-ns*
								 "name")))
		      ((string= core-display-psi href)
		       (incf topic-counter)
		       (format t "name: ~A~%"
			       (xpath-single-child-elem-by-qname topic *xtm1.0-ns*
								 "name")))
		      ((string= core-type-instance-psi href)
		       (incf topic-counter)
		       (format t "name: ~A~%"
			       (xpath-single-child-elem-by-qname topic *xtm1.0-ns*
								 "name")))
		      ((string= core-type-psi href)
		       (incf topic-counter)
		       (format t "name: ~A~%" 
			       (xpath-single-child-elem-by-qname topic *xtm1.0-ns*
								 "name")))
		      ((string= core-instance-psi href)
		       (incf topic-counter)
		       (format t "name: ~A~%"
			       (xpath-single-child-elem-by-qname topic *xtm1.0-ns*
								 "name")))))))
      (is (= topic-counter 13)))))


;; === checks the topics t1-t10 from sample_objects_2_0.xtm as xtm 1.0 file ====
(test test-sample-topics-t1-t10-xtm1.0
  (with-fixture refill-test-db()
    (export-xtm *out-xtm1.0-file* :xtm-format '1.0)
    (let ((document (dom:document-element (cxml:parse-file *out-xtm1.0-file* (cxml-dom:make-dom-builder)))))
      (check-document-structure document 38 2 :ns-uri *xtm1.0-ns*)
      (loop for topic across (xpath-child-elems-by-qname document *xtm1.0-ns* "topic")
	 do (loop for subjectIndicatorRef across (xpath-child-elems-by-qname
						  (xpath-single-child-elem-by-qname
						   topic *xtm1.0-ns* "subjectIdentity")
						  *xtm1.0-ns* "subjectIndicatorRef")
	       do (let ((href (dom:get-attribute-ns subjectIndicatorRef *xtm1.0-xlink* "href")))
		    (cond
		      ((string= href t1-psi)
		       (check-topic-id topic)
		       (check-baseNameStrings topic (list t1-name)))
		      ((string= href t3-psi)
		       (check-topic-id topic)
		       (check-single-instanceOf document topic t1-psi :xtm-format '1.0)
		       (check-baseNameStrings topic (list t3-name)))
		      ((string= href t3a-psi)
		       (check-topic-id topic)
		       (check-single-instanceOf document topic t3-psi :xtm-format '1.0)
		       (check-baseNameStrings topic (list t3a-name)))
		      ((string= href t4-psi)
		       (check-topic-id topic)
		       (check-single-instanceOf document topic t1-psi :xtm-format '1.0)
		       (check-baseNameStrings topic (list t4-name)))
		      ((string= href t6-psi)
		       (check-topic-id topic)
		       (check-single-instanceOf document topic t1-psi :xtm-format '1.0)
		       (check-baseNameStrings topic (list t6-name)))
		      ((string= href t7-psi)
		       (check-topic-id topic)
		       (check-single-instanceOf document topic t1-psi :xtm-format '1.0)
		       (check-baseNameStrings topic (list t7-name)))
		      ((string= href t8-psi)
		       (check-topic-id topic)
		       (check-single-instanceOf document topic t1-psi :xtm-format '1.0)
		       (check-baseNameStrings topic (list t8-name))))))))))


;; === checks the topics t50-t59 from sample_objects_2_0.xtm as xtm 1.0 file ===
(test test-sample-topics-t50-t59-xtm1.0
  (with-fixture refill-test-db()
    (export-xtm *out-xtm1.0-file* :xtm-format '1.0)
    (let ((document (dom:document-element (cxml:parse-file *out-xtm1.0-file* (cxml-dom:make-dom-builder)))))
      (check-document-structure document 38 2 :ns-uri *xtm1.0-ns*)
      (loop for topic across (xpath-child-elems-by-qname document *xtm1.0-ns* "topic")
	 do (loop for subjectIndicatorRef across (xpath-child-elems-by-qname
						  (xpath-single-child-elem-by-qname
						   topic *xtm1.0-ns* "subjectIdentity")
						  *xtm1.0-ns* "subjectIndicatorRef")
	       do (let ((href (dom:get-attribute-ns subjectIndicatorRef *xtm1.0-xlink* "href")))
		    (cond
		      ((string= href t50-psi)
		       (check-topic-id topic)
		       (check-single-instanceOf document topic t1-psi :xtm-format '1.0)
		       (check-baseNameStrings topic (list t50-name)))
		      ((string= href t50a-psi)
		       (check-topic-id topic)
		       (check-single-instanceOf document topic t6-psi :xtm-format '1.0)
		       (check-baseNameStrings topic (list t50a-name)))
		      ((string= href t51-psi)
		       (check-topic-id topic)
		       (check-single-instanceOf document topic t6-psi :xtm-format '1.0)
		       (check-baseNameStrings topic (list t51-name)))
		      ((string= href t52-psi)
		       (check-topic-id topic)
		       (check-single-instanceOf document topic t50-psi :xtm-format '1.0)
		       (check-baseNameStrings topic (list t52-name)))
		      ((string= href t53-psi)
		       (check-topic-id topic)
		       (check-single-instanceOf document topic t6-psi :xtm-format '1.0)
		       (check-baseNameStrings topic (list t53-name)))
		      ((string= href t54-psi)
		       (check-topic-id topic)
		       (check-single-instanceOf document topic t6-psi :xtm-format '1.0)
		       (check-baseNameStrings topic (list t54-name)))
		      ((string= href t55-psi)
		       (check-topic-id topic)
		       (check-single-instanceOf document topic t6-psi :xtm-format '1.0)
		       (check-baseNameStrings topic (list t55-name)))
		      ((string= href t56-psi)
		       (check-topic-id topic)
		       (check-single-instanceOf document topic t7-psi :xtm-format '1.0)
		       (check-baseNameStrings topic (list t56-name)))
		      ((string= href t57-psi)
		       (check-topic-id topic)
		       (check-single-instanceOf document topic t7-psi :xtm-format '1.0)
		       (check-baseNameStrings topic (list t57-name)))
		      ((string= href t58-psi)
		       (check-topic-id topic)
		       (check-single-instanceOf document topic t8-psi :xtm-format '1.0)
		       (check-baseNameStrings topic (list t58-name)))
		      ((string= href t59-psi)
		       (check-topic-id topic)
		       (check-single-instanceOf document topic t8-psi :xtm-format '1.0)
		       (check-baseNameStrings topic (list t59-name))))))))))

;; === checks the topics t60-t100 from sample_objects_2_0.xtm as xtm 1.0 file ==
(test test-sample-topics-t60-t100-xtm1.0
  (with-fixture refill-test-db()
    (export-xtm *out-xtm1.0-file* :xtm-format '1.0)
    (let ((document (dom:document-element (cxml:parse-file *out-xtm1.0-file* (cxml-dom:make-dom-builder))))
	  (t100-occurrences-resourceData (list "The ISO 19115 standard ..." "2003-01-01"))) ;local value->no type
      (check-document-structure document 38 2 :ns-uri *xtm1.0-ns*)
      (loop for topic across (xpath-child-elems-by-qname document *xtm1.0-ns* "topic")
	 do (loop for subjectIndicatorRef across (xpath-child-elems-by-qname
						  (xpath-single-child-elem-by-qname
						   topic *xtm1.0-ns* "subjectIdentity")
						  *xtm1.0-ns* "subjectIndicatorRef")
	       do (let ((href (dom:get-attribute-ns subjectIndicatorRef *xtm1.0-xlink* "href")))
		    (cond
		      ((string= href t62-psi)
		       (check-topic-id topic)
		       (check-single-instanceOf document topic t8-psi :xtm-format '1.0)
		       (check-baseNameStrings topic (list t62-name)))
		      ((string= href t63-psi)
		       (check-topic-id topic)
		       (check-single-instanceOf document topic t8-psi :xtm-format '1.0)
		       (check-baseNameStrings topic (list t63-name)))
		      ((string= href t64-psi)
		       (check-topic-id topic)
		       (check-single-instanceOf document topic t7-psi :xtm-format '1.0)
		       (check-baseNameStrings topic (list t64-name)))
		      ((string= href t100-psi)
		       (check-topic-id topic)
		       (check-single-instanceOf document topic t3a-psi :xtm-format '1.0)
		       (check-baseNameStrings topic (list t100-name))
		       (check-occurrences-instanceOf document topic (list t51-psi t53-psi t54-psi t55-psi))
		       (check-occurrences-resourceRef topic t100-occurrences-resourceRef :xtm-format '1.0)
		       (check-occurrences-resourceData topic t100-occurrences-resourceData :xtm-format '1.0)))))))))


;; === checks the topics t101-t301 from sample_objects_2_0.xtm as xtm 1.0 file =
(test test-sample-topics-t101-t301-xtm1.0
  (with-fixture refill-test-db()
    (export-xtm *out-xtm1.0-file* :xtm-format '1.0)
    (let ((document (dom:document-element (cxml:parse-file *out-xtm1.0-file* (cxml-dom:make-dom-builder)))))
      (check-document-structure document 38 2 :ns-uri *xtm1.0-ns*)      
      (loop for topic across (xpath-child-elems-by-qname document *xtm1.0-ns* "topic")
	 do (loop for subjectIndicatorRef across (xpath-child-elems-by-qname
						  (xpath-single-child-elem-by-qname
						   topic *xtm1.0-ns* "subjectIdentity")
						  *xtm1.0-ns* "subjectIndicatorRef")
	       do (let ((href (dom:get-attribute-ns subjectIndicatorRef *xtm1.0-xlink* "href")))
		    (cond
		      ((string= href t202-psi)
		       (check-topic-id topic)
		       (check-single-instanceOf document topic t4-psi :xtm-format '1.0)
		       (check-baseNameStrings topic (list t202-name)))
		      ((string= href t204-psi)
		       (check-topic-id topic)
		       (check-single-instanceOf document topic t4-psi :xtm-format '1.0)
		       (check-baseNameStrings topic (list t204-name)))
		      ((or (string= href t301a-psi-1)
			   (string= href t301a-psi-2))
		       (check-topic-id topic)
		       (loop for baseName across (xpath-child-elems-by-qname topic *xtm1.0-ns* "baseName")
			  do (let ((value (xpath-fn-string
					   (xpath-single-child-elem-by-qname baseName *xtm1.0-ns* "baseNameString"))))
			       (cond
				 ((string= t301a-name-value-1 value)
				  (is-true t))
				 ((string= t301a-name-value-2 value)
				  (let ((scopes (xpath-child-elems-by-qname baseName *xtm1.0-ns* "scope")))
				    (is (= (length scopes) 1))
				    (let ((topicRefs
					   (xpath-child-elems-by-qname (elt scopes 0) *xtm1.0-ns* "topicRef")))
				      (is (= (length topicRefs) 1))
				      (is (string= t50a-psi
						   (get-subjectIndicatorRef-by-ref
						    document
						    (dom:get-attribute-ns
						     (elt topicRefs 0) *xtm1.0-xlink* "href")))))))
				 (t
				  (is-true (format t "bad name value: ~A" value))))))
		       (let ((occurrences (xpath-child-elems-by-qname topic *xtm1.0-ns* "occurrence")))
			 (is (= (length occurrences) 1))
			 (let ((instanceOfs (xpath-child-elems-by-qname (elt occurrences 0) *xtm1.0-ns* "instanceOf")))
			   (is (= (length instanceOfs) 1))
			   (let ((topicRefs (xpath-child-elems-by-qname (elt instanceOfs 0) *xtm1.0-ns* "topicRef")))
			     (is (= (length topicRefs) 1))
			     (is (string= t55-psi
					  (get-subjectIndicatorRef-by-ref
					   document
					   (dom:get-attribute-ns (elt topicRefs 0) *xtm1.0-xlink* "href"))))))
			 (let ((resourceRefs
				(xpath-child-elems-by-qname (elt occurrences 0) *xtm1.0-ns* "resourceRef")))
			   (is (= (length resourceRefs) 1))
			   (is (string= t301a-occurrence-resourceRef
					(dom:get-attribute-ns (elt resourceRefs 0) *xtm1.0-xlink* "href")))))))))))))


;; === checks the associations from sample_objects_2_0.xtm as xtm 1.0 file =====
(test test-sample-associations-xtm1.0
  (with-fixture refill-test-db()
    (export-xtm *out-xtm1.0-file* :xtm-format '1.0)
    (let ((document (dom:document-element (cxml:parse-file *out-xtm1.0-file* (cxml-dom:make-dom-builder)))))
      (check-document-structure document 38 2 :ns-uri *xtm1.0-ns*)
      (loop for association across (xpath-child-elems-by-qname document *xtm1.0-ns* "association")
	 do (let ((instanceOfs (xpath-child-elems-by-qname association *xtm1.0-ns* "instanceOf")))
	      (is (= (length instanceOfs) 1))
	      (let ((topicRefs (xpath-child-elems-by-qname (elt instanceOfs 0) *xtm1.0-ns* "topicRef")))
		(is (= (length instanceOfs) 1))
		(let ((topic-ref (get-subjectIndicatorRef-by-ref
				  document
				  (dom:get-attribute-ns (elt topicRefs 0) *xtm1.0-xlink* "href"))))
		  (cond
		    ((string= topic-ref t57-psi)
		     (loop for member across (xpath-child-elems-by-qname association *xtm1.0-ns* "member")
			do (let ((players (xpath-child-elems-by-qname member *xtm1.0-ns* "topicRef")))
			     (is (= (length players)))
			     (let ((player (get-subjectIndicatorRef-by-ref
					    document
					    (dom:get-attribute-ns (elt players 0) *xtm1.0-xlink* "href"))))
			       (cond
				 ((string= t202-psi player)
				  (let ((roleSpecs (xpath-child-elems-by-qname member *xtm1.0-ns* "roleSpec")))
				    (is (= (length roleSpecs) 1))
				    (let ((rS-t-refs
					   (xpath-child-elems-by-qname (elt roleSpecs 0) *xtm1.0-ns* "topicRef")))
				      (is (= (length rS-t-refs) 1))
				      (is (string= t59-psi
						   (get-subjectIndicatorRef-by-ref
						    document
						    (dom:get-attribute-ns
						     (elt rS-t-refs 0)
						     *xtm1.0-xlink*
						     "href")))))))
				 ((string= t204-psi player)
				  (let ((roleSpecs (xpath-child-elems-by-qname member *xtm1.0-ns* "roleSpec")))
				    (is (= (length roleSpecs) 1))
				    (let ((rS-t-refs
					   (xpath-child-elems-by-qname (elt roleSpecs 0) *xtm1.0-ns* "topicRef")))
				      (is (= (length rS-t-refs) 1))
				      (is (string= t58-psi
						   (get-subjectIndicatorRef-by-ref
						    document
						    (dom:get-attribute-ns
						     (elt rS-t-refs 0)
						     *xtm1.0-xlink*
						     "href")))))))
				 (t
				  (is-true (format t "bad player found in member: ~A~%" player))))))))
		    ((string= topic-ref t64-psi)
		     (loop for member across (xpath-child-elems-by-qname association *xtm1.0-ns* "member")
			do (let ((players (xpath-child-elems-by-qname member *xtm1.0-ns* "topicRef")))
			     (is (= (length players)))
			     (let ((player (get-subjectIndicatorRef-by-ref
					    document
					    (dom:get-attribute-ns (elt players 0) *xtm1.0-xlink* "href"))))
			       (cond
				 ((or (string= t301a-psi-1 player)
				      (string= t301a-psi-2 player))
				  (let ((roleSpecs (xpath-child-elems-by-qname member *xtm1.0-ns* "roleSpec")))
				    (is (= (length roleSpecs) 1))
				    (let ((rS-t-refs
					   (xpath-child-elems-by-qname (elt roleSpecs 0) *xtm1.0-ns* "topicRef")))
				      (is (= (length rS-t-refs) 1))
				      (is (string= t63-psi
						   (get-subjectIndicatorRef-by-ref
						    document
						    (dom:get-attribute-ns
						     (elt rS-t-refs 0)
						     *xtm1.0-xlink*
						     "href")))))))
				 ((string= t100-psi player)
				  (let ((roleSpecs (xpath-child-elems-by-qname member *xtm1.0-ns* "roleSpec")))
				    (is (= (length roleSpecs) 1))
				    (let ((rS-t-refs
					   (xpath-child-elems-by-qname (elt roleSpecs 0) *xtm1.0-ns* "topicRef")))
				      (is (= (length rS-t-refs) 1))
				      (is (string= t62-psi
						   (get-subjectIndicatorRef-by-ref
						    document
						    (dom:get-attribute-ns
						     (elt rS-t-refs 0)
						     *xtm1.0-xlink*
						     "href")))))))
				 (t
				  (is-true (format t "bad player found in member: ~A~%" player))))))))
		    (t
		     (is-true (format t "bad instanceOf in association found: ~A~%" topic-ref)))))))))))


;; === checks the fragment of the topic t100 from sample_objects_2_0.xtm as ====
;; === xtm 1.0 file
(test test-fragments-xtm1.0
  (with-fixture refill-test-db()
    (let* ((psi "http://psi.egovpt.org/standard/ISO+19115%3A+Geographic+Information+-+Metadata")
	   (t100 (loop for item in (elephant:get-instances-by-class 'PersistentIdC)
		    when (string= (uri item) psi)
		    return (identified-construct item)))
	   (t100-start-revision (d::start-revision (first (d::versions t100)))))
      (d:get-fragments t100-start-revision)
      (let ((t100-fragment (loop for item in (elephant:get-instances-by-class 'FragmentC)
			      when (eq (topic item) t100)
			      return item)))
	(with-open-file (stream *out-xtm1.0-file* :direction :output)
	  (write-string (export-xtm-fragment t100-fragment :xtm-format '1.0) stream))))

    (let ((document (dom:document-element (cxml:parse-file *out-xtm1.0-file* (cxml-dom:make-dom-builder))))
	  (t100-occurrences-resourceData (list "The ISO 19115 standard ..." "2003-01-01"))) ;local value->no type
      (check-document-structure document 10 1 :ns-uri *xtm1.0-ns*)
      (loop for topic across (xpath-child-elems-by-qname document *xtm1.0-ns* "topic")
	 do (loop for subjectIndicatorRef across (xpath-child-elems-by-qname
						  (xpath-single-child-elem-by-qname
						   topic *xtm1.0-ns* "subjectIdentity")
						  *xtm1.0-ns* "subjectIndicatorRef")
	       do (let ((href (dom:get-attribute-ns subjectIndicatorRef *xtm1.0-xlink* "href")))
		    (cond
		      ((string= href t3a-psi)
		       (check-topic-id topic))
		      ((string= href t51-psi)
		       (check-topic-id topic))
		      ((string= href t53-psi)
		       (check-topic-id topic))
		      ((string= href t54-psi)
		       (check-topic-id topic))
		      ((string= href t55-psi)
		       (check-topic-id topic))
		      ((string= href t62-psi)
		       (check-topic-id topic))
		      ((string= href t63-psi)
		       (check-topic-id topic))
		      ((string= href t64-psi)
		       (check-topic-id topic))
		      ((string= href t100-psi)
		       (check-topic-id topic)
		       (check-single-instanceOf document topic t3a-psi :xtm-format '1.0)
		       (check-baseNameStrings topic (list t100-name))
		       (check-occurrences-instanceOf document topic (list t51-psi t53-psi t54-psi t55-psi))
		       (check-occurrences-resourceRef topic t100-occurrences-resourceRef :xtm-format '1.0)
		       (check-occurrences-resourceData topic t100-occurrences-resourceData :xtm-format '1.0))
		      ((or (string= href t301a-psi-1)
			   (string= href t301a-psi-2))
		       (check-topic-id topic)))))))))



;; === tests the export to an xtm1.0 file from a certain revision ==============
(test test-exporter-xtm1.0-versions-1
  (with-fixture merge-test-db ()
    (handler-case (delete-file *out-xtm1.0-file*)(error () )) ;deletes file - if exist
    (export-xtm *out-xtm1.0-file* :revision fixtures::revision1 :xtm-format '1.0)
    (let ((document
	   (dom:document-element
	    (cxml:parse-file *out-xtm1.0-file* (cxml-dom:make-dom-builder))))
	  (t100-occurrences-resourceData (list "The ISO 19115 standard ..." "2003-01-01"))) ;local value->no type
      (check-document-structure document 47 7 :ns-uri *xtm1.0-ns*)
      (loop for topic across (xpath-child-elems-by-qname document *xtm1.0-ns* "topic")
	 do (loop for subjectIndicatorRef across (xpath-child-elems-by-qname
						  (xpath-single-child-elem-by-qname
						   topic *xtm1.0-ns* "subjectIdentity")
						  *xtm1.0-ns* "subjectIndicatorRef")
	       do (let ((href (dom:get-attribute-ns subjectIndicatorRef *xtm1.0-xlink* "href")))
		    (cond
		      ((string= href t1-psi)
		       (check-topic-id topic)
		       (check-baseNameStrings topic (list t1-name)))
		      ((string= href t2-psi)
		       (check-topic-id topic)
		       (check-baseNameStrings topic (list t2-name))
		       (check-single-instanceOf document topic t1-psi :xtm-format '1.0))
		      ((string= href t3-psi)
		       (check-topic-id topic)
		       (check-baseNameStrings topic (list t3-name))
		       (check-single-instanceOf document topic t1-psi :xtm-format '1.0))
		      ((string= href t3a-psi)
		       (check-topic-id topic)
		       (check-baseNameStrings topic (list t3a-name))
		       (check-single-instanceOf document topic t3-psi :xtm-format '1.0))
		      ((string= href t50-psi)
		       (check-topic-id topic)
		       (check-baseNameStrings topic (list t50-name))
		       (check-single-instanceOf document topic t1-psi :xtm-format '1.0))
		      ((string= href t50a-psi)
		       (check-topic-id topic)
		       (check-baseNameStrings topic (list t50a-name))
		       (check-single-instanceOf document topic t6-psi :xtm-format '1.0)
		       (let ((variant-elem
			      (let ((variant-nodes
				     (xpath-child-elems-by-qname 
				      (xpath-single-child-elem-by-qname topic *xtm1.0-ns* "baseName")
				      *xtm1.0-ns* "variant")))
				(is (= (length variant-nodes) 1))
				(elt variant-nodes 0))))
			 (check-variant-xtm1.0 document variant-elem
					       (list core-sort-psi)
					       t50a-variant-name nil)))
		      ((string= href t55-psi)
		       (check-topic-id topic)
		       (check-baseNameStrings topic (list t55-name))
		       (check-single-instanceOf document topic t6-psi :xtm-format '1.0))
		      ((string= href t57-psi)
		       (check-topic-id topic)
		       (check-baseNameStrings topic (list t57-name))
		       (check-single-instanceOf document topic t7-psi :xtm-format '1.0))
		      ((string= href t58-psi)
		       (check-topic-id topic)
		       (check-baseNameStrings topic (list t58-name))
		       (check-single-instanceOf document topic t8-psi :xtm-format '1.0))
		      ((string= href t59-psi)
		       (check-topic-id topic)
		       (check-baseNameStrings topic (list t59-name))
		       (check-single-instanceOf document topic t8-psi :xtm-format '1.0))
		      ((string= href t62-psi)
		       (check-topic-id topic)
		       (check-baseNameStrings topic (list t62-name))
		       (check-single-instanceOf document topic t8-psi :xtm-format '1.0))
		      ((string= href t64-psi)
		       (check-topic-id topic)
		       (check-baseNameStrings topic (list t64-name))
		       (check-single-instanceOf document topic t7-psi :xtm-format '1.0))
		      ((string= href t100-psi)
		       (check-topic-id topic)
		       (check-baseNameStrings topic (list t100-name))
		       (check-single-instanceOf document topic t3a-psi :xtm-format '1.0)
		       (check-occurrences-instanceOf document topic (list t51-psi t53-psi t54-psi t55-psi))
		       (check-occurrences-resourceRef topic t100-occurrences-resourceRef :xtm-format '1.0)
		       (check-occurrences-resourceData topic t100-occurrences-resourceData :xtm-format '1.0)
		       (let ((variant-nodes (xpath-child-elems-by-qname
					     (xpath-single-child-elem-by-qname topic *xtm1.0-ns* "baseName")
					     *xtm1.0-ns* "variant")))
			 (is (= (length variant-nodes) 2))
			 (loop for variant across variant-nodes
			    do (let ((variantName
				      (xpath-fn-string
				       (xpath-single-child-elem-by-qname
					(xpath-single-child-elem-by-qname variant *xtm1.0-ns* "variantName")
					*xtm1.0-ns* "resourceData"))))
				 (cond
				   ((string= variantName t100-variant-1-name)
				    (check-variant-xtm1.0 document variant (list core-display-psi)
							  t100-variant-1-name nil))
				   ((string= variantName t100-variant-2-name)
				    (check-variant-xtm1.0 document variant (list core-sort-psi)
							  t100-variant-2-name nil))
				   (t
				    (is-true (format t "bad variantName found in t100: ~A~%" variantName))))))))
		      ((string= href t101-psi)
		       (check-topic-id topic)
		       (check-baseNameStrings topic (list t101-name-1 t101-name-2))
		       (loop for name across (xpath-child-elems-by-qname topic *xtm1.0-ns* "baseName")
			  do (let ((baseNameString
				    (xpath-fn-string
				     (xpath-single-child-elem-by-qname name *xtm1.0-ns* "baseNameString"))))
			       (when (string= baseNameString t101-name-2)
				 (let ((scope
				       (let ((scope-nodes (xpath-child-elems-by-qname name *xtm1.0-ns* "scope")))
					 (is (= (length scope-nodes) 1))
					 (let ((topicRef-nodes
						(xpath-child-elems-by-qname (elt scope-nodes 0)
									    *xtm1.0-ns* "topicRef")))
					   (is (= (length topicRef-nodes) 1))
					   (get-subjectIndicatorRef-by-ref document
									   (dom:get-attribute-ns
									    (elt topicRef-nodes 0) *xtm1.0-xlink*
									    "href"))))))
				   (is (string= scope t50a-psi)))
				 (is (= (length (xpath-child-elems-by-qname name *xtm1.0-ns* "type")) 0))
				 (let ((variant-node
					(let ((variant-nodes
					       (xpath-child-elems-by-qname name *xtm1.0-ns* "variant")))
					  (is (= (length variant-nodes) 1))
					  (elt variant-nodes 0))))
				   (check-variant-xtm1.0 document variant-node (list t50a-psi core-sort-psi)
							 t101-variant-name nil)))))
		       (check-single-instanceOf document topic t3a-psi :xtm-format '1.0)
		       (loop for occurrence across (xpath-child-elems-by-qname topic *xtm1.0-ns* "occurrence")
			  do (let ((instanceOf
				    (let ((instanceOf-nodes
					   (xpath-child-elems-by-qname occurrence *xtm1.0-ns* "instanceOf")))
				      (is (= (length instanceOf-nodes) 1))
				      (let ((topicRef-nodes
					     (xpath-child-elems-by-qname (elt instanceOf-nodes 0)
									 *xtm1.0-ns* "topicRef")))
					(is (= (length topicRef-nodes) 1))
					(get-subjectIndicatorRef-by-ref
					 document
					 (dom:get-attribute-ns (elt topicRef-nodes 0) *xtm1.0-xlink* "href"))))))
			       (cond
				 ((string= instanceOf t51-psi)
				  (let ((resourceRef
					 (get-subjectIndicatorRef-by-ref
					  document
					  (dom:get-attribute-ns 
					   (xpath-single-child-elem-by-qname occurrence *xtm1.0-ns* "resourceRef")
					   *xtm1.0-xlink* "href"))))
				    (is (string= resourceRef t52-psi))))
				 ((string= instanceOf t53-psi)
				  (is (= (length
					  (xpath-child-elems-by-qname occurrence *xtm1.0-ns* "resourceData")) 1))
				  (is (= (length
					  (dom:get-attribute-ns
					   (xpath-single-child-elem-by-qname occurrence *xtm1.0-ns* "resourceData")
					   *xtm1.0-xlink* "datatype"))
					 0)))
				 ((string= instanceOf t54-psi)
				  (let ((resourceData-node
					 (xpath-single-child-elem-by-qname occurrence *xtm1.0-ns* "resourceData")))
				    (let ((resourceData
					   (xpath-fn-string resourceData-node))
					  (datatype
					   (dom:get-attribute-ns resourceData-node *xtm1.0-xlink* "datatype")))
				      (is (= (length datatype) 0))
				      (is (string= resourceData (getf t101-occurrence-3-resourceData :data))))))
				 ((string= instanceOf t55-psi)
				  (let ((resourceRef
					 (dom:get-attribute-ns 
					  (xpath-single-child-elem-by-qname occurrence *xtm1.0-ns* "resourceRef")
					  *xtm1.0-xlink* "href")))
				    (is (string= resourceRef t101-occurrence-4-resourceRef))))
				 (t
				  (is-true (format t "bad occurrence instanceOf found in t101: ~A~%" instanceOf)))))))
		      ((string= href t202-psi)
		       (check-topic-id topic)
		       (check-baseNameStrings topic (list t202-name))
		       (check-single-instanceOf document topic t4-psi :xtm-format '1.0))
		      ((string= href t203-psi)
		       (check-topic-id topic)
		       (check-baseNameStrings topic (list t203-name))
		       (check-single-instanceOf document topic t4-psi :xtm-format '1.0))
		      ((string= href t300-psi)
		       (check-topic-id topic)
		       (check-baseNameStrings topic (list t300-name))
		       (check-single-instanceOf document topic t2-psi :xtm-format '1.0)))))))))


;; === tests the export to an xtm1.0 file from a certain revision ==============
(test test-exporter-xtm1.0-versions-2
  (with-fixture merge-test-db ()
    (handler-case (delete-file *out-xtm1.0-file*)(error () )) ;deletes file - if exist
    (export-xtm *out-xtm1.0-file* :revision fixtures::revision2 :xtm-format '1.0)
    (let ((document (dom:document-element (cxml:parse-file *out-xtm1.0-file* (cxml-dom:make-dom-builder))))
	  (t100-occurrences-resourceData (list "The ISO 19115 standard ..." "2003-01-01"))) ;local value->no type
      (check-document-structure document 48 7 :ns-uri *xtm1.0-ns*)
      (loop for topic across (xpath-child-elems-by-qname document *xtm1.0-ns* "topic")
	 do (loop for subjectIndicatorRef across (xpath-child-elems-by-qname
						  (xpath-single-child-elem-by-qname
						   topic *xtm1.0-ns* "subjectIdentity")
						  *xtm1.0-ns* "subjectIndicatorRef")
	       do (let ((href (dom:get-attribute-ns subjectIndicatorRef *xtm1.0-xlink* "href")))
		    (cond
		      ((string= href t1-psi)
		       (check-topic-id topic)
		       (check-baseNameStrings topic (list t1-name)))
		      ((string= href t2-psi)
		       (check-topic-id topic)
		       (check-baseNameStrings topic (list t2-name))
		       (check-single-instanceOf document topic t1-psi :xtm-format '1.0))
		      ((string= href t3-psi)
		       (check-topic-id topic)
		       (check-baseNameStrings topic (list t3-name))
		       (check-single-instanceOf document topic t1-psi :xtm-format '1.0))
		      ((string= href t3a-psi)
		       (check-topic-id topic)
		       (let ((subjectIdentity-nodes
			      (xpath-child-elems-by-qname topic *xtm1.0-ns* "subjectIdentity")))
			 (is (= (length subjectIdentity-nodes) 1))
			 (let ((subjectIndicatorRef-nodes
				(xpath-child-elems-by-qname (elt subjectIdentity-nodes 0)
							    *xtm1.0-ns* "subjectIndicatorRef")))
			   (let ((subjectIndicatorRefs
				  (map 'list #'(lambda(x)
						 (dom:get-attribute-ns x *xtm1.0-xlink* "href"))
						 subjectIndicatorRef-nodes)))
			     (is (= (length subjectIndicatorRefs) 2))
			     (is-true (find t3a-psi-merge-1 subjectIndicatorRefs :test #'string=)))))
		       (check-baseNameStrings topic (list t3a-name))
		       (check-single-instanceOf document topic t3-psi :xtm-format '1.0))
		      ((string= href t50-psi)
		       (check-topic-id topic)
		       (check-baseNameStrings topic (list t50-name))
		       (check-single-instanceOf document topic t1-psi :xtm-format '1.0))
		      ((string= href t50a-psi)
		       (check-topic-id topic)
		       (check-baseNameStrings topic (list t50a-name))
		       (check-single-instanceOf document topic t6-psi :xtm-format '1.0)
		       (let ((variant-elem
			      (let ((variant-nodes
				     (xpath-child-elems-by-qname 
				      (xpath-single-child-elem-by-qname topic *xtm1.0-ns* "baseName")
				      *xtm1.0-ns* "variant")))
				(is (= (length variant-nodes) 1))
				(elt variant-nodes 0))))
			 (check-variant-xtm1.0 document variant-elem
					       (list core-sort-psi)
					       t50a-variant-name nil)))
		      ((string= href t55-psi)
		       (check-topic-id topic)
		       (check-baseNameStrings topic (list t55-name))
		       (check-single-instanceOf document topic t6-psi :xtm-format '1.0))
		      ((string= href t57-psi)
		       (check-topic-id topic)
		       (check-baseNameStrings topic (list t57-name))
		       (check-single-instanceOf document topic t7-psi :xtm-format '1.0))
		      ((string= href t58-psi)
		       (check-topic-id topic)
		       (check-baseNameStrings topic (list t58-name))
		       (check-single-instanceOf document topic t8-psi :xtm-format '1.0))
		      ((string= href t59-psi)
		       (check-topic-id topic)
		       (check-baseNameStrings topic (list t59-name))
		       (check-single-instanceOf document topic t8-psi :xtm-format '1.0))
		      ((string= href t62-psi)
		       (check-topic-id topic)
		       (check-baseNameStrings topic (list t62-name))
		       (check-single-instanceOf document topic t8-psi :xtm-format '1.0))
		      ((string= href t64-psi)
		       (check-topic-id topic)
		       (check-baseNameStrings topic (list t64-name))
		       (check-single-instanceOf document topic t7-psi :xtm-format '1.0))
		      ((string= href t100-psi)
		       (check-topic-id topic)
		       (check-baseNameStrings topic (list t100-name t100-name-merge-1))
		       (check-single-instanceOf document topic t3a-psi :xtm-format '1.0)
		       (check-occurrences-instanceOf document topic (list t51-psi t53-psi t54-psi t55-psi t55-psi))
		       (check-occurrences-resourceRef topic t100-occurrences-resourceRef-merge-1 :xtm-format '1.0)
		       (check-occurrences-resourceData topic t100-occurrences-resourceData :xtm-format '1.0)
		       (let ((variant-nodes (xpath-child-elems-by-qname
					     (xpath-single-child-elem-by-qname topic *xtm1.0-ns* "baseName")
					     *xtm1.0-ns* "variant")))
			 (is (= (length variant-nodes) 2))
			 (loop for variant across variant-nodes
			    do (let ((variantName
				      (xpath-fn-string
				       (xpath-single-child-elem-by-qname
					(xpath-single-child-elem-by-qname variant *xtm1.0-ns* "variantName")
					*xtm1.0-ns* "resourceData"))))
				 (cond
				   ((string= variantName t100-variant-1-name)
				    (check-variant-xtm1.0 document variant (list core-display-psi)
							  t100-variant-1-name nil))
				   ((string= variantName t100-variant-2-name)
				    (check-variant-xtm1.0 document variant (list core-sort-psi)
							  t100-variant-2-name nil))
				   (t
				    (is-true (format t "bad variantName found in t100: ~A~%" variantName))))))))
		      ((string= href new-t100-psi)
		       (check-topic-id topic)
		       (check-baseNameStrings topic (list new-t100-name))
		       (check-single-instanceOf document topic t3-psi :xtm-format '1.0)
		       (check-occurrences-resourceRef topic (list new-t100-occurrence-resourceRef) :xtm-format '1.0)
		       (check-occurrences-instanceOf document topic (list t55-psi))
		       (let ((variant-node
			      (let ((variant-nodes
				     (xpath-child-elems-by-qname
				      (xpath-single-child-elem-by-qname topic *xtm1.0-ns* "baseName")
				      *xtm1.0-ns* "variant")))
				(is (= (length variant-nodes) 1))
				(elt variant-nodes 0))))
			 (check-variant-xtm1.0 document variant-node (list core-sort-psi t50a-psi)
					       new-t100-variant-name nil)))
		      ((string= href t101-psi)
		       (check-topic-id topic)
		       (check-baseNameStrings topic (list t101-name-1 t101-name-2))
		       (loop for name across (xpath-child-elems-by-qname topic *xtm1.0-ns* "baseName")
			  do (let ((baseNameString
				    (xpath-fn-string
				     (xpath-single-child-elem-by-qname name *xtm1.0-ns* "baseNameString"))))
			       (when (string= baseNameString t101-name-2)
				 (let ((scope
				       (let ((scope-nodes (xpath-child-elems-by-qname name *xtm1.0-ns* "scope")))
					 (is (= (length scope-nodes) 1))
					 (let ((topicRef-nodes
						(xpath-child-elems-by-qname (elt scope-nodes 0)
									    *xtm1.0-ns* "topicRef")))
					   (is (= (length topicRef-nodes) 1))
					   (get-subjectIndicatorRef-by-ref document
									   (dom:get-attribute-ns
									    (elt topicRef-nodes 0) *xtm1.0-xlink*
									    "href"))))))
				   (is (string= scope t50a-psi)))
				 (is (= (length (xpath-child-elems-by-qname name *xtm1.0-ns* "type")) 0))
				 (let ((variant-node
					(let ((variant-nodes
					       (xpath-child-elems-by-qname name *xtm1.0-ns* "variant")))
					  (is (= (length variant-nodes) 1))
					  (elt variant-nodes 0))))
				   (check-variant-xtm1.0 document variant-node (list t50a-psi core-sort-psi)
							 t101-variant-name nil)))))
		       (check-single-instanceOf document topic t3a-psi :xtm-format '1.0)
		       (loop for occurrence across (xpath-child-elems-by-qname topic *xtm1.0-ns* "occurrence")
			  do (let ((instanceOf
				    (let ((instanceOf-nodes
					   (xpath-child-elems-by-qname occurrence *xtm1.0-ns* "instanceOf")))
				      (is (= (length instanceOf-nodes) 1))
				      (let ((topicRef-nodes
					     (xpath-child-elems-by-qname (elt instanceOf-nodes 0)
									 *xtm1.0-ns* "topicRef")))
					(is (= (length topicRef-nodes) 1))
					(get-subjectIndicatorRef-by-ref
					 document
					 (dom:get-attribute-ns (elt topicRef-nodes 0) *xtm1.0-xlink* "href"))))))
			       (cond
				 ((string= instanceOf t51-psi)
				  (let ((resourceRef
					 (get-subjectIndicatorRef-by-ref
					  document
					  (dom:get-attribute-ns 
					   (xpath-single-child-elem-by-qname occurrence *xtm1.0-ns* "resourceRef")
					   *xtm1.0-xlink* "href"))))
				    (is (string= resourceRef t52-psi))))
				 ((string= instanceOf t53-psi)
				  (is (= (length
					  (xpath-child-elems-by-qname occurrence *xtm1.0-ns* "resourceData")) 1))
				  (is (= (length
					  (dom:get-attribute-ns
					   (xpath-single-child-elem-by-qname occurrence *xtm1.0-ns* "resourceData")
					   *xtm1.0-xlink* "datatype"))
					 0)))
				 ((string= instanceOf t54-psi)
				  (let ((resourceData-node
					 (xpath-single-child-elem-by-qname occurrence *xtm1.0-ns* "resourceData")))
				    (let ((resourceData
					   (xpath-fn-string resourceData-node))
					  (datatype
					   (dom:get-attribute-ns resourceData-node *xtm1.0-xlink* "datatype")))
				      (is (= (length datatype) 0))
				      (is (string= resourceData (getf t101-occurrence-3-resourceData :data))))))
				 ((string= instanceOf t55-psi)
				  (let ((resourceRef
					 (dom:get-attribute-ns 
					  (xpath-single-child-elem-by-qname occurrence *xtm1.0-ns* "resourceRef")
					  *xtm1.0-xlink* "href")))
				    (is (string= resourceRef t101-occurrence-4-resourceRef))))
				 (t
				  (is-true (format t "bad occurrence instanceOf found in t101: ~A~%" instanceOf)))))))
		      ((string= href t202-psi)
		       (check-topic-id topic)
		       (check-baseNameStrings topic (list t202-name))
		       (check-single-instanceOf document topic t4-psi :xtm-format '1.0))
		      ((string= href t203-psi)
		       (check-topic-id topic)
		       (check-baseNameStrings topic (list t203-name))
		       (check-single-instanceOf document topic t4-psi :xtm-format '1.0))
		      ((string= href t300-psi)
		       (check-topic-id topic)
		       (check-baseNameStrings topic (list t300-name))
		       (check-single-instanceOf document topic t2-psi :xtm-format '1.0)))))))))


;; === tests the export to an xtm1.0 file from a certain revision ==============
(test test-exporter-xtm1.0-versions-3
  (with-fixture merge-test-db ()
    (handler-case (delete-file *out-xtm1.0-file*)(error () )) ;deletes file - if exist
    (export-xtm *out-xtm1.0-file* :revision fixtures::revision3 :xtm-format '1.0)
    (let ((document (dom:document-element (cxml:parse-file *out-xtm1.0-file* (cxml-dom:make-dom-builder))))
	  (t100-occurrences-resourceData (list "The ISO 19115 standard ..." "2003-01-01"))) ;local value->no type
      (check-document-structure document 48 8 :ns-uri *xtm1.0-ns*)
      (loop for topic across (xpath-child-elems-by-qname document *xtm1.0-ns* "topic")
	 do (loop for subjectIndicatorRef across (xpath-child-elems-by-qname
						  (xpath-single-child-elem-by-qname
						   topic *xtm1.0-ns* "subjectIdentity")
						  *xtm1.0-ns* "subjectIndicatorRef")
	       do (let ((href (dom:get-attribute-ns subjectIndicatorRef *xtm1.0-xlink* "href")))
		    (cond
		      ((string= href t1-psi)
		       (check-topic-id topic)
		       (check-baseNameStrings topic (list t1-name)))
		      ((string= href t2-psi)
		       (check-topic-id topic)
		       (check-baseNameStrings topic (list t2-name))
		       (check-single-instanceOf document topic t1-psi :xtm-format '1.0))
		      ((string= href t3-psi)
		       (check-topic-id topic)
		       (check-baseNameStrings topic (list t3-name))
		       (check-single-instanceOf document topic t1-psi :xtm-format '1.0))
		      ((string= href t3a-psi)
		       (check-topic-id topic)
		       (let ((subjectIdentity-nodes
			      (xpath-child-elems-by-qname topic *xtm1.0-ns* "subjectIdentity")))
			 (is (= (length subjectIdentity-nodes) 1))
			 (let ((subjectIndicatorRef-nodes
				(xpath-child-elems-by-qname (elt subjectIdentity-nodes 0)
							    *xtm1.0-ns* "subjectIndicatorRef")))
			   (let ((subjectIndicatorRefs
				  (map 'list #'(lambda(x)
						 (dom:get-attribute-ns x *xtm1.0-xlink* "href"))
						 subjectIndicatorRef-nodes)))
			     (is (= (length subjectIndicatorRefs) 3))
			     (is-true (find t3a-psi-merge-1 subjectIndicatorRefs :test #'string=))
			     (is-true (find t3a-psi-merge-2 subjectIndicatorRefs :test #'string=)))))
		       (check-baseNameStrings topic (list t3a-name))
		       (check-single-instanceOf document topic t3-psi :xtm-format '1.0))
		      ((string= href t50-psi)
		       (check-topic-id topic)
		       (check-baseNameStrings topic (list t50-name))
		       (check-single-instanceOf document topic t1-psi :xtm-format '1.0))
		      ((string= href t50a-psi)
		       (check-topic-id topic)
		       (check-baseNameStrings topic (list t50a-name))
		       (check-single-instanceOf document topic t6-psi :xtm-format '1.0)
		       (let ((variant-elem
			      (let ((variant-nodes
				     (xpath-child-elems-by-qname 
				      (xpath-single-child-elem-by-qname topic *xtm1.0-ns* "baseName")
				      *xtm1.0-ns* "variant")))
				(is (= (length variant-nodes) 1))
				(elt variant-nodes 0))))
			 (check-variant-xtm1.0 document variant-elem
					       (list core-sort-psi)
					       t50a-variant-name nil)))
		      ((string= href t55-psi)
		       (check-topic-id topic)
		       (check-baseNameStrings topic (list t55-name))
		       (check-single-instanceOf document topic t6-psi :xtm-format '1.0))
		      ((string= href t57-psi)
		       (check-topic-id topic)
		       (check-baseNameStrings topic (list t57-name))
		       (check-single-instanceOf document topic t7-psi :xtm-format '1.0))
		      ((string= href t58-psi)
		       (check-topic-id topic)
		       (check-baseNameStrings topic (list t58-name))
		       (check-single-instanceOf document topic t8-psi :xtm-format '1.0))
		      ((string= href t59-psi)
		       (check-topic-id topic)
		       (check-baseNameStrings topic (list t59-name))
		       (check-single-instanceOf document topic t8-psi :xtm-format '1.0))
		      ((string= href t62-psi)
		       (check-topic-id topic)
		       (check-baseNameStrings topic (list t62-name))
		       (check-single-instanceOf document topic t8-psi :xtm-format '1.0))
		      ((string= href t64-psi)
		       (check-topic-id topic)
		       (check-baseNameStrings topic (list t64-name))
		       (check-single-instanceOf document topic t7-psi :xtm-format '1.0))
		      ((string= href t100-psi)
		       (check-topic-id topic)
		       (check-baseNameStrings topic (list t100-name t100-name-merge-1))
		       (check-single-instanceOf document topic t3a-psi :xtm-format '1.0)
		       (check-occurrences-instanceOf document topic (list t51-psi t53-psi t54-psi t55-psi t55-psi))
		       (check-occurrences-resourceRef topic t100-occurrences-resourceRef-merge-1 :xtm-format '1.0)
		       (check-occurrences-resourceData topic t100-occurrences-resourceData :xtm-format '1.0)
		       (let ((variant-nodes (xpath-child-elems-by-qname
					     (xpath-single-child-elem-by-qname topic *xtm1.0-ns* "baseName")
					     *xtm1.0-ns* "variant")))
			 (is (= (length variant-nodes) 2))
			 (loop for variant across variant-nodes
			    do (let ((variantName
				      (xpath-fn-string
				       (xpath-single-child-elem-by-qname
					(xpath-single-child-elem-by-qname variant *xtm1.0-ns* "variantName")
					*xtm1.0-ns* "resourceData"))))
				 (cond
				   ((string= variantName t100-variant-1-name)
				    (check-variant-xtm1.0 document variant (list core-display-psi)
							  t100-variant-1-name nil))
				   ((string= variantName t100-variant-2-name)
				    (check-variant-xtm1.0 document variant (list core-sort-psi)
							  t100-variant-2-name nil))
				   (t
				    (is-true (format t "bad variantName found in t100: ~A~%" variantName))))))))
		      ((string= href new-t100-psi)
		       (check-topic-id topic)
		       (check-baseNameStrings topic (list new-t100-name))
		       (check-single-instanceOf document topic t3-psi :xtm-format '1.0)
		       (check-occurrences-resourceRef topic new-t100-occurrence-resourceRef-merge-2 :xtm-format '1.0)
		       (check-occurrences-instanceOf document topic (list t55-psi t55-psi))
		       (let ((variant-nodes
			      (xpath-child-elems-by-qname
			       (xpath-single-child-elem-by-qname topic *xtm1.0-ns* "baseName")
			       *xtm1.0-ns* "variant")))
			 (is (= (length variant-nodes) 2))
			 (loop for variant across variant-nodes
			    do (let ((variantName
				      (xpath-fn-string
				       (xpath-single-child-elem-by-qname
					(xpath-single-child-elem-by-qname variant *xtm1.0-ns* "variantName")
					*xtm1.0-ns* "resourceData"))))
				 (cond
				   ((string= variantName new-t100-variant-name)
				    (check-variant-xtm1.0 document variant (list core-sort-psi t50a-psi)
							  new-t100-variant-name nil))
				   ((string= variantName new-t100-variant-name-merge-2)
				    (check-variant-xtm1.0 document variant (list core-display-psi)
							  new-t100-variant-name-merge-2 nil))
				   (t
				    (is-true (format t "bad variantName found in new-t100: ~A~%" variantName))))))))
		      ((string= href t101-psi)
		       (check-topic-id topic)
		       (check-baseNameStrings topic (list t101-name-1 t101-name-2))
		       (loop for name across (xpath-child-elems-by-qname topic *xtm1.0-ns* "baseName")
			  do (let ((baseNameString
				    (xpath-fn-string
				     (xpath-single-child-elem-by-qname name *xtm1.0-ns* "baseNameString"))))
			       (when (string= baseNameString t101-name-2)
				 (let ((scope
				       (let ((scope-nodes (xpath-child-elems-by-qname name *xtm1.0-ns* "scope")))
					 (is (= (length scope-nodes) 1))
					 (let ((topicRef-nodes
						(xpath-child-elems-by-qname (elt scope-nodes 0)
									    *xtm1.0-ns* "topicRef")))
					   (is (= (length topicRef-nodes) 1))
					   (get-subjectIndicatorRef-by-ref document
									   (dom:get-attribute-ns
									    (elt topicRef-nodes 0) *xtm1.0-xlink*
									    "href"))))))
				   (is (string= scope t50a-psi)))
				 (is (= (length (xpath-child-elems-by-qname name *xtm1.0-ns* "type")) 0))
				 (let ((variant-node
					(let ((variant-nodes
					       (xpath-child-elems-by-qname name *xtm1.0-ns* "variant")))
					  (is (= (length variant-nodes) 1))
					  (elt variant-nodes 0))))
				   (check-variant-xtm1.0 document variant-node (list t50a-psi core-sort-psi) t101-variant-name nil)))))
		       (check-single-instanceOf document topic t3a-psi :xtm-format '1.0)
		       (loop for occurrence across (xpath-child-elems-by-qname topic *xtm1.0-ns* "occurrence")
			  do (let ((instanceOf
				    (let ((instanceOf-nodes
					   (xpath-child-elems-by-qname occurrence *xtm1.0-ns* "instanceOf")))
				      (is (= (length instanceOf-nodes) 1))
				      (let ((topicRef-nodes
					     (xpath-child-elems-by-qname (elt instanceOf-nodes 0)
									 *xtm1.0-ns* "topicRef")))
					(is (= (length topicRef-nodes) 1))
					(get-subjectIndicatorRef-by-ref
					 document
					 (dom:get-attribute-ns (elt topicRef-nodes 0) *xtm1.0-xlink* "href"))))))
			       (cond
				 ((string= instanceOf t51-psi)
				  (let ((resourceRef
					 (get-subjectIndicatorRef-by-ref
					  document
					  (dom:get-attribute-ns 
					   (xpath-single-child-elem-by-qname occurrence *xtm1.0-ns* "resourceRef")
					   *xtm1.0-xlink* "href"))))
				    (is (string= resourceRef t52-psi))))
				 ((string= instanceOf t53-psi)
				  (is (= (length
					  (xpath-child-elems-by-qname occurrence *xtm1.0-ns* "resourceData")) 1))
				  (is (= (length
					  (dom:get-attribute-ns
					   (xpath-single-child-elem-by-qname occurrence *xtm1.0-ns* "resourceData")
					   *xtm1.0-xlink* "datatype"))
					 0)))
				 ((string= instanceOf t54-psi)
				  (let ((resourceData-node
					 (xpath-single-child-elem-by-qname occurrence *xtm1.0-ns* "resourceData")))
				    (let ((resourceData
					   (xpath-fn-string resourceData-node))
					  (datatype
					   (dom:get-attribute-ns resourceData-node *xtm1.0-xlink* "datatype")))
				      (is (= (length datatype) 0))
				      (is (string= resourceData (getf t101-occurrence-3-resourceData :data))))))
				 ((string= instanceOf t55-psi)
				  (let ((resourceRef
					 (dom:get-attribute-ns 
					  (xpath-single-child-elem-by-qname occurrence *xtm1.0-ns* "resourceRef")
					  *xtm1.0-xlink* "href")))
				    (is (string= resourceRef t101-occurrence-4-resourceRef))))
				 (t
				  (is-true (format t "bad occurrence instanceOf found in t101: ~A~%" instanceOf)))))))
		      ((string= href t202-psi)
		       (check-topic-id topic)
		       (check-baseNameStrings topic (list t202-name))
		       (check-single-instanceOf document topic t4-psi :xtm-format '1.0))
		      ((string= href t203-psi)
		       (check-topic-id topic)
		       (check-baseNameStrings topic (list t203-name))
		       (check-single-instanceOf document topic t4-psi :xtm-format '1.0))
		      ((string= href t300-psi)
		       (check-topic-id topic)
		       (check-baseNameStrings topic (list t300-name))
		       (check-single-instanceOf document topic t2-psi :xtm-format '1.0))))))
      (let ((matched-associations 0))
	(loop for association across (xpath-child-elems-by-qname document *xtm1.0-ns* "association")
	   do (is (= (length (xpath-child-elems-by-qname association *xtm1.0-ns* "member")) 2))
	      (let ((member-1 (elt (xpath-child-elems-by-qname association *xtm1.0-ns* "member") 0))
		    (member-2 (elt (xpath-child-elems-by-qname association *xtm1.0-ns* "member") 1))
		    (instanceOf
		     (let ((instanceOf-nodes
			    (xpath-child-elems-by-qname association *xtm1.0-ns* "instanceOf")))
		       (is (= (length instanceOf-nodes) 1))
		       (let ((topicRef-nodes
			      (xpath-child-elems-by-qname (elt instanceOf-nodes 0) *xtm1.0-ns* "topicRef")))
			 (is (= (length topicRef-nodes) 1))
			 (get-subjectIndicatorRef-by-ref
			  document (dom:get-attribute-ns (elt topicRef-nodes 0) *xtm1.0-xlink* "href"))))))
		(let ((member-1-type
		       (let ((roleSpec-nodes (xpath-child-elems-by-qname member-1 *xtm1.0-ns* "roleSpec")))
			 (is (= (length roleSpec-nodes) 1))
			 (let ((topicRef-nodes
				(xpath-child-elems-by-qname (elt roleSpec-nodes 0) *xtm1.0-ns* "topicRef")))
			   (is (= (length topicRef-nodes) 1))
			   (get-subjectIndicatorRef-by-ref
			    document (dom:get-attribute-ns (elt topicRef-nodes 0) *xtm1.0-xlink* "href")))))
		      (member-1-ref
		       (let ((topicRef-nodes (xpath-child-elems-by-qname member-1 *xtm1.0-ns* "topicRef")))
			 (is (= (length topicRef-nodes) 1))
			 (get-subjectIndicatorRef-by-ref 
			  document (dom:get-attribute-ns (elt topicRef-nodes 0) *xtm1.0-xlink* "href"))))
		      (member-2-type
		       (let ((roleSpec-nodes (xpath-child-elems-by-qname member-2 *xtm1.0-ns* "roleSpec")))
			 (is (= (length roleSpec-nodes) 1))
			 (let ((topicRef-nodes
				(xpath-child-elems-by-qname (elt roleSpec-nodes 0) *xtm1.0-ns* "topicRef")))
			   (is (= (length topicRef-nodes) 1))
			   (get-subjectIndicatorRef-by-ref
			    document (dom:get-attribute-ns (elt topicRef-nodes 0) *xtm1.0-xlink* "href")))))
		      (member-2-ref
		       (let ((topicRef-nodes (xpath-child-elems-by-qname member-2 *xtm1.0-ns* "topicRef")))
			 (is (= (length topicRef-nodes) 1))
			 (get-subjectIndicatorRef-by-ref 
			  document (dom:get-attribute-ns (elt topicRef-nodes 0) *xtm1.0-xlink* "href")))))
		  (when (and (string= instanceOf t64-psi)
			     (string= member-1-type t63-psi)
			     (string= member-1-ref t300-psi)
			     (string= member-2-type t62-psi)
			     (string= member-2-ref t101-psi))
		    (incf matched-associations)))))
	(is (= matched-associations 2)))))) ; 2 -> new association created after the second merge with the
                                            ; same association but additionally it contains a scope


;; === tests the fragment of the topic new-t100 from notificationbase.xtm, =====
;; === notification_merge1.xtm and notification_merge2.xtm in the latest version
(test test-fragments-xtm1.0-versions
  (with-fixture merge-test-db ()
    (handler-case (delete-file *out-xtm1.0-file*)(error () )) ;deletes file - if exist
    (let ((new-t100
	   (loop for item in (elephant:get-instances-by-class 'd:PersistentIdC)
	      when (string= (uri item) new-t100-psi)
	      return (identified-construct item :revision fixtures::revision3))))
      (d:get-fragments fixtures::revision3)
      (let ((fragment (loop for item in (elephant:get-instances-by-class 'd:FragmentC)
			 when (eq (topic item) new-t100)
			 return item)))
	(with-open-file (stream *out-xtm1.0-file* :direction :output)
	  (write-string (export-xtm-fragment fragment :xtm-format '1.0) stream))))
    (let ((document
	   (dom:document-element
	    (cxml:parse-file *out-xtm1.0-file* (cxml-dom:make-dom-builder)))))
      (check-document-structure document 6 0 :ns-uri *xtm1.0-ns*)
      (loop for topic across (xpath-child-elems-by-qname document *xtm1.0-ns* "topic")
	 do (loop for subjectIndicatorRef across (xpath-child-elems-by-qname
						  (xpath-single-child-elem-by-qname
						   topic *xtm1.0-ns* "subjectIdentity")
						  *xtm1.0-ns* "subjectIndicatorRef")
	       do (let ((href (dom:get-attribute-ns subjectIndicatorRef
						    *xtm1.0-xlink* "href")))
		    (cond
		      ((string= href core-sort-psi)
		       (check-topic-id topic))
		      ((string= href core-display-psi)
		       (check-topic-id topic))
		      ((string= href t50a-psi)
		       (check-topic-id topic))
		      ((string= href t3-psi)
		       (check-topic-id topic))
		      ((string= href t55-psi)
		       (check-topic-id topic))
		      ((string= href new-t100-psi)
		       (check-topic-id topic)
		       (check-single-instanceOf document topic t3-psi :xtm-format '1.0)
		       (loop for occurrence across (xpath-child-elems-by-qname
						    topic *xtm1.0-ns* "occurrence")
			  do (let ((resourceRef
				    (let ((resourceRef-nodes
					   (xpath-child-elems-by-qname
					    occurrence *xtm1.0-ns* "resourceRef")))
				      (is (= (length resourceRef-nodes) 1))
				      (dom:get-attribute-ns (elt resourceRef-nodes 0)
							    *xtm1.0-xlink* "href")))
				   (instanceOf
				    (let ((instanceOf-nodes
					   (xpath-child-elems-by-qname
					    occurrence *xtm1.0-ns* "instanceOf")))
				      (is (= (length instanceOf-nodes) 1))
				      (let ((topicRef-nodes
					     (xpath-child-elems-by-qname
					      (elt instanceOf-nodes 0) *xtm1.0-ns*
					      "topicRef")))
					(is (= (length topicRef-nodes) 1))
					(get-subjectIndicatorRef-by-ref
					 document
					 (dom:get-attribute-ns
					  (elt topicRef-nodes 0) *xtm1.0-xlink* "href"))))))
			       (cond
				 ((string= resourceRef
					   (first new-t100-occurrence-resourceRef-merge-2))
				  (is (string= instanceOf t55-psi)))
				 ((string= resourceRef
					   (second new-t100-occurrence-resourceRef-merge-2))
				  (is (string= instanceOf t55-psi)))
				 (t
				  (is-true
				   (format t "bad resourceRef in new-t100 fragment found: ~A~%" resourceRef)))))))
		      (t
		       (is-true (format t "unknown subjectIndentifier found in fragments: ~A~%" href))))))))))