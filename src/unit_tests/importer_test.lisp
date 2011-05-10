;;+-----------------------------------------------------------------------------
;;+  Isidorus
;;+  (c) 2008-2010 Marc Kuester, Christoph Ludwig, Lukas Georgieff
;;+
;;+  Isidorus is freely distributable under the LLGPL license.
;;+  You can find a detailed description in trunk/docs/LLGPL-LICENSE.txt and
;;+  trunk/docs/LGPL-LICENSE.txt.
;;+-----------------------------------------------------------------------------

(defpackage :importer-test
  (:use 
   :common-lisp
   :xtm-importer
   :base-tools
   :datamodel
   :it.bese.FiveAM
   :unittests-constants
   :fixtures)
  (:import-from :constants
                *xtm2.0-ns*)
  (:import-from :xml-tools
                xpath-child-elems-by-qname
                xpath-select-location-path)
  (:import-from :exceptions
                missing-reference-error
                duplicate-identifier-error
		not-mergable-error )
  (:export :importer-test 
           :test-error-detection
           :run-importer-tests
           :test-from-association-elem
           :test-create-instanceof-association
           :test-from-name-elem
           :test-from-scope-elem 
           :test-from-type-elem 
           :test-from-role-elem 
           :test-from-occurrence-elem 
           :test-merge-topic 
	   :test-setup-repository-xtm1.0
           :test-topic-t100
           :test-topicmaps
	   :test-variants
	   :test-variants-xtm1.0
	   :test-merge-topicmaps
	   :test-merge-topicmaps-xtm1.0))
(declaim (optimize (debug 3) (speed 0) (safety 3) (space 0) (compilation-speed 0)))

(in-package :importer-test)


(def-suite importer-test
     :description "tests  various key functions of the importer")

(in-suite importer-test)

(defvar *T100-TM*
  (dom:document-element
   (cxml:parse-file *t100.xtm* (cxml-dom:make-dom-builder))))

(test test-from-type-elem
  "Test the from-type-elem function of the importer"
  (with-fixture 
      initialized-test-db()
    (let ((type-elems 
	   (xpath-select-location-path
	    *XTM-TM*
	    '((*xtm2.0-ns* "topic")
	      (*xtm2.0-ns* "occurrence")
	      (*xtm2.0-ns* "type"))))
	  (rev-1 *TM-REVISION*))
      (loop for type-elem in type-elems do
           (is (typep (from-type-elem type-elem rev-1) 'TopicC)))
      (is-false (from-type-elem nil rev-1))
      (let
          ((t100-occtype
            (from-type-elem (first type-elems) rev-1)))        
        (format t "occtype: ~a~&" t100-occtype)
        (format t "occtype: ~a~&" (psis t100-occtype))
        (is 
         (string= "http://psi.egovpt.org/types/standardHasStatus"
          (uri (first (psis t100-occtype)))))))))
         

(test test-from-scope-elem
  "Test the from-scope-elem function of the importer"
  (declare (optimize (debug 3)))
  (with-fixture 
      initialized-test-db()
    (let ((scope-elems 
	   (xpath-select-location-path
	    *XTM-TM*
	    '((*xtm2.0-ns* "topic")
	      (*xtm2.0-ns* "name")
	      (*xtm2.0-ns* "scope"))))
	  (rev-1 *TM-REVISION*))
      (loop for scope-elem in scope-elems do
           (is (>= (length (from-scope-elem scope-elem rev-1)) 1)))
      (is-false (from-scope-elem nil rev-1))
      (let
          ((t101-themes
            (from-scope-elem (first scope-elems) rev-1)))
        (is (= 1 (length t101-themes)))
        (is 
         (string=
          (topic-id (first t101-themes) rev-1 *TEST-TM*)
          "t50a"))))))

(test test-from-name-elem
  "Test the from-name-elem function of the importer"
  (with-fixture 
      initialized-test-db()
    (let ((name-elems
	   (xpath-select-location-path
	    *XTM-TM*
	    '((*xtm2.0-ns* "topic")
	      (*xtm2.0-ns* "name"))))
	  (top (get-item-by-id "t1")) ;an arbitrary topic
	  (rev-1 *TM-REVISION*))
      (loop for name-elem in name-elems do
           (is (typep (from-name-elem name-elem top rev-1) 'NameC)))
      (let
          ((t1-name (from-name-elem (first name-elems) top rev-1))
           (t1-name-copy (from-name-elem (first name-elems) top rev-1))
           (t101-longname (from-name-elem (nth 27 name-elems) top rev-1)))
        (is (string= (charvalue t1-name) "Topic Type"))
        (is (string= (charvalue t101-longname) 
		     "ISO/IEC 13250:2002: Topic Maps"))
	(is (= 1 (length (item-identifiers t101-longname :revision rev-1))))
        (is (string= (uri (first (psis (instance-of t101-longname))))
		     "http://psi.egovpt.org/types/long-name"))
        (is (themes t101-longname :revision rev-1))
	(is (string= 
	     (topic-id (first (themes t101-longname :revision rev-1))
		       rev-1 *TEST-TM*)
	     "t50a"))
	(is (eq t1-name t1-name-copy)))))) ;must be merged


(test test-from-occurrence-elem
  "Test the form-occurrence-elem function of the importer"
  (with-fixture 
      initialized-test-db()
    (let ((occ-elems
	   (xpath-select-location-path
	    *XTM-TM*
	    '((*xtm2.0-ns* "topic")
	      (*xtm2.0-ns* "occurrence"))))
	  (top (get-item-by-id "t1")) ;an abritrary topic
	  (rev-1 *TM-REVISION*))
      (loop for occ-elem in occ-elems do
           (is (typep (from-occurrence-elem occ-elem top rev-1)
		      'OccurrenceC)))
      (is (= 1 (length (elephant:get-instances-by-value 
			'ItemIdentifierC
			'uri
			"http://psi.egovpt.org/itemIdentifiers#t100_o1"))))
      (let
          ((t100-occ1
            (identified-construct
             (elephant:get-instance-by-value 
              'ItemIdentifierC
              'uri
              "http://psi.egovpt.org/itemIdentifiers#t100_o1")))
           (t100-occ2
            (identified-construct
             (elephant:get-instance-by-value 
              'ItemIdentifierC
              'uri
              "http://psi.egovpt.org/itemIdentifiers#t100_o2"))))
	(is (= 1 (length (item-identifiers t100-occ1 :revision rev-1)))) ;just to double-check
        (is (string=
             (uri (first (item-identifiers t100-occ1 :revision rev-1)))
             "http://psi.egovpt.org/itemIdentifiers#t100_o1"))
        (is (string= (charvalue t100-occ1) "http://www.budabe.de/"))
        (is (string= (datatype t100-occ1) "http://www.w3.org/2001/XMLSchema#anyURI"))
        (is (string= (datatype t100-occ2)
                     "http://www.w3.org/2001/XMLSchema#string"))))))

(test test-merge-topic
  "Test the merge-topic-elem function of the importer"
  (with-fixture 
      initialized-test-db()
    (let ((topic-elems
	   (xpath-select-location-path
	    *XTM-TM*
	    '((*xtm2.0-ns* "topic"))))
	  (rev-1 *TM-REVISION*))
      (loop for topic-elem in topic-elems do
           (is (typep 
                (merge-topic-elem topic-elem rev-1 :tm fixtures::tm)
                    'TopicC)))
      (let
          ((top-t1 (merge-topic-elem (first topic-elems) 
                                     rev-1 :tm fixtures::tm))
           (top-t57 (get-item-by-id "t57"))
           (top-t101 (get-item-by-id "t101"))
           (top-t301 (get-item-by-id "t301"))
           (top-t301a (get-item-by-id "t301a"))
           ;one of the core PSIs
           (top-sup-sub (get-item-by-id "supertype-subtype" :xtm-id "core.xtm")))
	(is (= (elephant::oid top-t301) (elephant::oid top-t301a)))
	(is-true top-t301a)
        (is (= (length (occurrences top-t1 :revision rev-1)) 0))
	(is (= (length (occurrences top-t101 :revision rev-1)) 4))
        (is (= (length (names top-t57 :revision rev-1)) 1))
        (is (string= (uri (first (item-identifiers top-t57 :revision rev-1)))
                     "http://psi.egovpt.org/itemIdentifiers#t57"))
        (is (= 2 (length (names top-t101 :revision rev-1))))
        (is (= 2 (length (names top-t301 :revision rev-1)))) ;after merge
        (is-true (item-identifiers (first (names top-t301 :revision rev-1))
				   :revision rev-1)) ;after merge
        (is (= 2 (length (psis top-t301 :revision rev-1)))) ;after merge
        (is (= 3 (length (occurrences top-t301 :revision rev-1)))) ;after merge
        (is (string= "http://www.topicmaps.org/xtm/1.0/core.xtm#supertype-subtype"
                     (uri (first (psis top-sup-sub :revision rev-1)))))))
    ;34 topics in 35 topic elements in notificationbase.xtm and 14
    ;core topics
    (is (= (+ 34 14) (length (elephant:get-instances-by-class 'TopicC))))))

(test test-from-role-elem
  "Test the form-role-elem function of the importer"
  (with-fixture 
      initialized-test-db()
    (let 
        ((role-elems
          (xpath-select-location-path
           *XTM-TM*
           '((*xtm2.0-ns* "association")
             (*xtm2.0-ns* "role"))))
	 (rev-1 *TM-REVISION*))
      (loop for role-elem in role-elems do
           (is (typep (from-role-elem role-elem revision) 'list)))
      (let 
          ((12th-role
            (from-role-elem (nth 11 role-elems) revision)))
        (is (string= "t101" 
                     (topic-id 
                      (getf 12th-role :player) rev-1 *TEST-TM*))) 
        (is (string=  "t62" 
                      (topic-id
                       (getf 12th-role :instance-of) rev-1 *TEST-TM*)))))))


(test test-from-association-elem
  "Test the form-association-elem function of the importer"
  (with-fixture 
      initialized-test-db()
    (let ((assoc-elems
	   (xpath-select-location-path
	    *XTM-TM*
	    '((*xtm2.0-ns* "association"))))
	  (rev-1 *TM-REVISION*))
      (loop for assoc-elem in assoc-elems do
           (is 
            (typep (from-association-elem assoc-elem rev-1 :tm fixtures::tm)
                'AssociationC)))
      (let ((6th-assoc
	     (sixth (elephant:get-instances-by-class 'AssociationC)))
	    (last-assoc
	     (seventh (elephant:get-instances-by-class 'AssociationC))))
        (is (= 2 (length (roles last-assoc :revision rev-1))))
        (is (= 1 (length (item-identifiers last-assoc :revision rev-1))))
        (is (string= "t300"
             (topic-id (player (first (roles 6th-assoc :revision rev-1))
			       :revision rev-1) rev-1 *TEST-TM*)))
        (is (string= "t63" 
             (topic-id (instance-of (first (roles 6th-assoc :revision rev-1))
				    :revision rev-1) rev-1 *TEST-TM*)))
        (is (string= "t301" 
             (topic-id (player (first (roles last-assoc :revision rev-1))
			       :revision rev-1) rev-1 *TEST-TM*)))))
    (is (= 7
           (length (elephant:get-instances-by-class 'AssociationC))))))
                      
             
(test test-create-instanceof-association
  "Test the creation of instanceof associations"
  (declare (optimize (debug 3)))
  (with-fixture 
      initialized-test-db()
    (let ((topic-elems
	   (xpath-select-location-path
	    *XTM-TM*
	    '((*xtm2.0-ns* "topic"))))
	  (rev-1 *TM-REVISION*))
      (loop for topic-elem in topic-elems do
           (let (;this already implicitly creates the instanceOf
                 ;associations as needed
		 (topic (merge-topic-elem topic-elem rev-1 :tm fixtures::tm)))
	      (dolist (io-role (map 'list #'d::parent-construct
				    (d::slot-p topic 'd::player-in-roles)))
		(let ((io-assoc (parent io-role :revision rev-1)))
		  (is (typep io-assoc 'AssociationC))
		  (is (string= (topic-id topic rev-1)
			       (topic-id (player (second
						  (roles io-assoc :revision rev-1))
						 :revision rev-1) rev-1)))))))
      (let* ((t101-top (get-item-by-id "t101" :revision rev-1))
                                        ;get all the roles t101 is involved in
	     (roles-101 (map 'list #'d::parent-construct
			     (d::slot-p t101-top 'd::player-in-roles)))
                                        ;and filter those whose roletype is "instance"
                                        ;(returning, of course, a list)
                                        ;TODO: what we'd really need
                                        ;is a filter that works
                                        ;directly on the indices
                                        ;rather than instantiating
                                        ;many unnecessary role objects
	     (role-101 (remove-if-not 
			(lambda (role)
			  (string= (uri (first (psis
						(instance-of role :revision rev-1)
						:revision rev-1)))
				   "http://psi.topicmaps.org/iso13250/model/instance"))
			roles-101)))
                                        ;Topic t101 (= Topic Maps 2002
                                        ;standard) is subclass of
                                        ;topic t3a (semantic standard)
        (is-true t101-top)
        (is (= 1 (length role-101)))
        (is (string= "t3a"
                     (topic-id (player (first (roles (parent (first role-101))
						     :revision rev-1))
				       :revision rev-1)
			       rev-1 *TEST-TM*)))
        (is (string= "type-instance"
                     (topic-id (instance-of 
				(parent (first role-101) :revision rev-1))
			       rev-1 "core.xtm")))))))


(test test-error-detection
  "Test for the detection of common errors such as dangling
   references, duplicate PSIs or item identifiers"
  (declare (optimize (debug 3)))
  (with-fixture bare-test-db()
    (signals missing-reference-error
      (let 
          ((di-xtm-dom
            (dom:document-element
             (cxml:parse-file *dangling_instanceof.xtm* (cxml-dom:make-dom-builder)))))
        (importer di-xtm-dom :xtm-id  "missing-reference-error-1"
                  :tm-id "http://www.isidor.us/unittests/baretests"))))
  (with-fixture bare-test-db()
    (signals missing-reference-error
      (let 
          ((xtm-dom
            (dom:document-element
             (cxml:parse-file *dangling_topicref.xtm* (cxml-dom:make-dom-builder)))))
        (importer xtm-dom :xtm-id "missing-reference-error-2"
                  :tm-id "http://www.isidor.us/unittests/baretests"))))
  (with-fixture bare-test-db()
    (signals not-mergable-error
      (let 
          ((xtm-dom
            (dom:document-element
             (cxml:parse-file *duplicate_identifier.xtm* (cxml-dom:make-dom-builder)))))
        (importer xtm-dom :xtm-id "duplicate-identifier-error-1"
                  :tm-id "http://www.isidor.us/unittests/baretests")))))


(test test-topic-t100
  "test for the entire topic t100. checks all slot values and references"
  (let
      ((dir "data_base"))
    (with-fixture initialize-destination-db (dir)
      (xtm-importer:setup-repository *t100.xtm* dir :xtm-id *TEST-TM*
                                     :tm-id "http://www.isidor.us/unittests/topic-t100")
      (open-tm-store dir)
      (is (= 26 (length (elephant:get-instances-by-class 'TopicC)))) ;; are all topics in the db + std topics
      (is-true (get-item-by-id "t100" :revision 0)) ;; main topic
      (is-true (get-item-by-id "t3a" :revision 0))  ;; instanceOf
      (is-true (get-item-by-id "t50a" :revision 0)) ;; scope
      (is-true (get-item-by-id "t51" :revision 0))   ;; occurrence/type
      (is-true (get-item-by-id "t52" :revision 0))   ;; occurrence/resourceRef
      (is-true (get-item-by-id "t53" :revision 0))   ;; occurrence/type
      (is-true (get-item-by-id "t54" :revision 0))   ;; occurrence/type
      (is-true (get-item-by-id "t55" :revision 0))  ;; occurrence/type
      (let ((t100 (get-item-by-id "t100" :revision 0)))
	;; checks instanceOf
	(is (= 1 (length (player-in-roles t100 :revision 0))))
	(let* ((role-t100 (first (player-in-roles t100 :revision 0)))
	       (assoc (parent role-t100 :revision 0))
	       (role-t3a (first (roles assoc :revision 0))))
	  (is (= 1 (length (psis (instance-of role-t100 :revision 0) :revision 0))))
	  (is (string= (uri (first (psis (instance-of role-t100 :revision 0)
					 :revision 0)))
		       "http://psi.topicmaps.org/iso13250/model/instance"))
	  (is (= 1 (length (psis (instance-of role-t3a :revision 0) :revision 0))))
	  (is (string= (uri (first (psis (instance-of role-t3a :revision 0)
					 :revision 0)))
		       "http://psi.topicmaps.org/iso13250/model/type")))
	;; checks subjectIdentifier
	(is (= 1 (length (psis t100 :revision 0))))
	(is (string= "http://psi.egovpt.org/standard/ISO+19115%3A+Geographic+Information+-+Metadata"
		     (uri (first (psis t100 :revision 0)))))
	(is (equal (identified-construct (first (psis t100 :revision 0))
					 :revision 0) t100)) ;;other association part
	;; checks names
	(is (= 2 (length (names t100 :revision 0))))
	(loop for item in (names t100 :revision 0)
	   do (is (or (string= (charvalue item) "ISO 19115")
		      (and (string= (charvalue item) "ISO 19115:2003 Geographic Information - Metadata")
			   (= (length (themes item :revision 0)) 1)
			   (= (length (psis (first (themes item :revision 0))
					    :revision 0)))
			   (string= (uri (first (psis (first (themes item :revision 0))
						      :revision 0)))
				    "http://psi.egovpt.org/types/long-name")))))
   	(is-true (used-as-theme (get-item-by-id "t50a" :revision 0)
				:revision 0)) ;checks the other part of the association -> fails
	;; checks occurrences
	(setf *TM-REVISION* 0)
	(is (= 4 (length (occurrences (get-item-by-id "t100")))))
	(loop for item in (occurrences t100)
	   when (elephant:associatedp (get-item-by-id "t51") 'datamodel::used-as-type item)
 	   do (progn
		(is (string= (charvalue item) "#t52"))
		(is (string= (uri (first (psis (instance-of item)))) "http://psi.egovpt.org/types/standardHasStatus")))
	   when (elephant:associatedp (get-item-by-id "t53") 'datamodel::used-as-type item)
	   do (progn
		(is (string= (charvalue item) "The ISO 19115 standard ..."))
 		(is (string= (datatype item) "http://www.w3.org/2001/XMLSchema#string"))
		(is (string= (uri (first (psis (instance-of item)))) "http://psi.egovpt.org/types/description")))
	   when (elephant:associatedp (get-item-by-id "t54") 'datamodel::used-as-type item)
 	   do (progn
 		(is (string= (charvalue item) "2003-01-01"))
 		(is (string= (datatype item) "http://www.w3.org/2001/XMLSchema#date"))
		(is (string= (uri (first (psis (instance-of item)))) "http://psi.egovpt.org/types/standardValidFromDate")))
	   when (elephant:associatedp (get-item-by-id "t55") 'datamodel::used-as-type item)
 	   do (progn
		(is (string= (charvalue item) "http://www.editeur.org/standards/ISO19115.pdf"))
		(is (string= (uri (first (psis (instance-of item)))) "http://psi.egovpt.org/types/links"))))))))


(test test-setup-repository-xtm1.0
  "tests the importer-xtm1.0 functions"
  (let
      ((dir "data_base"))
    (with-fixture initialize-destination-db (dir)
      (xtm-importer:setup-repository 
       *sample_objects.xtm* dir 
       :tm-id "http://www.isidor.us/unittests/xtm1.0-tests"
       :xtm-id *TEST-TM* :xtm-format :1.0)
      (setf *TM-REVISION* 0)
      (open-tm-store dir)
      ;14 + (23 core topics)
      (is (=  37 (length (elephant:get-instances-by-class 'TopicC))))
      ;2 + (11 instanceOf)
      (is (= 13 (length (elephant:get-instances-by-class 'AssociationC))))
      ;4 + (22 instanceOf-associations)
      (is (= 26 (length (elephant:get-instances-by-class 'RoleC))))
      ;23 + (14 core topics)
      (is (= 37 (length (elephant:get-instances-by-class 'PersistentIdC))))
      (is (= 0 (length (elephant:get-instances-by-class 'SubjectLocatorC))))
      ;2 + (0 core topics)
      (is (= 2 (length (elephant:get-instances-by-class 'OccurrenceC))))
      ;18 + (0 core topics)
      (is (= 18 (length (elephant:get-instances-by-class 'NameC))))
      (let ((t-2526 (get-item-by-id "t-2526"))
	    (t-2656 (get-item-by-id "t-2656"))
	    (assoc (first (used-as-type (get-item-by-id "t89671052499")))))
	(is (= (length (player-in-roles t-2526)) 1))
	(is (= (length (psis t-2526)) 1))
	(is (string= (uri (first (psis t-2526)))
		     "http://psi.egovpt.org/types/serviceUsesTechnology"))
	(is (= (length (names t-2526)) 3))
	(is (or (string= (charvalue (first (names t-2526)))
			 "service uses technology")
		(string= (charvalue (second (names t-2526)))
			 "service uses technology")
		(string= (charvalue (third (names t-2526)))
			 "service uses technology")))
	(is (or (string= (charvalue (first (names t-2526)))
			 "uses technology")
		(string= (charvalue (second (names t-2526)))
			 "uses technology")
		(string= (charvalue (third (names t-2526)))
			 "uses technology")))
	(is (or (string= (charvalue (first (names t-2526)))
			 "used by service")
		(string= (charvalue (second (names t-2526)))
			 "used by service")
		(string= (charvalue (third (names t-2526)))
			 "used by service")))
	(loop for name in (names t-2526)
	   when (string= (charvalue name) "uses technology")
	   do (is (= (length (themes name)) 1))
 	      (is (eq (first (themes name)) (get-item-by-id "t-2555")))
	   when (string= (charvalue name) "used by service")
	   do (is (= (length (themes name)) 1))
 	      (is (eq (first (themes name)) (get-item-by-id "t-2593"))))
	(is (= (length (player-in-roles t-2656)) 2)) ;association + instanceOf
	(is (= (length (psis t-2656)) 1))
	(is (string= (uri (first (psis t-2656)))
		     "http://psi.egovpt.org/types/DO-NOT-SIGNAL-no-identifier-error"))
	(is (= (length (occurrences t-2656)) 2))
	(loop for occ in (occurrences t-2656)
	   when (eq (instance-of occ) (get-item-by-id "t-2625"))
	   do (is (string= (charvalue occ) "0"))
	      (is (string= (datatype occ)
			   "http://www.w3.org/2001/XMLSchema#string"))
	   when (eq (instance-of occ) (get-item-by-id "t-2626"))
	   do (is (string= (charvalue occ) "unbounded"))
	      (is (string= (datatype occ)
			   "http://www.w3.org/2001/XMLSchema#string"))
	   when (not (or (eq (instance-of occ) (get-item-by-id "t-2625"))
			 (eq (instance-of occ) (get-item-by-id "t-2626"))))
	   do (is-true (format t "bad occurrence found in t-2526")))
	(is (= (length (roles assoc)) 2))
	(loop for role in (roles assoc)
	   when (eq (player role) (get-item-by-id "all-subjects"))
	   do (is (eq (instance-of role) (get-item-by-id "broader-term")))
	   when (eq (player role) (get-item-by-id "t1106723946"))
	   do (is (eq (instance-of role) (get-item-by-id "narrower-term")))
	   when (not (or (eq (player role) (get-item-by-id "all-subjects"))
			 (eq (player role) (get-item-by-id "t1106723946"))))
	   do (is-true (format t "bad role found in association: ~A"
			       (topic-identifiers (player role)))))))))


(test test-variants
  (let
      ((dir "data_base"))
    (with-fixture initialize-destination-db (dir)
      (xtm-importer:setup-repository
       *notificationbase.xtm* dir :xtm-id *TEST-TM*
       :tm-id "http://isidorus.org/test-tm")
      (setf *TM-REVISION* 0)
      (open-tm-store dir)
      (let ((variants (elephant:get-instances-by-class 'VariantC)))
	(is (= (length variants) 4))
	(loop for variant in variants
	   do (let ((resourceData (charvalue variant))
		    (d-type (datatype variant))
		    (string-type "http://www.w3.org/2001/XMLSchema#string")
		    (itemIdentities (map 'list #'uri (item-identifiers variant)))
		    (parent-name-value (charvalue (parent variant)))
		    (scopes (map 'list #'uri
				  (map 'list #'(lambda(x)
						 (first (psis x))) ;these topics have only one psi
				       (themes variant))))
		    (sort-psi "http://www.topicmaps.org/xtm/1.0/core.xtm#sort")
		    (display-psi "http://www.topicmaps.org/xtm/1.0/core.xtm#display")
		    (t50a-psi "http://psi.egovpt.org/types/long-name"))
		(cond
		  ((string= resourceData "Long-Version")
		   (is (string= parent-name-value "long version of a name"))
		   (is (= (length (variants (parent variant))) 1))
		   (is (eql variant (first (variants (parent variant)))))
		   (check-for-duplicate-identifiers variant)
		   (is-false itemIdentities)
		   (is (= (length scopes) 1))
		   (is (string= (first scopes) sort-psi))
		   (is (string= d-type string-type)))
		  ((string= resourceData "Geographic Information - Metadata")
		   (is (string= parent-name-value "ISO 19115"))
		   (is (= (length (variants (parent variant))) 2))
		   (is (or (eql variant (first (variants (parent variant))))
			   (eql variant (second (variants (parent variant))))))
		   (check-for-duplicate-identifiers variant)
		   (is (= (length scopes) 1))
		   (is (string= (first scopes) display-psi))
		   (is (= (length itemIdentities) 1))
		   (is (string= (first itemIdentities)
				"http://psi.egovpt.org/itemIdentifiers#t100_n1_v1"))
		   (is (string= d-type string-type)))
		  ((string= resourceData "ISO-19115")
		   (check-for-duplicate-identifiers variant)
		   (is (= (length itemIdentities) 1))
		   (is (string= (first itemIdentities)
				"http://psi.egovpt.org/itemIdentifiers#t100_n1_v2"))
		   (is (= (length scopes) 1))
		   (is (string= (first scopes) sort-psi))
		   (is (string= d-type string-type)))
		  ((string= resourceData "ISO/IEC-13250:2002")
		   (is (string= parent-name-value "ISO/IEC 13250:2002: Topic Maps"))
		   (is (= (length (variants (parent variant))) 1))
		   (is (eql variant (first (variants (parent variant)))))
		   (check-for-duplicate-identifiers variant)
		   (check-for-duplicate-identifiers variant)		   
		   (is (= (length scopes) 2))
		   (is (or (string= (first scopes) t50a-psi)
			   (string= (first scopes) sort-psi)))
		   (is (or (string= (second scopes) t50a-psi)
			   (string= (second scopes) sort-psi)))
		   (is (= (length itemIdentities) 2))
		   (is (or (string= (first itemIdentities)
				    "http://psi.egovpt.org/itemIdentifiers#t101_n2_v1")
			   (string= (first itemIdentities)
				    "http://psi.egovpt.org/itemIdentifiers#t101_n2_v2")))
		   (is (or (string= (second itemIdentities)
				    "http://psi.egovpt.org/itemIdentifiers#t101_n2_v1")
			   (string= (second itemIdentities)
				    "http://psi.egovpt.org/itemIdentifiers#t101_n2_v2")))
		   (is (string= d-type string-type)))
		  (t
		   (is-true (format t "found bad resourceData in variant object: ~A~%" resourceData))))))))))



(test test-variants-xtm1.0
  "tests the importer-xtm1.0 -> variants"
  (let ((dir "data_base"))
    (with-fixture initialize-destination-db (dir)
      (xtm-importer:setup-repository 
       *sample_objects.xtm* dir :xtm-id *TEST-TM* :xtm-format :1.0
       :tm-id "http://isidorus.org/test-tm")
      (open-tm-store dir)
      (is (= (length (elephant:get-instances-by-class 'VariantC)) 5))
      (let ((t-2526 (get-item-by-id "t-2526")))
	(loop for baseName in (names t-2526)
	   do (let ((baseNameString (charvalue baseName))
		    (name-variants (variants baseName)))
		(loop for variant in name-variants
		   do (is (string= (datatype variant)
				   "http://www.w3.org/2001/XMLSchema#string")))
		(cond
		  ((string= baseNameString "service uses technology")
		   (is (= (length name-variants) 2))
		   (loop for variant in name-variants
		      do (is (eql baseName (parent variant)))
			 (let ((variantName (charvalue variant)))
			   (cond
			     ((string= variantName "service-uses-technology")
			      (is (= (length (themes variant)) 1))
			      (is (eql (first (themes variant))
				       (get-item-by-id "sort"))))
			     ((string= variantName "service uses technology")
			      (is (= (length (themes variant)) 1))
			      (is (eql (first (themes variant))
				       (get-item-by-id "display"))))
			     (t
			      (is-true (format t "basevariantName found in t-2526: ~A~%" variantName)))))))  
		  ((string= baseNameString "uses technology")
		   (is (= (length name-variants) 2))
		   (loop for variant in name-variants
		      do (is (eql baseName (parent variant)))
			 (let ((variantName (charvalue variant)))
			   (cond
			     ((string= variantName "uses technology")
			      (is (= (length (themes variant)) 2))
			      (is-true (find (get-item-by-id "t-2555")
					     (themes variant) :test #'eql))
			      (is-true (find (get-item-by-id "display")
					     (themes variant) :test #'eql)))
			     ((string= variantName "uses-technology")
			      (is (= (length (themes variant)) 3))
			      (is-true (find (get-item-by-id "t-2555")
					     (themes variant) :test #'eql))
			      (is-true (find (get-item-by-id "display")
					     (themes variant) :test #'eql))
			      (is-true (find (get-item-by-id "sort")
					     (themes variant) :test #'eql)))
			     (t
			      (is-true (format t "bad variantName found in t-2526: ~A~%" variantName)))))))
		  ((string= baseNameString "used by service")
		   (is (= (length name-variants) 1))
		   (loop for variant in name-variants
		      do (is (eql baseName (parent variant)))
			 (is (string= (charvalue variant) "used-by-service"))
			 (is (= (length (themes variant)) 3))
			 (is-true (find (get-item-by-id "t-2593")
					(themes variant) :test #'eql))
 			 (is-true (find (get-item-by-id "display")
					(themes variant) :test #'eql))
			(is-true (find (get-item-by-id "sort")
				       (themes variant) :test #'eql))))
		  (t
		   (is-true (format t "bad baseNameString found in names of t-2526: ~A~%" baseNameString))))))))))


(test test-topicmaps
  "Test the working of the TopicMap class"
  (with-fixture 
      initialized-test-db()  
    (let
        ((tms (elephant:get-instances-by-class 'd:TopicMapC)))
      (is (= 2 (length tms)))
      (is-false 
       (set-exclusive-or 
        '("http://www.isidor.us/unittests/testtm" 
          "http://www.topicmaps.org/xtm/1.0/core.xtm")
        (mapcan (lambda (tm) 
                 (mapcar #'uri (item-identifiers tm :revision 0)))
                tms) :test #'string=)))))


(test test-merge-topicmaps
  (let ((dir "data_base")
	(tm-id-1 "tm-id-1")
	(tm-id-2 "tm-id-2"))
    (with-fixture with-empty-db (dir)
      (xtm-importer:setup-repository *poems_light_tm_ii.xtm*
				     dir :tm-id tm-id-1)
      (xtm-importer:import-from-xtm *poems_light_tm_ii_merge.xtm*
				    dir :tm-id tm-id-2)
      (with-revision 0
	(let ((tm-1
	       (d:identified-construct
		(first (elephant:get-instances-by-value
			'd:ItemIdentifierC 'd:uri tm-id-1))))
	      (tm-2
	       (d:identified-construct
		(first (elephant:get-instances-by-value
			'd:ItemIdentifierC 'd:uri tm-id-2)))))
	  (is-true tm-1)
	  (is-true tm-2)
	  (is (eql tm-1 tm-2))
	  (is-false (set-exclusive-or (map 'list #'d:uri (item-identifiers tm-1))
				      (list tm-id-1 tm-id-2
					    "http://some.where/poems_light_tm_ii_1"
					    "http://some.where/poems_light_tm_ii_2")
				      :test #'string=))
	  (is (eql (reifier tm-1)
		   (d:get-item-by-item-identifier
		    "http://some.where/poems/topicMap-reifier")))
	  (is (= (length (d:topics tm-1)) (+ 9 3)))
	  (is (= (length (d:associations tm-1)) (+ 1 3)))
	  (is (= (length (d:in-topicmaps (d:get-item-by-id "schiller"))) 1))
	  (is (eql (first (d:in-topicmaps (d:get-item-by-id "schiller"))) tm-1))
	  

	  (let ((schiller-1 (d:get-item-by-id
			     "schiller"
			     :revision (first (last (d:get-all-revisions)))))
		(schiller-2 (d:get-item-by-id
			     "schiller"
			     :revision (elt (d:get-all-revisions)
					    (- (length (d:get-all-revisions)) 2)))))
	    (is-true schiller-1)
	    (is-false schiller-2)))))))


(test test-merge-topicmaps-xtm1.0
  (let ((dir "data_base")
	(tm-id-1 "tm-id-1"))
    (with-fixture with-empty-db (dir)
      (xtm-importer:setup-repository *poems_light_tm_reification_xtm1.0.xtm*
				     dir :tm-id tm-id-1 :xtm-format :1.0)
      (open-tm-store  dir)
      (with-revision 0
	(let ((tm-1
	       (d:identified-construct
		(first (elephant:get-instances-by-value
			'd:ItemIdentifierC 'd:uri tm-id-1)))))
	  (is-true tm-1)
	  (is (= (length (topics tm-1)) (+ 8 3)))
	  (is (= (length (associations tm-1)) (+ 1 2)))
	  (is (eql (reifier tm-1)
		   (get-item-by-psi "#tm-reifier"))))))))


(defun run-importer-tests ()
  (run! 'importer-test))

  
