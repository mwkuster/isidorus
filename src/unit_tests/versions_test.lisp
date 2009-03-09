;-*- standard-indent: 2; indent-tabs-mode: nil -*-
(defpackage :versions-test
  (:use 
   :common-lisp
   :xml-importer
   :datamodel
   :it.bese.FiveAM
   :fixtures
   :unittests-constants)
  (:import-from :constants
                *xtm2.0-ns*)
  (:import-from :xml-tools
                xpath-child-elems-by-qname
                xpath-select-location-path)
  (:import-from :exceptions
                missing-reference-error
                duplicate-identifier-error)
  (:export :test-get-item-by-id-t100
           :test-get-item-by-id-t301
           :test-get-item-by-id-common-lisp
           :test-mark-as-deleted
           :test-norwegian-curriculum-association
           :test-change-lists
           :test-changed-p
           :versions-test
           ))
(declaim (optimize (debug 3)))

(in-package :versions-test)

(def-suite versions-test
    :description "tests  various key functions of the importer")

(in-suite versions-test)

(test test-get-item-by-id-t100 ()
      "test certain characteristics of
http://psi.egovpt.org/standard/ISO+19115%3A+Geographic+Information+-+Metadata
of which two revisions are created, the original one and then one during the
merge with *XTM-MERGE1*"
      (with-fixture merge-test-db ()

        (let
            ((top-t100-current (get-item-by-id "t100" :xtm-id *TEST-TM*))
             (top-t100-first (get-item-by-id "t100" :xtm-id *TEST-TM* :revision fixtures::revision1))
             (top-t100-second (get-item-by-id "t100" :xtm-id *TEST-TM* :revision fixtures::revision2))
             (link-topic (get-item-by-id "t55" :xtm-id *TEST-TM* :revision fixtures::revision2)))

          (is (eq top-t100-current top-t100-second))
          (is (eq top-t100-current top-t100-first))

          (is (= 2 (length (names top-t100-current))))
          (with-revision fixtures::revision1
            (is (= 1 (length (names top-t100-first)))))
          (is (string= (charvalue (first (names top-t100-first)))
                       "ISO 19115"))
          (with-revision fixtures::revision2 
            (is (= 2 (length (names top-t100-second))))
            (is (= 5 (length (occurrences top-t100-second))))
            (is (eq link-topic (get-item-by-id "t50" :xtm-id "merge1"))) ;the topic with t55 in notificationbase has the id t50 in merge1
            (is (eq link-topic (instance-of (fifth (occurrences top-t100-second))))))

          (is (string= (charvalue (first (names top-t100-second)))
                       "ISO 19115"))
          (is (string= (charvalue (second (names top-t100-second)))
                       "Geo Data"))

          (is (= 5 (length (occurrences top-t100-current))))
          (is (= 2 (length (item-identifiers top-t100-current))))
    
          (with-revision fixtures::revision1
            (is (= 4 (length (occurrences top-t100-first))))
            (is (= 1 (length (item-identifiers top-t100-first)))))

          (is (= 2 (length (elephant:get-instances-by-class 'd:TopicMapC)))))))

(test test-get-item-by-id-t301 ()
      "test characteristics of http://psi.egovpt.org/service/Google+Maps which
occurs twice in notificationbase.xtm but is not subsequently revised"
      (with-fixture merge-test-db ()
        (let 
            ((top-t301-current (get-item-by-id "t301" :xtm-id *TEST-TM*)) 
             (top-t301-first (get-item-by-id "t301a" :xtm-id *TEST-TM* :revision fixtures::revision1))
             (top-t301-second (get-item-by-id "t301a" :xtm-id *TEST-TM* :revision fixtures::revision2)))

          (is (eq top-t301-current top-t301-first))
          (is (eq top-t301-current top-t301-second)))))

(test test-get-item-by-id-common-lisp ()
      "Get the http://psi.egovpt.org/standard/Common+Lisp topic that was first
introduced in merge1 and then modified in merge2"
      (with-fixture merge-test-db ()
        (let
            ((top-cl-current (get-item-by-id "t100" :xtm-id "merge2"))
             (top-cl-first (get-item-by-id "t100" :xtm-id  "merge2" :revision fixtures::revision1))
             (top-cl-second (get-item-by-id "t100" :xtm-id "merge2" :revision fixtures::revision2)))
          (is-false top-cl-first) ;did not yet exist then and should thus be nil
          (is (eq top-cl-second top-cl-current))
          (is (= 1 (length (names top-cl-current))))
          (with-revision fixtures::revision2
            (is (= 1 (length (item-identifiers top-cl-second)))))
          (is (= 2 (length (item-identifiers top-cl-current))))
          (with-revision fixtures::revision2
            (is (= 1 (length (occurrences top-cl-second)))))
          (is (= 2 (length (occurrences top-cl-current)))))))
  

;; tests for: - history of roles and associations
;;            - get list of all revisions
;;            - get changes

(test test-norwegian-curriculum-association ()
      "Check the various incarnations of the norwegian curriculum
associations across its revisions"
      (with-fixture merge-test-db ()
        (let*
            ((norwegian-curr-topic
              (get-item-by-id "t300" :xtm-id *TEST-TM*))
             
             (curriculum-assoc ;this is the only "true" association in which the
                                        ;Norwegian Curriculum is a player in revision1
              (parent 
               (second    ;the first one is the instanceOf association
                (player-in-roles 
                 norwegian-curr-topic))))
             (scoped-curriculum-assoc  ;this one is added in revision3
              (parent 
               (third 
                (player-in-roles 
                 norwegian-curr-topic))))
             (semantic-standard-topic
              (get-item-by-id "t3a" :xtm-id *TEST-TM*)))
          (is (string= "http://psi.egovpt.org/service/Norwegian+National+Curriculum"
                       (uri (first (psis norwegian-curr-topic)))))
          (is (= 1 (length (item-identifiers curriculum-assoc))))
          (is (= 3 (length (psis semantic-standard-topic))))

          (with-revision fixtures::revision1
                                        ;one explicit association and the association resulting
                                        ;from instanceOf
            (is (= 2 (length (player-in-roles norwegian-curr-topic))))
            (is-false (item-identifiers curriculum-assoc))
            (is-false (used-as-theme semantic-standard-topic))
            )
          (with-revision fixtures::revision2
                                        ;one explicit association and the association resulting
                                        ;from instanceOf
            (is (= 2 (length (player-in-roles norwegian-curr-topic))))
            (is (= 1 (length (item-identifiers curriculum-assoc))))
            (is (= 1 (length (item-identifiers (first  (roles curriculum-assoc))))))
            (is (= 2 (length (item-identifiers (second (roles curriculum-assoc))))))
            (is-false (used-as-theme semantic-standard-topic)))

          (with-revision fixtures::revision3
                                        ;two explicit associations and the association resulting
                                        ;from instanceOf
            (is (= 3 (length (player-in-roles norwegian-curr-topic))))
            (is (= 1 (length (item-identifiers curriculum-assoc))))
            (is (eq semantic-standard-topic (first (themes scoped-curriculum-assoc))))
            (is (= 1 (length (used-as-theme semantic-standard-topic))))
            (is (= 1 (length (item-identifiers (first  (roles curriculum-assoc))))))
            (is (= 3 (length (item-identifiers (second (roles curriculum-assoc))))))))))


(test test-instance-of-t64 ()
      "Check if all instances of t64 are properly registered."
      (with-fixture merge-test-db ()
        (let
            ((t63  (get-item-by-id "t63" :xtm-id *TEST-TM*))
             (t64  (get-item-by-id "t64" :xtm-id *TEST-TM*))
             (t300 (get-item-by-id "t300" :xtm-id *TEST-TM*)))
          (with-revision fixtures::revision1
            (let
                ((assocs (used-as-type t64)))
              (is (= 2 (length assocs)))
              (is (= (internal-id t63)
                     (internal-id (instance-of (first (roles (first assocs)))))))
              (is (= (internal-id t300)
                     (internal-id (player (first (roles (first assocs)))))))))
          (with-revision fixtures::revision2
            (let
                ((assocs (used-as-type t64)))
              (is (= 2 (length assocs)))))
          (with-revision fixtures::revision3
            (let
                ((assocs (used-as-type t64)))
              (is (= 3 (length assocs))))))))

(test test-change-lists ()
      "Check various properties of changes applied to Isidor in this
test suite"
      (with-fixture merge-test-db ()
        (let
            ((all-revision-set (get-all-revisions))
             (fragments-revision2
              (get-fragments fixtures::revision2))
             (fragments-revision3
              (get-fragments fixtures::revision3)))
          (is (= 3 (length all-revision-set)))
          (is (= fixtures::revision1 (first all-revision-set)))
          (is (= fixtures::revision2 (second all-revision-set)))
          (is (= fixtures::revision3 (third all-revision-set)))

          ;topics changed in revision2 / merge1: topic type, service,
          ;standard, semantic standard, standardHasStatus, geo data
          ;standard, common lisp, norwegian curriculum
          (is (= 8 (length fragments-revision2)))

          ;topics changed in revision3 / merge2: semantic standard, norwegian curriculum, common lisp
          (is (= 3 (length fragments-revision3)))
          (is (= fixtures::revision3 
                 (revision (first fragments-revision3))))
          (is (string= 
               "http://psi.egovpt.org/types/semanticstandard"
               (uri (first (psis (topic (first fragments-revision3)))))))

          (format t "semantic-standard: ~a~&"
                  (remove-duplicates (map 'list #'uri (mapcan #'psis (referenced-topics (first fragments-revision3))))
                                     :test #'string=))
          (is-false
           (set-exclusive-or 
            '("http://psi.egovpt.org/types/standard")
            (remove-duplicates (map 'list #'uri (mapcan #'psis (referenced-topics (first fragments-revision3))))
                               :test #'string=)
            :test #'string=))
                                        ; 0 if we ignore instanceOf associations
          (is (= 0 (length (associations (first fragments-revision3)))))
                               
          (is (string= 
               "http://psi.egovpt.org/standard/Common+Lisp"
               (uri (first (psis (topic (third fragments-revision3)))))))
          (is-false
           (set-exclusive-or 
            '("http://psi.egovpt.org/types/standard"
              "http://psi.egovpt.org/types/links";)
              "http://www.topicmaps.org/xtm/1.0/core.xtm#sort"
              "http://www.topicmaps.org/xtm/1.0/core.xtm#display"
              "http://psi.egovpt.org/types/long-name")
            (remove-duplicates 
             (map 'list 
                  #'uri 
                  (mapcan #'psis (referenced-topics (third fragments-revision3))))
             :test #'string=)
            :test #'string=))
                                        ;0 if we ignore instanceOf associations
          (is (= 0 (length (associations (third fragments-revision3)))))

          (is (string= 
               "http://psi.egovpt.org/service/Norwegian+National+Curriculum"
               (uri (first (psis (topic (second fragments-revision3)))))))
          (is-false
           (set-exclusive-or 
            '("http://psi.egovpt.org/types/service"
              "http://psi.egovpt.org/types/description"
              "http://psi.egovpt.org/types/links"
              "http://psi.egovpt.org/types/serviceUsesStandard"
              "http://psi.egovpt.org/types/StandardRoleType"
              "http://psi.egovpt.org/standard/Topic+Maps+2002"
              "http://psi.egovpt.org/types/ServiceRoleType"
              "http://psi.egovpt.org/types/semanticstandard" ;these three PSIS all stand for the same topic
              "http://psi.egovpt.org/types/greatstandard"
              "http://psi.egovpt.org/types/knowledgestandard")
                             (remove-duplicates (map 'list #'uri (mapcan #'psis (referenced-topics (second fragments-revision3))))
                                                :test #'string=)
                             :test #'string=))
          ;the second time round the object should be fetched from the
          ;cache
          (is (equal fragments-revision3 
                  (get-fragments fixtures::revision3)))
          )))

(test test-changed-p ()
      "Check the is-changed mechanism"
      (with-fixture merge-test-db ()
        (let*
            ((service-topic            ;changed in merge1
              (get-item-by-id "t2" :xtm-id *TEST-TM*))
             (service-name ;does not change after creation
              (first (names service-topic)))
             (google-maps-topic        ;does not change after creation
              (get-item-by-id "t301a" :xtm-id *TEST-TM*))
             (norwegian-curr-topic    ;changes in merge1 (only through
                                        ;association) and merge2 (again through association)
              (get-item-by-id "t300" :xtm-id *TEST-TM*))
             (geodata-topic             ;does not change after creation
              (get-item-by-id "t203" :xtm-id *TEST-TM*)) ;the subject "geodata", not the standard
             (semantic-standard-topic   ;changes in merge1 and merge2
              (get-item-by-id "t3a" :xtm-id *TEST-TM*))
             (common-lisp-topic ;created in merge1 and changed in merge2
              (get-item-by-id "t100" :xtm-id "merge1"))
             (subject-geodata-assoc    ;does not change after creation
              (parent 
               (second    ;the first one is the instanceOf association
                (player-in-roles
                 geodata-topic))))
             (norwegian-curriculum-assoc    ;changes in merge1 and merge2
              (identified-construct
               (elephant:get-instance-by-value 'ItemIdentifierC 'uri 
                                               "http://psi.egovpt.org/itemIdentifiers#assoc_6"))))

          (is-true (changed-p service-name fixtures::revision1))
          (is-false (changed-p service-name fixtures::revision2))
          (is-false (changed-p service-name fixtures::revision3))

          (is-true (changed-p service-topic fixtures::revision1))
          (is-true (changed-p service-topic fixtures::revision2))
          (is-false (changed-p service-topic fixtures::revision3))

          (is-true (changed-p google-maps-topic fixtures::revision1))
          (is-false (changed-p google-maps-topic fixtures::revision2))
          (is-false (changed-p google-maps-topic fixtures::revision3))

          (is-true (changed-p norwegian-curr-topic fixtures::revision1))
          (is-true (changed-p norwegian-curr-topic fixtures::revision2))
          (is-true (changed-p norwegian-curr-topic fixtures::revision3))

          (is-true (changed-p geodata-topic fixtures::revision1))
          (is-false (changed-p geodata-topic fixtures::revision2))
          (is-false (changed-p geodata-topic fixtures::revision3))
            
          (is-true (changed-p semantic-standard-topic fixtures::revision1))
          (is-true (changed-p semantic-standard-topic fixtures::revision2))
          (is-true (changed-p semantic-standard-topic fixtures::revision3))

          (is-false (changed-p common-lisp-topic fixtures::revision1)) ;didn't even exist then
          (is-true (changed-p common-lisp-topic fixtures::revision2))
          (is-true (changed-p common-lisp-topic fixtures::revision3))

          (is-true (changed-p subject-geodata-assoc fixtures::revision1))
          (is-false (changed-p subject-geodata-assoc fixtures::revision2))
          (is-false (changed-p subject-geodata-assoc fixtures::revision3))

          (is-true (changed-p norwegian-curriculum-assoc fixtures::revision1))
          (is-true (changed-p norwegian-curriculum-assoc fixtures::revision2))
          (is-true (changed-p norwegian-curriculum-assoc fixtures::revision3)))))

(test test-mark-as-deleted ()
      "Check the pseudo-deletion mechanism"
      (with-fixture merge-test-db ()
        (let
            ((norwegian-curriculum-topic
              (get-item-by-psi "http://psi.egovpt.org/service/Norwegian+National+Curriculum" :revision fixtures::revision3))
             (semantic-standard-topic
              (get-item-by-psi "http://psi.egovpt.org/types/semanticstandard" :revision fixtures::revision3)))
          (is-true norwegian-curriculum-topic)
          (is-true semantic-standard-topic)
          (mark-as-deleted norwegian-curriculum-topic :source-locator "http://psi.egovpt.org/"
                           :revision fixtures::revision3)
          (is-false (get-item-by-psi "http://psi.egovpt.org/service/Norwegian+National+Curriculum"
                    :revision (1+ fixtures::revision3)))
          (mark-as-deleted semantic-standard-topic :source-locator "http://blablub.egovpt.org/"
                           :revision fixtures::revision3)
          (is-true (get-item-by-psi "http://psi.egovpt.org/types/semanticstandard"
                                    :revision (1+ fixtures::revision3))))))


            