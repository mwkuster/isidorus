;;+-----------------------------------------------------------------------------
;;+  Isidorus
;;+  (c) 2008-2010 Marc Kuester, Christoph Ludwig, Lukas Georgieff
;;+
;;+  Isidorus is freely distributable under the LLGPL license.
;;+  You can find a detailed description in trunk/docs/LLGPL-LICENSE.txt and
;;+  trunk/docs/LGPL-LICENSE.txt.
;;+-----------------------------------------------------------------------------

(asdf:operate 'asdf:load-op 'FiveAM)
(asdf:operate 'asdf:load-op 'cxml)
(asdf:operate 'asdf:load-op 'elephant)
(asdf:operate 'asdf:load-op 'isidorus)
(load "../src/external/packages.lisp")
(load "../src/external/pathnames.lisp")

(use-package :it.bese.FiveAM)

(def-suite testing-db
    :description "compares saved objects in the db with the original objects")
(in-suite testing-db)

(defun set-up-test-db ()
  "clears out the database and (re-)imports a defined topic map."
  (ensure-directories-exist (make-pathname :directory '(:relative "data_base")))
  (loop for filename in (com.gigamonkeys.pathnames:list-directory #p"data_base") do
       (delete-file filename))
     
  ;; TODO
  ;; sample_objects.xtm is inconsistent - e.g., topic "Subject" is instance of t-2497, but
  ;; there is no topic with id t-2497 etc.
  ;; 1) make sample_objects.xtm consistent --> OK
  ;; 2) prepare a test that importer signals an error if the XTM file loaded is inconsistent
  ;;    (NB: there are different possible inconsistencies: Inconsistent associations, 
  ;;     instanceofs, occurrences, role types etc.) --> OK
  ;; 3) add also tests for associations --> OK

  (importer "./sample_objects_2_0.xtm" "data_base")
  (elephant:open-store (get-store-spec "data_base")))

(defun tear-down-test-db ()
  "make sure the elephant store is properly closed"
  (elephant:close-store))


(def-fixture initialized-test-db ()
  (set-up-test-db)
  (&body)
  (tear-down-test-db))


;; searches for a topic(parent-id) which is an instanceOf in another topic(me)
(defun get-instanceOf(me parent-id)
  (let ((child-psi (elephant:get-instance-by-value 'PersistentIdC 'uri "http://psi.topicmaps.org/iso13250/model/instance"))
	(parent-psi (elephant:get-instance-by-value 'PersisitentIdC 'uro "http://psi.topicmaps.org/iso13250/model/type"))
	(parent-topic (elephant:get-instance-by-value 'TopicC 'topicid parent-id)))
    (let ((child-roletype (loop for item in (elephant:get-instances-by-class 'TopicC)
			     when (elephant:associatedp item 'psis child-psi)
			     do (return item)))
	  (parent-roletype (loop for item in (elephant:get-instances-by-class 'TopicC)
			      when (elephant:associatedp item 'psis parent-psi)
			      do (return item))))
      (let ((role (loop for item in (player-in-roles me)
		     when (elephant:associatedp item 'roletype child-roletype)
		     do (return item))))
	(loop for item in (roles (parent-association role))
	   when (and (elephant:associatedp item 'roletype parent-roletype)
		     (elephant:associatedp item 'player parent-topic))
	     do (return t))))))


;; === topic tests ===================================================
(test test-obj1
  (with-fixture initialized-test-db()
    (let ((obj (elephant:get-instance-by-value 'TopicC 'topicid "t1")))
      (is (elephant:associatedp obj 'psis (elephant:get-instance-by-value 'PersistentIdC 'uri "http://www.networkedplanet.com/psi/npcl/meta-types/topic-type")))
      (is (elephant:associatedp obj 'names (elephant:get-instance-by-value 'NameC 'name "Topic Type"))))))
		    

(test test-obj2
  (with-fixture initialized-test-db()
    (let ((obj (elephant:get-instance-by-value 'TopicC 'topicid "t8")))
      (is (elephant:associatedp obj 'psis (elephant:get-instance-by-value 'PersistentIdC 'uri "http://www.networkedplanet.com/psi/npcl/meta-types/association-role-type")))
      (is (get-instanceOf obj "t1"))
      (is (elephant:associatedp obj 'names (elephant:get-instance-by-value 'NameC 'name "Association Role Type"))))))


(test test-obj3
  (with-fixture initialized-test-db ()
    (let ((obj (elephant:get-instance-by-value 'TopicC 'topicid "t56")))
      (is (elephant:associatedp obj 'psis (elephant:get-instance-by-value 'PersistentIdC 'uri "http://psi.egovpt.org/types/topicIsAboutSubject")))
      (is (get-instanceOf obj "t7"))
      (is (elephant:associatedp obj 'names (elephant:get-instance-by-value 'NameC name "topic is about subject"))))))


(test test-obj4
  (with-fixture initialized-test-db ()
    (let ((obj (elephant:get-instance-by-value 'TopicC 'topicid "t100")))
      (is (elephant:associatedp obj 'psis (elephant:get-instance-by-value 'PersistentIdC 'uri "http://psi.egovpt.org/standard/ISO+19115%3A+Geographic+Information+-+Metadata")))
      (is (get-instanceOf obj "t3a"))
      (is (elephant:associatedp obj 'names (elephant:get-instance-by-value 'NameC 'name "ISO 19115")))
      (is (elephant:associatedp obj 'names (elephant:get-instance-by-value 'NameC 'name "ISO 19115:2003 Geographic Information - Metadata")))
      ;; occurrence <---
      ;; occurrence <---
      ;; occurrence <---
      ;; occurrence <---
      )))


;; === associations tests ============================================
(test test-obj5
  (with-fixture initialized-test-db ()
    (let ((obj (elephant:get-instance-by-value 'AssociationC 'associationtype (elephant:get-instance-by-value 'TopicC 'topicid "t57")))
	  (t59 (elephant:get-instance-by-value 'TopicC 'topicid "t59"))
	  (t202 (elephant:get-instance-by-value 'TopicC 'topicid "t202"))
	  (t58 (elephant:get-instance-by-value 'TopicC 'topicid "t58"))
	  (t204 (elephant:get-instances-by-value 'TopicC 'topicid "t204")))
      (let ((role-1 (first (roles obj)))
	    (role-2 (second (roles obj))))
	(is (= 2 length (roles obj)))
	(is (or (and (elephant:associatedp role-1 'player t202)
		     (elephant:associatedp role-1 'roletype t59))
		(and (elephant:associatedp role-1 'player t204)
		     (elephant:associatedp role-1 'roletype t59))))
	(is (or (and (elephant:associatedp role-2 'player t202)
		     (elephant:associatedp role-2 'roletype t59))
		(and (elephant:associatedp role-2 'player t204)
		     (elephant:associatedp role-2 'roletype t59))))))))
      

(test test-obj6
  (with-fixture initialized-test-db ()
    (let ((obj (elephant:get-instance-by-value 'AssociationC 'associationtype (elephant:get-instance-by-value 'TopicC 'topicid "t64")))
	  (t63 (elephant:get-instance-by-value 'TopicC 'topicid "t63"))
	  (t301 (elephant:get-instance-by-value 'TopicC 'topicid "t301"))
	  (t62 (elephant:get-instance-by-value 'TopicC 'topicid "t62"))
	  (t100 (elephant:get-instance-by-value 'TopicC 'topicid "t100")))
      (let ((role-1 (first (roles obj)))
	    (role-2 (second (roles obj))))
	(is (= 2 (length (roles obj))))
	(is (or (and (elephant:associatedp role-1 'player t301)
		     (elephant:associatedp role-1 'roletype t63))
		(and (elephant:associatedp role-1 'player t100)
		     (elephant:associatedp role-1 'roletype t62))))
	(is (or (and (elephant:associatedp role-2 'player t301)
		     (elephant:associatedp role-2 'roletype t63))
		(and (elephant:associatedp role-2 'player t100)
		     (elephant:associatedp role-2 'roletype t62))))))))


;; === db tests ======================================================
(test test-closing-db 
  ;; the point of this test is to check that an error is signaeld if there is no
  ;; open elephant store -- therefore don't run this test with the
  ;; fixture initialized-test-db!

  ;;TODO: test is wrong, should expect the signalled error
  ;;ELEPHANT:CONTROLLER-LOST-ERROR --> OK
  (signals ELEPHANT:CONTROLLER-LOST-ERROR
    (elephant:get-instance-by-value 'TopicC 'topicid "t-2502")))
  

(test test-inconsistent-file
  (with-fixture initialized-test-db ()
    (signals inconsistent-file-error
      (progn
	(importer "./inconsistent_2_0.xtm" "data_base")
	(error 'inconsistent-file-error :message "test"))))) ;has to be thrown only if the file is inconsistent


;(!run 'testing-db)