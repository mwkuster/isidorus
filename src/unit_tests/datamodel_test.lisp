;;+-----------------------------------------------------------------------------
;;+  Isidorus
;;+  (c) 2008-2010 Marc Kuester, Christoph Ludwig, Lukas Georgieff
;;+
;;+  Isidorus is freely distributable under the LLGPL license.
;;+  You can find a detailed description in trunk/docs/LLGPL-LICENSE.txt and
;;+  trunk/docs/LGPL-LICENSE.txt.
;;+-----------------------------------------------------------------------------

(defpackage :datamodel-test
  (:use 
   :common-lisp
   :datamodel
   :it.bese.FiveAM
   :fixtures
   :unittests-constants)
  (:import-from :exceptions
		duplicate-identifier-error
		missing-argument-error
		tm-reference-error
		object-not-found-error
		not-mergable-error)
  (:import-from :constants
		*xml-string*
		*xml-uri*)
  (:export :run-datamodel-tests
	   :datamodel-test
	   :test-VersionInfoC
	   :test-VersionedConstructC
	   :test-ItemIdentifierC
	   :test-PersistentIdC
	   :test-SubjectLocatorC
	   :test-TopicIdentificationC
	   :test-get-item-by-id
	   :test-get-item-by-item-identifier
	   :test-get-item-by-locator
	   :test-get-item-by-psi
	   :test-ReifiableConstructC
	   :test-OccurrenceC
	   :test-VariantC
	   :test-NameC
	   :test-TypableC
	   :test-ScopableC
	   :test-RoleC
	   :test-player
	   :test-TopicMapC
	   :test-delete-ItemIdentifierC
	   :test-delete-PersistentIdC
	   :test-delete-SubjectLocatorC
	   :test-delete-ReifiableConstructC
	   :test-delete-VariantC
	   :test-delete-NameC
	   :test-delete-OccurrenceC
	   :test-delete-TypableC
	   :test-delete-ScopableC
	   :test-delete-AssociationC
	   :test-delete-RoleC
	   :test-equivalent-PointerC
	   :test-equivalent-OccurrenceC
	   :test-equivalent-NameC
	   :test-equivalent-VariantC
	   :test-equivalent-RoleC
	   :test-equivalent-AssociationC
	   :test-equivalent-TopicC
	   :test-equivalent-TopicMapC
	   :test-class-p
	   :test-find-item-by-revision
	   :test-make-Unknown
	   :test-make-VersionedConstructC
	   :test-make-TopicIdentificationC
	   :test-make-PersistentIdC
	   :test-make-SubjectLocatorC
	   :test-make-ItemIdentifierC
	   :test-make-OccurrenceC
	   :test-make-NameC
	   :test-make-VariantC
	   :test-make-RoleC
	   :test-make-TopicMapC
	   :test-make-AssociationC
	   :test-make-TopicC
	   :test-find-oldest-construct
	   :test-move-referenced-constructs-ReifiableConstructC
	   :test-move-referenced-constructs-NameC
	   :test-merge-constructs-TopicC-1
	   :test-merge-constructs-TopicC-2
	   :test-merge-constructs-TopicC-3
	   :test-merge-constructs-TopicC-4
	   :test-merge-constructs-TopicC-5
	   :test-merge-constructs-TopicC-6
	   :test-merge-constructs-TopicC-7
	   :test-merge-constructs-TopicC-8
	   :test-merge-constructs-TopicC-9
	   :test-merge-constructs-TopicC-10
	   :test-merge-constructs-AssociationC))


(declaim (optimize (debug 3)))

(in-package :datamodel-test)

(def-suite datamodel-test
    :description "tests  various key functions of the datamodel")

(in-suite datamodel-test)

(defvar *db-dir* "data_base")

(test test-VersionInfoC ()
  "Tests various functions of the VersionInfoC class."
  (with-fixture with-empty-db (*db-dir*)
    (let ((vi-1 (make-instance 'd::VersionInfoC
			       :start-revision 100
			       :end-revision 300))
	  (vi-2 (make-instance 'd::VersionInfoC
			       :start-revision 300))
	  (vc (make-instance 'd::VersionedConstructC)))
      (is (= (d::start-revision vi-1) 100))
      (is (= (d::end-revision vi-1) 300))
      (is (= (d::start-revision vi-2) 300))
      (is (= (d::end-revision vi-2) 0))
      (setf (d::versioned-construct vi-1) vc))))


(test test-VersionedConstructC ()
  "Tests various functions of the VersionedCoinstructC class."
  (with-fixture with-empty-db (*db-dir*)
    (let ((vc (make-instance 'd::VersionedConstructC)))
      (is-false (d::versions vc))
      (d::add-to-version-history vc
				 :start-revision 100
				 :end-revision 300)
      (is (= (length (d::versions vc)) 1))
      (is (= (d::end-revision (first (d::versions vc))) 300))
      (is (= (d::start-revision (first (d::versions vc))) 100))
      (d::add-to-version-history vc :start-revision 300)
      (is (= (length (d::versions vc)) 1))
      (is (= (d::end-revision (first (d::versions vc))) 0))
      (is (= (d::start-revision (first (d::versions vc))) 100))
      (d::add-to-version-history vc :start-revision 500)
      (is (= (length (d::versions vc)) 2))
      (let* ((vi-1 (first (d::versions vc)))
	     (vi-2 (second (d::versions vc)))
	     (sr-1 (d::start-revision vi-1))
	     (er-1 (d::end-revision vi-1))
	     (sr-2 (d::start-revision vi-2))
	     (er-2 (d::end-revision vi-2)))
	(is-true (or (and (= sr-1 100) (= er-1 500)
			  (= sr-2 500) (= er-2 0))
		     (and (= sr-1 500) (= er-1 0)
			  (= sr-2 100) (= er-2 500)))))
      (d::add-to-version-history vc :start-revision 600)
      (is (= (length (d::versions vc)) 3))
      (d::add-to-version-history vc
				 :start-revision 100
				 :end-revision 500)
      (is (= (length (d::versions vc)) 3))
      (is (= (length (elephant:get-instances-by-class 'd::VersionInfoC)) 3))
      (is (= (length
	      (elephant:get-instances-by-class 'd::VersionedConstructC)) 1))
      (d::delete-construct vc)
      (is (= (length (elephant:get-instances-by-class 'd::VersionInfoC)) 0))
      (is (= (length
	      (elephant:get-instances-by-class 'd::VersionedConstructC)) 0)))))


(test test-ItemIdentifierC ()
    "Tests various functions of the ItemIdentifierC class."
    (with-fixture with-empty-db (*db-dir*)
      (let ((ii-1 (make-instance 'ItemIdentifierC
				 :uri "ii-1"))
	    (ii-2 (make-instance 'ItemIdentifierC
				 :uri "ii-2"))
	    (topic-1 (make-instance 'TopicC))
	    (revision-0 0)
	    (revision-1 100)
	    (revision-2 200)
	    (revision-3 300)
	    (revision-3-5 350)
	    (revision-4 400))
	(setf d:*TM-REVISION* revision-1)
	(is-false (identified-construct ii-1))
	(signals missing-argument-error (make-instance 'ItemIdentifierC))
	(is-false (item-identifiers topic-1))
	(add-item-identifier topic-1 ii-1)
	(is (= (length (d::versions topic-1)) 1))
	(is-true (find-if #'(lambda(vi)
			      (and (= (d::start-revision vi) revision-1)
				   (= (d::end-revision vi) 0)))
			  (d::versions topic-1)))
	(is (= (length (item-identifiers topic-1)) 1))
	(is (eql (first (item-identifiers topic-1)) ii-1))
	(is (eql (identified-construct ii-1) topic-1))
	(add-item-identifier topic-1 ii-2 :revision revision-2)
	(is (= (length (d::versions topic-1)) 2))
	(is-true (find-if #'(lambda(vi)
			      (and (= (d::start-revision vi) revision-2)
				   (= (d::end-revision vi) 0)))
			  (d::versions topic-1)))
	(is (= (length (item-identifiers topic-1 :revision revision-0)) 2))
	(is (= (length (item-identifiers topic-1 :revision revision-1)) 1))
	(is (eql (first (item-identifiers topic-1 :revision revision-1)) ii-1))
	(is (= (length (union (list ii-1 ii-2)
			      (item-identifiers topic-1 :revision revision-2)))
	       2))
	(is (= (length (union (list ii-1 ii-2)
			      (item-identifiers topic-1 :revision revision-0)))
	       2))
	(delete-item-identifier topic-1 ii-1 :revision revision-3)
	(is (= (length (union (list ii-2)
			      (item-identifiers topic-1
						  :revision revision-0)))
	       1))
	(is (= (length (union (list ii-1 ii-2)
			      (item-identifiers topic-1
						  :revision revision-2)))
	       2))
	(delete-item-identifier topic-1 ii-2 :revision revision-3)
	(is (= (length (d::versions topic-1)) 3))
	(is-true (find-if #'(lambda(vi)
			      (and (= (d::start-revision vi) revision-3)
				   (= (d::end-revision vi) 0)))
			  (d::versions topic-1)))
	(is-false (item-identifiers topic-1 :revision revision-3))
	(add-item-identifier topic-1 ii-1 :revision revision-4)
	(is (= (length (union (list ii-1)
			      (item-identifiers topic-1 :revision revision-0)))
	       1))
	(is (= (length (d::slot-p topic-1 'd::item-identifiers)) 2))
	(is-false (item-identifiers topic-1 :revision revision-3-5)))))


(test test-PersistentIdC ()
    "Tests various functions of the PersistentIdC class."
    (with-fixture with-empty-db (*db-dir*)
      (let ((psi-1 (make-instance 'PersistentIdC
				  :uri "psi-1"))
	    (psi-2 (make-instance 'PersistentIdC
				  :uri "psi-2"))
	    (topic-1 (make-instance 'TopicC))
	    (revision-0 0)
	    (revision-1 100)
	    (revision-2 200)
	    (revision-3 300)
	    (revision-3-5 350)
	    (revision-4 400))
	(setf d:*TM-REVISION* revision-1)
	(is-false (identified-construct psi-1))
	(signals missing-argument-error (make-instance 'PersistentIdC))
	(is-false (psis topic-1))
	(add-psi topic-1 psi-1)
	(is (= (length (d::versions topic-1)) 1))
	(is-true (find-if #'(lambda(vi)
			      (and (= (d::start-revision vi) revision-1)
				   (= (d::end-revision vi) 0)))
			  (d::versions topic-1)))
	(is (= (length (psis topic-1)) 1))
	(is (eql (first (psis topic-1)) psi-1))
	(is (eql (identified-construct psi-1) topic-1))
	(add-psi topic-1 psi-2 :revision revision-2)
	(is (= (length (d::versions topic-1)) 2))
	(is-true (find-if #'(lambda(vi)
			      (and (= (d::start-revision vi) revision-2)
				   (= (d::end-revision vi) 0)))
			  (d::versions topic-1)))
	(is (= (length (psis topic-1 :revision revision-0)) 2))
	(is (= (length (psis topic-1 :revision revision-1)) 1))
	(is (eql (first (psis topic-1 :revision revision-1)) psi-1))
	(is (= (length (union (list psi-1 psi-2)
			      (psis topic-1 :revision revision-2)))
	       2))
	(is (= (length (union (list psi-1 psi-2)
			      (psis topic-1 :revision revision-0)))
	       2))
	(delete-psi topic-1 psi-1 :revision revision-3)
	(is (= (length (union (list psi-2)
			      (psis topic-1 :revision revision-0)))
	       1))
	(is (= (length (union (list psi-1 psi-2)
			      (psis topic-1 :revision revision-2)))
	       2))
	(delete-psi topic-1 psi-2 :revision revision-3)
	(is (= (length (d::versions topic-1)) 3))
	(is-true (find-if #'(lambda(vi)
			      (and (= (d::start-revision vi) revision-3)
				   (= (d::end-revision vi) 0)))
			  (d::versions topic-1)))
	(is-false (psis topic-1 :revision revision-3))
	(add-psi topic-1 psi-1 :revision revision-4)
	(is (= (length (union (list psi-1)
			      (psis topic-1 :revision revision-0)))
	       1))
	(is (= (length (d::slot-p topic-1 'd::psis)) 2))
	(is-false (psis topic-1 :revision revision-3-5)))))


(test test-SubjectLocatorC ()
    "Tests various functions of the SubjectLocatorC class."
    (with-fixture with-empty-db (*db-dir*)
      (let ((sl-1 (make-instance 'SubjectLocatorC
				 :uri "sl-1"))
	    (sl-2 (make-instance 'SubjectLocatorC
				 :uri "sl-2"))
	    (topic-1 (make-instance 'TopicC))
	    (revision-0 0)
	    (revision-1 100)
	    (revision-2 200)
	    (revision-3 300)
	    (revision-3-5 350)
	    (revision-4 400))
	(setf d:*TM-REVISION* revision-1)
	(is-false (identified-construct sl-1))
	(signals missing-argument-error (make-instance 'SubjectLocatorC))
	(is-false (locators topic-1))
	(add-locator topic-1 sl-1)
	(is (= (length (d::versions topic-1)) 1))
	(is-true (find-if #'(lambda(vi)
			      (and (= (d::start-revision vi) revision-1)
				   (= (d::end-revision vi) 0)))
			  (d::versions topic-1)))
	(is (= (length (locators topic-1)) 1))
	(is (eql (first (locators topic-1)) sl-1))
	(is (eql (identified-construct sl-1) topic-1))
	(add-locator topic-1 sl-2 :revision revision-2)
	(is (= (length (d::versions topic-1)) 2))
	(is-true (find-if #'(lambda(vi)
			      (and (= (d::start-revision vi) revision-2)
				   (= (d::end-revision vi) 0)))
			  (d::versions topic-1)))
	(is (= (length (locators topic-1 :revision revision-0)) 2))
	(is (= (length (locators topic-1 :revision revision-1)) 1))
	(is (eql (first (locators topic-1 :revision revision-1)) sl-1))
	(is (= (length (union (list sl-1 sl-2)
			      (locators topic-1 :revision revision-2)))
	       2))
	(is (= (length (union (list sl-1 sl-2)
			      (locators topic-1 :revision revision-0)))
	       2))
	(delete-locator topic-1 sl-1 :revision revision-3)
	(is (= (length (d::versions topic-1)) 3))
	(is-true (find-if #'(lambda(vi)
			      (and (= (d::start-revision vi) revision-3)
				   (= (d::end-revision vi) 0)))
			  (d::versions topic-1)))
	(is (= (length (union (list sl-2)
			      (locators topic-1 :revision revision-0)))
	       1))
	(is (= (length (union (list sl-1 sl-2)
			      (locators topic-1 :revision revision-2)))
	       2))
	(delete-locator topic-1 sl-2 :revision revision-3)
	(is-false (locators topic-1 :revision revision-3))
	(add-locator topic-1 sl-1 :revision revision-4)
	(is (= (length (union (list sl-1)
			      (locators topic-1 :revision revision-0)))
	       1))
	(is (= (length (d::slot-p topic-1 'd::locators)) 2))
	(is-false (locators topic-1 :revision revision-3-5)))))


(test test-TopicIdentificationC ()
    "Tests various functions of the TopicIdentificationC class."
    (with-fixture with-empty-db (*db-dir*)
      (let ((ti-1 (make-instance 'TopicIdentificationC
				 :uri "ti-1"
				 :xtm-id "xtm-id-1"))
	    (ti-2 (make-instance 'TopicIdentificationC
				 :uri "ti-2"
				 :xtm-id "xtm-id-2"))
	    (topic-1 (make-instance 'TopicC))
	    (revision-0 0)
	    (revision-1 100)
	    (revision-2 200)
	    (revision-3 300)
	    (revision-3-5 350)
	    (revision-4 400))
	(setf d:*TM-REVISION* revision-1)
	(is-false (identified-construct ti-1))
	(signals missing-argument-error (make-instance 'TopicIdentificationC
				      :uri "ti-1"))
	(signals missing-argument-error (make-instance 'TopicIdentificationC
				      :xtm-id "xtm-id-1"))
	(is-false (topic-identifiers topic-1))
	(add-topic-identifier topic-1 ti-1)
	(is (= (length (d::versions topic-1)) 1))
	(is-true (find-if #'(lambda(vi)
			      (and (= (d::start-revision vi) revision-1)
				   (= (d::end-revision vi) 0)))
			  (d::versions topic-1)))
	(is (= (length (topic-identifiers topic-1)) 1))
	(is (eql (first (topic-identifiers topic-1)) ti-1))
	(is (eql (identified-construct ti-1) topic-1))
	(add-topic-identifier topic-1 ti-2 :revision revision-2)
	(is (= (length (d::versions topic-1)) 2))
	(is-true (find-if #'(lambda(vi)
			      (and (= (d::start-revision vi) revision-2)
				   (= (d::end-revision vi) 0)))
			  (d::versions topic-1)))
	(is (= (length (topic-identifiers topic-1 :revision revision-0)) 2))
	(is (= (length (topic-identifiers topic-1 :revision revision-1)) 1))
	(is (eql (first (topic-identifiers topic-1 :revision revision-1)) ti-1))
	(is (= (length (union (list ti-1 ti-2)
			      (topic-identifiers topic-1 :revision revision-2)))
	       2))
	(is (= (length (union (list ti-1 ti-2)
			      (topic-identifiers topic-1 :revision revision-0)))
	       2))
	(delete-topic-identifier topic-1 ti-1 :revision revision-3)
	(is (= (length (d::versions topic-1)) 3))
	(is-true (find-if #'(lambda(vi)
			      (and (= (d::start-revision vi) revision-3)
				   (= (d::end-revision vi) 0)))
			  (d::versions topic-1)))
	(is (= (length (union (list ti-2)
			      (topic-identifiers topic-1 :revision revision-0)))
	       1))
	(is (= (length (union (list ti-1 ti-2)
			      (topic-identifiers topic-1 :revision revision-2)))
	       2))
	(delete-topic-identifier topic-1 ti-2 :revision revision-3)
	(is-false (topic-identifiers topic-1 :revision revision-3))
	(add-topic-identifier topic-1 ti-1 :revision revision-4)
	(is (= (length (union (list ti-1)
			      (topic-identifiers topic-1 :revision revision-0)))
	       1))
	(is (= (length (d::slot-p topic-1 'd::topic-identifiers)) 2))
	(is-false (topic-identifiers topic-1 :revision revision-3-5)))))


(test test-get-item-by-id ()
    "Tests the function test-get-item-by-id."
    (with-fixture with-empty-db (*db-dir*)
      (let ((top-id-1 (make-instance 'TopicIdentificationC
				     :uri "topid-1"
				     :xtm-id "xtm-id-1"))
	    (top-id-2 (make-instance 'TopicIdentificationC
				     :uri "topid-2"
				     :xtm-id "xtm-id-2"))
	    (top-id-3-1 (make-instance 'TopicIdentificationC
				       :uri "topid-3"
				       :xtm-id "xtm-id-3"))
	    (top-id-3-2 (make-instance 'TopicIdentificationC
				       :uri "topid-3"
				       :xtm-id "xtm-id-3"))
	    (top-1 (make-instance 'TopicC))
	    (top-2 (make-instance 'TopicC))
	    (top-3 (make-instance 'TopicC))
	    (rev-0 0)
	    (rev-1 100)
	    (rev-2 200))
	(setf d:*TM-REVISION* rev-1)
	(is-false (get-item-by-id "any-top-id" :revision rev-0))
	(signals object-not-found-error
	  (get-item-by-id "any-top-id" :xtm-id "any-xtm-id" :error-if-nil t))
	(signals object-not-found-error
	  (get-item-by-id "any-top-id" :error-if-nil t :revision rev-0))
	(is-false (get-item-by-id "any-top-id" :xtm-id "any-xtm-id"))
	(add-topic-identifier top-1 top-id-3-1 :revision rev-1)
	(add-topic-identifier top-1 top-id-3-2 :revision rev-1)
	(signals duplicate-identifier-error
	  (get-item-by-id "topid-3" :xtm-id "xtm-id-3" :revision rev-1))
	(add-topic-identifier top-2 top-id-1)
	(add-topic-identifier top-2 top-id-2 :revision rev-2)
	(is (eql top-2 (get-item-by-id "topid-1" :xtm-id "xtm-id-1"
				       :revision rev-0)))
	(is (eql top-2 (get-item-by-id "topid-2" :xtm-id "xtm-id-2"
				       :revision rev-0)))
	(is (eql top-2 (get-item-by-id "topid-1" :xtm-id "xtm-id-1"
				       :revision 500)))
	(is-false (get-item-by-id "topid-2" :xtm-id "xtm-id-2"
				  :revision rev-1))
	(delete-topic-identifier top-2 top-id-1 :revision rev-2)
	(is-false (get-item-by-id "topid-1" :xtm-id "xtm-id-1"
				  :revision rev-0))
	(is (eql top-2 (get-item-by-id "topid-1" :xtm-id "xtm-id-1"
				       :revision rev-1)))
	(add-topic-identifier top-3 top-id-1 :revision rev-2)
	(is (eql top-2 (get-item-by-id "topid-1" :xtm-id "xtm-id-1"
				       :revision rev-1)))
	(d::add-to-version-history top-3 :start-revision rev-2)
	(is (eql top-3 (get-item-by-id "topid-1" :xtm-id "xtm-id-1"
				       :revision rev-0)))
	(is (eql top-3
		 (get-item-by-id
		  (concatenate 'string "t" (write-to-string
					    (elephant::oid top-3)))
		  :revision rev-0 :xtm-id nil)))
	(is-false (get-item-by-id
		   (concatenate 'string "t" (write-to-string
					     (elephant::oid top-3)))
		   :revision rev-1 :xtm-id nil)))))


(test test-get-item-by-item-identifier ()
    "Tests the function test-get-item-by-item-identifier."
    (with-fixture with-empty-db (*db-dir*)
      (let ((ii-1 (make-instance 'ItemIdentifierC
				 :uri "ii-1"))
	    (ii-2 (make-instance 'ItemIdentifierC
				 :uri "ii-2"))
	    (ii-3-1 (make-instance 'ItemIdentifierC
				   :uri "ii-3"))
	    (ii-3-2 (make-instance 'ItemIdentifierC
				   :uri "ii-3"))
	    (top-1 (make-instance 'TopicC))
	    (top-2 (make-instance 'TopicC))
	    (top-3 (make-instance 'TopicC))
	    (rev-0 0)
	    (rev-1 100)
	    (rev-2 200))
	(setf d:*TM-REVISION* rev-1)
	(is-false (get-item-by-id "any-ii-id"))
	(signals object-not-found-error
	  (get-item-by-item-identifier
	   "any-ii-id" :error-if-nil t :revision rev-1))
	(signals object-not-found-error
	  (get-item-by-item-identifier
	   "any-ii-id" :error-if-nil t :revision rev-1))
	(is-false (get-item-by-item-identifier "any-ii-id"))
	(add-item-identifier top-1 ii-3-1 :revision rev-1)
	(add-item-identifier top-1 ii-3-2 :revision rev-1)
	(signals duplicate-identifier-error
	  (get-item-by-item-identifier "ii-3" :revision rev-1))
	(add-item-identifier top-2 ii-1)
	(add-item-identifier top-2 ii-2 :revision rev-2)
	(is (eql top-2 (get-item-by-item-identifier "ii-1" :revision rev-0)))
	(is (eql top-2 (get-item-by-item-identifier "ii-2" :revision rev-0)))
	(is (eql top-2 (get-item-by-item-identifier "ii-1" :revision 500)))
	(is-false (get-item-by-item-identifier "ii-2" :revision rev-1))
	(delete-item-identifier top-2 ii-1 :revision rev-2)
	(is-false (get-item-by-item-identifier "ii-1" :revision rev-0))
	(is (eql top-2 (get-item-by-item-identifier "ii-1" :revision rev-1)))
	(add-item-identifier top-3 ii-1 :revision rev-2)
	(is (eql top-2 (get-item-by-item-identifier "ii-1" :revision rev-1)))
	(d::add-to-version-history top-3 :start-revision rev-2)
	(is (eql top-3 (get-item-by-item-identifier "ii-1" :revision rev-0))))))


(test test-get-item-by-locator ()
    "Tests the function test-get-item-by-locator."
    (with-fixture with-empty-db (*db-dir*)
      (let ((sl-1 (make-instance 'SubjectLocatorC
				 :uri "sl-1"))
	    (sl-2 (make-instance 'SubjectLocatorC
				 :uri "sl-2"))
	    (sl-3-1 (make-instance 'SubjectLocatorC
				   :uri "sl-3"))
	    (sl-3-2 (make-instance 'SubjectLocatorC
				   :uri "sl-3"))
	    (top-1 (make-instance 'TopicC))
	    (top-2 (make-instance 'TopicC))
	    (top-3 (make-instance 'TopicC))
	    (rev-0 0)
	    (rev-1 100)
	    (rev-2 200))
	(setf d:*TM-REVISION* rev-1)
	(is-false (get-item-by-id "any-sl-id"))
	(signals object-not-found-error
	  (get-item-by-locator "any-sl-id" :error-if-nil t :revision rev-0))
	(signals object-not-found-error
	  (get-item-by-locator "any-sl-id" :error-if-nil t :revision rev-0))
	(is-false (get-item-by-locator "any-sl-id" :revision rev-0))
	(add-locator top-1 sl-3-1 :revision rev-1)
	(add-locator top-1 sl-3-2 :revision rev-1)
	(signals duplicate-identifier-error
	  (get-item-by-locator "sl-3" :revision rev-1))
	(add-locator top-2 sl-1)
	(add-locator top-2 sl-2 :revision rev-2)
	(is (eql top-2 (get-item-by-locator "sl-1" :revision rev-0)))
	(is (eql top-2 (get-item-by-locator "sl-2" :revision rev-0)))
	(is (eql top-2 (get-item-by-locator "sl-1" :revision 500)))
	(is-false (get-item-by-locator "sl-2" :revision rev-1))
	(delete-locator top-2 sl-1 :revision rev-2)
	(is-false (get-item-by-locator "sl-1" :revision rev-0))
	(is (eql top-2 (get-item-by-locator "sl-1" :revision rev-1)))
	(add-locator top-3 sl-1 :revision rev-2)
	(is (eql top-2 (get-item-by-locator "sl-1" :revision rev-1)))
	(d::add-to-version-history top-3 :start-revision rev-2)
	(is (eql top-3 (get-item-by-locator "sl-1" :revision rev-0))))))


(test test-get-item-by-psi ()
    "Tests the function test-get-item-by-psi."
    (with-fixture with-empty-db (*db-dir*)
      (let ((psi-1 (make-instance 'PersistentIdC
				  :uri "psi-1"))
	    (psi-2 (make-instance 'PersistentIdC
				  :uri "psi-2"))
	    (psi-3-1 (make-instance 'PersistentIdC
				    :uri "psi-3"))
	    (psi-3-2 (make-instance 'PersistentIdC
				    :uri "psi-3"))
	    (top-1 (make-instance 'TopicC))
	    (top-2 (make-instance 'TopicC))
	    (top-3 (make-instance 'TopicC))
	    (rev-0 0)
	    (rev-1 100)
	    (rev-2 200))
	(setf d:*TM-REVISION* rev-1)
	(is-false (get-item-by-id "any-psi-id"))
	(signals object-not-found-error
	 (get-item-by-psi "any-psi-id" :error-if-nil t :revision rev-0))
	(is-false (get-item-by-psi "any-psi-id"))
	(add-psi top-1 psi-3-1 :revision rev-1)
	(add-psi top-1 psi-3-2 :revision rev-1)
	(is-false (get-item-by-locator "psi-3" :revision rev-1))
	(signals duplicate-identifier-error
	  (get-item-by-psi "psi-3" :revision rev-1))
	(add-psi top-2 psi-1)
	(add-psi top-2 psi-2 :revision rev-2)
	(is (eql top-2 (get-item-by-psi "psi-1" :revision rev-0)))
	(is (eql top-2 (get-item-by-psi "psi-2" :revision rev-0)))
	(is (eql top-2 (get-item-by-psi "psi-1" :revision 500)))
	(is-false (get-item-by-psi "psi-2" :revision rev-1))
	(delete-psi top-2 psi-1 :revision rev-2)
	(is-false (get-item-by-psi "psi-1" :revision rev-0))
	(is (eql top-2 (get-item-by-psi "psi-1" :revision rev-1)))
	(add-psi top-3 psi-1 :revision rev-2)
	(is (eql top-2 (get-item-by-psi "psi-1" :revision rev-1)))
	(d::add-to-version-history top-3 :start-revision rev-2)
	(is (eql top-3 (get-item-by-psi "psi-1" :revision rev-0))))))


(test test-ReifiableConstructC ()
    "Tests variuas functions of the ReifialeConstructC."
    (with-fixture with-empty-db (*db-dir*)
      (let ((reifier-top (make-instance 'TopicC))
	    (reified-rc (make-instance 'd::AssociationC))
	    (version-0-5 50)
	    (version-1 100)
	    (version-2 200)
	    (version-3 300))
	(setf *TM-REVISION* version-1)
	(is-false (reifier reified-rc))
	(is-false (reified-construct reifier-top))
	(add-reifier reified-rc reifier-top :revision version-1)
	(is (= (length (d::versions reified-rc)) 1))
	(is-true (find-if #'(lambda(vi)
			      (and (= (d::start-revision vi) version-1)
				   (= (d::end-revision vi) 0)))
			  (d::versions reified-rc)))
	(is (eql reifier-top (reifier reified-rc)))
	(is (eql reified-rc (reified-construct reifier-top)))
	(is (eql reifier-top (reifier reified-rc :revision version-2)))
	(is (eql reified-rc (reified-construct reifier-top :revision version-2)))
	(is-false (reifier reified-rc :revision version-0-5))
	(is-false (reified-construct reifier-top :revision version-0-5))
	(delete-reifier reified-rc reifier-top :revision version-3)
	(is (= (length (d::versions reified-rc)) 2))
	(is-true (find-if #'(lambda(vi)
			      (and (= (d::start-revision vi) version-3)
				   (= (d::end-revision vi) 0)))
			  (d::versions reified-rc))))))


(test test-OccurrenceC ()
  "Tests various functions of OccurrenceC."
  (with-fixture with-empty-db (*db-dir*)
    (let ((occ-1 (make-instance 'OccurrenceC))
	  (occ-2 (make-instance 'OccurrenceC))
	  (top-1 (make-instance 'TopicC))
	  (top-2 (make-instance 'TopicC))
	  (rev-0 0)
	  (rev-1 100)
	  (rev-2 200)
	  (rev-3 300)
	  (rev-4 400)
	  (rev-5 500)
	  (rev-6 600)
	  (rev-7 700)
	  (rev-8 800))
      (setf *TM-REVISION* rev-1)
      (is-false (parent occ-1 :revision rev-0))
      (is-false (occurrences top-1 :revision rev-0))
      (add-occurrence top-1 occ-1 :revision rev-1)
      (is (= (length (d::versions top-1)) 1))
      (is-true (find-if #'(lambda(vi)
			    (and (= (d::start-revision vi) rev-1)
				 (= (d::end-revision vi) 0)))
			(d::versions top-1)))
      (is (= (length (union (list occ-1)
			    (occurrences top-1 :revision rev-0))) 1))
      (add-occurrence top-1 occ-2 :revision rev-2)
      (is (= (length (d::versions top-1)) 2))
      (is-true (find-if #'(lambda(vi)
			    (and (= (d::start-revision vi) rev-2)
				 (= (d::end-revision vi) 0)))
			(d::versions top-1)))
      (is (= (length (union (list occ-1 occ-2)
			    (occurrences top-1 :revision rev-0))) 2))
      (is (= (length (union (list occ-1)
			    (occurrences top-1 :revision rev-1))) 1))
      (add-occurrence top-1 occ-2 :revision rev-3)
      (is (= (length (d::slot-p top-1 'd::occurrences)) 2))
      (delete-occurrence top-1 occ-1 :revision rev-4)
      (is (= (length (d::versions top-1)) 4))
      (is-true (find-if #'(lambda(vi)
			    (and (= (d::start-revision vi) rev-4)
				 (= (d::end-revision vi) 0)))
			(d::versions top-1)))
      (is (= (length (union (list occ-2)
			    (occurrences top-1 :revision rev-4))) 1))
      (is (= (length (union (list occ-2)
			    (occurrences top-1 :revision rev-0))) 1))
      (is (= (length (union (list occ-1 occ-2)
			    (occurrences top-1 :revision rev-2))) 2))
      (add-occurrence top-1 occ-1 :revision rev-4)
      (is (= (length (union (list occ-2 occ-1)
			    (occurrences top-1 :revision rev-0))) 2))
      (signals tm-reference-error (add-occurrence top-2 occ-1 :revision rev-4))
      (delete-occurrence top-1 occ-1 :revision rev-5)
      (is (= (length (union (list occ-2)
			    (occurrences top-1 :revision rev-5))) 1))
      (add-occurrence top-2 occ-1 :revision rev-5)
      (is (eql (parent occ-1 :revision rev-0) top-2))
      (is (eql (parent occ-1 :revision rev-2) top-1))
      (delete-parent occ-2 top-1 :revision rev-4)
      (is-false (parent occ-2 :revision rev-4))
      (is (eql top-1 (parent occ-2 :revision rev-3)))
      (add-parent occ-2 top-1 :revision rev-5)
      (is-false (parent occ-2 :revision rev-4))
      (is (eql top-1 (parent occ-2 :revision rev-0)))
      (delete-parent occ-2 top-1 :revision rev-6)
      (add-parent occ-2 top-2 :revision rev-7)
      (is (= (length (d::versions top-2)) 2))
      (is-true (find-if #'(lambda(vi)
			    (and (= (d::start-revision vi) rev-7)
				 (= (d::end-revision vi) 0)))
			(d::versions top-2)))
      (delete-parent occ-2 top-2 :revision rev-8)
      (is (= (length (d::versions top-2)) 3))
      (is-true (find-if #'(lambda(vi)
			    (and (= (d::start-revision vi) rev-8)
				 (= (d::end-revision vi) 0)))
			(d::versions top-2)))
      (is-false (parent occ-2 :revision rev-0))
      (add-parent occ-2 top-1 :revision rev-8)
      (is (eql top-1 (parent occ-2 :revision rev-0))))))


(test test-VariantC ()
  "Tests various functions of VariantC."
  (with-fixture with-empty-db (*db-dir*)
    (let ((v-1 (make-instance 'VariantC))
	  (v-2 (make-instance 'VariantC))
	  (name-1 (make-instance 'NameC))
	  (name-2 (make-instance 'NameC))
	  (rev-0 0)
	  (rev-1 100)
	  (rev-2 200)
	  (rev-3 300)
	  (rev-4 400)
	  (rev-5 500)
	  (rev-6 600)
	  (rev-7 700)
	  (rev-8 800))
      (setf *TM-REVISION* rev-1)
      (is-false (parent v-1 :revision rev-0))
      (is-false (variants name-1 :revision rev-0))
      (add-variant name-1 v-1 :revision rev-1)
      (is (= (length (union (list v-1)
			    (variants name-1 :revision rev-0))) 1))
      (add-variant name-1 v-2 :revision rev-2)
      (is (= (length (union (list v-1 v-2)
			    (variants name-1 :revision rev-0))) 2))
      (is (= (length (union (list v-1)
			    (variants name-1 :revision rev-1))) 1))
      (add-variant name-1 v-2 :revision rev-3)
      (is (= (length (d::slot-p name-1 'd::variants)) 2))
      (delete-variant name-1 v-1 :revision rev-4)
      (is (= (length (union (list v-2)
			    (variants name-1 :revision rev-4))) 1))
      (is (= (length (union (list v-2)
			    (variants name-1 :revision rev-0))) 1))
      (is (= (length (union (list v-1 v-2)
			    (variants name-1 :revision rev-2))) 2))
      (add-variant name-1 v-1 :revision rev-4)
      (is (= (length (union (list v-2 v-1)
			    (variants name-1 :revision rev-0))) 2))
      (signals tm-reference-error (add-variant name-2 v-1 :revision rev-4))
      (delete-variant name-1 v-1 :revision rev-5)
      (is (= (length (union (list v-2)
			    (variants name-1 :revision rev-5))) 1))
      (add-variant name-2 v-1 :revision rev-5)
      (is (eql (parent v-1 :revision rev-0) name-2))
      (is (eql (parent v-1 :revision rev-2) name-1))
      (delete-parent v-2 name-1 :revision rev-4)
      (is-false (parent v-2 :revision rev-4))
      (is (eql name-1 (parent v-2 :revision rev-3)))
      (add-parent v-2 name-1 :revision rev-5)
      (is-false (parent v-2 :revision rev-4))
      (is (eql name-1 (parent v-2 :revision rev-0)))
      (delete-parent v-2 name-1 :revision rev-6)
      (add-parent v-2 name-2 :revision rev-7)
      (delete-parent v-2 name-2 :revision rev-8)
      (is-false (parent v-2 :revision rev-0))
      (add-parent v-2 name-1 :revision rev-8)
      (is (eql name-1 (parent v-2 :revision rev-0))))))


(test test-NameC ()
  "Tests various functions of NameC."
  (with-fixture with-empty-db (*db-dir*)
    (let ((name-1 (make-instance 'NameC))
	  (name-2 (make-instance 'NameC))
	  (top-1 (make-instance 'TopicC))
	  (top-2 (make-instance 'TopicC))
	  (rev-0 0)
	  (rev-1 100)
	  (rev-2 200)
	  (rev-3 300)
	  (rev-4 400)
	  (rev-5 500)
	  (rev-6 600)
	  (rev-7 700)
	  (rev-8 800))
      (setf *TM-REVISION* rev-1)
      (is-false (parent name-1 :revision rev-0))
      (is-false (names top-1 :revision rev-0))
      (add-name top-1 name-1 :revision rev-1)
      (is (= (length (d::versions top-1)) 1))
      (is-true (find-if #'(lambda(vi)
			    (and (= (d::start-revision vi) rev-1)
				 (= (d::end-revision vi) 0)))
			(d::versions top-1)))
      (is (= (length (union (list name-1)
			    (names top-1 :revision rev-0))) 1))
      (add-name top-1 name-2 :revision rev-2)
      (is (= (length (d::versions top-1)) 2))
      (is-true (find-if #'(lambda(vi)
			    (and (= (d::start-revision vi) rev-2)
				 (= (d::end-revision vi) 0)))
			(d::versions top-1)))
      (is (= (length (union (list name-1 name-2)
			    (names top-1 :revision rev-0))) 2))
      (is (= (length (union (list name-1)
			    (names top-1 :revision rev-1))) 1))
      (add-name top-1 name-2 :revision rev-3)
      (is (= (length (d::slot-p top-1 'd::names)) 2))
      (delete-name top-1 name-1 :revision rev-4)
      (is (= (length (d::versions top-1)) 4))
      (is-true (find-if #'(lambda(vi)
			    (and (= (d::start-revision vi) rev-4)
				 (= (d::end-revision vi) 0)))
			(d::versions top-1)))
      (is (= (length (union (list name-2)
			    (names top-1 :revision rev-4))) 1))
      (is (= (length (union (list name-2)
			    (names top-1 :revision rev-0))) 1))
      (is (= (length (union (list name-1 name-2)
			    (names top-1 :revision rev-2))) 2))
      (add-name top-1 name-1 :revision rev-4)
      (is (= (length (union (list name-2 name-1)
			    (names top-1 :revision rev-0))) 2))
      (signals tm-reference-error (add-name top-2 name-1 :revision rev-4))
      (delete-name top-1 name-1 :revision rev-5)
      (is (= (length (union (list name-2)
			    (names top-1 :revision rev-5))) 1))
      (add-name top-2 name-1 :revision rev-5)
      (is (eql (parent name-1 :revision rev-0) top-2))
      (is (eql (parent name-1 :revision rev-2) top-1))
      (delete-parent name-2 top-1 :revision rev-4)
      (is-false (parent name-2 :revision rev-4))
      (is (eql top-1 (parent name-2 :revision rev-3)))
      (add-parent name-2 top-1 :revision rev-5)
      (is-false (parent name-2 :revision rev-4))
      (is (eql top-1 (parent name-2 :revision rev-0)))
      (delete-parent name-2 top-1 :revision rev-6)
      (add-parent name-2 top-2 :revision rev-7)
      (is (= (length (d::versions top-2)) 2))
      (is-true (find-if #'(lambda(vi)
			    (and (= (d::start-revision vi) rev-7)
				 (= (d::end-revision vi) 0)))
			(d::versions top-2)))
      (delete-parent name-2 top-2 :revision rev-8)
      (is (= (length (d::versions top-2)) 3))
      (is-true (find-if #'(lambda(vi)
			    (and (= (d::start-revision vi) rev-8)
				 (= (d::end-revision vi) 0)))
			(d::versions top-2)))
      (is-false (parent name-2 :revision rev-0))
      (add-parent name-2 top-1 :revision rev-8)
      (is (eql top-1 (parent name-2 :revision rev-0))))))


(test test-TypableC ()
  "Tests various functions of the base class TypableC."
  (with-fixture with-empty-db (*db-dir*)
    (let ((name-1 (make-instance 'NameC))
	  (name-2 (make-instance 'NameC))
	  (top-1 (make-instance 'TopicC))
	  (top-2 (make-instance 'TopicC))
	  (revision-0 0)
	  (revision-0-5 50)
	  (revision-1 100)
	  (revision-2 200)
	  (revision-3 300))
      (setf *TM-REVISION* revision-1)
      (is-false (instance-of name-1 :revision revision-0))
      (add-type name-1 top-1)
      (is (eql top-1 (instance-of name-1)))
      (is-false (instance-of name-1 :revision revision-0-5))
      (is (eql top-1 (instance-of name-1 :revision revision-2)))
      (signals tm-reference-error (add-type name-1 top-2 :revision revision-0))
      (add-type name-2 top-1 :revision revision-2)
      (is (= (length (union (list name-1 name-2)
			    (used-as-type top-1 :revision revision-0))) 2))
      (is (= (length (union (list name-1)
			    (used-as-type top-1 :revision revision-1))) 1))
      (delete-type name-1 top-1 :revision revision-3)
      (is-false (instance-of name-1 :revision revision-0))
      (is (= (length (union (list name-2)
			    (used-as-type top-1 :revision revision-0))) 1))
      (add-type name-1 top-1 :revision revision-3)
      (is (eql top-1 (instance-of name-1 :revision revision-0)))
      (is (= (length (union (list name-1 name-2)
			    (used-as-type top-1 :revision revision-0))) 2))
      (is (= (length (slot-value top-1 'd::used-as-type)) 2)))))


(test test-ScopableC ()
  "Tests various functions of the base class ScopableC."
  (with-fixture with-empty-db (*db-dir*)
    (let ((occ-1 (make-instance 'OccurrenceC))
	  (occ-2 (make-instance 'OccurrenceC))
	  (top-1 (make-instance 'TopicC))
	  (top-2 (make-instance 'TopicC))
	  (revision-0 0)
	  (revision-1 100)
	  (revision-2 200)
	  (revision-3 300))
      (setf *TM-REVISION* revision-1)
      (is-false (themes occ-1 :revision revision-0))
      (is-false (used-as-theme top-1 :revision revision-0))
      (add-theme occ-1 top-1)
      (is (= (length (union (list top-1)
			    (themes occ-1 :revision revision-0))) 1))
      (is (= (length (union (list occ-1)
			    (used-as-theme top-1 :revision revision-0))) 1))
      (delete-theme occ-1 top-1 :revision revision-2)
      (is (= (length (union (list top-1)
			    (themes occ-1 :revision revision-1))) 1))
      (is-false (themes occ-1 :revision revision-0))
      (is-false (used-as-theme top-1 :revision revision-0))
      (is-false (themes occ-1 :revision revision-2))
      (add-theme occ-1 top-1 :revision revision-3)
      (is (= (length (union (list top-1)
			    (themes occ-1 :revision revision-0))) 1))
      (is (= (length (slot-value occ-1 'd::themes)) 1))
      (add-theme occ-1 top-2 :revision revision-2)
      (is (= (length (union (list top-1 top-2)
			    (themes occ-1 :revision revision-0))) 2))
      (is (= (length (union (list top-2)
			    (themes occ-1 :revision revision-2))) 1))
      (is (= (length (union (list top-1 top-2)
			    (themes occ-1 :revision revision-0))) 2))
      (add-theme occ-2 top-2 :revision revision-3)
      (is (= (length (union (list top-1 top-2)
			    (themes occ-1 :revision revision-0))) 2))
      (is (= (length (union (list top-2)
			    (themes occ-2 :revision revision-0))) 1))
      (is (= (length (union (list occ-1)
			    (used-as-theme top-1 :revision revision-0))) 1))
      (is (= (length (union (list occ-1 occ-2)
			    (used-as-theme top-2 :revision revision-0))) 2))
      (is (= (length (slot-value occ-1 'd::themes)) 2))
      (is (= (length (slot-value occ-2 'd::themes)) 1))
      (is (= (length (slot-value top-1 'd::used-as-theme)) 1))
      (is (= (length (slot-value top-2 'd::used-as-theme)) 2)))))


(test test-RoleC ()
  "Tests various functions of the class RoleC."
  (with-fixture with-empty-db (*db-dir*)
    (let ((role-1 (make-instance 'RoleC))
	  (role-2 (make-instance 'RoleC))
	  (assoc-1 (make-instance 'AssociationC))
	  (assoc-2 (make-instance 'AssociationC))
	  (rev-0 0)
	  (rev-1 100)
	  (rev-2 200)
	  (rev-3 300)
	  (rev-4 400))
      (setf *TM-REVISION* rev-1)
      (is-false (roles assoc-1 :revision rev-0))
      (is-false (parent role-1 :revision rev-0))
      (add-parent role-1 assoc-1)
      (is (= (length (d::versions assoc-1)) 1))
      (is-true (find-if #'(lambda(vi)
			    (and (= (d::start-revision vi) rev-1)
				 (= (d::end-revision vi) 0)))
			(d::versions assoc-1)))
      (is (eql (parent role-1 :revision rev-1) assoc-1))
      (is (= (length (union (list role-1)
			    (roles assoc-1))) 1))
      (add-role assoc-1 role-2 :revision rev-2)
      (is (= (length (d::versions assoc-1)) 2))
      (is-true (find-if #'(lambda(vi)
			    (and (= (d::start-revision vi) rev-2)
				 (= (d::end-revision vi) 0)))
			(d::versions assoc-1)))
      (is (= (length (union (list role-1 role-2)
			    (roles assoc-1 :revision rev-0))) 2))
      (is (= (length (union (list role-1)
			    (roles assoc-1 :revision rev-1))) 1))
      (is (eql (parent role-1 :revision rev-0) assoc-1))
      (is (eql (parent role-2 :revision rev-2) assoc-1))
      (is-false (parent role-2 :revision rev-1))
      (signals tm-reference-error (add-parent role-2 assoc-2 :revision rev-2))
      (delete-role assoc-1 role-1 :revision rev-3)
      (is (= (length (d::versions assoc-1)) 3))
      (is-true (find-if #'(lambda(vi)
			    (and (= (d::start-revision vi) rev-3)
				 (= (d::end-revision vi) 0)))
			(d::versions assoc-1)))
      (is-false (parent role-1 :revision rev-0))
      (is (= (length (union (list role-2)
			    (roles assoc-1 :revision rev-0))) 1))
      (delete-parent role-2 assoc-1 :revision rev-3)
      (is-false (parent role-2 :revision rev-0))
      (is (eql assoc-1 (parent role-2 :revision rev-2)))
      (is-false (roles assoc-1 :revision rev-0))
      (add-role assoc-2 role-1 :revision rev-3)
      (add-parent role-2 assoc-2 :revision rev-3)
      (is (eql (parent role-2 :revision rev-0) assoc-2))
      (is (= (length (union (list role-1 role-2)
			    (roles assoc-2))) 2))
      (add-role assoc-2 role-1 :revision rev-3)
      (add-parent role-2 assoc-2 :revision rev-3)
      (is (eql (parent role-2 :revision rev-0) assoc-2))
      (is (= (length (union (list role-1 role-2)
			    (roles assoc-2 :revision rev-0))) 2))
      (is (= (length (slot-value assoc-1 'roles)) 2))
      (is (= (length (slot-value assoc-2 'roles)) 2))
      (is (= (length (slot-value role-1 'parent)) 2))
      (is (= (length (slot-value role-2 'parent)) 2))
      (delete-parent role-1 assoc-2 :revision rev-4)
      (is (= (length (d::versions assoc-2)) 2))
      (is-true (find-if #'(lambda(vi)
			    (and (= (d::start-revision vi) rev-4)
				 (= (d::end-revision vi) 0)))
			(d::versions assoc-2))))))


(test test-player ()
  "Tests various functions of the topics that are used as player in roles."
  (with-fixture with-empty-db (*db-dir*)
    (let ((role-1 (make-instance 'RoleC))
	  (role-2 (make-instance 'RoleC))
	  (top-1 (make-instance 'TopicC))
	  (top-2 (make-instance 'TopicC))
	  (revision-0 0)
	  (revision-0-5 50)
	  (revision-1 100)
	  (revision-2 200)
	  (revision-3 300))
      (setf *TM-REVISION* revision-1)
      (is-false (player role-1 :revision revision-0))
      (add-player role-1 top-1)
      (is (eql top-1 (player role-1 :revision revision-0)))
      (is-false (player role-1 :revision revision-0-5))
      (is (eql top-1 (player role-1 :revision revision-2)))
      (add-player role-1 top-1)
      (is (eql top-1 (player role-1 :revision revision-0)))
      (is-false (player role-1 :revision revision-0-5))
      (is (eql top-1 (player role-1 :revision revision-2)))
      (signals tm-reference-error (add-player role-1 top-2))
      (add-player role-2 top-1 :revision revision-2)
      (is (= (length (union (list role-1 role-2)
			    (player-in-roles top-1 :revision revision-0))) 2))
      (is (= (length (union (list role-1)
			    (player-in-roles top-1
					  :revision revision-1))) 1))
      (delete-player role-1 top-1 :revision revision-3)
      (is-false (player role-1 :revision revision-0))
      (is (= (length (union (list role-2)
			    (player-in-roles top-1 :revision revision-0))) 1))
      (add-player role-1 top-1 :revision revision-3)
      (is (eql top-1 (player role-1 :revision revision-0)))
      (is (= (length (union (list role-1 role-2)
			    (player-in-roles top-1 :revision revision-0))) 2))
      (is (= (length (slot-value top-1 'd::player-in-roles)) 2)))))


(test test-TopicMapC ()
  "Tests various function of the class TopicMapC."
  (with-fixture with-empty-db (*db-dir*)
    (let ((tm-1 (make-instance 'TopicMapC))
	  (tm-2 (make-instance 'TopicMapC))
	  (top-1 (make-instance 'TopicC))
	  (assoc-1 (make-instance 'AssociationC))
	  (revision-0-5 50)
	  (revision-1 100))
      (setf *TM-REVISION* revision-1)
      (is-false (topics tm-1))
      (is-false (in-topicmaps top-1))
      (is-false (in-topicmaps assoc-1))
      (d::add-to-version-history top-1 :start-revision revision-1)
      (add-to-tm tm-1 top-1)
      (is (= (length (union (list top-1)
			    (topics tm-1))) 1))
      (is (= (length (union (list tm-1)
			    (in-topicmaps top-1))) 1))
      (is-false (in-topicmaps top-1 :revision revision-0-5))
      (d::add-to-version-history assoc-1 :start-revision revision-1)
      (add-to-tm tm-1 assoc-1)
      (is (= (length (union (list assoc-1)
			    (associations tm-1))) 1))
      (is (= (length (union (list tm-1)
			    (in-topicmaps assoc-1))) 1))
      (is-false (in-topicmaps assoc-1 :revision revision-0-5))
      (add-to-tm tm-2 top-1)
      (is (= (length (union (list top-1)
			    (topics tm-2))) 1))
      (is (= (length (union (list tm-2 tm-1)
			    (in-topicmaps top-1))) 2))
      (is-false (in-topicmaps top-1 :revision revision-0-5))
      (d::add-to-version-history assoc-1 :start-revision revision-1)
      (add-to-tm tm-2 assoc-1)
      (is (= (length (union (list assoc-1)
			    (associations tm-2))) 1))
      (is (= (length (union (list tm-2 tm-1)
			    (in-topicmaps assoc-1))) 2))
      (is-false (in-topicmaps assoc-1 :revision revision-0-5)))))


(test test-delete-ItemIdentifierC ()
  "Tests the function delete-construct of the class ItemIdentifierC."
  (with-fixture with-empty-db (*db-dir*)
    (let ((ii-1 (make-instance 'ItemIdentifierC :uri "ii-1"))
	  (ii-2 (make-instance 'ItemIdentifierC :uri "ii-2"))
	  (ii-3 (make-instance 'ItemIdentifierC :uri "ii-3"))
	  (ii-4 (make-instance 'ItemIdentifierC :uri "ii-4"))
	  (occ-1 (make-instance 'OccurrenceC))
	  (occ-2 (make-instance 'OccurrenceC))
	  (name-1 (make-instance 'NameC))
	  (name-2 (make-instance 'NameC))
	  (revision-1 100)
	  (revision-2 200))
      (setf *TM-REVISION* revision-1)
      (add-item-identifier occ-1 ii-1 :revision revision-1)
      (add-item-identifier occ-1 ii-2 :revision revision-2)
      (delete-item-identifier occ-1 ii-1 :revision revision-2)
      (add-item-identifier name-1 ii-1 :revision revision-2)
      (is (= (length (elephant:get-instances-by-class 'd::ItemIdAssociationC))
	     3))
      (is (= (length (elephant:get-instances-by-class 'ItemIdentifierC)) 4))
      (delete-construct ii-3)
      (is (= (length (elephant:get-instances-by-class 'ItemIdentifierC)) 3))
      (is (= (length (elephant:get-instances-by-class 'd::ItemIdAssociationC))
	     3))
      (delete-construct ii-1)
      (is (= (length (elephant:get-instances-by-class 'ItemIdentifierC)) 2))
      (is (= (length (elephant:get-instances-by-class 'd::ItemIdAssociationC))
	     1))
      (delete-construct occ-1)
      (is (= (length (elephant:get-instances-by-class 'ItemIdentifierC)) 1))
      (is-false (elephant:get-instances-by-class 'd::ItemIdAssociationC))
      (add-item-identifier occ-2 ii-4 :revision revision-1)
      (delete-item-identifier occ-2 ii-4 :revision revision-2)
      (add-item-identifier name-2 ii-4 :revision revision-2)
      (is (= (length (elephant:get-instances-by-class 'd::ItemIdAssociationC))
	     2))
      (delete-construct occ-2)
      (is (= (length (elephant:get-instances-by-class 'd::ItemIdAssociationC))
	     1))
      (is (= (length (elephant:get-instances-by-class 'ItemIdentifierC)) 1))
      (is (= (length (union (list ii-4) (item-identifiers name-2))) 1))
      (delete-construct name-2)
      (is-false (elephant:get-instances-by-class 'd::ItemIdAssociationC))
      (is-false (elephant:get-instances-by-class 'ItemIdentifierC)))))



(test test-delete-PersistentIdC ()
  "Tests the function delete-construct of the class PersistentIdC."
  (with-fixture with-empty-db (*db-dir*)
    (let ((psi-1 (make-instance 'PersistentIdC :uri "psi-1"))
	  (psi-2 (make-instance 'PersistentIdC :uri "psi-2"))
	  (psi-3 (make-instance 'PersistentIdC :uri "psi-3"))
	  (psi-4 (make-instance 'PersistentIdC :uri "psi-4"))
	  (topic-1 (make-instance 'TopicC))
	  (topic-2 (make-instance 'TopicC))
	  (topic-3 (make-instance 'TopicC))
	  (topic-4 (make-instance 'TopicC))
	  (revision-1 100)
	  (revision-2 200))
      (setf *TM-REVISION* revision-1)
      (add-psi topic-1 psi-1 :revision revision-1)
      (add-psi topic-1 psi-2 :revision revision-2)
      (delete-psi topic-1 psi-1 :revision revision-2)
      (add-psi topic-3 psi-1 :revision revision-2)
      (is (= (length (elephant:get-instances-by-class 'd::PersistentIdAssociationC))
	     3))
      (is (= (length (elephant:get-instances-by-class 'PersistentIdC)) 4))
      (delete-construct psi-3)
      (is (= (length (elephant:get-instances-by-class 'PersistentIdC)) 3))
      (is (= (length (elephant:get-instances-by-class 'd::PersistentIdAssociationC))
	     3))
      (delete-construct psi-1)
      (is (= (length (elephant:get-instances-by-class 'PersistentIdC)) 2))
      (is (= (length (elephant:get-instances-by-class 'd::PersistentIdAssociationC))
	     1))
      (delete-construct topic-1)
      (is (= (length (elephant:get-instances-by-class 'PersistentIdC)) 1))
      (is-false (elephant:get-instances-by-class 'd::PersistentIdAssociationC))
      (add-psi topic-2 psi-4 :revision revision-1)
      (delete-psi topic-2 psi-4 :revision revision-2)
      (add-psi topic-4 psi-4 :revision revision-2)
      (is (= (length (elephant:get-instances-by-class 'd::PersistentIdAssociationC))
	     2))
      (is (= (length (elephant:get-instances-by-class 'PersistentIdC)) 1))
      (delete-construct topic-2)
      (is (= (length (elephant:get-instances-by-class 'd::PersistentIdAssociationC))
	     1))
      (is (= (length (elephant:get-instances-by-class 'PersistentIdC)) 1))
      (is (= (length (union (list psi-4) (psis topic-4))) 1)))))


(test test-delete-SubjectLocatorC ()
  "Tests the function delete-construct of the class SubjectLocatorC."
  (with-fixture with-empty-db (*db-dir*)
    (let ((sl-1 (make-instance 'SubjectLocatorC :uri "sl-1"))
	  (sl-2 (make-instance 'SubjectLocatorC :uri "sl-2"))
	  (sl-3 (make-instance 'SubjectLocatorC :uri "sl-3"))
	  (sl-4 (make-instance 'SubjectLocatorC :uri "sl-4"))
	  (topic-1 (make-instance 'TopicC))
	  (topic-2 (make-instance 'TopicC))
	  (topic-3 (make-instance 'TopicC))
	  (topic-4 (make-instance 'TopicC))
	  (revision-1 100)
	  (revision-2 200))
      (setf *TM-REVISION* revision-1)
      (add-locator topic-1 sl-1 :revision revision-1)
      (add-locator topic-1 sl-2 :revision revision-2)
      (delete-locator topic-1 sl-1 :revision revision-2)
      (add-locator topic-3 sl-1 :revision revision-2)
      (is (= (length (elephant:get-instances-by-class 'd::SubjectLocatorAssociationC))
	     3))
      (is (= (length (elephant:get-instances-by-class 'SubjectLocatorC)) 4))
      (delete-construct sl-3)
      (is (= (length (elephant:get-instances-by-class 'SubjectLocatorC)) 3))
      (is (= (length (elephant:get-instances-by-class 'd::SubjectLocatorAssociationC))
	     3))
      (delete-construct sl-1)
      (is (= (length (elephant:get-instances-by-class 'SubjectLocatorC)) 2))
      (is (= (length (elephant:get-instances-by-class 'd::SubjectLocatorAssociationC))
	     1))
      (delete-construct topic-1)
      (is (= (length (elephant:get-instances-by-class 'SubjectLocatorC)) 1))
      (is-false (elephant:get-instances-by-class 'd::SubjectLocatorAssociationC))
      (add-locator topic-2 sl-4 :revision revision-1)
      (delete-locator topic-2 sl-4 :revision revision-2)
      (add-locator topic-4 sl-4 :revision revision-2)
      (is (= (length (elephant:get-instances-by-class 'd::SubjectLocatorAssociationC))
	     2))
      (is (= (length (elephant:get-instances-by-class 'SubjectLocatorC)) 1))
      (delete-construct topic-2)
      (is (= (length (elephant:get-instances-by-class 'd::SubjectLocatorAssociationC))
	     1))
      (is (= (length (elephant:get-instances-by-class 'SubjectLocatorC)) 1))
      (is (= (length (union (list sl-4) (locators topic-4))) 1)))))



(test test-delete-ReifiableConstructC ()
  "Tests the function delete-construct of the class ReifiableConstructC"
  (with-fixture with-empty-db (*db-dir*)
    (let ((rc-1 (make-instance 'd::ReifiableConstructC))
	  (rc-2 (make-instance 'd::ReifiableConstructC))
	  (reifier-1 (make-instance 'TopicC))
	  (reifier-2 (make-instance 'TopicC))
	  (ii-1 (make-instance 'ItemIdentifierC :uri "ii-1"))
	  (revision-0 0)
	  (revision-1 100)
	  (revision-2 200))
      (setf *TM-REVISION* revision-1)
      (add-reifier rc-1 reifier-1)
      (add-item-identifier rc-1 ii-1)
      (is (= (length (elephant:get-instances-by-class 'd::ReifiableConstructC))
	     2))
      (is (= (length (elephant:get-instances-by-class 'd::ItemIdAssociationC))
	     1))
      (is (= (length (elephant:get-instances-by-class 'd::ReifierAssociationC))
	     1))
      (delete-reifier rc-1 reifier-1 :revision revision-2)
      (delete-item-identifier rc-1 ii-1 :revision revision-2)
      (add-reifier rc-2 reifier-1 :revision revision-2)
      (add-item-identifier rc-2 ii-1 :revision revision-2)
      (is (= (length (elephant:get-instances-by-class 'd::ItemIdAssociationC))
	     2))
      (is (= (length (elephant:get-instances-by-class 'd::ReifierAssociationC))
	     2))
      (delete-construct rc-1)
      (is (= (length (elephant:get-instances-by-class 'd::ReifiableConstructC))
	     1))
      (is (= (length (elephant:get-instances-by-class 'd::ItemIdAssociationC))
	     1))
      (is (= (length (elephant:get-instances-by-class 'd::ReifierAssociationC))
	     1))
      (is (= (length (union (list ii-1) (item-identifiers rc-2))) 1))
      (is (eql reifier-1 (reifier rc-2 :revision revision-0)))
      (delete-construct ii-1)
      (delete-construct reifier-1)
      (is (= (length (elephant:get-instances-by-class 'd::ReifiableConstructC))
	     1))
      (is-false (elephant:get-instances-by-class 'd::ItemIdAssociationC))
      (is-false (elephant:get-instances-by-class 'd::ReifierAssociationC))
      (delete-construct reifier-2)
      (is-false (elephant:get-instances-by-class 'd::ReifierAssociationC)))))


(test test-delete-VariantC ()
  "Tests the function delete-construct of the class VariantC"
  (with-fixture with-empty-db (*db-dir*)
    (let ((name-1 (make-instance 'NameC))
	  (name-2 (make-instance 'NameC))
	  (variant-1 (make-instance 'VariantC))
	  (variant-2 (make-instance 'VariantC))
	  (variant-3 (make-instance 'VariantC))
	  (variant-4 (make-instance 'VariantC))
	  (revision-1 100)
	  (revision-2 200))
      (setf *TM-REVISION* revision-1)
      (add-variant name-1 variant-1)
      (add-variant name-1 variant-2)
      (add-variant name-1 variant-3)
      (delete-variant name-1 variant-1 :revision revision-2)
      (delete-variant name-1 variant-2 :revision revision-2)
      (add-variant name-2 variant-1 :revision revision-2)
      (add-variant name-2 variant-2 :revision revision-2)
      (is (= (length (elephant:get-instances-by-class 'd::VariantAssociationC))
	     5))
      (delete-construct variant-1)
      (is (= (length (elephant:get-instances-by-class 'd::VariantAssociationC))
	     3))
      (is (= (length (elephant:get-instances-by-class 'VariantC)) 3))
      (delete-construct name-1)
      (is (= (length (elephant:get-instances-by-class 'd::VariantAssociationC))
	     1))
      (is (= (length (elephant:get-instances-by-class 'VariantC)) 2))
      (delete-construct name-2)
      (is (= (length (elephant:get-instances-by-class 'VariantC)) 1))
      (is-false (elephant:get-instances-by-class 'd::VariantAssociationC))
      (delete-construct variant-4)
      (is-false (elephant:get-instances-by-class 'VariantC)))))


(test test-delete-NameC ()
  "Tests the function delete-construct of the class NameC"
  (with-fixture with-empty-db (*db-dir*)
    (let ((topic-1 (make-instance 'TopicC))
	  (topic-2 (make-instance 'TopicC))
	  (name-1 (make-instance 'NameC))
	  (name-2 (make-instance 'NameC))
	  (name-3 (make-instance 'NameC))
	  (name-4 (make-instance 'NameC))
	  (revision-1 100)
	  (revision-2 200))
      (setf *TM-REVISION* revision-1)
      (add-name topic-1 name-1)
      (add-name topic-1 name-2)
      (add-name topic-1 name-3)
      (delete-name topic-1 name-1 :revision revision-2)
      (delete-name topic-1 name-2 :revision revision-2)
      (add-name topic-2 name-1 :revision revision-2)
      (add-name topic-2 name-2 :revision revision-2)
      (is (= (length (elephant:get-instances-by-class 'd::NameAssociationC))
	     5))
      (delete-construct name-1)
      (is (= (length (elephant:get-instances-by-class 'd::NameAssociationC))
	     3))
      (is (= (length (elephant:get-instances-by-class 'NameC)) 3))
      (delete-construct topic-1)
      (is (= (length (elephant:get-instances-by-class 'd::NameAssociationC))
	     1))
      (is (= (length (elephant:get-instances-by-class 'NameC)) 2))
      (delete-construct topic-2)
      (is (= (length (elephant:get-instances-by-class 'NameC)) 1))
      (is-false (elephant:get-instances-by-class 'd::NameAssociationC))
      (delete-construct name-4)
      (is-false (elephant:get-instances-by-class 'NameC)))))


(test test-delete-OccurrenceC ()
  "Tests the function delete-construct of the class OccurrenceC"
  (with-fixture with-empty-db (*db-dir*)
    (let ((topic-1 (make-instance 'TopicC))
	  (topic-2 (make-instance 'TopicC))
	  (occurrence-1 (make-instance 'OccurrenceC))
	  (occurrence-2 (make-instance 'OccurrenceC))
	  (occurrence-3 (make-instance 'OccurrenceC))
	  (occurrence-4 (make-instance 'OccurrenceC))
	  (revision-1 100)
	  (revision-2 200))
      (setf *TM-REVISION* revision-1)
      (add-occurrence topic-1 occurrence-1)
      (add-occurrence topic-1 occurrence-2)
      (add-occurrence topic-1 occurrence-3)
      (delete-occurrence topic-1 occurrence-1 :revision revision-2)
      (delete-occurrence topic-1 occurrence-2 :revision revision-2)
      (add-occurrence topic-2 occurrence-1 :revision revision-2)
      (add-occurrence topic-2 occurrence-2 :revision revision-2)
      (is (= (length (elephant:get-instances-by-class
		      'd::OccurrenceAssociationC)) 5))
      (delete-construct occurrence-1)
      (is (= (length (elephant:get-instances-by-class
		      'd::OccurrenceAssociationC)) 3))
      (is (= (length (elephant:get-instances-by-class 'OccurrenceC)) 3))
      (delete-construct topic-1)
      (is (= (length (elephant:get-instances-by-class
		      'd::OccurrenceAssociationC)) 1))
      (is (= (length (elephant:get-instances-by-class 'OccurrenceC)) 2))
      (delete-construct topic-2)
      (is (= (length (elephant:get-instances-by-class 'OccurrenceC)) 1))
      (is-false (elephant:get-instances-by-class 'd::OccurrenceAssociationC))
      (delete-construct occurrence-4)
      (is-false (elephant:get-instances-by-class 'OccurrenceC)))))


(test test-delete-TypableC ()
  "Tests the function delete-construct of the class TypableC"
  (with-fixture with-empty-db (*db-dir*)
    (let ((name-1 (make-instance 'NameC))
	  (name-2 (make-instance 'NameC))
	  (type-1 (make-instance 'TopicC))
	  (type-2 (make-instance 'TopicC))
	  (revision-1 100)
	  (revision-2 200))
      (setf *TM-REVISION* revision-1)
      (add-type name-1 type-1)
      (delete-type name-1 type-1 :revision revision-2)
      (add-type name-1 type-2 :revision revision-2)
      (add-type name-2 type-2)
      (is (= (length (elephant:get-instances-by-class 'd::TypeAssociationC)) 3))
      (is (= (length (elephant:get-instances-by-class 'd::NameC)) 2))
      (delete-construct type-2)
      (is (= (length (elephant:get-instances-by-class 'd::TypeAssociationC)) 1))
      (is (= (length (elephant:get-instances-by-class 'd::NameC)) 1))
      (delete-construct name-1)
      (is-false (elephant:get-instances-by-class 'd::TypeAssociationC))
      (is-false (elephant:get-instances-by-class 'd::NameC)))))


(test test-delete-ScopableC ()
  "Tests the function delete-construct of the class ScopableC"
  (with-fixture with-empty-db (*db-dir*)
    (let ((assoc-1 (make-instance 'AssociationC))
	  (assoc-2 (make-instance 'AssociationC))
	  (assoc-3 (make-instance 'AssociationC))
	  (scope-1 (make-instance 'TopicC))
	  (scope-2 (make-instance 'TopicC))
	  (scope-3 (make-instance 'TopicC))
	  (revision-1 100))
      (setf *TM-REVISION* revision-1)
      (add-theme assoc-1 scope-1)
      (add-theme assoc-1 scope-2)
      (add-theme assoc-2 scope-1)
      (is (= (length (elephant:get-instances-by-class 'd::ScopeAssociationC))
	     3))
      (is (= (length (elephant:get-instances-by-class 'AssociationC)) 3))
      (delete-construct scope-1)
      (is (= (length (elephant:get-instances-by-class 'd::ScopeAssociationC))
	     1))
      (is (= (length (elephant:get-instances-by-class 'AssociationC)) 3))
      (delete-construct assoc-1)
      (is-false (elephant:get-instances-by-class 'd::ScopeAssociationC))
      (is (= (length (elephant:get-instances-by-class 'AssociationC)) 2))
      (add-theme assoc-2 scope-3)
      (add-theme assoc-3 scope-3)
      (is (= (length (elephant:get-instances-by-class 'd::ScopeAssociationC))
	     2))
      (is (= (length (elephant:get-instances-by-class 'AssociationC)) 2))
      (delete-construct assoc-2)
      (is (= (length (union (list scope-3) (themes assoc-3))) 1)))))


(test test-delete-AssociationC ()
  "Tests the function delete-construct of the class AssociationC"
  (with-fixture with-empty-db (*db-dir*)
    (let ((role-1 (make-instance 'RoleC))
	  (role-2 (make-instance 'RoleC))
	  (assoc-1 (make-instance 'AssociationC))
	  (assoc-2 (make-instance 'AssociationC))
	  (assoc-3 (make-instance 'AssociationC))
	  (revision-1 100)
	  (revision-2 200))
      (setf *TM-REVISION* revision-1)
      (add-role assoc-1 role-1)
      (delete-role assoc-1 role-1 :revision revision-2)
      (add-role assoc-2 role-1 :revision revision-2)
      (add-role assoc-2 role-2)
      (is (= (length (elephant:get-instances-by-class 'RoleC)) 2))
      (is (= (length (elephant:get-instances-by-class 'AssociationC)) 3))
      (is (= (length (elephant:get-instances-by-class 'd::RoleAssociationC)) 3))
      (delete-construct role-1)
      (is (= (length (elephant:get-instances-by-class 'RoleC)) 1))
      (is (= (length (elephant:get-instances-by-class 'AssociationC)) 3))
      (is (= (length (elephant:get-instances-by-class 'd::RoleAssociationC)) 1))
      (delete-role assoc-2 role-2 :revision revision-2)
      (add-role assoc-3 role-2 :revision revision-2)
      (is (= (length (elephant:get-instances-by-class 'RoleC)) 1))
      (is (= (length (elephant:get-instances-by-class 'AssociationC)) 3))
      (is (= (length (elephant:get-instances-by-class 'd::RoleAssociationC)) 2))
      (delete-construct assoc-3)
      (is (= (length (elephant:get-instances-by-class 'RoleC)) 1))
      (is (= (length (elephant:get-instances-by-class 'AssociationC)) 2))
      (is (= (length (elephant:get-instances-by-class 'd::RoleAssociationC))
	     1)))))


(test test-delete-RoleC ()
  "Tests the function delete-construct of the class RoleC"
  (with-fixture with-empty-db (*db-dir*)
    (let ((role-1 (make-instance 'RoleC))
	  (role-2 (make-instance 'RoleC))
	  (player-1 (make-instance 'TopicC))
	  (player-2 (make-instance 'TopicC))
	  (revision-1 100)
	  (revision-2 200))
      (setf *TM-REVISION* revision-1)
      (add-player role-1 player-1)
      (delete-player role-1 player-1 :revision revision-2)
      (add-player role-1 player-2 :revision revision-2)
      (add-player role-2 player-1)
      (is (= (length (elephant:get-instances-by-class 'RoleC)) 2))
      (is (= (length (elephant:get-instances-by-class 'd::PlayerAssociationC))
	     3))
      (delete-construct player-1)
      (is (= (length (elephant:get-instances-by-class 'RoleC)) 1))
      (is (= (length (elephant:get-instances-by-class 'd::PlayerAssociationC))
	     1))
      (delete-construct role-1)
      (is-false (elephant:get-instances-by-class 'RoleC))
      (is-false (elephant:get-instances-by-class 'd::PlayerAssociationC)))))


(test test-equivalent-PointerC ()
  "Tests the functions equivalent-construct and strictly-equivalent-constructs
   depending on PointerC and its subclasses."
  (with-fixture with-empty-db (*db-dir*)
    (let ((p-1 (make-instance 'd::PointerC :uri "p-1"))
	  (tid-1 (make-instance 'd:TopicIdentificationC :uri "tid-1"
				:xtm-id "xtm-1"))
	  (tid-2 (make-instance 'd:TopicIdentificationC :uri "tid-2"
				:xtm-id "xtm-1"))
	  (tid-3 (make-instance 'd:TopicIdentificationC :uri "tid-1"
				:xtm-id "xtm-2"))
	  (tid-4 (make-instance 'd:TopicIdentificationC :uri "tid-1"
				:xtm-id "xtm-1"))
	  (psi-1 (make-instance 'd:PersistentIdC :uri "psi-1"))
	  (psi-2 (make-instance 'd:PersistentIdC :uri "psi-2"))
	  (psi-3 (make-instance 'd:PersistentIdC :uri "psi-1"))
	  (rev-1 100))
      (setf *TM-REVISION* rev-1)
      (is-true (d::equivalent-construct p-1 :uri "p-1"))
      (is-false (d::equivalent-construct p-1 :uri "p-2"))
      (is-true (d::equivalent-construct tid-1 :uri "tid-1" :xtm-id "xtm-1"))
      (is-false (d::equivalent-construct tid-1 :uri "tid-2" :xtm-id "xtm-1"))
      (is-false (d::equivalent-construct tid-1 :uri "tid-1" :xtm-id "xtm-2"))
      (is-false (d::equivalent-construct tid-1 :uri "tid-2" :xtm-id "xtm-2"))
      (is-true (d::equivalent-construct psi-1 :uri "psi-1"))
      (is-false (d::equivalent-construct psi-1 :uri "psi-2"))
      (is-false (d::strictly-equivalent-constructs tid-1 tid-1))
      (is-false (d::strictly-equivalent-constructs tid-1 tid-2))
      (is-false (d::strictly-equivalent-constructs tid-1 tid-3))
      (is-true (d::strictly-equivalent-constructs tid-1 tid-4))
      (is-false (d::strictly-equivalent-constructs psi-1 psi-1))
      (is-false (d::strictly-equivalent-constructs psi-1 psi-2))
      (is-true (d::strictly-equivalent-constructs psi-1 psi-3)))))


(test test-equivalent-OccurrenceC ()
  "Tests the functions equivalent-construct depending on OccurrenceC."
  (with-fixture with-empty-db (*db-dir*)
    (let ((type-1 (make-instance 'd:TopicC))
	  (type-2 (make-instance 'd:TopicC))
	  (scope-1 (make-instance 'd:TopicC))
	  (scope-2 (make-instance 'd:TopicC))
	  (scope-3 (make-instance 'd:TopicC))
	  (rev-0-5 50)
	  (rev-1 100))
      (let ((occ-1 (make-construct 'OccurrenceC
				   :charvalue "occ-1"
				   :instance-of type-1
				   :themes (list scope-1 scope-2)
				   :start-revision rev-1))
	    (occ-2 (make-construct 'OccurrenceC
				   :charvalue "occ-1"
				   :instance-of type-2
				   :themes (list scope-1 scope-2)
				   :start-revision rev-1))
	    (occ-3 (make-construct 'OccurrenceC
				   :charvalue "occ-1"
				   :instance-of type-1
				   :themes (list scope-3 scope-2)
				   :start-revision rev-1))
	    (occ-4 (make-construct 'OccurrenceC
				   :charvalue "occ-2"
				   :instance-of type-1
				   :themes (list scope-1 scope-2)
				   :start-revision rev-1))
	    (occ-5 (make-construct 'OccurrenceC
				   :charvalue "occ-1"
				   :datatype *xml-uri*
				   :instance-of type-1
				   :themes (list scope-1 scope-2)
				   :start-revision rev-1))
	    (occ-6 (make-construct 'OccurrenceC
				   :charvalue "occ-1"
				   :instance-of type-1
				   :themes (list scope-1)
				   :start-revision rev-1)))
	(setf *TM-REVISION* rev-1)
	(add-theme occ-6 scope-2)
	(is-true (d::equivalent-construct
		  occ-1 :charvalue "occ-1" :datatype *xml-string*
		  :instance-of type-1 :themes (list scope-2 scope-1)))
	(is-false (d::equivalent-construct
		   occ-1 :charvalue "occ-1" :datatype *xml-string*
		   :instance-of type-1 :themes (list scope-2 scope-1)
		   :start-revision rev-0-5))
	(is-false (d::equivalent-construct
		   occ-1 :charvalue "occ-1" :datatype *xml-string*
		   :instance-of type-2 :themes (list scope-1 scope-2)))
	(is-false (d::equivalent-construct
		   occ-1 :charvalue "occ-1" :datatype *xml-string*
		   :instance-of type-1 :themes (list scope-3 scope-2)))
	(is-false (d::equivalent-construct
		   occ-1 :charvalue "occ-1"
		   :instance-of type-1 :themes (list scope-1 scope-2)))
	(is-false (d::equivalent-construct
		   occ-1 :charvalue "occ-2" :datatype *xml-string*
		   :instance-of type-1 :themes (list scope-2 scope-1)))
	(is-false (d::strictly-equivalent-constructs occ-1 occ-1))
	(is-false (d::strictly-equivalent-constructs occ-1 occ-2))
	(is-false (d::strictly-equivalent-constructs occ-1 occ-3))
	(is-false (d::strictly-equivalent-constructs occ-1 occ-4))
	(is-false (d::strictly-equivalent-constructs occ-1 occ-5))
	(is-true (d::strictly-equivalent-constructs occ-1 occ-6))))))


(test test-equivalent-NameC ()
  "Tests the functions equivalent-construct depending on NameC."
  (with-fixture with-empty-db (*db-dir*)
    (let ((type-1 (make-instance 'd:TopicC))
	  (type-2 (make-instance 'd:TopicC))
	  (scope-1 (make-instance 'd:TopicC))
	  (scope-2 (make-instance 'd:TopicC))
	  (scope-3 (make-instance 'd:TopicC))
	  (variant-1 (make-instance 'd:VariantC))
	  (variant-2 (make-instance 'd:VariantC))
	  (rev-0-5 50)
	  (rev-1 100))
      (let ((name-1 (make-construct 'NameC
				    :charvalue "name-1"
				    :instance-of type-1
				    :themes (list scope-1 scope-2)
				    :start-revision rev-1))
	    (name-2 (make-construct 'NameC
				    :charvalue "name-2"
				    :instance-of type-1
				    :themes (list scope-1 scope-2)
				    :start-revision rev-1))
	    (name-3 (make-construct 'NameC
				    :charvalue "name-1"
				    :instance-of type-2
				    :themes (list scope-1 scope-2)
				    :start-revision rev-1))
	    (name-4 (make-construct 'NameC
				    :charvalue "name-1"
				    :instance-of type-1
				    :themes (list scope-3 scope-2)
				    :start-revision rev-1))
	    (name-5 (make-construct 'NameC
				    :charvalue "name-1"
				    :instance-of type-1
				    :themes (list scope-2)
				    :variants (list variant-1 variant-2)
				    :start-revision rev-1)))
	(setf *TM-REVISION* rev-1)
	(add-theme name-5 scope-1)
	(is-true (d::equivalent-construct
		  name-1 :charvalue "name-1" :instance-of type-1
		  :themes (list scope-2 scope-1)))
	(is-false (d::equivalent-construct
		   name-1 :charvalue "name-1" :instance-of type-1
		   :themes (list scope-2 scope-1)
		   :start-revision rev-0-5))
	(is-false (d::equivalent-construct
		   name-1 :charvalue "name-1" :instance-of type-2
		   :themes (list scope-1 scope-2)))
	(is-false (d::equivalent-construct
		   name-1 :charvalue "name-1" :instance-of type-1
		   :themes (list scope-3 scope-2)))
	(is-false (d::equivalent-construct
		   name-1 :charvalue "name-2" :instance-of type-1
		   :themes (list scope-2 scope-1)))
	(is-false (d::strictly-equivalent-constructs name-1 name-1))
	(is-false (d::strictly-equivalent-constructs name-1 name-2))
	(is-false (d::strictly-equivalent-constructs name-1 name-3))
	(is-false (d::strictly-equivalent-constructs name-1 name-4))
	(is-true (d::strictly-equivalent-constructs name-1 name-5))))))


(test test-equivalent-VariantC ()
  "Tests the functions equivalent-construct depending on VariantC."
  (with-fixture with-empty-db (*db-dir*)
    (let ((scope-1 (make-instance 'd:TopicC))
	  (scope-2 (make-instance 'd:TopicC))
	  (scope-3 (make-instance 'd:TopicC))
	  (rev-0-5 50)
	  (rev-1 100))
      (let ((var-1 (make-construct 'VariantC
				   :charvalue "var-1"
				   :themes (list scope-1 scope-2)
				   :start-revision rev-1))
	    (var-2 (make-construct 'VariantC
				   :charvalue "var-2"
				   :themes (list scope-1 scope-2)
				   :start-revision rev-1))
	    (var-3 (make-construct 'VariantC
				   :charvalue "var-1"
				   :themes (list scope-1 scope-3)
				   :start-revision rev-1))
	    (var-4 (make-construct 'VariantC
				   :charvalue "var-1"
				   :datatype *xml-uri*
				   :themes (list scope-1 scope-2)
				   :start-revision rev-1))
	    (var-5 (make-construct 'VariantC
				   :charvalue "var-1"
				   :themes (list scope-1)
				   :start-revision rev-1)))
	(setf *TM-REVISION* rev-1)
	(add-theme var-5 scope-2)
	(is-true (d::equivalent-construct
		  var-1 :charvalue "var-1" :datatype constants:*xml-string*
		  :themes (list scope-2 scope-1)))
	(is-false (d::equivalent-construct
		   var-1 :charvalue "var-1" :datatype constants:*xml-string*
		   :themes (list scope-2 scope-1)
		   :start-revision rev-0-5))
	(is-false (d::equivalent-construct
		   var-1 :charvalue "var-1" :datatype constants:*xml-string*
		   :themes (list scope-3 scope-2)))
	(is-false (d::equivalent-construct
		   var-1 :charvalue "var-1"
		   :themes (list scope-1 scope-2)))
	(is-false (d::equivalent-construct
		   var-1 :charvalue "var-2" :datatype constants:*xml-string*
		   :themes (list scope-2 scope-1)))
	(is-false (d::strictly-equivalent-constructs var-1 var-1))
	(is-false (d::strictly-equivalent-constructs var-1 var-2))
	(is-false (d::strictly-equivalent-constructs var-1 var-3))
	(is-false (d::strictly-equivalent-constructs var-1 var-4))
	(is-true (d::strictly-equivalent-constructs var-1 var-5))))))


(test test-equivalent-RoleC ()
  "Tests the functions equivalent-construct depending on RoleC."
  (with-fixture with-empty-db (*db-dir*)
    (let ((type-1 (make-instance 'd:TopicC))
	  (type-2 (make-instance 'd:TopicC))
	  (player-1 (make-instance 'd:TopicC))
	  (player-2 (make-instance 'd:TopicC))
	  (rev-1 100)
	  (rev-2 200))
      (let ((role-1 (make-construct 'RoleC
				    :player player-1
				    :instance-of type-1
				    :start-revision rev-1))
	    (role-2 (make-construct 'RoleC
				    :player player-2
				    :instance-of type-1
				    :start-revision rev-1))
	    (role-3 (make-construct 'RoleC
				    :player player-1
				    :instance-of type-2
				    :start-revision rev-1))
	    (role-4 (make-construct 'RoleC
				    :instance-of type-1
				    :start-revision rev-1)))
	(setf *TM-REVISION* rev-1)
	(add-player role-4 player-1)
	(is-true (d::equivalent-construct role-1 :player player-1
					  :instance-of type-1))
	(is-false (d::equivalent-construct role-1 :player player-2
					   :instance-of type-1))
	(is-false (d::equivalent-construct role-1 :player player-1
					   :instance-of type-2))
	(is-false (d::strictly-equivalent-constructs role-1 role-1))
	(is-false (d::strictly-equivalent-constructs role-1 role-2))
	(is-false (d::strictly-equivalent-constructs role-1 role-3))
	(is-true (d::strictly-equivalent-constructs role-1 role-4))
	(setf *TM-REVISION* rev-2)
	(delete-player role-1 player-1 :revision rev-2)
	(add-player role-1 player-2)
	(delete-type role-1 type-1 :revision rev-2)
	(add-type role-1 type-2)
	(is-true (d::equivalent-construct role-1 :player player-2
					  :instance-of type-2))
	(is-false (d::equivalent-construct role-1 :player player-1
					   :instance-of type-2))
	(is-false (d::equivalent-construct role-1 :player player-2
					   :instance-of type-1))))))


(test test-equivalent-AssociationC ()
  "Tests the functions equivalent-construct depending on AssociationC."
  (with-fixture with-empty-db (*db-dir*)
    (let ((player-1 (make-instance 'TopicC))
	  (player-2 (make-instance 'TopicC))
	  (player-3 (make-instance 'TopicC))
	  (r-type-1 (make-instance 'TopicC))
	  (r-type-2 (make-instance 'TopicC))
	  (r-type-3 (make-instance 'TopicC))
	  (rev-1 100))
      (let ((role-1 (list :player player-1 :instance-of r-type-1
			  :start-revision rev-1))
	    (role-2 (list :player player-2 :instance-of r-type-2
			  :start-revision rev-1))
	    (role-3 (list :player player-3 :instance-of r-type-3
			  :start-revision rev-1))
	    (type-1 (make-instance 'd:TopicC))
	    (type-2 (make-instance 'd:TopicC))
	    (scope-1 (make-instance 'd:TopicC))
	    (scope-2 (make-instance 'd:TopicC))
	    (scope-3 (make-instance 'd:TopicC)))
	(let ((assoc-1 (make-construct 'AssociationC
				       :roles (list role-1 role-2)
				       :instance-of type-1
				       :themes (list scope-1 scope-2)
				       :start-revision rev-1))
	      (assoc-2 (make-construct 'AssociationC
				       :roles (list role-1 role-2 role-3)
				       :instance-of type-1
				       :themes (list scope-1 scope-2)
				       :start-revision rev-1))
	      (assoc-3 (make-construct 'AssociationC
				       :roles (list role-1 role-3)
				       :instance-of type-1
				       :themes (list scope-1 scope-2)
				       :start-revision rev-1))
	      (assoc-4 (make-construct 'AssociationC
				       :roles (list role-1 role-2)
				       :instance-of type-2
				       :themes (list scope-1 scope-2)
				       :start-revision rev-1))
	      (assoc-5 (make-construct 'AssociationC
				       :roles (list role-1 role-2)
				       :instance-of type-1
				       :themes (list scope-1 scope-3)
				       :start-revision rev-1))
	      (assoc-6 (make-construct 'AssociationC
				       :roles (list role-1)
				       :instance-of type-1
				       :themes (list scope-1 scope-2)
				       :start-revision rev-1)))
	  (setf *TM-REVISION* rev-1)
	  (add-role assoc-6 (apply #'make-construct 'RoleC role-2))
	  (is-true (d::equivalent-construct
		    assoc-1 :roles (list role-1 role-2)
		    :instance-of type-1 :themes (list scope-1 scope-2)))
	  (is-false (d::equivalent-construct
		     assoc-1 :roles (list role-1 role-2 role-3)
		     :instance-of type-1 :themes (list scope-1 scope-2)))
	  (is-false (d::equivalent-construct
		     assoc-1 :roles (list role-1)
		     :instance-of type-1 :themes (list scope-1 scope-2)))
	  (is-false (d::equivalent-construct
		     assoc-1 :roles (list role-1 role-3)
		     :instance-of type-1 :themes (list scope-1 scope-2)))
	  (is-false (d::equivalent-construct
		     assoc-1 :roles (list role-1 role-2)
		     :instance-of type-2 :themes (list scope-1 scope-2)))
	  (is-false (d::equivalent-construct
		     assoc-1 :roles (list role-1 role-2)
		     :instance-of type-2 :themes (list scope-1 scope-3)))
	  (is-false (d::strictly-equivalent-constructs assoc-1 assoc-1))
	  (is-false (d::strictly-equivalent-constructs assoc-1 assoc-2))
	  (is-false (d::strictly-equivalent-constructs assoc-1 assoc-3))
	  (is-false (d::strictly-equivalent-constructs assoc-1 assoc-4))
	  (is-false (d::strictly-equivalent-constructs assoc-1 assoc-5))
	  (is-true (d::strictly-equivalent-constructs assoc-1 assoc-6)))))))


(test test-equivalent-TopicC ()
  "Tests the functions equivalent-construct depending on TopicC."
  (with-fixture with-empty-db (*db-dir*)
    (let ((ii-1 (make-instance 'd:ItemIdentifierC :uri "ii-1"))
	  (ii-2 (make-instance 'd:ItemIdentifierC :uri "ii-2"))
	  (sl-1 (make-instance 'd:SubjectLocatorC :uri "sl-1"))
	  (sl-2 (make-instance 'd:SubjectLocatorC :uri "sl-2"))
	  (psi-1 (make-instance 'd:PersistentIdC :uri "psi-1"))
	  (psi-2 (make-instance 'd:PersistentIdC :uri "psi-2"))
	  (tid-1 (make-instance 'd:TopicIdentificationC :uri "tid-1"
				:xtm-id "xtm-id-1"))
	  (tid-2 (make-instance 'd:TopicIdentificationC :uri "tid-2"
				:xtm-id "xtm-id-2"))
	  (rev-1 100))
      (let ((top-1 (make-construct 'TopicC
				   :item-identifiers (list ii-1)
				   :locators (list sl-1)
				   :psis (list psi-1)
				   :topic-identifiers (list tid-1)
				   :start-revision rev-1))
	    (top-2 (make-construct 'TopicC
				   :item-identifiers (list ii-2)
				   :locators (list sl-2)
				   :psis (list psi-2)
				   :topic-identifiers (list tid-2)
				   :start-revision rev-1)))
	(setf *TM-REVISION* rev-1)
	(is-true (d::equivalent-construct top-1
					  :item-identifiers (list ii-1 ii-2)))
	(is-true (d::equivalent-construct top-1 :locators (list sl-1 sl-2)
					  :psis (list psi-1 psi-2)
					  :item-identifiers (list ii-1 ii-2)))
	(is-true (d::equivalent-construct top-1 :locators (list sl-1 sl-2)))
	(is-true (d::equivalent-construct top-1 :psis (list psi-1 psi-2)))
	(is-true (d::equivalent-construct top-1 :topic-identifiers (list tid-1)))
	(is-false (d::equivalent-construct top-1 :topic-identifiers (list tid-2)))
	(is-false (d::equivalent-construct top-1 :item-identifiers (list ii-2)
					   :psis (list psi-2)
					   :locators (list sl-2)))
	(is-false (d::strictly-equivalent-constructs top-1 top-1))
	(is-false (d::strictly-equivalent-constructs top-1 top-2))))))


(test test-equivalent-TopicMapC ()
  "Tests the functions equivalent-construct depending on TopicMapC."
  (with-fixture with-empty-db (*db-dir*)
    (let ((ii-1 (make-instance 'd:ItemIdentifierC :uri "ii-1"))
	  (ii-2 (make-instance 'd:ItemIdentifierC :uri "ii-2"))
	  (reifier-1 (make-instance 'd:TopicC))
	  (reifier-2 (make-instance 'd:TopicC))
	  (rev-1 100))
      (let ((tm-1 (make-construct 'TopicMapC
				  :item-identifiers (list ii-1)
				  :reifier reifier-1
				  :start-revision rev-1))
	    (tm-2 (make-construct 'TopicMapC
				  :item-identifiers (list ii-2)
				  :reifier reifier-2
				  :start-revision rev-1)))
	(setf *TM-REVISION* rev-1)
	(is-true (d::equivalent-construct tm-1
					  :item-identifiers (list ii-1 ii-2)))
	(is-true (d::equivalent-construct tm-1 :reifier reifier-1))
	(is-false (d::equivalent-construct tm-1 :item-identifiers (list ii-2)))
	(is-false (d::equivalent-construct tm-1 :reifier reifier-2))
	(is-false (d::strictly-equivalent-constructs tm-1 tm-1))
	;in our definition TopicMapC-constructs are always equal, since
	;item-identifiers and reifiers are not used for TMDM equlity
	(is-true (d::strictly-equivalent-constructs tm-1 tm-2))))))


(test test-class-p ()
  "Tests the functions <class>-p."
  (let ((identifier (list 'd::IdentifierC 'd::ItemIdentifierC 'd:PersistentIdC
			  'd:SubjectLocatorC))
	(topic-identifier (list 'd::TopicIdentificationC))
	(characteristic (list 'd::CharacteristicC 'd:OccurrenceC 'd:NameC
			      'd:VariantC))
	(topic (list 'd:TopicC))
	(assoc (list 'd:AssociationC))
	(role (list 'd:AssociationC))
	(tm (list 'd:TopicMapC)))
    (let ((pointer (append identifier topic-identifier))
	  (reifiable (append topic assoc role tm characteristic))
	  (typable (append characteristic assoc role))
	  (scopable (append characteristic assoc)))
  (dolist (class pointer)
    (is-true (d:PointerC-p class)))
  (dolist (class identifier)
    (is-true (d:IdentifierC-p class)))
  (dolist (class topic-identifier)
    (is-true (d:TopicIdentificationC-p class)))
  (is-true (d:PersistentIdC-p 'd:PersistentIdC))
  (is-true (d:SubjectLocatorC-p 'd:SubjectLocatorC))
  (is-true (d:ItemIdentifierC-p 'd:ItemIdentifierC))
  (dolist (class characteristic)
    (is-true (d:CharacteristicC-p class)))
  (is-true (d:OccurrenceC-p 'd:OccurrenceC))
  (is-true (d:VariantC-p 'd:VariantC))
  (is-true (d:NameC-p 'd:NameC))
  (is-true (d:RoleC-p 'd:RoleC))
  (is-true (d:AssociationC-p 'd:AssociationC))
  (is-true (d:TopicC-p 'd:TopicC))
  (is-true (d:TopicMapC-p 'd:TopicMapC))
  (dolist (class reifiable)
    (is-true (d:ReifiableconstructC-p class)))
  (dolist (class scopable)
    (is-true (d:ScopableC-p class)))
  (dolist (class typable)
    (is-true (d:TypableC-p class)))
  (dolist (class (append reifiable pointer))
    (is-true (d:TopicMapConstructC-p class)))
  (dolist (class (append topic tm assoc))
    (is-true (d:VersionedConstructC-p class)))
  (dolist (class identifier)
    (is-false (d:TopicIdentificationC-p class)))
  (dolist (class topic-identifier)
    (is-false (d:IdentifierC-p class)))
  (dolist (class characteristic)
    (is-false (d:PointerC-p class))))))


(test test-find-item-by-revision ()
  "Tests the function find-item-by-revision."
  (with-fixture with-empty-db (*db-dir*)
    (let ((top-1 (make-instance 'TopicC))
	  (top-2 (make-instance 'TopicC))
	  (assoc-1 (make-instance 'AssociationC))
	  (assoc-2 (make-instance 'AssociationC))
	  (ii-1 (make-instance 'ItemIdentifierC :uri "ii-1"))
	  (ii-2 (make-instance 'ItemIdentifierC :uri "ii-2"))
	  (psi-1 (make-instance 'PersistentIdC :uri "psi-1"))
	  (name-1 (make-instance 'NameC))
	  (name-2 (make-instance 'NameC))
	  (variant-1 (make-instance 'VariantC))
	  (role-1 (make-instance 'RoleC))
	  (rev-0 0)
	  (rev-0-5 50)
	  (rev-1 100)
	  (rev-2 200)
	  (rev-3 300)
	  (rev-4 400)
	  (rev-5 500))
      (setf *TM-REVISION* rev-1)
      (d::add-to-version-history top-1 :start-revision rev-1)
      (d::add-to-version-history top-1 :start-revision rev-3)
      (is (eql top-1 (find-item-by-revision top-1 rev-1)))
      (is (eql top-1 (find-item-by-revision top-1 rev-0)))
      (is (eql top-1 (find-item-by-revision top-1 rev-4)))
      (is (eql top-1 (find-item-by-revision top-1 rev-2)))
      (is-false (find-item-by-revision top-1 rev-0-5))
      (add-item-identifier top-1 ii-1 :revision rev-3)
      (add-item-identifier top-1 ii-2 :revision rev-3)
      (add-item-identifier top-1 ii-1 :revision rev-4)
      (delete-item-identifier top-1 ii-1 :revision rev-5)
      (add-item-identifier top-2 ii-1 :revision rev-5)
      (add-psi top-2 psi-1 :revision rev-1)
      (is (eql ii-1 (find-item-by-revision ii-1 rev-3 top-1)))
      (is (eql ii-1 (find-item-by-revision ii-1 rev-4 top-1)))
      (is-false (find-item-by-revision ii-1 rev-2 top-1))
      (is-false (find-item-by-revision ii-1 rev-5 top-1))
      (is-false (find-item-by-revision ii-1 rev-3))
      (is-false (find-item-by-revision ii-1 rev-0 top-1))
      (is (eql ii-1 (find-item-by-revision ii-1 rev-5 top-2)))
      (add-role assoc-1 role-1 :revision rev-1)
      (delete-role assoc-1 role-1 :revision rev-3)
      (add-role assoc-2 role-1 :revision rev-5)
      (is (eql role-1 (find-item-by-revision role-1 rev-1 assoc-1)))
      (is (eql role-1 (find-item-by-revision role-1 rev-2 assoc-1)))
      (is (eql role-1 (find-item-by-revision role-1 rev-5 assoc-2)))
      (is (eql role-1 (find-item-by-revision role-1 rev-0 assoc-2)))
      (is-false (find-item-by-revision role-1 rev-0-5 assoc-1))
      (is-false (find-item-by-revision role-1 rev-0 assoc-1))
      (is-false (find-item-by-revision role-1 rev-3 assoc-1))
      (is-false (find-item-by-revision role-1 rev-3 assoc-2))
      (add-name top-1 name-1 :revision rev-1)
      (delete-name top-1 name-1 :revision rev-3)
      (add-name top-2 name-1 :revision rev-3)
      (is (eql name-1 (find-item-by-revision name-1 rev-1 top-1)))
      (is (eql name-1 (find-item-by-revision name-1 rev-2 top-1)))
      (is (eql name-1 (find-item-by-revision name-1 rev-5 top-2)))
      (is (eql name-1 (find-item-by-revision name-1 rev-0 top-2)))
      (is-false (find-item-by-revision name-1 rev-0-5 top-1))
      (is-false (find-item-by-revision name-1 rev-0 top-1))
      (is-false (find-item-by-revision name-1 rev-3 top-1))
      (add-variant name-1 variant-1 :revision rev-1)
      (delete-variant name-1 variant-1 :revision rev-3)
      (add-variant name-2 variant-1 :revision rev-3)
      (is (eql variant-1 (find-item-by-revision variant-1 rev-1 name-1)))
      (is (eql variant-1 (find-item-by-revision variant-1 rev-2 name-1)))
      (is (eql variant-1 (find-item-by-revision variant-1 rev-5 name-2)))
      (is (eql variant-1 (find-item-by-revision variant-1 rev-0 name-2)))
      (is-false (find-item-by-revision variant-1 rev-0-5 name-1))
      (is-false (find-item-by-revision variant-1 rev-0 name-1))
      (is-false (find-item-by-revision variant-1 rev-3 name-1)))))



(test test-make-Unknown ()
  "Tests the function make-construct corresponding to an unknown class."
  (defclass Unknown ()
    ((value :initarg :value)))
  (let ((construct (make-construct 'Unknown :value "value")))
    (is-true construct)
    (is (string= (slot-value construct 'value) "value"))))


(test test-make-VersionedConstructC ()
  "Tests the function make-construct corresponding to VersionedConstructC."
  (with-fixture with-empty-db (*db-dir*)
    (let ((psi-1 (make-instance 'PersistentIdC :uri "psi-1"))
	  (top-1 (make-instance 'TopicC))
	  (rev-0 0)
	  (rev-1 100)
	  (rev-2 200))
      (setf *TM-REVISION* rev-1)
      (let ((vc (make-construct 'VersionedConstructC
				:start-revision rev-2))
	    (psi-assoc (make-construct 'd::PersistentIdAssociationC
				       :start-revision rev-1
				       :identifier psi-1
				       :parent-construct top-1)))
	(signals missing-argument-error
	  (make-construct 'd::PersistentIdAssociationC
			  :start-revision rev-1
			  :identifier psi-1))
	(setf *TM-REVISION* rev-1)
	(signals missing-argument-error (make-construct 'VersionedConstructC))
	(is (= (length (d::versions vc)) 1))
	(is-true (find-if #'(lambda(vi)
			      (and (= (d::start-revision vi) rev-2)
				   (= (d::end-revision vi) rev-0)))
			  (d::versions vc)))
	(is (= (length (d::versions psi-assoc)) 1))
	(is-true (find-if #'(lambda(vi)
			      (and (= (d::start-revision vi) rev-1)
				   (= (d::end-revision vi) rev-0)))
			  (d::versions psi-assoc)))))))


(test test-make-TopicIdentificationC ()
  "Tests the function make-construct corresponding to TopicIdentificationC."
  (with-fixture with-empty-db (*db-dir*)
    (let ((rev-0 0)
	  (rev-0-5 50)
	  (rev-1 100)
	  (top-1 (make-instance 'TopicC)))
      (let ((tid-1 (make-construct 'TopicIdentificationC
				   :uri "tid-1" :xtm-id "xtm-id-1"))
	    (tid-2 (make-construct 'TopicIdentificationC
				   :uri "tid-2" :xtm-id "xtm-id-2"
				   :identified-construct top-1
				   :start-revision rev-1)))
	(signals missing-argument-error (make-construct 'TopicIdentificationC
				       :uri "uri"))
	(signals missing-argument-error (make-construct 'TopicIdentificationC
				       :xtm-id "xtm-id"))
	(setf *TM-REVISION* rev-1)
	(signals missing-argument-error
	  (make-construct 'TopicIdentificationC :uri "uri"
			  :identified-construct top-1))
	(is (string= (uri tid-1) "tid-1"))
	(is (string= (xtm-id tid-1) "xtm-id-1"))
	(is-false (d::slot-p tid-1 'd::identified-construct))
	(is (string= (uri tid-2) "tid-2"))
	(is (string= (xtm-id tid-2) "xtm-id-2"))
	(is (= (length (d::slot-p tid-2 'd::identified-construct)) 1))
	(is (= (length (d::versions
			(first (d::slot-p tid-2 'd::identified-construct)))) 1))
	(is (= (d::start-revision
		(first (d::versions
			(first (d::slot-p tid-2 'd::identified-construct)))))
	       rev-1))
	(is (= (d::end-revision
		(first (d::versions
			(first (d::slot-p tid-2 'd::identified-construct)))))
	       rev-0))
	(is (eql (identified-construct tid-2 :revision rev-1) top-1))
	(is-false (identified-construct tid-2 :revision rev-0-5))
	(is (eql (find-item-by-revision tid-2 rev-1 top-1) tid-2))))))


(test test-make-PersistentIdC ()
  "Tests the function make-construct corresponding to PersistentIdC."
  (with-fixture with-empty-db (*db-dir*)
    (let ((rev-0 0)
	  (rev-0-5 50)
	  (rev-1 100)
	  (top-1 (make-instance 'TopicC)))
      (let ((psi-1 (make-construct 'PersistentIdC :uri "psi-1"))
	    (psi-2 (make-construct 'PersistentIdC
				   :uri "psi-2"
				   :identified-construct top-1
				   :start-revision rev-1)))
	(setf *TM-REVISION* rev-1)
	(signals missing-argument-error (make-construct 'PersistentIdC))
	(signals missing-argument-error (make-construct 'PersistentIdC :uri "uri"
				       :identified-construct top-1))
	(is (string= (uri psi-1) "psi-1"))
	(is-false (d::slot-p psi-1 'd::identified-construct))
	(is (string= (uri psi-2) "psi-2"))
	(is (= (length (d::slot-p psi-2 'd::identified-construct)) 1))
	(is (= (length (d::versions
			(first (d::slot-p psi-2 'd::identified-construct)))) 1))
	(is (= (d::start-revision
		(first (d::versions
			(first (d::slot-p psi-2 'd::identified-construct)))))
	       rev-1))
	(is (= (d::end-revision
		(first (d::versions
			(first (d::slot-p psi-2 'd::identified-construct)))))
	       rev-0))
	(is (eql (identified-construct psi-2 :revision rev-1) top-1))
	(is-false (identified-construct psi-2 :revision rev-0-5))
	(is (eql (find-item-by-revision psi-2 rev-1 top-1) psi-2))))))


(test test-make-SubjectLocatorC ()
  "Tests the function make-construct corresponding to SubjectLocatorC."
  (with-fixture with-empty-db (*db-dir*)
    (let ((rev-0 0)
	  (rev-0-5 50)
	  (rev-1 100)
	  (top-1 (make-instance 'TopicC)))
      (let ((sl-1 (make-construct 'SubjectLocatorC :uri "sl-1"))
	    (sl-2 (make-construct 'SubjectLocatorC
				  :uri "sl-2"
				  :identified-construct top-1
				  :start-revision rev-1)))
	(setf *TM-REVISION* rev-1)
	(signals missing-argument-error (make-construct 'SubjectLocatorC))
	(signals missing-argument-error (make-construct 'SubjectLocatorC :uri "uri"
				       :identified-construct top-1))
	(is (string= (uri sl-1) "sl-1"))
	(is-false (d::slot-p sl-1 'd::identified-construct))
	(is (string= (uri sl-2) "sl-2"))
	(is (= (length (d::slot-p sl-2 'd::identified-construct)) 1))
	(is (= (length (d::versions
			(first (d::slot-p sl-2 'd::identified-construct)))) 1))
	(is (= (d::start-revision
		(first (d::versions
			(first (d::slot-p sl-2 'd::identified-construct)))))
	       rev-1))
	(is (= (d::end-revision
		(first (d::versions
			(first (d::slot-p sl-2 'd::identified-construct)))))
	       rev-0))
	(is (eql (identified-construct sl-2 :revision rev-1) top-1))
	(is-false (identified-construct sl-2 :revision rev-0-5))
	(is (eql (find-item-by-revision sl-2 rev-1 top-1) sl-2))))))


(test test-make-ItemIdentifierC ()
  "Tests the function make-construct corresponding to ItemIdentifierC."
  (with-fixture with-empty-db (*db-dir*)
    (let ((rev-0 0)
	  (rev-0-5 50)
	  (rev-1 100)
	  (top-1 (make-instance 'AssociationC)))
      (let ((ii-1 (make-construct 'ItemIdentifierC :uri "ii-1"))
	    (ii-2 (make-construct 'ItemIdentifierC
				  :uri "ii-2"
				  :identified-construct top-1
				  :start-revision rev-1)))
	(setf *TM-REVISION* rev-1)
	(signals missing-argument-error (make-construct 'ItemIdentifierC))
	(signals missing-argument-error (make-construct 'ItemIdentifierC :uri "uri"
				       :identified-construct top-1))
	(is (string= (uri ii-1) "ii-1"))
	(is-false (d::slot-p ii-1 'd::identified-construct))
	(is (string= (uri ii-2) "ii-2"))
	(is (= (length (d::slot-p ii-2 'd::identified-construct)) 1))
	(is (= (length (d::versions
			(first (d::slot-p ii-2 'd::identified-construct)))) 1))
	(is (= (d::start-revision
		(first (d::versions
			(first (d::slot-p ii-2 'd::identified-construct)))))
	       rev-1))
	(is (= (d::end-revision
		(first (d::versions
			(first (d::slot-p ii-2 'd::identified-construct)))))
	       rev-0))
	(is (eql (identified-construct ii-2 :revision rev-1) top-1))
	(is-false (identified-construct ii-2 :revision rev-0-5))
	(is (eql (find-item-by-revision ii-2 rev-1 top-1) ii-2))))))


(test test-make-OccurrenceC ()
  "Tests the function make-construct corresponding to OccurrenceC."
  (with-fixture with-empty-db (*db-dir*)
    (let ((rev-0-5 50)
	  (rev-1 100)
	  (type-1 (make-instance 'TopicC))
	  (theme-1 (make-instance 'TopicC))
	  (theme-2 (make-instance 'TopicC))
	  (ii-1 (make-instance 'ItemIdentifierC :uri "ii-1"))
	  (ii-2 (make-instance 'ItemIdentifierC :uri "ii-2"))
	  (reifier-1 (make-instance 'TopicC))
	  (top-1 (make-instance 'TopicC)))
      (setf *TM-REVISION* rev-1)
      (let ((occ-1 (make-construct 'OccurrenceC))
	    (occ-2 (make-construct 'OccurrenceC
				   :charvalue "charvalue"
				   :datatype "datatype"
				   :item-identifiers (list ii-1 ii-2)
				   :reifier reifier-1
				   :instance-of type-1
				   :themes (list theme-1 theme-2)
				   :start-revision rev-1))
	    (occ-3 (make-construct 'OccurrenceC
				   :charvalue "charvalue-2"
				   :parent top-1
				   :start-revision rev-1)))
	(setf *TM-REVISION* rev-1)
	(signals missing-argument-error
	  (make-construct 'OccurrenceC :item-identifiers (list ii-1)))
	(signals missing-argument-error
	  (make-construct 'OccurrenceC :reifier reifier-1))
	(signals missing-argument-error
	  (make-construct 'OccurrenceC :parent top-1))
	(signals missing-argument-error
	  (make-construct 'OccurrenceC :instance-of type-1))
	(signals missing-argument-error
	  (make-construct 'OccurrenceC :themes (list theme-1)))
	(is (string= (charvalue occ-1) ""))
	(is (string= (datatype occ-1) *xml-string*))
	(is-false (item-identifiers occ-1))
	(is-false (reifier occ-1))
	(is-false (instance-of occ-1))
	(is-false (themes occ-1))
	(is-false (parent occ-1))
	(is (string= (charvalue occ-2) "charvalue"))
	(is (string= (datatype occ-2) "datatype"))
	(is-true (item-identifiers occ-2))
	(is (= (length (union (list ii-1 ii-2) (item-identifiers occ-2))) 2))
	(is (eql (reifier occ-2) reifier-1))
	(is (eql (instance-of occ-2) type-1))
	(is-true (themes occ-2))
	(is (= (length (union (list theme-1 theme-2) (themes occ-2))) 2))
	(is-false (parent occ-2))
	(is (eql ii-1 (find-item-by-revision ii-1 rev-1 occ-2)))
	(is-false (item-identifiers occ-2 :revision rev-0-5))
	(is (eql (parent occ-3) top-1))
	(is (eql occ-3 (find-item-by-revision occ-3 rev-1 top-1)))))))


(test test-make-NameC ()
  "Tests the function make-construct corresponding to NameC."
  (with-fixture with-empty-db (*db-dir*)
    (let ((rev-0-5 50)
	  (rev-1 100)
	  (type-1 (make-instance 'TopicC))
	  (theme-1 (make-instance 'TopicC))
	  (theme-2 (make-instance 'TopicC))
	  (ii-1 (make-instance 'ItemIdentifierC :uri "ii-1"))
	  (ii-2 (make-instance 'ItemIdentifierC :uri "ii-2"))
	  (reifier-1 (make-instance 'TopicC))
	  (variant-1 (make-instance 'VariantC))
	  (variant-2 (make-instance 'VariantC))
	  (top-1 (make-instance 'TopicC)))
      (setf *TM-REVISION* rev-1)
      (let ((name-1 (make-construct 'NameC))
	    (name-2 (make-construct 'NameC
				   :charvalue "charvalue"
				   :variants (list variant-1 variant-2)
				   :item-identifiers (list ii-1 ii-2)
				   :reifier reifier-1
				   :instance-of type-1
				   :themes (list theme-1 theme-2)
				   :start-revision rev-1))
	    (name-3 (make-construct 'NameC
				   :charvalue "charvalue-2"
				   :parent top-1
				   :start-revision rev-1)))
	(setf *TM-REVISION* rev-1)
	(signals missing-argument-error
	  (make-construct 'NameC :item-identifiers (list ii-1)))
	(signals missing-argument-error
	  (make-construct 'NameC :reifier reifier-1))
	(signals missing-argument-error
	  (make-construct 'NameC :parent top-1))
	(signals missing-argument-error
	  (make-construct 'NameC :instance-of type-1))
	(signals missing-argument-error
	  (make-construct 'NameC :themes (list theme-1)))
	(signals missing-argument-error
	  (make-construct 'NameC :variants (list variant-1)))
	(is (string= (charvalue name-1) ""))
	(is-false (item-identifiers name-1))
	(is-false (reifier name-1))
	(is-false (instance-of name-1))
	(is-false (themes name-1))
	(is-false (parent name-1))
	(is-false (variants name-1))
	(is (string= (charvalue name-2) "charvalue"))
	(is-true (item-identifiers name-2))
	(is (= (length (union (list ii-1 ii-2) (item-identifiers name-2))) 2))
	(is (eql (reifier name-2) reifier-1))
	(is (eql (instance-of name-2) type-1))
	(is-true (themes name-2))
	(is (= (length (union (list theme-1 theme-2) (themes name-2))) 2))
	(is-true (variants name-2))
	(is (= (length (union (list variant-1 variant-2) (variants name-2))) 2))
	(is-false (parent name-2))
	(is (eql ii-1 (find-item-by-revision ii-1 rev-1 name-2)))
	(is-false (item-identifiers name-2 :revision rev-0-5))
	(is (eql (parent name-3) top-1))
	(is (eql name-3 (find-item-by-revision name-3 rev-1 top-1)))))))


(test test-make-VariantC ()
  "Tests the function make-construct corresponding to VariantC."
  (with-fixture with-empty-db (*db-dir*)
    (let ((rev-0-5 50)
	  (rev-1 100)
	  (theme-1 (make-instance 'TopicC))
	  (theme-2 (make-instance 'TopicC))
	  (ii-1 (make-instance 'ItemIdentifierC :uri "ii-1"))
	  (ii-2 (make-instance 'ItemIdentifierC :uri "ii-2"))
	  (reifier-1 (make-instance 'TopicC))
	  (name-1 (make-instance 'NameC)))
      (setf *TM-REVISION* rev-1)
      (let ((variant-1 (make-construct 'VariantC))
	    (variant-2 (make-construct 'VariantC
				   :charvalue "charvalue"
				   :datatype "datatype"
				   :item-identifiers (list ii-1 ii-2)
				   :reifier reifier-1
				   :themes (list theme-1 theme-2)
				   :start-revision rev-1))
	    (variant-3 (make-construct 'VariantC
				   :charvalue "charvalue-2"
				   :parent name-1
				   :start-revision rev-1)))
	(setf *TM-REVISION* rev-1)
	(signals missing-argument-error
	  (make-construct 'VariantC :item-identifiers (list ii-1)))
	(signals missing-argument-error
	  (make-construct 'VariantC :reifier reifier-1))
	(signals missing-argument-error
	  (make-construct 'VariantC :parent name-1))
	(signals missing-argument-error
	  (make-construct 'VariantC :themes (list theme-1)))
	(is (string= (charvalue variant-1) ""))
	(is (string= (datatype variant-1) *xml-string*))
	(is-false (item-identifiers variant-1))
	(is-false (reifier variant-1))
	(is-false (instance-of variant-1))
	(is-false (themes variant-1))
	(is-false (parent variant-1))
	(is (string= (charvalue variant-2) "charvalue"))
	(is (string= (datatype variant-2) "datatype"))
	(is-true (item-identifiers variant-2))
	(is (= (length (union (list ii-1 ii-2) (item-identifiers variant-2))) 2))
	(is (eql (reifier variant-2) reifier-1))
	(is-true (themes variant-2))
	(is (= (length (union (list theme-1 theme-2) (themes variant-2))) 2))
	(is-false (parent variant-2))
	(is (eql ii-1 (find-item-by-revision ii-1 rev-1 variant-2)))
	(is-false (item-identifiers variant-2 :revision rev-0-5))
	(is (eql (parent variant-3) name-1))
	(is (eql variant-3 (find-item-by-revision variant-3 rev-1 name-1)))))))


(test test-make-RoleC ()
  "Tests the function make-construct corresponding to RoleC."
  (with-fixture with-empty-db (*db-dir*)
    (let ((rev-0-5 50)
	  (rev-1 100)
	  (type-1 (make-instance 'TopicC))
	  (ii-1 (make-instance 'ItemIdentifierC :uri "ii-1"))
	  (ii-2 (make-instance 'ItemIdentifierC :uri "ii-2"))
	  (player-1 (make-instance 'TopicC))
	  (reifier-1 (make-instance 'TopicC))
	  (assoc-1 (make-instance 'AssociationC)))
      (setf *TM-REVISION* rev-1)
      (let ((role-1 (make-construct 'RoleC))
	    (role-2 (make-construct 'RoleC
				   :item-identifiers (list ii-1 ii-2)
				   :player player-1
				   :reifier reifier-1
				   :instance-of type-1
				   :start-revision rev-1))
	    (role-3 (make-construct 'RoleC
				   :parent assoc-1
				   :start-revision rev-1)))
	(setf *TM-REVISION* rev-1)
	(signals missing-argument-error
	  (make-construct 'RoleC :item-identifiers (list ii-1)))
	(signals missing-argument-error
	  (make-construct 'RoleC :reifier reifier-1))
	(signals missing-argument-error
	  (make-construct 'RoleC :parent assoc-1))
	(signals missing-argument-error
	  (make-construct 'RoleC :instance-of type-1))
	(signals missing-argument-error
	  (make-construct 'RoleC :player player-1))
	(is-false (item-identifiers role-1))
	(is-false (reifier role-1))
	(is-false (instance-of role-1))
	(is-false (parent role-1))
	(is-false (player role-1))
	(is-true (item-identifiers role-2))
	(is (= (length (union (list ii-1 ii-2) (item-identifiers role-2))) 2))
	(is (eql (reifier role-2) reifier-1))
	(is (eql (instance-of role-2) type-1))
	(is-false (parent role-2))
	(is (eql (player role-2) player-1))
	(is (eql ii-1 (find-item-by-revision ii-1 rev-1 role-2)))
	(is-false (item-identifiers role-2 :revision rev-0-5))
	(is (eql (parent role-3) assoc-1))
	(is (eql role-3 (find-item-by-revision role-3 rev-1 assoc-1)))))))


(test test-make-TopicMapC ()
  "Tests the function make-construct corresponding to TopicMapC."
  (with-fixture with-empty-db (*db-dir*)
    (let ((rev-1 100)
	  (top-1 (make-instance 'TopicC))
	  (top-2 (make-instance 'TopicC))
	  (top-3 (make-instance 'TopicC))
	  (assoc-1 (make-instance 'AssociationC))
	  (assoc-2 (make-instance 'AssociationC))
	  (assoc-3 (make-instance 'AssociationC))
	  (ii-1 (make-instance 'ItemIdentifierC :uri "ii-1"))
	  (ii-2 (make-instance 'ItemIdentifierC :uri "ii-2"))
	  (ii-3 (make-instance 'ItemIdentifierC :uri "ii-3"))
	  (ii-4 (make-instance 'ItemIdentifierC :uri "ii-4"))
	  (reifier-1 (make-instance 'TopicC)))
      (let ((tm-1 (make-construct 'TopicMapC
				  :start-revision rev-1
				  :topics (list top-1 top-2)
				  :associations (list assoc-1 assoc-2)
				  :item-identifiers (list ii-1 ii-2)
				  :reifier reifier-1))
	    (tm-2 (make-construct 'TopicMapC
				  :start-revision rev-1
				  :item-identifiers (list ii-3))))
	(setf *TM-REVISION* rev-1)
	(signals missing-argument-error (make-construct 'TopicMapC))
	(is (eql (reifier tm-1) reifier-1))
	(is (= (length (item-identifiers tm-1)) 2))
	(is (= (length (union (item-identifiers tm-1) (list ii-1 ii-2))) 2))
	(is (= (length (topics tm-1)) 2))
	(is (= (length (union (topics tm-1) (list top-1 top-2))) 2))
	(is (= (length (associations tm-1)) 2))
	(is (= (length (union (associations tm-1) (list assoc-1 assoc-2))) 2))
	(is (eql (find-item-by-revision tm-1 rev-1) tm-1))
	(is (= (length (item-identifiers tm-2)) 1))
	(is (= (length (union (item-identifiers tm-2) (list ii-3))) 1))
	(is-false (topics tm-2))
	(is-false (associations tm-2))
	(is-false (reifier tm-2))
	(let ((tm-3 (make-construct 'TopicMapC
				    :start-revision rev-1
				    :topics (list top-3)
				    :associations (list assoc-3)
				    :item-identifiers (list ii-2 ii-4))))
	  (is (eql (reifier tm-3) reifier-1))
	  (is (= (length (item-identifiers tm-3)) 3))
	  (is (= (length (union (item-identifiers tm-3) (list ii-1 ii-2 ii-4)))
		 3))
	  (is (= (length (topics tm-3)) 3))
	  (is (= (length (union (topics tm-3) (list top-1 top-2 top-3))) 3))
	  (is (= (length (associations tm-3)) 3))
	  (is (= (length (union (associations tm-3)
				(list assoc-1 assoc-2 assoc-3)))
		 3))
	  (is (eql (find-item-by-revision tm-3 rev-1) tm-3)))))))


(test test-make-AssociationC ()
  "Tests the function make-construct corresponding to TopicMapC."
  (with-fixture with-empty-db (*db-dir*)
    (let ((rev-1 100)
	  (player-1 (make-instance 'TopicC))
	  (player-2 (make-instance 'TopicC))
	  (type-1 (make-instance 'TopicC))
	  (r-type-1 (make-instance 'TopicC))
	  (r-type-2 (make-instance 'TopicC))
	  (theme-1 (make-instance 'TopicC))
	  (theme-2 (make-instance 'TopicC))
	  (reifier-1 (make-instance 'TopicC))
	  (r-reifier-1 (make-instance 'TopicC))
	  (ii-1 (make-construct 'ItemIdentifierC :uri "ii-1"))
	  (ii-2 (make-construct 'ItemIdentifierC :uri "ii-2"))
	  (r-ii-1 (make-construct 'ItemIdentifierC :uri "r-ii-1"))
	  (r-ii-2 (make-construct 'ItemIdentifierC :uri "r-ii-2"))
	  (r-ii-3 (make-construct 'ItemIdentifierC :uri "r-ii-3")))
      (let ((role-1 (list :item-identifiers (list r-ii-1) :player player-1
			  :instance-of r-type-1 :reifier r-reifier-1
			  :start-revision rev-1))
	    (role-2 (list :item-identifiers (list r-ii-2 r-ii-3)
			  :player player-2 :instance-of r-type-2
			  :start-revision rev-1))
	    (role-2-2 (list :player player-2 :instance-of r-type-2
			    :start-revision rev-1))
	    (tm-1 (make-construct 'TopicMapC :start-revision rev-1))
	    (tm-2 (make-construct 'TopicMapC :start-revision rev-1)))
	(let ((assoc-1 (make-construct 'AssociationC
				       :start-revision rev-1
				       :instance-of type-1
				       :themes (list theme-1 theme-2)
				       :item-identifiers (list ii-1 ii-2)
				       :reifier reifier-1
				       :in-topicmaps (list tm-1 tm-2)
				       :roles (list role-1 role-2 role-2-2)))
	      (assoc-2 (make-construct 'AssociationC :start-revision rev-1)))
	  (setf *TM-REVISION* rev-1)
	  (signals missing-argument-error (make-construct 'AssociationC))
	  (signals missing-argument-error
	    (make-construct 'AssociationC
			    :start-revision rev-1
			    :roles (list (list :player player-1
					       :instance-of r-type-1))))
	  (is (eql (instance-of assoc-1) type-1))
	  (is-true (themes assoc-1))
	  (is (= (length (union (list theme-1 theme-2) (themes assoc-1))) 2))
	  (is-true (item-identifiers assoc-1))
	  (is (= (length (union (list ii-1 ii-2) (item-identifiers assoc-1))) 2))
	  (is (eql (reifier assoc-1) reifier-1))
	  (is-true (in-topicmaps assoc-1))
	  (is (= (length (union (list tm-1 tm-2) (in-topicmaps assoc-1))) 2))
	  (is (= (length (roles assoc-1)) 2))
	  (is (= (length
		  (remove-if
		   #'null
		   (map 
		    'list 
		    #'(lambda(role)
			(when (or (and (eql (player role :revision rev-1)
					    player-1)
				       (eql (instance-of role :revision rev-1)
					    r-type-1)
				       (= (length (item-identifiers
						   role :revision rev-1)) 1)
				       (string=
					(uri (first (item-identifiers role)))
					"r-ii-1"))
				  (and (eql (player role :revision rev-1)
					    player-2)
				       (eql (instance-of role :revision rev-1)
					    r-type-2)
				       (= (length (item-identifiers role)) 2)
				       (let ((uri-1
					      (uri (first 
						    (item-identifiers
						     role :revision rev-1))))
					     (uri-2
					      (uri (second
						    (item-identifiers
						     role :revision rev-1)))))
					 (and (or (string= uri-1 "r-ii-2")
						  (string= uri-2 "r-ii-2"))
					      (or (string= uri-1 "r-ii-3")
						  (string= uri-2 "r-ii-3"))))))
			  role))
		    (roles assoc-1 :revision rev-1))))
		 2))
	  (is (eql (find-item-by-revision assoc-1 rev-1) assoc-1))
	  (is-false (item-identifiers assoc-2))
	  (is-false (reifier assoc-2))
	  (is-false (instance-of assoc-2))
	  (is-false (themes assoc-2))
	  (is-false (roles assoc-2))
	  (is-false (in-topicmaps assoc-2))
	  (let ((assoc-3 (make-construct 'AssociationC
					 :start-revision rev-1
					 :roles (list role-1 role-2)
					 :instance-of type-1
					 :themes (list theme-1 theme-2))))
	    (is (eql (instance-of assoc-3) type-1))
	    (is-true (themes assoc-3))
	    (is (= (length (union (list theme-1 theme-2) (themes assoc-3))) 2))
	    (is-true (item-identifiers assoc-3))
	    (is (= (length (union (list ii-1 ii-2) (item-identifiers assoc-3))) 2))
	    (is (eql (reifier assoc-3) reifier-1))
	    (is-true (in-topicmaps assoc-3))
	    (is (= (length (union (list tm-1 tm-2) (in-topicmaps assoc-3))) 2))
	    (is (= (length (roles assoc-3)) 2))))))))


(test test-make-TopicC ()
  "Tests the function make-construct corresponding to TopicC."
  (with-fixture with-empty-db (*db-dir*)
    (let ((rev-1 100)
	  (ii-1 (make-construct 'ItemIdentifierC :uri "ii-1"))
	  (ii-2 (make-construct 'ItemIdentifierC :uri "ii-2"))
	  (ii-3 (make-construct 'ItemIdentifierC :uri "ii-3"))
	  (psi-1 (make-construct 'PersistentIdC :uri "psi-1"))
	  (psi-2 (make-construct 'PersistentIdC :uri "psi-2"))
	  (psi-3 (make-construct 'PersistentIdC :uri "psi-3"))
	  (sl-1 (make-construct 'SubjectLocatorC :uri "sl-1"))
	  (sl-2 (make-construct 'SubjectLocatorC :uri "sl-2"))
	  (sl-3 (make-construct 'SubjectLocatorC :uri "sl-3"))
	  (variant-1 (make-construct 'VariantC :datatype "dt-1"
				   :charvalue "cv-1"))
	  (variant-2 (make-construct 'VariantC :datatype "dt-2"
				     :charvalue "cv-2"))
	  (type-1 (make-instance 'TopicC))
	  (type-2 (make-instance 'TopicC))
	  (type-3 (make-instance 'TopicC))
	  (theme-1 (make-instance 'TopicC))
	  (theme-2 (make-instance 'TopicC))
	  (theme-3 (make-instance 'TopicC)))
      (let ((name-1 (make-construct 'NameC :charvalue "cv-3"
				    :start-revision rev-1
				    :variants (list variant-1)
				    :instance-of type-1
				    :themes (list theme-1 theme-2)))
	    (name-2 (make-construct 'NameC :charvalue "cv-4"
				    :start-revision rev-1
				    :variants (list variant-2)
				    :instance-of type-2
				    :themes (list theme-3 theme-2)))
	    (occ-1 (make-construct 'OccurrenceC :charvalue "cv-5"
				   :start-revision rev-1
				   :themes (list theme-1)
				   :instance-of type-3)))
	(let ((top-1 (make-construct 'TopicC :start-revision rev-1))
	      (top-2 (make-construct 'TopicC :start-revision rev-1
				     :item-identifiers (list ii-1 ii-2)
				     :psis (list psi-1 psi-2 psi-3)
				     :locators (list sl-1 sl-2)
				     :names (list name-1)
				     :occurrences (list occ-1))))
	  (setf *TM-REVISION* rev-1)
	  (signals missing-argument-error (make-construct 'TopicC))
	  (is-false (item-identifiers top-1))
	  (is-false (psis top-1))
	  (is-false (locators top-1))
	  (is-false (names top-1))
	  (is-false (occurrences top-1))
	  (is (eql (find-item-by-revision top-1 rev-1) top-1))
	  (is (= (length (item-identifiers top-2)) 2))
	  (is (= (length (union (list ii-1 ii-2) (item-identifiers top-2))) 2))
	  (is (= (length (locators top-2)) 2))
	  (is (= (length (union (list sl-1 sl-2) (locators top-2))) 2))
	  (is (= (length (psis top-2)) 3))
	  (is (= (length (union (list psi-1 psi-2 psi-3) (psis top-2))) 3))
	  (is (= (length (names top-2)) 1))
	  (is (eql (first (names top-2)) name-1))
	  (is (= (length (occurrences top-2)) 1))
	  (is (eql (first (occurrences top-2)) occ-1))
	  (is (eql (find-item-by-revision occ-1 rev-1 top-2) occ-1))
	  (let ((top-3 (make-construct 'TopicC :start-revision rev-1
				       :item-identifiers (list ii-2 ii-3)
				       :locators (list sl-3)
				       :names (list name-2))))
	    (is (= (length (item-identifiers top-3)) 3))
	    (is (= (length (union (list ii-1 ii-2 ii-3)
				  (item-identifiers top-3))) 3))
	    (is (= (length (locators top-3)) 3))
	    (is (= (length (union (list sl-1 sl-2 sl-3) (locators top-3))) 3))
	    (is (= (length (psis top-3)) 3))
	    (is (= (length (union (list psi-1 psi-2 psi-3) (psis top-3))) 3))
	    (is (= (length (names top-3)) 2))
	    (is (= (length (union (list name-1 name-2) (names top-3))) 2))
	    (is (= (length (occurrences top-3)) 1))
	    (is (eql (first (occurrences top-3)) occ-1))))))))


(test test-find-oldest-construct ()
  "Tests the generic find-oldest-construct."
  (with-fixture with-empty-db (*db-dir*)
    (let ((rev-1 100)
	  (rev-2 200)
	  (rev-3 300))
      (let ((theme-1 (make-construct 'TopicC :start-revision rev-1))
	    (theme-2 (make-construct 'TopicC :start-revision rev-1))
	    (player-1 (make-construct 'TopicC :start-revision rev-1))
	    (player-2 (make-construct 'TopicC :start-revision rev-1)))
	(let ((top-1 (make-instance 'TopicC))
	      (top-2 (make-instance 'TopicC))
	      (tm-1 (make-instance 'TopicMapC))
	      (tm-2 (make-instance 'TopicMapC))
	      (assoc-1 (make-instance 'AssociationC))
	      (assoc-2 (make-instance 'AssociationC))
	      (ii-1 (make-instance 'ItemIdentifierC :uri "ii-1"))
	      (ii-2 (make-instance 'ItemIdentifierC :uri "ii-2"))
	      (variant-1 (make-construct 'VariantC
					 :start-revision rev-1
					 :charvalue "var-1"
					 :themes (list theme-1)))
	      (variant-2 (make-construct 'VariantC
					 :start-revision rev-1
					 :charvalue "var-2"
					 :themes (list theme-2)))
	      (name-1 (make-instance 'NameC))
	      (name-2 (make-instance 'NameC))
	      (role-1 (make-construct 'RoleC
				      :start-revision rev-1
				      :player player-1))
	      (role-2 (make-construct 'RoleC
				      :start-revision rev-1
				      :player player-2)))
	  (setf *TM-REVISION* rev-1)
	  (is (eql ii-1 (d::find-oldest-construct ii-1 ii-2)))
	  (add-item-identifier top-1 ii-1 :revision rev-3)
	  (is (eql ii-1 (d::find-oldest-construct ii-1 ii-2)))
	  (add-item-identifier assoc-1 ii-2 :revision rev-2)
	  (is (eql ii-2 (d::find-oldest-construct ii-1 ii-2)))
	  (add-item-identifier top-2 ii-1 :revision rev-1)
	  (is (eql ii-1 (d::find-oldest-construct ii-1 ii-2)))
	  (is (eql variant-1 (d::find-oldest-construct variant-1 variant-2)))
	  (add-variant name-1 variant-1 :revision rev-3)
	  (is (eql variant-1 (d::find-oldest-construct variant-1 variant-2)))
	  (add-variant name-1 variant-2 :revision rev-2)
	  (is (eql variant-2 (d::find-oldest-construct variant-1 variant-2))) ;x
	  (add-variant name-2 variant-1 :revision rev-1)
	  (is (eql variant-1 (d::find-oldest-construct variant-1 variant-2)))
	  (is (eql role-1 (d::find-oldest-construct role-1 role-2)))
	  (add-role assoc-1 role-1 :revision rev-3)
	  (is (eql role-1 (d::find-oldest-construct role-1 role-2))) ;x
	  (add-role assoc-1 role-2 :revision rev-2)
	  (is (eql role-2 (d::find-oldest-construct role-1 role-2)))
	  (add-role assoc-2 role-1 :revision rev-1)
	  (is (eql role-1 (d::find-oldest-construct role-1 role-2)))
	  (is (eql tm-1 (d::find-oldest-construct tm-1 tm-2)))
	  (d::add-to-version-history tm-1 :start-revision rev-3)
	  (is (eql tm-1 (d::find-oldest-construct tm-1 tm-2)))
	  (d::add-to-version-history tm-2 :start-revision rev-1)
	  (is (eql tm-2 (d::find-oldest-construct tm-1 tm-2)))
	  (d::add-to-version-history tm-1 :start-revision rev-1)
	  (is (eql tm-1 (d::find-oldest-construct tm-1 tm-2)))
	  (is (eql tm-2 (d::find-oldest-construct tm-2 tm-1))))))))


(test test-move-referenced-constructs-ReifiableConstructC ()
  "Tests the generic move-referenced-constructs corresponding to ReifiableConstructC."
  (with-fixture with-empty-db (*db-dir*)
    (let ((rev-1 100)
	  (rev-2 200)
	  (ii-1 (make-construct 'ItemIdentifierC :uri "ii-1"))
	  (ii-2 (make-construct 'ItemIdentifierC :uri "ii-2"))
	  (ii-3 (make-construct 'ItemIdentifierC :uri "ii-3")))
      (let ((reifier-1 (make-construct 'TopicC :start-revision rev-2))
	    (reifier-2 (make-construct 'TopicC :start-revision rev-1))
	    (theme-1 (make-construct 'TopicC :start-revision rev-1))
	    (theme-2 (make-construct 'TopicC :start-revision rev-1))
	    (type-1 (make-construct 'TopicC :start-revision rev-1))
	    (type-2 (make-construct 'TopicC :start-revision rev-1)))
	(let ((occ-1 (make-construct 'OccurrenceC
				     :start-revision rev-1
				     :item-identifiers (list ii-1 ii-2)
				     :reifier reifier-1
				     :instance-of type-2
				     :themes (list theme-1 theme-2)
				     :charvalue "occ"))
	      (occ-2 (make-construct 'OccurrenceC
				     :start-revision rev-2
				     :item-identifiers (list ii-3)
				     :charvalue "occ"
				     :instance-of type-1
				     :themes (list theme-1 theme-2)
				     :reifier reifier-2)))
	  (setf *TM-REVISION* rev-1)
	  (delete-type occ-1 type-2 :revision rev-2)
	  (add-type occ-1 type-1 :revision rev-2)
	  (is (eql reifier-1 (reifier occ-1 :revision rev-2)))
	  (is (eql reifier-2 (reifier occ-2 :revision rev-2)))
	  (is (= (length (union (list ii-1 ii-2 reifier-2)
				(d::move-referenced-constructs occ-1 occ-2
							       :revision rev-2)))
		 3))
	  (is (= (length (item-identifiers occ-2 :revision rev-2)) 3))
	  (is (= (length (union (item-identifiers occ-2 :revision rev-2)
				(list ii-1 ii-2 ii-3)))
		 3))
	  (is-false (item-identifiers occ-1 :revision rev-2))
	  (is-false (reifier occ-1 :revision rev-2))
	  (is (eql (reifier occ-2 :revision rev-2) reifier-2))
	  (is-true (d::marked-as-deleted-p reifier-1)))))))


(test test-move-referenced-constructs-NameC ()
  "Tests the generic move-referenced-constructs corresponding to NameC."
  (with-fixture with-empty-db (*db-dir*)
    (let ((rev-1 100)
	  (rev-2 200))
      (let ((ii-1 (make-construct 'ItemIdentifierC :uri "ii-1"))
	    (ii-2 (make-construct 'ItemIdentifierC :uri "ii-2"))
	    (reifier-1 (make-construct 'TopicC :start-revision rev-1))
	    (reifier-2 (make-construct 'TopicC :start-revision rev-2))
	    (type-1 (make-construct 'TopicC :start-revision rev-1))
	    (theme-1 (make-construct 'TopicC :start-revision rev-1))
	    (theme-2 (make-construct 'TopicC :start-revision rev-1)))
	(let ((variant-1 (make-construct 'VariantC
					 :start-revision rev-1
					 :themes (list theme-1)
					 :charvalue "var-1"
					 :item-identifiers (list ii-1)
					 :reifier reifier-2))
	      (variant-2 (make-construct 'VariantC
					 :start-revision rev-1
					 :themes (list theme-1)
					 :charvalue "var-2+4"))
	      (variant-3 (make-construct 'VariantC
					 :start-revision rev-1
					 :themes (list theme-2)
					 :charvalue "var-3"))
	      (variant-4 (make-construct 'VariantC
					 :start-revision rev-1
					 :themes (list theme-1)
					 :charvalue "var-2+4")))
	  (let ((name-1 (make-construct 'NameC
					:start-revision rev-1
					:charvalue "name"
					:variants (list variant-1 variant-2)
					:instance-of type-1
					:item-identifiers (list ii-2)))
		(name-2 (make-construct 'NameC
					:start-revision rev-1
					:charvalue "name"
					:variants (list variant-3 variant-4)
					:instance-of type-1
					:reifier reifier-1)))
	    (setf *TM-REVISION* rev-1)
	    (is (= (length (union (list variant-1 variant-2)
				  (variants name-1))) 2))
	    (is (= (length (union (list variant-3 variant-4)
				  (variants name-2))) 2))
	    (is-false (reifier name-1))
	    (is (eql reifier-1 (reifier name-2)))
	    (is (= (length
		    (union (list variant-1 variant-2 ii-2)
			   (d::move-referenced-constructs name-1 name-2
							  :revision rev-2)))
		   3))
	    (is-false (item-identifiers name-1 :revision rev-2))
	    (is-false (reifier name-1 :revision rev-2))
	    (is-false (variants name-1 :revision rev-2))
	    (is (= (length (item-identifiers name-2 :revision rev-2)) 1))
	    (is (= (length (union (list ii-2)
				  (item-identifiers name-2 :revision rev-2)))
		   1))
	    (is (eql (reifier name-2 :revision rev-2) reifier-1))
	    (is (= (length (variants name-2 :revision rev-2)) 3))
	    (is (= (length (union (list variant-1 variant-3 variant-4)
				  (variants name-2 :revision rev-2)))
		   3))
	    (is-true 
	     (find-if 
	      #'(lambda(var)
		  (and (= (length (item-identifiers var :revision rev-2)) 1)
		       (string= (uri (first (item-identifiers var
							      :revision rev-2)))
				"ii-1")))
	      (variants name-2 :revision rev-2)))
	    (is-true 
	     (find-if #'(lambda(var)
			  (eql (reifier var :revision rev-2) reifier-2))
		      (variants name-2 :revision rev-2)))))))))


(test test-merge-constructs-TopicC-1 ()
  "Tests the generic merge-constructs corresüponding to TopicC."
  (with-fixture with-empty-db (*db-dir*)
    (let ((rev-1 100)
	  (rev-2 200)
	  (rev-3 300))
      (let ((ii-1 (make-construct 'ItemIdentifierC :uri "ii-1"))
	    (ii-2 (make-construct 'ItemIdentifierC :uri "ii-2"))
	    (ii-3 (make-construct 'ItemIdentifierC :uri "ii-3"))
	    (sl-1 (make-construct 'SubjectLocatorC :uri "sl-1"))
	    (sl-2 (make-construct 'SubjectLocatorC :uri "sl-2"))
	    (psi-1 (make-construct 'PersistentIdC :uri "psi-1"))
	    (psi-2 (make-construct 'PersistentIdC :uri "psi-2"))
	    (tid-1 (make-construct 'TopicIdentificationC :uri "tid-1"
				   :xtm-id "xtm-1"))
	    (tid-2 (make-construct 'TopicIdentificationC :uri "tid-2"
				   :xtm-id "xtm-2"))
	    (type-1 (make-construct 'TopicC :start-revision rev-1))
	    (type-2 (make-construct 'TopicC :start-revision rev-1))
	    (theme-1 (make-construct 'TopicC :start-revision rev-1))
	    (theme-2 (make-construct 'TopicC :start-revision rev-1)))
	(let ((variant-1 (make-construct 'VariantC
					 :start-revision rev-1
					 :charvalue "var-1"
					 :themes (list theme-1)))
	      (variant-2 (make-construct 'VariantC
					 :start-revision rev-2
					 :charvalue "var-2"
					 :themes (list theme-2)))
	      (variant-3 (make-construct 'VariantC
					 :start-revision rev-1
					 :charvalue "var-1"
					 :themes (list theme-1)))
	      (occ-1 (make-construct 'OccurrenceC
				     :start-revision rev-1
				     :charvalue "occ-1"
				     :instance-of type-1
				     :themes (list theme-1)))
	      (occ-2 (make-construct 'OccurrenceC
				     :start-revision rev-1
				     :charvalue "occ-2"
				     :instance-of type-2))
	      (occ-3 (make-construct 'OccurrenceC
				     :start-revision rev-2
				     :item-identifiers (list ii-3)
				     :charvalue "occ-1"
				     :instance-of type-1
				     :themes (list theme-1))))
	  (let ((name-1 (make-construct 'NameC
					:start-revision rev-1
					:charvalue "name-1"
					:instance-of type-1))
		(name-2 (make-construct 'NameC
					:start-revision rev-2
					:charvalue "name-2"
					:instance-of type-1
					:variants (list variant-1 variant-2)))
		(name-3 (make-construct 'NameC
					:start-revision rev-1
					:charvalue "name-1"
					:instance-of type-1
					:variants (list variant-3))))
	    (let ((top-1 (make-construct 'TopicC
					 :start-revision rev-1
					 :topic-identifiers (list tid-1)
					 :item-identifiers (list ii-1)
					 :locators (list sl-1)
					 :psis (list psi-1)
					 :names (list name-1 name-2)
					 :occurrences (list occ-1 occ-2)))
		  (top-2 (make-construct 'TopicC
					 :start-revision rev-2
					 :topic-identifiers (list tid-2)
					 :item-identifiers (list ii-2)
					 :locators (list sl-2)
					 :psis (list psi-2)
					 :names (list name-3)
					 :occurrences (list occ-3))))
	      (setf *TM-REVISION* rev-3)
	      (let ((top (d::merge-constructs top-1 top-2 :revision rev-3)))
		(is (eql top top-1))
		(is-true (d::marked-as-deleted-p top-2))
		(is-false (append (psis top-2) (item-identifiers top-2)
				  (locators top-2) (topic-identifiers top-2)
				  (names top-2) (occurrences top-2)))
		(setf *TM-REVISION* rev-2)
		(is (= (length (append (psis top-2) (item-identifiers top-2)
				       (locators top-2) (topic-identifiers top-2)
				       (names top-2) (occurrences top-2)))
		       6))
		(setf *TM-REVISION* rev-3)
		(is-false (set-exclusive-or (list ii-1 ii-2)
					    (item-identifiers top-1)))
		(is-false (set-exclusive-or (list sl-1 sl-2) (locators top-1)))
		(is-false (set-exclusive-or (list psi-1 psi-2) (psis top-1)))
		(is-false (set-exclusive-or (list tid-1 tid-2)
					    (topic-identifiers top-1)))
		(is-false (set-exclusive-or (list psi-1)
					    (psis top-1 :revision rev-2)))
		(is-false (set-exclusive-or (list name-1 name-2)
					    (names top-1)))
		(is-false (set-exclusive-or (variants name-1)
					    (list variant-3)))
		(is-false (variants name-3))
		(is-false (set-exclusive-or (occurrences top-1)
					    (list occ-1 occ-2)))
		(is-false (set-exclusive-or (item-identifiers occ-1)
					    (list ii-3)))
		(is-false (item-identifiers occ-3))
		(is-true (d::marked-as-deleted-p name-3))
		(is-true (d::marked-as-deleted-p occ-3))))))))))


(test test-merge-constructs-TopicC-2 ()
  "Tests the generic merge-constructs corresüponding to TopicC."
  (with-fixture with-empty-db (*db-dir*)
    (let ((rev-1 100)
	  (rev-2 200)
	  (rev-3 300))
      (let ((ii-1 (make-construct 'ItemIdentifierC :uri "ii-1"))
	    (ii-2 (make-construct 'ItemIdentifierC :uri "ii-2"))
	    (ii-3 (make-construct 'ItemIdentifierC :uri "ii-3"))
	    (sl-1 (make-construct 'SubjectLocatorC :uri "sl-1"))
	    (sl-2 (make-construct 'SubjectLocatorC :uri "sl-2"))
	    (psi-1 (make-construct 'PersistentIdC :uri "psi-1"))
	    (psi-2 (make-construct 'PersistentIdC :uri "psi-2"))
	    (tid-1 (make-construct 'TopicIdentificationC :uri "tid-1"
				   :xtm-id "xtm-1"))
	    (tid-2 (make-construct 'TopicIdentificationC :uri "tid-2"
				   :xtm-id "xtm-2"))
	    (type-1 (make-construct 'TopicC :start-revision rev-1))
	    (type-2 (make-construct 'TopicC :start-revision rev-1))
	    (theme-1 (make-construct 'TopicC :start-revision rev-1))
	    (theme-2 (make-construct 'TopicC :start-revision rev-1)))
	(let ((variant-1 (make-construct 'VariantC
					 :start-revision rev-1
					 :charvalue "var-1"
					 :themes (list theme-1)))
	      (variant-2 (make-construct 'VariantC
					 :start-revision rev-2
					 :charvalue "var-2"
					 :themes (list theme-2)))
	      (variant-3 (make-construct 'VariantC
					 :start-revision rev-1
					 :charvalue "var-1"
					 :themes (list theme-1)))
	      (occ-1 (make-construct 'OccurrenceC
				     :start-revision rev-1
				     :charvalue "occ-1"
				     :instance-of type-1
				     :themes (list theme-1)))
	      (occ-2 (make-construct 'OccurrenceC
				     :start-revision rev-1
				     :charvalue "occ-2"
				     :instance-of type-2))
	      (occ-3 (make-construct 'OccurrenceC
				     :start-revision rev-2
				     :item-identifiers (list ii-3)
				     :charvalue "occ-1"
				     :instance-of type-1
				     :themes (list theme-1))))
	  (let ((name-1 (make-construct 'NameC
					:start-revision rev-1
					:charvalue "name-1"
					:instance-of type-1))
		(name-2 (make-construct 'NameC
					:start-revision rev-2
					:charvalue "name-2"
					:instance-of type-1
					:variants (list variant-1 variant-2)))
		(name-3 (make-construct 'NameC
					:start-revision rev-1
					:charvalue "name-1"
					:instance-of type-1
					:variants (list variant-3))))
	    (let ((top-1 (make-construct 'TopicC
					 :start-revision rev-1
					 :topic-identifiers (list tid-1)
					 :item-identifiers (list ii-1)
					 :locators (list sl-1)
					 :psis (list psi-1)
					 :names (list name-1 name-2)
					 :occurrences (list occ-1 occ-2)))
		  (top-2 (make-construct 'TopicC
					 :start-revision rev-3
					 :topic-identifiers (list tid-2)
					 :item-identifiers (list ii-2)
					 :locators (list sl-2)
					 :psis (list psi-2)
					 :names (list name-3)
					 :occurrences (list occ-3))))
	      (setf *TM-REVISION* rev-3)
	      (is (= (length (elephant:get-instances-by-class 'TopicC)) 6))
	      (is (= (length (elephant:get-instances-by-class 'NameC)) 3))
	      (is (= (length (elephant:get-instances-by-class 'OccurrenceC)) 3))
	      (is (= (length (elephant:get-instances-by-class 'VariantC)) 3))
	      (let ((top (d::merge-constructs top-1 top-2 :revision rev-3)))
		(is (= (length (elephant:get-instances-by-class 'TopicC)) 5))
		(is (= (length (elephant:get-instances-by-class 'NameC)) 2))
		(is (= (length (elephant:get-instances-by-class 'OccurrenceC)) 2))
		(is (= (length (elephant:get-instances-by-class 'VariantC)) 3))
		(is (eql top top-1))
		(is-false (append (psis top-2) (item-identifiers top-2)
				  (locators top-2) (topic-identifiers top-2)
				  (names top-2) (occurrences top-2)))
		(is-false (set-exclusive-or (list ii-1 ii-2)
					    (item-identifiers top-1)))
		(is-false (set-exclusive-or (list sl-1 sl-2) (locators top-1)))
		(is-false (set-exclusive-or (list psi-1 psi-2) (psis top-1)))
		(is-false (set-exclusive-or (list tid-1 tid-2)
					    (topic-identifiers top-1)))
		(is-false (set-exclusive-or (list psi-1)
					    (psis top-1 :revision rev-2)))
		(is-false (set-exclusive-or (list name-1 name-2)
					    (names top-1)))
		(is-false (set-exclusive-or (variants name-1)
					    (list variant-3)))
		(is-false (variants name-3))
		(is-false (set-exclusive-or (occurrences top-1)
					    (list occ-1 occ-2)))
		(is-false (set-exclusive-or (item-identifiers occ-1)
					    (list ii-3)))
		(is-false (item-identifiers occ-3))
		(is-true (d::marked-as-deleted-p name-3))
		(is-true (d::marked-as-deleted-p occ-3))))))))))


(test test-merge-constructs-TopicC-3 ()
  "Tests the generic merge-constructs corresüponding to TopicC."
  (with-fixture with-empty-db (*db-dir*)
    (let ((rev-1 100)
	  (rev-3 300))
      (let ((type-1 (make-construct 'TopicC :start-revision rev-1))
	    (type-2 (make-construct 'TopicC :start-revision rev-1))
	    (n-type (make-construct 'TopicC :start-revision rev-1))
	    (ii-1 (make-construct 'ItemIdentifierC :uri "ii-1"))
	    (ii-2 (make-construct 'ItemIdentifierC :uri "ii-2"))
	    (ii-3 (make-construct 'ItemIdentifierC :uri "ii-3"))
	    (ii-4 (make-construct 'ItemIdentifierC :uri "ii-4"))
	    (ii-5 (make-construct 'ItemIdentifierC :uri "ii-5"))
	    (ii-6 (make-construct 'ItemIdentifierC :uri "ii-6"))
	    (var-0-1
	     (make-construct 'VariantC
			     :start-revision rev-1
			     :themes (list
				      (make-construct 'TopicC
						      :start-revision rev-1))
			     :charvalue "var-0-1"))
	    (var-0-2
	     (make-construct 'VariantC
			     :start-revision rev-1
			     :themes (list
				      (make-construct 'TopicC
						      :start-revision rev-1))
			     :charvalue "var-0-1")))
	(let ((occ-1 (make-construct 'OccurrenceC
				     :start-revision rev-1
				     :item-identifiers (list ii-1)
				     :charvalue "occ"
				     :instance-of type-1))
	      (occ-2 (make-construct 'OccurrenceC
				     :start-revision rev-1
				     :item-identifiers (list ii-2)
				     :charvalue "occ"
				     :instance-of type-2))
	      (name-1  (make-construct 'NameC
				       :start-revision rev-1
				       :item-identifiers (list ii-3)
				       :variants (list var-0-1)
				       :charvalue "name"
				       :instance-of type-1))
	      (name-2 (make-construct 'NameC
				      :start-revision rev-1
				      :item-identifiers (list ii-4)
				      :variants (list var-0-2)
				      :charvalue "name"
				      :instance-of type-2))
	      (var-1 (make-construct 'VariantC
					 :start-revision rev-1
					 :item-identifiers (list ii-5)
					 :charvalue "var"
					 :themes (list type-1)))
	      (var-2 (make-construct 'VariantC
					 :start-revision rev-1
					 :item-identifiers (list ii-6)
					 :charvalue "var"
					 :themes (list type-2))))
	  (let ((top-1 (make-construct 'TopicC
				       :start-revision rev-1
				       :occurrences (list occ-1 occ-2)
				       :names (list name-1 name-2)))
		(name-3 (make-construct 'NameC
					:start-revision rev-1
					:charvalue "name-3"
					:instance-of n-type
					:variants (list var-1 var-2))))
	    (let ((top-2 (make-construct 'TopicC
					 :start-revision rev-1
					 :names (list name-3))))
	      (setf *TM-REVISION* rev-3)
	      (is (eql (d::merge-constructs type-1 type-2 :revision rev-3) type-1))
	      (is (= (length (occurrences top-1)) 1))
	      (is-false (set-exclusive-or
			 (list ii-1 ii-2)
			 (item-identifiers (first (occurrences top-1)))))
	      (is (= (length (slot-value top-1 'd::occurrences)) 2))
	      (is (= (length (names top-1)) 1))
	      (is-false (set-exclusive-or
			 (list ii-3 ii-4)
			 (item-identifiers (first (names top-1)))))
	      (is (= (length (slot-value top-1 'd::names)) 2))
	      (is-false (set-exclusive-or (list var-0-1 var-0-2)
					  (variants (first (names top-1)))))
	      (is-true (d::marked-as-deleted-p
			(find-if-not #'(lambda(occ)
					 (eql occ (first (occurrences top-1))))
				     (slot-value top-1 'd::occurrences))))
	      (is-true (d::marked-as-deleted-p
			(find-if-not #'(lambda(name)
					 (eql name (first (names top-1))))
				     (slot-value top-1 'd::names))))
	      (is (= (length (variants (first (names top-2)))) 1))
	      (is (= (length (slot-value (first (names top-2)) 'd::variants)) 2))
	      (is (eql (first (themes (first (variants (first (names top-2))))))
		       type-1)))))))))


(test test-merge-constructs-TopicC-4 ()
  "Tests the generic merge-constructs corresüponding to TopicC."
  (with-fixture with-empty-db (*db-dir*)
    (let ((rev-1 100)
	  (rev-3 300))
      (let ((type-1 (make-construct 'TopicC :start-revision rev-1))
	    (type-2 (make-construct 'TopicC :start-revision rev-1))
	    (a-type (make-construct 'TopicC :start-revision rev-1))
	    (r-type (make-construct 'TopicC :start-revision rev-1))
	    (ii-1 (make-construct 'ItemIdentifierC :uri "ii-1"))
	    (ii-2 (make-construct 'ItemIdentifierC :uri "ii-2")))
	(let ((assoc-1 (make-construct 'AssociationC
				       :start-revision rev-1
				       :instance-of a-type
				       :roles (list (list :player type-1
							  :instance-of r-type
							  :item-identifiers (list ii-1)
							  :start-revision rev-1)
						    (list :player type-2
							  :item-identifiers (list ii-2)
							  :instance-of r-type
							  :start-revision rev-1)))))
	  (setf *TM-REVISION* rev-3)
	  (is (eql (d::merge-constructs type-1 type-2 :revision rev-3) type-1))
	  (is (= (length (roles assoc-1)) 1))
	  (is (= (length (slot-value assoc-1 'd::roles)) 2))
	  (is (eql (instance-of (first (roles assoc-1))) r-type))
	  (is (eql (player (first (roles assoc-1))) type-1))
	  (is-false (set-exclusive-or (list ii-1 ii-2)
				      (item-identifiers (first (roles assoc-1)))))
	  (let ((active-role (first (roles assoc-1)))
		(non-active-role 
		 (let ((r-assoc (find-if-not #'(lambda(role)
						 (eql role (first (roles assoc-1))))
					     (slot-value assoc-1 'd::roles))))
		   (when r-assoc
		     (d::role r-assoc)))))
	    (is (= (length (d::versions
			    (first (slot-value active-role 'd::parent)))) 2))
	    (is (= (length (d::versions
			    (first (slot-value non-active-role 'd::parent)))) 1))
	    (is-true (find-if #'(lambda(vi)
				  (and (= rev-1 (d::start-revision vi))
				       (= rev-3 (d::end-revision vi))))
			      (d::versions (first (slot-value non-active-role 
							      'd::parent)))))
	    (is-true (find-if #'(lambda(vi)
				  (and (= rev-1 (d::start-revision vi))
				       (= rev-3 (d::end-revision vi))))
			      (d::versions (first (slot-value active-role 
							      'd::parent)))))
	    (is-true (find-if #'(lambda(vi)
				  (and (= rev-3 (d::start-revision vi))
				       (= 0 (d::end-revision vi))))
			      (d::versions (first (slot-value active-role 
							      'd::parent)))))))))))


(test test-merge-constructs-TopicC-5 ()
  "Tests the generic merge-constructs corresüponding to TopicC."
  (with-fixture with-empty-db (*db-dir*)
    (let ((rev-1 100)
	  (rev-3 300))
      (let ((type-1 (make-construct 'TopicC :start-revision rev-1))
	    (type-2 (make-construct 'TopicC :start-revision rev-1))
	    (a-type (make-construct 'TopicC :start-revision rev-1))
	    (player-1 (make-construct 'TopicC :start-revision rev-1))
	    (ii-1 (make-construct 'ItemIdentifierC :uri "ii-1"))
	    (ii-2 (make-construct 'ItemIdentifierC :uri "ii-2")))
	(let ((assoc-2 (make-construct 'AssociationC
				       :start-revision rev-1
				       :instance-of a-type
				       :roles (list (list :player player-1
							  :instance-of type-1
							  :item-identifiers (list ii-1)
							  :start-revision rev-1)
						    (list :player player-1
							  :item-identifiers (list ii-2)
							  :instance-of type-2
							  :start-revision rev-1)))))
	  (setf *TM-REVISION* rev-3)
	  (is (eql (d::merge-constructs type-1 type-2 :revision rev-3) type-1))
	  (is (= (length (roles assoc-2)) 1))
	  (is (= (length (slot-value assoc-2 'd::roles)) 2))
	  (is (eql (instance-of (first (roles assoc-2))) type-1))
	  (is (eql (player (first (roles assoc-2))) player-1))
	  (is-false (set-exclusive-or (list ii-1 ii-2)
				      (item-identifiers (first (roles assoc-2)))))
	  (let ((active-role (first (roles assoc-2)))
		(non-active-role 
		 (let ((r-assoc (find-if-not #'(lambda(role)
						 (eql role (first (roles assoc-2))))
					     (slot-value assoc-2 'd::roles))))
		   (when r-assoc
		     (d::role r-assoc)))))
	    (is (= (length (d::versions
			    (first (slot-value active-role 'd::parent)))) 2))
	    (is (= (length (d::versions
			    (first (slot-value non-active-role 'd::parent)))) 1))
	    (is-true (find-if #'(lambda(vi)
				  (and (= rev-1 (d::start-revision vi))
				       (= rev-3 (d::end-revision vi))))
			      (d::versions (first (slot-value non-active-role 
							      'd::parent)))))
	    (is-true (find-if #'(lambda(vi)
				  (and (= rev-1 (d::start-revision vi))
				       (= rev-3 (d::end-revision vi))))
			      (d::versions (first (slot-value active-role 
							      'd::parent)))))
	    (is-true (find-if #'(lambda(vi)
				  (and (= rev-3 (d::start-revision vi))
				       (= 0 (d::end-revision vi))))
			      (d::versions (first (slot-value active-role 
							      'd::parent)))))))))))


(test test-merge-constructs-TopicC-6 ()
  "Tests the generic merge-constructs corresüponding to TopicC."
  (with-fixture with-empty-db (*db-dir*)
    (let ((rev-1 100)
	  (rev-2 200)
	  (rev-3 300))
      (let ((type-1 (make-construct 'TopicC :start-revision rev-1))
	    (type-2 (make-construct 'TopicC :start-revision rev-1))
	    (r-type-1 (make-construct 'TopicC :start-revision rev-1))
	    (r-type-2 (make-construct 'TopicC :start-revision rev-1))
	    (player-1 (make-construct 'TopicC :start-revision rev-1))
	    (player-2 (make-construct 'TopicC :start-revision rev-1))
	    (ii-1 (make-construct 'ItemIdentifierC :uri "ii-1"))
	    (ii-2 (make-construct 'ItemIdentifierC :uri "ii-2"))
	    (ii-3 (make-construct 'ItemIdentifierC :uri "ii-3"))
	    (ii-4 (make-construct 'ItemIdentifierC :uri "ii-4")))
	(let ((assoc-3 (make-construct 'AssociationC
				       :start-revision rev-1
				       :instance-of type-1
				       :item-identifiers (list ii-3)
				       :roles (list (list :player player-1
							  :instance-of r-type-1
							  :item-identifiers (list ii-1)
							  :start-revision rev-1)
						    (list :player player-2
							  :instance-of r-type-2
							  :start-revision rev-1))))
	      (assoc-4 (make-construct 'AssociationC
				       :start-revision rev-2
				       :instance-of type-2
				       :item-identifiers (list ii-4)
				       :roles (list (list :player player-1
							  :instance-of r-type-1
							  :start-revision rev-2)
						    (list :player player-2
							  :item-identifiers (list ii-2)
							  :instance-of r-type-2
							  :start-revision rev-2)))))
	  (setf *TM-REVISION* rev-3)
	  (is (eql (d::merge-constructs type-1 type-2 :revision rev-3) type-1))
	  (is (= (length (d::versions assoc-3)) 2))
	  (is (= (length (d::versions assoc-4)) 1))
	  (is-true (find-if #'(lambda(vi)
				(and (= (d::start-revision vi) rev-1)
				     (= (d::end-revision vi) rev-3)))
			    (d::versions assoc-3)))
	  (is-true (find-if #'(lambda(vi)
				(and (= (d::start-revision vi) rev-3)
				     (= (d::end-revision vi) 0)))
			    (d::versions assoc-3)))
	  (is-true (find-if #'(lambda(vi)
				(and (= (d::start-revision vi) rev-2)
				     (= (d::end-revision vi) rev-3)))
			    (d::versions assoc-4)))
	  (is (= (length (roles assoc-3)) 2))
	  (is (= (length (item-identifiers (first (roles assoc-3)))) 1))
	  (is (= (length (item-identifiers (second (roles assoc-3)))) 1))
	  (is (or (and (string= (uri (first (item-identifiers
					     (first (roles assoc-3)))))
				"ii-1")
		       (string= (uri (first (item-identifiers
					     (second (roles assoc-3)))))
				"ii-2"))
		  (and (string= (uri (first (item-identifiers
					     (first (roles assoc-3)))))
				"ii-2")
		       (string= (uri (first (item-identifiers
					     (second (roles assoc-3)))))
				"ii-1")))))))))


(test test-merge-constructs-TopicC-7 ()
  "Tests the generic merge-constructs corresüponding to TopicC."
  (with-fixture with-empty-db (*db-dir*)
    (let ((rev-1 100)
	  (rev-2 200)
	  (rev-3 300)
	  (psi-1 (make-construct 'PersistentIdC :uri "psi-1"))
	  (sl-1 (make-construct 'SubjectLocatorC :uri "sl-1"))
	  (tid-1 (make-construct 'TopicIdentificationC
				 :uri "tid-1" :xtm-id "xtm-1"))
	  (tid-2 (make-construct 'TopicIdentificationC
				 :uri "tid-2" :xtm-id "xtm-2"))
	  (ii-1 (make-construct 'ItemIdentifierC :uri "ii-1"))
	  (ii-2 (make-construct 'ItemIdentifierC :uri "ii-2"))
	  (ii-3 (make-construct 'ItemIdentifierC :uri "ii-3")))
      (let ((type-1 (make-construct 'TopicC :start-revision rev-1))
	    (scope-1 (make-construct 'TopicC :start-revision rev-1))
	    (scope-2 (make-construct 'TopicC :start-revision rev-1))
	    (top-1 (make-construct 'TopicC
				   :start-revision rev-1
				   :psis (list psi-1)
				   :topic-identifiers (list tid-1)))
	    (top-2 (make-construct 'TopicC
				   :start-revision rev-2
				   :locators (list sl-1)
				   :topic-identifiers (list tid-2))))
	(let ((occ-1 (make-construct 'OccurrenceC
				     :start-revision rev-1
				     :item-identifiers (list ii-1)
				     :instance-of type-1
				     :themes (list scope-1 scope-2)
				     :charvalue "occ"
				     :parent top-1))
	      (occ-2 (make-construct 'OccurrenceC
				     :start-revision rev-2
				     :item-identifiers (list ii-2)
				     :instance-of type-1
				     :themes (list scope-1 scope-2)
				     :charvalue "occ"
				     :parent top-2))
	      (occ-3 (make-construct 'OccurrenceC
				     :start-revision rev-1
				     :item-identifiers (list ii-3)
				     :instance-of type-1
				     :themes (list scope-1)
				     :charvalue "occ"
				     :parent top-1)))
	  (setf *TM-REVISION* rev-3)
	  (is (= (length (get-all-topics rev-1)) 4))
	  (is (= (length (get-all-topics rev-3)) 5))
	  (is (= (length (d::get-db-instances-by-class
			  'd::OccurrenceC :revision nil)) 3))
	  (signals not-mergable-error (add-item-identifier occ-3 ii-1))
	  (is (eql occ-1 (add-item-identifier occ-1 ii-2)))
	  (is (= (length (get-all-topics rev-3)) 4))
	  (is-true (d::marked-as-deleted-p occ-2))
	  (is-true (d::marked-as-deleted-p top-2))
	  (is-false (set-exclusive-or (list ii-1 ii-2)
				      (item-identifiers occ-1)))
	  (is-false (item-identifiers occ-2))
	  (is-false (set-exclusive-or (list ii-2)
				      (item-identifiers occ-2 :revision rev-2)))
	  (is-false (set-exclusive-or (list psi-1) (psis top-1)))
	  (is-false (set-exclusive-or (list sl-1) (locators top-1)))
	  (is-false (set-exclusive-or (list tid-1 tid-2)
				      (topic-identifiers top-1)))
	  (is-false (locators top-2)))))))


(test test-merge-constructs-TopicC-8 ()
  "Tests the generic merge-constructs corresüponding to TopicC."
  (with-fixture with-empty-db (*db-dir*)
    (let ((rev-1 100)
	  (rev-2 200)
	  (rev-3 300))
      (let ((top-1 (make-construct 'TopicC :start-revision rev-1))
	    (top-2 (make-construct 'TopicC :start-revision rev-2))
	    (reifier-1 (make-construct 'TopicC :start-revision rev-1))
	    (type-1 (make-construct 'TopicC :start-revision rev-1))
	    (type-2 (make-construct 'TopicC :start-revision rev-1)))
	(let ((occ-1 (make-construct 'OccurrenceC
				     :start-revision rev-1
				     :instance-of type-1
				     :charvalue "occ"
				     :reifier reifier-1
				     :parent top-1))
	      (occ-2 (make-construct 'OccurrenceC
				     :start-revision rev-2
				     :instance-of type-1
				     :charvalue "occ"
				     :parent top-2))
	      (occ-3 (make-construct 'OccurrenceC
				     :start-revision rev-1
				     :instance-of type-2
				     :charvalue "occ"
				     :parent top-1)))
	  (setf *TM-REVISION* rev-3)
	  (signals not-mergable-error (add-reifier occ-3 reifier-1))
	  (is (eql (add-reifier occ-2 reifier-1) occ-1))
	  (is-false (set-exclusive-or (list occ-1 occ-3) (occurrences top-1)))
	  (is-true (marked-as-deleted-p top-2))
	  (is-true (marked-as-deleted-p occ-2))
	  (is (= (length (d::versions top-1)) 2))
	  (is (= (length (d::versions top-2)) 1))
	  (is-true (find-if #'(lambda(vi)
				(and (= (d::end-revision vi) rev-3)
				     (= (d::start-revision vi) rev-1)))
			    (d::versions top-1)))
	  (is-true (find-if #'(lambda(vi)
				(and (= (d::end-revision vi) 0)
				     (= (d::start-revision vi) rev-3)))
			    (d::versions top-1)))
	  (is-true (find-if #'(lambda(vi)
				(and (= (d::end-revision vi) rev-3)
				     (= (d::start-revision vi) rev-2)))
			    (d::versions top-2)))
	  (is (= (length (slot-value occ-2 'd::parent)) 1))
	  (is (= (length (slot-value occ-1 'd::parent)) 1))
	  (is-true (find-if #'(lambda(vi)
				(and (= (d::end-revision vi) rev-3)
				     (= (d::start-revision vi) rev-2)))
			    (first (map 'list #'d::versions
					(slot-value occ-2 'd::parent)))))
	  (is-true (find-if #'(lambda(vi)
				(and (= (d::end-revision vi) rev-3)
				     (= (d::start-revision vi) rev-1)))
			    (first (map 'list #'d::versions
					(slot-value occ-1 'd::parent)))))
	  (is-true (find-if #'(lambda(vi)
				(and (= (d::end-revision vi) 0)
				     (= (d::start-revision vi) rev-3)))
			    (first (map 'list #'d::versions
					(slot-value occ-1 'd::parent))))))))))


(test test-merge-constructs-TopicC-9 ()
  "Tests the generic merge-constructs corresüponding to TopicC."
  (with-fixture with-empty-db (*db-dir*)
    (let ((rev-1 100)
	  (rev-2 200)
	  (rev-3 300)
	  (rev-4 400)
	  (psi-1 (make-construct 'PersistentIdC :uri "psi-1"))
	  (psi-2 (make-construct 'PersistentIdC :uri "psi-2")))
      (let ((top-1 (make-construct 'TopicC :start-revision rev-2
				   :psis (list psi-2)))
	    (top-2 (make-construct 'TopicC :start-revision rev-2))
	    (top-3 (make-construct 'TopicC :start-revision rev-1))
	    (reifier-1 (make-construct 'TopicC :start-revision rev-1))
	    (reifier-2 (make-construct 'TopicC :start-revision rev-2
				       :psis (list psi-1)))
	    (reifier-3 (make-construct 'TopicC :start-revision rev-1))
	    (reifier-4 (make-construct 'TopicC :start-revision rev-1))
	    (type-1 (make-construct 'TopicC :start-revision rev-1))
	    (type-2 (make-construct 'TopicC :start-revision rev-1)))
	(let ((occ-1 (make-construct 'OccurrenceC
				     :start-revision rev-2
				     :instance-of type-1
				     :charvalue "occ"
				     :reifier reifier-1
				     :parent top-1))
	      (occ-2 (make-construct 'OccurrenceC
				     :start-revision rev-2
				     :instance-of type-2
				     :charvalue "occ"
				     :reifier reifier-3
				     :parent top-2))
	      (occ-3 (make-construct 'OccurrenceC
				     :start-revision rev-1
				     :instance-of type-1
				     :charvalue "occ"
				     :reifier reifier-4
				     :parent top-3)))
	  (setf *TM-REVISION* rev-3)
	  (is (eql (reifier occ-2) reifier-3))
	  (signals not-mergable-error (add-reifier occ-1 reifier-3))
	  (is (eql occ-1 (add-reifier occ-1 reifier-2)))
	  (is-true (marked-as-deleted-p reifier-2))
	  (is-false (set-exclusive-or (list psi-1) (psis reifier-1)))
	  (setf *TM-REVISION* rev-4)
	  (is (eql (add-reifier occ-1 reifier-4) occ-3))
	  (is-true (marked-as-deleted-p top-1))
	  (is-false (marked-as-deleted-p top-3))
	  (is-false (set-exclusive-or (list psi-2) (psis top-3)))
	  (is-false (marked-as-deleted-p top-2))
	  (is-false (set-exclusive-or (list occ-2) (occurrences top-2))))))))



(test test-merge-constructs-TopicC-10 ()
  "Tests the generic merge-constructs corresüponding to TopicC."
  (with-fixture with-empty-db (*db-dir*)
    (let ((rev-1 100)
	  (rev-2 200)
	  (rev-3 300)
	  (psi-1 (make-construct 'PersistentIdC :uri "psi-1"))
	  (psi-2 (make-construct 'PersistentIdC :uri "psi-2"))
	  (ii-1 (make-construct 'ItemIdentifierC :uri "ii-1"))
	  (ii-2 (make-construct 'ItemIdentifierC :uri "ii-2"))
	  (ii-3 (make-construct 'ItemIdentifierC :uri "ii-3"))
	  (ii-4 (make-construct 'ItemIdentifierC :uri "ii-4")))
      (let ((top-1 (make-construct 'TopicC
				   :start-revision rev-1
				   :psis (list psi-1)))
	    (top-2 (make-construct 'TopicC
				   :start-revision rev-2
				   :psis (list psi-2)))
	    (type-1 (make-construct 'TopicC :start-revision rev-1))
	    (scope-1 (make-construct 'TopicC :start-revision rev-1)))
	(let ((name-1 (make-construct 'NameC
				      :start-revision rev-1
				      :instance-of nil
				      :charvalue "name"
				      :themes (list scope-1)
				      :item-identifiers (list ii-1)
				      :parent top-1))
	      (name-2 (make-construct 'NameC
				      :start-revision rev-1
				      :instance-of type-1
				      :charvalue "name"
				      :themes (list scope-1)
				      :parent top-1))
	      (name-3 (make-construct 'NameC
				      :start-revision rev-2
				      :instance-of nil
				      :charvalue "name"
				      :themes (list scope-1)
				      :item-identifiers (list ii-2)
				      :parent top-2))
	      (name-4  (make-construct 'NameC
				      :start-revision rev-2
				      :instance-of type-1
				      :charvalue "name"
				      :themes nil
				      :parent top-2)))
	  (let ((variant-1 (make-construct 'VariantC
					   :start-revision rev-1
					   :charvalue "variant"
					   :themes (list scope-1)
					   :item-identifiers (list ii-3 ii-4)
					   :parent name-1))
		(variant-2 (make-construct 'VariantC
					  :start-revision rev-1
					  :charvalue "variant"
					  :themes (list scope-1)
					  :parent name-4))
		(variant-3 (make-construct 'VariantC
					   :start-revision rev-2
					   :charvalue "variant"
					   :themes (list scope-1)
					   :parent name-3)))
	    (setf *TM-REVISION* rev-3)
	    (signals not-mergable-error (add-item-identifier variant-2 ii-4))
	    (is-false (marked-as-deleted-p top-2))
	    (is-false (marked-as-deleted-p top-1))
	    (is-false (marked-as-deleted-p name-4))
	    (is (eql (add-item-identifier variant-3 ii-4) variant-1))
	    (is-true (marked-as-deleted-p top-2))
	    (is-false (names top-2))
	    (is-false (psis top-2))
	    (is-false (set-exclusive-or (list name-1 name-2 name-4) (names top-1)))
	    (is-false (set-exclusive-or (list psi-1 psi-2) (psis top-1)))
	    (is-false (set-exclusive-or (list variant-1) (variants name-1)))
	    (is-false (set-exclusive-or (list variant-2) (variants name-4)))
	    (is (= (length (d::versions top-1)) 2))))))))


(test test-merge-constructs-AssociationC ()
  "Tests merge-constructs corresponding to AssociationC."
  (with-fixture with-empty-db (*db-dir*)
    (let ((rev-1 100)
	  (rev-2 200)
	  (rev-3 300))
      (let ((type-1 (make-construct 'TopicC :start-revision rev-1))
	    (r-type-1 (make-construct 'TopicC :start-revision rev-1))
	    (r-type-2 (make-construct 'TopicC :start-revision rev-1))
	    (player-1 (make-construct 'TopicC :start-revision rev-1))
	    (player-2 (make-construct 'TopicC :start-revision rev-1))
	    (ii-1 (make-construct 'ItemIdentifierC :uri "ii-1"))
	    (ii-2 (make-construct 'ItemIdentifierC :uri "ii-2")))
	(let ((role-1 (list :start-revision rev-1
			    :player player-1
			    :instance-of r-type-1))
	      (role-2-1 (list :start-revision rev-1
			      :player player-1
			      :instance-of r-type-2))
	      (role-2-2 (list :start-revision rev-2
			      :player player-1
			      :item-identifiers (list ii-2)
			      :instance-of r-type-2))
	      (role-3 (list :start-revision rev-2
			    :player player-2
			    :instance-of r-type-1
			    :item-identifiers (list ii-1)
			    :instance-of r-type-2)))
	  (let ((assoc-1 (make-construct 'AssociationC
					 :start-revision rev-1
					 :instance-of type-1
					 :roles (list role-1 role-2-1)))
		(assoc-2 (make-construct 'AssociationC
					 :start-revision rev-2
					 :instance-of type-1
					 :roles (list role-2-2 role-3))))
	    (setf *TM-REVISION* rev-3)
	    (is (= (length (get-all-associations nil)) 2))
	    (make-construct 'AssociationC
			    :start-revision rev-2
			    :instance-of type-1
			    :roles (list role-1 role-2-1))
	    (is (= (length (get-all-associations nil)) 2))
	    (let ((role-2-1-inst
		   (find-if #'(lambda(role)
				(and (eql (instance-of role) r-type-2)
				     (eql (player role) player-1)))
			    (roles assoc-1))))
	      (is-true role-2-1-inst)
	      (is (eql (add-item-identifier role-2-1-inst ii-2) role-2-1-inst))
	      (is-true (marked-as-deleted-p assoc-2))
	      (is-false (roles assoc-2))
	      (is-false (instance-of assoc-2))
	      (is-false (themes assoc-2))
	      (is (eql (instance-of assoc-2 :revision rev-2) type-1))
	      (is (= (length (roles assoc-1)) 3))
	      (is-true (find-if #'(lambda(role)
				    (and (eql (instance-of role) r-type-1)
					 (eql (player role) player-1)))
				(roles assoc-1)))
	      (is-true (find-if #'(lambda(role)
				    (and (eql (instance-of role) r-type-1)
					 (eql (player role) player-2)
					 (not (set-exclusive-or
					       (list ii-1)
					       (item-identifiers role)))))
				(roles assoc-1)))
	      (is-true (find-if #'(lambda(role)
				    (and (eql (instance-of role) r-type-2)
					 (eql (player role) player-1)
					 (not (set-exclusive-or
					       (list ii-2)
					       (item-identifiers role)))))
				(roles assoc-1))))))))))


(defun run-datamodel-tests()
  "Runs all tests of this test-suite."
  (it.bese.fiveam:run! 'test-VersionInfoC)
  (it.bese.fiveam:run! 'test-VersionedConstructC)
  (it.bese.fiveam:run! 'test-ItemIdentifierC)
  (it.bese.fiveam:run! 'test-PersistentIdC)
  (it.bese.fiveam:run! 'test-SubjectLocatorC)
  (it.bese.fiveam:run! 'test-TopicIdentificationC)
  (it.bese.fiveam:run! 'test-get-item-by-id)
  (it.bese.fiveam:run! 'test-get-item-by-item-identifier)
  (it.bese.fiveam:run! 'test-get-item-by-locator)
  (it.bese.fiveam:run! 'test-get-item-by-psi)
  (it.bese.fiveam:run! 'test-ReifiableConstructC)
  (it.bese.fiveam:run! 'test-OccurrenceC)
  (it.bese.fiveam:run! 'test-VariantC)
  (it.bese.fiveam:run! 'test-NameC)
  (it.bese.fiveam:run! 'test-TypableC)
  (it.bese.fiveam:run! 'test-ScopableC)
  (it.bese.fiveam:run! 'test-RoleC)
  (it.bese.fiveam:run! 'test-player)
  (it.bese.fiveam:run! 'test-TopicMapC)
  (it.bese.fiveam:run! 'test-delete-ItemIdentifierC)
  (it.bese.fiveam:run! 'test-delete-PersistentIdC)
  (it.bese.fiveam:run! 'test-delete-SubjectLocatorC)
  (it.bese.fiveam:run! 'test-delete-ReifiableConstructC)
  (it.bese.fiveam:run! 'test-delete-VariantC)
  (it.bese.fiveam:run! 'test-delete-NameC)
  (it.bese.fiveam:run! 'test-delete-OccurrenceC)
  (it.bese.fiveam:run! 'test-delete-TypableC)
  (it.bese.fiveam:run! 'test-delete-ScopableC)
  (it.bese.fiveam:run! 'test-delete-AssociationC)
  (it.bese.fiveam:run! 'test-delete-RoleC)
  (it.bese.fiveam:run! 'test-equivalent-PointerC)
  (it.bese.fiveam:run! 'test-equivalent-OccurrenceC)
  (it.bese.fiveam:run! 'test-equivalent-NameC)
  (it.bese.fiveam:run! 'test-equivalent-VariantC)
  (it.bese.fiveam:run! 'test-equivalent-RoleC)
  (it.bese.fiveam:run! 'test-equivalent-AssociationC)
  (it.bese.fiveam:run! 'test-equivalent-TopicC)
  (it.bese.fiveam:run! 'test-equivalent-TopicMapC)
  (it.bese.fiveam:run! 'test-class-p)
  (it.bese.fiveam:run! 'test-find-item-by-revision)
  (it.bese.fiveam:run! 'test-make-Unknown)
  (it.bese.fiveam:run! 'test-make-VersionedConstructC)
  (it.bese.fiveam:run! 'test-make-TopicIdentificationC)
  (it.bese.fiveam:run! 'test-make-PersistentIdC)
  (it.bese.fiveam:run! 'test-make-SubjectLocatorC)
  (it.bese.fiveam:run! 'test-make-ItemIdentifierC)
  (it.bese.fiveam:run! 'test-make-OccurrenceC)
  (it.bese.fiveam:run! 'test-make-NameC)
  (it.bese.fiveam:run! 'test-make-VariantC)
  (it.bese.fiveam:run! 'test-make-RoleC)
  (it.bese.fiveam:run! 'test-make-TopicMapC)
  (it.bese.fiveam:run! 'test-make-AssociationC)
  (it.bese.fiveam:run! 'test-make-TopicC)
  (it.bese.fiveam:run! 'test-find-oldest-construct)
  (it.bese.fiveam:run! 'test-move-referenced-constructs-ReifiableConstructC)
  (it.bese.fiveam:run! 'test-move-referenced-constructs-NameC)
  (it.bese.fiveam:run! 'test-merge-constructs-TopicC-1)
  (it.bese.fiveam:run! 'test-merge-constructs-TopicC-2)
  (it.bese.fiveam:run! 'test-merge-constructs-TopicC-3)
  (it.bese.fiveam:run! 'test-merge-constructs-TopicC-4)
  (it.bese.fiveam:run! 'test-merge-constructs-TopicC-5)
  (it.bese.fiveam:run! 'test-merge-constructs-TopicC-6)
  (it.bese.fiveam:run! 'test-merge-constructs-TopicC-7)
  (it.bese.fiveam:run! 'test-merge-constructs-TopicC-8)
  (it.bese.fiveam:run! 'test-merge-constructs-TopicC-9)
  (it.bese.fiveam:run! 'test-merge-constructs-TopicC-10)
  (it.bese.fiveam:run! 'test-merge-constructs-AssociationC))