;;+-----------------------------------------------------------------------------
;;+  Isidorus
;;+  (c) 2008-2009 Marc Kuester, Christoph Ludwig, Lukas Giessmann
;;+
;;+  Isidorus is freely distributable under the LGPL license.
;;+  You can find a detailed description in trunk/docs/LGPL-LICENSE.txt.
;;+-----------------------------------------------------------------------------


(defpackage :fixtures
  (:use 
   :common-lisp
   :xml-importer
   :datamodel
   :it.bese.FiveAM
   :unittests-constants)
  (:import-from :constants
                *xtm2.0-ns*)
  (:import-from :xml-tools
                xpath-child-elems-by-qname
                xpath-select-location-path)
  (:import-from :exceptions
                missing-reference-error
                duplicate-identifier-error)
  (:export :atom-test-db
           :bare-test-db
	   :clean-out-db
	   :initialized-test-db
	   :initialize-destination-db
           :merge-test-db
	   :set-up-test-db
	   :tear-down-test-db
	   :rdf-exporter-test-db
	   :*TEST-TM*
	   :*NOTIFICATIONBASE-TM*
	   :*XTM-TM*
           :*XTM-MERGE1-TM*
           :*XTM-MERGE2-TM*
	   :rdf-init-db
	   :rdf-test-db))

(in-package :fixtures)

(defvar *XTM-TM* nil)

(defvar *TEST-TM* "test-tm")

(defvar *NOTIFICATIONBASE-TM*
  (dom:document-element
   (cxml:parse-file *notificationbase.xtm* (cxml-dom:make-dom-builder))))

(setf *debug-on-error* t)
(setf *debug-on-failure* t)

(defun clean-out-db (dir)
  (let
      ((dirname (make-pathname :directory (list :relative dir))))
    (ensure-directories-exist dirname)
    (loop for filename in (com.gigamonkeys.pathnames:list-directory dirname) do
	 (delete-file filename))))
  
(defun set-up-test-db (&optional (revision 0))
  "clears out the database and parses the test file"
  (clean-out-db "data_base")

  (elephant:open-store (get-store-spec "data_base"))
  (init-isidorus revision)
  (setf *current-xtm* *TEST-TM*)
  ;deliberately only use stubs at this stage 
  (import-only-topics *xtm-tm* 
                      :tm-id "http://www.isidor.us/unittests/testtm"
                      :revision revision
                      :xtm-id *TEST-TM*))
 
(defun set-up-raw-test-db ()
  (clean-out-db "data_base")
  (elephant:open-store (get-store-spec "data_base"))
  (init-isidorus)
  (setf *current-xtm* *TEST-TM*))

(defun tear-down-test-db ()
  "make sure the elephant store is properly closed"
  (elephant:close-store))

(def-fixture bare-test-db ()
  (set-up-raw-test-db)
  (&body)
  (tear-down-test-db))

(def-fixture initialize-destination-db (dir)
  (clean-out-db dir)
  (&body)
  (tear-down-test-db))

(def-fixture initialized-test-db (&optional (xtm *NOTIFICATIONBASE-TM*))
  (let
      ((revision (get-revision)))
    (declare (ignorable revision))
    (setf *XTM-TM* xtm)
    (set-up-test-db revision)
    (let
        ((tm 
          (get-item-by-item-identifier "http://www.isidor.us/unittests/testtm" :revision (d:get-revision))))
      (declare (ignorable tm))
      (&body)
      (tear-down-test-db))))


(defvar *XTM-MERGE1-TM*
  (dom:document-element
   (cxml:parse-file *notification_merge1.xtm* (cxml-dom:make-dom-builder))))

(defparameter *XTM-MERGE2-TM*
  (dom:document-element
   (cxml:parse-file *notification_merge2.xtm* (cxml-dom:make-dom-builder))))

(defparameter *XTM-ATOM-TM*
  (dom:document-element
   (cxml:parse-file *atom_test.xtm* (cxml-dom:make-dom-builder))))

(def-fixture merge-test-db ()
  (setf *XTM-TM* *NOTIFICATIONBASE-TM*)
  (let*
	((revision1 (get-revision))
	 (revision2 (+ 200 revision1)) ; some arbitrary differences
	 (revision3 (+ 400 revision1)))
    (set-up-test-db revision1)

    (importer *XTM-TM* 
              :tm-id "http://www.isidor.us/unittests/testtm"
              :xtm-id *TEST-TM* :revision revision1)
    (importer *XTM-MERGE1-TM* :xtm-id "merge1" 
              :tm-id "http://www.isidor.us/unittests/testtm"
              :revision revision2)
    (importer *XTM-MERGE2-TM* :xtm-id "merge2" 
              :tm-id "http://www.isidor.us/unittests/testtm"
              :revision revision3)
    (&body) 
    (tear-down-test-db)))

(defun init-conf ()
  (in-package :atom)
  ;;test configuration
  ;(defparameter *tm-feed* nil)
  (setf atom:*base-url* "http://london.ztt.fh-worms.de:8000")
  (load *atom-conf.lisp*))

(def-fixture atom-test-db ()
  (setf *XTM-TM* *NOTIFICATIONBASE-TM*)
  (init-conf)
  (let*
	((revision1 (get-revision))
	 (revision2 (+ 200 revision1)) ; some arbitrary differences
	 (revision3 (+ 400 revision1)))
    (set-up-test-db revision1)
    
    (importer *XTM-TM* 
              ;;aligned with conf.lisp
              :tm-id "http://psi.egovpt.org/tm/worms"
              :xtm-id *TEST-TM* :revision revision1)
    (importer *XTM-MERGE1-TM* :xtm-id "merge1" 
              :tm-id "http://psi.egovpt.org/tm/worms"
              :revision revision2)
    (importer *XTM-MERGE2-TM* :xtm-id "merge2" 
              :tm-id "http://psi.egovpt.org/tm/worms"
              :revision revision3)

    (importer *XTM-ATOM-TM* :xtm-id "atom-tm1" :tm-id "http://psi.egovpt.org/tm/egov-ontology"
              :revision revision1)
    (&body) 
    (tear-down-test-db)))


(defun rdf-init-db (&key (db-dir "data_base") (start-revision (get-revision)))
  "Deletes the data base files and initializes isidorus for rdf."
  (when elephant:*store-controller*
    (elephant:close-store))
  (clean-out-db db-dir)
  (elephant:open-store (xml-importer:get-store-spec db-dir))
  (xml-importer:init-isidorus start-revision)
  (rdf-importer:init-rdf-module start-revision))


(def-fixture rdf-test-db ()
  (let ((db-dir "data_base")
	(tm-id "http://test-tm/")
	(document-id "doc-id"))
    (clean-out-db db-dir)
    (setf d:*current-xtm* document-id)
    (rdf-importer:setup-rdf-module *poems_light.rdf* db-dir :tm-id tm-id
				   :document-id document-id)
    (elephant:open-store (xml-importer:get-store-spec db-dir))
    (&body)
    (tear-down-test-db)))


(def-fixture rdf-exporter-test-db()
  (let ((db-dir "data_base")
	(tm-id "http://test-tm")
	(document-id "doc-id")
	(exported-file-path "./__out__.rdf"))
    (clean-out-db db-dir)
    (handler-case (delete-file exported-file-path)
      (error () )) ;do nothing
    (setf d:*current-xtm* document-id)
    (setup-repository *poems_light.xtm* db-dir :tm-id tm-id
		      :xtm-id document-id)
    (elephant:open-store (xml-importer:get-store-spec db-dir))
    (rdf-exporter:export-rdf exported-file-path :tm-id tm-id)
    (&body)
    (handler-case (delete-file exported-file-path)
      (error () )) ;do nothing
    (tear-down-test-db)))