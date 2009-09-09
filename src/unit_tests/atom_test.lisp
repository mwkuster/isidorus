;;+-----------------------------------------------------------------------------
;;+  Isidorus
;;+  (c) 2008-2009 Marc Kuester, Christoph Ludwig, Lukas Giessmann
;;+
;;+  Isidorus is freely distributable under the LGPL license.
;;+  You can find a detailed description in trunk/docs/LGPL-LICENSE.txt.
;;+-----------------------------------------------------------------------------


(defpackage :atom-test
  (:use 
   :common-lisp
   :datamodel
   :it.bese.FiveAM
   :fixtures
   :atom
   :xml-tools
   :cxml
   :unittests-constants)
  (:import-from :constants
                *atom-ns*
                *xtm2.0-ns*)
  (:import-from :xml-tools
                xpath-child-elems-by-qname
                xpath-select-location-path)
  (:import-from :exceptions
                missing-reference-error
                duplicate-identifier-error)
  (:export :atom-test
           :test-feed-to-string
           :test-collection-configuration
	   :test-changes-feeds
	   :run-atom-tests))

;test configuration
(in-package :atom-test)

(def-suite atom-test :description "Tests for the Atom interface")
(in-suite atom-test)

;; (test test-snapshots ()
;;       "test the behaviour of the snapshot feed"
;;       (with-fixture merge-test-db ()
;;         (let*
;;             ((feed-string
;;               (snapshot-feed-to-string))
;;              (feed-dom 
;;               (cxml:parse feed-string (cxml-dom:make-dom-builder)))
;;              (feed-elem (dom:document-element feed-dom)))
;;           ;very initial test
;;           (is (= 3 (length (xpath-child-elems-by-qname feed-elem *atom-ns* "entry")))))))

(test test-feed-to-string ()
      (with-fixture atom-test-db ()
        (let*
            ((worms-feed
              (find "http://psi.egovpt.org/tm/worms"
                    (atom:subfeeds atom:*tm-feed*)
                    :test #'string=
                    :key #'atom:id))
             (datetime-revision3 
              (atom::datetime-in-iso-format fixtures::revision3))
             (datetime-revision1
              (atom::datetime-in-iso-format fixtures::revision1))
             (collection-feed-string
              (format nil "<a:feed xmlns:a=\"http://www.w3.org/2005/Atom\" xmlns:e=\"http://www.egovpt.org/sdshare/\"><a:title>Topicmaps on psi.egovpt.org</a:title><a:id>http://london.ztt.fh-worms.de:8000/feeds</a:id><a:author><a:name>Isidor</a:name></a:author><a:link href=\"http://london.ztt.fh-worms.de:8000/feeds\" rel=\"self\"></a:link><a:updated>~a</a:updated><a:entry xmlns:a=\"http://www.w3.org/2005/Atom\" xmlns:e=\"http://www.egovpt.org/sdshare/\"><a:title>Data behind the portal of the city of Worms</a:title><a:id>http://psi.egovpt.org/tm/worms/entry</a:id><a:link href=\"http://london.ztt.fh-worms.de:8000/feeds/worms\" rel=\"alternate\"></a:link><a:link href=\"http://london.ztt.fh-worms.de:8000/feeds/worms\" rel=\"alternate\" type=\"application/atom+xml\"></a:link><a:author><a:name>Isidor</a:name></a:author><a:link href=\"http://london.ztt.fh-worms.de:8000/feeds/worms\" rel=\"http://www.egovpt.org/sdshare/collectionfeed\" type=\"application/atom+xml\"></a:link><a:updated>~a</a:updated></a:entry><a:entry xmlns:a=\"http://www.w3.org/2005/Atom\" xmlns:e=\"http://www.egovpt.org/sdshare/\"><a:title>eGov Reference Ontology</a:title><a:id>http://psi.egovpt.org/tm/egov-ontology/entry</a:id><a:link href=\"http://london.ztt.fh-worms.de:8000/feeds/egov-ontology\" rel=\"alternate\"></a:link><a:link href=\"http://london.ztt.fh-worms.de:8000/feeds/egov-ontology\" rel=\"alternate\" type=\"application/atom+xml\"></a:link><a:author><a:name>Isidor</a:name></a:author><a:link href=\"http://london.ztt.fh-worms.de:8000/feeds/egov-ontology\" rel=\"http://www.egovpt.org/sdshare/collectionfeed\" type=\"application/atom+xml\"></a:link><a:updated>~a</a:updated></a:entry></a:feed>" datetime-revision3 datetime-revision3 datetime-revision1))
             (worms-feed-string
              (format nil "<a:feed xmlns:a=\"http://www.w3.org/2005/Atom\" xmlns:e=\"http://www.egovpt.org/sdshare/\"><a:title>Data behind the portal of the city of Worms</a:title><a:id>http://london.ztt.fh-worms.de:8000/feeds/worms</a:id><a:author><a:name>Isidor</a:name></a:author><a:link href=\"http://london.ztt.fh-worms.de:8000/feeds/worms\" rel=\"self\"></a:link><e:dependency>http://london.ztt.fh-worms.de:8000/feeds/egov-ontology</e:dependency><a:updated>~a</a:updated><a:entry xmlns:a=\"http://www.w3.org/2005/Atom\" xmlns:e=\"http://www.egovpt.org/sdshare/\"><a:title>Snapshots of the Worms data</a:title><a:id>http://psi.egovpt.org/tm/worms/snapshots/entry</a:id><a:link href=\"http://london.ztt.fh-worms.de:8000/feeds/worms/snapshots\" rel=\"alternate\"></a:link><a:link href=\"http://london.ztt.fh-worms.de:8000/feeds/worms/snapshots\" rel=\"http://www.egovpt.org/sdshare/snapshotsfeed\" type=\"application/atom+xml\"></a:link><a:updated>~a</a:updated></a:entry><a:entry xmlns:a=\"http://www.w3.org/2005/Atom\" xmlns:e=\"http://www.egovpt.org/sdshare/\"><a:title>A list of all change fragments for the Worms data</a:title><a:id>http://psi.egovpt.org/tm/worms/fragments/entry</a:id><a:link href=\"http://london.ztt.fh-worms.de:8000/feeds/worms/fragments\" rel=\"alternate\"></a:link><a:link href=\"http://london.ztt.fh-worms.de:8000/feeds/worms/fragments\" rel=\"http://www.egovpt.org/sdshare/fragmentsfeed\" type=\"application/atom+xml\"></a:link><a:updated>~a</a:updated></a:entry></a:feed>" datetime-revision3 datetime-revision3 datetime-revision3)))
          (is 
           (string= 
            collection-feed-string
            (cxml:with-xml-output 
                (cxml:make-string-sink :canonical t)
              (atom:feed-to-elem atom:*tm-feed*))))
          (is (eq 'atom::collection-feed (type-of worms-feed)))
          (is 
           (string= 
            worms-feed-string
            (cxml:with-xml-output 
                (cxml:make-string-sink :canonical t)
              (atom:feed-to-elem worms-feed))))
        )))

(test test-changes-feeds ()
      "test the snapshots and fragments feeds"
      (with-fixture atom-test-db ()
        (let*
            ((worms-feed
              (find "http://psi.egovpt.org/tm/worms"
                    (atom:subfeeds atom:*tm-feed*)
                    :test #'string=
                    :key #'atom:id))
             (ont-feed
              (find "http://psi.egovpt.org/tm/egov-ontology"
                    (atom:subfeeds atom:*tm-feed*)
                    :test #'string=
                    :key #'atom:id))
             (fragments-feed 
              (find 'atom::fragments-feed
                    (atom:subfeeds worms-feed)
                    :key #'type-of))
             (snapshots-feed 
              (find 'atom::snapshots-feed
                    (atom:subfeeds worms-feed)
                    :key #'type-of)))
          (is (= 11 (length (atom:entries fragments-feed))))
          (is (string= "http://london.ztt.fh-worms.de:8000/feeds/worms/fragments" (link fragments-feed)))
          (is (string= "http://london.ztt.fh-worms.de:8000/feeds/worms/snapshots" (link snapshots-feed)))

          (format t "~a" (cxml:with-xml-output 
                             (cxml:make-string-sink :canonical t)
                           (atom::feed-to-elem fragments-feed)))
          (is (= 3 (length (atom:entries snapshots-feed))))

          ;;cross check against ontology feed (no changes there and
          ;;only one snapshot)
          (assert ont-feed)
          (is (= 0 (length
                    (atom:entries 
                     (find 'atom::fragments-feed
                           (atom:subfeeds ont-feed)
                           :key #'type-of)))))
          (is (= 1 (length
                    (atom:entries
                     (find 'atom::snapshots-feed
                           (atom:subfeeds ont-feed)
                           :key #'type-of)))))
	  (format t "~a" 
		(cxml:with-xml-output 
		    (cxml:make-string-sink :canonical t)
		  (atom:feed-to-elem snapshots-feed)))
      )))

(test test-collection-configuration ()
      "test the configuration reader"
      (with-fixture atom-test-db ()
        (let
            ((collection-feeds (atom:subfeeds atom:*tm-feed*))
             (collection-entries (atom:entries atom:*tm-feed*)))
          (is (= 2 (length collection-feeds)))
          (is (= 2 (length collection-entries)))
          (is (string= "psi.egovpt.org" (atom:id atom:*tm-feed*)))
          
          ;;id-test for feeds
          (is-false 
           (set-exclusive-or 
            '("http://psi.egovpt.org/tm/egov-ontology"
            "http://psi.egovpt.org/tm/worms" )
            (mapcar #'atom:id collection-feeds) :test #'string=))

          ;;id-test for entries
          ;!!!!
          (is-false 
           (set-exclusive-or 
            '("http://psi.egovpt.org/tm/egov-ontology/entry"
            "http://psi.egovpt.org/tm/worms/entry" )
            (mapcar #'atom:id collection-entries) :test #'string=))


          ;;test relative paths for feeds
          (is-false 
           (set-exclusive-or 
            '("feeds/egov-ontology"
              "feeds/worms")
            (mapcar #'atom:path collection-feeds) :test #'string=))

          ;;test relative paths for entries (the same as for feeds)
          (is-false 
           (set-exclusive-or 
            '("feeds/egov-ontology"
              "feeds/worms")
            (mapcar #'atom:path collection-entries) :test #'string=))

          ;;test self links for feeds
          (is-false 
           (set-exclusive-or 
            '("http://london.ztt.fh-worms.de:8000/feeds/egov-ontology"
              "http://london.ztt.fh-worms.de:8000/feeds/worms")
            (mapcar #'atom:link collection-feeds) :test #'string=))
          ;;test alternate links for entries (the same as the one of
          ;;the feed they point to)
          (is-false 
           (set-exclusive-or 
            '("http://london.ztt.fh-worms.de:8000/feeds/egov-ontology"
              "http://london.ztt.fh-worms.de:8000/feeds/worms")
            (mapcar #'atom:link collection-entries) :test #'string=))

          ;;test dependencies
          (is-false
           (set-exclusive-or
            '(() ("http://london.ztt.fh-worms.de:8000/feeds/egov-ontology"))
            (mapcar #'atom:dependency collection-feeds) :test #'equal))

          ;;test types for feeds
          (is-false 
           (set-exclusive-or 
            '(atom::collection-feed
              atom::collection-feed)
            (mapcar #'type-of collection-feeds)))

          ;;test types for entries
          (is-false 
           (set-exclusive-or 
            '(atom::overview-entry
              atom::overview-entry)
            (mapcar #'type-of collection-entries)))
          )))


(defun run-atom-tests()
  (it.bese.fiveam:run! 'test-feed-to-string)
  (it.bese.fiveam:run! 'test-changes-feeds)
  (it.bese.fiveam:run! 'test-collection-configuration))