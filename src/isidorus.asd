;;-*- mode: lisp -*-
(defpackage :isidorus-system
  (:use :asdf :cl))
(in-package :isidorus-system)

(asdf:defsystem "isidorus"
  :description "The future ingenious, self-evaluating Lisp TM engine"
  :version "0.1"
  :author "Marc Kuester, Christoph Ludwig, Lukas Giessmann"
  :licence "LGPL"
  :components (
	       (:file "constants")
               (:static-file "xml/core_psis.xtm")
	       (:file "xml-constants" 
                      :depends-on ("xml/core_psis.xtm"
                                   "constants"))
	       (:module "model"
			:components ((:file "exceptions")
				     (:file "datamodel"
					    :depends-on ("exceptions"))
                                     (:file "changes"
                                            :depends-on ("datamodel"))
                                     (:file "model_tools"
                                            :depends-on ("exceptions")))
			:depends-on ("constants"))
	       (:module "xml"
			:components ((:file "tools")
				     (:file "importer"
					    :depends-on ("tools"))
                                     (:file "importer_xtm2.0"
                                            :depends-on ("importer"))
                                     (:file "importer_xtm1.0"
                                            :depends-on ("importer"))
                                     (:file "setup"
                                            :depends-on ("importer_xtm2.0"
                                                         "importer_xtm1.0"))
				     (:file "exporter_xtm1.0")
				     (:file "exporter_xtm2.0"
                                            :depends-on ("exporter_xtm1.0"))
				     (:file "exporter"
					    :depends-on ("exporter_xtm1.0"
							 "exporter_xtm2.0")))
			:depends-on ("constants"
                                     "xml-constants"
				     "model"))
	       (:module "atom"
			:components ((:file "atom")
;;                                      (:file "configuration"
;;                                              :depends-on ("atom"))
                                     (:file "collection"
                                            :depends-on ("atom"))
				     (:file "snapshots"
                                            :depends-on ("atom"))
                                     (:file "fragments"
                                            :depends-on ("atom"))
                                     (:file "read"
					    :depends-on ("fragments" "snapshots"))
                                     (:file "confreader"
					    :depends-on ("collection" "fragments" "snapshots")))
		       	:depends-on ("model" "xml"))
	       (:module "rest_interface"
			:components ((:file "rest-interface")
                                     (:file "publish_feeds"
                                            :depends-on ("rest-interface"))
				     (:file "set-up-json-interface"
					    :depends-on ("rest-interface"))
                                     (:file "read" 
                                            :depends-on ("rest-interface")))
		       	:depends-on ("model" 
				     "atom" 
				     "xml"
				     "json"))
	       (:module "unit_tests"
			:components ((:static-file "dangling_topicref.xtm")
				     (:static-file "inconsistent.xtm")               
				     (:static-file "notificationbase.xtm")           
				     (:static-file "notification_merge1.xtm")           
				     (:static-file "notification_merge2.xtm")           
				     (:static-file "sample_objects_2_0.xtm")
				     (:static-file "dangling_instanceof.xtm")        
				     (:static-file "duplicate_identifier.xtm")       
				     (:static-file "inconsistent_2_0.xtm")           
				     (:static-file "sample_objects.xtm")             
				     (:static-file "t100.xtm")
				     (:static-file "atom_test.xtm")
				     (:file "atom-conf")
				     (:file "unittests-constants"
					    :depends-on ("dangling_topicref.xtm"
							 "inconsistent.xtm"               
							 "notificationbase.xtm"           
							 "notification_merge1.xtm"           
							 "notification_merge2.xtm"           
							 "sample_objects_2_0.xtm"
							 "dangling_instanceof.xtm"        
							 "duplicate_identifier.xtm"       
							 "inconsistent_2_0.xtm"           
							 "sample_objects.xtm"             
							 "t100.xtm"
							 "atom-conf"))
				     (:file "fixtures"
					    :depends-on ("unittests-constants"))
				     (:file "importer_test"
					    :depends-on ("fixtures"))
				     (:file "versions_test"
					    :depends-on ("fixtures"))
				     (:file "exporter_xtm2.0_test"
				            :depends-on ("fixtures"))
				     (:file "exporter_xtm1.0_test"
				      :depends-on ("fixtures" "exporter_xtm2.0_test"))
                                     (:file "atom_test"
					    :depends-on ("fixtures"))
				     (:file "json_test"
					    :depends-on ("fixtures")))
			:depends-on ("atom"
                                     "constants"
				     "model"
				     "xml"
				     "json"))
	       (:module "json"
	                :components ((:file "json_exporter")
				     (:file "json_importer"))
	                :depends-on ("model" "xml"))
	       (:module "ajax"
			:components ((:static-file "isidorus.html")
				     (:module "javascripts"
					      :components ((:static-file "builder.js")
							   (:static-file "controls.js")
							   (:static-file "dragdrop.js")
							   (:static-file "effects.js")
							   (:static-file "prototype.js")
							   (:static-file "scriptaculous.js")
							   (:static-file "slider.js")
							   (:static-file "sound.js")
							   (:static-file "unittest.js")
							   (:static-file "ajax_constants.js")
							   (:static-file "ajax_home.js")
							   (:static-file "ajax_navi.js")
							   (:static-file "ajax_edit_topic.js")))
				     (:module "css"
					      :components ((:static-file "create_topics.css")
							   (:static-file "edit_topics.css")
							   (:static-file "home.css")
							   (:static-file "navi.css")
							   (:static-file "search_topics.css")
							   (:static-file "main.css")))))
	       )
	       ;;(:module "threading"
	       ;;	:components ((:file "reader-writer"))))
  :depends-on (:cxml
               :drakma
	       :elephant
	       :fiveam
	       :pathnames
	       :hunchentoot
               :uuid
	       :cl-json))

;;
;; For the package pathnames, create a link from  ~/.sbcl/systems
;; to the file pathnames.asd in Seibel's pathname-library.
;;
