;;+-----------------------------------------------------------------------------
;;+  Isidorus
;;+  (c) 2008-2010 Marc Kuester, Christoph Ludwig, Lukas Georgieff
;;+
;;+  Isidorus is freely distributable under the LLGPL license.
;;+  You can find a detailed description in trunk/docs/LLGPL-LICENSE.txt and
;;+  trunk/docs/LGPL-LICENSE.txt.
;;+-----------------------------------------------------------------------------

(defpackage :isidorus-system
  (:use :asdf :cl))
(in-package :isidorus-system)

;(defvar *old-external-format* sb-impl::*default-external-format*) ;;should be set by user
;(setf sb-impl::*default-external-format* :UTF-8)

(asdf:defsystem "isidorus"
  :description "The future ingenious, self-evaluating Lisp TM engine"
  :version "0.1"
  :author "Marc Kuester, Christoph Ludwig, Lukas Georgieff"
  :licence "LGPL"
  :components ((:file "constants"
		      :depends-on ("base-tools"))
               (:static-file "xml/xtm/core_psis.xtm")
	       (:static-file "xml/rdf/rdf_core_psis.xtm")
	       (:static-file "TM-SPARQL/tmsparql_core_psis.xtm")
	       (:file "xml-constants" 
                      :depends-on ("xml/xtm/core_psis.xtm"
                                   "constants"))
	       (:module "base-tools"
			:components ((:file "base-tools")))
	       (:module "model"
			:components ((:file "exceptions")
				     (:file "datamodel"
					    :depends-on ("exceptions"))
				     (:file "trivial-queries"
					    :depends-on ("datamodel"))
                                     (:file "changes"
                                            :depends-on ("datamodel" "trivial-queries"))
                                     (:file "model_tools"
                                            :depends-on ("exceptions")))
			:depends-on ("constants" "base-tools"))
	       (:module "TM-SPARQL"
			:components ((:file "sparql_constants")
				     (:file "sparql"
					    :depends-on ("sparql_constants"))
				     (:file "sparql_special_uris"
					    :depends-on ("sparql"))
				     (:file "filter_wrappers"
					    :depends-on ("sparql"))
				     (:file "sparql_filter"
					    :depends-on ("sparql" "filter_wrappers"))
				     (:file "sparql_parser"
					    :depends-on ("sparql" "sparql_filter")))
			:depends-on ("constants"
				     "base-tools"
				     "model"
				     "xml-constants"
				     "xml"
				     "threading"))
	       (:module "xml"
			:components ((:module "xtm"
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
									       "exporter_xtm2.0"))))
				     (:module "rdf"
					      :components ((:file "rdf_tools")
							   (:file "map_to_tm"
								  :depends-on ("rdf_tools"))
							   (:file "importer"
								  :depends-on ("rdf_tools" "map_to_tm"))
							   (:file "exporter"))
					      :depends-on ("xtm")))
			:depends-on ("constants"
                                     "xml-constants"
				     "base-tools"
				     "model"
				     "threading"
				     "base-tools"))
	       (:module "atom"
			:components ((:file "atom")
				     ;; (:file "configuration"
				     ;;  :depends-on ("atom"))
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
		       	:depends-on ("model"
				     "xml"
				     "threading"))
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
				     "TM-SPARQL"
				     "json"
				     "threading"
				     "base-tools"))
	       (:module "unit_tests"
			:components ((:static-file "textgrid.xtm")
				     (:static-file "textgrid_old.xtm")
				     (:static-file "dangling_topicref.xtm")
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
				     (:static-file "poems.xtm")
				     (:static-file "poems.rdf")
				     (:static-file "poems_light.rdf")
				     (:static-file "poems_light.xtm")
				     (:static-file "poems_light.xtm.txt")
				     (:static-file "poems_light_tm_ii.xtm")
				     (:static-file "poems_light_tm_ii_merge.xtm")
				     (:static-file "poems_light_tm_reification_xtm1.0.xtm")
				     (:static-file "full_mapping.rdf")
				     (:static-file "reification_xtm1.0.xtm")
				     (:static-file "reification_xtm2.0.xtm")
				     (:static-file "reification.rdf")
				     (:static-file "sparql_test.xtm")
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
					    :depends-on ("fixtures"))
				     (:file "jtm_test"
					    :depends-on ("fixtures"))
				     (:file "threading_test")
				     (:file "rdf_importer_test"
					    :depends-on ("fixtures"))
				     (:file "rdf_exporter_test"
					    :depends-on ("fixtures"))
				     (:file "datamodel_test"
					    :depends-on ("fixtures"))
				     (:file "sparql_test"
					    :depends-on ("fixtures"))
				     (:file "trivial_queries_test"
					    :depends-on ("fixtures"))
				     (:file "reification_test"
					    :depends-on ("fixtures" "unittests-constants")))
			:depends-on ("atom"
                                     "constants"
				     "model"
				     "xml"
				     "json"
				     "threading"
				     "base-tools"
				     "TM-SPARQL"))
	       (:module "json"
			:components ((:module "isidorus-json"
					      :components ((:file "json_exporter"
								  :depends-on ("json_tmcl_constants"))
							   (:file "json_importer")
							   (:file "json_tmcl_validation"
								  :depends-on ("json_tmcl_constants" "json_exporter" "json_importer"))
							   (:file "json_tmcl_constants")
							   (:file "json_tmcl"
								  :depends-on ("json_tmcl_validation" "json_importer"))
							   (:file "json_delete_interface"
								  :depends-on ("json_importer"))))
				     (:module "JTM"
					      :components ((:file "jtm_tools")
							   (:file "jtm_importer"
								  :depends-on ("jtm_tools"))
							   (:file "jtm_exporter"
								  :depends-on ("jtm_tools")))))
	                :depends-on ("base-tools"
				     "model"
				     "xml"
				     "TM-SPARQL"))
	       (:module "ajax"
			:components ((:static-file "isidorus.html")
				     (:module "javascripts"
					      :components ((:static-file "constants.js")
							   (:static-file "home.js")
							   (:static-file "navi.js")
							   (:static-file "make_fragment_node.js")
							   (:static-file "edit_topic.js")
							   (:module "external"
								    :components ((:module "prototype"
											  :components ((:static-file "prototype.js")))
										 (:module "scriptaculous"
											  :components ((:static-file "builder.js")
												       (:static-file "controls.js")
												       (:static-file "dragdrop.js")
												       (:static-file "effects.js")
												       (:static-file "scriptaculous.js")
												       (:static-file "slider.js")
												       (:static-file "sound.js")
												       (:static-file "unittest.js")))))))
				     (:module "css"
					      :components ((:static-file "home.css")
							   (:static-file "navi.css")
							   (:static-file "main.css")))))
	       (:module "threading"
			:components ((:file "reader-writer"))))
  :depends-on (:cxml
               :drakma
	       :elephant
	       :fiveam
	       :pathnames
	       :hunchentoot
               :uuid
	       :cl-json))

;(setf sb-impl::*default-external-format* *old-external-format*)



;;
;; For the package pathnames, create a link from  ~/.sbcl/systems
;; to the file pathnames.asd in Seibel's pathname-library.
;;
