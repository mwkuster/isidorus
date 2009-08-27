;;+-----------------------------------------------------------------------------
;;+  Isidorus
;;+  (c) 2008-2009 Marc Kuester, Christoph Ludwig, Lukas Giessmann
;;+
;;+  Isidorus is freely distributable under the LGPL license.
;;+  You can find a detailed description in trunk/docs/LGPL-LICENSE.txt.
;;+-----------------------------------------------------------------------------


(defpackage :json-test
  (:use 
   :common-lisp
   :xml-importer
   :json-exporter
   :json-importer
   :datamodel
   :it.bese.FiveAM
   :unittests-constants
   :fixtures)
  (:export :test-to-json-string-topics
	   :test-to-json-string-associations
	   :test-to-json-string-fragments
	   :test-get-fragment-values-from-json-list-general
	   :test-get-fragment-values-from-json-list-names
	   :test-get-fragment-values-from-json-list-occurrences
	   :test-get-fragment-values-from-json-list-topicStubs
	   :test-get-fragment-values-from-json-list-associations
	   :run-json-tests
	   :test-json-importer-general-1
	   :test-json-importer-general-2
	   :test-json-importer-general-3
	   :test-json-importer-topics-1
	   :test-json-importer-topics-2
	   :test-json-importer-topics-3
	   :test-json-importer-topics-4
	   :test-json-importer-associations
	   :test-json-importer-merge-1
	   :test-json-importer-merge-2
	   :test-json-importer-merge-3
	   :test-get-all-topic-psis))


(in-package :json-test)


(def-suite json-tests
     :description "tests various functions of the json module")

(in-suite json-tests)


(defvar *t100-1* "{\"topic\":{\"id\":\"t970\",\"itemIdentities\":[\"http://www.egovpt.org/itemIdentifiers#t100\"],\"subjectLocators\":null,\"subjectIdentifiers\":[\"http://psi.egovpt.org/standard/Common+Lisp\"],\"instanceOfs\":[[\"http://psi.egovpt.org/types/standard\"]],\"names\":[{\"itemIdentities\":[\"http://www.egovpt.org/itemIdentifiers#t100_n1\",\"http://www.egovpt.org/itemIdentifiers#t100_n1a\"],\"type\":null,\"scopes\":null,\"value\":\"Common Lisp\",\"variants\":[{\"itemIdentities\":[\"http://www.egovpt.org/itemIdentifiers#t100_n_v1\"],\"scopes\":[[\"http://www.topicmaps.org/xtm/1.0/core.xtm#sort\"],[\"http://psi.egovpt.org/types/long-name\"]],\"resourceRef\":null,\"resourceData\":{\"datatype\":\"http://www.w3.org/2001/XMLSchema#string\",\"value\":\"Common-Lisp\"}}]}],\"occurrences\":[{\"itemIdentities\":[\"http://www.egovpt.org/itemIdentifiers#t100_o1\"],\"type\":[\"http://psi.egovpt.org/types/links\"],\"scopes\":null,\"resourceRef\":\"http://www.common-lisp.net/\",\"resourceData\":null}]},\"topicStubs\":[{\"id\":\"t220\",\"itemIdentities\":[\"http://psi.egovpt.org/itemIdentifiers#t3\",\"http://www.egovpt.org/itemIdentifiers#t3\"],\"subjectLocators\":null,\"subjectIdentifiers\":[\"http://psi.egovpt.org/types/standard\"]},{\"id\":\"t68\",\"itemIdentities\":null,\"subjectLocators\":null,\"subjectIdentifiers\":[\"http://www.topicmaps.org/xtm/1.0/core.xtm#sort\"]},{\"id\":\"t284\",\"itemIdentities\":[\"http://psi.egovpt.org/itemIdentifiers#t50a\"],\"subjectLocators\":null,\"subjectIdentifiers\":[\"http://psi.egovpt.org/types/long-name\"]},{\"id\":\"t324\",\"itemIdentities\":[\"http://psi.egovpt.org/itemIdentifiers#t55\",\"http://psi.egovpt.org/itemIdentifiers#t55_1\"],\"subjectLocators\":null,\"subjectIdentifiers\":[\"http://psi.egovpt.org/types/links\"]}],\"associations\":null,\"tmIds\":[\"http://www.isidor.us/unittests/testtm\"]}")

(defvar *t100-2* "{\"topic\":{\"id\":\"t945\",\"itemIdentities\":[\"http://www.egovpt.org/itemIdentifiers#t100\",\"http://www.egovpt.org/itemIdentifiers#t100_new\"],\"subjectLocators\":null,\"subjectIdentifiers\":[\"http://psi.egovpt.org/standard/Common+Lisp\"],\"instanceOfs\":[[\"http://psi.egovpt.org/types/standard\"]],\"names\":[{\"itemIdentities\":[\"http://www.egovpt.org/itemIdentifiers#t100_n1\"],\"type\":null,\"scopes\":null,\"value\":\"Common Lisp\",\"variants\":[{\"itemIdentities\":[\"http://www.egovpt.org/itemIdentifiers#t100_n_v1\"],\"scopes\":[[\"http://www.topicmaps.org/xtm/1.0/core.xtm#sort\"],[\"http://psi.egovpt.org/types/long-name\"]],\"resourceRef\":null,\"resourceData\":{\"datatype\":\"http://www.w3.org/2001/XMLSchema#string\",\"value\":\"Common-Lisp\"}},{\"itemIdentities\":[\"http://www.egovpt.org/itemIdentifiers#t100_n_v2\"],\"scopes\":[[\"http://www.topicmaps.org/xtm/1.0/core.xtm#display\"]],\"resourceRef\":null,\"resourceData\":{\"datatype\":\"http://www.w3.org/2001/XMLSchema#string\",\"value\":\"CL\"}}]}],\"occurrences\":[{\"itemIdentities\":[\"http://www.egovpt.org/itemIdentifiers#t100_o2\"],\"type\":[\"http://psi.egovpt.org/types/links\"],\"scopes\":null,\"resourceRef\":\"http://www.cliki.net/\",\"resourceData\":null}]},\"topicStubs\":[{\"id\":\"t220\",\"itemIdentities\":[\"http://psi.egovpt.org/itemIdentifiers#t3\",\"http://www.egovpt.org/itemIdentifiers#t3\"],\"subjectLocators\":null,\"subjectIdentifiers\":[\"http://psi.egovpt.org/types/standard\"]},{\"id\":\"t68\",\"itemIdentities\":null,\"subjectLocators\":null,\"subjectIdentifiers\":[\"http://www.topicmaps.org/xtm/1.0/core.xtm#sort\"]},{\"id\":\"t284\",\"itemIdentities\":[\"http://psi.egovpt.org/itemIdentifiers#t50a\"],\"subjectLocators\":null,\"subjectIdentifiers\":[\"http://psi.egovpt.org/types/long-name\"]},{\"id\":\"t74\",\"itemIdentities\":null,\"subjectLocators\":null,\"subjectIdentifiers\":[\"http://www.topicmaps.org/xtm/1.0/core.xtm#display\"]},{\"id\":\"t324\",\"itemIdentities\":[\"http://psi.egovpt.org/itemIdentifiers#t55\"],\"subjectLocators\":null,\"subjectIdentifiers\":[\"http://psi.egovpt.org/types/links\"]}],\"associations\":null,\"tmIds\":[\"http://www.isidor.us/unittests/testtm\"]}")

(defvar *t100-3* "{\"topic\":{\"id\":\"t404\",\"itemIdentities\":[\"http://psi.egovpt.org/itemIdentifiers#t100\"],\"subjectLocators\":null,\"subjectIdentifiers\":[\"http://psi.egovpt.org/standard/ISO+19115%3A+Geographic+Information+-+Metadata\"],\"instanceOfs\":[[\"http://psi.egovpt.org/types/semanticstandard\"]],\"names\":[{\"itemIdentities\":[\"http://psi.egovpt.org/itemIdentifiers#t100_n1\"],\"type\":null,\"scopes\":null,\"value\":\"ISO 19115\",\"variants\":[{\"itemIdentities\":[\"http://psi.egovpt.org/itemIdentifiers#t100_n1_v1\"],\"scopes\":[[\"http://www.topicmaps.org/xtm/1.0/core.xtm#display\"]],\"resourceRef\":null,\"resourceData\":{\"datatype\":\"http://www.w3.org/2001/XMLSchema#string\",\"value\":\"Geographic Information - Metadata\"}},{\"itemIdentities\":[\"http://psi.egovpt.org/itemIdentifiers#t100_n1_v2\"],\"scopes\":[[\"http://www.topicmaps.org/xtm/1.0/core.xtm#sort\"]],\"resourceRef\":null,\"resourceData\":{\"datatype\":\"http://www.w3.org/2001/XMLSchema#string\",\"value\":\"ISO-19115\"}}]}],\"occurrences\":[{\"itemIdentities\":[\"http://psi.egovpt.org/itemIdentifiers#t100_o1\"],\"type\":[\"http://psi.egovpt.org/types/standardHasStatus\"],\"scopes\":null,\"resourceRef\":\"http://www.budabe.de/\",\"resourceData\":null},{\"itemIdentities\":[\"http://psi.egovpt.org/itemIdentifiers#t100_o2\"],\"type\":[\"http://psi.egovpt.org/types/description\"],\"scopes\":null,\"resourceRef\":null,\"resourceData\":{\"datatype\":\"http://www.w3.org/2001/XMLSchema#string\",\"value\":\"The ISO 19115 standard ...\"}},{\"itemIdentities\":[\"http://psi.egovpt.org/itemIdentifiers#t100_o3\"],\"type\":[\"http://psi.egovpt.org/types/standardValidFromDate\"],\"scopes\":null,\"resourceRef\":null,\"resourceData\":{\"datatype\":\"http://www.w3.org/2001/XMLSchema#date\",\"value\":\"2003-01-01\"}},{\"itemIdentities\":[\"http://psi.egovpt.org/itemIdentifiers#t100_o4\"],\"type\":[\"http://psi.egovpt.org/types/links\"],\"scopes\":null,\"resourceRef\":\"http://www.editeur.org/standards/ISO19115.pdf\",\"resourceData\":null}]},\"topicStubs\":[{\"id\":\"t228\",\"itemIdentities\":[\"http://psi.egovpt.org/itemIdentifiers#t3a\"],\"subjectLocators\":null,\"subjectIdentifiers\":[\"http://psi.egovpt.org/types/semanticstandard\"]},{\"id\":\"t74\",\"itemIdentities\":null,\"subjectLocators\":null,\"subjectIdentifiers\":[\"http://www.topicmaps.org/xtm/1.0/core.xtm#display\"]},{\"id\":\"t68\",\"itemIdentities\":null,\"subjectLocators\":null,\"subjectIdentifiers\":[\"http://www.topicmaps.org/xtm/1.0/core.xtm#sort\"]},{\"id\":\"t292\",\"itemIdentities\":[\"http://psi.egovpt.org/itemIdentifiers#t51\"],\"subjectLocators\":null,\"subjectIdentifiers\":[\"http://psi.egovpt.org/types/standardHasStatus\"]},{\"id\":\"t308\",\"itemIdentities\":[\"http://psi.egovpt.org/itemIdentifiers#t53\"],\"subjectLocators\":null,\"subjectIdentifiers\":[\"http://psi.egovpt.org/types/description\"]},{\"id\":\"t316\",\"itemIdentities\":[\"http://psi.egovpt.org/itemIdentifiers#t54\"],\"subjectLocators\":null,\"subjectIdentifiers\":[\"http://psi.egovpt.org/types/standardValidFromDate\"]},{\"id\":\"t324\",\"itemIdentities\":[\"http://psi.egovpt.org/itemIdentifiers#t55\"],\"subjectLocators\":null,\"subjectIdentifiers\":[\"http://psi.egovpt.org/types/links\"]},{\"id\":\"t434\",\"itemIdentities\":null,\"subjectLocators\":null,\"subjectIdentifiers\":[\"http://psi.egovpt.org/subject/GeoData\"]},{\"id\":\"t364\",\"itemIdentities\":[\"http://psi.egovpt.org/itemIdentifiers#t60\"],\"subjectLocators\":null,\"subjectIdentifiers\":[\"http://psi.egovpt.org/types/standardIsAboutSubject\"]},{\"id\":\"t372\",\"itemIdentities\":[\"http://psi.egovpt.org/itemIdentifiers#t61\"],\"subjectLocators\":null,\"subjectIdentifiers\":[\"http://psi.egovpt.org/types/SubjectRoleType\"]},{\"id\":\"t422\",\"itemIdentities\":null,\"subjectLocators\":null,\"subjectIdentifiers\":[\"http://psi.egovpt.org/subject/Semantic+Description\"]},{\"id\":\"t396\",\"itemIdentities\":[\"http://psi.egovpt.org/itemIdentifiers#t64\"],\"subjectLocators\":null,\"subjectIdentifiers\":[\"http://psi.egovpt.org/types/serviceUsesStandard\"]},{\"id\":\"t388\",\"itemIdentities\":[\"http://psi.egovpt.org/itemIdentifiers#t63\"],\"subjectLocators\":null,\"subjectIdentifiers\":[\"http://psi.egovpt.org/types/ServiceRoleType\"]},{\"id\":\"t452\",\"itemIdentities\":null,\"subjectLocators\":null,\"subjectIdentifiers\":[\"http://psi.egovpt.org/service/Google+Maps\",\"http://maps.google.com\"]},{\"id\":\"t380\",\"itemIdentities\":[\"http://psi.egovpt.org/itemIdentifiers#t62\"],\"subjectLocators\":null,\"subjectIdentifiers\":[\"http://psi.egovpt.org/types/StandardRoleType\"]}],\"associations\":[{\"itemIdentities\":null,\"type\":[\"http://psi.egovpt.org/types/standardIsAboutSubject\"],\"scopes\":null,\"roles\":[{\"itemIdentities\":null,\"type\":[\"http://psi.egovpt.org/types/StandardRoleType\"],\"topicRef\":[\"http://psi.egovpt.org/standard/ISO+19115%3A+Geographic+Information+-+Metadata\"]},{\"itemIdentities\":null,\"type\":[\"http://psi.egovpt.org/types/SubjectRoleType\"],\"topicRef\":[\"http://psi.egovpt.org/subject/GeoData\"]}]},{\"itemIdentities\":null,\"type\":[\"http://psi.egovpt.org/types/standardIsAboutSubject\"],\"scopes\":null,\"roles\":[{\"itemIdentities\":null,\"type\":[\"http://psi.egovpt.org/types/StandardRoleType\"],\"topicRef\":[\"http://psi.egovpt.org/standard/ISO+19115%3A+Geographic+Information+-+Metadata\"]},{\"itemIdentities\":null,\"type\":[\"http://psi.egovpt.org/types/SubjectRoleType\"],\"topicRef\":[\"http://psi.egovpt.org/subject/Semantic+Description\"]}]},{\"itemIdentities\":[\"http://psi.egovpt.org/itemIdentifiers#assoc_7\"],\"type\":[\"http://psi.egovpt.org/types/serviceUsesStandard\"],\"scopes\":null,\"roles\":[{\"itemIdentities\":null,\"type\":[\"http://psi.egovpt.org/types/ServiceRoleType\"],\"topicRef\":[\"http://psi.egovpt.org/service/Google+Maps\",\"http://maps.google.com\"]},{\"itemIdentities\":null,\"type\":[\"http://psi.egovpt.org/types/StandardRoleType\"],\"topicRef\":[\"http://psi.egovpt.org/standard/ISO+19115%3A+Geographic+Information+-+Metadata\"]}]}],\"tmIds\":[\"http://www.isidor.us/unittests/testtm\"]}")

(defvar *t64* "{\"topic\":{\"id\":\"t396\",\"itemIdentities\":[\"http://psi.egovpt.org/itemIdentifiers#t64\"],\"subjectLocators\":null,\"subjectIdentifiers\":[\"http://psi.egovpt.org/types/serviceUsesStandard\"],\"instanceOfs\":[[\"http://www.networkedplanet.com/psi/npcl/meta-types/association-type\"]],\"names\":[{\"itemIdentities\":null,\"type\":null,\"scopes\":null,\"value\":\"service uses standard\",\"variants\":null}],\"occurrences\":null},\"topicStubs\":[{\"id\":\"t260\",\"itemIdentities\":[\"http://psi.egovpt.org/itemIdentifiers#t7\"],\"subjectLocators\":null,\"subjectIdentifiers\":[\"http://www.networkedplanet.com/psi/npcl/meta-types/association-type\"]}],\"associations\":null,\"tmIds\":[\"http://www.isidor.us/unittests/testtm\"]}")


(test test-to-json-string-topics
  (let
      ((dir "data_base"))
    (with-fixture initialize-destination-db (dir)
      (xml-importer:setup-repository
       *notificationbase.xtm* dir :tm-id "http://www.isidor.us/unittests/testtm"
                                  :xtm-id *TEST-TM*)

      (elephant:open-store (xml-importer:get-store-spec dir))
      (let ((t50a (get-item-by-id "t50a")))
	(let ((t50a-string (to-json-string t50a))
	      (json-string 
	       (concatenate 'string "{\"id\":\"" (topicid t50a) "\",\"itemIdentities\":[\"http:\\/\\/psi.egovpt.org\\/itemIdentifiers#t50a\"],\"subjectLocators\":null,\"subjectIdentifiers\":[\"http:\\/\\/psi.egovpt.org\\/types\\/long-name\"],\"instanceOfs\":[[\"http:\\/\\/www.networkedplanet.com\\/psi\\/npcl\\/meta-types\\/occurrence-type\"]],\"names\":[{\"itemIdentities\":null,\"type\":null,\"scopes\":null,\"value\":\"long version of a name\",\"variants\":[{\"itemIdentities\":null,\"scopes\":[[\"http:\\/\\/www.topicmaps.org\\/xtm\\/1.0\\/core.xtm#sort\"]],\"resourceRef\":null,\"resourceData\":{\"datatype\":\"http:\\/\\/www.w3.org\\/2001\\/XMLSchema#string\",\"value\":\"Long-Version\"}}]}],\"occurrences\":null}" )))
	  (is (string= t50a-string json-string)))
	(let ((t8 (get-item-by-id "t8")))
	  (let ((t8-string (to-json-string t8))
		(json-string 
		 (concatenate 'string "{\"id\":\"" (topicid t8) "\",\"itemIdentities\":[\"http:\\/\\/psi.egovpt.org\\/itemIdentifiers#t8\"],\"subjectLocators\":null,\"subjectIdentifiers\":[\"http:\\/\\/www.networkedplanet.com\\/psi\\/npcl\\/meta-types\\/association-role-type\"],\"instanceOfs\":[[\"http:\\/\\/www.networkedplanet.com\\/psi\\/npcl\\/meta-types\\/topic-type\"]],\"names\":[{\"itemIdentities\":null,\"type\":null,\"scopes\":null,\"value\":\"Association Role Type\",\"variants\":null}],\"occurrences\":null}")))
	    (is (string= t8-string json-string))))
	(let ((t-topic (get-item-by-id "topic" :xtm-id "core.xtm")))
	  (let ((t-topic-string (to-json-string t-topic))
		(json-string
		 (concatenate 'string "{\"id\":\"" (topicid t-topic) "\",\"itemIdentities\":null,\"subjectLocators\":null,\"subjectIdentifiers\":[\"http:\\/\\/www.topicmaps.org\\/xtm\\/1.0\\/core.xtm#topic\"],\"instanceOfs\":null,\"names\":null,\"occurrences\":null}")))
	    (is (string= t-topic-string json-string))))
	(let ((t301 (get-item-by-id "t301")))
	  (let ((t301-string (to-json-string t301))
		(json-string
		 (concatenate 'string "{\"id\":\"" (topicid t301) "\",\"itemIdentities\":null,\"subjectLocators\":null,\"subjectIdentifiers\":[\"http:\\/\\/psi.egovpt.org\\/service\\/Google+Maps\",\"http:\\/\\/maps.google.com\"],\"instanceOfs\":[[\"http:\\/\\/psi.egovpt.org\\/types\\/service\"]],\"names\":[{\"itemIdentities\":[\"http:\\/\\/psi.egovpt.org\\/topic\\/t301a_n1\"],\"type\":null,\"scopes\":[[\"http:\\/\\/psi.egovpt.org\\/types\\/long-name\"]],\"value\":\"Google Maps\",\"variants\":null},{\"itemIdentities\":null,\"type\":null,\"scopes\":[[\"http:\\/\\/psi.egovpt.org\\/types\\/long-name\"]],\"value\":\"Google Maps Application\",\"variants\":null}],\"occurrences\":[{\"itemIdentities\":null,\"type\":[\"http:\\/\\/psi.egovpt.org\\/types\\/description\"],\"scopes\":null,\"resourceRef\":null,\"resourceData\":{\"datatype\":\"http:\\/\\/www.w3.org\\/2001\\/XMLSchema#string\",\"value\":\"a popular geodata service that is widely used for mashups with geodataProbably not really conformant to ISO 19115, but who cares in this context.\"}},{\"itemIdentities\":null,\"type\":[\"http:\\/\\/psi.egovpt.org\\/types\\/links\"],\"scopes\":null,\"resourceRef\":\"http:\\/\\/maps.google.com\",\"resourceData\":null},{\"itemIdentities\":null,\"type\":[\"http:\\/\\/psi.egovpt.org\\/types\\/links\"],\"scopes\":null,\"resourceRef\":\"http:\\/\\/maps.google.de\",\"resourceData\":null}]}")))
	    (is (string= t301-string json-string))))
	(let ((t100 (get-item-by-id "t100")))
	  (let ((t100-string (to-json-string t100))
		(json-string
		 (concatenate 'string "{\"id\":\"" (topicid t100) "\",\"itemIdentities\":[\"http:\\/\\/psi.egovpt.org\\/itemIdentifiers#t100\"],\"subjectLocators\":null,\"subjectIdentifiers\":[\"http:\\/\\/psi.egovpt.org\\/standard\\/ISO+19115%3A+Geographic+Information+-+Metadata\"],\"instanceOfs\":[[\"http:\\/\\/psi.egovpt.org\\/types\\/semanticstandard\"]],\"names\":[{\"itemIdentities\":[\"http:\\/\\/psi.egovpt.org\\/itemIdentifiers#t100_n1\"],\"type\":null,\"scopes\":null,\"value\":\"ISO 19115\",\"variants\":[{\"itemIdentities\":[\"http:\\/\\/psi.egovpt.org\\/itemIdentifiers#t100_n1_v1\"],\"scopes\":[[\"http:\\/\\/www.topicmaps.org\\/xtm\\/1.0\\/core.xtm#display\"]],\"resourceRef\":null,\"resourceData\":{\"datatype\":\"http:\\/\\/www.w3.org\\/2001\\/XMLSchema#string\",\"value\":\"Geographic Information - Metadata\"}},{\"itemIdentities\":[\"http:\\/\\/psi.egovpt.org\\/itemIdentifiers#t100_n1_v2\"],\"scopes\":[[\"http:\\/\\/www.topicmaps.org\\/xtm\\/1.0\\/core.xtm#sort\"]],\"resourceRef\":null,\"resourceData\":{\"datatype\":\"http:\\/\\/www.w3.org\\/2001\\/XMLSchema#string\",\"value\":\"ISO-19115\"}}]}],\"occurrences\":[{\"itemIdentities\":[\"http:\\/\\/psi.egovpt.org\\/itemIdentifiers#t100_o1\"],\"type\":[\"http:\\/\\/psi.egovpt.org\\/types\\/standardHasStatus\"],\"scopes\":null,\"resourceRef\":\"http:\\/\\/www.budabe.de\\/\",\"resourceData\":null},{\"itemIdentities\":[\"http:\\/\\/psi.egovpt.org\\/itemIdentifiers#t100_o2\"],\"type\":[\"http:\\/\\/psi.egovpt.org\\/types\\/description\"],\"scopes\":null,\"resourceRef\":null,\"resourceData\":{\"datatype\":\"http:\\/\\/www.w3.org\\/2001\\/XMLSchema#string\",\"value\":\"The ISO 19115 standard ...\"}},{\"itemIdentities\":[\"http:\\/\\/psi.egovpt.org\\/itemIdentifiers#t100_o3\"],\"type\":[\"http:\\/\\/psi.egovpt.org\\/types\\/standardValidFromDate\"],\"scopes\":null,\"resourceRef\":null,\"resourceData\":{\"datatype\":\"http:\\/\\/www.w3.org\\/2001\\/XMLSchema#date\",\"value\":\"2003-01-01\"}},{\"itemIdentities\":[\"http:\\/\\/psi.egovpt.org\\/itemIdentifiers#t100_o4\"],\"type\":[\"http:\\/\\/psi.egovpt.org\\/types\\/links\"],\"scopes\":null,\"resourceRef\":\"http:\\/\\/www.editeur.org\\/standards\\/ISO19115.pdf\",\"resourceData\":null}]}")))
	    (is (string= t100-string json-string))))))))


(test test-to-json-string-associations
  (let
      ((dir "data_base"))
    (with-fixture initialize-destination-db (dir)
      (xml-importer:setup-repository
       *notificationbase.xtm* dir :tm-id "http://www.isidor.us/unittests/testtm"
                                  :xtm-id *TEST-TM*)

      (elephant:open-store (xml-importer:get-store-spec dir))
      (let ((t57 (get-item-by-id "t57"))
	    (t59 (get-item-by-id "t59"))
	    (t202 (get-item-by-id "t202"))
	    (t58 (get-item-by-id "t58"))
	    (t203 (get-item-by-id "t203"))
	    (t64 (get-item-by-id "t64"))
	    (t62 (get-item-by-id "t62")))
	(let ((association-1 
	       (loop for association in (elephant:get-instances-by-class 'AssociationC)
		  when (and (eq t57 (instance-of association))
			    (eq t59 (instance-of (first (roles association))))
			    (eq t202 (player (first (roles association))))
			    (eq t58 (instance-of (second (roles association))))
			    (eq t203 (player (second (roles association)))))
		  return association))
	      (association-7
	       (identified-construct 
		(elephant:get-instance-by-value 'ItemIdentifierC 'uri
						"http://psi.egovpt.org/itemIdentifiers#assoc_7"))))
	  (let ((association-1-string (to-json-string association-1))
		(json-string
		 (concatenate 'string "{\"itemIdentities\":null,\"type\":[\"http:\\/\\/psi.egovpt.org\\/types\\/isNarrowerSubject\"],\"scopes\":null,\"roles\":[{\"itemIdentities\":null,\"type\":[\"http:\\/\\/psi.egovpt.org\\/types\\/broaderSubject\"],\"topicRef\":[\"http:\\/\\/psi.egovpt.org\\/subject\\/Data\"]},{\"itemIdentities\":null,\"type\":[\"http:\\/\\/psi.egovpt.org\\/types\\/narrowerSubject\"],\"topicRef\":[\"http:\\/\\/psi.egovpt.org\\/subject\\/GeoData\"]}]}")))
	    (is (string= association-1-string json-string)))
	  (let ((association-7-string (to-json-string association-7))
		(json-string
		 (concatenate 'string "{\"itemIdentities\":[\"http:\\/\\/psi.egovpt.org\\/itemIdentifiers#assoc_7\"],\"type\":[\"http:\\/\\/psi.egovpt.org\\/types\\/serviceUsesStandard\"],\"scopes\":null,\"roles\":[{\"itemIdentities\":null,\"type\":[\"http:\\/\\/psi.egovpt.org\\/types\\/ServiceRoleType\"],\"topicRef\":[\"http:\\/\\/psi.egovpt.org\\/service\\/Google+Maps\",\"http:\\/\\/maps.google.com\"]},{\"itemIdentities\":null,\"type\":[\"http:\\/\\/psi.egovpt.org\\/types\\/StandardRoleType\"],\"topicRef\":[\"http:\\/\\/psi.egovpt.org\\/standard\\/ISO+19115%3A+Geographic+Information+-+Metadata\"]}]}")))
	    (is (string= association-7-string json-string)))
	  (elephant:remove-association association-7 'roles (first (roles association-7)))
	  (elephant:remove-association association-7 'roles (first (roles association-7)))
	  (elephant:remove-association association-7 'instance-of t64)
	  (elephant:add-association association-7 'themes t64)
	  (elephant:add-association association-7 'themes t62)
	  (let ((association-7-string (to-json-string association-7))
		(json-string
		 (concatenate 'string "{\"itemIdentities\":[\"http:\\/\\/psi.egovpt.org\\/itemIdentifiers#assoc_7\"],\"type\":null,\"scopes\":[[\"http:\\/\\/psi.egovpt.org\\/types\\/StandardRoleType\"],[\"http:\\/\\/psi.egovpt.org\\/types\\/serviceUsesStandard\"]],\"roles\":null}")))
	    (is (string= association-7-string json-string))))))))


(test test-to-json-string-fragments
  (let
      ((dir "data_base"))
    (with-fixture initialize-destination-db (dir)
      (xml-importer:setup-repository
       *notificationbase.xtm* dir  :tm-id "http://www.isidor.us/unittests/testtm"
                                   :xtm-id *TEST-TM*)

      (elephant:open-store (xml-importer:get-store-spec dir))
      (let ((frag-t100
	     (create-latest-fragment-of-topic
	      "http://psi.egovpt.org/standard/ISO+19115%3A+Geographic+Information+-+Metadata"))
	    (frag-topic
	     (create-latest-fragment-of-topic "http://www.topicmaps.org/xtm/1.0/core.xtm#topic")))
	(let ((frag-t100-string
	       (concatenate 'string "{\"topic\":{\"id\":\"" (d:topicid (d:topic frag-t100)) "\",\"itemIdentities\":[\"http:\\/\\/psi.egovpt.org\\/itemIdentifiers#t100\"],\"subjectLocators\":null,\"subjectIdentifiers\":[\"http:\\/\\/psi.egovpt.org\\/standard\\/ISO+19115%3A+Geographic+Information+-+Metadata\"],\"instanceOfs\":[[\"http:\\/\\/psi.egovpt.org\\/types\\/semanticstandard\"]],\"names\":[{\"itemIdentities\":[\"http:\\/\\/psi.egovpt.org\\/itemIdentifiers#t100_n1\"],\"type\":null,\"scopes\":null,\"value\":\"ISO 19115\",\"variants\":[{\"itemIdentities\":[\"http:\\/\\/psi.egovpt.org\\/itemIdentifiers#t100_n1_v1\"],\"scopes\":[[\"http:\\/\\/www.topicmaps.org\\/xtm\\/1.0\\/core.xtm#display\"]],\"resourceRef\":null,\"resourceData\":{\"datatype\":\"http:\\/\\/www.w3.org\\/2001\\/XMLSchema#string\",\"value\":\"Geographic Information - Metadata\"}},{\"itemIdentities\":[\"http:\\/\\/psi.egovpt.org\\/itemIdentifiers#t100_n1_v2\"],\"scopes\":[[\"http:\\/\\/www.topicmaps.org\\/xtm\\/1.0\\/core.xtm#sort\"]],\"resourceRef\":null,\"resourceData\":{\"datatype\":\"http:\\/\\/www.w3.org\\/2001\\/XMLSchema#string\",\"value\":\"ISO-19115\"}}]}],\"occurrences\":[{\"itemIdentities\":[\"http:\\/\\/psi.egovpt.org\\/itemIdentifiers#t100_o1\"],\"type\":[\"http:\\/\\/psi.egovpt.org\\/types\\/standardHasStatus\"],\"scopes\":null,\"resourceRef\":\"http:\\/\\/www.budabe.de\\/\",\"resourceData\":null},{\"itemIdentities\":[\"http:\\/\\/psi.egovpt.org\\/itemIdentifiers#t100_o2\"],\"type\":[\"http:\\/\\/psi.egovpt.org\\/types\\/description\"],\"scopes\":null,\"resourceRef\":null,\"resourceData\":{\"datatype\":\"http:\\/\\/www.w3.org\\/2001\\/XMLSchema#string\",\"value\":\"The ISO 19115 standard ...\"}},{\"itemIdentities\":[\"http:\\/\\/psi.egovpt.org\\/itemIdentifiers#t100_o3\"],\"type\":[\"http:\\/\\/psi.egovpt.org\\/types\\/standardValidFromDate\"],\"scopes\":null,\"resourceRef\":null,\"resourceData\":{\"datatype\":\"http:\\/\\/www.w3.org\\/2001\\/XMLSchema#date\",\"value\":\"2003-01-01\"}},{\"itemIdentities\":[\"http:\\/\\/psi.egovpt.org\\/itemIdentifiers#t100_o4\"],\"type\":[\"http:\\/\\/psi.egovpt.org\\/types\\/links\"],\"scopes\":null,\"resourceRef\":\"http:\\/\\/www.editeur.org\\/standards\\/ISO19115.pdf\",\"resourceData\":null}]},\"topicStubs\":[{\"id\":\"" (topicid (elt (referenced-topics frag-t100) 0)) "\",\"itemIdentities\":[\"http:\\/\\/psi.egovpt.org\\/itemIdentifiers#t3a\"],\"subjectLocators\":null,\"subjectIdentifiers\":[\"http:\\/\\/psi.egovpt.org\\/types\\/semanticstandard\"]},{\"id\":\"" (topicid (elt (referenced-topics frag-t100) 1)) "\",\"itemIdentities\":null,\"subjectLocators\":null,\"subjectIdentifiers\":[\"http:\\/\\/www.topicmaps.org\\/xtm\\/1.0\\/core.xtm#display\"]},{\"id\":\"" (topicid (elt (referenced-topics frag-t100) 2)) "\",\"itemIdentities\":null,\"subjectLocators\":null,\"subjectIdentifiers\":[\"http:\\/\\/www.topicmaps.org\\/xtm\\/1.0\\/core.xtm#sort\"]},{\"id\":\"" (topicid (elt (referenced-topics frag-t100) 3)) "\",\"itemIdentities\":[\"http:\\/\\/psi.egovpt.org\\/itemIdentifiers#t51\"],\"subjectLocators\":null,\"subjectIdentifiers\":[\"http:\\/\\/psi.egovpt.org\\/types\\/standardHasStatus\"]},{\"id\":\"" (topicid (elt (referenced-topics frag-t100) 4)) "\",\"itemIdentities\":[\"http:\\/\\/psi.egovpt.org\\/itemIdentifiers#t53\"],\"subjectLocators\":null,\"subjectIdentifiers\":[\"http:\\/\\/psi.egovpt.org\\/types\\/description\"]},{\"id\":\"" (topicid (elt (referenced-topics frag-t100) 5)) "\",\"itemIdentities\":[\"http:\\/\\/psi.egovpt.org\\/itemIdentifiers#t54\"],\"subjectLocators\":null,\"subjectIdentifiers\":[\"http:\\/\\/psi.egovpt.org\\/types\\/standardValidFromDate\"]},{\"id\":\"" (topicid (elt (referenced-topics frag-t100) 6)) "\",\"itemIdentities\":[\"http:\\/\\/psi.egovpt.org\\/itemIdentifiers#t55\"],\"subjectLocators\":null,\"subjectIdentifiers\":[\"http:\\/\\/psi.egovpt.org\\/types\\/links\"]},{\"id\":\"" (topicid (elt (referenced-topics frag-t100) 7)) "\",\"itemIdentities\":null,\"subjectLocators\":null,\"subjectIdentifiers\":[\"http:\\/\\/psi.egovpt.org\\/subject\\/GeoData\"]},{\"id\":\"" (topicid (elt (referenced-topics frag-t100) 8)) "\",\"itemIdentities\":[\"http:\\/\\/psi.egovpt.org\\/itemIdentifiers#t60\"],\"subjectLocators\":null,\"subjectIdentifiers\":[\"http:\\/\\/psi.egovpt.org\\/types\\/standardIsAboutSubject\"]},{\"id\":\"" (topicid (elt (referenced-topics frag-t100) 9)) "\",\"itemIdentities\":[\"http:\\/\\/psi.egovpt.org\\/itemIdentifiers#t61\"],\"subjectLocators\":null,\"subjectIdentifiers\":[\"http:\\/\\/psi.egovpt.org\\/types\\/SubjectRoleType\"]},{\"id\":\"" (topicid (elt (referenced-topics frag-t100) 10)) "\",\"itemIdentities\":null,\"subjectLocators\":null,\"subjectIdentifiers\":[\"http:\\/\\/psi.egovpt.org\\/subject\\/Semantic+Description\"]},{\"id\":\"" (topicid (elt (referenced-topics frag-t100) 11)) "\",\"itemIdentities\":[\"http:\\/\\/psi.egovpt.org\\/itemIdentifiers#t64\"],\"subjectLocators\":null,\"subjectIdentifiers\":[\"http:\\/\\/psi.egovpt.org\\/types\\/serviceUsesStandard\"]},{\"id\":\"" (topicid (elt (referenced-topics frag-t100) 12)) "\",\"itemIdentities\":[\"http:\\/\\/psi.egovpt.org\\/itemIdentifiers#t63\"],\"subjectLocators\":null,\"subjectIdentifiers\":[\"http:\\/\\/psi.egovpt.org\\/types\\/ServiceRoleType\"]},{\"id\":\"" (topicid (elt (referenced-topics frag-t100) 13)) "\",\"itemIdentities\":null,\"subjectLocators\":null,\"subjectIdentifiers\":[\"http:\\/\\/psi.egovpt.org\\/service\\/Google+Maps\",\"http:\\/\\/maps.google.com\"]},{\"id\":\"" (topicid (elt (referenced-topics frag-t100) 14)) "\",\"itemIdentities\":[\"http:\\/\\/psi.egovpt.org\\/itemIdentifiers#t62\"],\"subjectLocators\":null,\"subjectIdentifiers\":[\"http:\\/\\/psi.egovpt.org\\/types\\/StandardRoleType\"]}],\"associations\":[{\"itemIdentities\":null,\"type\":[\"http:\\/\\/psi.egovpt.org\\/types\\/standardIsAboutSubject\"],\"scopes\":null,\"roles\":[{\"itemIdentities\":null,\"type\":[\"http:\\/\\/psi.egovpt.org\\/types\\/StandardRoleType\"],\"topicRef\":[\"http:\\/\\/psi.egovpt.org\\/standard\\/ISO+19115%3A+Geographic+Information+-+Metadata\"]},{\"itemIdentities\":null,\"type\":[\"http:\\/\\/psi.egovpt.org\\/types\\/SubjectRoleType\"],\"topicRef\":[\"http:\\/\\/psi.egovpt.org\\/subject\\/GeoData\"]}]},{\"itemIdentities\":null,\"type\":[\"http:\\/\\/psi.egovpt.org\\/types\\/standardIsAboutSubject\"],\"scopes\":null,\"roles\":[{\"itemIdentities\":null,\"type\":[\"http:\\/\\/psi.egovpt.org\\/types\\/StandardRoleType\"],\"topicRef\":[\"http:\\/\\/psi.egovpt.org\\/standard\\/ISO+19115%3A+Geographic+Information+-+Metadata\"]},{\"itemIdentities\":null,\"type\":[\"http:\\/\\/psi.egovpt.org\\/types\\/SubjectRoleType\"],\"topicRef\":[\"http:\\/\\/psi.egovpt.org\\/subject\\/Semantic+Description\"]}]},{\"itemIdentities\":[\"http:\\/\\/psi.egovpt.org\\/itemIdentifiers#assoc_7\"],\"type\":[\"http:\\/\\/psi.egovpt.org\\/types\\/serviceUsesStandard\"],\"scopes\":null,\"roles\":[{\"itemIdentities\":null,\"type\":[\"http:\\/\\/psi.egovpt.org\\/types\\/ServiceRoleType\"],\"topicRef\":[\"http:\\/\\/psi.egovpt.org\\/service\\/Google+Maps\",\"http:\\/\\/maps.google.com\"]},{\"itemIdentities\":null,\"type\":[\"http:\\/\\/psi.egovpt.org\\/types\\/StandardRoleType\"],\"topicRef\":[\"http:\\/\\/psi.egovpt.org\\/standard\\/ISO+19115%3A+Geographic+Information+-+Metadata\"]}]}],\"tmIds\":[\"http:\\/\\/www.isidor.us\\/unittests\\/testtm\"]}"))
	      (frag-topic-string
	       (concatenate 'string "{\"topic\":{\"id\":\"" (topicid (topic frag-topic)) "\",\"itemIdentities\":null,\"subjectLocators\":null,\"subjectIdentifiers\":[\"http:\\/\\/www.topicmaps.org\\/xtm\\/1.0\\/core.xtm#topic\"],\"instanceOfs\":null,\"names\":null,\"occurrences\":null},\"topicStubs\":null,\"associations\":null,\"tmIds\":[\"http:\\/\\/www.topicmaps.org\\/xtm\\/1.0\\/core.xtm\"]}")))
	  (is (string= frag-t100-string (to-json-string frag-t100)))
	  (is (string= frag-topic-string (to-json-string frag-topic))))))))


(test test-get-fragment-values-from-json-list-general
  (let
      ((dir "data_base"))
    (with-fixture initialize-destination-db (dir)
      (xml-importer:setup-repository
       *notificationbase.xtm* dir :tm-id "http://www.isidor.us/unittests/testtm"
       :xtm-id *TEST-TM*)
      
      (elephant:open-store (xml-importer:get-store-spec dir))
      (let ((json-fragment
	     (let ((fragment-obj
		    (create-latest-fragment-of-topic "http://psi.egovpt.org/standard/Topic+Maps+2002")))
	       (to-json-string fragment-obj))))
	(let ((fragment-list
	       (json-importer::get-fragment-values-from-json-list
		(json:decode-json-from-string json-fragment))))
	  (let ((topic (getf fragment-list :topic)))
	    (is (string= (getf topic :ID)
			 (d:topicid
			  (d:identified-construct (elephant:get-instance-by-value 'd:PersistentIdC 'd:uri
										  "http://psi.egovpt.org/standard/Topic+Maps+2002")))))
	    (is-false (getf topic :itemIdentities))
	    (is-false (getf topic :subjectLocators))
	    (is (= (length (getf topic :subjectIdentifiers)) 1))
	    (is (string= (first (getf topic :subjectIdentifiers))
			 "http://psi.egovpt.org/standard/Topic+Maps+2002"))
	            (is (= (length (getf topic :instanceOfs)) 1))
	            (is (= (length (first (getf topic :instanceOfs))) 1))
	            (is (string= (first (first (getf topic :instanceOfs)))
	                         "http://psi.egovpt.org/types/semanticstandard"))))))))


(test test-get-fragment-values-from-json-list-names
  (let
      ((dir "data_base"))
    (with-fixture initialize-destination-db (dir)
      (xml-importer:setup-repository
       *notificationbase.xtm* dir :tm-id "http://www.isidor.us/unittests/testtm"
                                  :xtm-id *TEST-TM*)
      
      (elephant:open-store (xml-importer:get-store-spec dir))
      (let ((json-fragment
	     (let ((fragment-obj
		    (create-latest-fragment-of-topic "http://psi.egovpt.org/standard/Topic+Maps+2002")))
	       (to-json-string fragment-obj))))
	(let ((fragment-list
	       (json-importer::get-fragment-values-from-json-list
		(json:decode-json-from-string json-fragment))))
	  (let ((topic (getf fragment-list :topic)))
	    (is (= (length (getf topic :names)) 2))
	    (let ((name-1 (first (getf topic :names)))
		  (name-2 (second (getf topic :names))))
	      (is-false (getf name-1 :itemIdentities))
	      (is-false (getf name-1 :type))
	      (is-false (getf name-1 :scopes))
	      (is (string= (getf name-1 :value)
			  "Topic Maps 2002"))
	      (is-false (getf name-1 :variants))
	      (is (= (length (getf name-2 :itemIdentities)) 1))
	      (is (string= (first (getf name-2 :itemIdentities))
			   "http://psi.egovpt.org/itemIdentifiers#t101_n2"))
	      (is (= (length (getf name-2 :type)) 1))
	      (is (string= (first (getf name-2 :type))
			   "http://psi.egovpt.org/types/long-name"))
	      (is (= (length (getf name-2 :scopes)) 1))
	      (is (= (length (first (getf name-2 :scopes))) 1))
	      (is (string= (first (first (getf name-2 :scopes)))
			   "http://psi.egovpt.org/types/long-name"))
	      (is (string= (getf name-2 :value)
			   "ISO/IEC 13250:2002: Topic Maps"))
	      (is (= (length (getf name-2 :variants)) 1))
	      (let ((variant (first (getf name-2 :variants))))
		(is (= (length (getf variant :itemIdentities)) 2))
		(is (or (string= (first (getf variant :itemIdentities))
				 "http://psi.egovpt.org/itemIdentifiers#t101_n2_v1")
			(string= (first (getf variant :itemIdentities))
				 "http://psi.egovpt.org/itemIdentifiers#t101_n2_v2")))
		(is (or (string= (second (getf variant :itemIdentities))
				 "http://psi.egovpt.org/itemIdentifiers#t101_n2_v1")
			(string= (second (getf variant :itemIdentities))
				 "http://psi.egovpt.org/itemIdentifiers#t101_n2_v2")))
		(is (= (length (getf variant :scopes)) 2))
		(is (= (length (first (getf variant :scopes))) 1))
		(is (= (length (second (getf variant :scopes))) 1))
		(is (or (string= (first (first (getf variant :scopes)))
				 "http://www.topicmaps.org/xtm/1.0/core.xtm#sort")
			(string= (first (first (getf variant :scopes)))
				 "http://psi.egovpt.org/types/long-name")))
		(is (or (string= (first (second (getf variant :scopes)))
				 "http://www.topicmaps.org/xtm/1.0/core.xtm#sort")
			(string= (first (second (getf variant :scopes)))
				 "http://psi.egovpt.org/types/long-name")))
		(is-false (getf variant :resourceRef))
		(is (string= (getf (getf variant :resourceData) :datatype)
			     "http://www.w3.org/2001/XMLSchema#string"))
		(is (string= (getf (getf variant :resourceData) :value)
			     "ISO/IEC-13250:2002"))))))))))


(test test-get-fragment-values-from-json-list-occurrences
  (let
      ((dir "data_base"))
    (with-fixture initialize-destination-db (dir)
      (xml-importer:setup-repository
       *notificationbase.xtm* dir :tm-id "http://www.isidor.us/unittests/testtm"
                                  :xtm-id *TEST-TM*)
      
      (elephant:open-store (xml-importer:get-store-spec dir))
      (let ((json-fragment
	     (let ((fragment-obj
		    (create-latest-fragment-of-topic "http://psi.egovpt.org/standard/Topic+Maps+2002")))
	       (to-json-string fragment-obj))))
	(let ((fragment-list
	       (json-importer::get-fragment-values-from-json-list
		(json:decode-json-from-string json-fragment))))
	  (let ((topic (getf fragment-list :topic)))
	    (is (= (length (getf topic :occurrences)) 4))
	    (let ((occurrence-1 (first (getf topic :occurrences)))
		  (occurrence-2 (second (getf topic :occurrences)))
		  (occurrence-3 (third (getf topic :occurrences)))
		  (occurrence-4 (fourth (getf topic :occurrences)))
		  (ref-topic
		   (d:identified-construct
		    (elephant:get-instance-by-value 'd:PersistentIdC 'd:uri
						    "http://psi.egovpt.org/status/InternationalStandard"))))
	      (is-false (getf occurrence-1 :itemIdentities))
	      (is (= (length (getf occurrence-1 :type)) 1))
	      (is (string= (first (getf occurrence-1 :type))
			   "http://psi.egovpt.org/types/standardHasStatus"))
	      (is-false (getf occurrence-1 :scopes))
	      (is (string= (getf occurrence-1 :resourceRef)
			   (concatenate 'string "#" (d:topicid ref-topic))))
	      (is-false (getf occurrence-1 :resourceData))
	      (is-false (getf occurrence-2 :itemIdentities))
	      (is (= (length (getf occurrence-2 :type)) 1))
	      (is (string= (first (getf occurrence-2 :type))
			   "http://psi.egovpt.org/types/description"))
	      (is-false (getf occurrence-2 :scopes))
	      (is-false (getf occurrence-2 :resourceRef))
	      (is (string= (getf (getf occurrence-2 :resourceData) :datatype)
			   "http://www.w3.org/2001/XMLSchema#string"))
	      (is-true (getf (getf occurrence-2 :resourceData) :value))
	      (is-false (getf occurrence-3 :itemIdentities))
	      (is (= (length (getf occurrence-3 :type)) 1))
	      (is (string= (first (getf occurrence-3 :type))
			   "http://psi.egovpt.org/types/standardValidFromDate"))
	      (is-false (getf occurrence-3 :scopes))
	      (is-false (getf occurrence-3 :resourceRef))
	      (is (string= (getf (getf occurrence-3 :resourceData) :datatype)
			   "//www.w3.org/2001/XMLSchema#date"))
	      (is (string= (getf (getf occurrence-3 :resourceData) :value)
			   "2002-05-19"))
	      (is-false (getf occurrence-4 :itemIdentities))
	      (is (= (length (getf occurrence-4 :type)) 1))
	      (is (string= (first (getf occurrence-4 :type))
			   "http://psi.egovpt.org/types/links"))
	      (is-false (getf occurrence-4 :scopes))
	      (is (string= (getf occurrence-4 :resourceRef)
			   "http://www1.y12.doe.gov/capabilities/sgml/sc34/document/0322_files/iso13250-2nd-ed-v2.pdf"))
	      (is-false (getf occurrence-4 :resourceData)))))))))


(test test-get-fragment-values-from-json-list-topicStubs
  (let
      ((dir "data_base"))
    (with-fixture initialize-destination-db (dir)
      (xml-importer:setup-repository
       *notificationbase.xtm* dir :tm-id "http://www.isidor.us/unittests/testtm"
                                  :xtm-id *TEST-TM*)
      
      (elephant:open-store (xml-importer:get-store-spec dir))
      (let ((json-fragment
	     (let ((fragment-obj
		    (create-latest-fragment-of-topic "http://psi.egovpt.org/standard/Topic+Maps+2002")))
	       (to-json-string fragment-obj))))
	(let ((fragment-list
	       (json-importer::get-fragment-values-from-json-list
		(json:decode-json-from-string json-fragment))))
	  (let ((topicStubs (getf fragment-list :topicStubs)))		
	    (is (= (length topicStubs) 15))
	    (loop for topicStub in topicStubs
	       do (let ((id (getf topicStub :ID))
			(itemIdentities (getf topicStub :itemIdentities))
			(subjectLocators (getf topicStub :subjectLocators))
			(subjectIdentifiers (getf topicStub :subjectIdentifiers)))
		    (is (= (length subjectIdentifiers) 1))
		    (let ((subjectIdentifier
			   (first subjectIdentifiers)))
		      (let ((topic
			     (d:identified-construct
			      (elephant:get-instance-by-value 'd:PersistentIdC 'd:uri
							      subjectIdentifier))))
			(is-true topic)
			(is-false subjectLocators)
			(is (string= (d:topicid topic) id))
			(cond
			  ((string= subjectIdentifier "http://psi.egovpt.org/types/semanticstandard")
			   (is (= (length itemIdentities) 1))
			   (is (string= (first itemIdentities)
					"http://psi.egovpt.org/itemIdentifiers#t3a")))
			  ((string= subjectIdentifier "http://www.topicmaps.org/xtm/1.0/core.xtm#sort")
			   (is-false itemIdentities))
			  ((string= subjectIdentifier "http://psi.egovpt.org/types/long-name")
			   (is (= (length itemIdentities) 1))
			   (is (string= (first itemIdentities)
					"http://psi.egovpt.org/itemIdentifiers#t50a")))
			  ((string= subjectIdentifier "http://psi.egovpt.org/types/standardHasStatus")
			   (is (= (length itemIdentities) 1))
			   (is (string= (first itemIdentities)
					"http://psi.egovpt.org/itemIdentifiers#t51")))
			  ((string= subjectIdentifier "http://psi.egovpt.org/types/description")
			   (is (= (length itemIdentities) 1))
			   (is (string= (first itemIdentities)
					"http://psi.egovpt.org/itemIdentifiers#t53")))
			  ((string= subjectIdentifier "http://psi.egovpt.org/types/standardValidFromDate")
			   (is (= (length itemIdentities) 1))
			   (is (string= (first itemIdentities)
					"http://psi.egovpt.org/itemIdentifiers#t54")))
			  ((string= subjectIdentifier "http://psi.egovpt.org/types/links")
			   (is (= (length itemIdentities) 1))
			   (is (string= (first itemIdentities)
					"http://psi.egovpt.org/itemIdentifiers#t55")))
			  ((string= subjectIdentifier "http://psi.egovpt.org/types/standardIsAboutSubject")
			   (is (= (length itemIdentities) 1))
			   (is (string= (first itemIdentities)
					"http://psi.egovpt.org/itemIdentifiers#t60")))
			  ((string= subjectIdentifier "http://psi.egovpt.org/types/SubjectRoleType")
			   (is (= (length itemIdentities) 1))
			   (is (string= (first itemIdentities)
					"http://psi.egovpt.org/itemIdentifiers#t61")))
			  ((string= subjectIdentifier "http://psi.egovpt.org/subject/Semantic+Description")
			   (is-false itemIdentities))
			  ((string= subjectIdentifier "http://psi.egovpt.org/types/serviceUsesStandard")
			   (is (= (length itemIdentities) 1))
			   (is (string= (first itemIdentities)
					"http://psi.egovpt.org/itemIdentifiers#t64")))
			  ((string= subjectIdentifier "http://psi.egovpt.org/types/ServiceRoleType")
			   (is (= (length itemIdentities) 1))
			   (is (string= (first itemIdentities)
					"http://psi.egovpt.org/itemIdentifiers#t63")))
			  ((string= subjectIdentifier "http://psi.egovpt.org/service/Norwegian+National+Curriculum")
			   (is-false itemIdentities))
			  ((string= subjectIdentifier "http://psi.egovpt.org/types/StandardRoleType")
			   (is (= (length itemIdentities) 1))
			   (is (string= (first itemIdentities)
					"http://psi.egovpt.org/itemIdentifiers#t62")))
			  ((string= subjectIdentifier "http://psi.egovpt.org/status/InternationalStandard")
			   (is (= (length itemIdentities) 1))
			   (is (string= (first itemIdentities)
					"http://psi.egovpt.org/itemIdentifiers#t52")))
			  (t
			   (is-true (format t "bad subjectIdentifier found in topicStubs"))))))))))))))



(test test-get-fragment-values-from-json-list-associations
  (let
      ((dir "data_base"))
    (with-fixture initialize-destination-db (dir)
      (xml-importer:setup-repository
       *notificationbase.xtm* dir :tm-id "http://www.isidor.us/unittests/testtm"
                                  :xtm-id *TEST-TM*)
      
      (elephant:open-store (xml-importer:get-store-spec dir))
      (let ((json-fragment
	     (let ((fragment-obj
		    (create-latest-fragment-of-topic "http://psi.egovpt.org/standard/Topic+Maps+2002")))
	       (to-json-string fragment-obj))))
	(let ((fragment-list
	       (json-importer::get-fragment-values-from-json-list
		(json:decode-json-from-string json-fragment))))
	  (let ((f-associations (getf fragment-list :associations)))
	    (is (= (length f-associations) 2))
	    (is (= (length (getf (first f-associations) :type)) 1))
	    (is (= (length (getf (second f-associations) :type)) 1))
	    (let ((association-1
		   (if (string= (first (getf (first f-associations) :type))
				"http://psi.egovpt.org/types/standardIsAboutSubject")
		       (first f-associations)
		       (second f-associations)))
		  (association-2
		   (if (string= (first (getf (first f-associations) :type))
				"http://psi.egovpt.org/types/serviceUsesStandard")
		       (first f-associations)
		       (second f-associations))))
	      (is-true association-1)
	      (is-true association-2)
	      (is-false (getf association-1 :itemIdentities))
	      (is-false (getf association-1 :scopes))
	      (is (= (length (getf association-1 :roles)) 2))
	      (let ((role-1 (first (getf association-1 :roles)))
		    (role-2 (second (getf association-1 :roles))))
		(is-false (getf role-1 :itemIdentities))
		(is (= (length (getf role-1 :type))))
		(is (string= (first (getf role-1 :type))
			     "http://psi.egovpt.org/types/StandardRoleType"))
		(is (= (length (getf role-1 :topicRef)) 1))
		(is (string= (first (getf role-1 :topicRef))
			     "http://psi.egovpt.org/standard/Topic+Maps+2002"))
		(is-false (getf role-2 :itemIdentities))
		(is (= (length (getf role-2 :itemIdentities))))
		(is (string= (first (getf role-2 :type))
			     "http://psi.egovpt.org/types/SubjectRoleType"))
		(is (= (length (getf role-2 :topicRef)) 1))
		(is (string= (first (getf role-2 :topicRef))
			     "http://psi.egovpt.org/subject/Semantic+Description")))
	      (is-false (getf association-2 :itemIdentities))
	      (is-false (getf association-2 :scopes))
	      (is (= (length (getf association-2 :roles)) 2))
	      (let ((role-1 (first (getf association-2 :roles)))
		    (role-2 (second (getf association-2 :roles))))
		(is-false (getf role-1 :itemIdentities))
		(is (= (length (getf role-1 :type))))
		(is (string= (first (getf role-1 :type))
			     "http://psi.egovpt.org/types/ServiceRoleType"))
		(is (= (length (getf role-1 :topicRef)) 1))
		(is (string= (first (getf role-1 :topicRef))
			     "http://psi.egovpt.org/service/Norwegian+National+Curriculum"))
		(is-false (getf role-2 :itemIdentities))
		(is (= (length (getf role-2 :itemIdentities))))
		(is (string= (first (getf role-2 :type))
			     "http://psi.egovpt.org/types/StandardRoleType"))
		(is (= (length (getf role-2 :topicRef)) 1))
		(is (string= (first (getf role-2 :topicRef))
			     "http://psi.egovpt.org/standard/Topic+Maps+2002"))))))))))


(test test-json-importer-general-1
  (let
      ((dir "data_base"))
    (with-fixture initialize-destination-db (dir)
      (elephant:open-store (xml-importer:get-store-spec dir))
      (xml-importer:init-isidorus)
      (elephant:open-store (xml-importer:get-store-spec dir)) ; init-isiodurs closes the store
      (is (= (length (elephant:get-instances-by-class 'TopicC)) 13))
      (is (= (length (elephant:get-instances-by-class 'AssociationC)) 0))
      (is (= (length (elephant:get-instances-by-class 'TopicMapC)) 1))
      (json-importer:json-to-elem *t64*)
      (is (= (length (elephant:get-instances-by-class 'TopicC)) 15))
      (is (= (length (elephant:get-instances-by-class 'AssociationC)) 1))
      (is (= (length (elephant:get-instances-by-class 'TopicMapC)) 2))
      (let ((core-tm
	     (loop for tm in (elephant:get-instances-by-class 'TopicMapC)
		when (string= (uri (first (item-identifiers tm)))
			      "http://www.topicmaps.org/xtm/1.0/core.xtm")
		return tm))	      
	    (test-tm
	     (loop for tm in (elephant:get-instances-by-class 'TopicMapC)
		when (string= (uri (first (item-identifiers tm)))
			      "http://www.isidor.us/unittests/testtm")
		return tm)))
	(is-true (and core-tm test-tm))
	(is (= (length (topics core-tm)) 13))
	(is (= (length (associations core-tm)) 0))
	(is (= (length (topics test-tm)) 2))
	(is (= (length (associations test-tm)) 1))))))


(test test-json-importer-general-2
  (let
      ((dir "data_base"))
    (with-fixture initialize-destination-db (dir)
      (elephant:open-store (xml-importer:get-store-spec dir))
      (xml-importer:init-isidorus)
      (elephant:open-store (xml-importer:get-store-spec dir)) ; init-isiodurs closes the store
      (json-importer:json-to-elem *t64*)
      (let ((test-tm
	       (loop for tm in (elephant:get-instances-by-class 'TopicMapC)
		  when (string= (uri (first (item-identifiers tm)))
				"http://www.isidor.us/unittests/testtm")
		  return tm)))
	(let ((main-topic
	       (loop for topic in (topics test-tm)
		  when (string= (uri (first (psis topic)))
				"http://psi.egovpt.org/types/serviceUsesStandard")
		  return topic))
	      (sub-topic
	       (loop for topic in (topics test-tm)
		  when (string= (uri (first (psis topic)))
				"http://www.networkedplanet.com/psi/npcl/meta-types/association-type")
		  return topic)))
	  (is-true (and main-topic sub-topic))
	  (let ((instanceOf-assoc
		 (first (associations test-tm))))
	    (is (string= (uri (first (psis (instance-of instanceOf-assoc))))
			 constants::*type-instance-psi*))
	    (is-false (d:themes instanceOf-assoc))
	    (is (string= (d:uri (first (d:item-identifiers (first (d:in-topicmaps instanceOf-assoc)))))
			 "http://www.isidor.us/unittests/testtm"))
	    (is-false (d:item-identifiers instanceOf-assoc))
	    (let ((super-type-role
		   (loop for role in (roles instanceOf-assoc)
		      when (string= (uri (first (psis (instance-of role))))
				    constants:*type-psi*)
		      return role))
		  (sub-type-role
		   (loop for role in (roles instanceOf-assoc)
		      when (string= (uri (first (psis (instance-of role))))
				    constants:*instance-psi*)
		      return role)))
	      (is-true (and super-type-role sub-type-role))
	      (is (string= (uri (first (psis (player super-type-role))))
			   "http://www.networkedplanet.com/psi/npcl/meta-types/association-type"))
	      (is (string= (uri (first (psis (player sub-type-role))))
			   "http://psi.egovpt.org/types/serviceUsesStandard"))))
	  (is-true (= (length (item-identifiers main-topic)) 1))
	  (is-true (= (length (item-identifiers sub-topic)) 1))
	  (is-true (string= (uri (first (item-identifiers main-topic)))
			    "http://psi.egovpt.org/itemIdentifiers#t64"))
	  (is-true (string= (uri (first (item-identifiers sub-topic)))
			    "http://psi.egovpt.org/itemIdentifiers#t7"))
	  (is-true (= (length (names main-topic)) 1))
	  (is-true (string= (charvalue (first (names main-topic)))
			    "service uses standard")))))))


(test test-json-importer-general-3
  (let
      ((dir "data_base"))
    (with-fixture initialize-destination-db (dir)
      (elephant:open-store (xml-importer:get-store-spec dir))
      (xml-importer:init-isidorus)
      (elephant:open-store (xml-importer:get-store-spec dir)) ; init-isiodurs closes the store
      (json-importer:json-to-elem *t64*)
      (json-importer:json-to-elem *t100-3*)
      (is (= (length (elephant:get-instances-by-class 'TopicC)) 28)) ;13 new topics
      (is (= (length (elephant:get-instances-by-class 'AssociationC)) 5)) ;4 new associations
      (is (= (length (elephant:get-instances-by-class 'TopicMapC)) 2))
      (let ((core-tm
	     (loop for tm in (elephant:get-instances-by-class 'TopicMapC)
		when (string= (uri (first (item-identifiers tm)))
			      "http://www.topicmaps.org/xtm/1.0/core.xtm")
		return tm))	      
	    (test-tm
	     (loop for tm in (elephant:get-instances-by-class 'TopicMapC)
		when (string= (uri (first (item-identifiers tm)))
			      "http://www.isidor.us/unittests/testtm")
		return tm)))
	(is-true (and core-tm test-tm))
	(is (= (length (topics core-tm)) 13))
	(is (= (length (associations core-tm)) 0))
	(is (= (length (topics test-tm)) 17))
	(is (= (length (associations test-tm)) 5))))))


(test test-json-importer-topics-1
  (let
      ((dir "data_base"))
    (with-fixture initialize-destination-db (dir)
      (elephant:open-store (xml-importer:get-store-spec dir))
      (xml-importer:init-isidorus)
      (elephant:open-store (xml-importer:get-store-spec dir)) ; init-isiodurs closes the store
      (json-importer:json-to-elem *t64*)
      (json-importer:json-to-elem *t100-3*)
      (let ((topics (elephant:get-instances-by-class 'TopicC)))
	(loop for topic in topics
	   do (let ((psi (uri (first (psis topic)))))
		(cond
		  ((string= psi "http://psi.egovpt.org/types/semanticstandard") ;t3a
		   (is-false (names topic))
		   (is-false (occurrences topic))
		   (is-false (locators topic))
		   (is (= (length (psis topic)) 1))
		   (is (= (length (item-identifiers topic)) 1))
		   (is (string= (uri (first (item-identifiers topic)))
				"http://psi.egovpt.org/itemIdentifiers#t3a")))
		  ((string= psi "http://www.networkedplanet.com/psi/npcl/meta-types/association-type") ;t7
		   (is-false (names topic))
		   (is-false (occurrences topic))
		   (is-false (locators topic))
		   (is (= (length (psis topic)) 1))
		   (is (= (length (item-identifiers topic)) 1))
		   (is (string= (uri (first (item-identifiers topic)))
				"http://psi.egovpt.org/itemIdentifiers#t7")))
		  ((string= psi "http://psi.egovpt.org/types/standardHasStatus") ;t51
		   (is-false (names topic))
		   (is-false (occurrences topic))
		   (is-false (locators topic))
		   (is (= (length (psis topic)) 1))
		   (is (= (length (item-identifiers topic)) 1))
		   (is (string= (uri (first (item-identifiers topic)))
				"http://psi.egovpt.org/itemIdentifiers#t51")))
		  ((string= psi "http://psi.egovpt.org/types/description") ;t53
		   (is-false (names topic))
		   (is-false (occurrences topic))
		   (is-false (locators topic))
		   (is (= (length (psis topic)) 1))
		   (is (= (length (item-identifiers topic)) 1))
		   (is (string= (uri (first (item-identifiers topic)))
				"http://psi.egovpt.org/itemIdentifiers#t53")))
		  ((string= psi "http://psi.egovpt.org/types/standardValidFromDate") ;t54
		       (is-false (names topic))
		       (is-false (occurrences topic))
		       (is-false (locators topic))
		       (is (= (length (psis topic)) 1))
		       (is (= (length (item-identifiers topic)) 1))
		       (is (string= (uri (first (item-identifiers topic)))
				    "http://psi.egovpt.org/itemIdentifiers#t54"))))))))))


(test test-json-importer-topics-2
  (let
      ((dir "data_base"))
    (with-fixture initialize-destination-db (dir)
      (elephant:open-store (xml-importer:get-store-spec dir))
      (xml-importer:init-isidorus)
      (elephant:open-store (xml-importer:get-store-spec dir)) ; init-isiodurs closes the store
      (json-importer:json-to-elem *t64*)
      (json-importer:json-to-elem *t100-3*)
      (let ((topics (elephant:get-instances-by-class 'TopicC)))
	(loop for topic in topics
	   do (let ((psi (uri (first (psis topic)))))
		(cond ((string= psi "http://psi.egovpt.org/types/links") ;t55
		       (is-false (names topic))
		       (is-false (occurrences topic))
		       (is-false (locators topic))
		       (is (= (length (psis topic)) 1))
		       (is (= (length (item-identifiers topic)) 1))
		       (is (string= (uri (first (item-identifiers topic)))
				  "http://psi.egovpt.org/itemIdentifiers#t55")))
		      ((string= psi "http://psi.egovpt.org/types/standardIsAboutSubject") ;t60
		       (is-false (names topic))
		       (is-false (occurrences topic))
		       (is-false (locators topic))
		       (is (= (length (psis topic)) 1))
		       (is (= (length (item-identifiers topic)) 1))
		       (is (string= (uri (first (item-identifiers topic)))
				    "http://psi.egovpt.org/itemIdentifiers#t60")))
		      ((string= psi "http://psi.egovpt.org/types/SubjectRoleType") ;t61
		       (is-false (names topic))
		       (is-false (occurrences topic))
		       (is-false (locators topic))
		       (is (= (length (psis topic)) 1))
		       (is (= (length (item-identifiers topic)) 1))
		       (is (string= (uri (first (item-identifiers topic)))
				    "http://psi.egovpt.org/itemIdentifiers#t61")))
		      ((string= psi "http://psi.egovpt.org/types/StandardRoleType") ;t62
		       (is-false (names topic))
		       (is-false (occurrences topic))
		       (is-false (locators topic))
		       (is (= (length (psis topic)) 1))
		       (is (= (length (item-identifiers topic)) 1))
		       (is (string= (uri (first (item-identifiers topic)))
				    "http://psi.egovpt.org/itemIdentifiers#t62")))
		      ((string= psi "http://psi.egovpt.org/types/ServiceRoleType") ;t63
		       (is-false (names topic))
		       (is-false (occurrences topic))
		       (is-false (locators topic))
		       (is (= (length (psis topic)) 1))
		       (is (= (length (item-identifiers topic)) 1))
		       (is (string= (uri (first (item-identifiers topic)))
				    "http://psi.egovpt.org/itemIdentifiers#t63")))
		      ((string= psi "http://psi.egovpt.org/types/serviceUsesStandard") ;t64
		       (is (= (length (names topic)) 1))
		       (is (string= (charvalue (first (names topic)))
				    "service uses standard"))
		       (is-false (occurrences topic))
		       (is-false (locators topic))
		       (is (= (length (psis topic)) 1))
		       (is (= (length (item-identifiers topic)) 1))
		       (is (string= (uri (first (item-identifiers topic)))
				    "http://psi.egovpt.org/itemIdentifiers#t64"))))))))))


(test test-json-importer-topics-3
  (let
      ((dir "data_base"))
    (with-fixture initialize-destination-db (dir)
      (elephant:open-store (xml-importer:get-store-spec dir))
      (xml-importer:init-isidorus)
      (elephant:open-store (xml-importer:get-store-spec dir)) ; init-isiodurs closes the store
      (json-importer:json-to-elem *t64*)
      (json-importer:json-to-elem *t100-3*)
      (let ((topics (elephant:get-instances-by-class 'TopicC)))
	(loop for topic in topics
	   do (let ((psi (uri (first (psis topic)))))
		(cond ((string= psi "http://psi.egovpt.org/standard/ISO+19115%3A+Geographic+Information+-+Metadata") ;t100
		       (is (= (length (psis topic)) 1))
		       (is (= (length (item-identifiers topic)) 1))
		       (is (string= (uri (first (item-identifiers topic)))
				    "http://psi.egovpt.org/itemIdentifiers#t100"))
		       (is (= (length (names topic)) 1))
		       (is (string= (charvalue (first (names topic)))
				    "ISO 19115"))
		       (is (= (length (item-identifiers (first (names topic))))))
		       (is (string= (uri (first (item-identifiers (first (names topic)))))
				    "http://psi.egovpt.org/itemIdentifiers#t100_n1"))
		       (is (= (length (variants (first (names topic)))) 2))
		       (let ((variant-1 (first (variants (first (names topic)))))
			     (variant-2 (second (variants (first (names topic))))))
			 (is (= (length (item-identifiers variant-1)) 1))
			 (is (string= (uri (first (item-identifiers variant-1)))
				      "http://psi.egovpt.org/itemIdentifiers#t100_n1_v1"))
			 (is (= (length (item-identifiers variant-2)) 1))
			 (is (string= (uri (first (item-identifiers variant-2)))
				      "http://psi.egovpt.org/itemIdentifiers#t100_n1_v2"))
			 (is (= (length (themes variant-1)) 1))
			 (is (string= (uri (first (psis (first (themes variant-1)))))
				      "http://www.topicmaps.org/xtm/1.0/core.xtm#display"))
			 (is (= (length (themes variant-2)) 1))
			 (is (string= (uri (first (psis (first (themes variant-2)))))
				      "http://www.topicmaps.org/xtm/1.0/core.xtm#sort"))
			 (is (string= (charvalue variant-1)
				      "Geographic Information - Metadata"))
			 (is (string= (datatype variant-1)
				      "http://www.w3.org/2001/XMLSchema#string"))
			 (is (string= (charvalue variant-2)
				      "ISO-19115"))
			 (is (string= (datatype variant-2)
				      "http://www.w3.org/2001/XMLSchema#string")))
		       (is (= (length (occurrences topic)) 4))
		       (let ((occ-1 (first (occurrences topic)))
			     (occ-2 (second (occurrences topic)))
			     (occ-3 (third (occurrences topic)))
			     (occ-4 (fourth (occurrences topic))))
			 (is (= (length (item-identifiers occ-1)) 1))
			 (is (string= (uri (first (item-identifiers occ-1)))
				      "http://psi.egovpt.org/itemIdentifiers#t100_o1"))
			 (is (= (length (item-identifiers occ-2)) 1))
			 (is (string= (uri (first (item-identifiers occ-2)))
				      "http://psi.egovpt.org/itemIdentifiers#t100_o2"))
			 (is (= (length (item-identifiers occ-3)) 1))
			 (is (string= (uri (first (item-identifiers occ-3)))
				      "http://psi.egovpt.org/itemIdentifiers#t100_o3"))
			 (is (= (length (item-identifiers occ-4)) 1))
			 (is (string= (uri (first (item-identifiers occ-4)))
				      "http://psi.egovpt.org/itemIdentifiers#t100_o4"))
			 (is (string= (uri (first (psis (instance-of occ-1))))
				      "http://psi.egovpt.org/types/standardHasStatus"))
			 (is (string= (uri (first (psis (instance-of occ-2))))
				      "http://psi.egovpt.org/types/description"))
			 (is (string= (uri (first (psis (instance-of occ-3))))
				      "http://psi.egovpt.org/types/standardValidFromDate"))
			 (is (string= (uri (first (psis (instance-of occ-4))))
				      "http://psi.egovpt.org/types/links"))
			 (is (string= (datatype occ-1)
				      "http://www.w3.org/2001/XMLSchema#anyURI"))
			 (is (string= (charvalue occ-1)
				      "http://www.budabe.de/"))
			 (is (string= (datatype occ-2)
				      "http://www.w3.org/2001/XMLSchema#string"))
			 (is (string= (charvalue occ-2)
				      "The ISO 19115 standard ..."))
			 (is (string= (datatype occ-3)
				      "http://www.w3.org/2001/XMLSchema#date"))
			 (is (string= (charvalue occ-3)
				      "2003-01-01"))
			 (is (string= (datatype occ-4)
				      "http://www.w3.org/2001/XMLSchema#anyURI"))
			 (is (string= (charvalue occ-4)
				      "http://www.editeur.org/standards/ISO19115.pdf")))))))))))


(test test-json-importer-topics-4
  (let
      ((dir "data_base"))
    (with-fixture initialize-destination-db (dir)
      (elephant:open-store (xml-importer:get-store-spec dir))
      (xml-importer:init-isidorus)
      (elephant:open-store (xml-importer:get-store-spec dir)) ; init-isiodurs closes the store
      (json-importer:json-to-elem *t64*)
      (json-importer:json-to-elem *t100-3*)
      (let ((topics (elephant:get-instances-by-class 'TopicC)))
	(loop for topic in topics
	   do (let ((psi (uri (first (psis topic)))))
		(cond ((string= psi "http://psi.egovpt.org/subject/Semantic+Description") ;t201
		       (is-false (names topic))
		       (is-false (occurrences topic))
		       (is-false (locators topic))
		       (is (= (length (psis topic)) 1))
		       (is-false (item-identifiers topic)))
		      ((string= psi "http://psi.egovpt.org/subject/GeoData") ;t203
		       (is-false (names topic))
		       (is-false (occurrences topic))
		       (is-false (locators topic))
		       (is (= (length (psis topic)) 1))
		       (is-false (item-identifiers topic)))
		      ((or (string= psi "http://psi.egovpt.org/service/Google+Maps") ;t301a
			   (string= psi "http://maps.google.com"))
		       (is-false (names topic))
		       (is-false (occurrences topic))
		       (is-false (locators topic))
		       (is (= (length (psis topic)) 2))
		       (is (or (string= (uri (first (psis topic)))
					"http://psi.egovpt.org/service/Google+Maps")
			       (string= (uri (first (psis topic)))
					"http://maps.google.com")))
		       (is (or (string= (uri (second (psis topic)))
					"http://psi.egovpt.org/service/Google+Maps")
			       (string= (uri (second (psis topic)))
					"http://maps.google.com")))
		       (is-false (item-identifiers topic))))))))))
		      

(test test-json-importer-associations
  (let
      ((dir "data_base"))
    (with-fixture initialize-destination-db (dir)
      (elephant:open-store (xml-importer:get-store-spec dir))
      (xml-importer:init-isidorus)
      (elephant:open-store (xml-importer:get-store-spec dir)) ; init-isiodurs closes the store
      (json-importer:json-to-elem *t64*)
      (json-importer:json-to-elem *t100-3*)
      (let ((assoc-7
	     (identified-construct
	      (elephant:get-instance-by-value 'ItemidentifierC 'uri
					      "http://psi.egovpt.org/itemIdentifiers#assoc_7"))))
	(is (= (length (item-identifiers assoc-7))))
	(is (string= (uri (first (item-identifiers assoc-7)))
		     "http://psi.egovpt.org/itemIdentifiers#assoc_7"))
	(is (= (length (roles assoc-7)) 2))
	(is (string= (uri (first (psis (instance-of assoc-7))))
		     "http://psi.egovpt.org/types/serviceUsesStandard"))
	(let ((role-1 (first (roles assoc-7)))
	      (role-2 (second (roles assoc-7))))
	  (is (string= (uri (first (psis (instance-of role-1))))
		       "http://psi.egovpt.org/types/ServiceRoleType"))
	  (is (or (string= (uri (first (psis (player role-1))))
			   "http://psi.egovpt.org/service/Google+Maps")
		  (string= (uri (first (psis (player role-1))))
			   "http://maps.google.com")))
	  (is (string= (uri (first (psis (instance-of role-2))))
		       "http://psi.egovpt.org/types/StandardRoleType"))
	  (is (string= (uri (first (psis (player role-2))))
		       "http://psi.egovpt.org/standard/ISO+19115%3A+Geographic+Information+-+Metadata")))))))


(test test-json-importer-merge-1
  (let
      ((dir "data_base"))
    (with-fixture initialize-destination-db (dir)
      (elephant:open-store (xml-importer:get-store-spec dir))
      (xml-importer:init-isidorus)
      (elephant:open-store (xml-importer:get-store-spec dir)) ; init-isidorus closes the store
      (is (= (length (elephant:get-instances-by-class 'TopicC)) 13))
      (is (= (length (elephant:get-instances-by-class 'AssociationC)) 0))
      (is (= (length (elephant:get-instances-by-class 'TopicMapC)) 1))
      (json-importer:json-to-elem *t100-1*)
      (is (= (length (elephant:get-instances-by-class 'TopicC)) 17))
      (is (= (length (elephant:get-instances-by-class 'AssociationC)) 1))
      (is (= (length (elephant:get-instances-by-class 'TopicMapC)) 2))
      (let ((core-tm
	     (loop for tm in (elephant:get-instances-by-class 'TopicMapC)
		when (string= (uri (first (item-identifiers tm)))
			      "http://www.topicmaps.org/xtm/1.0/core.xtm")
		return tm))	      
	    (test-tm
	     (loop for tm in (elephant:get-instances-by-class 'TopicMapC)
		when (string= (uri (first (item-identifiers tm)))
			      "http://www.isidor.us/unittests/testtm")
		return tm)))
	(is-true (and core-tm test-tm)))
      (json-importer:json-to-elem *t100-2*)
      (is (= (length (elephant:get-instances-by-class 'TopicC)) 17))
      (is (= (length (elephant:get-instances-by-class 'AssociationC)) 1))
      (is (= (length (elephant:get-instances-by-class 'TopicMapC)) 2))
      (let ((core-tm
	     (loop for tm in (elephant:get-instances-by-class 'TopicMapC)
		when (string= (uri (first (item-identifiers tm)))
			      "http://www.topicmaps.org/xtm/1.0/core.xtm")
		return tm))	      
	    (test-tm
	     (loop for tm in (elephant:get-instances-by-class 'TopicMapC)
		when (string= (uri (first (item-identifiers tm)))
			      "http://www.isidor.us/unittests/testtm")
		return tm)))
	(is-true (and core-tm test-tm)))
      (let ((topics (elephant:get-instances-by-class 'TopicC)))
	(loop for topic in topics
	   do (let ((psi (uri (first (psis topic)))))
		(cond
		  ((string= psi "http://psi.egovpt.org/types/standard") ;t3
		   (is (= (length (in-topicmaps topic)) 1))
		   (is (string= (uri (first (item-identifiers (first (in-topicmaps topic)))))
				"http://www.isidor.us/unittests/testtm"))
		   (is-false (names topic))
		   (is-false (occurrences topic))
		   (is-false (locators topic))
		   (is (= (length (psis topic)) 1))
		   (is (= (length (item-identifiers topic)) 2))
		   (is (or (string= (uri (first (item-identifiers topic)))
				    "http://www.egovpt.org/itemIdentifiers#t3")
			   (string= (uri (second (item-identifiers topic)))
				    "http://www.egovpt.org/itemIdentifiers#t3")))
		   (is (or (string= (uri (first (item-identifiers topic)))
				    "http://psi.egovpt.org/itemIdentifiers#t3")
			   (string= (uri (second (item-identifiers topic)))
				    "http://psi.egovpt.org/itemIdentifiers#t3"))))
		  ((string= psi "http://psi.egovpt.org/types/long-name") ;t50a
		   (is (= (length (in-topicmaps topic)) 1))
		   (is (string= (uri (first (item-identifiers (first (in-topicmaps topic)))))
				"http://www.isidor.us/unittests/testtm"))
		   (is-false (names topic))
		   (is-false (occurrences topic))
		   (is-false (locators topic))
		   (is (= (length (psis topic)) 1))
		   (is (= (length (item-identifiers topic)) 1))
		   (is (string= (uri (first (item-identifiers topic)))
				"http://psi.egovpt.org/itemIdentifiers#t50a")))
		  ((string= psi "http://psi.egovpt.org/types/links") ;t50
		   (is (= (length (in-topicmaps topic)) 1))
		   (is (string= (uri (first (item-identifiers (first (in-topicmaps topic)))))
				"http://www.isidor.us/unittests/testtm"))
		   (is-false (names topic))
		   (is-false (occurrences topic))
		   (is-false (locators topic))
		   (is (= (length (psis topic)) 1))
		   (is (= (length (item-identifiers topic)) 2))
		   (is (or (string= (uri (first (item-identifiers topic)))
				    "http://psi.egovpt.org/itemIdentifiers#t55")
			   (string= (uri (second (item-identifiers topic)))
				    "http://psi.egovpt.org/itemIdentifiers#t55")))
		   (is (or (string= (uri (first (item-identifiers topic)))
				    "http://psi.egovpt.org/itemIdentifiers#t55_1")
			   (string= (uri (second (item-identifiers topic)))
				    "http://psi.egovpt.org/itemIdentifiers#t55_1")))))))))))


(test test-json-importer-merge-2
  (let
      ((dir "data_base"))
    (with-fixture initialize-destination-db (dir)
      (elephant:open-store (xml-importer:get-store-spec dir))
      (xml-importer:init-isidorus)
      (elephant:open-store (xml-importer:get-store-spec dir)) ; init-isidorus closes the store
      (json-importer:json-to-elem *t100-1*)
      (let ((core-tm
	     (loop for tm in (elephant:get-instances-by-class 'TopicMapC)
		when (string= (uri (first (item-identifiers tm)))
			      "http://www.topicmaps.org/xtm/1.0/core.xtm")
		return tm))	      
	    (test-tm
	     (loop for tm in (elephant:get-instances-by-class 'TopicMapC)
		when (string= (uri (first (item-identifiers tm)))
			      "http://www.isidor.us/unittests/testtm")
		return tm)))
	(is-true (and core-tm test-tm)))
      (json-importer:json-to-elem *t100-2*)
      (let ((topics (elephant:get-instances-by-class 'TopicC)))
	(loop for topic in topics
	   do (let ((psi (uri (first (psis topic)))))
		(cond
		  ((string= psi "http://psi.egovpt.org/types/standard") t) ;was already checked
		  ((string= psi "http://psi.egovpt.org/types/long-name") t) ;was already checked
		  ((string= psi "http://psi.egovpt.org/types/links") t) ;was already checked
		  ((string= psi "http://psi.egovpt.org/standard/Common+Lisp") ;t100
		   (is (= (length (in-topicmaps topic)) 1))
		   (is (string= (uri (first (item-identifiers (first (in-topicmaps topic)))))
				"http://www.isidor.us/unittests/testtm"))
		   (is (= (length (psis topic)) 1))
		   (is (= (length (item-identifiers topic)) 2))
		   (is (or (string= (uri (first (item-identifiers topic)))
				    "http://www.egovpt.org/itemIdentifiers#t100")
			   (string= (uri (second (item-identifiers topic)))
				    "http://www.egovpt.org/itemIdentifiers#t100")))
		   (is (or (string= (uri (first (item-identifiers topic)))
				    "http://www.egovpt.org/itemIdentifiers#t100_new")
			   (string= (uri (second (item-identifiers topic)))
				    "http://www.egovpt.org/itemIdentifiers#t100_new")))
		   (is (= (length (names topic))))
		   (let ((name (first (names topic))))
		     (is (= (length (item-identifiers name)) 2))
		     (is (or (string= (uri (first (item-identifiers name)))
				      "http://www.egovpt.org/itemIdentifiers#t100_n1")
			     (string= (uri (second (item-identifiers name)))
				      "http://www.egovpt.org/itemIdentifiers#t100_n1")))
		     (is (or (string= (uri (first (item-identifiers name)))
				      "http://www.egovpt.org/itemIdentifiers#t100_n1a")
			     (string= (uri (second (item-identifiers name)))
				      "http://www.egovpt.org/itemIdentifiers#t100_n1a")))
		     (is (string= (charvalue name)
				  "Common Lisp"))
		     (is (= (length (variants name)) 2))
		     (let ((variant-1 (first (variants name)))
			   (variant-2 (second (variants name))))
		       (is (= (length (item-identifiers variant-1)) 1))
		       (is (string= (uri (first (item-identifiers variant-1)))
				    "http://www.egovpt.org/itemIdentifiers#t100_n_v1"))
		       (is (= (length (item-identifiers variant-2)) 1))
		       (is (string= (uri (first (item-identifiers variant-2)))
				    "http://www.egovpt.org/itemIdentifiers#t100_n_v2"))
		       (is (= (length (themes variant-1)) 2))
		       (is (or (string= (uri (first (psis (first (themes variant-1)))))
					"http://www.topicmaps.org/xtm/1.0/core.xtm#sort")
			       (string= (uri (first (psis (second (themes variant-1)))))
					"http://www.topicmaps.org/xtm/1.0/core.xtm#sort")))
		       (is (or (string= (uri (first (psis (first (themes variant-1)))))
					"http://psi.egovpt.org/types/long-name")
			       (string= (uri (first (psis (second (themes variant-1)))))
					"http://psi.egovpt.org/types/long-name")))
		       (is (= (length (themes variant-2)) 1))
		       (is (string= (uri (first (psis (first (themes variant-2)))))
				    "http://www.topicmaps.org/xtm/1.0/core.xtm#display"))
		       (is (string= (datatype variant-1)
				    "http://www.w3.org/2001/XMLSchema#string"))
		       (is (string= (charvalue variant-1)
				    "Common-Lisp"))
		       (is (string= (datatype variant-2)
				    "http://www.w3.org/2001/XMLSchema#string"))
		       (is (string= (charvalue variant-2)
				    "CL"))))
		   (is (= (length (occurrences topic)) 2))
		   (let ((occ-1 (first (occurrences topic)))
			 (occ-2 (second (occurrences topic))))
		     (is (= (length (item-identifiers occ-1)) 1))
		     (is (string= (uri (first (item-identifiers occ-1)))
				  "http://www.egovpt.org/itemIdentifiers#t100_o1"))
		     (is (= (length (item-identifiers occ-2)) 1))
		     (is (string= (uri (first (item-identifiers occ-2)))
				  "http://www.egovpt.org/itemIdentifiers#t100_o2"))
		     (is (string= (uri (first (psis (instance-of occ-1))))
				  "http://psi.egovpt.org/types/links"))
		     (is (string= (uri (first (psis (instance-of occ-2))))
				  "http://psi.egovpt.org/types/links"))
		     (is (string= (datatype occ-1)
				  "http://www.w3.org/2001/XMLSchema#anyURI"))
		     (is (string= (charvalue occ-1)
				  "http://www.common-lisp.net/"))
		     (is (string= (datatype occ-2)
				  "http://www.w3.org/2001/XMLSchema#anyURI"))
		     (is (string= (charvalue occ-2)
				  "http://www.cliki.net/"))))
		  (t
		   (if (or (string= psi "http://www.topicmaps.org/xtm/1.0/core.xtm#sort")
			   (string= psi "http://www.topicmaps.org/xtm/1.0/core.xtm#display"))
		       (progn
			 (is (= (length (in-topicmaps topic)) 2))
			 (is (or (string= (uri (first (item-identifiers (first (in-topicmaps topic)))))
					  "http://www.topicmaps.org/xtm/1.0/core.xtm")
				 (string= (uri (first (item-identifiers (second (in-topicmaps topic)))))
					  "http://www.topicmaps.org/xtm/1.0/core.xtm")))
			 (is (or (string= (uri (first (item-identifiers (first (in-topicmaps topic)))))
					  "http://www.isidor.us/unittests/testtm")
				 (string= (uri (first (item-identifiers (second (in-topicmaps topic)))))
					  "http://www.isidor.us/unittests/testtm"))))
		       (progn
			 (is (= (length (in-topicmaps topic)) 1))
			 (is (string= (uri (first (item-identifiers (first (in-topicmaps topic)))))
				      "http://www.topicmaps.org/xtm/1.0/core.xtm"))))))))))))


(test test-json-importer-merge-3
  (let
      ((dir "data_base"))
    (with-fixture initialize-destination-db (dir)
      (elephant:open-store (xml-importer:get-store-spec dir))
      (xml-importer:init-isidorus)
      (elephant:open-store (xml-importer:get-store-spec dir)) ; init-isidorus closes the store
      (json-importer:json-to-elem *t100-1*)
      (let ((core-tm
	     (loop for tm in (elephant:get-instances-by-class 'TopicMapC)
		when (string= (uri (first (item-identifiers tm)))
			      "http://www.topicmaps.org/xtm/1.0/core.xtm")
		return tm))	      
	    (test-tm
	     (loop for tm in (elephant:get-instances-by-class 'TopicMapC)
		when (string= (uri (first (item-identifiers tm)))
			      "http://www.isidor.us/unittests/testtm")
		return tm)))
	(is-true (and core-tm test-tm)))
      (json-importer:json-to-elem *t100-2*)
      (let ((instanceOf-assoc
	     (first (elephant:get-instances-by-class 'AssociationC))))
	(is (string= (uri (first (psis (instance-of instanceOf-assoc))))
		     constants::*type-instance-psi*))
	(is-false (d:themes instanceOf-assoc))
	(is (string= (d:uri (first (d:item-identifiers (first (d:in-topicmaps instanceOf-assoc)))))
		     "http://www.isidor.us/unittests/testtm"))
	(is-false (d:item-identifiers instanceOf-assoc))
	(let ((super-type-role
	       (loop for role in (roles instanceOf-assoc)
		  when (string= (uri (first (psis (instance-of role))))
				constants:*type-psi*)
		  return role))
	      (sub-type-role
	       (loop for role in (roles instanceOf-assoc)
		  when (string= (uri (first (psis (instance-of role))))
				constants:*instance-psi*)
		  return role)))
	  (is-true (and super-type-role sub-type-role))
	  (is (string= (uri (first (psis (player super-type-role))))
		       "http://psi.egovpt.org/types/standard"))
	  (is (string= (uri (first (psis (player sub-type-role))))
		       "http://psi.egovpt.org/standard/Common+Lisp")))))))


(test test-get-all-topic-psis
  (let
      ((dir "data_base"))
    (with-fixture initialize-destination-db (dir)
      (xml-importer:setup-repository
       *notificationbase.xtm* dir :tm-id "http://www.isidor.us/unittests/testtm" :xtm-id *TEST-TM*)

      (elephant:open-store (xml-importer:get-store-spec dir))
      (let ((json-psis (json:decode-json-from-string (get-all-topic-psis))))
	(is (= (length json-psis) (length (elephant:get-instances-by-class 'd:TopicC))))
	(loop for topic-psis in json-psis
	   do (cond
		((string= (first topic-psis) "http://www.topicmaps.org/xtm/1.0/core.xtm#topic")
		 (is (= (length topic-psis) 1)))
		((string= (first topic-psis) "http://www.topicmaps.org/xtm/1.0/core.xtm#association")
		 (is (= (length topic-psis) 1)))
		((string= (first topic-psis) "http://www.topicmaps.org/xtm/1.0/core.xtm#occurrence")
		 (is (= (length topic-psis) 1)))
		((string= (first topic-psis) "http://www.topicmaps.org/xtm/1.0/core.xtm#class-instance")
		 (is (= (length topic-psis) 1)))
		((string= (first topic-psis) "http://www.topicmaps.org/xtm/1.0/core.xtm#class")
		 (is (= (length topic-psis) 1)))
		((string= (first topic-psis) "http://www.topicmaps.org/xtm/1.0/core.xtm#superclass-subclass")
		 (is (= (length topic-psis) 1)))
		((string= (first topic-psis) "http://www.topicmaps.org/xtm/1.0/core.xtm#superclass")
		 (is (= (length topic-psis) 1)))
		((string= (first topic-psis) "http://www.topicmaps.org/xtm/1.0/core.xtm#subclass")
		 (is (= (length topic-psis) 1)))
		((string= (first topic-psis) "http://www.topicmaps.org/xtm/1.0/core.xtm#sort")
		 (is (= (length topic-psis) 1)))
		((string= (first topic-psis) "http://www.topicmaps.org/xtm/1.0/core.xtm#display")
		 (is (= (length topic-psis) 1)))
		((string= (first topic-psis) "http://psi.topicmaps.org/iso13250/model/type-instance")
		 (is (= (length topic-psis) 1)))
		((string= (first topic-psis) "http://psi.topicmaps.org/iso13250/model/type")
		 (is (= (length topic-psis) 1)))
		((string= (first topic-psis) "http://psi.topicmaps.org/iso13250/model/instance")
		 (is (= (length topic-psis) 1)))
		((string= (first topic-psis) "http://www.networkedplanet.com/psi/npcl/meta-types/topic-type")
		 (is (= (length topic-psis) 1)))
		((string= (first topic-psis) "http://psi.egovpt.org/types/service")
		 (is (= (length topic-psis) 1)))
		((string= (first topic-psis) "http://psi.egovpt.org/types/standard")
		 (is (= (length topic-psis) 1)))
		((string= (first topic-psis) "http://psi.egovpt.org/types/semanticstandard")
		 (is (= (length topic-psis) 1)))
		((string= (first topic-psis) "http://psi.egovpt.org/types/technicalstandard")
		 (is (= (length topic-psis) 1)))
		((string= (first topic-psis) "http://psi.egovpt.org/types/subject")
		 (is (= (length topic-psis) 1)))
		((string= (first topic-psis) "http://www.networkedplanet.com/psi/npcl/meta-types/occurrence-type")
		 (is (= (length topic-psis) 1)))
		((string= (first topic-psis) "http://www.networkedplanet.com/psi/npcl/meta-types/association-type")
		 (is (= (length topic-psis) 1)))
		((string= (first topic-psis) "http://www.networkedplanet.com/psi/npcl/meta-types/association-role-type")
		 (is (= (length topic-psis) 1)))
		((string= (first topic-psis) "http://psi.egovpt.org/types/topicInTaxonomy")
		 (is (= (length topic-psis) 1)))
		((string= (first topic-psis) "http://psi.egovpt.org/types/long-name")
		 (is (= (length topic-psis) 1)))
		((string= (first topic-psis) "http://psi.egovpt.org/types/standardHasStatus")
		 (is (= (length topic-psis) 1)))
		((string= (first topic-psis) "http://psi.egovpt.org/status/InternationalStandard")
		 (is (= (length topic-psis) 1)))
		((string= (first topic-psis) "http://psi.egovpt.org/types/description")
		 (is (= (length topic-psis) 1)))
		((string= (first topic-psis) "http://psi.egovpt.org/types/standardValidFromDate")
		 (is (= (length topic-psis) 1)))
		((string= (first topic-psis) "http://psi.egovpt.org/types/links")
		 (is (= (length topic-psis) 1)))
		((string= (first topic-psis) "http://psi.egovpt.org/types/topicIsAboutSubject")
		 (is (= (length topic-psis) 1)))
		((string= (first topic-psis) "http://psi.egovpt.org/types/isNarrowerSubject")
		 (is (= (length topic-psis) 1)))
		((string= (first topic-psis) "http://psi.egovpt.org/types/narrowerSubject")
		 (is (= (length topic-psis) 1)))
		((string= (first topic-psis) "http://psi.egovpt.org/types/broaderSubject")
		 (is (= (length topic-psis) 1)))
		((string= (first topic-psis) "http://psi.egovpt.org/types/standardIsAboutSubject")
		 (is (= (length topic-psis) 1)))
		((string= (first topic-psis) "http://psi.egovpt.org/types/SubjectRoleType")
		 (is (= (length topic-psis) 1)))
		((string= (first topic-psis) "http://psi.egovpt.org/types/StandardRoleType")
		 (is (= (length topic-psis) 1)))
		((string= (first topic-psis) "http://psi.egovpt.org/types/ServiceRoleType")
		 (is (= (length topic-psis) 1)))
		((string= (first topic-psis) "http://psi.egovpt.org/types/serviceUsesStandard")
		 (is (= (length topic-psis) 1)))
		((string= (first topic-psis) "http://psi.egovpt.org/standard/ISO+19115%3A+Geographic+Information+-+Metadata")
		 (is (= (length topic-psis) 1)))
		((string= (first topic-psis) "http://psi.egovpt.org/standard/Topic+Maps+2002")
		 (is (= (length topic-psis) 1)))
		((string= (first topic-psis) "http://psi.egovpt.org/subject/Web+Services")
		 (is (= (length topic-psis) 1)))
		((string= (first topic-psis) "http://psi.egovpt.org/subject/Semantic+Description")
		 (is (= (length topic-psis) 1)))
		((string= (first topic-psis) "http://psi.egovpt.org/subject/Data")
		 (is (= (length topic-psis) 1)))
		((string= (first topic-psis) "http://psi.egovpt.org/subject/GeoData")
		 (is (= (length topic-psis) 1)))
		((string= (first topic-psis) "http://psi.egovpt.org/subject/Legal+Data")
		 (is (= (length topic-psis) 1)))
		((string= (first topic-psis) "http://psi.egovpt.org/service/Norwegian+National+Curriculum")
		 (is (= (length topic-psis) 1)))
		((or (string= (first topic-psis) "http://psi.egovpt.org/service/Google+Maps")
		     (string= (first topic-psis) "http://maps.google.com")
		     (is (= (length topic-psis) 2))
		     (is (or (string= (second topic-psis) "http://psi.egovpt.org/service/Google+Maps")
			     (string= (second topic-psis) "http://maps.google.com")))))
		(t
		 (is-true (format t "found bad topic-psis: ~a" topic-psis)))))))))


(defun run-json-tests()
  (tear-down-test-db)
  (it.bese.fiveam:run! 'test-get-fragment-values-from-json-list-general)
  (it.bese.fiveam:run! 'test-get-fragment-values-from-json-list-names)
  (it.bese.fiveam:run! 'test-get-fragment-values-from-json-list-occurrences)
  (it.bese.fiveam:run! 'test-get-fragment-values-from-json-list-topicStubs)
  (it.bese.fiveam:run! 'test-get-fragment-values-from-json-list-associations)
  (it.bese.fiveam:run! 'test-json-importer-general-1)
  (it.bese.fiveam:run! 'test-json-importer-general-2)
  (it.bese.fiveam:run! 'test-json-importer-general-3)
  (it.bese.fiveam:run! 'test-json-importer-topics-1)
  (it.bese.fiveam:run! 'test-json-importer-topics-2)
  (it.bese.fiveam:run! 'test-json-importer-topics-3)
  (it.bese.fiveam:run! 'test-json-importer-topics-4)
  (it.bese.fiveam:run! 'test-json-importer-associations)
  (it.bese.fiveam:run! 'test-json-importer-merge-1)
  (it.bese.fiveam:run! 'test-json-importer-merge-2)
  (it.bese.fiveam:run! 'test-json-importer-merge-3)
  (it.bese.fiveam:run! 'test-to-json-string-associations)
  (it.bese.fiveam:run! 'test-to-json-string-fragments)
  (it.bese.fiveam:run! 'test-to-json-string-topics)
  (it.bese.fiveam:run! 'test-get-all-topic-psis))
