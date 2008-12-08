(defpackage :json-test
  (:use 
   :common-lisp
   :xml-importer
   :json-exporter
   :datamodel
   :it.bese.FiveAM
   :unittests-constants
   :fixtures)
  (:export :test-to-json-string-topics
	   :test-to-json-string-associations
	   :run-json-tests))


(in-package :json-test)


(def-suite json-tests
     :description "tests various functions of the json module")

(in-suite json-tests)


(test test-to-json-string-topics
  (let
      ((dir "data_base"))
    (with-fixture initialize-destination-db (dir)
      (xml-importer:setup-repository
       *notificationbase.xtm* dir :xtm-id *TEST-TM*)

      (elephant:open-store (xml-importer:get-store-spec dir))
      (let ((t50a (get-item-by-id "t50a")))
	(let ((t50a-string (to-json-string t50a))
	      (json-string 
	       (concatenate 'string "{\"id\":\"" (topicid t50a) "\",\"itemIdentities\":[\"http://psi.egovpt.org/itemIdentifiers#t50a\"],\"subjectLocators\":null,\"subjectIdentifiers\":[\"http://psi.egovpt.org/types/long-name\"],\"instanceOfs\":[[\"http://www.networkedplanet.com/psi/npcl/meta-types/occurrence-type\"]],\"names\":[{\"itemIdentities\":null,\"type\":null,\"scopes\":null,\"value\":\"long version of a name\",\"variants\":[{\"itemIdentities\":null,\"scopes\":[[\"http://www.topicmaps.org/xtm/1.0/core.xtm#sort\"]],\"resourceRef\":null,\"resourceData\":{\"datatype\":\"http://www.w3.org/2001/XMLSchema#string\",\"value\":\"Long-Version\"}}]}],\"occurrences\":null}" )))
	  (is (string= t50a-string json-string)))
	(let ((t8 (get-item-by-id "t8")))
	  (let ((t8-string (to-json-string t8))
		(json-string 
		 (concatenate 'string "{\"id\":\"" (topicid t8) "\",\"itemIdentities\":[\"http://psi.egovpt.org/itemIdentifiers#t8\"],\"subjectLocators\":null,\"subjectIdentifiers\":[\"http://www.networkedplanet.com/psi/npcl/meta-types/association-role-type\"],\"instanceOfs\":[[\"http://www.networkedplanet.com/psi/npcl/meta-types/topic-type\"]],\"names\":[{\"itemIdentities\":null,\"type\":null,\"scopes\":null,\"value\":\"Association Role Type\",\"variants\":null}],\"occurrences\":null}")))
	    (is (string= t8-string json-string))))
	(let ((t-topic (get-item-by-id "topic" :xtm-id "core.xtm")))
	  (let ((t-topic-string (to-json-string t-topic))
		(json-string
		 (concatenate 'string "{\"id\":\"" (topicid t-topic) "\",\"itemIdentities\":null,\"subjectLocators\":null,\"subjectIdentifiers\":[\"http://www.topicmaps.org/xtm/1.0/core.xtm#topic\"],\"instanceOfs\":null,\"names\":null,\"occurrences\":null}")))
	    (is (string= t-topic-string json-string))))
	(let ((t301 (get-item-by-id "t301")))
	  (let ((t301-string (to-json-string t301))
		(json-string
		 (concatenate 'string "{\"id\":\"" (topicid t301) "\",\"itemIdentities\":null,\"subjectLocators\":null,\"subjectIdentifiers\":[\"http://psi.egovpt.org/service/Google+Maps\",\"http://maps.google.com\"],\"instanceOfs\":[[\"http://psi.egovpt.org/types/service\"]],\"names\":[{\"itemIdentities\":[\"http://psi.egovpt.org/topic/t301a_n1\"],\"type\":null,\"scopes\":[[\"http://psi.egovpt.org/types/long-name\"]],\"value\":\"Google Maps\",\"variants\":null},{\"itemIdentities\":null,\"type\":null,\"scopes\":[[\"http://psi.egovpt.org/types/long-name\"]],\"value\":\"Google Maps Application\",\"variants\":null}],\"occurrences\":[{\"itemIdentities\":null,\"type\":[\"http://psi.egovpt.org/types/description\"],\"scopes\":null,\"resourceRef\":null,\"resourceData\":{\"datatype\":\"http://www.w3.org/2001/XMLSchema#string\",\"value\":\"a popular geodata service that is widely used for mashups with geodataProbably not really conformant to ISO 19115, but who cares in this context.\"}},{\"itemIdentities\":null,\"type\":[\"http://psi.egovpt.org/types/links\"],\"scopes\":null,\"resourceRef\":\"http://maps.google.com\",\"resourceData\":null},{\"itemIdentities\":null,\"type\":[\"http://psi.egovpt.org/types/links\"],\"scopes\":null,\"resourceRef\":\"http://maps.google.de\",\"resourceData\":null}]}")))
	    (is (string= t301-string json-string))))
	(let ((t100 (get-item-by-id "t100")))
	  (let ((t100-string (to-json-string t100))
		(json-string
		 (concatenate 'string "{\"id\":\"" (topicid t100) "\",\"itemIdentities\":[\"http://psi.egovpt.org/itemIdentifiers#t100\"],\"subjectLocators\":null,\"subjectIdentifiers\":[\"http://psi.egovpt.org/standard/ISO+19115%3A+Geographic+Information+-+Metadata\"],\"instanceOfs\":[[\"http://psi.egovpt.org/types/semanticstandard\"]],\"names\":[{\"itemIdentities\":[\"http://psi.egovpt.org/itemIdentifiers#t100_n1\"],\"type\":null,\"scopes\":null,\"value\":\"ISO 19115\",\"variants\":[{\"itemIdentities\":[\"http://psi.egovpt.org/itemIdentifiers#t100_n1_v1\"],\"scopes\":[[\"http://www.topicmaps.org/xtm/1.0/core.xtm#display\"]],\"resourceRef\":null,\"resourceData\":{\"datatype\":\"http://www.w3.org/2001/XMLSchema#string\",\"value\":\"Geographic Information - Metadata\"}},{\"itemIdentities\":[\"http://psi.egovpt.org/itemIdentifiers#t100_n1_v2\"],\"scopes\":[[\"http://www.topicmaps.org/xtm/1.0/core.xtm#sort\"]],\"resourceRef\":null,\"resourceData\":{\"datatype\":\"http://www.w3.org/2001/XMLSchema#string\",\"value\":\"ISO-19115\"}}]}],\"occurrences\":[{\"itemIdentities\":[\"http://psi.egovpt.org/itemIdentifiers#t100_o1\"],\"type\":[\"http://psi.egovpt.org/types/standardHasStatus\"],\"scopes\":null,\"resourceRef\":\"http://www.budabe.de/\",\"resourceData\":null},{\"itemIdentities\":[\"http://psi.egovpt.org/itemIdentifiers#t100_o2\"],\"type\":[\"http://psi.egovpt.org/types/description\"],\"scopes\":null,\"resourceRef\":null,\"resourceData\":{\"datatype\":\"http://www.w3.org/2001/XMLSchema#string\",\"value\":\"The ISO 19115 standard ...\"}},{\"itemIdentities\":[\"http://psi.egovpt.org/itemIdentifiers#t100_o3\"],\"type\":[\"http://psi.egovpt.org/types/standardValidFromDate\"],\"scopes\":null,\"resourceRef\":null,\"resourceData\":{\"datatype\":\"http://www.w3.org/2001/XMLSchema#date\",\"value\":\"2003-01-01\"}},{\"itemIdentities\":[\"http://psi.egovpt.org/itemIdentifiers#t100_o4\"],\"type\":[\"http://psi.egovpt.org/types/links\"],\"scopes\":null,\"resourceRef\":\"http://www.editeur.org/standards/ISO19115.pdf\",\"resourceData\":null}]}")))
	    (is (string= t100-string json-string))))))))


(test test-to-json-string-associations
  (let
      ((dir "data_base"))
    (with-fixture initialize-destination-db (dir)
      (xml-importer:setup-repository
       *notificationbase.xtm* dir :xtm-id *TEST-TM*)

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
		 (concatenate 'string "{\"itemIdentities\":null,\"type\":[\"http://psi.egovpt.org/types/isNarrowerSubject\"],\"scopes\":null,\"roles\":[{\"itemIdentities\":null,\"type\":[\"http://psi.egovpt.org/types/broaderSubject\"],\"topicRef\":[\"http://psi.egovpt.org/subject/Data\"]},{\"itemIdentities\":null,\"type\":[\"http://psi.egovpt.org/types/narrowerSubject\"],\"topicRef\":[\"http://psi.egovpt.org/subject/GeoData\"]}]}")))
	    (is (string= association-1-string json-string)))
	  (let ((association-7-string (to-json-string association-7))
		(json-string
		 (concatenate 'string "{\"itemIdentities\":[\"http://psi.egovpt.org/itemIdentifiers#assoc_7\"],\"type\":[\"http://psi.egovpt.org/types/serviceUsesStandard\"],\"scopes\":null,\"roles\":[{\"itemIdentities\":null,\"type\":[\"http://psi.egovpt.org/types/ServiceRoleType\"],\"topicRef\":[\"http://psi.egovpt.org/service/Google+Maps\",\"http://maps.google.com\"]},{\"itemIdentities\":null,\"type\":[\"http://psi.egovpt.org/types/StandardRoleType\"],\"topicRef\":[\"http://psi.egovpt.org/standard/ISO+19115%3A+Geographic+Information+-+Metadata\"]}]}")))
	    (is (string= association-7-string json-string)))
	  (elephant:remove-association association-7 'roles (first (roles association-7)))
	  (elephant:remove-association association-7 'roles (first (roles association-7)))
	  (elephant:remove-association association-7 'instance-of t64)
	  (elephant:add-association association-7 'themes t64)
	  (elephant:add-association association-7 'themes t62)
	  (let ((association-7-string (to-json-string association-7))
		(json-string
		 (concatenate 'string "{\"itemIdentities\":[\"http://psi.egovpt.org/itemIdentifiers#assoc_7\"],\"type\":null,\"scopes\":[\"" (topicid t62) "\",\"" (topicid t64) "\"],\"roles\":null}")))
	    (is (string= association-7-string json-string))))))))


(defun run-json-tests()
  (tear-down-test-db)
  (run! 'json-tests))