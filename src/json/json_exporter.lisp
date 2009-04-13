;;+-----------------------------------------------------------------------------
;;+  Isidorus
;;+  (c) 2008-2009 Marc Kuester, Christoph Ludwig, Lukas Giessmann
;;+
;;+  Isidorus is freely distributable under the LGPL license.
;;+  You can find a detailed description in trunk/docs/LGPL-LICENSE.txt.
;;+-----------------------------------------------------------------------------


(defpackage :json-exporter
  (:use :cl :json :datamodel)
  (:export :to-json-string
	   :get-all-topic-psis
	   :to-json-string-summary
	   :make-topic-summary))

(in-package :json-exporter)

;; the json schema for our datamodel is in ".../docs/xtm_json.txt"


;; =============================================================================
;; --- main json data model ----------------------------------------------------
;; =============================================================================
(defgeneric to-json-string (instance &key xtm-id)
  (:documentation "converts the Topic Map construct instance to a json string"))


(defun identifiers-to-json-string (parent-construct &key (what 'd:psis))
  "returns the identifiers of a TopicMapConstructC as a json list"
  (when (and parent-construct
	     (or (eql what 'psis) (eql what 'item-identifiers) (eql what 'locators)))
    (let ((items 
	   (map 'list #'uri (funcall what parent-construct))))
      (declare (TopicMapConstructC parent-construct)) ;must be a topic for psis and locators
      (json:encode-json-to-string items))))


(defun resourceX-to-json-string (value datatype &key (xtm-id d:*current-xtm*))
  "returns a resourceRef and resourceData json object"
  ;(declare (string value datatype))
  (if (string= datatype "http://www.w3.org/2001/XMLSchema#anyURI")
      (concatenate 'string "\"resourceRef\":"		   
		   (let ((inner-value
			  (let ((ref-topic (when (and (> (length value) 0)
						      (eql (elt value 0) #\#))
					     (get-item-by-id (subseq value 1) :xtm-id xtm-id))))
			    (if ref-topic
				(concatenate 'string "#" (topicid ref-topic))
				value))))
		           (json:encode-json-to-string inner-value))
		           ",\"resourceData\":null")
      (concatenate 'string "\"resourceRef\":null,"
		           "\"resourceData\":{\"datatype\":"
			   (json:encode-json-to-string datatype)
			   ",\"value\":"
			   (json:encode-json-to-string value) "}")))


(defun ref-topics-to-json-string (topics)
  "returns a json string of all psi-uris of the passed topics as a list of lists"
  (if topics
      (let ((psis (json:encode-json-to-string
		   (map 'list #'(lambda(topic)
				  (declare (topicC topic))
				  (map 'list #'uri (psis topic)))
			topics))))
	(declare (list topics))
	psis)
      "null"))


(defun type-to-json-string (parent-elem)
  "returns a json string of the type of the passed parent-elem"
  (declare (TypableC parent-elem))
  (concatenate 'string "\"type\":"
	       (if (slot-boundp parent-elem 'instance-of)
		   (json:encode-json-to-string (map 'list #'uri (psis (instance-of parent-elem))))
		   "null")))


(defmethod to-json-string ((instance VariantC) &key (xtm-id d:*current-xtm*))
  "transforms a VariantC object to a json string"
  (let ((itemIdentity
	 (concatenate 'string "\"itemIdentities\":"
		      (identifiers-to-json-string instance :what 'item-identifiers)))
	(scope
	 (concatenate 'string "\"scopes\":" (ref-topics-to-json-string (themes instance))))
	(resourceX
	 (let ((value
		(when (slot-boundp instance 'charvalue)
		  (charvalue instance)))
	       (type
		(when (slot-boundp instance 'datatype)
		  (datatype instance))))
	   (resourceX-to-json-string value type :xtm-id xtm-id))))
    (concatenate 'string "{" itemIdentity "," scope "," resourceX "}")))


(defmethod to-json-string ((instance NameC) &key (xtm-id d:*current-xtm*))
  "transforms a NameC object to a json string"
  (let ((itemIdentity
	 (concatenate 'string "\"itemIdentities\":"
		      (identifiers-to-json-string instance :what 'item-identifiers)))
	(type
	 (type-to-json-string instance))
	(scope
	 (concatenate 'string "\"scopes\":" (ref-topics-to-json-string (themes instance))))
	(value
	 (concatenate 'string "\"value\":"
		      (if (slot-boundp instance 'charvalue)
			  (json:encode-json-to-string (charvalue instance))
			  "null")))
	(variant
	 (if (variants instance)
	     (concatenate 'string "\"variants\":"
			  (let ((j-variants "["))
			    (loop for variant in (variants instance)
			       do (setf j-variants
					(concatenate 'string j-variants
						     (json-exporter::to-json-string variant :xtm-id xtm-id) ",")))
			    (concatenate 'string (subseq j-variants 0 (- (length j-variants) 1)) "]")))
	     (concatenate 'string "\"variants\":null"))))
    (concatenate 'string "{" itemIdentity "," type "," scope "," value "," variant "}")))


(defmethod to-json-string ((instance OccurrenceC) &key (xtm-id d:*current-xtm*))
  "transforms an OccurrenceC object to a json string"
  (let ((itemIdentity
	 (concatenate 'string "\"itemIdentities\":"
		      (identifiers-to-json-string instance :what 'item-identifiers)))
	(type
	 (type-to-json-string instance))
	(scope
	 (concatenate 'string "\"scopes\":" (ref-topics-to-json-string (themes instance))))
	(resourceX
	 (let ((value
		(when (slot-boundp instance 'charvalue)
		  (charvalue instance)))
	       (type
		(when (slot-boundp instance 'datatype)
		  (datatype instance))))
	   (resourceX-to-json-string value type :xtm-id xtm-id))))
    (concatenate 'string "{" itemIdentity "," type "," scope "," resourceX "}")))


(defmethod to-json-string ((instance TopicC) &key (xtm-id d:*current-xtm*))
  "transforms an TopicC object to a json string"
  (let ((id
	 (concatenate 'string "\"id\":" (json:encode-json-to-string (topicid instance))))
	(itemIdentity
	 (concatenate 'string "\"itemIdentities\":"
		      (identifiers-to-json-string instance :what 'item-identifiers)))
	(subjectLocator 
	 (concatenate 'string "\"subjectLocators\":"
		      (identifiers-to-json-string instance :what 'locators)))
	(subjectIdentifier
	 (concatenate 'string "\"subjectIdentifiers\":"
		      (identifiers-to-json-string instance :what 'psis)))
	(instanceOf
	 (concatenate 'string "\"instanceOfs\":" (ref-topics-to-json-string (list-instanceOf instance))))
	(name
	 (concatenate 'string "\"names\":"
		      (if (names instance)
			  (let ((j-names "["))
			    (loop for item in (names instance)
			       do (setf j-names
					(concatenate 'string j-names (to-json-string item :xtm-id xtm-id) ",")))
			    (concatenate 'string (subseq j-names 0 (- (length j-names) 1)) "]"))
			  "null")))
	(occurrence
	 (concatenate 'string "\"occurrences\":"
		      (if (occurrences instance)
			  (let ((j-occurrences "["))
			    (loop for item in (occurrences instance)
			       do (setf j-occurrences
					(concatenate 'string j-occurrences (to-json-string item :xtm-id xtm-id) ",")))
			    (concatenate 'string (subseq j-occurrences 0 (- (length j-occurrences) 1)) "]"))
			  "null"))))
    (concatenate 'string "{" id "," itemIdentity "," subjectLocator "," subjectIdentifier ","
		 instanceOf "," name "," occurrence "}")))


(defun to-json-topicStub-string (topic)
  "transforms the passed TopicC object to a topic stub
   string in the json format, which contains an id,
   all itemIdentities, all subjectLocators and all
   subjectIdentifiers"
  (when topic
    (let ((id
	   (concatenate 'string "\"id\":" (json:encode-json-to-string (topicid topic))))
	  (itemIdentity
	   (concatenate 'string "\"itemIdentities\":"
			(identifiers-to-json-string topic :what 'item-identifiers)))
	  (subjectLocator 
	   (concatenate 'string "\"subjectLocators\":"
			(identifiers-to-json-string topic :what 'locators)))
	  (subjectIdentifier
	   (concatenate 'string "\"subjectIdentifiers\":"
			(identifiers-to-json-string topic :what 'psis))))
      (declare (TopicC topic))
      (concatenate 'string "{" id "," itemIdentity "," subjectLocator ","
		   subjectIdentifier "}"))))


(defmethod to-json-string ((instance RoleC) &key (xtm-id d:*current-xtm*))
  "transforms an RoleC object to a json string"
  (declare (ignorable xtm-id))
  (let ((itemIdentity
	 (concatenate 'string "\"itemIdentities\":"
		      (identifiers-to-json-string instance :what 'item-identifiers)))
	(type
	 (type-to-json-string instance))
	(topicRef
	 (concatenate 'string "\"topicRef\":"
		      (if (slot-boundp instance 'player)
			  (json:encode-json-to-string (map 'list #'uri (psis (player instance))))
			  "null"))))
    (concatenate 'string "{" itemIdentity "," type "," topicRef "}")))


(defmethod to-json-string ((instance AssociationC) &key (xtm-id d:*current-xtm*))
  "transforms an AssociationC object to a json string"
  (let ((itemIdentity
	 (concatenate 'string "\"itemIdentities\":"
		      (identifiers-to-json-string instance :what 'item-identifiers)))
	(type
	 (type-to-json-string instance))
	(scope
	 (concatenate 'string "\"scopes\":" (ref-topics-to-json-string (themes instance))))
	(role
	 (concatenate 'string "\"roles\":"
		      (if (roles instance)
			  (let ((j-roles "["))
			    (loop for item in (roles instance)
			       do (setf j-roles
					(concatenate 'string j-roles (to-json-string item :xtm-id xtm-id) ",")))
			    (concatenate 'string (subseq j-roles 0 (- (length j-roles) 1)) "]"))
			  "null"))))
    (concatenate 'string "{" itemIdentity "," type "," scope "," role "}")))


(defmethod to-json-string ((instance TopicMapC) &key (xtm-id d:*current-xtm*))
  "returns the ItemIdentifier's uri"
  (declare (ignorable xtm-id))
  (let ((ii (item-identifiers instance)))
    (when ii
      (uri (first ii)))))


(defmethod to-json-string ((instance FragmentC) &key (xtm-id d:*current-xtm*))
  "transforms an FragmentC object to a json string,
   which contains the main topic, all depending topicStubs
   and all associations depending on the main topic"
  (let ((main-topic
	 (concatenate 'string "\"topic\":"
		      (to-json-string (topic instance) :xtm-id xtm-id)))
	(topicStubs
	 (concatenate 'string "\"topicStubs\":"
		      (if (referenced-topics instance)
			  (let ((j-topicStubs "["))
			    (loop for item in (referenced-topics instance)
			       do (setf j-topicStubs (concatenate 'string j-topicStubs
								  (to-json-topicStub-string item) ",")))
			    (concatenate 'string (subseq j-topicStubs 0 (- (length j-topicStubs) 1)) "]"))
			  "null")))
	(associations
	 (concatenate 'string "\"associations\":"
		      (if (associations instance)
			  (let ((j-associations "["))
			    (loop for item in (associations instance)
			       do (setf j-associations
					(concatenate 'string j-associations
						     (to-json-string item :xtm-id xtm-id) ",")))
			    (concatenate 'string (subseq j-associations 0 (- (length j-associations) 1)) "]"))
			  "null")))
	(tm-ids
	 (concatenate 'string "\"tmIds\":"
		      (if (in-topicmaps (topic instance))
			  (let ((j-tm-ids "["))
			    (loop for item in (in-topicmaps (topic instance))
			       ;do (setf j-tm-ids (concatenate 'string j-tm-ids "\""
				;			      (d:uri (first (d:item-identifiers item))) "\",")))
			       do (setf j-tm-ids (concatenate 'string j-tm-ids 
							      (json:encode-json-to-string (d:uri (first (d:item-identifiers item)))) ",")))
			    (concatenate 'string (subseq j-tm-ids 0 (- (length j-tm-ids) 1)) "]"))
			  "null"))))
    (concatenate 'string "{" main-topic "," topicStubs "," associations "," tm-ids "}")))


;; =============================================================================
;; --- json data summeries -----------------------------------------------------
;; =============================================================================
(defun get-all-topic-psis()
  "returns all topic psis as a json list of the form
   [[topic-1-psi-1, topic-1-psi-2],[topic-2-psi-1, topic-2-psi-2],...]"
  (encode-json-to-string
   (remove-if #'null (map 'list #'(lambda(psi-list)
				    (when psi-list
				      (map 'list #'uri psi-list)))
			  (map 'list #'psis (elephant:get-instances-by-class 'TopicC))))))


(defun to-json-string-summary (topic)
  "creates a json string of called topic element. the following elements are within this
   summary:
    *topic id
    *all identifiers
    *names (only the real name value)
    *occurrences (jonly the resourceRef and resourceData elements)"
  (declare (TopicC topic))
  (let ((id
	 (concatenate 'string "\"id\":\"" (topicid topic) "\""))
	(itemIdentity
	 (concatenate 'string "\"itemIdentities\":"
		      (identifiers-to-json-string topic :what 'item-identifiers)))
	(subjectLocator 
	 (concatenate 'string "\"subjectLocators\":"
		      (identifiers-to-json-string topic :what 'locators)))
	(subjectIdentifier
	 (concatenate 'string "\"subjectIdentifiers\":"
		      (identifiers-to-json-string topic :what 'psis)))
	(instanceOf
	 (concatenate 'string "\"instanceOfs\":" (ref-topics-to-json-string (list-instanceOf topic))))
	(name
	 (concatenate 'string "\"names\":"
		      (if (names topic)
			  (json:encode-json-to-string (loop for name in (names topic)
							 when (slot-boundp name 'charvalue)
							 collect (charvalue name)))
			  "null")))
	(occurrence
	 (concatenate 'string "\"occurrences\":"
		      (if (occurrences topic)
			  (json:encode-json-to-string (loop for occurrence in (occurrences topic)
							 when (slot-boundp occurrence 'charvalue)
							 collect (charvalue occurrence)))
			  "null"))))
    (concatenate 'string "{" id "," itemIdentity "," subjectLocator "," subjectIdentifier ","
		 instanceOf "," name "," occurrence "}")))


(defun make-topic-summary (topic-list)
  "creates a json list of the produced json-strings by to-json-string-summary"
  (if topic-list
      (let ((json-string
	     (let ((inner-string nil))
	       (concatenate 'string
			    (loop for topic in topic-list
			       do (setf inner-string (concatenate 'string inner-string (to-json-string-summary topic) ","))))
	       (subseq inner-string 0 (- (length inner-string) 1)))))
	(concatenate 'string "[" json-string "]"))
      "null"))