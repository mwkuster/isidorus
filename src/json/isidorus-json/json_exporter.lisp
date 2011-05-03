;;+-----------------------------------------------------------------------------
;;+  Isidorus
;;+  (c) 2008-2010 Marc Kuester, Christoph Ludwig, Lukas Georgieff
;;+
;;+  Isidorus is freely distributable under the LLGPL license.
;;+  You can find a detailed description in trunk/docs/LLGPL-LICENSE.txt and
;;+  trunk/docs/LGPL-LICENSE.txt.
;;+-----------------------------------------------------------------------------

(defpackage :json-exporter
  (:use :cl :json :datamodel :TM-SPARQL :base-tools)
  (:export :export-construct-as-isidorus-json-string
	   :get-all-topic-psis
	   :to-json-string-summary
	   :make-topic-summary))

(in-package :json-exporter)

;; the json schema for our datamodel is in ".../docs/xtm_json.txt"


;; =============================================================================
;; --- main json data model ----------------------------------------------------
;; =============================================================================
(defgeneric export-construct-as-isidorus-json-string (instance &key xtm-id revision)
  (:documentation "converts the Topic Map construct instance to a json string"))


(defun identifiers-to-json-string (parent-construct &key (what 'd:psis)
				   (revision *TM-REVISION*))
  "returns the identifiers of a TopicMapConstructC as a json list"
  (declare (TopicMapConstructC parent-construct)
	   (symbol what)
	   (type (or integer null) revision))
  (when (and parent-construct
	     (or (eql what 'psis)
		 (eql what 'item-identifiers)
		 (eql what 'locators)))
    (let ((items
	   (map 'list #'uri
		(funcall what parent-construct :revision revision))))
      (json:encode-json-to-string items))))


(defun resourceX-to-json-string (value datatype &key (xtm-id d:*current-xtm*))
  "returns a resourceRef and resourceData json object"
  (if (string= datatype "http://www.w3.org/2001/XMLSchema#anyURI")
      (concat "\"resourceRef\":"		   
	      (let ((inner-value
		     (let ((ref-topic
			    (when (and (> (length value) 0)
				       (eql (elt value 0) #\#))
			      (get-item-by-id (subseq value 1) :xtm-id xtm-id))))
		       (if ref-topic
			   (concat "#" (topic-id ref-topic))
			   value))))
		(json:encode-json-to-string inner-value))
	      ",\"resourceData\":null")
      (concat "\"resourceRef\":null,"
	      "\"resourceData\":{\"datatype\":"
	      (json:encode-json-to-string datatype)
	      ",\"value\":"
	      (json:encode-json-to-string value) "}")))


(defun ref-topics-to-json-string (topics &key (revision *TM-REVISION*))
  "returns a json string of all psi-uris of the passed topics as a list of lists"
  (declare (list topics)
	   (type (or integer null) revision))
  (if topics
      (let ((psis (json:encode-json-to-string
		   (map 'list #'(lambda(topic)
				  (declare (topicC topic))
				  (map 'list #'uri (psis topic :revision revision)))
			topics))))
	(declare (list topics))
	psis)
      "null"))


(defun type-to-json-string (parent-elem &key (revision *TM-REVISION*))
  "returns a json string of the type of the passed parent-elem"
  (declare (TypableC parent-elem)
	   (type (or integer null) revision))
  (concat "\"type\":"
	  (if (instance-of parent-elem :revision revision)
	      (json:encode-json-to-string
	       (map 'list #'uri (psis (instance-of parent-elem :revision revision)
				      :revision revision)))
	      "null")))


(defmethod export-construct-as-isidorus-json-string
    ((instance VariantC) &key (xtm-id d:*current-xtm*)
     (revision *TM-REVISION*))
  "transforms a VariantC object to a json string"
  (declare (type (or string null) xtm-id)
	   (type (or integer null) revision))
  (let ((itemIdentity
	 (concat "\"itemIdentities\":"
		 (identifiers-to-json-string instance :what 'item-identifiers
					     :revision revision)))
	(scope
	 (concat "\"scopes\":" (ref-topics-to-json-string
				(themes instance :revision revision)
				:revision revision)))
	(resourceX
	 (let ((value
		(when (slot-boundp instance 'charvalue)
		  (charvalue instance)))
	       (type
		(when (slot-boundp instance 'datatype)
		  (datatype instance))))
	   (resourceX-to-json-string value type :xtm-id xtm-id))))
    (concat "{" itemIdentity "," scope "," resourceX "}")))


(defmethod export-construct-as-isidorus-json-string
    ((instance NameC) &key (xtm-id d:*current-xtm*)
     (revision *TM-REVISION*))
  "transforms a NameC object to a json string"
  (declare (type (or string null) xtm-id)
	   (type (or integer null) revision))
  (let ((itemIdentity
	 (concat "\"itemIdentities\":"
		 (identifiers-to-json-string instance :what 'item-identifiers
					     :revision revision)))
	(type
	 (type-to-json-string instance :revision revision))
	(scope
	 (concat "\"scopes\":"
		 (ref-topics-to-json-string (themes instance :revision revision)
					    :revision revision)))
	(value
	 (concat "\"value\":"
		 (if (slot-boundp instance 'charvalue)
		     (json:encode-json-to-string (charvalue instance))
		     "null")))
	(variant
	 (if (variants instance :revision revision)
	     (concat
	      "\"variants\":"
	      (let ((j-variants "["))
		(loop for variant in (variants instance :revision revision)
		   do (push-string
		       (concat (export-construct-as-isidorus-json-string
				variant :xtm-id xtm-id :revision revision)
			       ",")
		       j-variants))
		(concat (subseq j-variants 0 (- (length j-variants) 1)) "]")))
	     (concat "\"variants\":null"))))
    (concat "{" itemIdentity "," type "," scope "," value "," variant "}")))


(defmethod export-construct-as-isidorus-json-string
    ((instance OccurrenceC) &key (xtm-id d:*current-xtm*)
     (revision *TM-REVISION*))
  "transforms an OccurrenceC object to a json string"
  (declare (type (or string null) xtm-id)
	   (type (or integer null) revision))
  (let ((itemIdentity
	 (concat "\"itemIdentities\":"
		 (identifiers-to-json-string instance :what 'item-identifiers
					     :revision revision)))
	(type
	 (type-to-json-string instance :revision revision))
	(scope
	 (concat "\"scopes\":"
		 (ref-topics-to-json-string (themes instance :revision revision)
					    :revision revision)))
	(resourceX
	 (let ((value
		(when (slot-boundp instance 'charvalue)
		  (charvalue instance)))
	       (type
		(when (slot-boundp instance 'datatype)
		  (datatype instance))))
	   (resourceX-to-json-string value type :xtm-id xtm-id))))
    (concat "{" itemIdentity "," type "," scope "," resourceX "}")))


(defmethod export-construct-as-isidorus-json-string
    ((instance TopicC) &key (xtm-id d:*current-xtm*)
     (revision *TM-REVISION*))
  "transforms an TopicC object to a json string"
  (declare (type (or string null) xtm-id)
	   (type (or integer null) revision))
  (let ((id
	 (concat "\"id\":"
		 (json:encode-json-to-string (topic-id instance revision))))
	(itemIdentity
	 (concat "\"itemIdentities\":"
		 (identifiers-to-json-string instance :what 'item-identifiers
					     :revision revision)))
	(subjectLocator 
	 (concat "\"subjectLocators\":"
		 (identifiers-to-json-string instance :what 'locators
					     :revision revision)))
	(subjectIdentifier
	 (concat "\"subjectIdentifiers\":"
		 (identifiers-to-json-string instance :what 'psis
					     :revision revision)))
	(instanceOf
	 (concat "\"instanceOfs\":"
		 (ref-topics-to-json-string
		  (list-instanceOf instance :revision revision)
		  :revision revision)))
	(name
	 (concat "\"names\":"
		 (if (names instance :revision revision)
		     (let ((j-names "["))
		       (loop for item in (names instance :revision revision)
			  do (push-string
			      (concat
			       (export-construct-as-isidorus-json-string
				item :xtm-id xtm-id
				:revision revision) ",")
			      j-names))
		       (concat (subseq j-names 0 (- (length j-names) 1)) "]"))
		     "null")))
	(occurrence
	 (concat
	  "\"occurrences\":"
	  (if (occurrences instance :revision revision)
	      (let ((j-occurrences "["))
		(loop for item in (occurrences instance :revision revision)
		   do (push-string
		       (concat
			(export-construct-as-isidorus-json-string
			 item :xtm-id xtm-id :revision revision)
			",")
		       j-occurrences))
		(concat (subseq j-occurrences 0 (- (length j-occurrences) 1)) "]"))
	      "null"))))
    (concat "{" id "," itemIdentity "," subjectLocator "," subjectIdentifier ","
	    instanceOf "," name "," occurrence "}")))


(defun to-json-topicStub-string (topic &key (revision *TM-REVISION*))
  "transforms the passed TopicC object to a topic stub
   string in the json format, which contains an id,
   all itemIdentities, all subjectLocators and all
   subjectIdentifiers"
  (declare (type (or TopicC null) topic)
	   (type (or integer null) revision))
  (when topic
    (let ((id
	   (concat "\"id\":"
		   (json:encode-json-to-string (topic-id topic revision))))
	  (itemIdentity
	   (concat "\"itemIdentities\":"
		   (identifiers-to-json-string topic :what 'item-identifiers
					       :revision revision)))
	  (subjectLocator 
	   (concat "\"subjectLocators\":"
		   (identifiers-to-json-string topic :what 'locators :revision revision)))
	  (subjectIdentifier
	   (concat "\"subjectIdentifiers\":"
		   (identifiers-to-json-string topic :what 'psis :revision revision))))
      (concat "{" id "," itemIdentity "," subjectLocator "," subjectIdentifier "}"))))


(defmethod export-construct-as-isidorus-json-string
    ((instance RoleC) &key (xtm-id d:*current-xtm*)
     (revision *TM-REVISION*))
  "transforms an RoleC object to a json string"
  (declare (ignorable xtm-id)
	   (type (or integer null) revision))
  (let ((itemIdentity
	 (concat "\"itemIdentities\":"
		 (identifiers-to-json-string instance :what 'item-identifiers
					     :revision revision)))
	(type
	 (type-to-json-string instance :revision revision))
	(topicRef
	 (concat "\"topicRef\":"
		 (if (player instance :revision revision)
		     (json:encode-json-to-string
		      (map 'list #'uri (psis (player instance :revision revision)
					     :revision revision)))
		     "null"))))
    (concat "{" itemIdentity "," type "," topicRef "}")))


(defmethod export-construct-as-isidorus-json-string
    ((instance AssociationC) &key (xtm-id d:*current-xtm*)
     (revision *TM-REVISION*))
  "transforms an AssociationC object to a json string"
  (let ((itemIdentity
	 (concat "\"itemIdentities\":"
		 (identifiers-to-json-string instance :what 'item-identifiers
					     :revision revision)))
	(type
	 (type-to-json-string instance :revision revision))
	(scope
	 (concat "\"scopes\":"
		 (ref-topics-to-json-string (themes instance :revision revision)
					    :revision revision)))
	(role
	 (concat "\"roles\":"
		 (if (roles instance :revision revision)
		     (let ((j-roles "["))
		       (loop for item in (roles instance :revision revision)
			  do (push-string
			      (concat
			       (export-construct-as-isidorus-json-string
				item :xtm-id xtm-id
				:revision revision) ",")
			      j-roles))
		       (concat (subseq j-roles 0 (- (length j-roles) 1)) "]"))
		     "null"))))
    (concat "{" itemIdentity "," type "," scope "," role "}")))


(defmethod export-construct-as-isidorus-json-string
    ((instance TopicMapC) &key (xtm-id d:*current-xtm*)
     (revision *TM-REVISION*))
  "returns the ItemIdentifier's uri"
  (declare (ignorable xtm-id)
	   (type (or integer null) revision))
  (let ((ii (item-identifiers instance :revision revision)))
    (when ii
      (uri (first ii)))))


(defmethod export-construct-as-isidorus-json-string
    ((instance FragmentC) &key (xtm-id d:*current-xtm*)
     (revision *TM-REVISION*))
  "transforms an FragmentC object to a json string,
   which contains the main topic, all depending topicStubs
   and all associations depending on the main topic"
  (declare (type (or string null) xtm-id)
	   (type (or integer null) revision))
  (let ((main-topic
	 (concat "\"topic\":"
		 (export-construct-as-isidorus-json-string
		  (topic instance) :xtm-id xtm-id :revision revision)))
	(topicStubs
	 (concat "\"topicStubs\":"
		 (if (referenced-topics instance)
		     (let ((j-topicStubs "["))
		       (loop for item in (referenced-topics instance)
			  do (push-string
			      (concat (to-json-topicStub-string item :revision revision)
				      ",")
			      j-topicStubs))
		       (concat (subseq j-topicStubs 0 (- (length j-topicStubs) 1)) "]"))
		     "null")))
	(associations
	 (concat "\"associations\":"
		 (if (associations instance)
		     (let ((j-associations "["))
		       (loop for item in (associations instance)
			  do (push-string
			      (concat (export-construct-as-isidorus-json-string
				       item :xtm-id xtm-id
				       :revision revision) ",")
			      j-associations))
		       (concat (subseq j-associations 0
				       (- (length j-associations) 1)) "]"))
		     "null")))
	(tm-ids
	 (concat "\"tmIds\":"
		 (let ((uris
			(loop for tm in (in-topicmaps (topic instance))
			   collect (when (item-identifiers tm)
				     (uri (first (item-identifiers
						  tm :revision revision)))))))
		   (json:encode-json-to-string uris)))))
    (concat "{" main-topic "," topicStubs "," associations "," tm-ids "}")))


;; =============================================================================
;; --- json data summeries -----------------------------------------------------
;; =============================================================================
(defun get-all-topic-psis(&key (revision *TM-REVISION*))
  "returns all topic psis as a json list of the form
   [[topic-1-psi-1, topic-1-psi-2],[topic-2-psi-1, topic-2-psi-2],...]"
  (declare (type (or integer null) revision))
  (encode-json-to-string
   (remove-if #'null
	      (map 'list
		   #'(lambda(psi-list)
		       (when psi-list
			 (map 'list #'uri psi-list)))
		   (map 'list #'psis (get-all-topics revision))))))


(defun to-json-string-summary (topic &key (revision *TM-REVISION*))
  "creates a json string of called topic element. the following elements are within this
   summary:
    *topic id
    *all identifiers
    *names (only the real name value)
    *occurrences (jonly the resourceRef and resourceData elements)"
  (declare (TopicC topic)
	   (type (or integer null) revision))
  (let ((id
	 (concat "\"id\":\"" (topic-id topic revision) "\""))
	(itemIdentity
	 (concat "\"itemIdentities\":"
		 (identifiers-to-json-string topic :what 'item-identifiers
					     :revision revision)))
	(subjectLocator 
	 (concat "\"subjectLocators\":"
		 (identifiers-to-json-string topic :what 'locators :revision revision)))
	(subjectIdentifier
	 (concat "\"subjectIdentifiers\":"
		 (identifiers-to-json-string topic :what 'psis :revision revision)))
	(instanceOf
	 (concat "\"instanceOfs\":"
		 (ref-topics-to-json-string (list-instanceOf topic :revision revision)
					    :revision revision)))
	(name
	 (concat "\"names\":"
		 (if (names topic :revision revision)
		     (json:encode-json-to-string
		      (loop for name in (names topic :revision revision)
			 when (slot-boundp name 'charvalue)
			 collect (charvalue name)))
		     "null")))
	(occurrence
	 (concat "\"occurrences\":"
		 (if (occurrences topic :revision revision)
		     (json:encode-json-to-string
		      (loop for occurrence in (occurrences topic :revision revision)
			 when (slot-boundp occurrence 'charvalue)
			 collect (charvalue occurrence)))
		     "null"))))
    (concat "{" id "," itemIdentity "," subjectLocator "," subjectIdentifier
	    "," instanceOf "," name "," occurrence "}")))


(defun make-topic-summary (topic-list &key (revision *TM-REVISION*))
  "creates a json list of the produced json-strings by to-json-string-summary"
  (declare (list topic-list)
	   (type (or integer null) revision))
  (if topic-list
      (let ((json-string
	     (let ((inner-string nil))
	       (loop for topic in topic-list
		  do (push-string 
		      (concat (to-json-string-summary topic :revision revision) ",")
		      inner-string))
	       (subseq inner-string 0 (- (length inner-string) 1)))))
	(concat "[" json-string "]"))
      "null"))


;; =============================================================================
;; --- json data sparql-results ------------------------------------------------
;; =============================================================================

(defmethod export-construct-as-isidorus-json-string
    ((construct SPARQL-Query) &key xtm-id revision)
  "Returns a JSON string that represents the object query result."
  (declare (Ignorable revision xtm-id))
  (let ((query-result (result construct)))
    (if (not query-result)
	"null"
	(let ((j-str "{"))
	  (loop for entry in query-result
	     do (push-string
		 (concat
		  (json:encode-json-to-string (getf entry :variable)) ":"
		  (json:encode-json-to-string (getf entry :result)) ",")
		 j-str))
	  (concat (subseq j-str 0 (- (length j-str) 1)) "}")))))