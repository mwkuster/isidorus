;;+-----------------------------------------------------------------------------
;;+  Isidorus
;;+  (c) 2008-2010 Marc Kuester, Christoph Ludwig, Lukas Georgieff
;;+
;;+  Isidorus is freely distributable under the LLGPL license.
;;+  You can find a detailed description in trunk/docs/LLGPL-LICENSE.txt and
;;+  trunk/docs/LGPL-LICENSE.txt.
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
(defgeneric to-json-string (instance &key xtm-id revision)
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
	   (map 'list #'uri (funcall what parent-construct :revision revision))))
      (json:encode-json-to-string items))))


(defun resourceX-to-json-string (value datatype &key (xtm-id d:*current-xtm*))
  "returns a resourceRef and resourceData json object"
  ;(declare (string value datatype))
  (if (string= datatype "http://www.w3.org/2001/XMLSchema#anyURI")
      (concatenate
       'string "\"resourceRef\":"		   
       (let ((inner-value
	      (let ((ref-topic (when (and (> (length value) 0)
					  (eql (elt value 0) #\#))
				 (get-item-by-id (subseq value 1) :xtm-id xtm-id))))
		(if ref-topic
		    (concatenate 'string "#" (topic-id ref-topic))
		    value))))
	 (json:encode-json-to-string inner-value))
       ",\"resourceData\":null")
      (concatenate 'string "\"resourceRef\":null,"
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
  (concatenate
   'string "\"type\":"
   (if (instance-of parent-elem :revision revision)
       (json:encode-json-to-string
	(map 'list #'uri (psis (instance-of parent-elem :revision revision)
			       :revision revision)))
       "null")))


(defmethod to-json-string ((instance VariantC) &key (xtm-id d:*current-xtm*)
			   (revision *TM-REVISION*))
  "transforms a VariantC object to a json string"
  (declare (type (or string null) xtm-id)
	   (type (or integer null) revision))
  (let ((itemIdentity
	 (concatenate
	  'string "\"itemIdentities\":"
	  (identifiers-to-json-string instance :what 'item-identifiers
				      :revision revision)))
	(scope
	 (concatenate
	  'string "\"scopes\":" (ref-topics-to-json-string
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
    (concatenate 'string "{" itemIdentity "," scope "," resourceX "}")))


(defmethod to-json-string ((instance NameC) &key (xtm-id d:*current-xtm*)
			   (revision *TM-REVISION*))
  "transforms a NameC object to a json string"
  (declare (type (or string null) xtm-id)
	   (type (or integer null) revision))
  (let ((itemIdentity
	 (concatenate
	  'string "\"itemIdentities\":"
	  (identifiers-to-json-string instance :what 'item-identifiers
				      :revision revision)))
	(type
	 (type-to-json-string instance :revision revision))
	(scope
	 (concatenate 
	  'string "\"scopes\":"
	  (ref-topics-to-json-string (themes instance :revision revision)
				     :revision revision)))
	(value
	 (concatenate 'string "\"value\":"
		      (if (slot-boundp instance 'charvalue)
			  (json:encode-json-to-string (charvalue instance))
			  "null")))
	(variant
	 (if (variants instance :revision revision)
	     (concatenate
	      'string "\"variants\":"
	      (let ((j-variants "["))
		(loop for variant in (variants instance :revision revision)
		   do (setf j-variants
			    (concatenate
			     'string j-variants
			     (json-exporter::to-json-string variant :xtm-id xtm-id
							    :revision revision)
			     ",")))
			    (concatenate
			     'string (subseq j-variants 0
					     (- (length j-variants) 1)) "]")))
	     (concatenate 'string "\"variants\":null"))))
    (concatenate 'string "{" itemIdentity "," type "," scope "," value
		 "," variant "}")))


(defmethod to-json-string ((instance OccurrenceC) &key (xtm-id d:*current-xtm*)
			   (revision *TM-REVISION*))
  "transforms an OccurrenceC object to a json string"
  (declare (type (or string null) xtm-id)
	   (type (or integer null) revision))
  (let ((itemIdentity
	 (concatenate
	  'string "\"itemIdentities\":"
	  (identifiers-to-json-string instance :what 'item-identifiers
				      :revision revision)))
	(type
	 (type-to-json-string instance :revision revision))
	(scope
	 (concatenate
	  'string "\"scopes\":"
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
    (concatenate 'string "{" itemIdentity "," type "," scope "," resourceX "}")))


(defmethod to-json-string ((instance TopicC) &key (xtm-id d:*current-xtm*)
			   (revision *TM-REVISION*))
  "transforms an TopicC object to a json string"
  (declare (type (or string null) xtm-id)
	   (type (or integer null) revision))
  (let ((id
	 (concatenate
	  'string "\"id\":"
	  (json:encode-json-to-string (topic-id instance revision))))
	(itemIdentity
	 (concatenate
	  'string "\"itemIdentities\":"
	  (identifiers-to-json-string instance :what 'item-identifiers
				      :revision revision)))
	(subjectLocator 
	 (concatenate
	  'string "\"subjectLocators\":"
	  (identifiers-to-json-string instance :what 'locators
				      :revision revision)))
	(subjectIdentifier
	 (concatenate
	  'string "\"subjectIdentifiers\":"
	  (identifiers-to-json-string instance :what 'psis
				      :revision revision)))
	(instanceOf
	 (concatenate
	  'string "\"instanceOfs\":"
	  (ref-topics-to-json-string (list-instanceOf instance :revision revision)
				     :revision revision)))
	(name
	 (concatenate
	  'string "\"names\":"
	  (if (names instance :revision revision)
	      (let ((j-names "["))
		(loop for item in (names instance :revision revision)
		   do (setf j-names
			    (concatenate 
			     'string j-names (to-json-string item :xtm-id xtm-id
							     :revision revision)
			     ",")))
		(concatenate 'string (subseq j-names 0 (- (length j-names) 1)) "]"))
	      "null")))
	(occurrence
	 (concatenate
	  'string "\"occurrences\":"
	  (if (occurrences instance :revision revision)
	      (let ((j-occurrences "["))
		(loop for item in (occurrences instance :revision revision)
		   do (setf j-occurrences
			    (concatenate
			     'string j-occurrences
			     (to-json-string item :xtm-id xtm-id :revision revision)
			     ",")))
		(concatenate
		 'string (subseq j-occurrences 0 (- (length j-occurrences) 1)) "]"))
	      "null"))))
    (concatenate 'string "{" id "," itemIdentity "," subjectLocator "," 
		 subjectIdentifier ","
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
	   (concatenate
	    'string "\"id\":"
	    (json:encode-json-to-string (topic-id topic revision))))
	  (itemIdentity
	   (concatenate
	    'string "\"itemIdentities\":"
	    (identifiers-to-json-string topic :what 'item-identifiers
					:revision revision)))
	  (subjectLocator 
	   (concatenate
	    'string "\"subjectLocators\":"
	    (identifiers-to-json-string topic :what 'locators :revision revision)))
	  (subjectIdentifier
	   (concatenate
	    'string "\"subjectIdentifiers\":"
	    (identifiers-to-json-string topic :what 'psis :revision revision))))
      (concatenate 'string "{" id "," itemIdentity "," subjectLocator ","
		   subjectIdentifier "}"))))


(defmethod to-json-string ((instance RoleC) &key (xtm-id d:*current-xtm*)
			   (revision *TM-REVISION*))
  "transforms an RoleC object to a json string"
  (declare (ignorable xtm-id)
	   (type (or integer null) revision))
  (let ((itemIdentity
	 (concatenate
	  'string "\"itemIdentities\":"
	  (identifiers-to-json-string instance :what 'item-identifiers
				      :revision revision)))
	(type
	 (type-to-json-string instance :revision revision))
	(topicRef
	 (concatenate
	  'string "\"topicRef\":"
	  (if (player instance :revision revision)
	      (json:encode-json-to-string
	       (map 'list #'uri (psis (player instance :revision revision)
				      :revision revision)))
	      "null"))))
    (concatenate 'string "{" itemIdentity "," type "," topicRef "}")))


(defmethod to-json-string ((instance AssociationC) &key (xtm-id d:*current-xtm*)
			   (revision *TM-REVISION*))
  "transforms an AssociationC object to a json string"
  (let ((itemIdentity
	 (concatenate 
	  'string "\"itemIdentities\":"
	  (identifiers-to-json-string instance :what 'item-identifiers
				      :revision revision)))
	(type
	 (type-to-json-string instance :revision revision))
	(scope
	 (concatenate
	  'string "\"scopes\":"
	  (ref-topics-to-json-string (themes instance :revision revision)
				     :revision revision)))
	(role
	 (concatenate
	  'string "\"roles\":"
	  (if (roles instance :revision revision)
	      (let ((j-roles "["))
		(loop for item in (roles instance :revision revision)
		   do (setf j-roles
			    (concatenate
			     'string j-roles (to-json-string item :xtm-id xtm-id
							     :revision revision)
			     ",")))
		(concatenate 'string (subseq j-roles 0 (- (length j-roles) 1)) "]"))
	      "null"))))
    (concatenate 'string "{" itemIdentity "," type "," scope "," role "}")))


(defmethod to-json-string ((instance TopicMapC) &key (xtm-id d:*current-xtm*)
			   (revision *TM-REVISION*))
  "returns the ItemIdentifier's uri"
  (declare (ignorable xtm-id)
	   (type (or integer null) revision))
  (let ((ii (item-identifiers instance :revision revision)))
    (when ii
      (uri (first ii)))))


(defmethod to-json-string ((instance FragmentC) &key (xtm-id d:*current-xtm*)
			   (revision *TM-REVISION*))
  "transforms an FragmentC object to a json string,
   which contains the main topic, all depending topicStubs
   and all associations depending on the main topic"
  (declare (type (or string null) xtm-id)
	   (type (or integer null) revision))
  (let ((main-topic
	 (concatenate
	  'string "\"topic\":"
	  (to-json-string (topic instance) :xtm-id xtm-id :revision revision)))
	(topicStubs
	 (concatenate
	  'string "\"topicStubs\":"
	  (if (referenced-topics instance)
	      (let ((j-topicStubs "["))
		(loop for item in (referenced-topics instance)
		   do (setf j-topicStubs
			    (concatenate
			     'string j-topicStubs
			     (to-json-topicStub-string item :revision revision)
			     ",")))
		(concatenate
		 'string (subseq j-topicStubs 0 (- (length j-topicStubs) 1)) "]"))
	      "null")))
	(associations
	 (concatenate
	  'string "\"associations\":"
	  (if (associations instance)
	      (let ((j-associations "["))
		(loop for item in (associations instance)
		   do (setf j-associations
			    (concatenate 'string j-associations
					 (to-json-string item :xtm-id xtm-id
							 :revision revision) ",")))
		(concatenate 'string (subseq j-associations 0
					     (- (length j-associations) 1)) "]"))
	      "null")))
	(tm-ids
	 (concatenate
	  'string "\"tmIds\":"
	  (if (in-topicmaps (topic instance))
	      (let ((j-tm-ids "["))
		(loop for item in (in-topicmaps (topic instance))
		   do (setf j-tm-ids
			    (concatenate
			     'string j-tm-ids 
			     (json:encode-json-to-string
			      (d:uri (first (d:item-identifiers item
								:revision revision))))
			     ",")))
		(concatenate 'string (subseq j-tm-ids 0 (- (length j-tm-ids) 1)) "]"))
	      "null"))))
    (concatenate 'string "{" main-topic "," topicStubs "," associations
		 "," tm-ids "}")))


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
	 (concatenate 'string "\"id\":\"" (topic-id topic revision) "\""))
	(itemIdentity
	 (concatenate
	  'string "\"itemIdentities\":"
	  (identifiers-to-json-string topic :what 'item-identifiers
				      :revision revision)))
	(subjectLocator 
	 (concatenate
	  'string "\"subjectLocators\":"
	  (identifiers-to-json-string topic :what 'locators :revision revision)))
	(subjectIdentifier
	 (concatenate
	  'string "\"subjectIdentifiers\":"
	  (identifiers-to-json-string topic :what 'psis :revision revision)))
	(instanceOf
	 (concatenate
	  'string "\"instanceOfs\":"
	  (ref-topics-to-json-string (list-instanceOf topic :revision revision)
				     :revision revision)))
	(name
	 (concatenate
	  'string "\"names\":"
	  (if (names topic :revision revision)
	      (json:encode-json-to-string
	       (loop for name in (names topic :revision revision)
		  when (slot-boundp name 'charvalue)
		  collect (charvalue name)))
	      "null")))
	(occurrence
	 (concatenate
	  'string "\"occurrences\":"
	  (if (occurrences topic :revision revision)
	      (json:encode-json-to-string
	       (loop for occurrence in (occurrences topic :revision revision)
		  when (slot-boundp occurrence 'charvalue)
		  collect (charvalue occurrence)))
	      "null"))))
    (concatenate 'string "{" id "," itemIdentity "," subjectLocator ","
		 subjectIdentifier "," instanceOf "," name "," occurrence "}")))


(defun make-topic-summary (topic-list &key (revision *TM-REVISION*))
  "creates a json list of the produced json-strings by to-json-string-summary"
  (declare (list topic-list)
	   (type (or integer null) revision))
  (if topic-list
      (let ((json-string
	     (let ((inner-string nil))
	       (concatenate
		'string
		(loop for topic in topic-list
		   do (setf inner-string
			    (concatenate
			     'string inner-string
			     (to-json-string-summary topic :revision revision) ","))))
	       (subseq inner-string 0 (- (length inner-string) 1)))))
	(concatenate 'string "[" json-string "]"))
      "null"))