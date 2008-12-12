(defpackage :json-exporter
  (:use :cl :json :datamodel)
  (:export :to-json-string))

(in-package :json-exporter)

;; the json schema for our datamodel is in ".../docs/xtm_json.txt"

(defgeneric to-json-string (instance)
  (:documentation "converts the Topic Maps construct instance to an json string"))


(defun identifiers-to-json-string (parent-construct &key (what 'd:psis))
  "returns the identifiers of a TopicMapConstructC as a json list"
  (when (and parent-construct
	     (or (eql what 'psis) (eql what 'item-identifiers) (eql what 'locators)))
    (let ((items 
	   (map 'list #'uri (funcall what parent-construct))))
      (declare (TopicMapConstructC parent-construct)) ;must be a topic for psis and locators
      (json:encode-json-to-string items))))


(defun resourceX-to-json-string (value datatype)
  "returns a resourceRef and resourceData json object"
  ;(declare (string value datatype))
  (if (string= datatype "http://www.w3.org/2001/XMLSchema#anyURI")
      (concatenate 'string "\"resourceRef\":"
		           (json:encode-json-to-string value)
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


(defmethod to-json-string ((instance VariantC))
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
	   (resourceX-to-json-string value type))))
    (concatenate 'string "{" itemIdentity "," scope "," resourceX "}")))


(defmethod to-json-string ((instance NameC))
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
			       do (setf j-variants (concatenate 'string j-variants
							      (json-exporter::to-json-string variant) ",")))
			    (concatenate 'string (subseq j-variants 0 (- (length j-variants) 1)) "]")))
	     (concatenate 'string "\"variants\":null"))))
    (concatenate 'string "{" itemIdentity "," type "," scope "," value "," variant "}")))


(defmethod to-json-string ((instance OccurrenceC))
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
	   (resourceX-to-json-string value type))))
    (concatenate 'string "{" itemIdentity "," type "," scope "," resourceX "}")))


(defmethod to-json-string ((instance TopicC))
  "transforms an OccurrenceC object to a json string"
  (let ((id
	 (concatenate 'string "\"id\":\"" (topicid instance) "\""))
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
			       do (setf j-names (concatenate 'string j-names (to-json-string item) ",")))
			    (concatenate 'string (subseq j-names 0 (- (length j-names) 1)) "]"))
			  "null")))
	(occurrence
	 (concatenate 'string "\"occurrences\":"
		      (if (occurrences instance)
			  (let ((j-occurrences "["))
			    (loop for item in (occurrences instance)
			       do (setf j-occurrences (concatenate 'string j-occurrences (to-json-string item) ",")))
			    (concatenate 'string (subseq j-occurrences 0 (- (length j-occurrences) 1)) "]"))
			  "null"))))
    (concatenate 'string "{" id "," itemIdentity "," subjectLocator "," subjectIdentifier ","
		         instanceOf "," name "," occurrence "}")))


(defmethod to-json-string ((instance RoleC))
  "transforms an RoleC object to a json string"
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


(defmethod to-json-string ((instance AssociationC))
  "transforms an AssociationC object to a json string"
  (let ((itemIdentity
	 (concatenate 'string "\"itemIdentities\":"
		      (identifiers-to-json-string instance :what 'item-identifiers)))
	(type
	 (type-to-json-string instance))
	(scope
	 (let ((scopes (map 'list #'topicid (themes instance))))
	   (concatenate 'string "\"scopes\":" (json:encode-json-to-string scopes))))
	(role
	 (concatenate 'string "\"roles\":"
		      (if (roles instance)
			  (let ((j-roles "["))
			    (loop for item in (roles instance)
			       do (setf j-roles (concatenate 'string j-roles (to-json-string item) ",")))
			    (concatenate 'string (subseq j-roles 0 (- (length j-roles) 1)) "]"))
			  "null"))))
    (concatenate 'string "{" itemIdentity "," type "," scope "," role "}")))