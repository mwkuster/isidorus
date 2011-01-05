;;+-----------------------------------------------------------------------------
;;+  Isidorus
;;+  (c) 2008-2010 Marc Kuester, Christoph Ludwig, Lukas Georgieff
;;+
;;+  Isidorus is freely distributable under the LLGPL license.
;;+  You can find a detailed description in trunk/docs/LLGPL-LICENSE.txt and
;;+  trunk/docs/LGPL-LICENSE.txt.
;;+-----------------------------------------------------------------------------

(in-package :exporter)

(defun to-reifier-elem (reifiable-construct revision)
  "Exports the reifier-attribute.
   The attribute is only exported if the reifier-topic contains at least
   one item-identifier."
  (declare (type (or ReifiableConstructC nil) reifiable-construct)
	   (type (or integer nil) revision))
  (when (and (reifier reifiable-construct :revision revision)
	     (item-identifiers (reifier reifiable-construct :revision revision)
			       :revision revision))
    (cxml:attribute "reifier"
		    (uri (first (item-identifiers (reifier reifiable-construct
							   :revision revision)
						  :revision revision))))))

(defun ref-to-elem (topic revision)
  (declare (TopicC topic)
	   (type (or integer nil) revision))
  (cxml:with-element "t:topicRef"
    ;;TODO: this is pretty much of a hack that works only for local
    ;;references
    (cxml:attribute "href" 
                    (format nil "#~a" (topic-id topic revision)))))


(defgeneric to-elem (instance revision)
  (:documentation "converts the Topic Maps construct instance to an XTM 2.0 element"))


(defmethod to-elem ((psi PersistentIdC) revision)
  (declare (ignorable revision))
  (cxml:with-element "t:subjectIdentifier"
    (cxml:attribute "href" (uri psi))))


(defmethod to-elem ((name NameC) revision)
  "name = element name { reifiable, 
                         type?, scope?, value, variant* }"
  (declare (type (or integer nil) revision))
  (cxml:with-element "t:name"
    (to-reifier-elem name revision)
    (map 'list #'(lambda(x)
		   (to-elem x revision))
	 (item-identifiers name :revision revision))
    (when (instance-of name :revision revision)
      (cxml:with-element "t:type"
	(ref-to-elem (instance-of name :revision revision) revision)))
    (when (themes name :revision revision)
      (cxml:with-element "t:scope"
	(map 'list #'(lambda(x)
		       (ref-to-elem x revision))
	     (themes name :revision revision))))
    (cxml:with-element "t:value"
      (cxml:text
       (if (slot-boundp name 'charvalue)
	   (charvalue name)
	   "")))
    (when (variants name :revision revision)
      (map 'list #'(lambda(x)
		     (to-elem x revision))
	   (variants name :revision revision)))))


(defun to-resourceX-elem (characteristic revision)
  "returns a resourceData or resourceRef element"
  (declare (CharacteristicC characteristic))
  (let ((characteristic-value
	 (if (slot-boundp characteristic 'charvalue)
	     (charvalue characteristic)
	     ""))
	(characteristic-type 
	 (if (slot-boundp characteristic 'datatype)
	     (datatype characteristic)
	     "")))
    (if (string= characteristic-type *xml-uri*) ;-> resourceRef
	(cxml:with-element "t:resourceRef"
	  (let ((ref-topic (when (and (> (length characteristic-value) 0)
				      (eql (elt characteristic-value 0) #\#))
			     (get-item-by-id (subseq characteristic-value 1)
					     :revision revision))))
	    (cxml:attribute "href"
			    (if ref-topic
				(concat "#" (topic-id ref-topic revision))
				characteristic-value))))
	(cxml:with-element "t:resourceData"
	  (when (slot-boundp characteristic 'datatype)
	    (cxml:attribute "datatype" characteristic-type))
	  (cxml:text characteristic-value)))))


(defmethod to-elem ((variant VariantC) revision)
  "variant = element variant { reifiable, scope, (resourceRef | resourceData) }"
  (cxml:with-element "t:variant"
    (to-reifier-elem variant revision)
    (map 'list #'(lambda(x)
		   (to-elem x revision))
	 (item-identifiers variant :revision revision))
    (when (themes variant :revision revision)
      (cxml:with-element "t:scope"
	(map 'list #'(lambda(x)
		       (ref-to-elem x revision))
	     (themes variant :revision revision))))
    (to-resourceX-elem variant revision)))


(defmethod to-elem ((ii ItemIdentifierC) revision)
  "itemIdentity = element itemIdentity { href }"
  (declare (ignorable revision))
  (cxml:with-element "t:itemIdentity" 
    (cxml:attribute "href" (uri ii))))


(defmethod to-elem ((occ OccurrenceC) revision)
  "occurrence = element occurrence { reifiable, 
                         type, scope?, (resourceRef | resourceData) }"
  (declare (type (or integer nil) revision))
  (cxml:with-element "t:occurrence"
    (to-reifier-elem occ revision)
    (map 'list #'(lambda(x)
		   (to-elem x revision))
	 (item-identifiers occ :revision revision))
    (cxml:with-element "t:type"
      (ref-to-elem (instance-of occ :revision revision) revision))
    (map 'list #'(lambda(x)
		   (cxml:with-element "t:scope"
		     (ref-to-elem x revision))) (themes occ :revision revision))
    (to-resourceX-elem occ revision)))


(defmethod to-elem ((locator SubjectLocatorC) revision)
  "subjectLocator = element subjectLocator { href }"
  (declare (ignorable revision))
  (cxml:with-element "t:subjectLocator"
    (cxml:attribute "href" (uri locator))))


(defmethod to-elem ((topic TopicC) revision)
  "topic = element topic { id, 
                        (itemIdentity | subjectLocator | subjectIdentifier)*,
                        instanceOf?, (name | occurrence)* }"
  (declare (type (or integer nil) revision))
  (cxml:with-element "t:topic"
    (cxml:attribute "id" (topic-id topic revision))
    (map 'list #'(lambda(x)
		   (to-elem x revision))
	 (item-identifiers topic :revision revision))
    (map 'list #'(lambda(x)
		   (to-elem x revision))
	 (locators topic :revision revision))
    (map 'list #'(lambda(x)
		   (to-elem x revision))
	 (psis topic :revision revision))
    (let ((ios (list-instanceOf topic :tm *export-tm* :revision revision)))
      (when ios
	(cxml:with-element "t:instanceOf"
	  (loop for item in ios
	     do (cxml:with-element "t:topicRef"
		  (cxml:attribute "href" (concat "#" (topic-id item revision))))))))
    (map 'list #'(lambda(x)
		   (to-elem x revision))
	 (names topic :revision revision))
    (map 'list #'(lambda(x)
		   (to-elem x revision))
	 (occurrences topic :revision revision))))


(defun to-stub-elem (topic revision)
  "transforms a TopicC object to a topic stub element
   with a topicid, a subjectLocator and an itemIdentity element"
  (declare (TopicC topic)
	   (type (or nil integer) revision))
  (cxml:with-element "t:topic"
    (cxml:attribute "id" (topic-id topic revision))
    (map 'list #'(lambda(x)
		   (to-elem x revision))
	 (psis topic :revision revision))
    (map 'list #'(lambda(x)
		   (to-elem x revision))
	 (item-identifiers topic :revision revision))
    (map 'list #'(lambda(x)
		   (to-elem x revision))
	 (locators topic :revision revision))))


(defmethod to-elem ((role RoleC) revision)
  "role = element role { reifiable, type, topicRef }"
  (declare (type (or integer nil) revision))
  (cxml:with-element "t:role"
    (to-reifier-elem role revision)
    (map 'list #'(lambda(x)
		   (to-elem x revision))
	 (item-identifiers role :revision revision))
    (cxml:with-element "t:type"
      (ref-to-elem (instance-of role) revision))
    (ref-to-elem (player role :revision revision) revision)))


(defmethod to-elem ((assoc AssociationC) revision)
  "association = element association { reifiable, type, scope?, role+ }"
  (declare (type (or integer nil) revision))
  (cxml:with-element "t:association"
    (to-reifier-elem assoc revision)
    (map 'list #'(lambda(x)
		   (to-elem x revision))
	 (item-identifiers assoc :revision revision))
    (cxml:with-element "t:type"
      (ref-to-elem (instance-of assoc :revision revision) revision))
    (when (themes assoc :revision revision)
      (cxml:with-element "t:scope"
	(map 'list #'(lambda(x)
		       (ref-to-elem x revision))
	     (themes assoc :revision revision))))
    (map 'list #'(lambda(x)
		   (to-elem x revision))
	 (roles assoc :revision revision))))


(defmethod to-elem ((fragment FragmentC) revision)
  "transforms all sub-elements of the passed FragmentC instance"
  (declare (type (or integer nil) revision))
  (to-elem (topic fragment) revision)
  (map 'list #'(lambda(x)
		 (to-stub-elem x revision))
       (referenced-topics fragment))
  (map 'list #'(lambda(x)
		 (to-elem x revision))
       (associations fragment)))


(defgeneric to-string (construct &key revision)
  (:documentation "Print the string representation of a TM element"))

(defmethod to-string ((construct TopicMapConstructC) &key (revision *TM-REVISION*))
  (cxml:with-xml-output (cxml:make-string-sink :indentation 2 :canonical nil)
    (cxml:with-namespace ("t" *xtm2.0-ns*)
        (to-elem construct revision))))


