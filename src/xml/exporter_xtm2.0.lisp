;;+-----------------------------------------------------------------------------
;;+  Isidorus
;;+  (c) 2008-2009 Marc Kuester, Christoph Ludwig, Lukas Giessmann
;;+
;;+  Isidorus is freely distributable under the LGPL license.
;;+  You can find a detailed description in trunk/docs/LGPL-LICENSE.txt.
;;+-----------------------------------------------------------------------------


(in-package :exporter)

(defun ref-to-elem (topic)
  (declare (TopicC topic))
  (cxml:with-element "t:topicRef"
    ;;TODO: this is pretty much of a hack that works only for local
    ;;references
    (cxml:attribute "href" 
                    (format nil "#~a" (topicid topic)))))

(defgeneric to-elem (instance)
  (:documentation "converts the Topic Maps construct instance to an XTM 2.0 element"))

(defmethod to-elem ((psi PersistentIdC))
  (cxml:with-element "t:subjectIdentifier"
    (cxml:attribute "href" (uri psi))))


(defmethod to-elem ((name NameC))
  "name = element name { reifiable, 
                         type?, scope?, value, variant* }"
  (cxml:with-element "t:name"
    (map 'list #'to-elem (item-identifiers name))
    (when (slot-boundp name 'instance-of)
      (cxml:with-element "t:type"
	(ref-to-elem (instance-of name))))
    (when (themes name)
      (cxml:with-element "t:scope"
	(map 'list #'ref-to-elem (themes name))))
    (cxml:with-element "t:value"
      (cxml:text
       (if (slot-boundp name 'charvalue)
	   (charvalue name)
	   "")))
    (when (variants name)
      (map 'list #'to-elem (variants name)))))


(defun to-resourceX-elem (characteristic)
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
    (if (string= characteristic-type "http://www.w3.org/2001/XMLSchema#anyURI") ;-> resourceRef
	(cxml:with-element "t:resourceRef"
	  (let ((ref-topic (when (and (> (length characteristic-value) 0)
				      (eql (elt characteristic-value 0) #\#))
			     (get-item-by-id (subseq characteristic-value 1)))))
	    (cxml:attribute "href"
			    (if ref-topic
				(concatenate 'string "#" (topicid ref-topic))
				characteristic-value))))
	(cxml:with-element "t:resourceData"
	  (when (slot-boundp characteristic 'datatype)
	    (cxml:attribute "datatype" characteristic-type))
	  (cxml:text characteristic-value)))))


(defmethod to-elem ((variant VariantC))
  "variant = element variant { reifiable, scope, (resourceRef | resourceData) }"
  (cxml:with-element "t:variant"
    (map 'list #'to-elem (item-identifiers variant))
    (when (themes variant)
      (cxml:with-element "t:scope"
	(map 'list #'ref-to-elem (themes variant))))
    (to-resourceX-elem variant)))


(defmethod to-elem ((ii ItemIdentifierC))
  "itemIdentity = element itemIdentity { href }"
  (cxml:with-element "t:itemIdentity" 
    (cxml:attribute "href" (uri ii))))


(defmethod to-elem ((occ OccurrenceC))
  "occurrence = element occurrence { reifiable, 
                         type, scope?, (resourceRef | resourceData) }"
  (cxml:with-element "t:occurrence"
    (map 'list #'to-elem (item-identifiers occ))
    (cxml:with-element "t:type"
      (ref-to-elem (instance-of occ)))
    (map 'list #'(lambda(x)
		   (cxml:with-element "t:scope"
		     (ref-to-elem x))) (themes occ))
    (to-resourceX-elem occ)))


(defmethod to-elem ((locator SubjectLocatorC))
  "subjectLocator = element subjectLocator { href }"
  (cxml:with-element "t:subjectLocator"
    (cxml:attribute "href" (uri locator))))


(defmethod to-elem ((topic TopicC))
  "topic = element topic { id, 
                        (itemIdentity | subjectLocator | subjectIdentifier)*,
                        instanceOf?, (name | occurrence)* }"
  (cxml:with-element "t:topic"
    (cxml:attribute "id" (topicid topic))
    (map 'list #'to-elem (item-identifiers topic))
    (map 'list #'to-elem (locators topic))
    (map 'list #'to-elem (psis topic))
    (when (list-instanceOf topic :tm *export-tm*)
      (cxml:with-element "t:instanceOf"
	(loop for item in (list-instanceOf topic :tm *export-tm*)
	   do (cxml:with-element "t:topicRef"
		(cxml:attribute "href" (concatenate 'string "#" (topicid item)))))))
    (map 'list #'to-elem (names topic))
    (map 'list #'to-elem (occurrences topic))))


(defun to-stub-elem (topic)
  "transforms a TopicC object to a topic stub element
   with a topicid, a subjectLocator and an itemIdentity element"
  (declare (TopicC topic))
  (cxml:with-element "t:topic"
    (cxml:attribute "id" (topicid topic))
    (map 'list #'to-elem (psis topic))
    (map 'list #'to-elem (item-identifiers topic))
    (map 'list #'to-elem (locators topic))))


(defmethod to-elem ((role RoleC))
  "role = element role { reifiable, type, topicRef }"
  (cxml:with-element "t:role"
    (map 'list #'to-elem (item-identifiers role))
    (cxml:with-element "t:type"
      (ref-to-elem (instance-of role)))
    (ref-to-elem (player role))))


(defmethod to-elem ((assoc AssociationC))
  "association = element association { reifiable, type, scope?, role+ }"
  (cxml:with-element "t:association"
    (map 'list #'to-elem (item-identifiers assoc))
    (cxml:with-element "t:type"
      (ref-to-elem (instance-of assoc)))
    (when (themes assoc)
      (cxml:with-element "t:scope"
	(map 'list #'ref-to-elem (themes assoc))))
    (map 'list #'to-elem (roles assoc))))



(defmethod to-elem ((fragment FragmentC))
  "transforms all sub-elements of the passed FragmentC instance"
  (to-elem (topic fragment))
  (map 'list #'to-stub-elem (referenced-topics fragment))
  (map 'list #'to-elem (associations fragment)))


(defgeneric to-string (construct)
  (:documentation "Print the string representation of a TM element"))


(defmethod to-string ((construct TopicMapConstructC))
  (cxml:with-xml-output (cxml:make-string-sink :indentation 2 :canonical nil)
    (cxml:with-namespace ("t" *xtm2.0-ns*)
      ;(sb-pcl:class-slots (find-class 'PersistentIdC))
      ;(format t "~a" (length (dom:child-nodes (to-elem construct))))
        (to-elem construct))))


