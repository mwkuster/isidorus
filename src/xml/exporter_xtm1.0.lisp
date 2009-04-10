;;+-----------------------------------------------------------------------------
;;+  Isidorus
;;+  (c) 2008-2009 Marc Kuester, Christoph Ludwig, Lukas Giessmann
;;+
;;+  Isidorus is freely distributable under the LGPL license.
;;+  You can find a detailed description in trunk/docs/LGPL-LICENSE.txt.
;;+-----------------------------------------------------------------------------


(defpackage :exporter
  (:use :cl :cxml :elephant :datamodel)
  (:import-from :constants
                *XTM2.0-NS*
		*XTM1.0-NS*
		*XTM1.0-XLINK*)
  (:export :to-elem 
           :to-string
           :list-extern-associations
	   :export-xtm
	   :export-xtm-to-string
	   :export-xtm-fragment))

(in-package :exporter)

(defparameter *export-tm* nil "TopicMap which is exported (nil if all is to be exported")

(defgeneric to-elem-xtm1.0 (instance)
  (:documentation "converts the Topic Maps construct instance to an XTM 1.0 element"))


(defun to-topicRef-elem-xtm1.0 (topic)
  (declare (TopicC topic))
  (cxml:with-element "t:topicRef"
    (cxml:attribute "xlink:href" (format nil "#~a" (topicid topic)))))


(defun to-resourceX-elem-xtm1.0 (characteristic)
  (declare (CharacteristicC characteristic))
  (let ((characteristic-value
	 (if (slot-boundp characteristic 'charvalue)
	     (charvalue characteristic)
	     ""))
	(characteristic-type 
	 (if (slot-boundp characteristic 'datatype)
	     (datatype characteristic)
	     "")))
  (if (string= characteristic-type  "http://www.w3.org/2001/XMLSchema#anyURI")
      (cxml:with-element "t:resourceRef"
	(cxml:attribute "xlink:href"
			(let ((ref-topic (when (and (> (length characteristic-value) 0)
						    (eql (elt characteristic-value 0) #\#))
					   (get-item-by-id (subseq characteristic-value 1)))))
			  (if ref-topic (concatenate 'string "#" (topicid ref-topic)) characteristic-value))))
      (cxml:with-element "t:resourceData"
	(cxml:text characteristic-value)))))


(defmethod to-elem-xtm1.0 ((psi PersistentIdC))
  "subjectIndocatorRef = element subjectIndicatorRef { href }"
  (cxml:with-element "t:subjectIndicatorRef"
    (cxml:attribute "xlink:href" (uri psi))))


(defun to-instanceOf-elem-xtm1.0 (topic)
  "instanceOf = element instanceOf { topicRef | subjectIndicatorRef }"
  (declare (TopicC topic))
  (cxml:with-element "t:instanceOf"
    (cxml:with-element "t:topicRef"
      (cxml:attribute "xlink:href" (concatenate 'string "#" (topicid topic))))))


(defun to-subjectIdentity-elem-xtm1.0 (psis locator)
  "subjectIdentity = element subjectIdentity { resourceRef?,
                       (topicRef | subjectIndicatorRef)* }"
  (when (or psis locator)
    (cxml:with-element "t:subjectIdentity"
      (map 'list #'to-elem-xtm1.0 psis)
      (when locator
	(cxml:with-element "t:resourceRef"
	  (cxml:attribute "xlink:href" (uri locator)))))))


(defun to-scope-elem-xtm1.0 (scopable)
  "scope = element scope { (topicRef | resourceRef | subjectIndicatorRef)+ }"
  (declare (ScopableC scopable))
  (cxml:with-element "t:scope"
    (to-topicRef-elem-xtm1.0 (first (themes scopable)))))


(defmethod to-elem-xtm1.0 ((variant VariantC))
  "variant = element { parameters, variantName?, variant* }"
  (cxml:with-element "t:variant"
    (when (themes variant)
      (cxml:with-element "t:parameters"
	(map 'list #'to-topicRef-elem-xtm1.0 (themes variant))))
    (cxml:with-element "t:variantName"
      (to-resourceX-elem-xtm1.0 variant))))


(defmethod to-elem-xtm1.0 ((name NameC))
  "baseName = element baseName { scope?, baseNameString, variant* }"
  (cxml:with-element "t:baseName"
    (when (themes name)
      (to-scope-elem-xtm1.0 name))
    (cxml:with-element "t:baseNameString"
      (cxml:text (if (slot-boundp name 'charvalue)
		     (charvalue name)
		     "")))
    (when (variants name)
      (map 'list #'to-elem-xtm1.0 (variants name)))))


(defmethod to-elem-xtm1.0 ((occurrence OccurrenceC))
  "occurrence = element occurrence { instanceOf?, scope?,
                   (resourceRef | resourceData) }"
  (cxml:with-element "t:occurrence"
    (when (instance-of occurrence)
      (to-instanceOf-elem-xtm1.0 (instance-of occurrence)))
    (when (themes occurrence)
      (to-scope-elem-xtm1.0 occurrence))
    (to-resourceX-elem-xtm1.0 occurrence)))


(defmethod to-elem-xtm1.0 ((topic TopicC))
  "topic = element topic { id, instanceOf*, subjectIdentity,
                           (baseName | occurrence)* }"
  (cxml:with-element "t:topic"
    (cxml:attribute "id" (topicid topic))
    (when (list-instanceOf topic :tm *export-tm*)
      (map 'list #'to-instanceOf-elem-xtm1.0 (list-instanceOf topic :tm *export-tm*)))
    (when (or (psis topic) (locators topic))
      (to-subjectIdentity-elem-xtm1.0 (psis topic) (first (locators topic))))
    (when (names topic)
      (map 'list #'to-elem-xtm1.0 (names topic)))
    (when (occurrences topic)
      (map 'list #'to-elem-xtm1.0 (occurrences topic)))))


(defun to-roleSpec-elem-xtm1.0 (topic)
  "roleSpec = element roleSpec { topicRef | subjectIndicatorRef }"
  (cxml:with-element "t:roleSpec"
    (to-topicRef-elem-xtm1.0 topic)))


(defmethod to-elem-xtm1.0 ((role RoleC))
  "member = element member { roleSpec?,
              (topicRef | resourceRef | subjectIndicatorRef)+ }"
  (cxml:with-element "t:member"
    (when (instance-of role)
      (to-roleSpec-elem-xtm1.0 (instance-of role)))
    (to-topicRef-elem-xtm1.0 (player role))))


(defmethod to-elem-xtm1.0 ((association AssociationC))
  "association = element association { instanceOf?, scope?, member+ }"
  (cxml:with-element "t:association"
    (when (instance-of association)
      (to-instanceOf-elem-xtm1.0 (instance-of association)))
    (when (themes association)
      (to-scope-elem-xtm1.0 association))
    (map 'list #'to-elem-xtm1.0 (roles association))))


(defun to-stub-elem-xtm1.0 (topic)
  "transforms a TopicC object to a topic stub element
   with a topicid, psis and subjectLocators"
  (declare (TopicC topic))
  (cxml:with-element "t:topic"
    (cxml:attribute "id" (topicid topic))
    (to-subjectIdentity-elem-xtm1.0 (psis topic) (first (locators topic)))))


(defmethod to-elem-xtm1.0 ((fragment FragmentC))
  "transforms all sub-elements of the passed FragmentC instance"
  (to-elem-xtm1.0 (topic fragment))
  (map 'list #'to-stub-elem-xtm1.0 (referenced-topics fragment))
  (map 'list #'to-elem-xtm1.0 (associations fragment)))


