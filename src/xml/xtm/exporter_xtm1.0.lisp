;;+-----------------------------------------------------------------------------
;;+  Isidorus
;;+  (c) 2008-2010 Marc Kuester, Christoph Ludwig, Lukas Georgieff
;;+
;;+  Isidorus is freely distributable under the LLGPL license.
;;+  You can find a detailed description in trunk/docs/LLGPL-LICENSE.txt and
;;+  trunk/docs/LGPL-LICENSE.txt.
;;+-----------------------------------------------------------------------------

(defpackage :xtm-exporter
  (:use :cl :cxml :elephant :datamodel :isidorus-threading :base-tools)
  (:import-from :constants
                *XTM2.0-NS*
		*XTM1.0-NS*
		*XTM1.0-XLINK*
		*type-psi*
		*instance-psi*
		*type-instance-psi*
		*topic-name-psi*
		*xml-uri*
		*xml-string*)
  (:export :to-elem 
           :to-string
           :list-extern-associations
	   :export-as-xtm
	   :export-as-xtm-string
	   :export-construct-as-xtm-string))

(in-package :xtm-exporter)

(defparameter *export-tm* nil "TopicMap which is exported (nil if all is to be exported")

(defgeneric to-elem-xtm1.0 (instance revision)
  (:documentation "converts the Topic Maps construct instance to an XTM 1.0 element"))


(defun to-topicRef-elem-xtm1.0 (topic revision)
  (declare (TopicC topic)
	   (type (or integer nil) revision))
  (cxml:with-element "t:topicRef"
    (cxml:attribute "xlink:href" (format nil "#~a" (topic-id topic revision)))))


(defun to-reifier-elem-xtm1.0 (reifiable-construct revision)
  "Exports an ID indicating a reifier.
   The reifier is only exported if the reifier-topic contains a PSI starting with #.
   This may cause differences since the xtm2.0 defines the referencing
   of reifiers with item-identifiers."
  (declare (ReifiableConstructC reifiable-construct)
	   (type (or integer nil) revision))
  (when (reifier reifiable-construct :revision revision)
    (let ((reifier-psi
	   (find-if #'(lambda(x)
			(when (and (stringp (uri x))
				   (> (length (uri x)) 0))
			  (eql (elt (uri x) 0) #\#)))
		    (psis (reifier reifiable-construct :revision revision)
			  :revision revision))))
      (when reifier-psi
	(cxml:attribute "id" (subseq (uri reifier-psi) 1
				     (length (uri reifier-psi))))))))
		    

(defun to-resourceX-elem-xtm1.0 (characteristic revision)
  (declare (CharacteristicC characteristic)
	   (type (or integer nil) revision))
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
	(cxml:attribute
	 "xlink:href"
	 (let ((ref-topic (when (and (> (length characteristic-value) 0)
				     (eql (elt characteristic-value 0) #\#))
			    (get-item-by-id (subseq characteristic-value 1)
					    :revision revision))))
	   (if ref-topic (concat "#" (topic-id ref-topic revision))
	       characteristic-value))))
      (cxml:with-element "t:resourceData"
	(cxml:text characteristic-value)))))


(defmethod to-elem-xtm1.0 ((psi PersistentIdC) revision)
  "subjectIndocatorRef = element subjectIndicatorRef { href }"
  (declare (ignorable revision))
  (cxml:with-element "t:subjectIndicatorRef"
    (cxml:attribute "xlink:href" (uri psi))))


(defun to-instanceOf-elem-xtm1.0 (topic revision)
  "instanceOf = element instanceOf { topicRef | subjectIndicatorRef }"
  (declare (TopicC topic)
	   (type (or integer nil) revision))
  (cxml:with-element "t:instanceOf"
    (cxml:with-element "t:topicRef"
      (cxml:attribute "xlink:href" (concat "#" (topic-id topic revision))))))


(defun to-subjectIdentity-elem-xtm1.0 (psis locator revision)
  "subjectIdentity = element subjectIdentity { resourceRef?,
                       (topicRef | subjectIndicatorRef)* }"
  (declare (type (or integer nil) revision))
  (when (or psis locator)
    (cxml:with-element "t:subjectIdentity"
      (map 'list #'(lambda(x)
		     (to-elem-xtm1.0 x revision))
	   psis)
      (when locator
	(cxml:with-element "t:resourceRef"
	  (cxml:attribute "xlink:href" (uri locator)))))))


(defun to-scope-elem-xtm1.0 (scopable revision)
  "scope = element scope { (topicRef | resourceRef | subjectIndicatorRef)+ }"
  (declare (ScopableC scopable)
	   (type (or integer nil) revision))
  (cxml:with-element "t:scope"
    (to-topicRef-elem-xtm1.0 (first (themes scopable :revision revision)) revision)))


(defmethod to-elem-xtm1.0 ((variant VariantC) revision)
  "variant = element { parameters, variantName?, variant* }"
  (declare (type (or integer nil) revision))
  (cxml:with-element "t:variant"
    (to-reifier-elem-xtm1.0 variant revision)
    (when (themes variant :revision revision)
      (cxml:with-element "t:parameters"
	(map 'list #'(lambda(x)
		       (to-topicRef-elem-xtm1.0 x revision))
	     (themes variant :revision revision))))
    (cxml:with-element "t:variantName"
      (to-resourceX-elem-xtm1.0 variant revision))))


(defmethod to-elem-xtm1.0 ((name NameC) revision)
  "baseName = element baseName { scope?, baseNameString, variant* }"
  (declare (type (or integer nil) revision))
  (cxml:with-element "t:baseName"
    (to-reifier-elem-xtm1.0 name revision)
    (when (themes name :revision revision)
      (to-scope-elem-xtm1.0 name revision))
    (cxml:with-element "t:baseNameString"
      (cxml:text (if (slot-boundp name 'charvalue)
		     (charvalue name)
		     "")))
    (when (variants name :revision revision)
      (map 'list #'(lambda(x)
		     (to-elem-xtm1.0 x revision))
	   (variants name :revision revision)))))


(defmethod to-elem-xtm1.0 ((occurrence OccurrenceC) revision)
  "occurrence = element occurrence { instanceOf?, scope?,
                   (resourceRef | resourceData) }"
  (declare (type (or integer nil) revision))
  (cxml:with-element "t:occurrence"
    (to-reifier-elem-xtm1.0 occurrence revision)
    (when (instance-of occurrence :revision revision)
      (to-instanceOf-elem-xtm1.0 (instance-of occurrence :revision revision)
				 revision))
    (when (themes occurrence :revision revision)
      (to-scope-elem-xtm1.0 occurrence revision))
    (to-resourceX-elem-xtm1.0 occurrence revision)))


(defmethod to-elem-xtm1.0 ((topic TopicC) revision)
  "topic = element topic { id, instanceOf*, subjectIdentity,
                           (baseName | occurrence)* }"
  (declare (type (or integer nil) revision))
  (cxml:with-element "t:topic"
    (cxml:attribute "id" (topic-id topic revision))
    (let ((ios (list-instanceOf topic :tm *export-tm* :revision revision)))
      (when ios
	(map 'list #'(lambda(x)
		       (to-instanceOf-elem-xtm1.0 x revision))
	     ios)))
    (let ((t-psis (psis topic :revision revision))
	  (first-locator (when (locators topic :revision revision)
			   (first (locators topic :revision revision)))))
      (when (or t-psis first-locator)
	(to-subjectIdentity-elem-xtm1.0 t-psis first-locator revision)))
    (when (names topic :revision revision)
       (map 'list #'(lambda(x)
		     (to-elem-xtm1.0 x revision))
	   (names topic :revision revision)))
    (when (occurrences topic :revision revision)
      (map 'list #'(lambda(x)
		     (to-elem-xtm1.0 x revision))
	   (occurrences topic :revision revision)))))


(defun to-roleSpec-elem-xtm1.0 (topic revision)
  "roleSpec = element roleSpec { topicRef | subjectIndicatorRef }"
  (declare (type (or integer nil) revision))
  (cxml:with-element "t:roleSpec"
    (to-topicRef-elem-xtm1.0 topic revision)))


(defmethod to-elem-xtm1.0 ((role RoleC) revision)
  "member = element member { roleSpec?,
              (topicRef | resourceRef | subjectIndicatorRef)+ }"
  (declare (type (or integer nil) revision))
  (cxml:with-element "t:member"
    (to-reifier-elem-xtm1.0 role revision)
    (when (instance-of role :revision revision)
      (to-roleSpec-elem-xtm1.0 (instance-of role :revision revision) revision))
    (to-topicRef-elem-xtm1.0 (player role :revision revision) revision)))


(defmethod to-elem-xtm1.0 ((association AssociationC) revision)
  "association = element association { instanceOf?, scope?, member+ }"
  (declare (type (or integer nil) revision))
  (cxml:with-element "t:association"
    (to-reifier-elem-xtm1.0 association revision)
    (when (instance-of association :revision revision)
      (to-instanceOf-elem-xtm1.0 (instance-of association :revision revision) revision))
    (when (themes association :revision revision)
      (to-scope-elem-xtm1.0 association revision))
    (map 'list #'(lambda(x)
		   (to-elem-xtm1.0 x revision))
	 (roles association :revision revision))))


(defun to-stub-elem-xtm1.0 (topic revision)
  "transforms a TopicC object to a topic stub element
   with a topicid, psis and subjectLocators"
  (declare (TopicC topic)
	   (type (or integer nil) revision))
  (cxml:with-element "t:topic"
    (cxml:attribute "id" (topic-id topic revision))
    (to-subjectIdentity-elem-xtm1.0 (psis topic :revision revision)
				    (when (locators topic :revision revision)
				      (first (locators topic :revision revision)))
				    revision)))


(defmethod to-elem-xtm1.0 ((fragment FragmentC) revision)
  "transforms all sub-elements of the passed FragmentC instance"
  (declare (type (or integer nil) revision))
  (to-elem-xtm1.0 (topic fragment) revision)
  (map 'list #'(lambda(x)
		 (to-stub-elem-xtm1.0 x revision))
       (referenced-topics fragment))
  (map 'list #'(lambda(x)
		 (to-elem-xtm1.0 x revision))
       (associations fragment)))


