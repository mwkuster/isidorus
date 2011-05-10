;;+-----------------------------------------------------------------------------
;;+  Isidorus
;;+  (c) 2008-2010 Marc Kuester, Christoph Ludwig, Lukas Georgieff
;;+
;;+  Isidorus is freely distributable under the LLGPL license.
;;+  You can find a detailed description in trunk/docs/LLGPL-LICENSE.txt and
;;+  trunk/docs/LGPL-LICENSE.txt.
;;+-----------------------------------------------------------------------------


;; TODOs (in descending priority):
;; * resolve non-local topicRefs
;; * either check the input document for XTM2.0 conformance in advance or
;;   raise some kind of error (--> condition) if something goes wrong. 

(defpackage :xtm-importer
  (:use :cl :cxml :elephant :datamodel :isidorus-threading :base-tools)
  (:import-from :constants
		*type-instance-psi*
		*type-psi*
		*instance-psi*
                *XTM2.0-NS*
		*XTM1.0-NS*
		*XTM1.0-XLINK*
		*XML-STRING*
		*XML-URI*
		*topic-name-psi*)
  (:import-from :xml-constants
		*core_psis.xtm*)
  (:import-from :xml-tools
                get-attribute
                 xpath-fn-string
                 xpath-child-elems-by-qname
                 xpath-single-child-elem-by-qname
                 xpath-select-location-path
                 xpath-select-single-location-path)
  (:import-from :exceptions
                missing-reference-error
                duplicate-identifier-error)
  (:export :create-instanceof-association
	   :from-association-elem
	   :from-name-elem
	   :from-occurrence-elem
	   :from-role-elem
	   :from-scope-elem
	   :from-topic-elem-to-stub
	   :from-type-elem
	   :get-topicref-uri
           :import-only-topics
           :import-from-xtm
	   :importer 
	   :init-isidorus
	   :merge-topic-elem 
	   :setup-repository
	   :get-topicid-by-psi
	   :get-topic-id-xtm1.0
	   :from-resourceRef-elem-xtm1.0
	   :from-baseName-elem-xtm1.0
	   :from-variant-elem-xtm1.0
	   :from-topicRef-elem-xtm1.0
	   :from-resourceX-elem-xtm1.0
	   :from-variant-elem-xtm1.0
	   :from-parameters-elem-xtm1.0
	   :get-xlink-attribute
	   :get-instanceOf-refs-xtm1.0
	   :from-roleSpec-elem-xtm1.0
	   :from-scope-elem-xtm1.0
	   :from-occurrence-elem-xtm1.0
	   :from-subjectIdentity-elem-xtm1.0
	   :from-member-elem-xtm1.0
	   :from-topic-elem-to-stub-xtm1.0
	   :merge-topic-elem-xtm1.0
	   :from-association-elem-xtm1.0
	   :importer-xtm1.0
	   :get-uuid
           :with-tm))

(in-package :xtm-importer)


(defun get-topicref-uri (topicref-elem)
  "Extract the uri from a topicref"
  ;TODO: at present, this resolves only local topicRefs by cutting off
  ;the first character ('#')
  (declare (dom:element topicref-elem))
  (let 
      ((topicref (get-attribute topicref-elem "href")))
    (unless (char= (elt topicref 0) #\#)
      (error "cannot handle topicrefs that don't start with #"))
    (subseq topicref 1)))

(defun get-topicid-by-psi (uri &key (xtm-id d:*current-xtm*) (revision *TM-REVISION*))
  (when uri
    (loop for item in 
         (topic-identifiers
          (identified-construct (elephant:get-instance-by-value 'PersistentIdC 'uri uri)) :revision revision)
       when (string= xtm-id (xtm-id item))
       return (uri item))))


(defmacro with-tm ((revision xtm-id tm-id) &body body)
  "creates a topic map object called tm and puts it into the local scope"
  `(let ((ii (make-construct 'ItemIdentifierC 
			     :uri ,tm-id
			     :start-revision ,revision)))
     (let ((tm 
	    (make-construct 'TopicMapC 
			    :start-revision ,revision
			    :xtm-id ,xtm-id
			    :item-identifiers (list ii))))
       (declare (ItemIdentifierC ii))
       (declare (TopicMapC tm))
       ,@body)))


(defun init-isidorus (&optional (revision (get-revision)))
  "Initiatlize the database with the stubs of the core topics + PSIs
defined in the XTM 1.0 spec. This includes a topic that represents the
core TM"
  (with-writer-lock
    (with-tm (revision "core.xtm" "http://www.topicmaps.org/xtm/1.0/core.xtm")
      (let
	  ((core-dom 
	    (cxml:parse-file *core_psis.xtm* (cxml-dom:make-dom-builder))))
	(loop for top-elem across 
	     (xpath-child-elems-by-qname (dom:document-element core-dom)
					 *xtm2.0-ns* "topic")
	   do
	     (let
		 ((top
		   (from-topic-elem-to-stub top-elem revision :xtm-id "core.xtm")))
	       (add-to-tm tm top)))))))


;TODO: replace the two importers with this macro
(defmacro importer-mac
    (get-topic-elems get-association-elems 
     from-topic-elem-to-stub merge-topic-elem from-association-elem)
  `(lambda (xtm-dom &key (xtm-id d:*current-xtm*) (revision (get-revision)))
    (declare (dom:element xtm-dom))
    (declare (integer revision))        ;all topics that are imported in one go share the same revision
    
    (let
        ((topic-vector (,get-topic-elems xtm-dom))
         (assoc-vector (,get-association-elems xtm-dom)))
      (loop for top-elem across topic-vector do
           (,from-topic-elem-to-stub top-elem revision :xtm-id xtm-id))
      (loop for top-elem across topic-vector do
           (format t "t")
           (,merge-topic-elem top-elem revision :xtm-id xtm-id))
      (loop for assoc-elem across assoc-vector do
           (,from-association-elem assoc-elem revision :xtm-id xtm-id)))))
  

(defun create-instanceof-association (topicid-of-supertype player2-obj start-revision 
                                      &key 
                                      tm
                                      (xtm-id *current-xtm*))
  "handle the instanceOf element. The instanceOf element is different
  from all the others in that it is not modelled one to one, but
  following the suggestion of the XTM 2.0 spec (4.9) and the
  TMDM (7.2) as an association"
 ;instanceOf = element instanceOf { topicRef+ }
  (declare (string topicid-of-supertype))
  (declare (TopicC player2-obj))
  (declare (TopicMapC tm))
  (let
      ((associationtype 
        (get-item-by-psi *type-instance-psi* :revision start-revision))
       (roletype1
        (get-item-by-psi *type-psi* :revision start-revision))
       (roletype2
        (get-item-by-psi *instance-psi* :revision start-revision))
       (player1
	(get-item-by-id topicid-of-supertype 
			:xtm-id xtm-id 
			:revision start-revision)))
    (unless (and associationtype roletype1 roletype2)
      (error "Error in the creation of an instanceof association: core topics are missing"))
    (unless player1 
      (error
       (make-condition 'missing-reference-error
                       :message "could not find type topic (first player)"
                       :reference topicid-of-supertype)))
    (add-to-tm tm associationtype)
    (add-to-tm tm roletype1)
    (add-to-tm tm roletype2)
    (add-to-tm 
     tm
     (make-construct 
      'AssociationC
      :item-identifiers nil
      :themes nil
      :start-revision start-revision
      :instance-of associationtype
      :roles (list (list :start-revision start-revision
			 :instance-of roletype1
			 :player player1)
                   (list :start-revision start-revision
			 :instance-of roletype2
			 :player player2-obj))))))
