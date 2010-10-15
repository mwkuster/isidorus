;;+-----------------------------------------------------------------------------
;;+  Isidorus
;;+  (c) 2008-2010 Marc Kuester, Christoph Ludwig, Lukas Georgieff
;;+
;;+  Isidorus is freely distributable under the LLGPL license.
;;+  You can find a detailed description in trunk/docs/LLGPL-LICENSE.txt and
;;+  trunk/docs/LGPL-LICENSE.txt.
;;+-----------------------------------------------------------------------------

(in-package :atom)

(defclass fragment-entry (entry)
  ((summary :accessor summary :initarg :summary)
   (psi :accessor psi :initarg :psi))
  (:documentation "a fragment in a fragment feed"))

(defclass fragments-feed (feed)
  ((source-locator-prefix :accessor source-locator-prefix
                          :initarg :source-locator-prefix
                          :type string)
   (tm-id :accessor tm-id
          :initarg :tm-id
          :type string)))

(defmethod entry-to-elem ((entry fragment-entry))
  (to-link (link entry) "alternate" "application/x-tm+xml;version=1.0")
  (to-elem "a:summary" (summary entry))
  (to-elem "e:TopicSI" (psi entry)))

(defmethod feed-to-elem ((feed fragments-feed))
  (setf (updated feed) (get-most-recent-datetime-for-tm (tm-id feed)))
  (to-elem "e:ServerSrcLocatorPrefix" (source-locator-prefix feed)))

(defmethod entries ((feed fragments-feed))
  "Unlike for the other feed types, entries can be calculated"
  (remove 
   nil
   (with-writer-lock
     (loop for fragment in 
	  (mapcan #'d:get-fragments (rest (d:get-all-revisions)))
	collect 
	  (let
	      ((tm (d:get-item-by-item-identifier (tm-id feed) :revision 0))
	       (xtm-link (format nil "~a/~a" 
				 (link feed) (d:unique-id fragment)))
	       (psi (d:uri (first (d:psis (d:topic fragment))))))
	    (when (d:in-topicmap tm (d:topic fragment))
	      (make-instance 'fragment-entry
			     :id xtm-link
			     :title psi
			     :psi psi
			     :path (format nil "~a/~a" (path feed) (d:unique-id fragment))
			     :updated (datetime-in-iso-format (d:revision fragment))
			     :link xtm-link
			     :summary (format nil "Fragment for topic ~a" psi))))))))


;; (defun build-fragments-feed (tm-id)
;;   "Build a feed of changes for the Topic Map identified by tm-id from the revisions in the engine"
;;   (loop for fragment in 
;;        (mapcan #'d:get-fragments (rest (d:get-all-revisions)))
;;      collect 
;;        (register-entry
;;         *testtm-fragments-feed*
;;         (let
;;             ((xtm-link (format nil "~a~a" 
;;                                (link *testtm-fragments-feed*) (d:unique-id fragment)))
;; 	     (psi (d:uri (first (d:psis (d:topic fragment))))))
              
;;           (make-instance 'fragment-entry
;;                          :title psi
;;                          :psi psi
;; 			 :path (format nil "~a" (d:unique-id fragment))
;;                          :updated (datetime-in-iso-format (d:revision fragment))
;;                          :xtm-link xtm-link
;;                          :description (format nil "Fragment for topic with psi~a" psi)))))
;;   (feed-to-elem *testtm-fragments-feed*))
