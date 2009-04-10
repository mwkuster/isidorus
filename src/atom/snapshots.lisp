;;+-----------------------------------------------------------------------------
;;+  Isidorus
;;+  (c) 2008-2009 Marc Kuester, Christoph Ludwig, Lukas Giessmann
;;+
;;+  Isidorus is freely distributable under the LGPL license.
;;+  You can find a detailed description in trunk/docs/LGPL-LICENSE.txt.
;;+-----------------------------------------------------------------------------


(in-package :atom)

(defclass snapshots-feed (feed)
  ((source-locator-prefix :accessor source-locator-prefix
                          :initarg :source-locator-prefix
                          :type string)
   (tm-id :accessor tm-id
          :initarg :tm-id
          :type string)))

(defclass snapshot-entry (entry)
  ()
  (:documentation "a snapshot in a snapshot feed"))

(defmethod entry-to-elem ((entry snapshot-entry))
  (to-link (link entry) "alternate" "application/x-tm+xml;version=1.0"))

(defmethod feed-to-elem ((feed snapshots-feed))
  (setf (updated feed) (get-most-recent-datetime-for-tm (tm-id feed)))
  (to-elem "e:ServerSrcLocatorPrefix" (source-locator-prefix feed)))

(defmethod entries ((feed snapshots-feed))
  (loop for revision in (d:get-all-revisions-for-tm (tm-id feed)) 
     collect 
       (let
           ((link
             (format nil "~a/~a" (link feed) revision)))
          (make-instance 'snapshot-entry
                         :id link
                         :title (format nil "Snapshot ~a" revision)
                         :updated (datetime-in-iso-format revision)
                         :link link
			 :path (format nil "~a/~a" (path feed) revision)))))

;; (defun build-snapshots-feed (tm-id)
;;   "Build a feed of snapshots for the Topic Map with the given tm-id
;; from the revisions in the engine"
;;   ;Strings to be replaced by configuration options in a config file
;;   (loop for revision in (d:get-all-revisions-for-tm tm-id) 
;;      collect 
;;        (let
;;            ((link
;;              (format nil "~a~a" (link *testtm-snapshots-feed*)
;;                                            revision)))
;;          (register-entry
;;           *testtm-snapshots-feed*
;;           (make-instance 'snapshot-entry
;;                          :title (format nil "Snapshot ~a" revision)
;;                          :updated (datetime-in-iso-format revision)
;;                          :link link
;; 			 :path (format nil "~a" revision)))))
                         
;;   (feed-to-elem *testtm-snapshots-feed*))
