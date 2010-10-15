;;+-----------------------------------------------------------------------------
;;+  Isidorus
;;+  (c) 2008-2010 Marc Kuester, Christoph Ludwig, Lukas Georgieff
;;+
;;+  Isidorus is freely distributable under the LLGPL license.
;;+  You can find a detailed description in trunk/docs/LLGPL-LICENSE.txt and
;;+  trunk/docs/LGPL-LICENSE.txt.
;;+-----------------------------------------------------------------------------

(in-package :atom)

(defclass collection-feed (feed)
  ((dependency :accessor dependency 
               :initarg :dependency :initform nil
               :type list
               :documentation "URLs of the feeds that this feed depends on")
   (source-locator-prefix 
    :accessor source-locator-prefix
    :initarg :source-locator-prefix))
  (:documentation "abstract class for atom feeds"))

(defmethod feed-to-elem ((feed collection-feed))
  (setf (updated feed) (get-most-recent-datetime-for-tm (id feed)))
  (dolist (dependency (dependency feed))
    (to-elem "e:dependency" dependency)))

(defclass collection-entry (entry)
  ((link-type :accessor link-type
              :initarg :link-type
              :type symbol
              :documentation "one of 'fragments-feed or 'snapshots-feed")
   (tm-id :accessor tm-id
          :initarg :tm-id
          :type string))
  (:documentation "Class that represents an entry for a fragments feed or snapshots feed in a collection feed"))

(defmethod entry-to-elem ((entry collection-entry))
  (setf (updated entry) (get-most-recent-datetime-for-tm (tm-id entry)))
  (to-link (link entry) 
           (if (eq 'snapshots-feed (link-type entry))
               "http://www.egovpt.org/sdshare/snapshotsfeed"
               "http://www.egovpt.org/sdshare/fragmentsfeed") "application/atom+xml"))

