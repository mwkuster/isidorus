(in-package :atom)

(defclass collection-feed (feed)
  ((depends-on :accessor depends-on 
               :initarg :depends-on :initform nil
               :type list
               :documentation "URLs of the feeds that this feed depends on")
   (source-locator-prefix 
    :accessor source-locator-prefix
    :initarg :source-locator-prefix))
  (:documentation "abstract class for atom feeds"))

(defmethod feed-to-elem ((feed collection-feed))
  (setf (updated feed) (get-most-recent-datetime-for-tm (id feed)))
  (to-elem "e:depends-on" (depends-on feed)))

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

