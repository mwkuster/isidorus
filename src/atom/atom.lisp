(defpackage :atom
  (:use :cl :cxml :constants :xml-tools :datamodel :drakma)
  (:export :collection-feed
           :defsite
           :dependency
           :entries
           :feed
           :feed-to-elem
           :feed-to-string
           :fragments-feed
           :id
           :link
           :parse-fragments-feed
           :parse-snapshots-feed
           :path
           :psi
           :snapshots-feed
           :source-locator-prefix
           :subfeeds
           :tm-id
           :tm-id-p
	   :updated
           :*base-url*
           :*tm-feed*))

(in-package :atom)

;;General configuration options
(defparameter *base-url* "") ;*base-url* is set by hunchentoot
(defvar *author*)
(defvar *source-locator*)
(defparameter *tm-feed* nil)
(defvar *testtm-snapshotfeed*)
(defvar *testtm-fragmentfeed*)
(defvar *testtm-feed*)
(defvar *testtm-toplevel*)
(defvar *testtm-snapshots-feed*)
(defvar *testtm-fragments-feed*)

(defun datetime-in-iso-format (&optional (seconds-since-epoch (get-universal-time)))
  "Formats a time (seconds since epoch) in ISO format. If no parameter
  is given, return the current time in ISO format"
  (multiple-value-bind
	(second minute hour date month year day daylight-p zone)
      (decode-universal-time seconds-since-epoch)
    (declare (ignore day))
    (let*
	((offset
	  (if daylight-p 0 -1))
	 (timezone
	  (if (>= (+ zone offset) 0)
	      (format nil "+~2,'0d:00" zone)
	      (format nil "-~2,'0d:00" (abs zone))))) 
	 (format nil "~d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0d~@d"
		 year month date hour minute second timezone))))

(defun get-most-recent-datetime ()
  "Gets the most datetime of the most recent revision"
  (datetime-in-iso-format (apply #'max (get-all-revisions))))

(defun get-most-recent-datetime-for-tm (tm-id)
  "Gets the most datetime of the most recent revision for a given TM"
  (datetime-in-iso-format (apply #'max (get-all-revisions-for-tm tm-id))))

(defun to-link (href rel &optional application-type)
  "Generates an Atom link element. The function is expected to be used
in the in-feed macro"
  (cxml:with-element "a:link"
    (cxml:attribute "rel" rel)
    ;goes around a stupid IE bug
    (when application-type
      (cxml:attribute "type" application-type))
    (cxml:attribute "href" href)))

(defun to-elem (qname text)
  (cxml:with-element qname (if (typep text 'string) (cxml:text text) (format nil "~a" text))))

(defmacro in-feed ((title subtitle author link) &body body)
  "Builds the element structure for the outer elements of an Atom feed. Expects a list of entry elements in the body"
  `(cxml:with-namespace ("a" *atom-ns*)
     (cxml:with-namespace ("e" *egovpt-ns*)
       (cxml:with-element "a:feed"
         (to-elem "a:title" ,title)
         (when ,subtitle
           (to-elem "a:subtitle" ,subtitle))
         (to-elem "a:id" ,link)
         (cxml:with-element "a:author"
           (to-elem "a:name" ,author))
         (to-link ,link "self")
         ,@body))))

(defmacro feed-to-string (&body body)
  "Serialize a feed as a string"
  `(cxml:with-xml-output (cxml:make-string-sink :canonical t)
    ,@body))

(defclass atom-element () 
  ((title :accessor title :initarg :title)
   (updated :accessor updated :initarg :updated :initform nil)
   (id :accessor id :initarg :id :initform (error "An atom element must always have an id"))
   (path :accessor path :initarg :path)
   (link :initarg :link)))

(defgeneric link (ae)
  (:documentation "calculate the link of an atom element (entry or feed). The link can be realized as a self link (for feeds) or an alternate link (for entries)"))

(defmethod link ((ae atom-element))
  (if (slot-boundp ae 'link)
      (slot-value ae 'link)
      (format nil "~a/~a" *base-url* (path ae))))

(defclass feed (atom-element)
  ((subtitle :accessor subtitle :initarg :subtitle :initform nil)
   (author :accessor author :initarg :author)   
   (entries :accessor entries :initarg :entries :initform nil
            :type list)
   (subfeeds :accessor subfeeds :initarg :subfeeds :initform nil
             :type list))
  (:documentation "abstract class for atom feeds"))


(defclass entry (atom-element) 
  ()
  (:documentation "Class that represents a minimalistic entry in an
  Atom feed. Concrete classes implement the correct behaviour for the
  individual entry types in the Atom protocol"))

(defgeneric register-entry (feed entry)
  (:documentation "Register an entry for a given feed"))

(defmethod register-entry ((feed feed) (entry entry))
  (format t "feed: ~s; entry: ~s" feed entry)
  (push entry (slot-value feed 'entries))
  (format t "entries of ~s: ~s" feed (slot-value feed 'entries)))

(defgeneric register-subfeed (feed subfeed)
  (:documentation "Register a subfeed for a given feed"))

(defmethod register-subfeed ((feed feed) (subfeed feed))
  (push subfeed (subfeeds feed)))

(defgeneric entry-to-elem (entry)
  (:documentation "build an entry element"))

(defmethod entry-to-elem :around ((entry entry))
  (cxml:with-namespace ("a" *atom-ns*)
    (cxml:with-namespace ("e" *egovpt-ns*)
      (cxml:with-element "a:entry"
        (to-elem "a:title" (title entry))
        (to-elem "a:id" (id entry))
        (to-link (link entry) "alternate") ;this version of the alternate link works around an IE bug
        (call-next-method)
        (to-elem "a:updated" (updated entry))))))

(defmethod entry-to-elem ((entry entry))
  ;do nothing
  )

(defmethod id ((entry entry))
  ;TODO: consider to revisit that convention
  (format nil "~a/~a" (slot-value entry 'id) "entry"))

(defclass overview-entry (entry)
  ((author :accessor author :initarg :author))
  (:documentation "Class that represents an entry for a collection in an overview feed"))

(defmethod entry-to-elem ((entry overview-entry))
  (to-link (link entry) "alternate" "application/atom+xml")
  (cxml:with-element "a:author"
    (to-elem "a:name" (author entry)))
  (setf (updated entry) (get-most-recent-datetime-for-tm (slot-value entry 'id)))
  (to-link (link entry) "http://www.egovpt.org/sdshare/collectionfeed" "application/atom+xml"))
 
(defgeneric feed-to-elem (feed)
  (:documentation "Render an Atom feed to XML"))

(defmethod feed-to-elem :around ((feed feed))
   (in-feed
       ((title feed) (subtitle feed) (author feed) (link feed))
     (call-next-method)
     (to-elem "a:updated" (updated feed))
     (map 'list #'entry-to-elem (entries feed))))

(defmethod feed-to-elem ((feed feed))
  (setf (updated feed) (get-most-recent-datetime)))

;; (defun build-overview-feed ()
;;   (feed-to-elem *tm-feed*))
