;;+-----------------------------------------------------------------------------
;;+  Isidorus
;;+  (c) 2008-2010 Marc Kuester, Christoph Ludwig, Lukas Georgieff
;;+
;;+  Isidorus is freely distributable under the LLGPL license.
;;+  You can find a detailed description in trunk/docs/LLGPL-LICENSE.txt and
;;+  trunk/docs/LGPL-LICENSE.txt.
;;+-----------------------------------------------------------------------------

(in-package :atom)
;the configuration of the eGov application (at present)
;TODO: convert to an XML configuration file for the site
;of type
;<tm-feeds><author><title>...</title>
;<tm-feed><title>...</title><snapshopfeed>...</snapshotfeed>
;...</tm-feed></tm-feeds>
;generate the corresponding feed instances automatically

(defun setup-feeds ()
  (setf *base-url* "http://london.ztt.fh-worms.de:8000")
  (setf *source-locator* "http://psi.egovpt.org/")
  (setf *author* "Isidor")

  (setf *tm-feed*
        (make-instance 'feed 
                       :title "List of TMs on this test server"
                       :subtitle "Test TMs"
                       :updated (datetime-in-iso-format (first (last (d:get-all-revisions))))
                       :path "/feeds/"
                       :author *author*))

  (setf *testtm-snapshots-feed*
        (make-instance 'snapshot-feed 
                       :title "A list of all Test TM snapshots"
                       :subtitle "Snapshots"
		       :source-locator *source-locator*
                       :updated (datetime-in-iso-format (first (last (d:get-all-revisions))))
                       :path "/testtm/snapshots/"
                       :author *author*))

  (setf *testtm-fragments-feed*
        (make-instance 'fragment-feed 
                       :title "A list of all Test TM fragments"
                       :subtitle "Changes"
		       :source-locator *source-locator*
                       :updated (datetime-in-iso-format (first (last (d:get-all-revisions))))
                       :path "/testtm/fragments/"
                       :author *author*))

  (setf *testtm-toplevel*
        (make-instance 'entry
                       :title "Test TM"
                       :updated (datetime-in-iso-format (first (last (d:get-all-revisions))))
                       :path "/feeds/testtm/"))

  (register-entry *tm-feed* *testtm-toplevel*)

  (setf *testtm-feed*
        (make-instance 'feed
                       :title (title *testtm-toplevel*)
		       :subtitle "Entry-Feed for the Test TM"
                       :updated (updated *testtm-toplevel*)
		       :author *author* 
                       :path (path *testtm-toplevel*)))

  (setf *testtm-snapshotfeed*
        (make-instance 'tm-entry 
               :title (format nil "~a: Snapshots" (title *testtm-toplevel*))
	       :feedtype "snapshotsfeed"
               :updated (updated *testtm-toplevel*)
	       :path (self-link *testtm-snapshots-feed*)
               :feed *testtm-snapshots-feed*))

  (setf *testtm-fragmentfeed*
        (make-instance 'tm-entry 
               :title (format nil "~a: Changes" (title *testtm-toplevel*))
	       :feedtype "fragmentsfeed"
               :updated (updated *testtm-toplevel*)
	       :path (self-link *testtm-fragments-feed*)
               :feed *testtm-fragments-feed*))

  (register-entry *testtm-feed* *testtm-snapshotfeed*)
  (register-entry *testtm-feed* *testtm-fragmentfeed*))
   