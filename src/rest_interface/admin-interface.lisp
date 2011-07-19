;;+-----------------------------------------------------------------------------
;;+  Isidorus
;;+  (c) 2008-2010 Marc Kuester, Christoph Ludwig, Lukas Georgieff
;;+
;;+  Isidorus is freely distributable under the LLGPL license.
;;+  You can find a detailed description in trunk/docs/LLGPL-LICENSE.txt and
;;+  trunk/docs/LGPL-LICENSE.txt.
;;+-----------------------------------------------------------------------------

(in-package :rest-interface)

;;TODO: add functions to export statement

(defparameter *admin-local-backup* "/admin/local-backup")
(defparameter *admin-remote-backup* "/admin/remote-backup")
(defparameter *admin-shutdown* "/admin/shutdown")


(defun set-up-admin-interface ()
  (push
   (create-regex-dispatcher *admin-local-backup* #'admin-local-backup)
   hunchentoot:*dispatch-table*)
  (push
   (create-regex-dispatcher *admin-remote-backup* #'admin-remote-backup)
   hunchentoot:*dispatch-table*)
  (push
   (create-regex-dispatcher *admin-shutdown* #'admin-shutdown)
   hunchentoot:*dispatch-table*))



(defun admin-shutdown()
  (handler-case
      (if (string= *admin-remote-name* (hunchentoot:remote-addr*))
	  (progn
	    (when elephant:*store-controller*
	      (xtm-exporter:export-as-xtm
	       (concat "backup_" (make-date-string (get-universal-time)) ".xtm")
	       :tm-id "http://isidor.us/backup-tm"
	       :revision 0))
	    (shutdown-json-engine)
	    (shutdown-atom-engine)
	    (shutdown-admin-server)
	    (close-tm-store)) ;in case the json and atom services are not running
	  (setf (hunchentoot:return-code*) hunchentoot:+http-forbidden+))
    (condition (err)
      (progn
	(tools:close-tm-store)
	(setf (hunchentoot:return-code*) hunchentoot:+http-internal-server-error+)
	(setf (hunchentoot:content-type*) "text")
	(format nil "closed the tm store, but:~%condition: \"~a\"" err)))))

	


(defun admin-local-backup()
  (handler-case
      (if (string= "127.0.0.1" (hunchentoot:remote-addr*))
	  (let ((destination-path
		 (hunchentoot:url-decode (hunchentoot:get-parameter "path"))))
	    (xtm-exporter:export-as-xtm destination-path :revision 0))
	  (setf (hunchentoot:return-code*) hunchentoot:+http-forbidden+))
    (condition (err)
      (progn
	(setf (hunchentoot:return-code*) hunchentoot:+http-internal-server-error+)
	(setf (hunchentoot:content-type*) "text")
	(format nil "Condition: \"~a\"" err)))))


(defun admin-remote-backup()
  (handler-case
      (if (string= "127.0.0.1" (hunchentoot:remote-addr*))
	  (progn (hunchentoot:url-decode (hunchentoot:get-parameter "path"))
		 (setf (hunchentoot:content-type*) "application/xml")
		 (xtm-exporter:export-as-xtm-string :revision 0))
	  (setf (hunchentoot:return-code*) hunchentoot:+http-forbidden+))
    (condition (err)
      (progn
	(setf (hunchentoot:return-code*) hunchentoot:+http-internal-server-error+)
	(setf (hunchentoot:content-type*) "text")
	(format nil "Condition: \"~a\"" err)))))


(defun make-date-string (universal-time)
  (tools:concat
   (write-to-string (nth-value 3 (decode-universal-time universal-time))) "."
   (write-to-string (nth-value 4 (decode-universal-time universal-time))) "."
   (write-to-string (nth-value 5 (decode-universal-time universal-time))) ":"
   (write-to-string (nth-value 2 (decode-universal-time universal-time))) ":"
   (write-to-string (nth-value 1 (decode-universal-time universal-time))) ":"
   (write-to-string (nth-value 0 (decode-universal-time universal-time)))))