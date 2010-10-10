;;+-----------------------------------------------------------------------------
;;+  Isidorus
;;+  (c) 2008-2009 Marc Kuester, Christoph Ludwig, Lukas Giessmann
;;+
;;+  Isidorus is freely distributable under the LGPL license.
;;+  You can find a detailed description in trunk/docs/LGPL-LICENSE.txt.
;;+-----------------------------------------------------------------------------


(defpackage :rest-interface
  (:nicknames :rest)
  (:use :cl :hunchentoot 
        :constants
        :atom 
        :datamodel
        :exporter
        :xml-tools
        :xml-importer
	:json-exporter
	:json-importer
        :isidorus-threading)
  (:export :import-fragments-feed
           :import-snapshots-feed
           :import-tm-feed
           :read-url
           :read-fragment-feed
           :start-tm-engine
	   :shutdown-tm-engine
	   :*json-get-prefix*
	   :*get-rdf-prefix*
	   :*json-commit-url*
	   :*json-get-all-psis*
	   :*json-get-summary-prefix*
	   :*json-get-all-type-psis*
	   :*json-get-all-instance-psis*
	   :*json-get-topic-stub-prefix*
	   :*json-get-type-tmcl-prefix*
	   :*json-get-instance-tmcl-prefix*
	   :*json-get-overview*
	   :*ajax-user-interface-url*
	   :*ajax-user-interface-file-path*
	   :*ajax-javascript-directory-path*
	   :*ajax-javascript-url-prefix*))


(in-package :rest-interface)

(defun create-regex-dispatcher (regex page-function)
  "Just like hunchentoot:create-regex-dispatcher except it extracts the matched values
   and passes them onto PAGE-FUNCTION as arguments. 

Copied from http://uint32t.blogspot.com/2007/12/restful-handlers-with-hunchentoot.html"
  (let ((scanner (cl-ppcre:create-scanner regex)))
    (lambda (request)
      (multiple-value-bind (whole-match matched-registers)
          (cl-ppcre:scan-to-strings scanner (hunchentoot:script-name request))
        (when whole-match
          (lambda ()
            (apply page-function (coerce matched-registers 'list))))))))


(defvar *server-acceptor* nil)


(defun start-tm-engine (repository-path &key (conffile "atom/conf.lisp")
			(host-name "localhost") (port 8000))
  "Start the Topic Map Engine on a given port, assuming a given
   hostname. Use the repository under repository-path"
  (when *server-acceptor*
    (error "Ther server is already running"))
  (setf hunchentoot:*show-lisp-errors-p* t) ;for now
  (setf hunchentoot:*hunchentoot-default-external-format* 
	(flex:make-external-format :utf-8 :eol-style :lf))
  (setf atom:*base-url* (format nil "http://~a:~a" host-name port))
  (unless elephant:*store-controller*
    (elephant:open-store  
     (xml-importer:get-store-spec repository-path)))
  (load conffile)
  (publish-feed atom:*tm-feed*)
  (set-up-json-interface)
  (setf *server-acceptor* (make-instance 'hunchentoot:acceptor :address host-name :port port))
  (setf hunchentoot:*lisp-errors-log-level* :info)
  (setf hunchentoot:*message-log-pathname* "./hunchentoot-errors.log")
  (map 'list #'(lambda(top)
		 (let ((psis-of-top (psis top)))
		   (when psis-of-top
		     (create-latest-fragment-of-topic (uri (first psis-of-top))))))
       (elephant:get-instances-by-class 'd:TopicC))
  (hunchentoot:start *server-acceptor*))

(defun shutdown-tm-engine ()
  "Shut down the Topic Map Engine"
  (hunchentoot:stop *server-acceptor*)
  (setf *server-acceptor* nil)
  (elephant:close-store))