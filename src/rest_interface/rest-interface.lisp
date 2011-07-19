;;+-----------------------------------------------------------------------------
;;+  Isidorus
;;+  (c) 2008-2010 Marc Kuester, Christoph Ludwig, Lukas Georgieff
;;+
;;+  Isidorus is freely distributable under the LLGPL license.
;;+  You can find a detailed description in trunk/docs/LLGPL-LICENSE.txt and
;;+  trunk/docs/LGPL-LICENSE.txt.
;;+-----------------------------------------------------------------------------

(defpackage :rest-interface
  (:nicknames :rest)
  (:use :cl :hunchentoot 
	:cxml
        :constants
	:exceptions
	:TM-SPARQL
        :atom 
        :datamodel
        :xtm-exporter
        :xml-tools
        :xtm-importer
	:json-exporter
	:json-importer
	:base-tools
        :isidorus-threading)
  (:export :import-fragments-feed
           :import-snapshots-feed
           :import-tm-feed
           :read-url
           :read-fragment-feed
           :start-json-engine
	   :start-atom-engine
	   :start-admin-server
	   :shutdown-json-engine
	   :shutdown-atom-engine
	   :*admin-local-backup*
	   :*admin-remote-backup*
	   :*admin-shutdown*
	   :shutdown-admin-server
	   :*admin-host-name*
	   :*admin-port*
	   :*admin-remote-name*
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
	   :*ajax-javascript-url-prefix*
	   :*xtm-commit-prefix*
	   :*ready-to-die*
	   :die-when-finished
	   :*sparql-url*))


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


(defvar *json-server-acceptor* nil)
(defvar *atom-server-acceptor* nil)
(defvar *admin-server-acceptor* nil)
(defvar *admin-host-name* "127.0.0.1")
(defvar *admin-port* 11008)
(defvar *admin-remote-name* "127.0.0.1")


(defun start-admin-server ()
  (when *admin-server-acceptor*
    (error "The admin-server is already running"))
  (set-up-admin-interface )
  (setf hunchentoot:*show-lisp-errors-p* t)
  (setf hunchentoot:*hunchentoot-default-external-format* 
	(flex:make-external-format :utf-8 :eol-style :lf))
  (setf *admin-server-acceptor*
	(make-instance 'hunchentoot:acceptor
		       :address *admin-host-name*
		       :port *admin-port*))
  (hunchentoot:start *admin-server-acceptor*))


(defun shutdown-admin-server ()
  "Shut down the admin server."
  (when *admin-server-acceptor*
    (hunchentoot:stop *admin-server-acceptor*))
  (setf *admin-server-acceptor* nil))


(defun start-json-engine (repository-path &key
			  (host-name "localhost") (port 8000))
  "Start the Topic Maps Engine on a given port, assuming a given
   hostname. Use the repository under repository-path.
   This function starts only the json/xtm/rdf handlers for the UI,
   The atom interface has to be started separately."
  (when *json-server-acceptor*
    (error "The json-server is already running"))
  (setf hunchentoot:*show-lisp-errors-p* t) ;for now
  (setf hunchentoot:*hunchentoot-default-external-format* 
	(flex:make-external-format :utf-8 :eol-style :lf))
  (open-tm-store repository-path)
  (set-up-json-interface)
  (setf *json-server-acceptor*
	(make-instance 'hunchentoot:acceptor :address host-name :port port))
  (setf hunchentoot:*lisp-errors-log-level* :info)
  (setf hunchentoot:*message-log-pathname* "./json-hunchentoot-errors.log")
  (hunchentoot:start *json-server-acceptor*))


(defun shutdown-json-engine ()
  "Shut down the Topic Map Engine, only the json part."
  (when *json-server-acceptor*
    (hunchentoot:stop *json-server-acceptor*))
  (setf *json-server-acceptor* nil)
  (close-tm-store))


(defun start-atom-engine (repository-path &key (conf-file "atom/conf.lisp")
			  (host-name "localhost") (port 8001))
  "Start the Topic Maps Engine on a given port, assuming a given
   hostname. Use the repository under repository-path.
   This function starts only the atom interface.
   The json/xtm/rdf interface has to be started separately."
  (when *atom-server-acceptor*
    (error "The atom-server is already running"))
  (setf hunchentoot:*show-lisp-errors-p* t) ;for now
  (setf hunchentoot:*hunchentoot-default-external-format* 
	(flex:make-external-format :utf-8 :eol-style :lf))
  (setf atom:*base-url* (format nil "http://~a:~a" host-name port))
  (open-tm-store repository-path)
  (load conf-file)
  (publish-feed atom:*tm-feed*)
  (setf *atom-server-acceptor*
	(make-instance 'hunchentoot:acceptor :address host-name :port port))
  (setf hunchentoot:*lisp-errors-log-level* :info)
  (setf hunchentoot:*message-log-pathname* "./atom-hunchentoot-errors.log")
  (hunchentoot:start *atom-server-acceptor*))


(defun shutdown-atom-engine ()
  "Shut down the Topic Map Engine, only the atom part."
  (when *atom-server-acceptor*
    (hunchentoot:stop *atom-server-acceptor*))
  (setf *atom-server-acceptor* nil)
  (close-tm-store))
