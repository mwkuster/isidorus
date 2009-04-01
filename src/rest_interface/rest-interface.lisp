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
	:json-importer)
  (:export :import-fragments-feed
           :import-snapshots-feed
           :import-tm-feed
           :read-url
           :read-fragment-feed
           :start-tm-engine
	   :shutdown-tm-engine
	   :*json-get-prefix*
	   :*json-commit-url*
	   :*json-get-all-psis*
	   :*json-get-summary-prefix**
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

;; (defun feeds ()
;;   "interface funtion to the corresponding Atom method"
;;   (setf (content-type) "application/atom+xml; charset=UTF-8")
;;   (cxml:with-xml-output (cxml:make-string-sink :canonical t)
;;     (atom:feed-to-elem atom::*tm-feed*)))

;; (defun snapshot-feed ()
;;   "Interface function to the corresponding Atom method"
;;   (setf (content-type) "application/atom+xml; charset=UTF-8")
;;   (cxml:with-xml-output (cxml:make-string-sink :canonical t)
;;     ;(atom:build-snapshot-feed)))
;; ))

;; (defun snapshots (&optional revision)
;;   "Export a snapshot by revision"
;;   (assert revision)
;;   (format t "in snapshots~&")
;;   (setf (content-type) "application/xtm+xml; charset=utf-8")
;;   (exporter:export-xtm-to-string :revision (parse-integer revision) 
;;                                  :xtm-format '1.0))


;; (defun fragments (&optional unique-id)
;;   "Export a fragment by its unique id"
;;   (assert unique-id)
;;   (setf (content-type) "application/xtm+xml; charset=utf-8")
;;   (let 
;;       ((fragment 
;; 	(d:get-fragment (parse-integer unique-id))))
;;     (if fragment
;; 	(exporter:export-xtm-fragment fragment :xtm-format '1.0)
;; 	(format nil "<t:topicMap xmlns:t=\"http://www.topicmaps.org/xtm/1.0/\" xmlns:xlink=\"http://www.w3.org/1999/xlink\"/>"))))


;; (push 
;;  (create-regex-dispatcher "/feeds/?$" #'feeds) 
;;  hunchentoot:*dispatch-table*)

;; (push 
;;  (create-regex-dispatcher "/feeds/testtm/?$" #'tm-feed)
;;  hunchentoot:*dispatch-table*)

;; (push 
;;  (create-regex-dispatcher "/testtm/snapshots/$" #'snapshot-feed) 
;;  hunchentoot:*dispatch-table*)

;; (push 
;;  (create-regex-dispatcher "/testtm/snapshots/([0-9]+)$" #'snapshots) 
;;  hunchentoot:*dispatch-table*)

;; (push 
;;  (create-regex-dispatcher "/testtm/fragments/?$" #'fragments-feed) 
;;  hunchentoot:*dispatch-table*)

;; (push 
;;  (create-regex-dispatcher "/testtm/fragments/([0-9]+)$" #'fragments) 
;;  hunchentoot:*dispatch-table*)



(defvar *acceptor*)

(defun start-tm-engine (repository-path &key (conffile "atom/conf.lisp") (host-name "localhost") (port 8000))
  "Start the Topic Map Engine on a given port, assuming a given
hostname. Use the repository under repository-path"
  (setf hunchentoot:*show-lisp-errors-p* t) ;for now
  ;(setf hunchentoot:*show-lisp-backtraces-p* t) ;hunchentoot 0.15.7
  (setf hunchentoot:*hunchentoot-default-external-format* 
	(flex:make-external-format :utf-8 :eol-style :lf))
  (setf atom:*base-url* (format nil "http://~a:~a" host-name port))
  (elephant:open-store  
   (xml-importer:get-store-spec repository-path))
  (load conffile)
  (publish-feed atom:*tm-feed*)
  (set-up-json-interface)
  (setf *acceptor* (make-instance 'hunchentoot:acceptor :address host-name :port port))
  (setf hunchentoot:*lisp-errors-log-level* :info)
  (setf hunchentoot:*message-log-pathname* "./hunchentoot-errors.log")
  (hunchentoot:start *acceptor*))

(defun shutdown-tm-engine ()
  "Shut down the Topic Map Engine"
  (hunchentoot:stop *acceptor*)
  (elephant:close-store))