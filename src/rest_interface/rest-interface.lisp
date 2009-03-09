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
	   :*json-rest-prefix*
	   :*json-user-interface-url*
	   :*json-user-interface-file-path*))


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


;;(defun make-json (&optional uri)
;;  "returns a json-string of the topic with the passed psi-uri"
;;  (assert uri)
;;  ;decodes the url-encoding "%23" to "#" character (only the first which will be found)
;;  (let ((identifier (let ((pos (search "%23" uri)))
;;		      (if pos
;;			  (let ((str-1 (subseq uri 0 pos))
;;				(str-2 (if (> (length uri) (+ pos 3))
;;					   (subseq uri (+ pos 3))
;;					   "")))
;;			    (concatenate 'string str-1 "#" str-2))
;;			  uri)))
;;	(http-method (request-method))
;;	(external-format (flexi-streams:make-external-format :UTF-8 :eol-style :LF))) ;;is needed to get a string of the put-request
;;    (if (eq http-method :GET)
;;	(progn
;;	  (setf (hunchentoot:content-type) "application/json")
;;	  (let ((fragment
;;		 (get-latest-fragment-of-topic identifier)))
;;	    (if fragment
;;		(handler-case (to-json-string fragment)
;;		  (condition (err) (format nil "{\"fault\":\"~a\"}" err)))
;;		"{}")))
;;	(if (eq http-method :PUT)
;;	    (let ((put-data (raw-post-data :external-format external-format :force-text t)))	      
;;	      (handler-case (json-importer:json-to-elem put-data)
;;		(condition () (setf (return-code) +http-internal-server-error+))))    
;;	    (setf (return-code) +http-internal-server-error+))))) ; for all htt-methods except for get and post


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

;;(push
;; (create-regex-dispatcher "/json/psi/(.+)$" #'make-json)
;; hunchentoot:*dispatch-table*)
    

(defvar *server*)

(defun start-tm-engine (repository-path &key (conffile "atom/conf.lisp") (host-name "localhost") (port 8000))
  "Start the Topic Map Engine on a given port, assuming a given
hostname. Use the repository under repository-path"
  (setf hunchentoot:*show-lisp-errors-p* t) ;for now 
  (setf hunchentoot:*show-lisp-backtraces-p* t)
  (setf hunchentoot:*hunchentoot-default-external-format* 
	(flex:make-external-format :utf-8 :eol-style :lf))
  (setf atom:*base-url* (format nil "http://~a:~a" host-name port))
  (elephant:open-store  
   (xml-importer:get-store-spec repository-path))
  (load conffile)
  (publish-feed atom:*tm-feed*)
  (set-up-json-interface)
  (setf *server* (hunchentoot:start-server :address host-name :port port)))

(defun shutdown-tm-engine ()
  "Shut down the Topic Map Engine"
  (hunchentoot:stop-server *server*)
  (elephant:close-store))