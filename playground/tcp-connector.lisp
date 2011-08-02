;;+-----------------------------------------------------------------------------
;;+  Isidorus
;;+  (c) 2008-2010 Marc Kuester, Christoph Ludwig, Lukas Georgieff
;;+
;;+  Isidorus is freely distributable under the LLGPL license.
;;+  You can find a detailed description in trunk/docs/LLGPL-LICENSE.txt and
;;+  trunk/docs/LGPL-LICENSE.txt.
;;+-----------------------------------------------------------------------------

;; source: http://mihai.bazon.net/blog/howto-multi-threaded-tcp-server-in-common-lisp

(asdf:operate 'asdf:load-op :isidorus)


(defun make-server (&key (hostname "localhost") (port 8000))
  (declare (string hostname) (number port))
  (usocket:socket-listen hostname port :reuse-address t))


(defun wait-for-client (server-socket)
  (declare (usocket:stream-server-usocket server-socket))
  (usocket:wait-for-input server-socket)
  (usocket:socket-accept server-socket))


(defun read-from-client (client-socket)
  (declare (usocket:stream-usocket client-socket))
  (let* ((header (read-tcp-header (usocket:socket-stream client-socket)))
	 (payload (read-tcp-payload (usocket:socket-stream client-socket) header)))
    (list :headers header
	  :payload payload)))


(defun read-tcp-header (stream)
  (declare (Stream stream))
  (let ((line (string-right-trim (list #\cr) (read-line stream))))
    (if (string= "" line)
	(list "")
	(progn
	  (append (list line) (read-tcp-header stream))))))


(defun read-tcp-payload (stream header-list)
  (declare (Stream stream)
	   (list header-list))
  (let ((content-length
	 (let ((val
		(loop for line in header-list
		   when (search "content-length:" (string-downcase line) :test #'string=)
		   return (let ((value (subseq line (length "content-length:"))))
			    (parse-integer value)))))
	   (if val val 0)))
	(payload ""))
    (dotimes (idx content-length payload)
      (setf payload (concatenate 'string payload (string (read-char stream)))))))


(defun send-to-client (client-socket message-string &key (content-type "text/plain"))
  (declare (usocket:stream-usocket client-socket)
	   (String message-string content-type))
  (format (usocket:socket-stream client-socket)
	  "~a~c~c~a~a~c~c~a~a~c~c~a~c~c~c~c~a"
	  "HTTP/1.1 200 OK" #\return #\newline
	  "Content-Length: " (write-to-string (length message-string)) #\return #\newline
	  "Content-Type: " content-type #\return #\newline
	  "Connection: close" #\return #\newline
	  #\return #\newline
	  message-string)
  (force-output (usocket:socket-stream client-socket))
  (usocket:socket-close client-socket))


(defvar *stop-listen* nil "if this variable is set to t, the listener stops to listen after the next client has been accepted")


(defun stop-listen-for-clients (server)
  (setf *stop-listen* t)
  (usocket:socket-close server)
  (base-tools:close-tm-store))


(defun client-task (client-socket)
  (declare (usocket:stream-usocket client-socket))
  (handler-case
      (let ((client-data (read-from-client client-socket)))
	(let ((response
	       (cond ((string-starts-with (first (getf client-data :headers))
					  "GET /json/psis")
		      (get-psis))
		     ((string-starts-with (first (getf client-data :headers))
					  "GET /json/get/")
		      (get-fragment (get-requested-psi-of-http-header
				     (first (getf client-data :headers)))))
		     (t
		      (concatenate 'string ">> bad request: ~a~%"
				   (first (getf client-data :headers)))))))
	  (send-to-client client-socket response)))
    (condition ()
      (usocket:socket-close client-socket))))


(defun listen-for-clients (server)
  (declare (usocket:stream-server-usocket server))
  (setf *stop-listen* nil)
  (sb-thread:make-thread
   (lambda()
     (funcall (lambda(srv)
		(do ((stop-p *stop-listen*) (counter 0)) ((not (null stop-p)))
		  (let ((client (wait-for-client srv)))
		    (format t "client # ~a connected~%" counter)
		    (sb-thread:make-thread
		     (lambda() (funcall #'client-task client))
		     :name (format nil "worker-thread: ~a" counter)))
		  (incf counter)
		  (setf stop-p *stop-listen*)))
	      server))
   :name "server-listener"))


(defun get-psis ()
  (isidorus-threading:with-reader-lock
    (json-exporter:get-all-topic-psis :revision 0)))


(defun get-fragment(psi)
  (let ((fragment (isidorus-threading:with-reader-lock
		    (d:get-latest-fragment-of-topic psi))))
    (if (and fragment
	     (d:find-item-by-revision (d:topic fragment) 0))
	(json-exporter:export-construct-as-isidorus-json-string fragment :revision 0)
	(concatenate 'string psi " not found"))))
	  

(defun get-requested-psi-of-http-header (first-header-line)
  (declare (String first-header-line))
  (when (and (string-starts-with first-header-line "GET /json/get/")
	     (or (string-ends-with first-header-line "HTTP/1.0")
		 (string-ends-with first-header-line "HTTP/1.1")))
    (let ((psi (subseq first-header-line
		       (length "GET /json/get/")
		       (- (length first-header-line) (length "HTTP/1.0")))))
      (hunchentoot:url-decode (string-trim '(#\space) psi)))))
    

(defun string-starts-with (str prefix &key (ignore-case nil))
  "Checks if string str starts with a given prefix."
  (declare (String str prefix)
	   (Boolean ignore-case))
  (let ((str-i (if ignore-case
		   (string-downcase str :start 0 :end (min (length str)
							   (length prefix)))
		   str))
	(prefix-i (if ignore-case
		      (string-downcase prefix)
		      prefix)))
    (string= str-i prefix-i :start1 0 :end1
	     (min (length prefix-i)
		  (length str-i)))))


(defun string-ends-with (str suffix &key (ignore-case nil))
  "Checks if string str ends with a given suffix."
  (declare (String str suffix)
	   (Boolean ignore-case))
  (let ((str-i (if ignore-case
		   (string-downcase str :start (max (- (length str)
						       (length suffix))
						    0)
				    :end (length str))
		   str))
	(suffix-i (if ignore-case
		      (string-downcase suffix)
		      suffix)))
    (string= str-i suffix-i :start1 (max (- (length str)
					    (length suffix))
					 0))))


(defun main()
  (format t ">> entered (main)")
  (base-tools:open-tm-store "/home/lukas/.sbcl/site/isidorus/trunk/src/data_base")
  (defvar *server* (make-server :port 8080))
  (listen-for-clients *server*))


(main)