;;+-----------------------------------------------------------------------------
;;+  Isidorus
;;+  (c) 2008-2010 Marc Kuester, Christoph Ludwig, Lukas Georgieff
;;+
;;+  Isidorus is freely distributable under the LLGPL license.
;;+  You can find a detailed description in trunk/docs/LLGPL-LICENSE.txt and
;;+  trunk/docs/LGPL-LICENSE.txt.
;;+-----------------------------------------------------------------------------

;; source: http://mihai.bazon.net/blog/howto-multi-threaded-tcp-server-in-common-lisp

(asdf:operate 'asdf:load-op :usocket)
(asdf:operate 'asdf:load-op :bordeaux-threads)


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


(defun task (client-socket mega-loops name)
  (declare (String name)
	   (integer mega-loops)
	   (usocket:stream-usocket client-socket))
  (let ((loops (* 1000000 mega-loops)))
    (dotimes (counter loops)
      (/ (* loops loops) loops))
    (read-from-client client-socket) ;ignore cient data
    (send-to-client client-socket (format nil "~a finished ~a loops" name loops))))


(defvar *stop-listen* nil "if tis variable is set to t, te listener stops to listen after the next client is accepted")


(defun stop-listen-for-clients ()
  (setf *stop-listen* t))


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
		     (lambda()
		       (funcall (lambda(client-socket thread-name)
				  (declare (usocket:stream-usocket client-socket)
					   (String thread-name))
				  (read-from-client client-socket) ;ignore client data
				  (send-to-client client-socket thread-name))
				client (format nil "thread-~a" counter)))
		     :name (format nil "worker-thread: ~a" counter)))
		  (incf counter)
		  (setf stop-p *stop-listen*)))
	      server))))
