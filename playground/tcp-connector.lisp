;;+-----------------------------------------------------------------------------
;;+  Isidorus
;;+  (c) 2008-2010 Marc Kuester, Christoph Ludwig, Lukas Georgieff
;;+
;;+  Isidorus is freely distributable under the LLGPL license.
;;+  You can find a detailed description in trunk/docs/LLGPL-LICENSE.txt and
;;+  trunk/docs/LGPL-LICENSE.txt.
;;+-----------------------------------------------------------------------------

;; source: http://mihai.bazon.net/blog/howto-multi-threaded-tcp-server-in-common-lisp


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


