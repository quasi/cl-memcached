(in-package #:cl-memcached)

(defconstant +crlf+
  (if (boundp '+crlf+)
      (symbol-value '+crlf+)
      (concatenate 'string
		   (string (code-char 13))
		   (string (code-char 10)))))

(defconstant +mc-END+
  (if (boundp '+mc-END+)
      (symbol-value '+mc-END+)
      (concatenate 'string
		   (string "END")
		   (string (code-char 13))
		   (string (code-char 10)))))



(defun hash-SDBM (str)
  (declare (type simple-string str)
	   (optimize (speed 3) (safety 0) (space 0) (debug 0)))
  (let ((hash 0))
    (declare (type (unsigned-byte 32) hash))
    (loop for x across str
       do (setf hash (ldb (byte 32 0) (+ (char-int x) (ldb (byte 32 0) (ash hash 6)) (ldb (byte 32 0) (ash hash 16)) (- hash))))
       finally (return hash))))


(defun mc-server-check (&key (memcache *memcache*))
  "Performs some basic tests on the Memcache instance and outputs a status string"
  (with-output-to-string (s)
    (let ((key "MEMCACHESERVERCHECK")
	  (data (flex:string-to-octets "IS THE SERVER OK ?")))
      (progn
	(format s "Checking Memcached Server ~A running on ~A:~A ..." (name memcache) (ip memcache) (port memcache))
	(format s "~%Sending data of length ~D with key ~A..." (length data) key)
	(format s "~%Storage Command Rreturned : ~A" (handler-case (mc-store memcache key data)
						       (socket-error () (format t "~2%CANNOT CONNECT TO CACHE !~%"))
						       (error (c) (format t "GET COMMAND ERROR ~A" c))))
	(format s "~%Trying to get back stored data with key ~A" key)
	(format s "~%Retrieve Command Returned : ~a" (when (handler-case (mc-get memcache (list key))
							     (socket-error () (format t "~2%CANNOT CONNECT TO CACHE !~%"))
							     (error (c) (format t "GET COMMAND ERROR ~A" c)))
						       "DATA"))
	(format s "~%Delete Command Returned : ~A" (handler-case (mc-del memcache key)
						     (socket-error () (format t "~2%CANNOT CONNECT TO CACHE !~%"))
						     (error (c) (format t "DEL COMMAND ERROR ~A" c))))
	(format s "~2%~a" (quick-stat memcache))))))



;;; EOF

