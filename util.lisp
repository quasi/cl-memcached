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



(defun mc-make-benchmark-data (n)
  (with-output-to-string (s)
    (dotimes (i n)
      (write-char #\x s))))


(defun mc-benchmark (n data-size &key (memcache *memcache*) (use-pool t) (action :write))
  (let ((data (flex:string-to-octets (mc-make-benchmark-data data-size))))
    (dotimes (i n)
      (let ((key (concatenate 'simple-string "key_" (princ-to-string i))))
	(case action
	  (:write (mc-store memcache key data :use-pool use-pool :timeout 600))
	  (:read (mc-get memcache (list key) :use-pool use-pool)))))))



#+(and flexi-streams cl-store)
(progn

  (defun cl-store-in-memcache (key obj &key (memcache *memcache*) ((:command command) :set) ((:timeout timeout) 0) ((:use-pool use-pool) *use-pool*))
    "Serializes 'obj' with cl-store and stores in memcache"
    (let ((seq (flexi-streams:with-output-to-sequence (out)
		 (cl-store:store obj out))))
      (cl-memcached:mc-store key seq :memcache memcache :command command :timeout timeout :use-pool use-pool)))


  (defun cl-restore-from-memcache (keys-list &key (memcache *memcache*) ((:use-pool use-pool) *use-pool*))
    "Gets objects corresponding to keys from keys-list.  cl-store will throw and RESTORE-ERROR if an object retreived has not
been encoded by cl-store. i.e. if it cannot find a method to de-serialize it
Returned is an alist of key and objects"
    (mapcar #'(lambda (x)
		(list (first x)
		      (flexi-streams:with-input-from-sequence (in (second x))
			(cl-store:restore in))))
	    (cl-memcached:mc-get keys-list :memcache memcache :use-pool use-pool))))
