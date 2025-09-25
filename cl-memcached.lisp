;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

;;; Copyright (c) 2006-2013, Abhijit 'quasi' Rao.  All rights reserved.

;;; Library provided under MIT Licence.


(in-package #:cl-memcached)


;; (eval-when (:compile-toplevel :load-toplevel :execute)
;;   (declaim (optimize (speed 3) (debug 0))))

;;; Some global variables

(defvar *memcache* nil
  "Represents a particular Memcached server")

(defvar *mc-use-pool* nil
  "Default value for the MC-USE-POOL keyword parameter in memcached functions")

(defvar *mc-default-encoding* (babel:make-external-format :UTF-8)
  "Default encoding")

(defvar +command-encoding+ (babel:make-external-format :ASCII))

;;; Some constants
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


;;;
;;; The main structures which represents the memcached server
;;;

(defstruct (memcache
	     (:conc-name mc-)
	     (:print-function
	      (lambda (struct stream depth)
		(declare (ignore depth))
		(format stream "#<MEMCACHED-SERVER Name:~A IP:~a Port:~d >"
			(mc-name struct) (mc-ip struct) (mc-port struct))))
	     (:constructor make-memcache% (name ip port pool-size)))
  (name "Memcache" :type simple-string :read-only t)
  (ip "127.0.0.1" :type simple-string :read-only t)
  (port 11211 :type fixnum :read-only t)
  (pool-size 20 :type fixnum :read-only t)
  pool)


(defun make-memcache (&key (ip "127.0.0.1") (port 11211) (name "Memcache") (pool-size 2))
  "Creates an instance of class MEMCACHE which represents a memcached server"
  (let ((memcache (make-memcache% name ip port pool-size)))
    (setf (mc-pool memcache) (pooler:make-pool :name "Memcache Connection Pool"
					       :capacity (mc-pool-size memcache)
					       :item-maker #'(lambda () (new-memcache-connection memcache))
					       :item-destroyer #'close-memcache-connection))
    memcache))



(defun new-memcache-connection (memcache)
  (handler-case (usocket:socket-connect (mc-ip memcache) (mc-port memcache) :element-type '(unsigned-byte 8))
    (error () (error 'memcached-server-unreachable))))

(defun close-memcache-connection (connection)
  (ignore-errors (usocket:socket-close connection)))


;;; Error Conditions

(define-condition cl-mc-condition (condition)
  ()
  (:documentation "Superclass for all conditions related to CL-MC."))

(define-condition cl-mc-error (cl-mc-condition error)
  ()
  (:documentation "Superclass for all errors related to CL-MC."))


(define-condition memcached-server-unreachable (cl-mc-error)
  ())

(defun cl-mc-error (format-control &rest format-arguments)
  "Signals an error of type TRIPR-SIMPLE-ERROR with the provided
format control and arguments."
  (error 'cl-mc-error
         :format-control format-control
         :format-arguments format-arguments))


;;; Helper Macros

(defmacro mc-with-pool-y/n ((memcache use-pool stream) &body body)
  (let ((conn (gensym "MC-"))
	(up (gensym "MC-")))
    `(let ((,up ,use-pool)
	   ,conn)
       (if ,up
	   (setf ,conn (pooler:fetch-from (mc-pool ,memcache)))
	   (setf ,conn (new-memcache-connection ,memcache)))
       (unwind-protect
	    (when ,conn
	      (let ((,stream (usocket:socket-stream ,conn)))
		(handler-case (progn ,@body)
		  (error () (setf ,up nil)))))
	 (if ,up
	     (pooler:return-to (mc-pool ,memcache) ,conn)
	     (close-memcache-connection ,conn))))))


(defun server-request (command-param-list &key cas-unique noreply)
  (babel:string-to-octets
   (with-output-to-string (str)
     (loop for x in command-param-list
	do (write-string x str)
	do (write-char #\Space str))
     (when cas-unique
       (write-string (princ-to-string cas-unique) str)
       (write-char #\Space str))
     (when noreply
       (write-string "noreply" str))
     (write-string +crlf+ str))
    :encoding +command-encoding+))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Memcached API functionality
;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; SET/ADD/REPLACE/APPEND/PREPEND functionality


(defun mc-store (key data &key (memcache *memcache*) (command :set) (timeout 0) (flags 0) (noreply nil) (cas-unique nil) (mc-use-pool *mc-use-pool*))
  "Stores data in the memcached server using the :command command.
KEY => KEY by which the DATA is stored. this is of type SIMPLE-STRING
DATA => DATA to be stored into the cache. DATA has to be a sequence of TYPE (UNSIGNED-BYTE 8).
MEMCACHE => A structure representing the MEMCACHE we want to use.
command => The storage command we want to use.  There are 5 available : set, add, replace, append & prepend.
timeout => The time in seconds when this data expires.  0 is never expire.
flags =>
noreply => server does not send a reply
mc-use-pool => use connection from pool (much faster for load)

response :
- 'STORED\r\n', to indicate success.
- 'NOT_STORED\r\n' to indicate the data was not stored, but not
   because of an error. This normally means that the
   condition for an 'add' or a 'replace' command wasn't met.
- 'EXISTS\r\n' to indicate that the item you are trying to store with
   a 'cas' command has been modified since you last fetched it.
- 'NOT_FOUND\r\n' to indicate that the item you are trying to store
   with a 'cas' command did not exist.
"
  (declare (type fixnum timeout) (type simple-string key)
	   (type (member :set :add :replace :append :prepend :cas) command))
  (unless (equal (array-element-type data) '(UNSIGNED-BYTE 8))
    (cl-mc-error "Data has to be a ARRAY with ELEMENT-TYPE of (UNSIGNED-BYTE 8)"))
  (when (and cas-unique (not (eq command :cas)))
    (cl-mc-error "CAS-UNIQUE is only useed with the CAS command"))
  (let* ((len (length data))
	 (server-command (case command
			   (:set "set")
			   (:add "add")
			   (:replace "replace")
			   (:append "append")
			   (:prepend "prepend")
			   (:cas "cas")
			   (t (cl-mc-error "Unknown Command : ~a." command))))
	 (command-list (list server-command key (princ-to-string flags) (princ-to-string timeout) (princ-to-string len))))
    (mc-with-pool-y/n (memcache mc-use-pool s)
      (write-sequence (server-request command-list :cas-unique cas-unique :noreply noreply) s)
      (force-output s)
      (write-sequence data s)
      (write-sequence (babel:string-to-octets +crlf+) s)
      (force-output s)
      (unless noreply
	(read-line-from-binary-stream s)))))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro mc-make-command (command)
    (let ((mc-function (intern (string-upcase (concatenate 'string "mc-" (symbol-name command))))))
      `(export ',mc-function)
      `(defun ,mc-function (key data &key (memcache *memcache*) (timeout 0) (flags 0) (noreply nil) (external-format *mc-default-encoding*) (mc-use-pool *mc-use-pool*))
	 (let ((unsigned-byte-data (if (equal (array-element-type data) '(UNSIGNED-BYTE 8))
				       data
				       (babel:string-to-octets data :encoding external-format))))
	   (mc-store key unsigned-byte-data :memcache memcache :command ,command :timeout timeout :flags flags :noreply noreply :mc-use-pool mc-use-pool)))))
  )


(mc-make-command :set)
(mc-make-command :add)
(mc-make-command :replace)
(mc-make-command :append)
(mc-make-command :prepend)


;;; "cas" is a check and set operation which means 'store this data but
;;;  only if no one else has updated since I last fetched it.'

(defun mc-cas (key data cas-unique &key (memcache *memcache*) (timeout 0) (flags 0) (noreply nil) (external-format *mc-default-encoding*) (mc-use-pool *mc-use-pool*))
  "Check And Set Operation : Store this data buy only if no one else has updated since I last fetched it"
  (let ((unsigned-byte-data (if (equal (array-element-type data) '(UNSIGNED-BYTE 8))
				data
				(babel:string-to-octets data :encoding external-format))))
    (mc-store key unsigned-byte-data :memcache memcache :command :cas :timeout timeout :flags flags :noreply noreply :cas-unique cas-unique :mc-use-pool mc-use-pool)))



;;; GET 'key(s)' functionality

;; code given by stassats over IRC
(defun read-line-from-binary-stream (stream)
  (with-output-to-string (str)
    (loop named outer
       for byte = (read-byte stream)
       do (let ((char (code-char byte)))
            (loop while (char= char #\Return)
	       do (let ((next-char (code-char (read-byte stream))))
                    (cond ((char= next-char #\Newline)
                           (return-from outer))
                          (t
                           (write-char char str)
                           (setf char next-char)))))
            (write-char char str)))))



(defun mc-get-internal (command keys-list &key (memcache *memcache*) (mc-use-pool *mc-use-pool*))
  (when (not (listp keys-list))
    (cl-mc-error "KEYS-LIST has to be a LIST of keys"))
  (mc-with-pool-y/n (memcache mc-use-pool s)
    (write-sequence (server-request (append (list command) keys-list)) s)
    (force-output s)
    (loop for x = (read-line-from-binary-stream s)
       until (string-equal "END" x)
       collect (let* ((status-line (split-sequence:split-sequence #\Space x))
                      (key (second status-line))
                      (flags (third status-line))
                      (bytes (parse-integer (fourth status-line)))
                      (cas-unique (when (string-equal command "gets") (fifth status-line)))
                      (seq (make-sequence '(vector (unsigned-byte 8)) bytes)))
                 (read-sequence seq s)
                 (read-line-from-binary-stream s)
                 (list key flags bytes cas-unique seq)))))

(defun mc-get (keys-list &key (memcache *memcache*) (mc-use-pool *mc-use-pool*))
  (mc-get-internal "get" keys-list :memcache memcache :mc-use-pool mc-use-pool))

(defun mc-gets (keys-list &key (memcache *memcache*) (mc-use-pool *mc-use-pool*))
  (mc-get-internal "gets" keys-list :memcache memcache :mc-use-pool mc-use-pool))



;;; Response Structure

(defstruct (memcache-response
	     (:conc-name mc-)
	     (:print-function
	      (lambda (struct stream depth)
		(declare (ignore depth))
		(format stream "#<MEMCACHED-RESPONSE Key:~a Data-Length:~A >"
			(mc-key struct) (mc-bytes struct))))
	     (:constructor make-memcache-response% (key flags bytes cas-unique data-raw)))
  (key "" :type simple-string :read-only t)
  (flags "" :read-only t)
  (bytes 0 :type fixnum :read-only t)
  (cas-unique "" :read-only t)
  (data-raw nil :type (array (UNSIGNED-BYTE 8)) :read-only t))


(defun mc-data (response &key (external-format *mc-default-encoding*))
  (when (eq (type-of response) 'MEMCACHE-RESPONSE)
    (babel:octets-to-string (mc-data-raw response) :encoding external-format)))


(defun mc-get+ (key-or-list-of-keys &key (memcache *memcache*) (mc-use-pool *mc-use-pool*))
  "Takes a key or a list of keys are returns a list of MEMCACHE-RESPONSE structures"
  (let* ((keys (if (listp key-or-list-of-keys) key-or-list-of-keys (list key-or-list-of-keys)))
	 (result (loop for x in (mc-get keys :memcache memcache :mc-use-pool mc-use-pool)
		    when x
		    collect (make-memcache-response% (first x) (second x) (third x) (fourth x) (fifth x)))))
    (if (and (not (listp key-or-list-of-keys)) (= (length result) 1))
        (first result)
        result)))

(defun mc-gets+ (key-or-list-of-keys &key (memcache *memcache*) (mc-use-pool *mc-use-pool*))
  "Takes a key or a list of keys are returns a list of MEMCACHE-RESPONSE structures with CAS values"
  (let* ((keys (if (listp key-or-list-of-keys) key-or-list-of-keys (list key-or-list-of-keys)))
	 (result (loop for x in (mc-gets keys :memcache memcache :mc-use-pool mc-use-pool)
		    when x
		    collect (make-memcache-response% (first x) (second x) (third x) (fourth x) (fifth x)))))
    (if (and (not (listp key-or-list-of-keys)) (= (length result) 1))
	(first result)
	result)))


(defun mc-get-value (key &key (memcache *memcache*) (mc-use-pool *mc-use-pool*) (external-format *mc-default-encoding*))
  "A utility macro to query a key and return a external-format decoded string"
  (mc-data (mc-get+ key :memcache memcache :mc-use-pool mc-use-pool) :external-format external-format))



;;; DELETE functionality

(defun mc-del (key &key (noreply nil) (memcache *memcache*) (mc-use-pool *mc-use-pool*))
  (mc-with-pool-y/n (memcache mc-use-pool s)
    (write-sequence (server-request (list "delete" key) :noreply noreply) s)
    (force-output s)
    (unless noreply
      (read-line-from-binary-stream s))))


;;; INCR functionality

(defun mc-incr (key &key (value 1) (noreply nil) (memcache *memcache*) (mc-use-pool *mc-use-pool*))
  (mc-with-pool-y/n (memcache mc-use-pool s)
    (write-sequence (server-request (list "incr" key (princ-to-string value)) :noreply noreply) s)
    (force-output s)
    (unless noreply
      (let ((l (read-line-from-binary-stream s)))
	(if (string-equal l "NOT_FOUND")
	    'NOT_FOUND
	    (parse-integer l))))))

;;; DECR functionality

(defun mc-decr (key &key (value 1) (noreply nil) (memcache *memcache*) (mc-use-pool *mc-use-pool*))
  (mc-with-pool-y/n (memcache mc-use-pool s)
    (write-sequence (server-request (list "decr" key (princ-to-string value)) :noreply noreply) s)
    (force-output s)
    (unless noreply
      (let ((l (read-line-from-binary-stream s)))
	(if (string-equal l "NOT_FOUND")
	    'NOT_FOUND
	    (parse-integer l))))))


;;; TOUCH
;;; The "touch" command is used to update the expiration time of an existing item
;;; without fetching it.

(defun mc-touch (key expiry-time &key (noreply nil) (memcache *memcache*) (mc-use-pool *mc-use-pool*))
  (mc-with-pool-y/n (memcache mc-use-pool s)
    (write-sequence (server-request (list "touch" key (princ-to-string expiry-time)) :noreply noreply) s)
    (force-output s)
    (unless noreply
      (read-line-from-binary-stream s))))


;;; FLUSH_ALL

(defun mc-flush-all (&key (delay 0) (noreply nil) (memcache *memcache*) (mc-use-pool *mc-use-pool*))
  (mc-with-pool-y/n (memcache mc-use-pool s)
    (write-sequence (server-request (list "flush_all" (princ-to-string delay)) :noreply noreply) s)
    (force-output s)
    (unless noreply
      (read-line-from-binary-stream s))))


;;; VERSION

(defun mc-version (&key (memcache *memcache*) (mc-use-pool *mc-use-pool*))
  (mc-with-pool-y/n (memcache mc-use-pool s)
    (write-sequence (server-request (list "version")) s)
    (force-output s)
    (read-line-from-binary-stream s)))


;;; VERBOSITY (of loggin output)

(defun mc-verbosity (&key (level 1) (noreply nil) (memcache *memcache*) (mc-use-pool *mc-use-pool*))
  (mc-with-pool-y/n (memcache mc-use-pool s)
    (write-sequence (server-request (list "verbosity" (princ-to-string level)) :noreply noreply) s)
    (force-output s)
    (unless noreply
      (read-line-from-binary-stream s))))


;;;
;;; Statistics from the MEMCACHED server
;;;

(defun mc-stats-internal (command &key (memcache *memcache*) (mc-use-pool *mc-use-pool*))
  "Internal function to query memcached stats"
  (mc-with-pool-y/n (memcache mc-use-pool s)
    (write-sequence (server-request (list command)) s)
    (force-output s)
    (loop for line = (read-line-from-binary-stream s)
       while (not (string-equal "END" line))
       collect (let ((param (split-sequence:split-sequence #\Space line)))
                 (cons (second param) (third param))))))

(defun mc-stats (&key (memcache *memcache*) (mc-use-pool *mc-use-pool*))
  "Returns an ALIST of general stats data from memcached server"
  (mc-stats-internal "stats" :memcache memcache :mc-use-pool mc-use-pool))

(defun mc-stats-items (&key (memcache *memcache*) (mc-use-pool *mc-use-pool*))
  "Returns an ALIST of stats items data from memcached server"
  (mc-stats-internal "stats items" :memcache memcache :mc-use-pool mc-use-pool))

(defun mc-stats-slabs (&key (memcache *memcache*) (mc-use-pool *mc-use-pool*))
  "Returns an ALIST of stats slabs data from memcached server"
  (mc-stats-internal "stats slabs" :memcache memcache :mc-use-pool mc-use-pool))

(defun mc-stats-sizes (&key (memcache *memcache*) (mc-use-pool *mc-use-pool*))
  "Returns an ALIST of stats sizes data from memcached server"
  (mc-stats-internal "stats sizes" :memcache memcache :mc-use-pool mc-use-pool))



(defun mc-stats-summary (&key (memcache *memcache*))
  (let ((s (mc-stats :memcache memcache)))
    (format t "Memcached Server Stats~%----------------------")
    (loop for x in s
       do (format t "~%~A ~25T: ~A" (string-capitalize (substitute #\Space #\_ (first x))) (rest x)))))



;;; quick tests

(defun mc-quick-test (&key (memcache *memcache*) (mc-use-pool *mc-use-pool*))
  (let ((key "test-key")
	(data "test daaaaaaaaaaaaaaaataaaaaaaaaaaaa"))
    (progn
      (format t "~%~:[FAIL~;Success~] SET" (string= (mc-set key data :memcache memcache :mc-use-pool mc-use-pool) "STORED"))
      (format t "~%~:[FAIL~;Success~] GET" (= (length (mc-get-value key :memcache memcache :mc-use-pool mc-use-pool)) (length data)))
      (mc-set key "0" :memcache memcache :mc-use-pool mc-use-pool)
      (format t "~%~:[FAIL~;Success~] INCR" (eq (mc-incr key  :memcache memcache :mc-use-pool mc-use-pool) 1))
      (format t "~%~:[FAIL~;Success~] DECR" (eq (mc-decr key  :memcache memcache :mc-use-pool mc-use-pool) 0)))))

(defun mc-gets-test (&key (memcache *memcache*) (mc-use-pool *mc-use-pool*))
  (let* ((key "gets-test-key")
         (data1 "some initial data")
         (data2 "some new data")
         (response nil)
         (cas-unique nil))
    (mc-set key data1 :memcache memcache :mc-use-pool mc-use-pool)
    (setf response (mc-gets+ key :memcache memcache :mc-use-pool mc-use-pool))
    (when response
      (setf cas-unique (mc-cas-unique response))
      (format t "~%~:[FAIL~;Success~] GETS found key and cas value" cas-unique)
      (when cas-unique
        (format t "~%~:[FAIL~;Success~] CAS with correct value" (string= (mc-cas key data2 cas-unique :memcache memcache :mc-use-pool mc-use-pool) "STORED"))
        (format t "~%~:[FAIL~;Success~] CAS with incorrect value" (string= (mc-cas key data1 cas-unique :memcache memcache :mc-use-pool mc-use-pool) "EXISTS"))))))


;;;
;;; Meta Protocol functionality
;;;

(defun meta-server-request (command key &key flags data-len)
  "Internal function to construct a meta protocol command string."
  (babel:string-to-octets
   (with-output-to-string (str)
     (write-string command str)
     (write-char #\Space str)
     (write-string key str)
     (when data-len
       (write-char #\Space str)
       (princ data-len str))
     ;; Flags can be a single char, or a cons cell of (char . value)
     (loop for flag in flags
           do (write-char #\Space str)
              (if (consp flag)
                  (progn
                    (write-char (car flag) str)
                    (princ (cdr flag) str))
                  (write-char flag str)))
     (write-string +crlf+ str))
    :encoding +command-encoding+))

(defmacro mc-with-connection ((stream-var &key (memcache *memcache*) (use-pool *mc-use-pool*)) &body body)
  "Provides a macro to wrap a series of commands in a single connection from the pool.
The connection stream is bound to STREAM-VAR. This is the primary mechanism for pipelining."
  `(mc-with-pool-y/n (,memcache ,use-pool ,stream-var)
     ,@body))

(defun mc-read-meta-response (stream &key requested-flags)
  "Reads and parses a single response from a meta protocol stream.
This is intended for use after sending one or more quiet requests in a pipeline.
Returns two values:
1. A hash table containing the parsed response data (for 'VA' responses), or a string with the response code for other responses.
2. A boolean indicating if a value was found (only true for 'VA' responses)."
  (let* ((line (read-line-from-binary-stream stream))
         (parts (split-sequence:split-sequence #\Space line))
         (rc (first parts)))
    (cond
      ((string= rc "VA")
       ;; A VA response indicates a value is being returned.
       (let* ((data-len (parse-integer (second parts)))
              (tokens (cddr parts))
              (response-data (make-hash-table))
              (data-raw nil))
         ;; Parse the response tokens. The server returns tokens for flags
         ;; that were requested and have a value.
         (loop for token in tokens
               do (let ((flag-char (char token 0)))
                    (case flag-char
                      (#\c (setf (gethash :cas response-data) (subseq token 1)))
                      (#\O (setf (gethash :opaque response-data) (subseq token 1)))
                      (#\k (setf (gethash :key response-data) (subseq token 1)))
                      (#\W (setf (gethash :win response-data) t))
                      (#\Z (setf (gethash :already-won response-data) t))
                      (#\X (setf (gethash :stale response-data) t)))))
         ;; If the 'v' flag was requested, read the data block.
         (when (find #\v requested-flags)
           (setf data-raw (make-sequence '(vector (unsigned-byte 8)) data-len))
           (read-sequence data-raw stream)
           (read-line-from-binary-stream stream) ; consume trailing CRLF
           (setf (gethash :value response-data) data-raw))
         (values response-data t)))
      ;; Other response codes are simpler and don't have a data block.
      ((or (string= rc "EN") (string= rc "HD") (string= rc "EX") (string= rc "ST") (string= rc "MN"))
       (values rc nil))
      (t
       (cl-mc-error "Unknown meta response: ~a" line)))))

(defun mc-meta-get (key &key (stream nil) (value t) (cas nil) (recache-on-miss-ttl nil) (early-recache-ttl nil)
                        (quiet nil) (opaque nil) (return-key nil) (key-is-base64 nil))
  "Retrieves a key using the 'mg' meta command.
This is a flexible get operation that supports advanced caching features.

- :stream: An existing connection stream for pipelining. If NIL, a new connection is used.
- :value: If true, requests the item's value (the 'v' flag).
- :cas: If true, requests the item's CAS value (the 'c' flag).
- :recache-on-miss-ttl: If set, uses the 'N' flag to create a placeholder item if the key is not found, to prevent dogpiling. The value is the TTL of the placeholder.
- :early-recache-ttl: If set, uses the 'R' flag. If the item's remaining TTL is less than this value, the server may signal to recache.
- :quiet: If true, sends the command but does not read the response (the 'q' flag). For pipelining.
- :opaque: An opaque token to be reflected in the response (the 'O' flag).
- :return-key: If true, asks the server to return the key in the response (the 'k' flag).
- :key-is-base64: If true, indicates the key is base64-encoded (the 'b' flag).

Returns two values:
1. A hash table with the response data, or a response code string.
2. A boolean indicating if a value was found."
  (flet ((do-it (s)
           (let ((flags ()))
             (when value (push #\v flags))
             (when cas (push #\c flags))
             (when recache-on-miss-ttl (push (cons #\N recache-on-miss-ttl) flags))
             (when early-recache-ttl (push (cons #\R early-recache-ttl) flags))
             (when quiet (push #\q flags))
             (when opaque (push (cons #\O opaque) flags))
             (when return-key (push #\k flags))
             (when key-is-base64 (push #\b flags))
             (setf flags (reverse flags))

             (write-sequence (meta-server-request "mg" key :flags flags) s)
             (force-output s)

             (unless quiet
               (mc-read-meta-response s :requested-flags flags)))))
    (if stream
        (do-it stream)
        (mc-with-connection (s)
          (do-it s)))))

(defun mc-meta-set (key data &key (stream nil) (ttl 0) (client-flags 0) (cas nil) (quiet nil) (keep-stale nil)
                               (key-is-base64 nil) (opaque nil) (return-key nil))
  "Stores a key using the 'ms' meta command.

- :stream: An existing connection stream for pipelining.
- :ttl: The time-to-live for the item in seconds (the 'T' flag).
- :client-flags: The 32-bit flags value for the item (the 'F' flag).
- :cas: A CAS value for a check-and-set operation (the 'C' flag).
- :quiet: If true, sends the command but does not read the response (the 'q' flag).
- :keep-stale: If true, keeps a stale item marked as stale after an update (the 'I' flag).
- :key-is-base64: If true, indicates the key is base64-encoded (the 'b' flag).
- :opaque: An opaque token to be reflected in the response (the 'O' flag).
- :return-key: If true, asks the server to return the key in the response (the 'k' flag).

Returns the server response code (e.g., \"HD\", \"EX\") or NIL if quiet."
  (flet ((do-it (s)
           (let ((flags ())
                 (data-octets (if (typep data '(array (unsigned-byte 8)))
                                  data
                                  (babel:string-to-octets data))))
             (when (> ttl 0) (push (cons #\T ttl) flags))
             (when (> client-flags 0) (push (cons #\F client-flags) flags))
             (when cas (push (cons #\C cas) flags))
             (when quiet (push #\q flags))
             (when keep-stale (push #\I flags))
             (when key-is-base64 (push #\b flags))
             (when opaque (push (cons #\O opaque) flags))
             (when return-key (push #\k flags))
             (setf flags (reverse flags))

             (write-sequence (meta-server-request "ms" key :flags flags :data-len (length data-octets)) s)
             (force-output s)
             (write-sequence data-octets s)
             (write-sequence (babel:string-to-octets +crlf+) s)
             (force-output s)

             (unless quiet
               (mc-read-meta-response s)))))
    (if stream
        (do-it stream)
        (mc-with-connection (s)
          (do-it s)))))

(defun mc-meta-delete (key &key (stream nil) (cas nil) (quiet nil) (mark-stale nil) (stale-ttl nil)
                           (key-is-base64 nil) (opaque nil) (return-key nil))
  "Deletes a key using the 'md' meta command.

- :stream: An existing connection stream for pipelining.
- :cas: A CAS value for a check-and-set delete.
- :quiet: If true, sends the command but does not read the response.
- :mark-stale: If true, marks the item as stale instead of deleting it (the 'I' flag).
- :stale-ttl: When marking as stale, sets a new TTL for the stale item (the 'T' flag).
- :key-is-base64: If true, indicates the key is base64-encoded (the 'b' flag).
- :opaque: An opaque token to be reflected in the response (the 'O' flag).
- :return-key: If true, asks the server to return the key in the response (the 'k' flag).

Returns the server response code (e.g., \"HD\") or NIL if quiet."
  (flet ((do-it (s)
           (let ((flags ()))
             (when cas (push (cons #\C cas) flags))
             (when quiet (push #\q flags))
             (when mark-stale (push #\I flags))
             (when stale-ttl (push (cons #\T stale-ttl) flags))
             (when key-is-base64 (push #\b flags))
             (when opaque (push (cons #\O opaque) flags))
             (when return-key (push #\k flags))
             (setf flags (reverse flags))

             (write-sequence (meta-server-request "md" key :flags flags) s)
             (force-output s)

             (unless quiet
               (mc-read-meta-response s)))))
    (if stream
        (do-it stream)
        (mc-with-connection (s)
          (do-it s)))))

(defun mc-meta-noop (&key (stream nil))
  "Sends a 'mn' (meta no-op) command.
This is primarily used to flush a pipeline of quiet requests and get a definitive response.
Returns \"MN\"."
  (flet ((do-it (s)
           (write-sequence (meta-server-request "mn" "") s)
           (force-output s)
           (mc-read-meta-response s)))
    (if stream
        (do-it stream)
        (mc-with-connection (s)
          (do-it s)))))

(defun mc-meta-test (&key (memcache *memcache*) (mc-use-pool *mc-use-pool*))
  (let ((key "meta-test-key")
        (data1 "meta data 1")
        (data2 "meta data 2"))
    (mc-meta-set key data1)
    (multiple-value-bind (response foundp) (mc-meta-get key)
      (if foundp
          (progn
            (format t "~%~:[FAIL~;Success~] Meta GET found key" t)
            (format t "~%~:[FAIL~;Success~] Meta GET got correct data"
                    (string= (babel:octets-to-string (gethash :value response)) data1)))
          (format t "~%FAIL Meta GET did not find key")))

    (multiple-value-bind (response foundp) (mc-meta-get key :cas t)
      (if foundp
          (let ((cas (gethash :cas response)))
            (if cas
                (progn
                  (format t "~%~:[FAIL~;Success~] Meta GET with CAS got a CAS value" t)
                  (let ((set-response (mc-meta-set key data2 :cas cas)))
                    (format t "~%~:[FAIL~;Success~] Meta SET with correct CAS" (string= set-response "HD")))
                  (let ((set-response (mc-meta-set key data1 :cas cas)))
                    (format t "~%~:[FAIL~;Success~] Meta SET with incorrect CAS" (string= set-response "EX"))))
                (format t "~%FAIL Meta GET with CAS did not get a CAS value")))
          (format t "~%FAIL Meta GET did not find key for CAS test")))))

(defun mc-meta-advanced-test (&key (memcache *memcache*) (mc-use-pool *mc-use-pool*))
  (let ((key "meta-adv-test-key")
        (data "some data"))
    ;; Test for N flag (dogpiling)
    (mc-del key) ; ensure key doesn't exist
    (multiple-value-bind (response foundp) (mc-meta-get key :cas t :recache-on-miss-ttl 30)
      (if foundp
          (progn
            (format t "~%~:[FAIL~;Success~] Meta GET with N flag got a win" (gethash :win response))
            (let ((cas (gethash :cas response)))
              (when cas
                (mc-meta-set key data :cas cas)
                (multiple-value-bind (r2 f2) (mc-meta-get key)
                  (format t "~%~:[FAIL~;Success~] Can get key after setting with win" f2)
                  (when f2
                    (format t "~%~:[FAIL~;Success~] Data is correct after setting with win"
                            (string= (babel:octets-to-string (gethash :value r2)) data)))))))
          (format t "~%FAIL Meta GET with N flag did not get a win response")))

    (multiple-value-bind (response foundp) (mc-meta-get key :recache-on-miss-ttl 30)
      (if foundp
          (format t "~%~:[FAIL~;Success~] Meta GET with N flag on existing key got already-won"
                  (gethash :already-won response))
          (format t "~%FAIL Meta GET with N on existing key failed")))

    ;; Test for I flag (stale data)
    (mc-meta-set key data)
    (mc-meta-delete key :mark-stale t)
    (multiple-value-bind (response foundp) (mc-meta-get key)
      (if foundp
          (progn
            (format t "~%~:[FAIL~;Success~] Meta GET on stale data found key" t)
            (format t "~%~:[FAIL~;Success~] Meta GET on stale data returned stale flag" (gethash :stale response))
            (format t "~%~:[FAIL~;Success~] Meta GET on stale data returned win flag" (gethash :win response))
            (format t "~%~:[FAIL~;Success~] Meta GET on stale data returned correct data"
                    (string= (babel:octets-to-string (gethash :value response)) data)))
          (format t "~%FAIL Meta GET on stale data did not find key")))))

(defun mc-meta-pipelining-test (&key (memcache *memcache*) (mc-use-pool *mc-use-pool*))
  (let ((key1 "pipe-test-1") (data1 "pipe-data-1")
        (key2 "pipe-test-2") (data2 "pipe-data-2"))
    (mc-with-connection (s :memcache memcache :use-pool mc-use-pool)
      ;; Quiet sets
      (mc-meta-set key1 data1 :stream s :quiet t)
      (mc-meta-set key2 data2 :stream s :quiet t)

      ;; Quiet gets with opaque tokens
      (mc-meta-get key1 :stream s :quiet t :opaque "req1")
      (mc-meta-get key2 :stream s :quiet t :opaque "req2")
      (mc-meta-get "non-existent-key" :stream s :quiet t :opaque "req3")

      ;; Final non-quiet command to flush the pipeline
      (mc-meta-noop :stream s)

      ;; Read responses
      (multiple-value-bind (r1 f1) (mc-read-meta-response s)
        (format t "~%~:[FAIL~;Success~] Pipeline response 1 found" f1)
        (when f1 (format t "~%~:[FAIL~;Success~] Pipeline response 1 opaque correct" (string= (gethash :opaque r1) "req1"))))

      (multiple-value-bind (r2 f2) (mc-read-meta-response s)
        (format t "~%~:[FAIL~;Success~] Pipeline response 2 found" f2)
        (when f2 (format t "~%~:[FAIL~;Success~] Pipeline response 2 opaque correct" (string= (gethash :opaque r2) "req2"))))

      (multiple-value-bind (r3 f3) (mc-read-meta-response s)
        (format t "~%~:[FAIL~;Success~] Pipeline response 3 not found" (not f3))
        (when (not f3) (format t "~%~:[FAIL~;Success~] Pipeline response 3 is EN" (string= r3 "EN"))))

      (let ((r4 (mc-read-meta-response s)))
        (format t "~%~:[FAIL~;Success~] Pipeline noop response is MN" (string= r4 "MN"))))))

;;;EOF
