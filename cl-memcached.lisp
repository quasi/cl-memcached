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



(defun mc-get (keys-list &key (memcache *memcache*) (mc-use-pool *mc-use-pool*))
  (when (not (listp keys-list))
    (cl-mc-error "KEYS-LIST has to be a LIST of keys"))
  (mc-with-pool-y/n (memcache mc-use-pool s)
    (write-sequence (server-request (append (list "get") keys-list)) s)
    (force-output s)
    (loop for x = (read-line-from-binary-stream s)
       until (search "END" x :test #'string-equal)
       collect (let* ((status-line (split-sequence:split-sequence #\Space x))
		      (key (second status-line))
		      (flags (third status-line))
		      (bytes (parse-integer (fourth status-line)))
		      (cas-unique (fifth status-line))
		      (seq (make-sequence '(vector (unsigned-byte 8)) bytes)))
		 (read-sequence seq s)
		 (read-line-from-binary-stream s)
		 (list key flags bytes cas-unique seq)))))



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
    (if (= (length result) 1)
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

(defun mc-stats (&key (memcache *memcache*) (mc-use-pool *mc-use-pool*))
  "Returns an ALIST of stats data from memcached server"
  (mc-with-pool-y/n (memcache mc-use-pool s)
    (write-sequence (server-request (list "stats")) s)
    (force-output s)
    (loop for line = (read-line-from-binary-stream s)
       collect (let ((param (split-sequence:split-sequence #\Space line)))
		 (cons (second param) (third param)))
       until (search "END" line  :test #'string-equal))))



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

;;;EOF
