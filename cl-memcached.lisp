;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

;;; Copyright (c) 2006-2009, Abhijit 'quasi' Rao.  All rights reserved.

;;; Library provided under BSD Licence.

;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


(in-package #:cl-memcached)


;; (eval-when (:compile-toplevel :load-toplevel :execute)
;;   (declaim (optimize (speed 3) (debug 0))))

;;; Some global variables

(defvar *memcache*
  "Represents a particular Memcached server")

(defvar *use-pool* nil
  "Default value for the USE-POOL keyword parameter in memcached functions")

(defvar *pool-get-trys?* nil
  "If true then it will try to wait and sleep for a while if pool item in unavailable,
if nil then will return immideatly")

(defvar *default-encoding* (flex:make-external-format :iso-8859-1)
  "Default encoding")

;;; Information from the Memcached server

(defstruct
  (memcache-stats
   (:conc-name mc-stats-)
   (:print-function
    (lambda (struct stream depth)
      (declare (ignore depth))
      (format stream "#<MEMCACHED-SERVER-STATS PID:~A MAX-BYTES:~dMb CURR-ITEMS:~d TOT-ITEMS:~D>"
	      (mc-stats-pid struct)
	      (/ (mc-stats-limit-maxbytes struct) 1024 1024)
	      (mc-stats-curr-items struct)
	      (mc-stats-total-items struct)))))
"The structure which holds the statistics from the memcached server. The fields are :
field-name                 accessor-function                 documentation
----------                 -----------------                 -------------
pid                        mc-stats-pid                      Process id of this server process
uptime                     mc-stats-uptime                   Number of seconds this server has been running
time                       mc-stats-time                     current UNIX time according to the server
version                    mc-stats-version                  Version string of this server
rusage-user                mc-stats-rusage-user              Accumulated user time for this process
rusage-system              mc-stats-rusage-system            Accumulated system time for this process
curr-items                 mc-stats-curr-items               Current number of items stored by the server
total-items                mc-stats-total-items              Total number of items stored by this server ever since it started
bytes                      mc-stats-bytes                    Current number of bytes used by this server to store items
curr-connections           mc-stats-curr-connections         Number of open connections
total-connections          mc-stats-total-connections        Total number of connections opened since the server started running
connection-structures      mc-stats-connection-structures    Number of connection structures allocated by the server
cmd-get                    mc-stats-cmd-get                  Cumulative number of retrieval requests
cmd-set                    mc-stats-cmd-set                  Cumulative number of storage requests
get-hits                   mc-stats-get-hits                 Number of keys that have been requested and found present
get-misses                 mc-stats-get-misses               Number of items that have been requested and not found
evictions                  mc-stats-evictions                Number of items removed from cache because they passed their expiration time
bytes-read                 mc-stats-bytes-read               Total number of bytes read by this server from network
bytes-written              mc-stats-bytes-written            Total number of bytes sent by this server to network
limit-maxbytes             mc-stats-limit-maxbytes           Number of bytes this server is allowed to use for storage.
"
  pid uptime time version rusage-user rusage-system curr-items total-items
  bytes curr-connections total-connections connection-structures cmd-get cmd-set
  get-hits get-misses evictions bytes-read bytes-written limit-maxbytes)





;;;
;;; The main class which represents the memcached server
;;;

(defclass cache ()
  ((name
    :initarg :name
    :reader name
    :type simple-string
    :documentation "String identifier"))
  (:documentation "Base class of all cache types"))


(defclass memcache (cache)
  ((ip
    :initarg :ip
    :initform "127.0.0.1"
    :accessor ip
    :type simple-string
    :documentation "The IP address of the Memcached server this instance represents")
   (port
    :initarg :port
    :initform 11211
    :accessor port
    :type fixnum
    :documentation "The port on which the Memcached server this instance represents runs")
   (pool-size
    :initarg :pool-size
    :initform 2
    :type fixnum
    :reader pool-size)
   (pool
    :reader pool))
  (:documentation "This class represents an instance of the Memcached server"))


(defmethod print-object ((mc memcache) stream)
  (format stream "#<~S ~A on ~A:~A>"
	  (type-of mc)
	  (when (slot-boundp mc 'name) (name mc))
	  (when (slot-boundp mc 'ip) (ip mc))
	  (when (slot-boundp mc 'port) (port mc))))


(defmethod initialize-instance :after ((memcache memcache) &rest initargs)
  (declare (ignore initargs))
  (setf (slot-value memcache 'pool) (make-instance 'memcache-connection-pool :name (concatenate 'simple-string (name memcache) " - Connection Pool") :max-capacity (pool-size memcache)))
  (handler-case (mc-pool-init :memcache memcache)
    (error () nil)))


(defun make-memcache (&key (ip "127.0.0.1") (port 11211) (name "Cache Interface Instance") (pool-size 2))
  "Creates an instance of class MEMCACHE which represents a memcached server"
  (make-instance 'memcache :name name :ip ip :port port :pool-size pool-size))




;;; Error Conditions

(define-condition cl-mc-condition (condition)
  ()
  (:documentation "Superclass for all conditions related to CL-MC."))

(define-condition cl-mc-error (cl-mc-condition error)
  ()
  (:documentation "Superclass for all errors related to CL-MC."))

(define-condition cl-mc-simple-error (cl-mc-error simple-condition)
  ()
  (:documentation "Like CL-MC-ERROR but with formatting capabilities."))


(define-condition memcached-server-unreachable (cl-mc-error)
  ())

(define-condition memcache-pool-empty (cl-mc-error)
  ())

(define-condition cannot-make-pool-object (cl-mc-error)
  ())

(define-condition bad-pool-object (cl-mc-error)
  ())

(defun cl-mc-error (format-control &rest format-arguments)
  "Signals an error of type TRIPR-SIMPLE-ERROR with the provided
format control and arguments."
  (error 'cl-mc-simple-error
         :format-control format-control
         :format-arguments format-arguments))

;;;
;;;
;;; Memcached Pooled Access
;;;
;;;

(defclass memcache-connection-pool ()
  ((name
    :initarg :name
    :reader name
    :initform "Connection Pool"
    :type simple-string
    :documentation "Name of this pool")
   (pool
    :initform (make-queue)
    :accessor pool)
   (pool-lock
    :reader pool-lock
    :initform (bordeaux-threads:make-lock "Memcache Connection Pool Lock"))
   (max-capacity
    :initarg :max-capacity
    :reader max-capacity
    :initform 2
    :type fixnum
    :documentation "Total capacity of the pool to hold pool objects")
   (current-size
    :accessor current-size
    :type fixnum
    :initform 0)
   (currently-in-use
    :accessor currently-in-use
    :initform 0
    :type fixnum
    :documentation "Pool objects currently in Use")
   (total-uses
    :accessor total-uses
    :initform 0
    :type fixnum
    :documentation "Total uses of the pool")
   (total-created
    :accessor total-created
    :initform 0
    :type fixnum
    :documentation "Total pool objects created")
   (pool-grow-requests
    :initform 0
    :accessor pool-grow-requests
    :type fixnum
    :documentation "Pool Grow Request pending Action")
   (pool-grow-lock
    :initform (bordeaux-threads:make-lock "Pool Grow Lock")
    :reader pool-grow-lock))
  (:documentation "A memcached connection pool object"))


(defmethod print-object ((mcp memcache-connection-pool) stream)
  (format stream "#<~S Capacity:~d, Currently in use:~d>"
	  (type-of mcp)
	  (when (slot-boundp mcp 'max-capacity) (max-capacity mcp))
	  (when (slot-boundp mcp 'currently-in-use) (currently-in-use mcp))))


(defun mc-put-in-pool (conn &key (memcache *memcache*))
  (bordeaux-threads:with-lock-held ((pool-lock (pool memcache)))
    (enqueue (pool (pool memcache)) conn)
    (decf (currently-in-use (pool memcache)))))


(defun mc-get-from-pool (&key (memcache *memcache*))
  "Returns a pool object from pool."
  (let (pool-object (state t))
    (bordeaux-threads:with-lock-held ((pool-lock (pool memcache)))
      (if (queue-empty-p (pool (pool memcache)))
	  (setf state nil)
	  (progn (incf (currently-in-use (pool memcache)))
		 (incf (total-uses (pool memcache)))
		 (setf pool-object (dequeue (pool (pool memcache)))))))
    (if state
	pool-object
	(error 'memcache-pool-empty))))


(defun mc-get-from-pool-with-try (&key (memcache *memcache*) (tries 5) (try-interval 1))
  ""
  (let ((tr 1))
    (loop
       (progn (when (> tr tries)
		(return nil))
	      (let ((conn (handler-case (mc-get-from-pool :memcache memcache)
			    (memcache-pool-empty () nil))))
		(if (not conn)
		    (incf tr)
		    (return conn)))))))


(defun mc-pool-init (&key (memcache *memcache*))
  "Cleans up the pool for this particular instance of memcache
& reinits it with POOL-SIZE number of objects required by this pool"
  (mc-pool-cleanup memcache)
  (dotimes (i (pool-size memcache))
    (mc-pool-grow-request memcache))
  (mc-pool-grow memcache))


(defun mc-make-pool-item (&key (memcache *memcache*))
  (handler-case (usocket:socket-connect (ip memcache) (port memcache)
					:element-type #-sbcl '(unsigned-byte 8) #+sbcl :default)
    (usocket:socket-error () (error 'memcached-server-unreachable))
    (error () (error 'cannot-make-pool-object))))


(defun mc-pool-grow (memcache)
  (let (grow-count pool-item-list)
    (bordeaux-threads:with-lock-held ((pool-grow-lock (pool memcache)))
      (setf grow-count (pool-grow-requests (pool memcache)))
      (setf pool-item-list (remove nil (loop for x from 1 to grow-count
					  collect (mc-make-pool-item :memcache memcache))))
      (loop for x from 1 to (length pool-item-list)
	 do (bordeaux-threads:with-lock-held ((pool-lock (pool memcache)))
	      (enqueue (pool (pool memcache)) (pop pool-item-list))
	      (incf (total-created (pool memcache)))
	      (incf (current-size (pool memcache))))
	 do (decf (pool-grow-requests (pool memcache)))))))




(defun mc-destroy-pool-item (pool-item)
  (ignore-errors (usocket:socket-close pool-item)))


(defun mc-pool-grow-request (memcache)
  (bordeaux-threads:with-lock-held ((pool-grow-lock (pool memcache)))
    (if (> (max-capacity (pool memcache)) (+ (current-size (pool memcache))
					     (pool-grow-requests (pool memcache))))
	(incf (pool-grow-requests (pool memcache)))
	(warn "CL-MC: Pool is at Capacity"))))


(defun mc-chuck-from-pool (object memcache)
 #| (mc-destroy-pool-item object)
  (bordeaux-threads:with-lock-held ((pool-lock (pool memcache)))
    (decf (current-size (pool memcache))))
  (loop while (mc-pool-grow-request memcache))
  (mc-pool-grow memcache)|#
  (mc-pool-init :memcache memcache))

(defun mc-pool-cleanup (memcache)
  (bordeaux-threads:with-lock-held ((pool-lock (pool memcache)))
    (bordeaux-threads:with-lock-held ((pool-grow-lock (pool memcache)))
      (loop
	 when (queue-empty-p (pool (pool memcache)))
	 do (return)
	 else do (mc-destroy-pool-item (dequeue (pool (pool memcache)))))
      (setf (current-size (pool memcache)) 0
	    (currently-in-use (pool memcache)) 0
	    (pool-grow-requests (pool memcache)) 0
	    (total-created (pool memcache)) 0
	    (total-uses (pool memcache)) 0))))





(defmacro mc-with-pool-y/n (&body body)
  "Macro to wrap the use-pool/dont-use-pool stuff and the cleanup
around a body of actual action statements"
  `(let (us)
     (if use-pool
	 (setf us (if *pool-get-trys?*
		      (mc-get-from-pool-with-try :memcache cache)
		      (mc-get-from-pool :memcache cache)))
	 (setf us (mc-make-pool-item :memcache cache)))
     (unwind-protect
	  (when us
	    (let ((s (usocket:socket-stream us)))
	      (handler-case (progn ,@body)
		(error (c) (when use-pool
			     (mc-chuck-from-pool us cache))
		       (error c)))))
       (if use-pool
	   (mc-put-in-pool us :memcache cache)
	   (ignore-errors (usocket:socket-close us))))))



(defun mc-stats-raw (&key (cache *memcache*) (use-pool *use-pool*))
  "Returns Raw stats data from memcached server to be used by the mc-stats function"
  (mc-with-pool-y/n
    (with-output-to-string (str)
      (format s "stats~A" +crlf+)
      (force-output s)
      (loop for line = (copy-seq (read-line s))
	 do (princ line str)
	 until (search "END" line)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;---------------------------------------------------------


;;;
;;;
;;; Memcached API functionality
;;;
;;;




;;;
;;; Statistics from the memcached server
;;;

;;; FIXME: Need to write methods to get good stats for different types types of caches

(defun mc-stats (memcache &key (use-pool *use-pool*))
  "Returns a struct of type memcache-stats which contains internal statistics from the
memcached server instance.  Please refer to documentation of memcache-stats for detailed
information about each slot"
  (let* ((result (mc-stats-raw :cache memcache :use-pool use-pool))
	 (l (split-sequence:split-sequence #\Return result))
	 xx temp)
    (setf temp (loop for x in l
		  do (setf xx (split-sequence:split-sequence " " x :remove-empty-subseqs t :test #'string-equal))
		  collect (list (second xx) (third xx))))
    (make-memcache-stats
     :pid (parse-integer (second (assoc "pid" temp :test #'string-equal)) :junk-allowed t)
     :uptime (parse-integer (second (assoc "uptime" temp :test #'string-equal)) :junk-allowed t)
     :time (parse-integer (second (assoc "time" temp :test #'string-equal)) :junk-allowed t)
     :version (second (assoc "version" temp :test #'string-equal))
     :rusage-user (second (assoc "rusage_user" temp :test #'string-equal))
     :rusage-system (second (assoc "rusage_system" temp :test #'string-equal))
     :curr-items (parse-integer (second (assoc "curr_items" temp :test #'string-equal)) :junk-allowed t)
     :total-items (parse-integer (second (assoc "total_items" temp :test #'string-equal)) :junk-allowed t)
     :bytes (parse-integer (second (assoc "bytes" temp :test #'string-equal)) :junk-allowed t)
     :curr-connections (parse-integer (second (assoc "curr_connections" temp :test #'string-equal)) :junk-allowed t)
     :total-connections (parse-integer (second (assoc "total_connections" temp :test #'string-equal)) :junk-allowed t)
     :connection-structures (parse-integer (second (assoc "connection_structures" temp :test #'string-equal)) :junk-allowed t)
     :cmd-get (parse-integer (second (assoc "cmd_get" temp :test #'string-equal)) :junk-allowed t)
     :cmd-set (parse-integer (second (assoc "cmd_set" temp :test #'string-equal)) :junk-allowed t)
     :get-hits (parse-integer (second (assoc "get_hits" temp :test #'string-equal)) :junk-allowed t)
     :get-misses (parse-integer (second (assoc "get_misses" temp :test #'string-equal)) :junk-allowed t)
     :evictions (parse-integer (second (assoc "evictions" temp :test #'string-equal)) :junk-allowed t)
     :bytes-read (parse-integer (second (assoc "bytes_read" temp :test #'string-equal)) :junk-allowed t)
     :bytes-written (parse-integer (second (assoc "bytes_written" temp :test #'string-equal)) :junk-allowed t)
     :limit-maxbytes (parse-integer (second (assoc "limit_maxbytes" temp :test #'string-equal)) :junk-allowed t))))




;;; SET/ADD/REPLACE functionality

(defgeneric mc-store (cache key data &key command timeout use-pool)
  (:documentation ""))

(defmethod mc-store ((cache memcache) key data &key (command :set) (timeout 0) (use-pool *use-pool*))
  "Stores data in the memcached server using the :command command.
key => key by which the data is stored. this is of type SIMPLE-STRING
data => data to be stored into the cache. data is a sequence of type (UNSIGNED-BYTE 8)
length => size of data
memcache => The instance of class memcache which represnts the memcached we want to use.
command => The storage command we want to use.  There are 3 available : set, add & replace.
timeout => The time in seconds when this data expires.  0 is never expire."
  (declare (type fixnum timeout) (type simple-string key)
	   (type (member :set :add :replace) command))
  (unless (equal (array-element-type data) '(UNSIGNED-BYTE 8))
    (cl-mc-error "Data has to be a ARRAY with ELEMENT-TYPE of (UNSIGNED-BYTE 8)"))
  (let ((len (length data)))
    (mc-with-pool-y/n
      (write-string (case command
		      (:set "set")
		      (:add "add")
		      (:replace "replace")
		      (t (error "Unknown Command : ~a.  'command' has to be one of :set :add or :replace" command))) s)
      (write-char #\Space s)
      (write-string key s)
      (write-char #\Space s)
      (write-string "0" s)
      (write-char #\Space s)
      (write-string (princ-to-string timeout) s)
      (write-char #\Space s)
      (write-string (princ-to-string len) s)
      (write-string +crlf+ s)
      (force-output s)
      (write-sequence data s)
      (write-string +crlf+ s)
      (force-output s)
      (remove #\Return (read-line s nil nil)))))

(defgeneric mc-store+ (cache key data &key command timeout use-pool external-format))

(defmethod mc-store+ ((cache memcache) key data &key (command :set) (timeout 0) (use-pool *use-pool*) (external-format *default-encoding*))
  (if (equal (array-element-type data) 'CHARACTER)
      (mc-store cache key (flex:string-to-octets data :external-format external-format) :command command :timeout timeout :use-pool use-pool)
      (cl-mc-error "To be used only on strings")))


;;; GET 'key(s)' functionality

(defgeneric mc-get (cache keys-list &key use-pool))

(defmethod mc-get ((cache memcache) keys-list &key (use-pool *use-pool*))
  "Retrive value for key from memcached server.
keys-list => is a list of the keys, seperated by whitespace, by which data is stored in memcached
memcache => The instance of class memcache which represnts the memcached we want to use.

Returns a list of lists where each list has two elements key and value
key is of type SIMPLE-STRING
value is of type (UNSIGNED-BYTE 8)"
  (when (not (listp keys-list))
    (cl-mc-error "KEYS-LIST has to be of type LIST"))
  (mc-with-pool-y/n
    (write-string "get " s)
    (loop for key in keys-list
       do (write-string key s)
       do (write-char #\Space s))
    (write-string +crlf+ s)
    (force-output s)
    (loop for x = (read-line s nil nil)
       until (search "END" x :test #'string-equal)
       collect (let* ((status-line (split-sequence:split-sequence #\Space x))
		      (len (parse-integer (fourth status-line)))
		      (seq (make-sequence '(vector (unsigned-byte 8)) len)))
		 (read-sequence seq s)
		 (read-line s nil nil)
		 (list (second status-line) seq)))))


;;; Utility function to get single key->value pairs, especially when value is of type simple string

(defgeneric mc-get+ (cache key-or-list-of-keys &key use-pool external-format))

(defmethod mc-get+ ((cache cache) key-or-list-of-keys &key (use-pool *use-pool*) (external-format *default-encoding*))
  (if (listp key-or-list-of-keys)
      (mapcar #'(lambda (x) (list (first x) (flex:octets-to-string (second x) :external-format external-format)))
	      (mc-get cache key-or-list-of-keys :use-pool use-pool))
      (flex:octets-to-string (second (first (mc-get cache (list key-or-list-of-keys) :use-pool use-pool))) :external-format external-format)))




;;; DELETE functionality

(defgeneric mc-del (cache key &key time use-pool))

(defmethod mc-del ((cache memcache) key &key (time 0) (use-pool *use-pool*))
  "Deletes a particular 'key' and it's associated data from the memcached server"
  (declare (type fixnum time))
  (mc-with-pool-y/n
    (write-string "delete " s)
    (write-string key s)
    (write-char #\Space s)
    (write-string (princ-to-string time) s)
    (write-string +crlf+ s)
    (force-output s)
    (let ((l (read-line s nil nil)))
      (remove #\Return l))))


;;; INCR functionality

(defgeneric mc-incr (cache key &key value use-pool))

(defmethod mc-incr ((cache memcache) key &key (value 1) (use-pool *use-pool*))
  "Implements the INCR command.  Increments the value of a key. Please read memcached documentation for more information
key is a string value is an integer"
  (declare (type fixnum value))
  (mc-with-pool-y/n
    (write-string "incr " s)
    (write-string key s)
    (write-char #\Space s)
    (write-string (princ-to-string value) s)
    (write-string +crlf+ s)
    (force-output s)
    (let ((l (read-line s nil nil)))
      (remove #\Return l))))

;;; DECR functionality

(defgeneric mc-decr (cache key &key value use-pool))

(defmethod mc-decr ((cache memcache) key &key (value 1) (use-pool *use-pool*))
  "Implements the DECR command.  Decrements the value of a key. Please read memcached documentation for more information"
  (declare (type fixnum value))
  (mc-with-pool-y/n
    (write-string "decr " s)
    (write-string key s)
    (write-char #\Space s)
    (write-string (princ-to-string value) s)
    (write-string +crlf+ s)
    (force-output s)
    (let ((l (read-line s nil nil)))
      (remove #\Return l))))


(defgeneric quick-stat (cache))

(defmethod quick-stat ((cache memcache))
  (let ((s (mc-stats cache)))
    (format nil "CURR-ITEMS:~A TOT-ITEMS:~A CMD-GET:~A CMD-SET:~A GET-HITS:~A GET-MISSES:~A~%"
	    (mc-stats-curr-items s)
	    (mc-stats-total-items s)
	    (mc-stats-cmd-get s)
	    (mc-stats-cmd-set s)
	    (mc-stats-get-hits s)
	    (mc-stats-get-misses s))))


;;;--------------


(defclass cache-node (memcache)
  ((priority
    :initarg :priority
    :initform 1
    :reader priority)
   (weight
    :initarg :weight
    :initform 1
    :reader weight)
   (status
    :initarg :status
    :initform 'ACTIVE
    :accessor status))
  (:documentation "An individual entity from the RCLUSTER"))

(defmethod print-object ((r cache-node) stream)
  (format stream "#<~S ~a:~a priority:~A weight:~A status:~A>"
	  (type-of r)
	  (when (slot-boundp r 'ip) (ip r))
	  (when (slot-boundp r 'port) (port r))
	  (when (slot-boundp r 'priority) (priority r))
	  (when (slot-boundp r 'weight) (weight r))
	  (when (slot-boundp r 'status) (status r))))


(defun make-cache (name &key type)
  "Makes a CLUSTER object with an empty node list"
  (case type
    (:replicated (make-instance 'replicated-cache :node-list nil :name name))
    (:distributed (make-instance 'distributed-cache :name name))))


;;;-----------------------------------------------------------------
;;; Replication Cluster
;;;


(defclass replicated-cache (cache)
  ((node-list
    :initarg :node-list
    :accessor node-list
    :documentation "List of objects representing individual cache nodes"))
  (:documentation "A Replicated Cache Object"))

(defmethod print-object ((rc replicated-cache) stream)
  (format stream "#<~S ~A with ~A nodes>"
	  (type-of rc)
	  (when (slot-boundp rc 'name) (name rc))
	  (when (slot-boundp rc 'node-list) (length (node-list rc)))))



(defmethod add-cache-node ((cache replicated-cache) ip port &key (pool-size 2) (weight 1) (priority 1) (status 'ACTIVE))
  "Adds a node to the Replication Cache"
  (setf (node-list cache)
	(append (node-list cache)
		(list (make-instance 'cache-node :ip ip :port port :pool-size pool-size :priority priority :weight weight :status status)))))



(defmethod mc-store ((cache replicated-cache) key data &key (command :set) (timeout 0) (use-pool *use-pool*))
  "Store method for the Replicated-Cache"
  (loop for node in (node-list cache)
     collect (handler-case (mc-store node key data :command command :timeout timeout :use-pool use-pool)
	       (error nil))))


(defmethod mc-get ((cache replicated-cache) keys-list &key (use-pool *use-pool*))
  "GET method for the Replicated-Cache"
  (let ((len (length (node-list cache)))
	visited)
    (do ((pick 0 (loop for x = (random len)
		    until (not (member x visited))
		    do (push x visited)
		    return x))
	 (result (handler-case (mc-get (nth (random len) (node-list cache)) keys-list :use-pool use-pool)
		   (error nil)))
	 (count 0 (1+ count)))
	((or result (> count len)) result))))



(defmethod mc-del ((cache replicated-cache) key &key (time 0) (use-pool *use-pool*))
  "DEL method for the CACHE"
  (loop for node in (node-list cache)
     collect (handler-case (mc-del node key :time time :use-pool use-pool)
	       (error nil))))





;;;
;;; Distributed Cache
;;;

(defparameter +min-instances-on-continuum+ 200)

(defclass distributed-cache (cache)
  ((node-lookup-table
    :initarg :node-lookup-table
    :accessor node-lookup-table
    :initform (make-hash-table :test #'eql)
    :documentation "List of objects representing individual cache nodes")
   (continuum
    :initarg :continuum
    :accessor continuum
    :initform nil))
  (:documentation "A Distributed Cache Object"))

(defmethod print-object ((dc distributed-cache) stream)
  (format stream "#<~S ~A with ~A nodes>"
	  (type-of dc)
	  (when (slot-boundp dc 'name) (name dc))
	  (when (slot-boundp dc 'node-lookup-table) (hash-table-count (node-lookup-table dc)))))




(defmethod add-cache-node ((cache distributed-cache) ip port &key (pool-size 0) (weight 1) (priority 1) (status 'ACTIVE) (name "some-name"))
  "Adds a node to the DISTRIBUTED CACHE"
  (let ((server-lookup-hash (hash-sdbm (flex:string-to-octets (format nil "~A:~A" ip port)))))
    (loop for n from 1 to (* +min-instances-on-continuum+ weight)
       do (push (list (hash-sdbm (flex:string-to-octets (format nil "~A:~A:~A" ip port n))) server-lookup-hash) (continuum cache)))
    (setf (gethash server-lookup-hash (node-lookup-table cache))
	  (make-instance 'cache-node :ip ip :port port :pool-size pool-size :priority priority :weight weight :status status :name name))
    (setf (continuum cache) (sort (continuum cache) #'< :key #'first))
    ))


(defmethod delete-cache-node ((cache distributed-cache) cache-node)
  (let ((server-lookup-hash (hash-SDBM (format nil "~A:~A" (ip cache-node) (port cache-node)))))
    (setf (continuum cache) (remove-if #'(lambda (x) (equal (second x) server-lookup-hash)) (continuum cache)))
    (remhash server-lookup-hash (node-lookup-table cache))))


(defun get-server-from-continuum (distributed-cache key)
  (let* ((key-hash (hash-sdbm key))
	 (continuum-entry (or (find key-hash (continuum distributed-cache) :test #'<= :key #'first)
			      (when (> key-hash (first (first (last (continuum distributed-cache)))))
				  (first (continuum distributed-cache))))))
    (if continuum-entry
	(gethash (second continuum-entry) (node-lookup-table distributed-cache)))))


(defmethod mc-store ((cache distributed-cache) key data &key (command :set) (timeout 0) (use-pool *use-pool*))
  "Store method for the distributed-Cache"
  (let ((node (get-server-from-continuum cache key)))
    (handler-case (mc-store node key data :command command :timeout timeout :use-pool use-pool)
    (error nil))))


(defmethod mc-get ((cache distributed-cache) key &key (use-pool *use-pool*))
  "GET method for the distributed-Cache"
  (let ((node (get-server-from-continuum cache key)))
    (handler-case (mc-get node (list key) :use-pool use-pool)
      (error nil))))



(defmethod mc-del ((cache distributed-cache) key &key (time 0) (use-pool *use-pool*))
  "DEL method for the CACHE"
  (let ((node (get-server-from-continuum cache key)))
    (handler-case (mc-del node key :use-pool use-pool :time time)
      (error nil))))



(defmethod quick-stat ((cache distributed-cache))
  (format t "~&Distributed Cache '~A' with ~a nodes:~%" (name cache) (hash-table-count (node-lookup-table cache)))
  (maphash #'(lambda (k v) (format t "~A:~A -> ~A" (ip v) (port v) (quick-stat v))) (node-lookup-table cache)))



