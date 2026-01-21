;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;;
;;; Tests for cl-memcached - verifying new features implemented by Google Jules
;;;
;;; ABOUTME: Test suite for cl-memcached covering mc-gets, stats commands,
;;; and the meta protocol implementation.

(defpackage #:cl-memcached-test
  (:use #:cl #:fiveam #:cl-memcached)
  (:export #:run-tests))

(in-package #:cl-memcached-test)

(def-suite cl-memcached-suite
  :description "Test suite for cl-memcached new features")

(in-suite cl-memcached-suite)

;;; Test fixture - create a memcache connection for tests
(defvar *test-memcache* nil)

(defun setup-test-memcache ()
  (setf *test-memcache* (make-memcache :ip "127.0.0.1" :port 11298))
  ;; Also bind the global *memcache* for meta protocol functions which don't
  ;; accept a :memcache parameter (bug in Jules' implementation)
  (setf *memcache* *test-memcache*))

(defun cleanup-test-key (key)
  (ignore-errors (mc-del key :memcache *test-memcache*)))

;;;
;;; Tests for mc-gets and mc-gets+ (Commit: 71fd866)
;;;

(def-suite gets-suite :in cl-memcached-suite
  :description "Tests for mc-gets and mc-gets+ functions")

(in-suite gets-suite)

(test mc-gets-returns-cas-value
  "Test that mc-gets returns CAS unique value"
  (setup-test-memcache)
  (let ((key "test-gets-cas"))
    (cleanup-test-key key)
    ;; Set a value first
    (mc-set key "test-data" :memcache *test-memcache*)
    ;; Get with mc-gets
    (let ((result (mc-gets (list key) :memcache *test-memcache*)))
      (is (not (null result)) "mc-gets should return a result")
      (is (= 1 (length result)) "Should have one result")
      (let ((item (first result)))
        (is (string= key (first item)) "Key should match")
        (is (not (null (fourth item))) "CAS unique value should be present")))
    (cleanup-test-key key)))

(test mc-gets+-returns-cas-in-response
  "Test that mc-gets+ returns a response with CAS unique value"
  (setup-test-memcache)
  (let ((key "test-gets-plus-cas"))
    (cleanup-test-key key)
    (mc-set key "hello-world" :memcache *test-memcache*)
    (let ((response (mc-gets+ key :memcache *test-memcache*)))
      (is (not (null response)) "mc-gets+ should return a response")
      (is (eq 'cl-memcached::memcache-response (type-of response)) "Should be a MEMCACHE-RESPONSE")
      (is (not (null (mc-cas-unique response))) "CAS unique should be present"))
    (cleanup-test-key key)))

(test mc-cas-with-correct-cas-succeeds
  "Test that CAS operation succeeds with correct CAS value"
  (setup-test-memcache)
  (let ((key "test-cas-correct"))
    (cleanup-test-key key)
    (mc-set key "initial-data" :memcache *test-memcache*)
    (let* ((response (mc-gets+ key :memcache *test-memcache*))
           (cas-value (mc-cas-unique response)))
      (is (not (null cas-value)) "Should have CAS value")
      (let ((result (mc-cas key "updated-data" cas-value :memcache *test-memcache*)))
        (is (string= "STORED" result) "CAS with correct value should return STORED")))
    (cleanup-test-key key)))

(test mc-cas-with-incorrect-cas-fails
  "Test that CAS operation fails with incorrect CAS value"
  (setup-test-memcache)
  (let ((key "test-cas-incorrect"))
    (cleanup-test-key key)
    (mc-set key "initial-data" :memcache *test-memcache*)
    (let* ((response (mc-gets+ key :memcache *test-memcache*))
           (cas-value (mc-cas-unique response)))
      ;; Update the key to invalidate the CAS
      (mc-set key "middle-data" :memcache *test-memcache*)
      ;; Try to CAS with old value
      (let ((result (mc-cas key "updated-data" cas-value :memcache *test-memcache*)))
        (is (string= "EXISTS" result) "CAS with stale value should return EXISTS")))
    (cleanup-test-key key)))

;;;
;;; Tests for stats commands (Commit: 71fd866)
;;;

(def-suite stats-suite :in cl-memcached-suite
  :description "Tests for mc-stats-items, mc-stats-slabs, mc-stats-sizes")

(in-suite stats-suite)

(test mc-stats-items-returns-alist
  "Test that mc-stats-items returns an alist"
  (setup-test-memcache)
  ;; Ensure there's at least one item in cache
  (mc-set "stats-test-key" "some-data" :memcache *test-memcache*)
  (let ((result (mc-stats-items :memcache *test-memcache*)))
    (is (listp result) "mc-stats-items should return a list")
    ;; May be empty if no items, but should still be a list
    (when result
      (is (consp (first result)) "Each item should be a cons cell")))
  (cleanup-test-key "stats-test-key"))

(test mc-stats-slabs-returns-alist
  "Test that mc-stats-slabs returns an alist"
  (setup-test-memcache)
  (let ((result (mc-stats-slabs :memcache *test-memcache*)))
    (is (listp result) "mc-stats-slabs should return a list")
    (when result
      (is (consp (first result)) "Each item should be a cons cell"))))

(test mc-stats-sizes-returns-alist
  "Test that mc-stats-sizes returns an alist"
  (setup-test-memcache)
  (let ((result (mc-stats-sizes :memcache *test-memcache*)))
    (is (listp result) "mc-stats-sizes should return a list")))

(test mc-stats-basic-returns-data
  "Test that basic mc-stats returns expected fields"
  (setup-test-memcache)
  (let ((result (mc-stats :memcache *test-memcache*)))
    (is (listp result) "mc-stats should return a list")
    (is (> (length result) 0) "mc-stats should return some data")
    ;; Check for some expected fields
    (is (not (null (assoc "version" result :test #'string=)))
        "Should have version field")
    (is (not (null (assoc "pid" result :test #'string=)))
        "Should have pid field")))

;;;
;;; Tests for Meta Protocol (Commit: 95087f9)
;;;

(def-suite meta-suite :in cl-memcached-suite
  :description "Tests for meta protocol functions")

(in-suite meta-suite)

(test mc-meta-set-and-get-basic
  "Test basic mc-meta-set and mc-meta-get"
  (setup-test-memcache)
  (let ((key "meta-basic-test")
        (data "hello meta protocol"))
    (cleanup-test-key key)
    ;; Set with meta protocol
    (let ((set-result (mc-meta-set key data)))
      (is (string= "HD" set-result) "mc-meta-set should return HD on success"))
    ;; Get with meta protocol
    (multiple-value-bind (response foundp) (mc-meta-get key)
      (is-true foundp "mc-meta-get should find the key")
      (is-true (hash-table-p response) "Response should be a hash table")
      (let ((value (gethash :value response)))
        (is (not (null value)) "Should have a value")
        (is (string= data (babel:octets-to-string value))
            "Retrieved data should match stored data")))
    (cleanup-test-key key)))

(test mc-meta-get-with-cas
  "Test mc-meta-get returns CAS value when requested"
  (setup-test-memcache)
  (let ((key "meta-cas-test"))
    (cleanup-test-key key)
    (mc-meta-set key "cas-test-data")
    (multiple-value-bind (response foundp) (mc-meta-get key :cas t)
      (is-true foundp "Should find the key")
      (is-true (not (null (gethash :cas response))) "Should have CAS value"))
    (cleanup-test-key key)))

(test mc-meta-set-with-cas-succeeds
  "Test mc-meta-set with correct CAS value succeeds"
  (setup-test-memcache)
  (let ((key "meta-cas-set-test"))
    (cleanup-test-key key)
    (mc-meta-set key "initial")
    (multiple-value-bind (response foundp) (mc-meta-get key :cas t)
      (declare (ignore foundp))
      (let ((cas (gethash :cas response)))
        (is (not (null cas)) "Should have CAS")
        (let ((result (mc-meta-set key "updated" :cas cas)))
          (is (string= "HD" result) "CAS set with correct value should succeed"))))
    (cleanup-test-key key)))

(test mc-meta-set-with-wrong-cas-fails
  "Test mc-meta-set with wrong CAS value returns EX"
  (setup-test-memcache)
  (let ((key "meta-cas-fail-test"))
    (cleanup-test-key key)
    (mc-meta-set key "initial")
    (multiple-value-bind (response foundp) (mc-meta-get key :cas t)
      (declare (ignore foundp))
      (let ((cas (gethash :cas response)))
        ;; Modify the value to invalidate CAS
        (mc-meta-set key "middle")
        ;; Try to set with old CAS
        (let ((result (mc-meta-set key "final" :cas cas)))
          (is (string= "EX" result) "CAS set with stale value should return EX"))))
    (cleanup-test-key key)))

(test mc-meta-delete-basic
  "Test mc-meta-delete removes a key"
  (setup-test-memcache)
  (let ((key "meta-delete-test"))
    (cleanup-test-key key)
    (mc-meta-set key "to-be-deleted")
    ;; Verify it exists
    (multiple-value-bind (response foundp) (mc-meta-get key)
      (declare (ignore response))
      (is-true foundp "Key should exist before delete"))
    ;; Delete it
    (let ((result (mc-meta-delete key)))
      (is (string= "HD" result) "Delete should return HD"))
    ;; Verify it's gone
    (multiple-value-bind (response foundp) (mc-meta-get key)
      (declare (ignore response))
      (is-false foundp "Key should not exist after delete"))))

(test mc-meta-delete-nonexistent
  "Test mc-meta-delete on nonexistent key returns NF or EN"
  (setup-test-memcache)
  (let ((key "meta-delete-nonexistent-test"))
    (cleanup-test-key key)
    (let ((result (mc-meta-delete key)))
      ;; Should return "NF" (Not Found) or similar
      (is (member result '("NF" "EN") :test #'string=)
          "Delete of nonexistent key should return NF or EN"))))

(test mc-meta-noop-works
  "Test mc-meta-noop returns MN"
  (setup-test-memcache)
  (let ((result (mc-meta-noop)))
    (is (string= "MN" result) "Noop should return MN")))

(test mc-meta-get-nonexistent-key
  "Test mc-meta-get on nonexistent key returns EN and foundp=nil"
  (setup-test-memcache)
  (let ((key "meta-nonexistent-key-12345"))
    (cleanup-test-key key)
    (multiple-value-bind (response foundp) (mc-meta-get key)
      (is-false foundp "foundp should be nil for nonexistent key")
      (is (string= "EN" response) "Response should be EN"))))

(test mc-with-connection-pipelining
  "Test mc-with-connection allows pipelining"
  (setup-test-memcache)
  (let ((key1 "pipe-test-1")
        (key2 "pipe-test-2"))
    (cleanup-test-key key1)
    (cleanup-test-key key2)
    (mc-with-connection (s :memcache *test-memcache*)
      ;; Send quiet sets
      (mc-meta-set key1 "data1" :stream s :quiet t)
      (mc-meta-set key2 "data2" :stream s :quiet t)
      ;; Sync with noop - mc-meta-noop reads and returns the response
      (let ((result (mc-meta-noop :stream s)))
        (is (string= "MN" result) "Should get MN response")))
    ;; Verify data was stored
    (multiple-value-bind (r1 f1) (mc-meta-get key1)
      (is-true f1 "Key1 should exist")
      (is (string= "data1" (babel:octets-to-string (gethash :value r1)))))
    (multiple-value-bind (r2 f2) (mc-meta-get key2)
      (is-true f2 "Key2 should exist")
      (is (string= "data2" (babel:octets-to-string (gethash :value r2)))))
    (cleanup-test-key key1)
    (cleanup-test-key key2)))

(test mc-meta-get-with-opaque
  "Test mc-meta-get with opaque token"
  (setup-test-memcache)
  (let ((key "meta-opaque-test"))
    (cleanup-test-key key)
    (mc-meta-set key "opaque-data")
    (multiple-value-bind (response foundp) (mc-meta-get key :opaque "mytoken123")
      (is-true foundp "Should find the key")
      (is (string= "mytoken123" (gethash :opaque response))
          "Opaque token should be returned"))
    (cleanup-test-key key)))

(test mc-meta-set-with-ttl
  "Test mc-meta-set with TTL"
  (setup-test-memcache)
  (let ((key "meta-ttl-test"))
    (cleanup-test-key key)
    ;; Set with 60 second TTL
    (let ((result (mc-meta-set key "ttl-data" :ttl 60)))
      (is (string= "HD" result) "Set with TTL should succeed"))
    ;; Verify it exists
    (multiple-value-bind (response foundp) (mc-meta-get key)
      (declare (ignore response))
      (is-true foundp "Key with TTL should exist"))
    (cleanup-test-key key)))

;;;
;;; Benchmark Tests - Pool vs No-Pool Performance
;;;

(defun benchmark-pool-performance (&key (iterations 100) (port 11298))
  "Compare performance of pool vs no-pool connections over ITERATIONS.
Returns timing results for both modes."
  (let* ((memcache-pool (make-memcache :ip "127.0.0.1" :port port :pool-size 5))
         (memcache-no-pool (make-memcache :ip "127.0.0.1" :port port :pool-size 1))
         (test-key "benchmark-test-key")
         (test-data "benchmark-test-data-payload-for-timing")
         (pool-times nil)
         (no-pool-times nil))

    (format t "~%=== Pool vs No-Pool Benchmark ===~%")
    (format t "Iterations: ~A~%~%" iterations)

    ;; Warm up
    (mc-set test-key test-data :memcache memcache-pool :mc-use-pool t)
    (mc-set test-key test-data :memcache memcache-no-pool :mc-use-pool nil)

    ;; Benchmark WITH pool
    (format t "Running WITH pool (~A iterations)...~%" iterations)
    (let ((start-time (get-internal-real-time)))
      (dotimes (i iterations)
        (mc-set test-key test-data :memcache memcache-pool :mc-use-pool t)
        (mc-get (list test-key) :memcache memcache-pool :mc-use-pool t))
      (let ((elapsed (/ (- (get-internal-real-time) start-time)
                        internal-time-units-per-second)))
        (setf pool-times elapsed)
        (format t "  Pool time: ~,4F seconds (~,4F ms/op)~%"
                elapsed (* 1000 (/ elapsed (* 2 iterations))))))

    ;; Benchmark WITHOUT pool
    (format t "Running WITHOUT pool (~A iterations)...~%" iterations)
    (let ((start-time (get-internal-real-time)))
      (dotimes (i iterations)
        (mc-set test-key test-data :memcache memcache-no-pool :mc-use-pool nil)
        (mc-get (list test-key) :memcache memcache-no-pool :mc-use-pool nil))
      (let ((elapsed (/ (- (get-internal-real-time) start-time)
                        internal-time-units-per-second)))
        (setf no-pool-times elapsed)
        (format t "  No-pool time: ~,4F seconds (~,4F ms/op)~%"
                elapsed (* 1000 (/ elapsed (* 2 iterations))))))

    ;; Summary
    (format t "~%=== Summary ===~%")
    (format t "Pool:    ~,4F seconds total~%" pool-times)
    (format t "No-Pool: ~,4F seconds total~%" no-pool-times)
    (format t "Speedup: ~,2Fx faster with pool~%" (/ no-pool-times pool-times))

    ;; Cleanup
    (mc-del test-key :memcache memcache-pool)

    (values pool-times no-pool-times)))


;;;
;;; Distributed Pool Tests
;;;

(defvar *distributed-memcache* nil
  "Distributed memcache pool for testing")

(defun make-distributed-memcache (&key (ports '(11298 11299)) (pool-size 5))
  "Create a distributed memcache pool across multiple ports.
Returns both the distributed pool and a mapping of node-name -> port."
  (let ((node-port-map (make-hash-table :test 'equal)))
    (values
     (pooler:make-distributed-pool
      :name "Distributed-Memcache"
      :replicas 100  ; More replicas for better distribution
      :node-configs
      (loop for port in ports
            for i from 0
            collect (let ((node-name (format nil "mc-node-~A" port)))
                      (setf (gethash node-name node-port-map) port)
                      (list :name node-name
                            :capacity pool-size
                            :item-maker (let ((p port))
                                         (lambda ()
                                           (usocket:socket-connect
                                            "127.0.0.1" p
                                            :element-type '(unsigned-byte 8))))
                            :item-destroyer (lambda (conn)
                                             (ignore-errors (usocket:socket-close conn)))))))
     node-port-map)))

(defun distributed-mc-set (dist-pool key data &key (timeout 0) (flags 0))
  "Set a value using the distributed pool. Returns which node was used."
  (let* ((node (pooler:get-node-for-key dist-pool key))
         (node-name (pooler::pool-name node)))
    (pooler:with-pool (conn node)
      (let ((s (usocket:socket-stream conn))
            (data-bytes (babel:string-to-octets data)))
        (write-sequence
         (babel:string-to-octets
          (format nil "set ~A ~A ~A ~A~C~C"
                  key flags timeout (length data-bytes)
                  #\Return #\Newline))
         s)
        (force-output s)
        (write-sequence data-bytes s)
        (write-sequence (babel:string-to-octets (format nil "~C~C" #\Return #\Newline)) s)
        (force-output s)
        (let ((response (cl-memcached::read-line-from-binary-stream s)))
          (values response node-name))))))

(defun distributed-mc-get (dist-pool key)
  "Get a value using the distributed pool. Returns value and which node was used."
  (let* ((node (pooler:get-node-for-key dist-pool key))
         (node-name (pooler::pool-name node)))
    (pooler:with-pool (conn node)
      (let ((s (usocket:socket-stream conn)))
        (write-sequence
         (babel:string-to-octets
          (format nil "get ~A~C~C" key #\Return #\Newline))
         s)
        (force-output s)
        (let ((response-line (cl-memcached::read-line-from-binary-stream s)))
          (if (string= response-line "END")
              (values nil node-name)
              (let* ((parts (split-sequence:split-sequence #\Space response-line))
                     (bytes (parse-integer (fourth parts)))
                     (data (make-array bytes :element-type '(unsigned-byte 8))))
                (read-sequence data s)
                (cl-memcached::read-line-from-binary-stream s)  ; consume trailing CRLF
                (cl-memcached::read-line-from-binary-stream s)  ; consume END
                (values (babel:octets-to-string data) node-name))))))))

(defun test-distributed-pool (&key (num-keys 20) (ports '(11298 11299)))
  "Test distributed pool and show which node handles each key."
  (format t "~%=== Distributed Pool Test ===~%")
  (format t "Nodes: ~{port ~A~^, ~}~%~%" ports)

  (multiple-value-bind (dist-pool node-port-map)
      (make-distributed-memcache :ports ports)

    (let ((node-counts (make-hash-table :test 'equal))
          (results nil))

      ;; Initialize counts
      (maphash (lambda (name port)
                 (declare (ignore port))
                 (setf (gethash name node-counts) 0))
               node-port-map)

      ;; Test SET operations
      (format t "--- SET Operations ---~%")
      (format t "~20A ~15A ~10A~%" "Key" "Node" "Port")
      (format t "~20,,,'-A ~15,,,'-A ~10,,,'-A~%" "" "" "")

      (dotimes (i num-keys)
        (let* ((key (format nil "dist-test-key-~A" i))
               (data (format nil "value-~A" i)))
          (multiple-value-bind (response node-name)
              (distributed-mc-set dist-pool key data)
            (declare (ignore response))
            (let ((port (gethash node-name node-port-map)))
              (incf (gethash node-name node-counts))
              (push (list key node-name port) results)
              (format t "~20A ~15A ~10A~%" key node-name port)))))

      ;; Show distribution summary
      (format t "~%--- Distribution Summary ---~%")
      (maphash (lambda (name count)
                 (format t "~A: ~A keys (~,1F%)~%"
                         name count (* 100.0 (/ count num-keys))))
               node-counts)

      ;; Verify GET operations return from same nodes
      (format t "~%--- Verifying GET Operations ---~%")
      (let ((all-correct t))
        (dolist (result (reverse results))
          (destructuring-bind (key expected-node expected-port) result
            (declare (ignore expected-port))
            (multiple-value-bind (value actual-node)
                (distributed-mc-get dist-pool key)
              (declare (ignore value))
              (let ((correct (string= expected-node actual-node)))
                (unless correct (setf all-correct nil))
                (when (not correct)
                  (format t "MISMATCH: ~A - SET on ~A, GET on ~A~%"
                          key expected-node actual-node))))))
        (if all-correct
            (format t "All keys retrieved from correct nodes!~%")
            (format t "WARNING: Some keys retrieved from wrong nodes!~%")))

      ;; Cleanup
      (format t "~%--- Cleanup ---~%")
      (dolist (result results)
        (let ((key (first result)))
          (distributed-mc-set dist-pool key "" :timeout 1)))  ; expire immediately
      (format t "Test keys cleaned up.~%")

      dist-pool)))

;;;
;;; Test runner
;;;

(defun run-tests ()
  "Run all cl-memcached tests"
  (run! 'cl-memcached-suite))

(defun run-gets-tests ()
  "Run just the gets tests"
  (run! 'gets-suite))

(defun run-stats-tests ()
  "Run just the stats tests"
  (run! 'stats-suite))

(defun run-meta-tests ()
  "Run just the meta protocol tests"
  (run! 'meta-suite))

(defun run-benchmarks ()
  "Run pool benchmark test"
  (benchmark-pool-performance :iterations 100))

(defun run-distributed-test ()
  "Run distributed pool test"
  (test-distributed-pool :num-keys 20))
