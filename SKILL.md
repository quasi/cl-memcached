---
type: skill-template
version: 1.0.0
domain: caching
language: Common Lisp
library: cl-memcached
generated: 2026-01-21
---

# cl-memcached Skill Template

Skill for LLM agents implementing memcached caching in Common Lisp applications. Use this skill when working with cl-memcached for caching, sessions, counters, or distributed data.

## When to Invoke This Skill

Use this skill when the task involves:
- Storing/retrieving data from memcached
- Implementing caching layers in Lisp applications
- Using atomic counters or CAS operations
- Optimizing performance with connection pooling
- Handling distributed cache failures
- Implementing session storage
- Batch operations or pipelining

**Do NOT use this skill for**:
- Pure database work (use database-specific skills)
- Non-caching distributed systems
- Applications not using memcached

## Core Competencies

This skill covers:

1. **Connection Management**: Creating connections, pooling, lifecycle
2. **Storage Operations**: SET, ADD, REPLACE, APPEND, PREPEND, CAS
3. **Retrieval Operations**: GET, GETS (with CAS tokens)
4. **Atomic Operations**: INCR, DECR, atomic counters
5. **Key Management**: TTL/expiration, deletion, touching
6. **Error Handling**: Connection failures, type validation
7. **Performance**: Connection pooling, pipelining, batch operations
8. **Protocols**: TEXT protocol (classic) and META protocol (modern)

## Architecture Summary

### High-Level Design

```dot
digraph cl_memcached {
    rankdir=LR;

    Application [shape=box];
    PublicAPI [shape=box, label="Public API (mc-set, mc-get, ...)"];
    TextProtocol [shape=box, label="TEXT Protocol Handler"];
    MetaProtocol [shape=box, label="META Protocol Handler"];
    ConnectionPool [shape=box, label="Connection Pool"];
    SocketIO [shape=box, label="Binary Socket I/O"];
    Memcached [shape=ellipse, label="Memcached Server"];

    Application -> PublicAPI;
    PublicAPI -> TextProtocol;
    PublicAPI -> MetaProtocol;
    TextProtocol -> ConnectionPool;
    MetaProtocol -> ConnectionPool;
    ConnectionPool -> SocketIO;
    SocketIO -> Memcached;
}
```

### Component Responsibilities

| Component | Responsibility |
|-----------|---|
| **Public API** | Type conversion, global variable management, function entry points |
| **Protocol Handlers** | Wire format encoding/decoding, command construction, response parsing |
| **Connection Pool** | Connection reuse, lifecycle management, thread safety |
| **Socket I/O** | Binary stream handling, encoding, multi-implementation compatibility |

### Key Files

- **cl-memcached.lisp** (711 lines): All implementation
- **packages.lisp** (57 lines): Exports 44 public symbols
- **tests.lisp** (529 lines): 27 comprehensive tests

### Dependencies

- **usocket**: Socket I/O
- **babel**: Encoding/decoding
- **split-sequence**: String utilities
- **pooler**: Connection pooling

## Operation Reference

### Storage (Set/Add/Replace/CAS)

```lisp
;; Basic storage
(mc-set "key" "value" :timeout 3600)  ;; Returns "STORED"

;; Add only if missing
(mc-add "key" "value")  ;; "STORED" or "NOT_STORED"

;; Replace only if exists
(mc-replace "key" "new-value")  ;; "STORED" or "NOT_STORED"

;; Compare-and-set (atomic, safe for concurrency)
(mc-cas "key" "new-value" cas-token)  ;; "STORED", "EXISTS", or "NOT_FOUND"
```

### Retrieval

```lisp
;; Get multiple values (returns list or nil)
(mc-get+ (list "k1" "k2" "k3"))  ;; List of memcache-response objects

;; Get with CAS tokens (for safe updates)
(mc-gets+ (list "k1" "k2"))  ;; Same as mc-get+ but with CAS tokens

;; Convenience: single value as string
(mc-get-value "key")  ;; Returns string or nil
```

### Counters (Atomic)

```lisp
;; Initialize counter
(mc-set "counter" "0")

;; Increment
(mc-incr "counter")      ;; Returns 1
(mc-incr "counter" 10)   ;; Returns 11

;; Decrement (never negative, clamps at 0)
(mc-decr "counter" 5)    ;; Returns 6
```

### Deletion & Maintenance

```lisp
;; Delete key
(mc-del "key")  ;; "DELETED" or "NOT_FOUND"

;; Update expiration without modifying data
(mc-touch "key" 3600)  ;; "TOUCHED" or "NOT_FOUND"

;; Delete all keys (dangerous!)
(mc-flush-all)  ;; "OK"
```

### Statistics

```lisp
;; Server stats
(mc-stats)  ;; Returns alist of stats

;; Per-slab statistics
(mc-stats-items)  ;; Item count per slab
(mc-stats-slabs)  ;; Slab allocation stats
(mc-stats-sizes)  ;; Item size distribution
```

## Protocol Comparison

### TEXT Protocol (Classic)

```lisp
;; Simple, widely compatible
(cl-memcached:mc-set "key" "value")
(cl-memcached:mc-get+ (list "key"))
(cl-memcached:mc-del "key")
```

**Characteristics**:
- ASCII command format
- Single-response per operation
- Compatible with all memcached servers
- Simpler error codes

### META Protocol (Modern)

```lisp
;; Modern, supports advanced features
(cl-memcached:mc-meta-set "key" "value" :ttl 3600)
(cl-memcached:mc-meta-get "key")
(cl-memcached:mc-meta-delete "key")
```

**Characteristics**:
- More concise wire format
- Pipelining support
- Advanced flags (recache-on-miss, stale-data)
- Better for batch operations

**Choose META for**: New code, pipelining, advanced features
**Choose TEXT for**: Compatibility, existing code, simplicity

## Exemplar Operations

### Pattern A: Cache-Aside (Try Cache, Fall Back to Source)

```lisp
(defun get-with-cache (key source-fn)
  "Retrieve from cache or compute and cache."
  (handler-case
    ;; Try cache first
    (let ((response (first (cl-memcached:mc-get+ (list key)))))
      (when response
        (return-from get-with-cache
          (babel:octets-to-string (cl-memcached:mc-data response)))))

    ;; Cache unavailable, use source
    (cl-memcached:memcached-server-unreachable ()
      nil))  ;; Fall through to source

  ;; Compute from source
  (let ((value (funcall source-fn key)))
    ;; Try to cache (best effort)
    (handler-case
      (cl-memcached:mc-set key value :timeout 3600)
      (cl-memcached:memcached-server-unreachable ()
        nil))  ;; OK if cache fails

    value))
```

### Pattern B: Atomic Counter with Verification

```lisp
(defun increment-counter (key)
  "Safely increment counter, verifying initialization."
  ;; Initialize if needed
  (let ((exists (not (null (cl-memcached:mc-get+ (list key))))))
    (unless exists
      (cl-memcached:mc-set key "0")))

  ;; Increment atomically
  (let ((new-value (cl-memcached:mc-incr key 1)))
    (format t "Counter is now: ~D~%" new-value)
    new-value))
```

### Pattern C: CAS Update Loop

```lisp
(defun update-with-cas (key update-fn)
  "Update value safely using CAS loop."
  (loop
    (let* ((response (first (cl-memcached:mc-gets+ (list key))))
           (cas-token (when response (cl-memcached:mc-cas-unique response)))
           (old-value (when response
                       (babel:octets-to-string
                         (cl-memcached:mc-data response)))))

      (unless response
        ;; Key doesn't exist, try add
        (let ((new-value (funcall update-fn nil)))
          (when (string= (cl-memcached:mc-add key new-value) "STORED")
            (return new-value))))

      ;; Update with CAS
      (let* ((new-value (funcall update-fn old-value))
             (result (cl-memcached:mc-cas key new-value cas-token)))
        (case (read-from-string result)
          (STORED (return new-value))
          (EXISTS (format t "Conflict, retrying~%"))
          (NOT_FOUND (format t "Key deleted, retrying~%")))))))
```

### Pattern D: Batch Operations with Pipelining

```lisp
(defun batch-set (key-values)
  "Set multiple values efficiently via pipelining."
  (cl-memcached:mc-with-connection (s)
    ;; Send all operations (no responses)
    (dolist (kv key-values)
      (destructuring-bind (key value) kv
        (cl-memcached:mc-meta-set key value :stream s :quiet t)))

    ;; Flush pipeline
    (cl-memcached:mc-meta-noop :stream s)

    (format t "Set ~D key-value pairs efficiently~%" (length key-values))))
```

## Error Handling Patterns

### Handle Connection Failure

```lisp
(handler-case
  (cl-memcached:mc-set "key" "value")
  (cl-memcached:memcached-server-unreachable (e)
    (format *error-output* "Cache error: ~A~%" e)
    ;; Fallback logic here
    ))
```

### Handle Type Errors

```lisp
(handler-case
  (cl-memcached:mc-get+ "not-a-list")  ;; BUG: should be list
  (cl-memcached:cl-mc-error (e)
    (format *error-output* "Programming error: ~A~%" e)
    ;; Fix the bug, don't suppress
    ))
```

### Ignore Missing Keys (Normal)

```lisp
(let ((response (first (cl-memcached:mc-get+ (list "key")))))
  (if response
    ;; Key found
    (format t "Value: ~A~%"
      (babel:octets-to-string (cl-memcached:mc-data response)))
    ;; Key not found (not an error)
    (format t "Key not in cache~%")))
```

## Global Configuration

```lisp
;; Default connection
(setf cl-memcached:*memcache*
  (cl-memcached:make-memcache :host "cache.example.com" :port 11211))

;; Enable connection pooling (5-7x speedup)
(setf cl-memcached:*mc-use-pool* t)

;; Change default encoding (UTF-8 by default)
(setf cl-memcached:*mc-default-encoding* :latin1)
```

## Performance Tuning

### 1. Enable Connection Pooling

```lisp
(setf cl-memcached:*mc-use-pool* t)
;; Expected: 5-7x speedup
```

### 2. Use Pipelining for Batches

```lisp
;; Without pipelining: N round-trips
(dotimes (i 1000)
  (cl-memcached:mc-set (format nil "k~D" i) "v"))

;; With pipelining: O(1) round-trips
(cl-memcached:mc-with-connection (s)
  (dotimes (i 1000)
    (cl-memcached:mc-meta-set (format nil "k~D" i) "v"
                              :stream s :quiet t))
  (cl-memcached:mc-meta-noop :stream s))
```

### 3. Use Batch GET Operations

```lisp
;; Without batching: 3 requests
(mc-get+ (list "k1"))
(mc-get+ (list "k2"))
(mc-get+ (list "k3"))

;; With batching: 1 request
(mc-get+ (list "k1" "k2" "k3"))
```

### 4. Increase Pool Size for High Concurrency

```lisp
(setf mc (cl-memcached:make-memcache
           :host "cache.example.com"
           :pool-size 20))  ;; Increased from 5
```

## Type Constraints (CRITICAL)

| Type | Constraint | Example |
|------|---|---|
| Keys | String, 1-250 chars | `"user:123"` |
| Values | Octets (for mc-store) or string (for mc-set) | `#(72 101 108 108 111)` or `"hello"` |
| TTL | Integer seconds, 0 = never expire | `3600` |
| CAS token | String from previous gets+ | From `mc-cas-unique` |
| Keys-list | List, not string | `(list "k1" "k2")` not `"k1 k2"` |

## Debugging Checklist

When operations fail:

```
[ ] Is memcached server running? (echo "stats" | nc localhost 11211)
[ ] Are host/port correct? (matches mc-make-memcache args)
[ ] Are keys strings? (not symbols or numbers)
[ ] Are values strings or octets? (not structures)
[ ] Is keys-list a list? (not a string)
[ ] Did you decode the response? (use babel:octets-to-string)
[ ] Is TTL a number? (not a string)
[ ] Did you initialize counter before increment? (mc-set first)
[ ] Did you check CAS response code? (STORED, EXISTS, or NOT_FOUND)
[ ] Is pooling enabled? (setf *mc-use-pool* t)
```

## Conformance Requirements

For implementation:

- [ ] All string keys (RULE-001)
- [ ] All values stored as octets (RULE-002)
- [ ] GET operations accept lists (RULE-003)
- [ ] CAS only with `:cas` command (RULE-004)
- [ ] Counters initialized before use (RULE-005)
- [ ] TTL=0 means never expire (RULE-006)
- [ ] Connection cleanup guaranteed (RULE-007)
- [ ] Both protocols semantically equivalent (RULE-008)
- [ ] Missing keys return nil, not error (RULE-009)
- [ ] Counters atomic (RULE-010)

See CLAUDE.md for complete specification.

## Implementation Workflow

### Step 1: Setup

```lisp
(ql:quickload :cl-memcached)
(setf cl-memcached:*mc-use-pool* t)  ;; Enable pooling
```

### Step 2: Connect

```lisp
(defvar *cache*
  (cl-memcached:make-memcache :host "localhost" :port 11211))
```

### Step 3: Store

```lisp
(cl-memcached:mc-set "key" "value" :timeout 3600 :memcache *cache*)
```

### Step 4: Retrieve

```lisp
(let ((response (first (cl-memcached:mc-get+ (list "key") :memcache *cache*))))
  (when response
    (format t "~A~%" (babel:octets-to-string (cl-memcached:mc-data response)))))
```

### Step 5: Handle Errors

```lisp
(handler-case
  (cl-memcached:mc-set "key" "value")
  (cl-memcached:memcached-server-unreachable ()
    ;; Fallback or retry
    ))
```

## Reference Documents

- **CLAUDE.md**: Complete implementation specification
- **canon/canon.yaml**: Full architecture and decisions
- **docs/**: Human-readable guides and tutorials
- **canon/features/*/contracts/**: Detailed API contracts
- **canon/features/*/scenarios/**: Usage examples and patterns

## Verification

Run tests:

```lisp
(asdf:test-system :cl-memcached)
```

Check invariants:

```lisp
;; Data round-trip
(let ((original "test"))
  (mc-set "k" original)
  (assert (string= original
    (babel:octets-to-string (mc-data (first (mc-get+ (list "k"))))))))

;; Counter atomicity
(mc-set "c" "0")
(loop repeat 100 do (mc-incr "c"))
(assert (= 100 (parse-integer (mc-get-value "c"))))
```

---

**Skill Version**: 1.0.0
**Library Version**: 1.0.0
**Confidence**: 0.94
**Last Updated**: 2026-01-21
