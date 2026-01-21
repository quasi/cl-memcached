# How-To: Use Pipelining for Batch Operations

<!-- Generated from: canon/features/meta-protocol/scenarios/pipelining.md -->

Reduce network round-trips from O(N) to O(1) for batch operations using meta protocol pipelining.

## Problem

You need to perform many memcached operations (100+ sets or gets), but each operation requires a network round-trip. This creates significant latency:

- 100 operations × 1ms RTT = 100ms total latency
- High latency impacts application performance

## Solution

Use meta protocol pipelining to batch operations:

- Send all 100 operations without waiting for responses
- Read all responses at once
- 100 operations with 2 round-trips = ~2ms total latency

**Speedup:** 50x reduction in latency for 100 operations.

## Prerequisites

- Memcached server with meta protocol support (1.4+)
- cl-memcached loaded
- Understanding of basic operations ([see Tutorial](../tutorials/01-basics.md))

## Quick Example

```lisp
(cl-memcached:mc-with-connection (s)
  ;; Send 100 sets without waiting for responses
  (dotimes (i 100)
    (cl-memcached:mc-meta-set
      (format nil "key~A" i)
      (format nil "value~A" i)
      :stream s
      :quiet t))

  ;; Flush pipeline and synchronize
  (cl-memcached:mc-meta-noop :stream s))

;; All 100 keys now stored with only 2 round-trips!
```

## How It Works

### Without Pipelining (Classic Approach)

```
Client                  Server
  |                        |
  |--- SET key1 ---------> |
  | <---- STORED ----------|
  |--- SET key2 ---------> |
  | <---- STORED ----------|
  |--- SET key3 ---------> |
  | <---- STORED ----------|

Total round-trips: N (3 in this example)
```

### With Pipelining (Meta Protocol)

```
Client                  Server
  |                        |
  |--- SET key1 (quiet)--> |
  |--- SET key2 (quiet)--> |
  |--- SET key3 (quiet)--> |
  |--- NOOP -------------> |
  | <---- MN (all done) ---|

Total round-trips: 2 (send batch + receive noop)
```

## Step-by-Step Guide

### Step 1: Use mc-with-connection Macro

The macro manages the connection lifecycle:

```lisp
(cl-memcached:mc-with-connection (s :memcache *cache*)
  ;; s is the stream - pass it to pipelined operations
  ...)
```

**Why:** Ensures proper connection cleanup even on errors.

### Step 2: Add :stream Parameter

Pass the stream to meta protocol operations:

```lisp
(cl-memcached:mc-meta-set "key" "value" :stream s :quiet t)
```

**Parameters:**
- `:stream s` - Use this connection (enables pipelining)
- `:quiet t` - Don't send individual responses

### Step 3: Flush Pipeline with Noop

Signal completion and synchronize:

```lisp
(let ((result (cl-memcached:mc-meta-noop :stream s)))
  (assert (string= "MN" result)))
```

**Why:** Acts as a synchronization barrier - confirms all previous operations completed.

## Complete Examples

### Example 1: Batch Writes

Write 1000 key-value pairs efficiently:

```lisp
(defun batch-write-keys (key-value-alist)
  "Write multiple key-value pairs with pipelining."
  (cl-memcached:mc-with-connection (s)
    ;; Send all sets
    (loop for (key . value) in key-value-alist
          do (cl-memcached:mc-meta-set key value
                                       :stream s
                                       :quiet t))

    ;; Flush and verify
    (let ((result (cl-memcached:mc-meta-noop :stream s)))
      (format t "Batch complete: ~A~%" result))))

;; Usage
(batch-write-keys
  '(("user:1" . "Alice")
    ("user:2" . "Bob")
    ("user:3" . "Charlie")))
```

**Performance:**
- Without pipelining: 3 round-trips (3ms @ 1ms RTT)
- With pipelining: 2 round-trips (2ms @ 1ms RTT)

### Example 2: Batch Reads with Opaque Tokens

Read multiple keys and correlate responses:

```lisp
(defun batch-read-keys (keys)
  "Read multiple keys with pipelining and opaque tokens."
  (cl-memcached:mc-with-connection (s)
    ;; Send all gets with opaque tokens
    (loop for key in keys
          do (cl-memcached:mc-meta-get key
                                       :stream s
                                       :quiet t
                                       :opaque key))

    ;; Flush pipeline
    (cl-memcached:mc-meta-noop :stream s)

    ;; Read responses
    (loop repeat (length keys)
          collect (cl-memcached:mc-read-meta-response s))))

;; Usage
(batch-read-keys '("user:1" "user:2" "user:3"))
```

**What are opaque tokens?**
- Arbitrary strings you provide with each request
- Server echoes them back in responses
- Use for request/response correlation

### Example 3: Mixed Operations

Combine sets, gets, and deletes in one pipeline:

```lisp
(defun pipeline-mixed-operations ()
  "Demonstrate mixed operation pipelining."
  (cl-memcached:mc-with-connection (s)
    ;; Set new values
    (cl-memcached:mc-meta-set "key1" "new-value" :stream s :quiet t)
    (cl-memcached:mc-meta-set "key2" "another-value" :stream s :quiet t)

    ;; Delete old value
    (cl-memcached:mc-meta-delete "old-key" :stream s :quiet t)

    ;; Get existing value (with opaque token for correlation)
    (cl-memcached:mc-meta-get "key3" :stream s :quiet t :opaque "get-key3")

    ;; Flush pipeline
    (cl-memcached:mc-meta-noop :stream s)

    ;; Read the GET response
    (let ((response (cl-memcached:mc-read-meta-response s)))
      (format t "key3 value: ~A~%"
        (babel:octets-to-string (gethash :value response))))))
```

### Example 4: Batch Set with TTL

Set many keys with expiration times:

```lisp
(defun cache-user-sessions (sessions)
  "Cache multiple sessions with 30-minute TTL."
  (cl-memcached:mc-with-connection (s)
    (loop for session in sessions
          for key = (format nil "session:~A" (session-id session))
          do (cl-memcached:mc-meta-set key
                                       (session-data session)
                                       :stream s
                                       :quiet t
                                       :ttl 1800))  ; 30 minutes

    (let ((result (cl-memcached:mc-meta-noop :stream s)))
      (format t "Cached ~A sessions: ~A~%"
              (length sessions) result))))
```

## When to Use Pipelining

**✓ Use pipelining when:**
- Batch operations (10+ operations)
- High-latency networks
- Tight performance requirements
- Initializing cache with many keys
- Bulk updates

**✗ Don't use pipelining when:**
- Single operations
- Need immediate response for each operation
- Operations depend on previous results
- Low-latency local networks (overhead > benefit)

## Performance Characteristics

### Latency Comparison

| Operations | Without Pipelining | With Pipelining | Speedup |
|------------|-------------------|-----------------|---------|
| 10 | 10ms | 2ms | 5x |
| 100 | 100ms | 2ms | 50x |
| 1000 | 1000ms | 2ms | 500x |

*Assumes 1ms round-trip time*

### Throughput Comparison

```lisp
;; Benchmark: 10,000 operations
(time
  (dotimes (i 10000)
    (mc-meta-set (format nil "k~A" i) "v")))
;; Without pipelining: ~10 seconds

(time
  (mc-with-connection (s)
    (dotimes (i 10000)
      (mc-meta-set (format nil "k~A" i) "v" :stream s :quiet t))
    (mc-meta-noop :stream s)))
;; With pipelining: ~0.02 seconds (500x faster!)
```

## Common Mistakes

### Mistake 1: Forgetting :stream parameter

```lisp
;; Wrong - no pipelining
(mc-with-connection (s)
  (mc-meta-set "key" "value" :quiet t))  ; Missing :stream s

;; Right
(mc-with-connection (s)
  (mc-meta-set "key" "value" :stream s :quiet t))
```

### Mistake 2: Not flushing with noop

```lisp
;; Wrong - operations may not complete
(mc-with-connection (s)
  (mc-meta-set "key" "value" :stream s :quiet t))
;; Connection closes before operation completes!

;; Right
(mc-with-connection (s)
  (mc-meta-set "key" "value" :stream s :quiet t)
  (mc-meta-noop :stream s))  ; Wait for completion
```

### Mistake 3: Using text protocol instead of meta protocol

```lisp
;; Wrong - text protocol doesn't support pipelining
(mc-with-connection (s)
  (mc-set "key" "value" :stream s))  ; mc-set, not mc-meta-set

;; Right - use meta protocol functions
(mc-with-connection (s)
  (mc-meta-set "key" "value" :stream s :quiet t))
```

## Troubleshooting

**Problem: Operations seem to fail silently**

Cause: Forgot to flush pipeline with noop.

Solution:
```lisp
(mc-with-connection (s)
  (mc-meta-set "key" "value" :stream s :quiet t)
  (mc-meta-noop :stream s))  ; Add this!
```

**Problem: Getting errors about missing :stream**

Cause: Using text protocol functions instead of meta protocol.

Solution: Use `mc-meta-set`, `mc-meta-get`, `mc-meta-delete` (not `mc-set`, `mc-get`, etc.)

**Problem: Can't correlate responses with requests**

Cause: Not using opaque tokens.

Solution:
```lisp
(mc-meta-get "key" :stream s :quiet t :opaque "my-request-id")
```

## Advanced: Reading Individual Responses

If you need responses during pipelining (not just noop):

```lisp
(mc-with-connection (s)
  ;; Send gets (non-quiet to get responses)
  (mc-meta-get "key1" :stream s :opaque "r1")
  (mc-meta-get "key2" :stream s :opaque "r2")
  (mc-meta-get "key3" :stream s :opaque "r3")

  ;; Read responses in order
  (let ((r1 (mc-read-meta-response s))
        (r2 (mc-read-meta-response s))
        (r3 (mc-read-meta-response s)))

    (list (gethash :opaque r1)  ; => "r1"
          (gethash :opaque r2)  ; => "r2"
          (gethash :opaque r3)))) ; => "r3"
```

## Verification

Test your pipelining implementation:

```lisp
(defun test-pipelining ()
  "Verify pipelining works correctly."
  ;; Write with pipelining
  (mc-with-connection (s)
    (dotimes (i 10)
      (mc-meta-set (format nil "test-pipe-~A" i)
                   (format nil "value-~A" i)
                   :stream s
                   :quiet t))
    (mc-meta-noop :stream s))

  ;; Read back and verify
  (dotimes (i 10)
    (let ((value (mc-get-value (format nil "test-pipe-~A" i))))
      (assert (string= (format nil "value-~A" i) value))))

  (format t "Pipelining test passed!~%"))

(test-pipelining)
```

## Recap

You've learned:

✓ How pipelining reduces round-trips from O(N) to O(1)
✓ Using `mc-with-connection` for pipeline management
✓ Passing `:stream` and `:quiet t` to operations
✓ Flushing pipelines with `mc-meta-noop`
✓ Using opaque tokens for request/response correlation
✓ When to use pipelining (and when not to)

## What's Next

- [Performance Tuning](../performance.md) - Further optimization
- [Connection Pooling](connection-pooling.md) - Combine with pooling for maximum speed
- [API Reference](../reference/api-reference.md) - Complete meta protocol API

---

**See also:** [Meta Protocol Scenarios](../../canon/features/meta-protocol/scenarios/pipelining.md) (Canon specification)
