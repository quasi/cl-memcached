# How-To: Use Atomic Counters

<!-- Generated from: canon/features/counters/contracts/counter-operations.md -->

Use memcached's atomic counter operations for rate limiting, statistics tracking, and distributed counting.

## Problem

You need to track counts across multiple application instances:

- Page view counters
- API rate limiting
- Distributed locks
- Session counters

Regular read-modify-write has race conditions in concurrent environments.

## Solution

Use `mc-incr` and `mc-decr` for atomic, server-side counter operations. No race conditions, even with concurrent access.

## Prerequisites

- Memcached server running
- cl-memcached loaded
- Understanding of basic operations ([see Tutorial](../tutorials/01-basics.md))

## Quick Example

```lisp
;; Initialize counter
(cl-memcached:mc-set "page-views" "0")

;; Increment atomically (safe for concurrent use)
(cl-memcached:mc-incr "page-views")
;; => 1

(cl-memcached:mc-incr "page-views")
;; => 2

(cl-memcached:mc-incr "page-views" 10)
;; => 12
```

## How It Works

### Why Atomic Operations Matter

**Without atomic operations (RACE CONDITION):**

```
Thread 1: Read counter (100)
Thread 2: Read counter (100)
Thread 1: Increment to 101
Thread 2: Increment to 101  <- Lost update!
Thread 1: Write 101
Thread 2: Write 101
```

**Result:** Counter should be 102, but it's 101. One increment was lost.

**With atomic operations (SAFE):**

```
Thread 1: INCR counter
Server: Atomically increment 100 → 101, return 101
Thread 2: INCR counter
Server: Atomically increment 101 → 102, return 102
```

**Result:** Counter is correctly 102. All increments counted.

## Step-by-Step Guide

### Step 1: Initialize the Counter

Counters MUST be initialized before use:

```lisp
(cl-memcached:mc-set "my-counter" "0")
```

**Why:** `mc-incr` and `mc-decr` return `NOT_FOUND` if key doesn't exist.

### Step 2: Increment the Counter

```lisp
(cl-memcached:mc-incr "my-counter")    ; Increment by 1
;; => 1

(cl-memcached:mc-incr "my-counter" 5)  ; Increment by 5
;; => 6
```

**Return value:** New counter value after increment.

### Step 3: Decrement the Counter

```lisp
(cl-memcached:mc-decr "my-counter")    ; Decrement by 1
;; => 5

(cl-memcached:mc-decr "my-counter" 3)  ; Decrement by 3
;; => 2
```

**Important:** Counters never go negative. Decrementing below 0 clamps at 0.

```lisp
(cl-memcached:mc-set "counter" "5")
(cl-memcached:mc-decr "counter" 10)
;; => 0 (not -5!)
```

## Complete Examples

### Example 1: Page View Counter

Track page views for a website:

```lisp
(defun track-page-view (page-id)
  "Increment page view counter atomically."
  (let ((counter-key (format nil "page:~A:views" page-id)))

    ;; Initialize if doesn't exist
    (unless (cl-memcached:mc-get+ (list counter-key))
      (cl-memcached:mc-set counter-key "0"))

    ;; Increment atomically
    (let ((new-count (cl-memcached:mc-incr counter-key)))
      (format t "Page ~A has ~A views~%" page-id new-count)
      new-count)))

;; Usage
(track-page-view "homepage")
;; => Page homepage has 1 views

(track-page-view "homepage")
;; => Page homepage has 2 views
```

**Safe for concurrent use:** Multiple web servers can call this simultaneously.

### Example 2: API Rate Limiting

Limit API requests per user:

```lisp
(defun check-rate-limit (user-id max-requests-per-minute)
  "Check if user has exceeded rate limit."
  (let* ((minute (floor (get-universal-time) 60))
         (counter-key (format nil "ratelimit:~A:~A" user-id minute)))

    ;; Initialize counter for this minute if needed
    (unless (cl-memcached:mc-get+ (list counter-key))
      (cl-memcached:mc-set counter-key "0" :timeout 120))  ; Expire after 2 minutes

    ;; Increment request count
    (let ((request-count (cl-memcached:mc-incr counter-key)))

      ;; Check limit
      (if (> request-count max-requests-per-minute)
          (progn
            (format t "Rate limit exceeded for user ~A~%" user-id)
            nil)
          (progn
            (format t "Request ~A/~A for user ~A~%"
                    request-count max-requests-per-minute user-id)
            t)))))

;; Usage
(check-rate-limit "user123" 10)
;; => Request 1/10 for user123
;;    T

;; After 11 requests in same minute:
(check-rate-limit "user123" 10)
;; => Rate limit exceeded for user123
;;    NIL
```

### Example 3: Distributed Lock with Counters

Simple distributed lock implementation:

```lisp
(defun acquire-lock (resource-id timeout-seconds)
  "Acquire lock using counter. Returns T if acquired, NIL if already locked."
  (let ((lock-key (format nil "lock:~A" resource-id)))

    (handler-case
      ;; Try to create lock (fails if exists)
      (progn
        (cl-memcached:mc-add lock-key "1" :timeout timeout-seconds)
        (format t "Lock acquired for ~A~%" resource-id)
        t)

      ;; Lock already exists
      (error ()
        (format t "Lock already held for ~A~%" resource-id)
        nil))))

(defun release-lock (resource-id)
  "Release lock."
  (let ((lock-key (format nil "lock:~A" resource-id)))
    (cl-memcached:mc-del lock-key)
    (format t "Lock released for ~A~%" resource-id)))

;; Usage
(acquire-lock "database-backup" 300)  ; 5 minute lock
;; => Lock acquired for database-backup
;;    T

;; Another process tries to acquire same lock
(acquire-lock "database-backup" 300)
;; => Lock already held for database-backup
;;    NIL
```

### Example 4: Session Counter

Track active sessions:

```lisp
(defun session-created ()
  "Increment active session counter."
  (unless (cl-memcached:mc-get+ (list "sessions:active"))
    (cl-memcached:mc-set "sessions:active" "0"))

  (let ((active (cl-memcached:mc-incr "sessions:active")))
    (format t "Active sessions: ~A~%" active)
    active))

(defun session-destroyed ()
  "Decrement active session counter."
  (let ((active (cl-memcached:mc-decr "sessions:active")))
    (format t "Active sessions: ~A~%" active)
    active))

;; Usage
(session-created)   ;; => Active sessions: 1
(session-created)   ;; => Active sessions: 2
(session-destroyed) ;; => Active sessions: 1
```

## When to Use Counters

**✓ Use counters for:**
- Page view tracking
- API rate limiting
- Active session counts
- Distributed statistics
- Event counting
- Simple distributed locks

**✗ Don't use counters for:**
- Complex state management (use CAS instead)
- Large numbers (>2^64-1)
- Decimal/float values (integers only)
- Values that need to go negative

## Performance Characteristics

**Atomicity:** Server-side, no race conditions
**Speed:** Very fast (single operation, no read-modify-write cycle)
**Scalability:** Safe across multiple application instances

**Benchmark:**

```lisp
(time
  (dotimes (i 10000)
    (mc-incr "benchmark-counter")))
;; With connection pooling: ~200ms
;; Without pooling: ~1400ms
```

**Recommendation:** Enable connection pooling for high-throughput counters.

## Common Mistakes

### Mistake 1: Not initializing counter

```lisp
;; Wrong - key doesn't exist
(mc-incr "uninitialized-counter")
;; => NOT_FOUND error

;; Right - initialize first
(mc-set "uninitialized-counter" "0")
(mc-incr "uninitialized-counter")
;; => 1
```

### Mistake 2: Expecting negative values

```lisp
;; Wrong expectation
(mc-set "counter" "5")
(mc-decr "counter" 10)
;; => 0 (not -5!)
```

Counters clamp at 0. Use signed integers stored as strings if you need negative values.

### Mistake 3: Using non-integer initial values

```lisp
;; Wrong - counter must be integer
(mc-set "counter" "12.5")
(mc-incr "counter")
;; => Error or undefined behavior

;; Right
(mc-set "counter" "12")
(mc-incr "counter")
;; => 13
```

### Mistake 4: Concurrent initialization race

```lisp
;; Potential race condition
(defun increment-counter (key)
  (unless (mc-get+ (list key))
    (mc-set key "0"))
  (mc-incr key))

;; Problem: Two threads might both initialize the counter
```

**Better approach:**

```lisp
(defun increment-counter-safe (key)
  "Initialize with ADD (fails if exists), then increment."
  (handler-case
    (cl-memcached:mc-add key "0")
    (error () nil))  ; OK if key already exists

  (cl-memcached:mc-incr key))
```

## Troubleshooting

**Problem: Getting NOT_FOUND errors**

Cause: Counter not initialized.

Solution:
```lisp
(mc-set "counter" "0")
```

**Problem: Counter goes to 0 unexpectedly**

Cause: Decrementing below 0 (clamps at 0).

Solution: Track negative separately or use CAS with signed integers.

**Problem: Lost increments under high concurrency**

Cause: Not using atomic operations.

Solution: Use `mc-incr`/`mc-decr`, not read-modify-write.

## Advanced: Counter with Expiration

Auto-reset counters using TTL:

```lisp
(defun hourly-counter (event-name)
  "Counter that resets every hour."
  (let* ((hour (floor (get-universal-time) 3600))
         (key (format nil "hourly:~A:~A" event-name hour)))

    ;; Initialize with 1 hour TTL
    (handler-case
      (cl-memcached:mc-add key "0" :timeout 3600)
      (error () nil))

    ;; Increment
    (cl-memcached:mc-incr key)))

;; Usage
(hourly-counter "api-calls")
;; Automatically resets each hour
```

## Verification

Test counter operations:

```lisp
(defun test-counters ()
  "Verify counter operations work correctly."
  ;; Initialize
  (mc-set "test-counter" "0")

  ;; Test increment
  (assert (= 1 (mc-incr "test-counter")))
  (assert (= 2 (mc-incr "test-counter")))
  (assert (= 7 (mc-incr "test-counter" 5)))

  ;; Test decrement
  (assert (= 6 (mc-decr "test-counter")))
  (assert (= 3 (mc-decr "test-counter" 3)))

  ;; Test non-negative clamping
  (assert (= 0 (mc-decr "test-counter" 100)))

  ;; Cleanup
  (mc-del "test-counter")

  (format t "Counter tests passed!~%"))

(test-counters)
```

## Recap

You've learned:

✓ Why atomic operations prevent race conditions
✓ How to initialize, increment, and decrement counters
✓ Counters never go negative (clamp at 0)
✓ Real-world use cases (rate limiting, page views, sessions)
✓ Performance characteristics and best practices

## What's Next

- [CAS Operations](cas-operations.md) - Complex concurrent updates
- [Pipelining](pipelining.md) - Batch counter operations
- [Performance Tuning](../performance.md) - Optimize counter performance

---

**See also:** [Counter Contracts](../../canon/features/counters/contracts/counter-operations.md) (Canon specification)
