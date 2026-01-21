# How-To: Handle Errors Gracefully

<!-- Generated from: CLAUDE.md Error Conditions -->

Build resilient applications that handle memcached failures gracefully without impacting users.

## Problem

Memcached can fail for many reasons:

- Server crashes or restarts
- Network issues
- Configuration problems
- Invalid inputs

Applications must continue working even when cache fails.

## Solution

Use proper error handling patterns:

1. Catch memcached-specific exceptions
2. Provide fallback mechanisms
3. Log errors for debugging
4. Fail gracefully (degrade, don't crash)

**Principle:** Cache failures should never crash your application.

## Prerequisites

- Understanding of Common Lisp condition system
- Basic cl-memcached knowledge ([see Tutorial](../tutorials/01-basics.md))

## Error Types

cl-memcached signals two main error conditions:

### 1. memcached-server-unreachable

**When:** Connection to memcached fails or times out.

**Causes:**
- Memcached server not running
- Network issues
- Wrong host/port
- Firewall blocking connection

### 2. cl-mc-error

**When:** Invalid input types or protocol violations.

**Causes:**
- Non-string key
- Non-list keys-list in GET
- CAS token with non-CAS command
- Invalid timeout value

## Basic Error Handling

### Pattern 1: Fail Gracefully with Fallback

Most common pattern - fall back to source if cache fails:

```lisp
(defun get-user-data (user-id)
  "Get user data with cache fallback."
  (let ((cache-key (format nil "user:~A" user-id)))

    (handler-case
      ;; Try cache first
      (let ((cached (cl-memcached:mc-get-value cache-key)))
        (when cached
          (return-from get-user-data cached)))

      ;; Catch cache errors
      (cl-memcached:memcached-server-unreachable (e)
        (format *error-output* "Cache unavailable: ~A~%" e)))

    ;; Fall back to database (cache miss or error)
    (fetch-from-database user-id)))
```

**What this does:**
- Try cache first
- If cache fails, log error but continue
- Fall back to source (database, API, etc.)
- User never sees cache failures

### Pattern 2: Best-Effort Caching

Write to cache, but don't fail if it doesn't work:

```lisp
(defun save-user-data (user-id data)
  "Save data to database and cache (best effort)."
  ;; Save to database (critical - must succeed)
  (save-to-database user-id data)

  ;; Try to cache (optional - OK if fails)
  (handler-case
    (cl-memcached:mc-set (format nil "user:~A" user-id)
                         data
                         :timeout 3600)
    (cl-memcached:memcached-server-unreachable ()
      nil))  ; Silently fail cache write

  (format t "Data saved for user ~A~%" user-id))
```

**What this does:**
- Database write is critical (not wrapped in handler)
- Cache write is optional (wrapped, failures ignored)
- Application continues even if cache is down

### Pattern 3: Retry with Exponential Backoff

Retry failed operations with increasing delays:

```lisp
(defun get-with-retry (key &key (max-retries 3))
  "Get value with retry on connection failure."
  (loop for attempt from 1 to max-retries
        for backoff = 0.1 then (* backoff 2)
        do
    (handler-case
      (return (cl-memcached:mc-get-value key))

      (cl-memcached:memcached-server-unreachable (e)
        (if (< attempt max-retries)
            (progn
              (format *error-output*
                      "Attempt ~A failed: ~A. Retrying in ~,2Fs...~%"
                      attempt e backoff)
              (sleep backoff))
            (progn
              (format *error-output*
                      "All ~A attempts failed. Giving up.~%" max-retries)
              (return nil)))))))

;; Usage
(get-with-retry "important-key" :max-retries 5)
```

## Complete Examples

### Example 1: Robust Cache-Aside Pattern

```lisp
(defun robust-get-or-fetch (key fetch-fn &key (ttl 3600))
  "Get from cache or fetch from source, handling all errors."
  (handler-case
    ;; Try cache
    (let ((cached (cl-memcached:mc-get-value key)))
      (when cached
        (format t "Cache HIT: ~A~%" key)
        (return-from robust-get-or-fetch cached)))

    ;; Cache error - fall through to fetch
    (cl-memcached:memcached-server-unreachable (e)
      (format *error-output* "Cache error: ~A~%" e))

    ;; Input validation error - should fix code
    (cl-memcached:cl-mc-error (e)
      (format *error-output* "Programming error: ~A~%" e)
      (error e)))  ; Re-raise - this is a bug

  ;; Cache miss or error - fetch from source
  (format t "Cache MISS: ~A~%" key)
  (let ((data (funcall fetch-fn)))

    ;; Try to cache for next time (best effort)
    (handler-case
      (cl-memcached:mc-set key data :timeout ttl)
      (cl-memcached:memcached-server-unreachable ()
        nil))  ; OK if cache write fails

    data))

;; Usage
(robust-get-or-fetch "user:123"
                     (lambda () (fetch-user-from-db 123))
                     :ttl 3600)
```

### Example 2: Circuit Breaker Pattern

Temporarily stop using cache if it's consistently failing:

```lisp
(defvar *cache-circuit-open* nil)
(defvar *cache-failure-count* 0)
(defvar *cache-failure-threshold* 5)

(defun cache-get-with-circuit-breaker (key)
  "Get from cache with circuit breaker."
  (when *cache-circuit-open*
    (format *error-output* "Circuit breaker OPEN - skipping cache~%")
    (return-from cache-get-with-circuit-breaker nil))

  (handler-case
    (let ((result (cl-memcached:mc-get-value key)))
      ;; Success - reset failure count
      (setf *cache-failure-count* 0)
      result)

    (cl-memcached:memcached-server-unreachable (e)
      (incf *cache-failure-count*)
      (format *error-output* "Cache failure #~A: ~A~%"
              *cache-failure-count* e)

      ;; Open circuit if too many failures
      (when (>= *cache-failure-count* *cache-failure-threshold*)
        (format *error-output* "Opening circuit breaker~%")
        (setf *cache-circuit-open* t)

        ;; Schedule circuit closer (in real code, use timer)
        (bt:make-thread
          (lambda ()
            (sleep 60)  ; Wait 1 minute
            (format t "Closing circuit breaker (retry)~%")
            (setf *cache-circuit-open* nil)
            (setf *cache-failure-count* 0))))

      nil)))

;; Usage
(cache-get-with-circuit-breaker "key")
```

### Example 3: Timeout Handling

Don't wait forever for slow connections:

```lisp
(defun get-with-timeout (key timeout-seconds)
  "Get value with timeout."
  (let ((result nil)
        (done nil))

    ;; Run cache operation in separate thread
    (let ((thread
            (bt:make-thread
              (lambda ()
                (handler-case
                  (setf result (cl-memcached:mc-get-value key))
                  (error (e)
                    (format *error-output* "Cache error: ~A~%" e)))
                (setf done t)))))

      ;; Wait for result or timeout
      (loop for i from 0 below (* timeout-seconds 10)
            while (not done)
            do (sleep 0.1))

      ;; Check if completed
      (if done
          result
          (progn
            (format *error-output* "Cache timeout after ~As~%" timeout-seconds)
            ;; Note: thread still running but we've given up
            nil)))))

;; Usage
(get-with-timeout "slow-key" 2)  ; 2 second timeout
```

### Example 4: Logging and Monitoring

Track errors for debugging and alerting:

```lisp
(defvar *cache-error-log* nil)

(defun log-cache-error (operation error)
  "Log cache error for monitoring."
  (let ((entry (list :timestamp (get-universal-time)
                    :operation operation
                    :error (princ-to-string error))))
    (push entry *cache-error-log*)

    ;; Keep only recent errors
    (when (> (length *cache-error-log*) 100)
      (setf *cache-error-log* (subseq *cache-error-log* 0 100)))

    ;; Log to file/monitoring system
    (format *error-output* "[~A] Cache error in ~A: ~A~%"
            (get-universal-time) operation error)))

(defun monitored-cache-get (key)
  "Get with error logging."
  (handler-case
    (cl-memcached:mc-get-value key)

    (cl-memcached:memcached-server-unreachable (e)
      (log-cache-error 'get e)
      nil)))

;; Check recent errors
(defun recent-cache-errors ()
  "Show recent cache errors."
  (loop for entry in *cache-error-log*
        for i from 1 to 10
        do (format t "~A: ~A in ~A~%"
                  (getf entry :timestamp)
                  (getf entry :error)
                  (getf entry :operation))))
```

## Validation Errors (cl-mc-error)

These indicate programming errors - fix them, don't catch them:

```lisp
;; BAD - hiding bugs
(handler-case
  (cl-memcached:mc-get+ "not-a-list")  ; BUG!
  (cl-memcached:cl-mc-error ()
    nil))  ; Silently fails

;; GOOD - let error surface during development
(cl-memcached:mc-get+ (list "key"))  ; Correct usage
```

**When to catch cl-mc-error:**
- Never in production code
- Only for debugging/testing
- When validating user input before calling memcached

## Error Handling Best Practices

### 1. Distinguish Error Types

```lisp
(handler-case
  (cache-operation)

  ;; Network/server errors - expected, handle gracefully
  (cl-memcached:memcached-server-unreachable (e)
    (log-and-continue e))

  ;; Programming errors - unexpected, should fix
  (cl-memcached:cl-mc-error (e)
    (error "Bug in code: ~A" e)))
```

### 2. Log Errors Appropriately

```lisp
;; INFO level - cache miss (normal)
(format t "Cache miss for ~A~%" key)

;; WARNING level - cache error (degraded)
(format *error-output* "Cache unavailable: ~A~%" error)

;; ERROR level - repeated failures (critical)
(when (> failure-count 10)
  (format *error-output* "CRITICAL: Cache failing repeatedly~%"))
```

### 3. Provide Context in Errors

```lisp
(handler-case
  (cl-memcached:mc-get-value key)

  (cl-memcached:memcached-server-unreachable (e)
    ;; Include context for debugging
    (format *error-output*
            "Failed to get key ~S from cache ~A:~A - ~A~%"
            key
            (cl-memcached:mc-ip *cache*)
            (cl-memcached:mc-port *cache*)
            e)))
```

### 4. Test Error Paths

```lisp
(defun test-error-handling ()
  "Test that errors are handled gracefully."
  ;; Save original cache
  (let ((original-cache cl-memcached:*memcache*))

    ;; Point to non-existent server
    (setf cl-memcached:*memcache*
          (cl-memcached:make-memcache :host "invalid-host" :port 1))

    ;; Verify error handling works
    (handler-case
      (progn
        (robust-get-or-fetch "key" (lambda () "fallback-data"))
        (format t "Error handling test PASSED~%"))

      (error (e)
        (format t "Error handling test FAILED: ~A~%" e)))

    ;; Restore
    (setf cl-memcached:*memcache* original-cache)))

(test-error-handling)
```

## Common Mistakes

### Mistake 1: Catching too broadly

```lisp
;; Wrong - catches all errors (including bugs)
(handler-case
  (cache-operation)
  (error () nil))

;; Right - catch specific errors
(handler-case
  (cache-operation)
  (cl-memcached:memcached-server-unreachable () nil))
```

### Mistake 2: Silently failing

```lisp
;; Wrong - no logging
(handler-case
  (mc-set key value)
  (error () nil))

;; Right - log for debugging
(handler-case
  (mc-set key value)
  (cl-memcached:memcached-server-unreachable (e)
    (format *error-output* "Cache error: ~A~%" e)
    nil))
```

### Mistake 3: Not providing fallback

```lisp
;; Wrong - application fails if cache fails
(defun get-data (id)
  (mc-get-value (format nil "data:~A" id)))

;; Right - fallback to source
(defun get-data (id)
  (or (handler-case
        (mc-get-value (format nil "data:~A" id))
        (error () nil))
      (fetch-from-source id)))
```

## Troubleshooting

**Problem: Application crashes when memcached is down**

Solution: Wrap all cache operations in handlers with fallbacks.

**Problem: Can't tell why cache is failing**

Solution: Add detailed logging:
```lisp
(handler-case
  (mc-get-value key)
  (error (e)
    (format *error-output* "Cache error: ~A~%  Key: ~A~%  Stack: ~A~%"
            e key (trivial-backtrace:print-backtrace-to-string))))
```

**Problem: Too many timeout errors**

Solution: Check network, increase memcached memory, or review what's being cached.

## Verification

Test your error handling:

```lisp
(defun test-error-handling ()
  "Verify robust error handling."
  ;; Test 1: Invalid host
  (let ((bad-cache (make-memcache :host "invalid" :port 1)))
    (assert (null (handler-case
                    (mc-get-value "key" :memcache bad-cache)
                    (error () nil)))))

  ;; Test 2: Invalid input
  (assert (null (handler-case
                  (mc-get+ "not-a-list")
                  (cl-mc-error () nil))))

  (format t "Error handling tests passed!~%"))

(test-error-handling)
```

## Recap

You've learned:

✓ Two error types: `memcached-server-unreachable` and `cl-mc-error`
✓ Fail gracefully with fallbacks
✓ Best-effort caching pattern
✓ Retry with exponential backoff
✓ Circuit breaker pattern
✓ Proper logging and monitoring

## What's Next

- [Monitoring](monitoring.md) - Track cache health
- [Troubleshooting](../troubleshooting.md) - Debug common issues
- [Performance Tuning](../performance.md) - Optimize cache usage

---

**See also:** [Error Conditions](../../CLAUDE.md#9-error-conditions) (Implementation specification)
