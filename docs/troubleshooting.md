# Troubleshooting Guide

<!-- Generated from: CLAUDE.md + common issues -->

Solutions to common problems when using cl-memcached.

## Connection Issues

### Can't Connect to Memcached

**Symptom:**
```lisp
(mc-set "key" "value")
;; => Error: MEMCACHED-SERVER-UNREACHABLE
```

**Causes & Solutions:**

1. **Memcached not running**

   Check if memcached is running:
   ```bash
   echo "stats" | nc localhost 11211
   ```

   If no response, start memcached:
   ```bash
   # macOS
   brew services start memcached

   # Linux
   sudo systemctl start memcached
   ```

2. **Wrong host/port**

   Verify your connection settings:
   ```lisp
   (cl-memcached:mc-ip *memcache*)    ; => "127.0.0.1"
   (cl-memcached:mc-port *memcache*)  ; => 11211
   ```

   Create correct connection:
   ```lisp
   (setf *cache* (make-memcache :host "localhost" :port 11211))
   ```

3. **Firewall blocking**

   Test with telnet:
   ```bash
   telnet localhost 11211
   ```

   If blocked, configure firewall or use correct host.

### Timeout Errors

**Symptom:** Operations hang or timeout

**Causes:**
- Network latency
- Memcached overloaded
- Large value transfers

**Solutions:**

Enable connection pooling (faster):
```lisp
(setf cl-memcached:*mc-use-pool* t)
```

Check server load:
```lisp
(let ((stats (mc-stats)))
  (cdr (assoc :threads stats)))
```

## Data Type Errors

### "KEYS-LIST has to be a LIST of keys"

**Symptom:**
```lisp
(mc-get+ "my-key")
;; => Error: CL-MC-ERROR: KEYS-LIST has to be a LIST of keys
```

**Solution:** Wrap key in list:
```lisp
(mc-get+ (list "my-key"))  ; Correct
```

### Getting Raw Octets Instead of Strings

**Symptom:**
```lisp
(mc-data (first (mc-get+ (list "key"))))
;; => #(72 101 108 108 111)  (not "Hello")
```

**Solution:** Decode octets to string:
```lisp
(babel:octets-to-string (mc-data response))
;; => "Hello"
```

Or use convenience function:
```lisp
(mc-get-value "key")
;; => "Hello"
```

### Type Errors with Keys

**Symptom:**
```lisp
(mc-set :my-key "value")
;; => Error: CL-MC-ERROR
```

**Solution:** Keys MUST be strings:
```lisp
(mc-set "my-key" "value")  ; Correct
```

## Missing Data

### Key Not Found (NIL Return)

**Symptom:**
```lisp
(mc-get-value "missing-key")
;; => NIL
```

**This is NORMAL, not an error.** Missing keys return NIL.

**Possible causes:**
1. Key never existed
2. Key expired (TTL reached)
3. Key evicted (memory pressure)
4. Typo in key name (keys are case-sensitive)

**Debug:**
```lisp
;; Check if key exists
(mc-get+ (list "key"))
;; => NIL (doesn't exist) or (#<MEMCACHE-RESPONSE ...>)

;; List all keys (if memcached compiled with --enable-dumper)
;; Note: Not available in standard memcached
```

### Data Disappeared After Restart

**Cause:** Memcached is in-memory only (not persistent)

**Solution:** Memcached doesn't persist data across restarts. Use database for persistent storage, memcached for caching.

### Unexpected Evictions

**Symptom:**
```lisp
(let ((stats (mc-stats)))
  (cdr (assoc :evictions stats)))
;; => 1000  (many evictions)
```

**Cause:** Memory limit reached

**Solutions:**

1. Increase memcached memory:
   ```bash
   memcached -m 128  # 128MB
   ```

2. Reduce TTL values:
   ```lisp
   (mc-set "key" "value" :timeout 300)  ; 5 minutes instead of hours
   ```

3. Review what you're caching (too much?).

## CAS Operation Issues

### Always Getting "EXISTS" Response

**Symptom:**
```lisp
(mc-cas "key" "new-value" cas-token)
;; => "EXISTS"  (every time)
```

**Causes:**

1. **Using mc-get instead of mc-gets**

   Wrong:
   ```lisp
   (let ((response (first (mc-get+ (list "key")))))
     (mc-cas-unique response))  ; => NIL!
   ```

   Right:
   ```lisp
   (let ((response (first (mc-gets+ (list "key")))))
     (mc-cas-unique response))  ; => "123abc..."
   ```

2. **CAS token is stale**

   ```lisp
   ;; Token becomes stale after ANY write to the key
   (let ((response (first (mc-gets+ (list "key")))))
     (mc-set "key" "modified")  ; Invalidates CAS token!
     (mc-cas "key" "new" (mc-cas-unique response)))  ; => "EXISTS"
   ```

   Solution: Get fresh CAS token immediately before update.

### "NOT_FOUND" on CAS

**Cause:** Key was deleted between GETS and CAS

**Solution:** Handle NOT_FOUND response:
```lisp
(let ((result (mc-cas "key" "new" cas-token)))
  (cond
    ((string= result "STORED") ...)
    ((string= result "EXISTS") ...)  ; Retry
    ((string= result "NOT_FOUND") ...)))  ; Recreate key
```

## Counter Issues

### "NOT_FOUND" on Increment

**Symptom:**
```lisp
(mc-incr "counter")
;; => "NOT_FOUND"
```

**Cause:** Counter not initialized

**Solution:** Initialize first:
```lisp
(mc-set "counter" "0")
(mc-incr "counter")
;; => 1
```

### Counter Goes to Zero Unexpectedly

**Cause:** Decrementing below zero (counters clamp at 0)

```lisp
(mc-set "counter" "5")
(mc-decr "counter" 10)
;; => 0  (not -5!)
```

**Solution:** Use signed integers stored as strings if you need negative values.

## Performance Issues

### Slow Operations

**Symptom:** Operations take >10ms

**Solutions:**

1. **Enable connection pooling:**
   ```lisp
   (setf cl-memcached:*mc-use-pool* t)
   ```
   Expected: 5-7x speedup

2. **Use pipelining for batches:**
   ```lisp
   (mc-with-connection (s)
     (dotimes (i 100)
       (mc-meta-set (format nil "k~A" i) "v" :stream s :quiet t))
     (mc-meta-noop :stream s))
   ```
   Expected: 50x+ speedup for 100 operations

3. **Batch GET operations:**
   ```lisp
   ;; Slow: 3 requests
   (mc-get+ (list "k1"))
   (mc-get+ (list "k2"))
   (mc-get+ (list "k3"))

   ;; Fast: 1 request
   (mc-get+ (list "k1" "k2" "k3"))
   ```

### High Cache Miss Ratio

**Check:**
```lisp
(let* ((stats (mc-stats))
       (hits (parse-integer (cdr (assoc :get_hits stats))))
       (misses (parse-integer (cdr (assoc :get_misses stats)))))
  (* 100.0 (/ hits (+ hits misses))))
;; => 45.0  (low hit ratio)
```

**Solutions:**
- Increase TTL values
- Review caching strategy
- Check if evictions are happening

## Encoding Issues

### Unicode Characters Corrupted

**Symptom:**
```lisp
(mc-set "key" "Hello, 世界!")
(mc-get-value "key")
;; => "Hello, ???"  (corrupted)
```

**Cause:** Wrong encoding

**Solution:** Ensure UTF-8:
```lisp
(setf cl-memcached:*mc-default-encoding* :utf-8)
(mc-set "key" "Hello, 世界!")
(mc-get-value "key")
;; => "Hello, 世界!"  (correct)
```

### Binary Data Corrupted

**Symptom:** Binary data (images, etc.) corrupted after round-trip

**Cause:** Trying to decode binary as UTF-8

**Solution:** Use octets directly:
```lisp
;; Store binary
(mc-store "image" binary-octets :command :set)

;; Retrieve binary (don't decode!)
(let ((response (first (mc-get+ (list "image")))))
  (mc-data response))  ; Use octets directly
```

## Meta Protocol Issues

### Pipelining Doesn't Work

**Symptom:** Operations seem to fail silently

**Cause:** Forgot to flush pipeline with noop

**Solution:**
```lisp
(mc-with-connection (s)
  (mc-meta-set "key" "value" :stream s :quiet t)
  (mc-meta-noop :stream s))  ; Must flush!
```

### Missing :stream Parameter

**Symptom:**
```lisp
(mc-with-connection (s)
  (mc-meta-set "key" "value" :quiet t))
;; => Error or unexpected behavior
```

**Solution:** Add `:stream s`:
```lisp
(mc-with-connection (s)
  (mc-meta-set "key" "value" :stream s :quiet t))
```

## Debugging Tools

### Enable Debug Output

```lisp
;; Trace specific functions
(trace cl-memcached:mc-set)
(trace cl-memcached:mc-get+)

;; Untrace
(untrace)
```

### Check Server Stats

```lisp
(mc-stats)
;; Returns alist of all server stats
```

### Verify Connection

```lisp
(defun test-connection ()
  (handler-case
    (progn
      (mc-set "test-key" "test-value")
      (let ((result (mc-get-value "test-key")))
        (assert (string= result "test-value"))
        (mc-del "test-key")
        (format t "Connection OK~%")))
    (error (e)
      (format t "Connection FAILED: ~A~%" e))))

(test-connection)
```

## Getting Help

If you're still stuck:

1. **Check this guide** - Most issues are covered here
2. **Read error messages carefully** - They usually indicate the problem
3. **Verify memcached is running** - `echo "stats" | nc localhost 11211`
4. **Check Canon specification** - See `CLAUDE.md` and `canon/` directory
5. **File an issue** - https://github.com/quasi/cl-memcached/issues

## Quick Reference: Common Errors

| Error | Cause | Solution |
|-------|-------|----------|
| MEMCACHED-SERVER-UNREACHABLE | Can't connect | Check memcached running, host/port correct |
| "KEYS-LIST has to be a LIST" | Passing string instead of list | Wrap in `(list ...)` |
| NIL from mc-get-value | Key doesn't exist | Normal - check TTL, evictions, typos |
| Raw octets instead of string | Not decoding | Use `babel:octets-to-string` or `mc-get-value` |
| "EXISTS" on CAS | Stale CAS token | Use `mc-gets+` not `mc-get+`, get fresh token |
| "NOT_FOUND" on incr | Counter not initialized | `mc-set "key" "0"` first |
| Slow performance | Not using pooling | `(setf *mc-use-pool* t)` |
| Unicode corruption | Wrong encoding | `(setf *mc-default-encoding* :utf-8)` |

---

**See also:**
- [Error Handling](how-to/error-handling.md) - Robust error handling patterns
- [Monitoring](how-to/monitoring.md) - Track cache health
- [FAQ](faq.md) - Frequently asked questions
