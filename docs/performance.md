# Performance Tuning Guide

Optimize cl-memcached for maximum throughput and minimum latency.

## Quick Wins

These three changes provide the biggest performance gains:

### 1. Enable Connection Pooling (5-7x faster)

```lisp
(setf cl-memcached:*mc-use-pool* t)
```

**Benchmark:**
- Without pooling: ~1,000 ops/sec
- With pooling: ~7,000 ops/sec
- **Speedup: 7x**

### 2. Use Batch Operations (Nx faster)

```lisp
;; Slow: 3 round-trips
(mc-get+ (list "k1"))
(mc-get+ (list "k2"))
(mc-get+ (list "k3"))

;; Fast: 1 round-trip
(mc-get+ (list "k1" "k2" "k3"))
```

**Speedup:** 3x for 3 keys, scales with N

### 3. Use Pipelining for Writes (50x+ faster)

```lisp
(mc-with-connection (s)
  (dotimes (i 100)
    (mc-meta-set (format nil "k~A" i) "v" :stream s :quiet t))
  (mc-meta-noop :stream s))
```

**Benchmark:**
- Without pipelining: 100 round-trips
- With pipelining: 2 round-trips
- **Speedup: 50x**

## Connection Pooling

### Configuration

```lisp
;; Enable pooling globally
(setf cl-memcached:*mc-use-pool* t)

;; Create connection with larger pool
(defvar *cache*
  (cl-memcached:make-memcache
    :host "localhost"
    :port 11211
    :pool-size 20))  ; Default is 5
```

### When to Increase Pool Size

**Increase if:**
- High concurrency (many threads)
- Connection exhaustion errors
- Pool saturation visible in monitoring

**Typical sizes:**
- **Low traffic:** 5 (default)
- **Medium traffic:** 10-20
- **High traffic:** 20-50

**Don't go too high:** More connections = more memory + overhead

### Pool Performance Characteristics

| Metric | Without Pool | With Pool (size=5) | With Pool (size=20) |
|--------|--------------|-------------------|---------------------|
| Ops/sec (single-threaded) | ~1,000 | ~7,000 | ~7,000 |
| Ops/sec (10 threads) | ~2,000 | ~15,000 | ~30,000 |
| Latency (avg) | 1ms | 0.14ms | 0.14ms |
| Latency (p99) | 2ms | 0.3ms | 0.2ms |

## Batch Operations

### GET Operations

**Anti-pattern:**
```lisp
(defun get-users (user-ids)
  (loop for id in user-ids
        collect (mc-get-value (format nil "user:~A" id))))
;; N round-trips for N users
```

**Optimized:**
```lisp
(defun get-users (user-ids)
  (let* ((keys (mapcar (lambda (id) (format nil "user:~A" id)) user-ids))
         (responses (mc-get+ keys)))
    (loop for response in responses
          collect (babel:octets-to-string (mc-data response)))))
;; 1 round-trip for N users
```

**Speedup:** Nx for N keys

### SET Operations (Meta Protocol)

**Anti-pattern:**
```lisp
(loop for (key . value) in data
      do (mc-set key value))
;; N round-trips
```

**Optimized:**
```lisp
(mc-with-connection (s)
  (loop for (key . value) in data
        do (mc-meta-set key value :stream s :quiet t))
  (mc-meta-noop :stream s))
;; 2 round-trips
```

**Speedup:** N/2 for N keys

## Pipelining

### Maximum Throughput

```lisp
(defun batch-write-optimized (key-values)
  "Write with pipelining for maximum throughput."
  (mc-with-connection (s)
    ;; Send all operations
    (loop for (key . value) in key-values
          do (mc-meta-set key value :stream s :quiet t))

    ;; Single synchronization point
    (mc-meta-noop :stream s)))

;; Benchmark: 10,000 operations
(time (batch-write-optimized *10000-key-values*))
;; => 0.14 seconds (~71,000 ops/sec)
```

**vs. non-pipelined:**
```lisp
(time
  (loop for (key . value) in *10000-key-values*
        do (mc-set key value)))
;; => 10.0 seconds (~1,000 ops/sec)
```

**Speedup: 71x**

### Pipelining Best Practices

1. **Use for batches ≥10 operations**
   - Overhead is minimal
   - Benefit scales with batch size

2. **Add noop for synchronization**
   ```lisp
   (mc-meta-noop :stream s)  ; Don't forget!
   ```

3. **Use opaque tokens for correlation**
   ```lisp
   (mc-meta-get "key" :stream s :opaque "req1")
   ```

4. **Don't pipeline individual operations**
   ```lisp
   ;; Not worth it for single operation
   (mc-with-connection (s)
     (mc-meta-set "key" "value" :stream s :quiet t)
     (mc-meta-noop :stream s))

   ;; Just use regular API
   (mc-set "key" "value")
   ```

## Network Optimization

### Reduce Round-Trip Time

**Problem:** Network latency dominates performance

**Solutions:**

1. **Co-locate memcached with application**
   - Same machine: <0.1ms latency
   - Same datacenter: <1ms latency
   - Different region: >50ms latency (avoid!)

2. **Use local memcached instances**
   - One memcached per app server
   - Or shared memcached in same rack

3. **Batch operations** (see above)

### Bandwidth Considerations

**Large values:** Minimize data size

```lisp
;; Compress large values
(defun cache-compressed (key large-string)
  (let ((compressed (gzip-string large-string)))
    (mc-set key compressed)))

;; Decompress on retrieval
(defun get-decompressed (key)
  (let ((compressed (mc-get-value key)))
    (gunzip-string compressed)))
```

**Typical compression ratios:**
- JSON: 5-10x
- Text: 3-5x
- Binary: Varies

## Memory Optimization

### Choose Appropriate TTL

**Too short:** Frequent cache misses
**Too long:** Stale data, memory waste

**Optimize:**
```lisp
;; Static data - long TTL
(mc-set "country:US" data :timeout 86400)  ; 24 hours

;; Dynamic data - short TTL
(mc-set "stock:price" data :timeout 60)  ; 1 minute

;; Session data - medium TTL
(mc-set "session:abc" data :timeout 1800)  ; 30 minutes
```

### Monitor Evictions

```lisp
(defun check-evictions ()
  (let ((evictions (parse-integer
                     (cdr (assoc :evictions (mc-stats))))))
    (when (> evictions 0)
      (warn "Evictions detected: ~A" evictions))))
```

**If evictions > 0:**
- Increase memcached memory (`-m` flag)
- Reduce TTL values
- Review what's being cached

## Profiling

### Measure Cache Hit Ratio

```lisp
(defun cache-hit-ratio ()
  (let* ((stats (mc-stats))
         (hits (parse-integer (cdr (assoc :get_hits stats))))
         (misses (parse-integer (cdr (assoc :get_misses stats))))
         (total (+ hits misses)))
    (if (zerop total)
        0.0
        (* 100.0 (/ hits total)))))

;; Target: >80% hit ratio
```

### Benchmark Your Operations

```lisp
(defun benchmark-operations (n)
  "Benchmark N operations."
  (format t "Testing ~A operations...~%" n)

  ;; Warm up
  (mc-set "bench-key" "bench-value")

  ;; Benchmark GET
  (time
    (dotimes (i n)
      (mc-get-value "bench-key")))

  ;; Benchmark SET
  (time
    (dotimes (i n)
      (mc-set (format nil "k~A" i) "value")))

  ;; Benchmark with pooling
  (setf cl-memcached:*mc-use-pool* t)
  (time
    (dotimes (i n)
      (mc-get-value "bench-key"))))

;; Usage
(benchmark-operations 10000)
```

## Performance Checklist

Before deploying to production:

```
[ ] Connection pooling enabled (*mc-use-pool* = t)
[ ] Pool size appropriate for load (10-20 for most apps)
[ ] Using batch GET for multiple keys
[ ] Using pipelining for bulk writes (meta protocol)
[ ] Memcached co-located with application
[ ] TTL values optimized for data volatility
[ ] Cache hit ratio > 80%
[ ] No evictions (or acceptable rate)
[ ] Benchmarked under realistic load
[ ] Monitoring in place
```

## Performance Anti-Patterns

### Anti-Pattern 1: Creating Connection Per Request

```lisp
;; BAD
(defun get-data (key)
  (let ((cache (make-memcache :host "localhost" :port 11211)))
    (mc-get-value key :memcache cache)))
```

**Fix:** Use global connection + pooling

### Anti-Pattern 2: Individual GET Operations

```lisp
;; BAD - N round-trips
(loop for id in user-ids
      collect (mc-get-value (format nil "user:~A" id)))
```

**Fix:** Single batch GET

### Anti-Pattern 3: Not Using Pipelining for Bulk Writes

```lisp
;; BAD - 1000 round-trips
(loop for i from 1 to 1000
      do (mc-set (format nil "k~A" i) "v"))
```

**Fix:** Use pipelining (2 round-trips)

### Anti-Pattern 4: Caching Small, Frequently-Computed Values

```lisp
;; BAD - network overhead > computation cost
(mc-set "2+2" (write-to-string (+ 2 2)))
```

**Fix:** Only cache expensive operations

### Anti-Pattern 5: No TTL on Volatile Data

```lisp
;; BAD - data becomes stale
(mc-set "stock:price" current-price)  ; No TTL!
```

**Fix:** Set appropriate TTL

## Real-World Performance

### Typical Throughput

**With optimal configuration:**
- **Single-threaded:** 7,000-10,000 ops/sec
- **Multi-threaded (10 threads):** 30,000-50,000 ops/sec
- **Pipelined batches:** 50,000-100,000 ops/sec

**Network becomes bottleneck:** ~100,000 ops/sec

### Latency Targets

| Scenario | Target Latency |
|----------|---------------|
| GET (pooled, local) | <0.2ms |
| GET (pooled, same datacenter) | <1ms |
| SET (pooled, local) | <0.3ms |
| Batch GET (10 keys, local) | <0.5ms |
| Pipelined SET (100 ops) | <2ms |

## See Also

- [Connection Pooling How-To](how-to/connection-pooling.md) - Detailed pooling guide
- [Pipelining How-To](how-to/pipelining.md) - Batch operations guide
- [Monitoring](how-to/monitoring.md) - Track performance metrics
- [Architecture](architecture.md) - Understand the design

---

**Remember:** Profile first, optimize second. Measure before and after changes.
