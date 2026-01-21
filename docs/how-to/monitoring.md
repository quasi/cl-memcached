# How-To: Monitor Memcached Health

<!-- Generated from: canon/features/stats/scenarios/stats-operations.md -->

Monitor memcached server health, performance, and usage using statistics commands.

## Problem

You need to:

- Verify memcached is working correctly
- Track cache hit/miss ratios
- Monitor memory usage
- Identify performance bottlenecks
- Alert on issues before they cause outages

## Solution

Use cl-memcached's statistics commands to collect metrics:

- `mc-stats` - Overall server statistics
- `mc-stats-items` - Per-slab item counts
- `mc-stats-slabs` - Slab allocation details
- `mc-stats-sizes` - Item size distribution

## Prerequisites

- Memcached server running
- cl-memcached loaded
- Understanding of basic operations ([see Tutorial](../tutorials/01-basics.md))

## Quick Example

```lisp
;; Get overall statistics
(cl-memcached:mc-stats)
;; => ((pid . "1234")
;;     (uptime . "86400")
;;     (curr_items . "1000")
;;     (get_hits . "5000")
;;     (get_misses . "500")
;;     ...)
```

## Key Metrics to Monitor

### 1. Cache Hit Ratio

Most important metric - higher is better:

```lisp
(defun cache-hit-ratio ()
  "Calculate cache hit ratio as percentage."
  (let* ((stats (cl-memcached:mc-stats))
         (hits (parse-integer (cdr (assoc :get_hits stats))))
         (misses (parse-integer (cdr (assoc :get_misses stats))))
         (total (+ hits misses)))

    (if (zerop total)
        0.0
        (* 100.0 (/ hits total)))))

;; Usage
(cache-hit-ratio)
;; => 90.9  (90.9% hit ratio - good!)
```

**What's good?**
- 80%+ - Excellent
- 60-80% - Good
- 40-60% - Fair (consider increasing TTL or cache size)
- <40% - Poor (caching may not be helping)

### 2. Memory Usage

Monitor to prevent evictions:

```lisp
(defun memory-usage ()
  "Get memory usage statistics."
  (let* ((stats (cl-memcached:mc-stats))
         (bytes (parse-integer (cdr (assoc :bytes stats))))
         (limit (parse-integer (cdr (assoc :limit_maxbytes stats))))
         (used-pct (* 100.0 (/ bytes limit))))

    (list :bytes bytes
          :limit limit
          :used-percent used-pct)))

;; Usage
(memory-usage)
;; => (:BYTES 52428800 :LIMIT 67108864 :USED-PERCENT 78.1)
```

**Alert if:**
- >90% - Critical (evictions imminent)
- >80% - Warning (may need more memory)
- <50% - OK (plenty of room)

### 3. Evictions

Items removed due to memory pressure:

```lisp
(defun check-evictions ()
  "Check if items are being evicted."
  (let* ((stats (cl-memcached:mc-stats))
         (evictions (parse-integer (cdr (assoc :evictions stats)))))

    (if (zerop evictions)
        (format t "No evictions - good!~%")
        (format t "WARNING: ~A items evicted~%" evictions))))

;; Usage
(check-evictions)
;; => WARNING: 1234 items evicted
```

**If evictions are happening:**
- Increase memcached memory limit
- Reduce TTL values
- Review what's being cached (too much?)

### 4. Connection Count

Monitor active connections:

```lisp
(defun connection-count ()
  "Get current connection count."
  (let* ((stats (cl-memcached:mc-stats))
         (curr-conn (parse-integer (cdr (assoc :curr_connections stats))))
         (total-conn (parse-integer (cdr (assoc :total_connections stats)))))

    (list :current curr-conn
          :total total-conn)))

;; Usage
(connection-count)
;; => (:CURRENT 25 :TOTAL 50000)
```

## Complete Examples

### Example 1: Health Check Script

```lisp
(defun memcached-health-check ()
  "Comprehensive health check."
  (handler-case
    (let* ((stats (cl-memcached:mc-stats))
           (hits (parse-integer (cdr (assoc :get_hits stats))))
           (misses (parse-integer (cdr (assoc :get_misses stats))))
           (total-gets (+ hits misses))
           (hit-ratio (if (zerop total-gets)
                         0.0
                         (* 100.0 (/ hits total-gets))))
           (evictions (parse-integer (cdr (assoc :evictions stats))))
           (bytes (parse-integer (cdr (assoc :bytes stats))))
           (limit (parse-integer (cdr (assoc :limit_maxbytes stats))))
           (mem-pct (* 100.0 (/ bytes limit))))

      (format t "=== Memcached Health Check ===~%")
      (format t "Hit Ratio: ~,1F%~%" hit-ratio)
      (format t "Memory Usage: ~,1F% (~A / ~A bytes)~%" mem-pct bytes limit)
      (format t "Evictions: ~A~%" evictions)

      ;; Determine health
      (cond
        ((and (> hit-ratio 80) (zerop evictions) (< mem-pct 80))
         (format t "Status: HEALTHY ✓~%"))
        ((or (< hit-ratio 60) (> evictions 100) (> mem-pct 90))
         (format t "Status: CRITICAL ✗~%"))
        (t
         (format t "Status: WARNING ⚠~%"))))

    (cl-memcached:memcached-server-unreachable (e)
      (format t "Status: DOWN ✗ (~A)~%" e))))

;; Usage
(memcached-health-check)
```

### Example 2: Continuous Monitoring

```lisp
(defun start-monitoring (&key (interval 60))
  "Monitor memcached every INTERVAL seconds."
  (loop
    (format t "~%[~A] Memcached Stats~%" (get-universal-time))
    (memcached-health-check)
    (sleep interval)))

;; Usage (runs forever)
(start-monitoring :interval 300)  ; Check every 5 minutes
```

### Example 3: Alert on Thresholds

```lisp
(defun check-alerts ()
  "Check metrics against thresholds and alert."
  (let* ((stats (cl-memcached:mc-stats))
         (hits (parse-integer (cdr (assoc :get_hits stats))))
         (misses (parse-integer (cdr (assoc :get_misses stats))))
         (hit-ratio (if (zerop (+ hits misses))
                       100.0
                       (* 100.0 (/ hits (+ hits misses)))))
         (evictions (parse-integer (cdr (assoc :evictions stats))))
         (bytes (parse-integer (cdr (assoc :bytes stats))))
         (limit (parse-integer (cdr (assoc :limit_maxbytes stats))))
         (mem-pct (* 100.0 (/ bytes limit)))
         (alerts nil))

    ;; Check thresholds
    (when (< hit-ratio 60)
      (push (format nil "Low hit ratio: ~,1F%" hit-ratio) alerts))

    (when (> evictions 0)
      (push (format nil "Evictions detected: ~A" evictions) alerts))

    (when (> mem-pct 90)
      (push (format nil "High memory usage: ~,1F%" mem-pct) alerts))

    ;; Report alerts
    (if alerts
        (progn
          (format t "~%⚠ ALERTS:~%")
          (dolist (alert alerts)
            (format t "  - ~A~%" alert))
          alerts)
        (format t "✓ All metrics within thresholds~%"))))

;; Usage
(check-alerts)
```

### Example 4: Per-Slab Statistics

Understand memory distribution:

```lisp
(defun slab-report ()
  "Report on slab allocation."
  (let ((slabs (cl-memcached:mc-stats-slabs)))

    (format t "~%=== Slab Report ===~%")
    (format t "~20A ~10A ~10A~%" "Slab" "Items" "Memory")

    (loop for (key . value) in slabs
          when (search "chunk_size" (string key))
          do (let* ((slab-id (subseq (string key) 0
                                    (position #\: (string key))))
                    (items-key (intern (format nil "~A:USED_CHUNKS" slab-id) :keyword))
                    (mem-key (intern (format nil "~A:MEM_REQUESTED" slab-id) :keyword))
                    (items (cdr (assoc items-key slabs)))
                    (mem (cdr (assoc mem-key slabs))))

               (when (and items mem)
                 (format t "~20A ~10A ~10A~%" slab-id items mem))))))

;; Usage
(slab-report)
```

## Understanding Stats Output

### Most Important Stats

| Stat | What It Means | Good Value |
|------|---------------|------------|
| `get_hits` | Successful GET operations | High |
| `get_misses` | Failed GET operations (key not found) | Low |
| `evictions` | Items removed due to memory | 0 |
| `curr_items` | Current items stored | Depends on use case |
| `bytes` | Current memory usage | < 80% of limit |
| `limit_maxbytes` | Maximum memory allowed | Configured value |
| `curr_connections` | Active connections | Normal for your load |
| `cmd_get` | Total GET commands | Increasing |
| `cmd_set` | Total SET commands | Increasing |

### Derived Metrics

Calculate useful metrics from raw stats:

```lisp
(defun derived-metrics ()
  "Calculate derived metrics."
  (let* ((stats (cl-memcached:mc-stats))
         (hits (parse-integer (cdr (assoc :get_hits stats))))
         (misses (parse-integer (cdr (assoc :get_misses stats))))
         (total-gets (+ hits misses))
         (sets (parse-integer (cdr (assoc :cmd_set stats))))
         (bytes (parse-integer (cdr (assoc :bytes stats))))
         (items (parse-integer (cdr (assoc :curr_items stats)))))

    (list
      :hit-ratio (if (zerop total-gets) 0.0 (* 100.0 (/ hits total-gets)))
      :avg-item-size (if (zerop items) 0 (/ bytes items))
      :get-to-set-ratio (if (zerop sets) 0.0 (/ total-gets sets)))))

;; Usage
(derived-metrics)
;; => (:HIT-RATIO 85.5 :AVG-ITEM-SIZE 2048 :GET-TO-SET-RATIO 10.5)
```

## Monitoring Best Practices

### 1. Collect Metrics Regularly

```lisp
(defun log-metrics-to-file (filename)
  "Append metrics to log file."
  (with-open-file (out filename
                       :direction :output
                       :if-exists :append
                       :if-does-not-exist :create)
    (let* ((stats (mc-stats))
           (timestamp (get-universal-time)))

      (format out "~A,~A,~A,~A~%"
              timestamp
              (cdr (assoc :get_hits stats))
              (cdr (assoc :get_misses stats))
              (cdr (assoc :evictions stats))))))

;; Run periodically
(loop
  (log-metrics-to-file "/var/log/memcached-metrics.csv")
  (sleep 60))
```

### 2. Set Up Alerts

```lisp
(defun send-alert (message)
  "Send alert (implement with your alerting system)."
  (format t "ALERT: ~A~%" message)
  ;; Email, Slack, PagerDuty, etc.
  )

(defun monitoring-loop ()
  "Continuous monitoring with alerts."
  (loop
    (let ((alerts (check-alerts)))
      (when alerts
        (dolist (alert alerts)
          (send-alert alert))))
    (sleep 300)))  ; Check every 5 minutes
```

### 3. Track Trends

```lisp
(defvar *metric-history* (make-array 100 :fill-pointer 0))

(defun track-hit-ratio ()
  "Track hit ratio over time."
  (let ((ratio (cache-hit-ratio)))
    (vector-push ratio *metric-history*)

    ;; Calculate trend
    (when (>= (fill-pointer *metric-history*) 10)
      (let* ((recent (subseq *metric-history*
                            (- (fill-pointer *metric-history*) 10)))
             (avg (/ (reduce #'+ recent) (length recent))))
        (format t "Current: ~,1F%, 10-sample avg: ~,1F%~%" ratio avg)))))
```

## Troubleshooting with Stats

### High Miss Ratio

**Cause:** TTL too short, cache too small, or poor caching strategy.

**Investigation:**
```lisp
(let* ((stats (mc-stats))
       (evictions (parse-integer (cdr (assoc :evictions stats)))))
  (if (> evictions 0)
      (format t "Cause: Memory pressure (evictions)~%")
      (format t "Cause: TTL or caching strategy~%")))
```

### Memory Issues

**Check slab distribution:**
```lisp
(mc-stats-slabs)  ; See which slabs are using memory
```

**Check item sizes:**
```lisp
(mc-stats-sizes)  ; See distribution of item sizes
```

### Connection Issues

**Check connection stats:**
```lisp
(let* ((stats (mc-stats))
       (rejected (parse-integer (cdr (assoc :rejected_connections stats)))))
  (when (> rejected 0)
    (format t "WARNING: ~A connections rejected~%" rejected)))
```

## Recap

You've learned:

✓ Key metrics to monitor (hit ratio, memory, evictions)
✓ How to use `mc-stats`, `mc-stats-items`, `mc-stats-slabs`, `mc-stats-sizes`
✓ Building health check scripts
✓ Setting up alerts on thresholds
✓ Troubleshooting with statistics

## What's Next

- [Error Handling](error-handling.md) - Handle memcached failures
- [Performance Tuning](../performance.md) - Optimize based on metrics
- [API Reference](../reference/api-reference.md) - Complete stats API

---

**See also:** [Stats Scenarios](../../canon/features/stats/scenarios/stats-operations.md) (Canon specification)
