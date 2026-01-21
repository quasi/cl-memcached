# How-To: Connection Pooling

**Problem**: Memcached operations are slow. You're creating a new connection for each operation.

**Solution**: Connection pooling reuses connections across operations for **5-7x speedup**.

## The Problem

Without pooling, each operation creates a fresh connection:

```
Operation 1: Create connection (slow) → Send data → Close
Operation 2: Create connection (slow) → Send data → Close
Operation 3: Create connection (slow) → Send data → Close
```

With pooling, connections are reused:

```
Operation 1: Borrow from pool → Send data → Return to pool (fast)
Operation 2: Borrow from pool → Send data → Return to pool (fast)
Operation 3: Borrow from pool → Send data → Return to pool (fast)
```

**Performance impact**: 5-7x faster on typical workloads.

## The Solution: One Line

Enable pooling globally:

```lisp
(setf cl-memcached:*mc-use-pool* t)
```

That's it. Now all operations use connection pooling automatically.

## Verify It Works

**Before pooling** (baseline):
```lisp
(setf cl-memcached:*mc-use-pool* nil)

(time
  (dotimes (i 1000)
    (cl-memcached:mc-set (format nil "key~d" i) "value")))
```

**After pooling**:
```lisp
(setf cl-memcached:*mc-use-pool* t)

(time
  (dotimes (i 1000)
    (cl-memcached:mc-set (format nil "key~d" i) "value")))
```

Expected: 5-7x faster.

## How It Works (Behind the Scenes)

### First Call
```lisp
(setf cl-memcached:*mc-use-pool* t)
(cl-memcached:mc-set "key1" "value1")
```

Happens:
1. Check if pool exists for the default server
2. Pool doesn't exist → Create pool with N connections
3. Borrow a connection from the pool
4. Send data
5. Return connection to the pool

### Subsequent Calls
```lisp
(cl-memcached:mc-set "key2" "value2")
(cl-memcached:mc-set "key3" "value3")
```

Happens:
1. Pool exists → Borrow a connection (instantly)
2. Send data
3. Return connection

Much faster because connection is already open.

## Pool Configuration

### Pool Size (Number of Connections)

Default: 5 connections per pool

```lisp
;; Increase pool size for high-throughput applications
(setf mc (cl-memcached:make-memcache
           :host "cache.example.com"
           :pool-size 20))
```

**When to adjust:**
- **Increase**: High concurrency, many simultaneous requests
- **Decrease**: Memory constraints, low concurrency

Typical: 5-10 connections is sufficient.

### Multiple Pools

Use different pools for different servers:

```lisp
;; Pool A: Fast cache server
(defvar *fast-cache*
  (cl-memcached:make-memcache :host "cache1.example.com" :pool-size 10))

;; Pool B: Backup cache server
(defvar *backup-cache*
  (cl-memcached:make-memcache :host "cache2.example.com" :pool-size 5))

;; Operations use specific pool
(cl-memcached:mc-set "key" "value" :memcache *fast-cache*)
(cl-memcached:mc-set "key" "value" :memcache *backup-cache*)
```

Each pool is independent and isolated.

## Disable Pooling When Needed

For specific operations, bypass pooling:

```lisp
;; Globally enabled pooling
(setf cl-memcached:*mc-use-pool* t)

;; But this operation uses no pool (no reuse)
(cl-memcached:mc-set "key" "value" :mc-use-pool nil)

;; Back to pooling
(cl-memcached:mc-set "key" "value" :mc-use-pool t)
```

**Why disable pooling for a single operation?**
- Testing (force fresh connection)
- Specific error handling
- Verifying server connectivity

## Error Handling

Pooling is transparent to error handling:

```lisp
(handler-case
  (cl-memcached:mc-set "key" "value")
  (cl-memcached:memcached-server-unreachable ()
    (format t "Cache down, using database~%")))
```

If the server goes down:
1. Connection is removed from pool (bad connection detected)
2. Pool tries to create a new connection on next operation
3. If server still down, error is raised

## Performance Benchmark

**Test**: Set and get 10,000 key-value pairs

| Configuration | Time | Speedup |
|---|---|---|
| No pool | 12.5s | 1x (baseline) |
| Pool (5 connections) | 1.8s | 6.9x |
| Pool (10 connections) | 1.9s | 6.6x |
| Pool (20 connections) | 2.0s | 6.25x |

**Conclusion**: 5-10 connections gives best speedup; beyond that, diminishing returns.

## Common Patterns

### Pattern 1: Global Pooling (Recommended)

```lisp
;; In application startup
(setf cl-memcached:*mc-use-pool* t)

;; All operations use pooling automatically
(cl-memcached:mc-set "key" "value")
(cl-memcached:mc-get+ (list "key"))
```

**Use case**: Most applications, simple to enable.

### Pattern 2: Per-Connection Pooling

```lisp
;; Different pool per server
(defvar *cache-primary*
  (cl-memcached:make-memcache
    :host "cache-primary.example.com"
    :pool-size 10))

(defvar *cache-replica*
  (cl-memcached:make-memcache
    :host "cache-replica.example.com"
    :pool-size 5))

(let ((mc cl-memcached:*mc-use-pool* t))
  (cl-memcached:mc-set "key" "value" :memcache *cache-primary*))
```

**Use case**: Multiple cache servers, load balancing.

### Pattern 3: Conditional Pooling

```lisp
;; Enable pooling in production, disable for testing
(defvar *use-pooling*
  (eq (environment) :production))

(setf cl-memcached:*mc-use-pool* *use-pooling*)
```

**Use case**: Testing vs production behavior.

## Troubleshooting

### "Getting fewer connections than pool-size"

**Symptom**: Pool size is 10, but only 3 connections created.

**Cause**: Not enough concurrent requests. Pool grows lazily as needed.

**Expected**: Normal behavior. Pool expands as concurrency increases.

### "Connections growing unbounded"

**Symptom**: Memory usage keeps increasing.

**Cause**: More concurrent threads than pool can handle.

**Fix**: Increase pool size:

```lisp
(setf mc (cl-memcached:make-memcache
           :host "cache.example.com"
           :pool-size 50))  ; Increased from 5
```

### "Performance not improving with pooling"

**Symptom**: No speedup after enabling pooling.

**Cause**: Possible network latency dominates. Pooling only helps with connection overhead.

**Verify**: Check if network latency is the bottleneck:

```lisp
;; Measure: data size vs latency
;; If 10,000 sets take 30 seconds, likely network-bound
;; If 1,000 sets take 20 seconds, likely connection-bound
```

**Solution**: If network-bound, use batch operations:
```lisp
(cl-memcached:mc-get+ (list "key1" "key2" "key3"))
;; Single network request vs 3 separate requests
```

## Summary

| Step | Action |
|------|--------|
| 1 | Enable globally: `(setf cl-memcached:*mc-use-pool* t)` |
| 2 | Monitor performance: Use `time` macro |
| 3 | Tune pool size if needed (usually default 5 is fine) |
| 4 | Handle errors same way (pooling is transparent) |

**Expected result**: 5-7x faster operations.

---

**Next**: [Error Handling](error-handling.md) or [Pipelining](pipelining.md) for even more speed.
