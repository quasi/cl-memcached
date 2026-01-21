# Tutorial: Implementing Caching Strategies

<!-- Generated from: canon/features/storage/scenarios/ -->

Learn practical caching patterns for real-world applications. This tutorial covers cache-aside, write-through, and cache invalidation strategies.

## What You'll Learn

- Cache-aside pattern (lazy loading)
- Setting appropriate TTL values
- Cache invalidation strategies
- Handling cache failures gracefully
- When to cache and when not to

## Prerequisites

- Complete [Tutorial 01: Basics](01-basics.md)
- Memcached server running
- Understanding of your application's data access patterns

## Scenario: Caching HTTP API Responses

You're building a web service that fetches user data from a slow external API. Caching can reduce latency and API costs.

### Step 1: Implement Cache-Aside Pattern

The cache-aside pattern:
1. Check cache first
2. If miss, fetch from source
3. Store in cache for next time

```lisp
(defun get-user-data (user-id)
  "Get user data, checking cache first."
  (let ((cache-key (format nil "user:~A" user-id)))

    ;; Try cache first
    (let ((cached (cl-memcached:mc-get-value cache-key)))
      (when cached
        (format t "Cache HIT for user ~A~%" user-id)
        (return-from get-user-data cached)))

    ;; Cache miss - fetch from source
    (format t "Cache MISS for user ~A, fetching from API...~%" user-id)
    (let ((data (fetch-from-external-api user-id)))

      ;; Store in cache for 5 minutes
      (cl-memcached:mc-set cache-key data :timeout 300)

      data)))

;; Simulated external API
(defun fetch-from-external-api (user-id)
  "Simulate slow external API call."
  (sleep 1)  ; Simulate network latency
  (format nil "{\"id\": ~A, \"name\": \"User ~A\"}" user-id user-id))
```

**Test it:**

```lisp
;; First call - cache miss (slow)
(time (get-user-data 123))
;; Cache MISS for user 123, fetching from API...
;; Evaluation took 1.001 seconds

;; Second call - cache hit (fast!)
(time (get-user-data 123))
;; Cache HIT for user 123
;; Evaluation took 0.001 seconds
```

**Result:** 1000x speedup on cache hit!

### Step 2: Choosing TTL Values

TTL (Time-To-Live) determines how long data stays in cache. Choose based on:

**Data volatility:**

```lisp
;; Static data - long TTL
(mc-set "country:US:name" "United States" :timeout 86400)  ; 24 hours

;; Semi-static data - medium TTL
(mc-set "user:123:profile" profile-data :timeout 3600)  ; 1 hour

;; Volatile data - short TTL
(mc-set "stock:AAPL:price" "150.25" :timeout 60)  ; 1 minute

;; Session data - until logout or expire
(mc-set "session:abc123" session-data :timeout 1800)  ; 30 minutes
```

**Rule of thumb:**
- Can tolerate stale data? → Longer TTL
- Data changes frequently? → Shorter TTL
- Critical accuracy? → Very short TTL or no cache

### Step 3: Handle Cache Failures Gracefully

Always have a fallback when cache is unavailable:

```lisp
(defun get-user-data-robust (user-id)
  "Get user data with cache fallback."
  (let ((cache-key (format nil "user:~A" user-id)))

    (handler-case
      ;; Try cache first
      (let ((cached (cl-memcached:mc-get-value cache-key)))
        (when cached
          (return-from get-user-data-robust cached)))

      ;; Cache unavailable - fall through to source
      (cl-memcached:memcached-server-unreachable (e)
        (format *error-output* "Cache error: ~A~%" e)))

    ;; Fetch from source (cache miss or cache unavailable)
    (let ((data (fetch-from-external-api user-id)))

      ;; Try to cache (best effort)
      (handler-case
        (cl-memcached:mc-set cache-key data :timeout 300)
        (cl-memcached:memcached-server-unreachable ()
          nil))  ; OK if cache write fails

      data)))
```

**What this does:**
- If cache is down, still fetch from source
- If cache write fails, don't fail the request
- Application stays available even if memcached crashes

### Step 4: Cache Invalidation

Update cache when source data changes:

**Pattern A: Explicit Invalidation**

```lisp
(defun update-user-profile (user-id new-profile)
  "Update user profile and invalidate cache."
  ;; Update in database
  (save-to-database user-id new-profile)

  ;; Invalidate cache
  (let ((cache-key (format nil "user:~A" user-id)))
    (cl-memcached:mc-del cache-key))

  (format t "Profile updated and cache invalidated~%"))
```

**Pattern B: Write-Through Cache**

```lisp
(defun update-user-profile-write-through (user-id new-profile)
  "Update user profile with write-through caching."
  ;; Update in database
  (save-to-database user-id new-profile)

  ;; Update cache immediately
  (let ((cache-key (format nil "user:~A" user-id)))
    (cl-memcached:mc-set cache-key new-profile :timeout 3600))

  (format t "Profile updated in both DB and cache~%"))
```

**Pattern C: TTL-Based Expiration**

```lisp
;; Just let TTL handle it - simplest approach
(defun get-product-info (product-id)
  (let ((cache-key (format nil "product:~A" product-id)))
    (or (cl-memcached:mc-get-value cache-key)
        (let ((data (fetch-product-from-db product-id)))
          (cl-memcached:mc-set cache-key data :timeout 600)  ; 10 minutes
          data))))
```

**When to use each:**
- **Explicit invalidation:** When you know exactly when data changes
- **Write-through:** For frequently read data that changes occasionally
- **TTL-based:** When exact freshness isn't critical

### Step 5: Batch Caching

Cache multiple items efficiently:

```lisp
(defun get-users-batch (user-ids)
  "Get multiple users, using cache when possible."
  (let* ((cache-keys (mapcar (lambda (id) (format nil "user:~A" id))
                             user-ids))
         (responses (cl-memcached:mc-get+ cache-keys))
         (cached-users (make-hash-table :test 'equal))
         (missing-ids nil))

    ;; Process cache hits
    (dolist (response responses)
      (when response
        (let* ((key (cl-memcached:mc-key response))
               (user-id (parse-integer (subseq key 5))))  ; Extract ID from "user:123"
          (setf (gethash user-id cached-users)
                (babel:octets-to-string (cl-memcached:mc-data response))))))

    ;; Find cache misses
    (dolist (id user-ids)
      (unless (gethash id cached-users)
        (push id missing-ids)))

    ;; Fetch missing users from database
    (when missing-ids
      (format t "Cache misses: ~A~%" missing-ids)
      (dolist (id missing-ids)
        (let ((data (fetch-from-external-api id)))
          (setf (gethash id cached-users) data)
          ;; Cache for next time
          (cl-memcached:mc-set (format nil "user:~A" id) data :timeout 300))))

    ;; Return all users
    (loop for id in user-ids
          collect (gethash id cached-users))))
```

**Test it:**

```lisp
(get-users-batch '(1 2 3))
;; Cache misses: (3 2 1)
;; => ("User 1" "User 2" "User 3")

(get-users-batch '(1 2 3))
;; => ("User 1" "User 2" "User 3")  [All from cache!]
```

## Real-World Example: HTTP Response Caching

Complete example for a web service:

```lisp
(defun cache-http-response (url ttl)
  "Fetch URL, caching the response."
  (let ((cache-key (format nil "http:~A" url)))

    ;; Check cache
    (let ((cached (cl-memcached:mc-get-value cache-key)))
      (when cached
        (format t "Serving ~A from cache~%" url)
        (return-from cache-http-response cached)))

    ;; Cache miss - fetch from network
    (format t "Fetching ~A from network~%" url)
    (let ((response (drakma:http-request url)))

      ;; Cache the response
      (cl-memcached:mc-set cache-key response :timeout ttl)

      response)))

;; Usage
(cache-http-response "https://api.example.com/data" 300)
```

## Decision Guide: To Cache or Not to Cache?

**✓ Good candidates for caching:**
- Database query results (especially expensive joins)
- External API responses
- Computed values (aggregations, statistics)
- Session data
- User profiles
- Product catalogs
- Configuration data

**✗ Poor candidates for caching:**
- Constantly changing data (real-time stock prices)
- User-specific data with low reuse (one-off queries)
- Data that must be perfectly consistent
- Very large objects (>1MB)
- Security-sensitive data requiring audit trails

## Performance Tips

1. **Use connection pooling:**
   ```lisp
   (setf cl-memcached:*mc-use-pool* t)
   ```

2. **Batch operations when possible:**
   ```lisp
   ;; Good - 1 request
   (mc-get+ (list "k1" "k2" "k3"))

   ;; Bad - 3 requests
   (mc-get+ (list "k1"))
   (mc-get+ (list "k2"))
   (mc-get+ (list "k3"))
   ```

3. **Choose appropriate TTL:**
   - Too short: Frequent cache misses
   - Too long: Stale data
   - Monitor hit/miss ratios and adjust

4. **Cache at the right level:**
   - Cache expensive operations, not cheap ones
   - Don't cache data that's already fast to retrieve

## Common Pitfalls

**Pitfall 1: Cache stampede**

Problem: TTL expires on popular item, many requests hit database simultaneously.

Solution: Use probabilistic early expiration or locking:

```lisp
(defun get-with-soft-ttl (key fetch-fn hard-ttl soft-ttl)
  "Refresh cache probabilistically before hard TTL."
  (let ((cached (mc-get-value key)))
    (if (and cached (< (random 1.0) 0.1))
        ;; 10% chance of refreshing even if cached
        (let ((fresh-data (funcall fetch-fn)))
          (mc-set key fresh-data :timeout hard-ttl)
          fresh-data)
        (or cached
            (let ((fresh-data (funcall fetch-fn)))
              (mc-set key fresh-data :timeout hard-ttl)
              fresh-data)))))
```

**Pitfall 2: Caching large objects**

Problem: Memcached limit is ~1MB per item.

Solution: Cache smaller pieces or use compression:

```lisp
;; Bad - cache entire 5MB user list
(mc-set "all-users" huge-list)

;; Good - cache individual users
(dolist (user huge-list)
  (mc-set (format nil "user:~A" (user-id user)) user))
```

**Pitfall 3: Ignoring cache errors**

Problem: Application fails when cache is down.

Solution: Always have fallback (see Step 3).

## Recap

You've learned:

✓ Cache-aside pattern implementation
✓ Choosing appropriate TTL values
✓ Handling cache failures gracefully
✓ Cache invalidation strategies
✓ Batch caching for efficiency
✓ When to cache and when not to

## What's Next

- [Counters How-To](../how-to/counters.md) - Atomic counters for rate limiting
- [CAS Operations](../how-to/cas-operations.md) - Safe concurrent updates
- [Pipelining](../how-to/pipelining.md) - Batch operations for speed
- [Performance Tuning](../performance.md) - Optimize cache performance

---

**Previous:** [Basics Tutorial](01-basics.md) | **Next:** [Counters How-To](../how-to/counters.md)
