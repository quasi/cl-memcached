# API Reference

Complete documentation of all cl-memcached functions. Look up signatures, parameters, and return values.

## Connection Management

### make-memcache
**Returns**: A memcache connection object

```lisp
(make-memcache &key (host "localhost") (port 11211) (pool-size 5))
```

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `host` | string | "localhost" | Memcached server hostname or IP |
| `port` | integer | 11211 | Memcached server port |
| `pool-size` | integer | 5 | Connection pool size (when pooling enabled) |
| **Returns** | memcache | | Connection object |

**Example:**
```lisp
(setf mc (make-memcache :host "cache.example.com" :port 11211))
```

**See also**: Global variable `*memcache*` (default instance)

---

## Storage Operations

### mc-set
Store data, creating or overwriting key.

```lisp
(mc-set key data &key (timeout 0) (memcache *memcache*) (mc-use-pool *mc-use-pool*))
```

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `key` | string | required | Key identifier |
| `data` | string \| octets | required | Data to store |
| `timeout` | integer | 0 | Seconds until expiration (0 = never) |
| `memcache` | memcache | *memcache* | Connection to use |
| `mc-use-pool` | boolean | *mc-use-pool* | Use connection pool |
| **Returns** | string | | "STORED" or error |

**Example:**
```lisp
(mc-set "user:123" "John Doe" :timeout 3600)  ; Expires in 1 hour
```

---

### mc-add
Store data only if key doesn't exist.

```lisp
(mc-add key data &key (timeout 0) (memcache *memcache*) (mc-use-pool *mc-use-pool*))
```

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `key` | string | required | Key identifier |
| `data` | string \| octets | required | Data to store |
| `timeout` | integer | 0 | Seconds until expiration (0 = never) |
| `memcache` | memcache | *memcache* | Connection to use |
| `mc-use-pool` | boolean | *mc-use-pool* | Use connection pool |
| **Returns** | string | | "STORED" if added, "NOT_STORED" if exists |

**Example:**
```lisp
(mc-add "session:token123" "session-data")
;; Returns "STORED" if key didn't exist, "NOT_STORED" if it did
```

---

### mc-replace
Update data only if key exists.

```lisp
(mc-replace key data &key (timeout 0) (memcache *memcache*) (mc-use-pool *mc-use-pool*))
```

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `key` | string | required | Key identifier |
| `data` | string \| octets | required | Data to store |
| `timeout` | integer | 0 | Seconds until expiration (0 = never) |
| `memcache` | memcache | *memcache* | Connection to use |
| `mc-use-pool` | boolean | *mc-use-pool* | Use connection pool |
| **Returns** | string | | "STORED" if replaced, "NOT_STORED" if key doesn't exist |

**Example:**
```lisp
(mc-replace "user:123" "Jane Doe")
;; Only works if "user:123" was previously set
```

---

### mc-append / mc-prepend
Add data to existing value (append at end, prepend at start).

```lisp
(mc-append key data &key (memcache *memcache*) (mc-use-pool *mc-use-pool*))
(mc-prepend key data &key (memcache *memcache*) (mc-use-pool *mc-use-pool*))
```

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `key` | string | required | Key identifier |
| `data` | string \| octets | required | Data to append/prepend |
| `memcache` | memcache | *memcache* | Connection to use |
| `mc-use-pool` | boolean | *mc-use-pool* | Use connection pool |
| **Returns** | string | | "STORED" or "NOT_FOUND" |

**Example:**
```lisp
(mc-set "log" "Started: 10:00 ")
(mc-append "log" "Finished: 10:05")
;; Now "log" contains "Started: 10:00 Finished: 10:05"
```

---

### mc-cas
Compare-And-Set: Update only if value unchanged (atomic, prevents race conditions).

```lisp
(mc-cas key data cas-unique &key (timeout 0) (memcache *memcache*) (mc-use-pool *mc-use-pool*))
```

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `key` | string | required | Key identifier |
| `data` | string \| octets | required | Data to store |
| `cas-unique` | string | required | CAS token from previous gets |
| `timeout` | integer | 0 | Seconds until expiration (0 = never) |
| `memcache` | memcache | *memcache* | Connection to use |
| `mc-use-pool` | boolean | *mc-use-pool* | Use connection pool |
| **Returns** | string | | "STORED", "EXISTS" (modified), or "NOT_FOUND" |

**Example:**
```lisp
;; Get current value and CAS token
(let* ((response (first (mc-gets+ (list "counter")))))
  (when response
    ;; Try to update only if value hasn't changed
    (mc-cas "counter" "new-value" (mc-cas-unique response))))
```

See [CAS Operations How-To](../how-to/cas-operations.md) for detailed patterns.

---

## Retrieval Operations

### mc-get
Get values as raw list (low-level).

```lisp
(mc-get keys-list &key (memcache *memcache*) (mc-use-pool *mc-use-pool*))
```

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `keys-list` | list of strings | required | Keys to retrieve |
| `memcache` | memcache | *memcache* | Connection to use |
| `mc-use-pool` | boolean | *mc-use-pool* | Use connection pool |
| **Returns** | list of alist | | Raw protocol response |

**Example:**
```lisp
(mc-get (list "key1" "key2"))
;; Returns: (("key1" "data1") ("key2" "data2"))
```

**Use `mc-get+` instead for cleaner response objects.**

---

### mc-get+
Get values as memcache-response objects (recommended).

```lisp
(mc-get+ keys-list &key (memcache *memcache*) (mc-use-pool *mc-use-pool*))
```

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `keys-list` | list of strings | required | Keys to retrieve |
| `memcache` | memcache | *memcache* | Connection to use |
| `mc-use-pool` | boolean | *mc-use-pool* | Use connection pool |
| **Returns** | list of memcache-response | | Response objects |

**Example:**
```lisp
(let ((responses (mc-get+ (list "user:1" "user:2"))))
  (dolist (resp responses)
    (format t "~A = ~A~%"
      (mc-key resp)
      (babel:octets-to-string (mc-data resp)))))
```

---

### mc-gets / mc-gets+
Get with CAS tokens (for safe concurrent updates).

```lisp
(mc-gets keys-list &key (memcache *memcache*) (mc-use-pool *mc-use-pool*))
(mc-gets+ keys-list &key (memcache *memcache*) (mc-use-pool *mc-use-pool*))
```

Same as `mc-get` / `mc-get+`, but includes `mc-cas-unique` field for CAS operations.

**Example:**
```lisp
(let ((response (first (mc-gets+ (list "counter")))))
  (format t "CAS token: ~A~%" (mc-cas-unique response)))
```

See [CAS Operations How-To](../how-to/cas-operations.md) for complete patterns.

---

### mc-get-value
Convenience wrapper: Get single value as string.

```lisp
(mc-get-value key &key (memcache *memcache*) (mc-use-pool *mc-use-pool*))
```

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `key` | string | required | Key to retrieve |
| `memcache` | memcache | *memcache* | Connection to use |
| `mc-use-pool` | boolean | *mc-use-pool* | Use connection pool |
| **Returns** | string \| nil | | Value or NIL if not found |

**Example:**
```lisp
(mc-get-value "user:123")
;; Returns: "John Doe" (or NIL if not found)
```

---

### mc-data
Extract data from memcache-response object.

```lisp
(mc-data response)
```

Returns raw octets. Convert to string with:

```lisp
(babel:octets-to-string (mc-data response))
```

---

## Counter Operations

### mc-incr
Increment counter atomically.

```lisp
(mc-incr key &optional (amount 1) &key (memcache *memcache*) (mc-use-pool *mc-use-pool*))
```

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `key` | string | required | Counter key |
| `amount` | integer | 1 | Amount to add |
| `memcache` | memcache | *memcache* | Connection to use |
| `mc-use-pool` | boolean | *mc-use-pool* | Use connection pool |
| **Returns** | integer | | New counter value |

**Example:**
```lisp
(mc-set "page-views" "0")
(mc-incr "page-views")      ; Returns: 1
(mc-incr "page-views" 10)   ; Returns: 11
```

---

### mc-decr
Decrement counter atomically.

```lisp
(mc-decr key &optional (amount 1) &key (memcache *memcache*) (mc-use-pool *mc-use-pool*))
```

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `key` | string | required | Counter key |
| `amount` | integer | 1 | Amount to subtract |
| `memcache` | memcache | *memcache* | Connection to use |
| `mc-use-pool` | boolean | *mc-use-pool* | Use connection pool |
| **Returns** | integer | | New counter value (never negative, clamped at 0) |

**Example:**
```lisp
(mc-set "credits" "100")
(mc-decr "credits" 25)     ; Returns: 75
(mc-decr "credits" 100)    ; Returns: 0 (not negative)
```

---

## Deletion and Maintenance

### mc-del
Delete a key.

```lisp
(mc-del key &key (memcache *memcache*) (mc-use-pool *mc-use-pool*))
```

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `key` | string | required | Key to delete |
| `memcache` | memcache | *memcache* | Connection to use |
| `mc-use-pool` | boolean | *mc-use-pool* | Use connection pool |
| **Returns** | string | | "DELETED" or "NOT_FOUND" |

**Example:**
```lisp
(mc-del "session:abc123")
```

---

### mc-touch
Update expiration time without modifying data.

```lisp
(mc-touch key expiry-time &key (memcache *memcache*) (mc-use-pool *mc-use-pool*))
```

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `key` | string | required | Key to touch |
| `expiry-time` | integer | required | New expiration in seconds |
| `memcache` | memcache | *memcache* | Connection to use |
| `mc-use-pool` | boolean | *mc-use-pool* | Use connection pool |
| **Returns** | string | | "TOUCHED" or "NOT_FOUND" |

**Example:**
```lisp
;; Keep session alive for another hour
(mc-touch "session:token" 3600)
```

---

### mc-flush-all
Delete all keys from memcached.

```lisp
(mc-flush-all &key (delay 0) (memcache *memcache*) (mc-use-pool *mc-use-pool*))
```

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `delay` | integer | 0 | Seconds before flush executes (0 = immediate) |
| `memcache` | memcache | *memcache* | Connection to use |
| `mc-use-pool` | boolean | *mc-use-pool* | Use connection pool |
| **Returns** | string | | "OK" |

**Example:**
```lisp
(mc-flush-all)  ; Immediate
(mc-flush-all :delay 60)  ; Wait 60 seconds, then flush
```

---

## Statistics and Monitoring

### mc-stats
Get all statistics from memcached.

```lisp
(mc-stats &key (memcache *memcache*) (mc-use-pool *mc-use-pool*))
```

Returns: alist of `("stat-name" . "value")`

**Example:**
```lisp
(mc-stats)
;; Returns: (("pid" . "1234") ("uptime" . "86400") ("bytes" . "512000") ...)
```

---

### mc-stats-items
Per-slab-class item statistics.

```lisp
(mc-stats-items &key (memcache *memcache*) (mc-use-pool *mc-use-pool*))
```

Returns: alist with per-slab stats like `("items:1:number" . "42")` (42 items in slab 1)

---

### mc-stats-slabs
Slab allocation statistics.

```lisp
(mc-stats-slabs &key (memcache *memcache*) (mc-use-pool *mc-use-pool*))
```

Returns: alist with slab stats like `("slabs:1:chunk_size" . "96")` bytes per chunk

---

### mc-stats-sizes
Item size distribution.

```lisp
(mc-stats-sizes &key (memcache *memcache*) (mc-use-pool *mc-use-pool*))
```

Returns: alist mapping item sizes to counts like `("96" . "100")` (100 items of 96 bytes)

---

### mc-version
Get memcached server version.

```lisp
(mc-version &key (memcache *memcache*) (mc-use-pool *mc-use-pool*))
```

Returns: version string like `"1.6.0"`

---

## Global Variables

### \*memcache\*
Default memcache connection (created automatically).

```lisp
(setf cl-memcached:*memcache*
  (make-memcache :host "new-cache.example.com"))
```

---

### \*mc-use-pool\*
Enable/disable connection pooling globally.

```lisp
(setf cl-memcached:*mc-use-pool* t)  ; Enable pooling (5-7x faster)
```

---

### \*mc-default-encoding\*
Default encoding for strings (default: UTF-8).

```lisp
(setf cl-memcached:*mc-default-encoding* :latin1)
```

---

## Meta Protocol (Modern)

For pipelining and advanced features, use the meta protocol:

### mc-meta-set, mc-meta-get, mc-meta-delete
Modern protocol versions of set/get/del.

```lisp
(mc-meta-set key data &key (ttl 0) (stream nil) (quiet nil) ...)
(mc-meta-get key &key (stream nil) (quiet nil) ...)
(mc-meta-delete key &key (stream nil) (quiet nil) ...)
```

**Example with pipelining:**
```lisp
(mc-with-connection (s)
  (mc-meta-set "key1" "data1" :stream s :quiet t)
  (mc-meta-set "key2" "data2" :stream s :quiet t)
  (mc-meta-noop :stream s))
```

See [Pipelining How-To](../how-to/pipelining.md) for complete patterns.

---

## Error Handling

All functions can raise exceptions:

| Exception | When | Handling |
|-----------|------|----------|
| `MEMCACHED-SERVER-UNREACHABLE` | Server down or unreachable | Connection retry, fallback |
| `CL-MC-ERROR` | Invalid input (wrong types) | Fix input validation |

See [Error Handling How-To](../how-to/error-handling.md) for patterns.

---

## See Also

- [Quickstart](../quickstart.md) — Get running in 5 minutes
- [How-To Guides](../how-to/) — Solve specific problems
- [Core Concepts](../concepts/core-concepts.md) — Understand memcached
