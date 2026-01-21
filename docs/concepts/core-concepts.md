# Core Concepts

Before writing code, understand what memcached does and how cl-memcached works.

## What is Memcached?

Memcached is a distributed **in-memory cache**. Think of it as a giant hash table on a server:

```
Key → Value
"user:123" → "John Doe"
"session:abc" → "{ isLoggedIn: true }"
"counter:page-views" → "5432"
```

**Fast**: Data is in RAM, not on disk
**Volatile**: If the server restarts, data is gone
**Simple**: No complex queries, just key-value storage
**Distributed**: Multiple servers, single logical cache

## Why Use Memcached?

Your application is slow because:
1. Database queries are expensive
2. Computing results takes time
3. External APIs are unreliable

Memcached speeds things up by **storing intermediate results**:

```
User requests → Check memcached (fast miss) → Query database → Store in memcached → Return
User requests → Check memcached (fast hit) → Return immediately
```

**Common uses:**
- Cache database query results
- Store user sessions
- Count page views, API calls, errors
- Prevent duplicate work in concurrent requests

## Keys and Values

### Keys
- **Text identifiers**: `"user:123"`, `"products:coffee"`, `"session:token-abc"`
- **Max 250 characters**: Longer keys are rejected
- **Must be unique**: One key = one value
- **Case-sensitive**: `"USER:123"` ≠ `"user:123"`

Best practice: Use structured naming like `"resource:id"` to avoid collisions.

### Values
- **Binary data**: Numbers, strings, JSON, serialized objects, anything
- **Limit ~1MB**: Individual values capped (configurable, rarely changed)
- **Opaque to memcached**: Memcached doesn't know what's inside, doesn't parse it

In cl-memcached, values are **byte arrays** internally:

```lisp
;; String "hello" becomes byte array #(104 101 108 108 111)
(babel:string-to-octets "hello")
;; Conversion back to string
(babel:octets-to-string #(104 101 108 108 111))
```

**cl-memcached handles this conversion automatically** when you use `mc-set` (not `mc-store`).

## Operations: Store, Get, Delete

### Store (mc-set)
Save data to memcached. Overwrites if key exists.

```lisp
(cl-memcached:mc-set "user:123" "John Doe")
;; Returns: "STORED"
```

### Retrieve (mc-get)
Get data back. Returns `NIL` if key doesn't exist.

```lisp
(cl-memcached:mc-get+ (list "user:123"))
;; Returns: (#<MEMCACHE-RESPONSE key="user:123" data="...">)
;;          or NIL if not found
```

### Delete (mc-del)
Remove data from memcached.

```lisp
(cl-memcached:mc-del "user:123")
;; Returns: "DELETED" or "NOT_FOUND"
```

## Expiration (TTL)

Data doesn't stay forever. You set a **Time To Live**:

```lisp
;; Data expires after 60 seconds
(cl-memcached:mc-set "session:token" "abc123" :timeout 60)

;; After 60 seconds, trying to get it returns NIL
(cl-memcached:mc-get+ (list "session:token"))
;; Returns: NIL
```

**Common TTLs:**
- User sessions: 30 minutes (1800 seconds)
- Database query results: 5 minutes (300 seconds)
- Page HTML: 1 hour (3600 seconds)
- Temporary data: 10 seconds (10)

**Special case**: TTL = 0 means "never expire" (stored forever, or until manual delete/server restart).

## Atomic Counters

Increment/decrement numbers without race conditions:

```lisp
;; Initialize a counter
(cl-memcached:mc-set "page-views" "0")

;; Increment (safe across concurrent requests)
(cl-memcached:mc-incr "page-views")
;; Returns: 1

(cl-memcached:mc-incr "page-views")
;; Returns: 2

;; Decrement
(cl-memcached:mc-decr "page-views")
;; Returns: 1

;; Decrement by more than 1
(cl-memcached:mc-decr "page-views" 5)
;; Returns: -4, but clamped to 0 (counters can't be negative)
```

**Why use counters?**
- Database updates are slow
- Multiple threads/processes might count simultaneously
- Memcached guarantees atomicity (updates are thread-safe)

**Constraints:**
- Counter must be initialized with `mc-set` first
- Can't go below 0 (clamped automatically)
- Values are 64-bit unsigned integers

## Batch Operations

Instead of multiple round-trips:

```lisp
;; Old way: 3 network requests
(mc-get+ (list "key1"))
(mc-get+ (list "key2"))
(mc-get+ (list "key3"))
```

Get all at once:

```lisp
;; New way: 1 network request
(mc-get+ (list "key1" "key2" "key3"))
```

Even better with pipelining (see [Pipelining How-To](../how-to/pipelining.md)):

```lisp
(cl-memcached:mc-with-connection (s)
  (cl-memcached:mc-meta-set "key1" "data1" :stream s :quiet t)
  (cl-memcached:mc-meta-set "key2" "data2" :stream s :quiet t)
  (cl-memcached:mc-meta-noop :stream s))
;; Multiple operations, 2 network round-trips (instead of N)
```

## Connection Pooling

Creating a connection is expensive. **Pooling reuses connections** for 5-7x speedup:

```lisp
;; Enable pooling (default disabled)
(setf cl-memcached:*mc-use-pool* t)

;; Now all operations reuse a connection pool automatically
(cl-memcached:mc-set "key" "value")
```

Under the hood:
- First call: Create pool, make connections, store in memory
- Subsequent calls: Borrow connection from pool, use it, return it
- Same correctness, much faster

See [Connection Pooling How-To](../how-to/connection-pooling.md) for details.

## Global Memcache Instance

By default, cl-memcached uses a **global memcache instance**:

```lisp
;; Don't need to pass memcache instance each time
(cl-memcached:mc-set "key" "value")
;; Uses cl-memcached:*memcache* (created automatically)

;; Change default server
(setf cl-memcached:*memcache*
  (cl-memcached:make-memcache :host "cache.example.com" :port 11211))

;; Or pass it explicitly to any function
(cl-memcached:mc-set "key" "value" :memcache my-custom-connection)
```

## Encoding: UTF-8 by Default

cl-memcached assumes **UTF-8 encoding** for strings:

```lisp
;; String is automatically encoded as UTF-8 bytes
(cl-memcached:mc-set "greeting" "Hello, 世界")

;; Retrieved back as UTF-8 bytes, decoded to string
(babel:octets-to-string
  (cl-memcached:mc-data
    (first (cl-memcached:mc-get+ (list "greeting")))))
;; Returns: "Hello, 世界"
```

Override the encoding if needed:

```lisp
;; Store with different encoding
(cl-memcached:mc-store "key"
  (babel:string-to-octets "data" :encoding :latin1)
  :encoding :latin1)
```

## Data Consistency: No Transactions

Memcached has **no transactions**. If multiple clients update the same key simultaneously:

```
Time 1: Client A reads counter (value: 5)
Time 2: Client B reads counter (value: 5)
Time 3: Client A increments, writes back (value: 6)
Time 4: Client B increments, writes back (value: 6)  ← Lost update!
```

**Solutions:**
1. Use atomic counters (`mc-incr`, `mc-decr`) for simple increments
2. Use **CAS (Check-And-Set)** for complex updates

See [CAS Operations How-To](../how-to/cas-operations.md) for preventing race conditions.

## Two Protocols: TEXT and META

cl-memcached supports both:

### TEXT Protocol (Classic)
```lisp
(cl-memcached:mc-set "key" "value")
(cl-memcached:mc-get+ (list "key"))
```

Older, widely compatible, simpler.

### META Protocol (Modern)
```lisp
(cl-memcached:mc-meta-set "key" "value")
(cl-memcached:mc-meta-get "key")
```

Newer, more features (pipelining, advanced caching semantics), slightly different response format.

**Use META protocol for:**
- Batch operations (pipelining)
- Advanced features (recache-on-miss, stale data serving)
- New applications

**Use TEXT protocol for:**
- Compatibility with older servers
- Simpler code
- Legacy integration

See [Architecture](../architecture.md) for protocol details.

## Error Handling

Operations can fail:

```lisp
;; Server unreachable
(cl-memcached:mc-set "key" "value")
;; Raises: MEMCACHED-SERVER-UNREACHABLE

;; Key doesn't exist (not an error, just returns NIL)
(cl-memcached:mc-get+ (list "nonexistent"))
;; Returns: NIL

;; Wrong type (not a list)
(cl-memcached:mc-get+ "not-a-list")
;; Raises: CL-MC-ERROR
```

Handle errors gracefully:

```lisp
(handler-case
  (cl-memcached:mc-set "key" "value")
  (cl-memcached:memcached-server-unreachable ()
    (format t "Cache unavailable, using database instead~%")))
```

See [Error Handling How-To](../how-to/error-handling.md) for patterns.

## Summary

| Concept | What | Why |
|---------|------|-----|
| **Key-Value Store** | Text key → binary value | Simple, fast lookup |
| **Expiration (TTL)** | Data expires after timeout | Prevent stale data |
| **Atomic Counters** | Thread-safe increment/decrement | Accurate counts without locks |
| **Batch Operations** | Get multiple keys at once | Fewer network round-trips |
| **Connection Pooling** | Reuse connections | 5-7x speedup |
| **CAS** | Update only if unchanged | Prevent race conditions |
| **Two Protocols** | TEXT and META | Choose based on needs |

---

**Next:** [Quickstart](../quickstart.md) to write your first code, or pick a [Tutorial](../tutorials/).
