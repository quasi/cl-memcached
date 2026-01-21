# Frequently Asked Questions

Quick answers to common questions about cl-memcached.

## General

### What is cl-memcached?

cl-memcached is a Common Lisp client library for memcached, a distributed memory caching system. It lets you store and retrieve data quickly across multiple application instances.

### Which Lisp implementations are supported?

- **SBCL** (Steel Bank Common Lisp) - Fully supported
- **CCL** (Clozure Common Lisp) - Fully supported
- **CMUCL** (Carnegie Mellon University Common Lisp) - Fully supported

Other implementations may work but are untested.

### What memcached versions are supported?

Any memcached 1.4+ is supported. Meta protocol features require modern memcached (1.5+).

### Is cl-memcached production-ready?

Yes. It's actively maintained, has comprehensive tests, and has been used in production since 2011.

## Installation

### How do I install cl-memcached?

Via Quicklisp:
```lisp
(ql:quickload :cl-memcached)
```

### What are the dependencies?

- **usocket** - Cross-platform sockets
- **babel** - String encoding/decoding
- **split-sequence** - String utilities
- **pooler** - Connection pooling

All automatically installed via Quicklisp.

### Do I need to install memcached separately?

Yes. cl-memcached is a *client* library. You need a memcached *server* running.

Install memcached:
```bash
# macOS
brew install memcached

# Ubuntu/Debian
sudo apt-get install memcached

# Start it
memcached -m 64  # 64MB memory
```

## Usage

### How do I connect to memcached?

```lisp
(defvar *cache*
  (cl-memcached:make-memcache :host "localhost" :port 11211))
```

Or use the global default:
```lisp
(setf cl-memcached:*memcache*
      (cl-memcached:make-memcache :host "localhost" :port 11211))
```

### What's the difference between mc-get and mc-get+?

- **mc-get**: Returns alist with key-value pairs (legacy API)
- **mc-get+**: Returns list of `memcache-response` objects (recommended)

Use `mc-get+` for new code.

### How do I store strings?

```lisp
(cl-memcached:mc-set "my-key" "my-value")
```

### How do I retrieve strings?

```lisp
(cl-memcached:mc-get-value "my-key")
;; => "my-value"
```

Or use `mc-get+` for full response:
```lisp
(let ((response (first (cl-memcached:mc-get+ (list "my-key")))))
  (babel:octets-to-string (cl-memcached:mc-data response)))
```

### How do I set expiration times?

Use the `:timeout` parameter (in seconds):
```lisp
(mc-set "session" "data" :timeout 3600)  ; Expires in 1 hour
```

`timeout=0` means never expire (default).

### How do I delete keys?

```lisp
(mc-del "my-key")
;; => "DELETED" or "NOT_FOUND"
```

### Can I cache binary data?

Yes, use `mc-store` with octets:
```lisp
(mc-store "image" binary-octets :command :set)
```

Retrieve as octets:
```lisp
(mc-data (first (mc-get+ (list "image"))))
```

## Performance

### How fast is cl-memcached?

**With connection pooling:** ~7,000 operations/second (SBCL)
**Without pooling:** ~1,000 operations/second (SBCL)

Always enable pooling for production:
```lisp
(setf cl-memcached:*mc-use-pool* t)
```

### What's the speedup from connection pooling?

Approximately **5-7x faster** on SBCL, ~3x on other implementations.

### What's the speedup from pipelining?

For N operations:
- **Without pipelining:** N round-trips
- **With pipelining:** ~2 round-trips
- **Speedup:** Up to N/2 (e.g., 50x for 100 operations)

### How do I enable pipelining?

Use meta protocol with `:stream` and `:quiet`:
```lisp
(mc-with-connection (s)
  (dotimes (i 100)
    (mc-meta-set (format nil "k~A" i) "v" :stream s :quiet t))
  (mc-meta-noop :stream s))
```

## Features

### Does cl-memcached support CAS (Check-And-Set)?

Yes. Use `mc-gets+` to get CAS token, then `mc-cas` to update:
```lisp
(let* ((response (first (mc-gets+ (list "key"))))
       (cas-token (mc-cas-unique response)))
  (mc-cas "key" "new-value" cas-token))
```

### Does it support atomic counters?

Yes. Use `mc-incr` and `mc-decr`:
```lisp
(mc-set "counter" "0")
(mc-incr "counter")  ; => 1
(mc-incr "counter" 5)  ; => 6
(mc-decr "counter" 2)  ; => 4
```

### What's the difference between TEXT and META protocols?

| Feature | TEXT Protocol | META Protocol |
|---------|---------------|---------------|
| Compatibility | All memcached versions | Modern (1.5+) |
| Pipelining | No | Yes |
| Advanced flags | No | Yes (stampede protection, stale data) |
| Functions | `mc-set`, `mc-get`, etc. | `mc-meta-set`, `mc-meta-get`, etc. |

Use META for new code if you have modern memcached.

### Can I use both protocols?

Yes. They work together seamlessly:
```lisp
(mc-set "key" "value")  ; TEXT protocol
(mc-meta-get "key")      ; META protocol - works!
```

## Data

### What's the maximum key size?

**250 characters** (memcached limit).

### What's the maximum value size?

**~1MB** (default memcached limit, configurable).

For larger data, split into chunks or use a database.

### Do strings require encoding/decoding?

**High-level functions:** No, handled automatically (UTF-8 default)
```lisp
(mc-set "key" "Hello, 世界!")  ; Automatic encoding
(mc-get-value "key")            ; Automatic decoding
```

**Low-level functions:** Yes, manual conversion required
```lisp
(mc-store "key" (babel:string-to-octets "data") :command :set)
(babel:octets-to-string (mc-data response))
```

### Can I cache Lisp objects?

Not directly. Serialize first:
```lisp
;; JSON (recommended)
(mc-set "key" (json:encode-json-to-string my-object))

;; Lisp format
(mc-set "key" (write-to-string my-object))

;; Marshal (if available)
(mc-set "key" (marshal:marshal my-object))
```

### Does memcached persist data across restarts?

**No.** Memcached is in-memory only. Data is lost on restart.

Use a database for persistence, memcached for caching.

## Errors

### What does "MEMCACHED-SERVER-UNREACHABLE" mean?

Can't connect to memcached server. Check:
1. Memcached is running: `echo "stats" | nc localhost 11211`
2. Host/port are correct
3. Firewall isn't blocking

### What does "KEYS-LIST has to be a LIST of keys" mean?

You passed a string instead of a list:
```lisp
;; Wrong
(mc-get+ "key")

;; Right
(mc-get+ (list "key"))
```

### Why am I getting NIL when retrieving data?

Key doesn't exist. Common reasons:
- Never stored
- Expired (TTL reached)
- Evicted (memory pressure)
- Typo in key name (case-sensitive)

This is **normal**, not an error.

## Troubleshooting

### My cache hit ratio is low. How do I improve it?

1. Increase TTL values
2. Increase memcached memory
3. Review what you're caching
4. Check for evictions: `(cdr (assoc :evictions (mc-stats)))`

### How do I debug connection issues?

Test manually:
```bash
echo "stats" | nc localhost 11211
```

If that works, check your host/port settings in Lisp.

### Performance is slow despite pooling. What's wrong?

1. Verify pooling is enabled: `cl-memcached:*mc-use-pool*`
2. Use batch GET operations for multiple keys
3. Use pipelining for batch writes (meta protocol)
4. Check network latency to memcached server

## Best Practices

### Should I use connection pooling?

**Yes, always** in production:
```lisp
(setf cl-memcached:*mc-use-pool* t)
```

5-7x performance improvement.

### Should I use TEXT or META protocol?

- **TEXT:** Compatibility with all memcached versions
- **META:** Better performance, modern features

Use META if you have modern memcached and need pipelining.

### How should I handle cache failures?

Always provide fallback:
```lisp
(defun get-data (id)
  (or (handler-case
        (mc-get-value (format nil "data:~A" id))
        (error () nil))
      (fetch-from-database id)))
```

Never let cache failures crash your application.

### What should I cache?

**Good candidates:**
- Database query results
- External API responses
- Expensive computations
- Session data
- User profiles

**Poor candidates:**
- Constantly changing data
- Data requiring perfect consistency
- Very large objects (>1MB)
- Security-sensitive data

### What TTL should I use?

Depends on data volatility:
- **Static data:** Hours to days (86400 = 24 hours)
- **Semi-static:** Minutes to hours (3600 = 1 hour)
- **Volatile:** Seconds to minutes (60 = 1 minute)
- **Sessions:** Until logout (1800 = 30 minutes)

## Development

### How do I run the tests?

```lisp
(asdf:test-system :cl-memcached)
```

Requires memcached running on localhost:11211.

### Is cl-memcached thread-safe?

Yes. Connection pooling is thread-safe.

### Can I contribute?

Yes! File issues or pull requests at:
https://github.com/quasi/cl-memcached

### What license is cl-memcached?

MIT License (permissive, commercial-friendly).

## See Also

- [Quickstart](quickstart.md) - Get started in 5 minutes
- [Tutorials](tutorials/) - Learn by example
- [Troubleshooting](troubleshooting.md) - Fix common problems
- [API Reference](reference/api-reference.md) - Complete API

---

**Still have questions?** File an issue: https://github.com/quasi/cl-memcached/issues
