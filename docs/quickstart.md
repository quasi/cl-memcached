# Quickstart: Get Running in 5 Minutes

**Goal**: Store and retrieve data from memcached using cl-memcached.

## Prerequisites

- Memcached running locally on port 11211
- Common Lisp installed (SBCL, CCL, or CMUCL)
- Quicklisp available

## Step 1: Verify Memcached is Running

Check that memcached is accessible:

```bash
echo "stats" | nc localhost 11211
```

Expected output starts with `STAT pid`:

```
STAT pid 12345
STAT uptime 3600
...
```

If this fails:
- **macOS**: `brew install memcached && brew services start memcached`
- **Linux**: `sudo apt install memcached && sudo systemctl start memcached`
- See [Troubleshooting](troubleshooting.md) for more

## Step 2: Load the Library

In your REPL:

```lisp
(ql:quickload :cl-memcached)
```

Expected output: `To load "cl-memcached": ...` (loading happens, no errors)

## Step 3: Connect

```lisp
(defvar *mc* (cl-memcached:make-memcache
               :host "localhost"
               :port 11211))
```

Expected: Variable `*MC*` is created. No output means success.

Verify the connection:

```lisp
(cl-memcached:mc-version :memcache *mc*)
```

Expected output: `"1.6.0"` (or your memcached version)

If connection fails: Jump to [Troubleshooting](troubleshooting.md#cant-connect)

## Step 4: Store Data

```lisp
(cl-memcached:mc-set "greeting" "Hello, Memcached!" :memcache *mc*)
```

Expected: `"STORED"` returned. Your data is now in memcached.

## Step 5: Retrieve Data

```lisp
(cl-memcached:mc-get+ (list "greeting") :memcache *mc*)
```

Expected output (pretty-printed):

```
(#<MEMCACHE-RESPONSE
   :KEY "greeting"
   :DATA #(72 101 108 108 111 44 32 77 101 109 99 97 99 104 101 100 33)>)
```

The `:DATA` is raw bytes. Convert it to a string:

```lisp
(babel:octets-to-string
  (cl-memcached:mc-data (first (cl-memcached:mc-get+ (list "greeting") :memcache *mc*))))
```

Expected: `"Hello, Memcached!"`

**It works!** 🎉

## It's That Simple

You just:
1. Connected to memcached
2. Stored a value
3. Retrieved it back

## Next Steps

**Learn more patterns:**
- [Core Concepts](concepts/core-concepts.md) — What memcached actually does
- [Tutorial: Caching](tutorials/02-caching.md) — Real-world usage
- [API Reference](reference/api-reference.md) — All functions at a glance

**Solve common problems:**
- [How to use counters](how-to/counters.md)
- [How to handle errors](how-to/error-handling.md)
- [How to batch operations](how-to/pipelining.md)

**Optimize performance:**
- [Enable connection pooling](how-to/connection-pooling.md) (5-7x faster!)
- [Performance Tuning Guide](performance.md)

## Common First Steps

### Storing Different Data Types

Strings (what we did):
```lisp
(cl-memcached:mc-set "key" "string value" :memcache *mc*)
```

JSON (serialize first):
```lisp
(cl-memcached:mc-set "user:1"
  (json:encode-json-to-string (list :name "Alice" :age 30))
  :memcache *mc*)
```

Binary data:
```lisp
(cl-memcached:mc-store "binary-key"
  (your-data-to-octets)
  :memcache *mc*)
```

See [Data Types](concepts/data-types.md) for details.

### Setting Expiration

Data expires automatically after 60 seconds:

```lisp
(cl-memcached:mc-set "temporary" "will expire"
  :timeout 60
  :memcache *mc*)
```

### Checking if a Key Exists

```lisp
(cl-memcached:mc-get+ (list "greeting") :memcache *mc*)
```

If the result is `NIL` or empty, the key doesn't exist.

## Troubleshooting This Guide

**"Package CL-MEMCACHED not found"**
- Run `(ql:quickload :cl-memcached)` first

**"Connection refused"**
- Memcached isn't running. See Step 1.

**"Got weird bytes instead of text"**
- Data in memcached is binary. Use `(babel:octets-to-string data)` to convert.

**Want more help?** → [Troubleshooting Guide](troubleshooting.md)

---

**You're ready!** Explore [How-To Guides](how-to/) or read [Core Concepts](concepts/core-concepts.md) to understand memcached better.
