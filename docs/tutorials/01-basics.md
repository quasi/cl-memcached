# Tutorial: Getting Started with cl-memcached

<!-- Generated from: canon/features/*/scenarios/*.md -->

Learn the basics of using cl-memcached by working through practical examples. This tutorial covers installation, connection setup, basic operations, and data retrieval.

## What You'll Learn

- Installing and loading cl-memcached
- Creating and managing connections
- Storing and retrieving data
- Working with expiration times
- Understanding data encoding

## Prerequisites

- Common Lisp implementation (SBCL, CCL, or CMUCL)
- Quicklisp installed
- Memcached server running (locally or accessible)

**Check if memcached is running:**

```bash
echo "stats" | nc localhost 11211
```

If you see stats output, memcached is ready. If not, install and start it:

```bash
# macOS
brew install memcached
brew services start memcached

# Ubuntu/Debian
sudo apt-get install memcached
sudo systemctl start memcached
```

## Step 1: Install cl-memcached

Load cl-memcached via Quicklisp:

```lisp
(ql:quickload :cl-memcached)
```

You should see output indicating successful loading of dependencies (usocket, babel, split-sequence, pooler).

## Step 2: Create a Connection

Create a connection to your memcached server:

```lisp
(defvar *cache*
  (cl-memcached:make-memcache :host "localhost" :port 11211))
```

**What this does:**
- Creates a memcache connection object
- Default host is "127.0.0.1", default port is 11211
- Connection is lazy - it won't actually connect until first use

**Optional - Enable connection pooling for better performance:**

```lisp
(setf cl-memcached:*mc-use-pool* t)
```

This provides a 5-7x speedup for repeated operations.

## Step 3: Store Your First Value

Store a simple string value:

```lisp
(cl-memcached:mc-set "greeting" "Hello, World!" :memcache *cache*)
```

**Expected output:**
```lisp
"STORED"
```

The `"STORED"` response confirms the data was saved successfully.

**What happened:**
- Key: `"greeting"`
- Value: `"Hello, World!"` (automatically converted to binary)
- Expiration: No expiration (default is 0 = never expire)

## Step 4: Retrieve the Value

Retrieve your stored data:

```lisp
(let* ((responses (cl-memcached:mc-get+ (list "greeting") :memcache *cache*))
       (response (first responses)))
  (when response
    (format t "Got: ~A~%"
      (babel:octets-to-string (cl-memcached:mc-data response)))))
```

**Expected output:**
```
Got: Hello, World!
T
```

**Why the complexity?**
- `mc-get+` returns a list of response objects (to support batch retrieval)
- Data is stored as octets (binary), so we decode with `babel:octets-to-string`
- Always check if response is non-nil (key might not exist)

**Simpler alternative** - Use the convenience function:

```lisp
(cl-memcached:mc-get-value "greeting" :memcache *cache*)
```

**Expected output:**
```lisp
"Hello, World!"
```

This handles the decoding automatically.

## Step 5: Store with Expiration

Store data that automatically expires:

```lisp
;; Store for 60 seconds
(cl-memcached:mc-set "session-token"
                     "abc123xyz"
                     :timeout 60
                     :memcache *cache*)
```

**Expected output:**
```lisp
"STORED"
```

After 60 seconds, the key will automatically be deleted by memcached.

**Verify expiration:**

```lisp
;; Immediately after storing
(cl-memcached:mc-get-value "session-token" :memcache *cache*)
;; => "abc123xyz"

;; Wait 61 seconds, then try again
(sleep 61)
(cl-memcached:mc-get-value "session-token" :memcache *cache*)
;; => NIL
```

## Step 6: Store Multiple Values

Store several values at once:

```lisp
(cl-memcached:mc-set "user:1:name" "Alice" :memcache *cache*)
(cl-memcached:mc-set "user:1:email" "alice@example.com" :memcache *cache*)
(cl-memcached:mc-set "user:1:age" "30" :memcache *cache*)
```

Retrieve them in a single batch operation:

```lisp
(let ((responses (cl-memcached:mc-get+
                   (list "user:1:name" "user:1:email" "user:1:age")
                   :memcache *cache*)))
  (loop for response in responses
        do (format t "~A: ~A~%"
                  (cl-memcached:mc-key response)
                  (babel:octets-to-string (cl-memcached:mc-data response)))))
```

**Expected output:**
```
user:1:name: Alice
user:1:email: alice@example.com
user:1:age: 30
```

**Performance tip:** Batch GET operations are more efficient than individual requests.

## Step 7: Delete Data

Remove a key from the cache:

```lisp
(cl-memcached:mc-del "greeting" :memcache *cache*)
```

**Expected output:**
```lisp
"DELETED"
```

Try to retrieve it:

```lisp
(cl-memcached:mc-get-value "greeting" :memcache *cache*)
```

**Expected output:**
```lisp
NIL
```

If you try to delete a non-existent key:

```lisp
(cl-memcached:mc-del "non-existent-key" :memcache *cache*)
```

**Expected output:**
```lisp
"NOT_FOUND"
```

This is not an error - it's normal behavior.

## Recap

You've learned:

✓ How to install and load cl-memcached
✓ How to create a connection to memcached
✓ How to store and retrieve string data
✓ How to set expiration times (TTL)
✓ How to batch retrieve multiple keys
✓ How to delete keys

## Common Mistakes

**Mistake 1: Forgetting to convert octets to strings**

```lisp
;; Wrong - prints raw octets
(let ((response (first (mc-get+ (list "key")))))
  (format t "~A" (mc-data response)))
;; => #(72 101 108 108 111)

;; Right - decode to string
(let ((response (first (mc-get+ (list "key")))))
  (format t "~A" (babel:octets-to-string (mc-data response))))
;; => Hello
```

**Mistake 2: Not providing a list to mc-get+**

```lisp
;; Wrong
(mc-get+ "key")
;; => ERROR: KEYS-LIST has to be a LIST of keys

;; Right
(mc-get+ (list "key"))
;; => (#<MEMCACHE-RESPONSE ...>)
```

**Mistake 3: Assuming nil means error**

```lisp
;; This is NORMAL behavior, not an error
(mc-get-value "missing-key")
;; => NIL
```

Missing keys return nil - they don't raise exceptions.

## What's Next

- [Caching Tutorial](02-caching.md) - Learn caching strategies
- [Connection Pooling](../how-to/connection-pooling.md) - Optimize performance
- [Counters](../how-to/counters.md) - Use atomic counters
- [API Reference](../reference/api-reference.md) - Complete function reference

## Troubleshooting

**Can't connect to memcached?**
- Verify memcached is running: `echo "stats" | nc localhost 11211`
- Check host and port match your server configuration

**Type errors about octets?**
- Remember: stored data is always binary (octets)
- Use `babel:octets-to-string` to convert or use `mc-get-value`

**Keys not found?**
- Check for typos in key names
- Check if data expired (TTL)
- Keys are case-sensitive

---

**Next:** [Caching Strategies Tutorial](02-caching.md)
