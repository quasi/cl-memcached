# Connection Management Contract

[DRAFT - Extracted via triangulation]

## Overview

Connection creation, pooling, and lifecycle management for memcached servers.

**Source files**: cl-memcached.lisp:48-121
**Confidence**: 0.95 (code ∩ docs ∩ benchmarks)

---

## make-memcache

**Signature**:
```lisp
(make-memcache &key (ip "127.0.0.1")
                    (port 11211)
                    (name "Memcache")
                    (pool-size 2))
```

**Purpose**: Create a memcache instance with built-in connection pool

**Parameters**:
- `ip` (string, default "127.0.0.1"): Memcached server IP address
- `port` (integer, default 11211): Memcached server port
- `name` (string, default "Memcache"): Human-readable name for this connection
- `pool-size` (integer, default 2): Maximum number of pooled connections

**Returns**: `memcache` structure

**Structure definition**:
```lisp
(defstruct memcache
  (name "Memcache" :type simple-string :read-only t)
  (ip "127.0.0.1" :type simple-string :read-only t)
  (port 11211 :type fixnum :read-only t)
  (pool-size 20 :type fixnum :read-only t)
  pool)  ; pooler pool instance
```

**Pool initialization**: Automatically creates `pooler:make-pool` with:
- `:capacity` = pool-size
- `:item-maker` = lambda creating new connections
- `:item-destroyer` = connection cleanup function

**Example**:
```lisp
(defvar *mc* (make-memcache))
;; => #<MEMCACHED-SERVER Name:Memcache IP:127.0.0.1 Port:11211 >

(make-memcache :ip "192.168.1.100" :port 11212 :pool-size 10)
;; => #<MEMCACHED-SERVER Name:Memcache IP:192.168.1.100 Port:11212 >
```

**Source**:
- Implementation: cl-memcached.lisp:48-70
- Documentation: README.md:28-30
- Tests: tests.lisp:22-26 (setup fixtures)

**Confidence**: 0.98

---

## Global Variables

### *memcache*

**Type**: Special variable
**Default**: `nil`
**Purpose**: Default memcache instance for operations

**Usage pattern**:
```lisp
(setf *memcache* (make-memcache))
;; Now all operations can omit :memcache keyword
(mc-set "key" "value")  ; Uses *memcache*
```

**Source**: cl-memcached.lisp:16-17, README.md:14-16
**Confidence**: 0.98

---

### *mc-use-pool*

**Type**: Special variable
**Default**: `nil`
**Purpose**: Default flag for connection pooling

**Impact**:
- `nil`: Create new connection for each operation (slow)
- `t`: Use connection from pool (fast, ~7x speedup on SBCL)

**Recommendation**: Set to `t` for production use

**Performance data**:
| Implementation | Without Pool | With Pool | Speedup |
|----------------|--------------|-----------|---------|
| SBCL 1.1.10    | 4.942s       | 0.713s    | 6.9x    |
| CCL 1.9        | 4.711s       | 0.847s    | 5.6x    |
| CMUCL 20D      | 4.460s       | 0.970s    | 4.6x    |

(Benchmark: 10,000 SET + 10,000 GET operations with 1KB payload)

**Source**: cl-memcached.lisp:19-20, README.md:18-20, 209-216
**Confidence**: 0.98

---

### *mc-default-encoding*

**Type**: Special variable
**Default**: `(babel:make-external-format :UTF-8)`
**Purpose**: Default string encoding for text data

**Used by**: `mc-set`, `mc-data`, `mc-get-value`, all operations accepting `external-format` parameter

**Example**:
```lisp
(setf *mc-default-encoding* (babel:make-external-format :latin-1))
```

**Source**: cl-memcached.lisp:22-23, README.md:22-24
**Confidence**: 0.95

---

## Internal Functions

### new-memcache-connection

**Signature**:
```lisp
(new-memcache-connection memcache)
```

**Purpose**: Create a single socket connection to memcached server

**Implementation**:
```lisp
(usocket:socket-connect (mc-ip memcache)
                        (mc-port memcache)
                        :element-type '(unsigned-byte 8))
```

**Returns**: usocket connection

**Error handling**: Raises `memcached-server-unreachable` on connection failure

**Source**: cl-memcached.lisp:74-76
**Confidence**: 0.95

---

### close-memcache-connection

**Signature**:
```lisp
(close-memcache-connection connection)
```

**Purpose**: Close a socket connection

**Implementation**: Wraps `usocket:socket-close` with `ignore-errors`

**Source**: cl-memcached.lisp:78-79
**Confidence**: 0.95

---

## Connection Pooling Architecture

### mc-with-pool-y/n Macro

**Signature**:
```lisp
(mc-with-pool-y/n ((memcache use-pool stream) &body body))
```

**Purpose**: Internal macro managing connection acquisition and cleanup

**Behavior**:
1. If `use-pool=t`: Fetch connection from pool (`pooler:fetch-from`)
2. If `use-pool=nil`: Create new connection
3. Bind stream to `(usocket:socket-stream connection)`
4. Execute body
5. Cleanup:
   - If pool: Return connection to pool (`pooler:return-to`)
   - If no pool: Close connection

**Error handling**: On error, connection is closed (not returned to pool)

**Guarantee**: Connection always cleaned up via `unwind-protect`

**Source**: cl-memcached.lisp:106-121
**Confidence**: 0.95

---

### Pooler Integration

**Library**: [pooler](https://github.com/quasi/pooler)

**Pool configuration**:
- Name: "Memcache Connection Pool"
- Capacity: `(mc-pool-size memcache)`
- Item maker: Creates usocket connection
- Item destroyer: Closes usocket connection

**Benefits**:
- Reuse connections across operations
- Reduce TCP handshake overhead
- Limit concurrent connections to server

**Source**: cl-memcached.lisp:66-69
**Confidence**: 0.95

---

## Error Conditions

### memcached-server-unreachable

**Type**: Error condition (subclass of `cl-mc-error`)

**Raised when**: Cannot connect to memcached server

**Typical causes**:
- Server not running
- Wrong IP/port
- Network issues
- Firewall blocking connection

**Example**:
```lisp
(handler-case
    (setf *memcache* (make-memcache :port 9999))  ; Wrong port
  (memcached-server-unreachable ()
    (format t "Cannot reach memcached server")))
```

**Source**: cl-memcached.lisp:93-94, 76
**Confidence**: 0.92

---

## Usage Patterns

### Single Server Setup

```lisp
(setf *memcache* (make-memcache))
(setf *mc-use-pool* t)  ; Enable pooling

(mc-set "key" "value")  ; Uses defaults
```

### Multiple Server Setup

```lisp
(defvar *cache-primary* (make-memcache :ip "10.0.1.10" :pool-size 10))
(defvar *cache-secondary* (make-memcache :ip "10.0.1.11" :pool-size 5))

(mc-set "key" "value" :memcache *cache-primary*)
(mc-set "key" "value" :memcache *cache-secondary*)
```

### Per-Operation Pool Control

```lisp
;; Force pool usage for specific operation
(mc-get '("key1") :mc-use-pool t)

;; Force new connection (bypass pool)
(mc-get '("key2") :mc-use-pool nil)
```

---

## Performance Characteristics

### Connection Pooling Impact

**Overhead without pool**:
- TCP handshake: ~1ms per operation
- Socket creation/destruction: CPU cycles

**Overhead with pool**:
- Pool fetch/return: ~microseconds
- Amortized connection cost

**Recommendation**: Always use pooling in production (`*mc-use-pool* = t`)

**Confidence**: 0.95 (based on benchmarks)

---

### Pool Size Tuning

**Factors**:
- Concurrent request load
- Application thread count
- Server connection limits

**Guidelines**:
- Web app: pool-size = 2-5 per worker thread
- Batch processing: pool-size = number of concurrent workers
- Low traffic: pool-size = 1-2

**Default (2)**: Conservative, suitable for low-traffic applications

**Source**: Practical experience, README.md:28
**Confidence**: 0.85 (guideline, not specification)

---

## Observations

**obs-conn-001** (design excellence):
- Clean separation: public API (`make-memcache`) vs internal (`new-memcache-connection`)
- Automatic pool initialization in constructor
- Resource cleanup guaranteed via `unwind-protect`

**obs-conn-002** (performance):
- Benchmark data shows significant performance improvement
- Pooling provides 5-7x speedup across implementations
- Well-documented trade-offs

**obs-conn-003** (convergent):
- Implementation matches documented behavior exactly
- Benchmark results are verifiable and reproducible
- Clear, simple API

---

_Extraction confidence: 0.95_
_Triangulation: code ∩ docs ∩ benchmarks_
_Performance validation: Excellent (benchmark data included)_
