# cl-memcached Architecture

<!-- Generated from: canon/canon.yaml + .canon-initiation/git-archaeology.md -->

Understand the design decisions, architecture, and implementation choices that make cl-memcached fast, simple, and reliable.

## Design Philosophy

cl-memcached follows four core principles:

1. **Simple** - Single-file library, minimal dependencies
2. **Focused** - Does one thing well (memcached client)
3. **Performant** - Connection pooling provides 5-7x speedup
4. **Compatible** - Works across SBCL, CCL, and CMUCL

## High-Level Architecture

```
┌─────────────────────────────────────────────────┐
│  Application Code                               │
└────────────────┬────────────────────────────────┘
                 │
    ┌────────────▼───────────────────────────┐
    │  Public API Layer                       │
    │  (mc-set, mc-get, mc-incr, mc-stats...) │
    └────────────┬───────────────────────────┘
                 │
     ┌───────────┴───────────┐
     │                       │
┌────▼───────────┐  ┌───────▼──────────┐
│ TEXT Protocol  │  │ META Protocol    │
│  (Classic)     │  │  (Modern)        │
└────┬───────────┘  └───────┬──────────┘
     │                      │
     └──────────┬───────────┘
                │
    ┌───────────▼──────────────┐
    │  Connection Management   │
    │  - Connection Pool       │
    │  - Socket Lifecycle      │
    └───────────┬──────────────┘
                │
    ┌───────────▼──────────────┐
    │  Binary Socket I/O       │
    │  (usocket + babel)       │
    └───────────┬──────────────┘
                │
    ┌───────────▼──────────────┐
    │  Memcached Server        │
    └──────────────────────────┘
```

## Key Components

### 1. Public API Layer

**File:** `cl-memcached.lisp` (lines 1-711)
**Exports:** 44 public symbols (see `packages.lisp`)

**Responsibilities:**
- Type conversion (strings ↔ octets)
- Global variable management (`*memcache*`, `*mc-use-pool*`, `*mc-default-encoding*`)
- Parameter handling (`:memcache`, `:timeout`, `:flags`)
- Error signaling

**Design decision:** Simple function-based API (not CLOS)
- Easier to use
- Lower overhead
- Sufficient for use case

### 2. Protocol Handlers

#### TEXT Protocol (Classic)

**Functions:** `mc-set`, `mc-get`, `mc-add`, `mc-replace`, `mc-cas`, `mc-incr`, `mc-decr`, `mc-del`, `mc-stats`, etc.

**Wire format:** ASCII commands + CRLF termination

**Example:**
```
set key 0 3600 5\r\n
value\r\n
```

**Characteristics:**
- Widely compatible (all memcached versions)
- Simple to debug (human-readable)
- Single-response per operation
- Well-tested since 2011

#### META Protocol (Modern)

**Added:** September 2025 (commit `95087f9`)
**Functions:** `mc-meta-set`, `mc-meta-get`, `mc-meta-delete`, `mc-meta-noop`

**Wire format:** ASCII meta commands + optional data

**Example:**
```
ms key T60 v\r\n
5\r\n
value\r\n
```

**Advanced features:**
- **Pipelining:** Batch operations with `:quiet t` flag
- **Opaque tokens:** Request/response correlation
- **Stampede protection:** `N` flag for dogpile prevention
- **Stale data:** `I` flag to serve stale while revalidating

**Why added:**
> "a more efficient and feature-rich alternative to the classic text protocol"
> — Commit message, Sep 24, 2025

**Design choice:** Incremental adoption
- Both protocols coexist
- No breaking changes
- Applications can mix both

### 3. Connection Management

#### Without Pooling

**Simple approach:**
```lisp
(defun mc-set (key value)
  (let ((socket (usocket:socket-connect host port)))
    (unwind-protect
      (progn
        (send-command socket "set" key value)
        (read-response socket))
      (usocket:socket-close socket))))
```

**Performance:** 1 connection per operation (slow)

#### With Pooling

**Using `pooler` library** (https://github.com/quasi/pooler)

**Performance:** ~7x speedup on SBCL
- Reuse connections
- Avoid connection overhead
- Thread-safe pool management

**Activation:**
```lisp
(setf cl-memcached:*mc-use-pool* t)
```

**Pool lifecycle:**
1. First request creates connection
2. Connection returned to pool after use
3. Next request reuses connection
4. Idle connections eventually closed

**Design decision:** Optional but recommended
- Default: disabled (for simplicity)
- Production: enabled (for performance)

### 4. Binary Socket I/O

#### Why Binary Streams?

**Historical problem (Sep 2013):**
> "Testing on CCL revealed some flaws. The stream to read write the memcached server was a character stream. But we had to read octets off it. This worked on SBCL but not CCL."
> — Commit `980dc80`, Sep 10, 2013

**Solution:**
> "So rewrote/refactored the code. The streams are now binary. The commands are converted to octets and then sent to the server."

**Result:** Works on SBCL, CCL, and CMUCL

**Technical details:**
- Element type: `'(unsigned-byte 8)`
- Commands encoded with Babel
- Responses decoded with Babel
- Binary data preserved correctly

#### Socket Utilities

**Functions:**
- `read-line-from-binary-stream` - Read ASCII line from binary stream
- `read-binary-data` - Read N octets
- `write-octets-to-stream` - Write octets to stream

**Design:** Simple, focused helpers for protocol implementation

## Data Flow

### Writing Data (SET)

```
Application
  ↓ mc-set "key" "value"
  ↓ Convert string to octets (Babel UTF-8)
  ↓ Format command: "set key 0 0 5\r\nvalue\r\n"
  ↓ Convert command to octets
  ↓ Get connection (from pool or new)
  ↓ Write octets to socket
  ↓ Read response: "STORED\r\n"
  ↓ Return "STORED"
  ↓ Return connection to pool
```

### Reading Data (GET)

```
Application
  ↓ mc-get+ '("key1" "key2")
  ↓ Format command: "get key1 key2\r\n"
  ↓ Convert command to octets
  ↓ Get connection (from pool or new)
  ↓ Write octets to socket
  ↓ Read response:
     VALUE key1 0 6\r\n
     value1\r\n
     VALUE key2 0 6\r\n
     value2\r\n
     END\r\n
  ↓ Parse into memcache-response structs
  ↓ Return list of responses
  ↓ Return connection to pool
```

## Design Decisions (Rationale)

### Decision 1: Binary Streams for Compatibility

**When:** September 2013 (commit `980dc80`)

**Problem:** Character streams worked on SBCL but not CCL

**Solution:** Use binary streams everywhere

**Trade-off:**
- ✓ Multi-implementation compatibility
- ✓ Correct binary data handling
- ✗ Slightly more complex (manual encoding/decoding)

**Verdict:** Worth it for portability

### Decision 2: Connection Pooling for Performance

**When:** Added early (2012-2013 timeframe)

**Problem:** Creating connections per request is slow

**Solution:** Connection pooling via `pooler` library

**Results:**
- ~7x speedup on SBCL
- ~3x speedup on other implementations

**Trade-off:**
- ✓ Massive performance gain
- ✓ Production-ready performance
- ✗ Additional dependency
- ✗ Slightly more complex

**Verdict:** Essential for production use

### Decision 3: Backward Compatibility Priority

**When:** Ongoing principle

**Examples:**
- Adding `mc-gets` without breaking `mc-get` (Sep 2025)
- Adding meta protocol without breaking text protocol (Sep 2025)
- Preserving API signatures during bug fixes (Jan 2026)

**Quote:**
> "Existing `mc-get` and `mc-get+` functions are preserved"
> — Commit `71fd866`, Sep 23, 2025

**Trade-off:**
- ✓ No breaking changes for users
- ✓ Incremental adoption possible
- ✗ Some API redundancy

**Verdict:** User-friendly, worth the redundancy

### Decision 4: Dual Protocol Support

**When:** September 2025 (commit `95087f9`)

**Rationale:**
> "fully compatible with existing text protocol functions, allowing users to adopt the new features incrementally"

**Implementation:**
- Both protocols share connection management
- Both protocols share error handling
- Applications can use both simultaneously

**Trade-off:**
- ✓ Modern features available (pipelining, opaque tokens)
- ✓ No breaking changes
- ✗ More code to maintain
- ✗ Larger API surface

**Verdict:** Future-proof without disruption

### Decision 5: Single-File Architecture

**When:** Original design (2011)

**Current state:** 711 lines in `cl-memcached.lisp`

**Benefits:**
- Easy to understand
- Simple to audit
- Quick to load
- No internal file dependencies

**Trade-off:**
- ✓ Simplicity
- ✓ Easy to inspect
- ✗ Large file (but manageable at 711 lines)

**Verdict:** Appropriate for focused library

## Performance Characteristics

### Benchmarks (from test suite)

**With pooling:**
```
10,000 SET operations: ~1.4 seconds (SBCL)
Throughput: ~7,100 ops/sec
```

**Without pooling:**
```
10,000 SET operations: ~10 seconds (SBCL)
Throughput: ~1,000 ops/sec
```

**Speedup:** ~7x with pooling enabled

### Pipelining Performance

**Without pipelining (100 operations):**
- Round-trips: 100
- Latency @ 1ms RTT: 100ms

**With pipelining (100 operations):**
- Round-trips: 2
- Latency @ 1ms RTT: 2ms
- **Speedup:** 50x

## Dependencies

| Dependency | Purpose | License |
|------------|---------|---------|
| **usocket** | Cross-platform sockets | MIT |
| **babel** | String encoding/decoding | MIT |
| **split-sequence** | String utilities | Public Domain |
| **pooler** | Connection pooling | MIT |

**Test-only:**
- **fiveam** | Test framework | BSD

**Philosophy:** Minimal, well-maintained dependencies

## Code Quality Metrics

```
Lines of code: 711
Lines of tests: 529
Test/code ratio: 0.74
Exported symbols: 44
Test count: 27
Confidence: 0.94 (Canon extraction)
```

## Evolution Timeline

| Date | Event | Impact |
|------|-------|--------|
| 2011 | Initial release | MIT license, basic operations |
| 2012-2013 | Binary streams, pooling | Multi-implementation support, 7x speedup |
| Sep 2025 | GETS, stats variants | Protocol completeness |
| Sep 2025 | Meta protocol | Modern features, pipelining |
| Jan 2026 | Meta protocol fixes, test suite | Stability, comprehensive tests |

## Future Considerations

**From Canon recommendations:**

1. Add dedicated tests for append/prepend operations
2. Add dedicated tests for counter operations
3. Consider CHANGELOG or conventional commits
4. Extract formal ADRs from git archaeology
5. Add ABOUTME comments to source file

**Status:** Active development, responsive maintenance

## Recap

cl-memcached's architecture prioritizes:

✓ **Simplicity** - Single-file, minimal dependencies
✓ **Performance** - Connection pooling, pipelining
✓ **Compatibility** - SBCL, CCL, CMUCL support
✓ **Stability** - Backward compatibility, comprehensive tests
✓ **Completeness** - Both TEXT and META protocols

## What's Next

- [Performance Tuning](performance.md) - Optimize your usage
- [How-To Guides](how-to/) - Solve specific problems
- [API Reference](reference/api-reference.md) - Complete API

---

**See also:**
- [Canon Manifest](../canon/canon.yaml) - Formal specification
- [Git Archaeology](../.canon-initiation/git-archaeology.md) - Design decisions
