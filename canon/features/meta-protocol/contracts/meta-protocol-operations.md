# Meta Protocol Operations Contract

[DRAFT - Extracted via triangulation]

## Overview

The meta protocol is a modern, efficient memcached protocol supporting advanced caching semantics and pipelining. It uses single-letter commands (`mg`, `ms`, `md`, `mn`) with flexible flag-based options.

**Source files**: cl-memcached.lisp:472-711
**Added**: Commit 95087f9 (feat: Implement memcached meta protocol)
**Fixed**: Commit 0141acf (fix: Fix bugs in meta protocol)
**Confidence**: 0.95 (convergent: code ∩ docs ∩ tests)

---

## mc-meta-get

**Signature**:
```lisp
(mc-meta-get key
             &key (stream nil)
                  (memcache *memcache*)
                  (mc-use-pool *mc-use-pool*)
                  (value t)
                  (cas nil)
                  (recache-on-miss-ttl nil)
                  (early-recache-ttl nil)
                  (quiet nil)
                  (opaque nil)
                  (return-key nil)
                  (key-is-base64 nil))
```

**Purpose**: Retrieve a key using the `mg` meta command with advanced caching semantics

**Parameters**:
- `key` (string): Cache key
- `stream`: Optional stream for pipelining (use with `mc-with-connection`)
- `memcache`, `mc-use-pool`: Standard connection parameters
- `value` (boolean, default t): Retrieve value data (v flag)
- `cas` (boolean): Return CAS token (c flag)
- `recache-on-miss-ttl` (integer): Create placeholder with TTL on miss to prevent dogpiling (N flag)
- `early-recache-ttl` (integer): Signal recache if remaining TTL < this value (R flag)
- `quiet` (boolean): Suppress response (q flag, for pipelining)
- `opaque` (string): Token reflected in response for request correlation (O flag)
- `return-key` (boolean): Include key in response (k flag)
- `key-is-base64` (boolean): Key is base64-encoded (b flag)

**Returns**: Two values:
1. **Found**: Hash table with response data, containing:
   - `:value` - (unsigned-byte 8) array (if value=t)
   - `:cas` - CAS token string (if cas=t)
   - `:opaque` - Opaque token (if provided)
   - `:key` - Key (if return-key=t)
   - `:win` - t if recache flag set (recache-on-miss or early-recache triggered)
   - `:stale` - t if item is stale (X flag)
   - `:already-won` - t if another client already won the recache race (Z flag)
2. **Foundp**: t if key found, nil otherwise

**Not found behavior**: Returns ("EN", nil)

**Protocol**: Sends `mg <key> [flags]\r\n`, receives `VA <len> [tokens]\r\n<data>\r\n` or `EN\r\n`

**Example**:
```lisp
;; Simple get
(multiple-value-bind (response foundp) (mc-meta-get "mykey")
  (when foundp
    (babel:octets-to-string (gethash :value response))))
;; => "hello world"

;; Get with CAS
(multiple-value-bind (response foundp) (mc-meta-get "mykey" :cas t)
  (when foundp
    (gethash :cas response)))
;; => "12345678"

;; Pipelining with opaque token
(mc-with-connection (s)
  (mc-meta-get "key1" :stream s :quiet t :opaque "req1")
  (mc-meta-get "key2" :stream s :quiet t :opaque "req2")
  (mc-meta-noop :stream s)
  (let ((r1 (mc-read-meta-response s))
        (r2 (mc-read-meta-response s)))
    (list r1 r2)))
```

**Source**:
- Implementation: cl-memcached.lisp:525-548
- Documentation: README.md:234-236
- Tests: tests.lisp:152-169, 171-180, 246-253, 279-289

**Confidence**: 0.95

---

## mc-meta-set

**Signature**:
```lisp
(mc-meta-set key data
             &key (stream nil)
                  (memcache *memcache*)
                  (mc-use-pool *mc-use-pool*)
                  (ttl 0)
                  (client-flags 0)
                  (cas nil)
                  (quiet nil)
                  (keep-stale nil)
                  (key-is-base64 nil)
                  (opaque nil)
                  (return-key nil))
```

**Purpose**: Store a key-value pair using the `ms` meta command

**Parameters**:
- `key` (string): Cache key
- `data` (string or (unsigned-byte 8) array): Data to store
- `stream`: Optional stream for pipelining
- `memcache`, `mc-use-pool`: Standard connection parameters
- `ttl` (integer, default 0): Time-to-live in seconds (T flag)
- `client-flags` (integer): Client-defined flags (F flag)
- `cas` (string): CAS token for conditional update (C flag)
- `quiet` (boolean): Suppress response (q flag)
- `keep-stale` (boolean): When used with CAS, keep stale marker (I flag)
- `key-is-base64`, `opaque`, `return-key`: As in mc-meta-get

**Returns**:
- `"HD"` on success (Hit/Stored)
- `"EX"` if CAS mismatch (Exists)
- `"NS"` if not stored
- nil if quiet=t

**Protocol**: Sends `ms <key> <datalen> [flags]\r\n<data>\r\n`, receives `HD\r\n` or error

**Example**:
```lisp
;; Simple set
(mc-meta-set "mykey" "hello world")
;; => "HD"

;; Set with TTL
(mc-meta-set "tempkey" "expires in 60s" :ttl 60)
;; => "HD"

;; Conditional set with CAS
(multiple-value-bind (response foundp) (mc-meta-get "counter" :cas t)
  (when foundp
    (let ((cas (gethash :cas response)))
      (mc-meta-set "counter" "new-value" :cas cas))))
;; => "HD" if no concurrent modification
;; => "EX" if CAS mismatch
```

**Source**:
- Implementation: cl-memcached.lisp:550-579
- Documentation: README.md:238-240
- Tests: tests.lisp:152-160, 182-210, 291-303

**Confidence**: 0.95

---

## mc-meta-delete

**Signature**:
```lisp
(mc-meta-delete key
                &key (stream nil)
                     (memcache *memcache*)
                     (mc-use-pool *mc-use-pool*)
                     (cas nil)
                     (quiet nil)
                     (mark-stale nil)
                     (stale-ttl nil)
                     (key-is-base64 nil)
                     (opaque nil)
                     (return-key nil))
```

**Purpose**: Delete a key using the `md` meta command

**Parameters**:
- `key` (string): Cache key to delete
- `stream`: Optional stream for pipelining
- `memcache`, `mc-use-pool`: Standard connection parameters
- `cas` (string): CAS token for conditional delete (C flag)
- `quiet` (boolean): Suppress response (q flag)
- `mark-stale` (boolean): Mark as stale instead of deleting (I flag)
- `stale-ttl` (integer): TTL for stale marker (T flag, used with mark-stale)
- Other flags: As in mc-meta-get

**Returns**:
- `"HD"` on successful delete
- `"NF"` if key not found (Not Found)
- `"EN"` if not deleted
- `"EX"` if CAS mismatch
- nil if quiet=t

**Protocol**: Sends `md <key> [flags]\r\n`, receives `HD\r\n` or error

**Stale marking**: When `mark-stale=t`, marks item as stale rather than deleting it. Useful for lazy deletion patterns where stale items can still be served temporarily.

**Example**:
```lisp
;; Simple delete
(mc-meta-delete "mykey")
;; => "HD"

;; Delete non-existent key
(mc-meta-delete "nosuchkey")
;; => "NF" or "EN"

;; Conditional delete with CAS
(mc-meta-delete "mykey" :cas "12345678")
;; => "HD" if CAS matches
;; => "EX" if CAS mismatch

;; Mark as stale instead of delete
(mc-meta-delete "mykey" :mark-stale t :stale-ttl 300)
;; => "HD" (item marked stale for 5 minutes)
```

**Source**:
- Implementation: cl-memcached.lisp:581-603
- Documentation: README.md:242-244
- Tests: tests.lisp:212-238

**Confidence**: 0.93

---

## mc-meta-noop

**Signature**:
```lisp
(mc-meta-noop &key (stream nil)
                   (memcache *memcache*)
                   (mc-use-pool *mc-use-pool*))
```

**Purpose**: Send a no-op command to flush pipelined operations and get a response marker

**Parameters**:
- `stream`: Optional stream (commonly used with pipelining)
- `memcache`, `mc-use-pool`: Standard connection parameters

**Returns**: `"MN"` (Meta Noop)

**Protocol**: Sends `mn\r\n`, receives `MN\r\n`

**Typical usage**: Pipeline terminator to force all queued operations to complete

**Example**:
```lisp
(mc-with-connection (s)
  ;; Send multiple quiet operations
  (mc-meta-set "key1" "data1" :stream s :quiet t)
  (mc-meta-set "key2" "data2" :stream s :quiet t)
  (mc-meta-set "key3" "data3" :stream s :quiet t)

  ;; Flush pipeline with noop
  (let ((result (mc-meta-noop :stream s)))
    (assert (string= result "MN")))

  ;; Now all sets are guaranteed complete
  )
```

**Source**:
- Implementation: cl-memcached.lisp:605-613
- Documentation: README.md:262 (in pipelining example)
- Tests: tests.lisp:240-244, 255-277

**Confidence**: 0.95

---

## mc-with-connection

**Signature**:
```lisp
(mc-with-connection ((stream-var &key (memcache '*memcache*)
                                       (use-pool '*mc-use-pool*))
                     &body body))
```

**Purpose**: Macro providing a stream for pipelined meta protocol operations

**Parameters**:
- `stream-var`: Symbol bound to the connection stream
- `memcache`, `use-pool`: Connection parameters (evaluated)
- `body`: Forms to execute with stream

**Behavior**:
- Acquires connection from pool or creates new one
- Binds stream to `stream-var`
- Executes body
- Ensures connection is returned to pool (or closed) via `unwind-protect`

**Typical usage**: Pipelining multiple operations

**Example**:
```lisp
(mc-with-connection (s)
  ;; Send commands without waiting for responses
  (mc-meta-set "key1" "value1" :stream s :quiet t)
  (mc-meta-set "key2" "value2" :stream s :quiet t)
  (mc-meta-get "key3" :stream s :quiet t :opaque "get1")

  ;; Send noop to flush and get final response
  (mc-meta-noop :stream s)

  ;; Read responses
  (let ((resp1 (mc-read-meta-response s))
        (resp2 (mc-read-meta-response s))
        (resp3 (mc-read-meta-response s)))
    (list resp1 resp2 resp3)))
```

**Source**:
- Implementation: cl-memcached.lisp:491-493 (macro expansion to mc-with-pool-y/n)
- Documentation: README.md:250-272
- Tests: tests.lisp:255-277

**Confidence**: 0.95

---

## mc-read-meta-response

**Signature**:
```lisp
(mc-read-meta-response stream &key requested-flags)
```

**Purpose**: Read and parse a single meta protocol response from a stream

**Parameters**:
- `stream`: Binary stream to read from
- `requested-flags`: List of flags sent in request (used to determine if value data follows)

**Returns**: Two values:
1. Response data (hash table or response code string)
2. Foundp (t if value response, nil if error/not-found)

**Response codes parsed**:
- `"VA"` - Value response (returns hash table with data)
- `"HD"` - Hit/Deleted/Stored (success, no data)
- `"EN"` - Not found / Not stored
- `"EX"` - Exists (CAS mismatch)
- `"ST"` - Stored
- `"MN"` - Meta noop response
- `"NF"` - Not found
- `"NS"` - Not stored

**Hash table keys for VA responses**:
- `:value` - Data octets
- `:cas` - CAS token (c flag)
- `:opaque` - Opaque token (O flag)
- `:key` - Key (k flag)
- `:win` - Recache win flag (W flag)
- `:already-won` - Already won flag (Z flag)
- `:stale` - Stale flag (X flag)

**Example**:
```lisp
(mc-with-connection (s)
  (mc-meta-get "mykey" :stream s :cas t :opaque "token1")
  (multiple-value-bind (response foundp) (mc-read-meta-response s :requested-flags '(#\v #\c))
    (when foundp
      (list :data (gethash :value response)
            :cas (gethash :cas response)
            :opaque (gethash :opaque response)))))
```

**Source**:
- Implementation: cl-memcached.lisp:495-523
- Documentation: README.md:265-270 (implicit in pipelining examples)
- Tests: tests.lisp:265-267

**Confidence**: 0.90 (primarily internal, tested via higher-level functions)

---

## Protocol Wire Format

### Meta Get (mg)
```
mg <key> v c O<opaque>\r\n
VA 11 c12345678 Otoken1\r\n
hello world\r\n
```

### Meta Set (ms)
```
ms <key> 11 T60 F0\r\n
hello world\r\n
HD\r\n
```

### Meta Delete (md)
```
md <key> C12345678\r\n
HD\r\n
```

### Meta Noop (mn)
```
mn\r\n
MN\r\n
```

**Source**: meta-server-request (cl-memcached.lisp:472-490)

---

## Flag Reference

| Flag | Parameter | Meaning | Commands |
|------|-----------|---------|----------|
| v | (implicit) | Return value data | mg |
| c | :cas | Return/require CAS token | mg, ms, md |
| T | :ttl / :stale-ttl | Time-to-live | ms, md |
| F | :client-flags | Client flags | ms |
| N | :recache-on-miss-ttl | Create placeholder on miss | mg |
| R | :early-recache-ttl | Early recache signal | mg |
| q | :quiet | Suppress response | All |
| O | :opaque | Opaque token for correlation | All |
| k | :return-key | Return key in response | All |
| b | :key-is-base64 | Key is base64-encoded | All |
| I | :mark-stale / :keep-stale | Stale marking | md, ms |
| C | :cas (ms) | CAS for set (capital C) | ms, md |
| W | (response) | Won recache race | mg (response) |
| X | (response) | Item is stale | mg (response) |
| Z | (response) | Already won | mg (response) |

**Confidence**: 0.95 (extracted from code)

---

## Advanced Features

### Dogpile Prevention

**Problem**: Multiple clients simultaneously request missing key, causing cache stampede

**Solution**: `recache-on-miss-ttl`

```lisp
(multiple-value-bind (response foundp)
    (mc-meta-get "expensive-key" :recache-on-miss-ttl 30)
  (if foundp
      (if (gethash :win response)
          ;; Won the race - compute and store
          (let ((computed-value (expensive-computation)))
            (mc-meta-set "expensive-key" computed-value :ttl 300)
            computed-value)
          ;; Lost the race - wait for winner to populate
          (gethash :value response))
      ;; Normal cache hit
      (gethash :value response)))
```

**Confidence**: 0.88 (documented feature, not directly tested in test suite)

---

### Early Recache

**Problem**: Cache item expires, causing temporary miss

**Solution**: `early-recache-ttl` signals client to refresh before expiration

```lisp
(multiple-value-bind (response foundp)
    (mc-meta-get "key" :early-recache-ttl 60)
  (when (and foundp (gethash :win response))
    ;; Less than 60s TTL remaining - refresh in background
    (spawn-background-refresh "key")))
```

**Confidence**: 0.85 (documented feature, not directly tested)

---

### Pipelining

**Performance benefit**: Reduces round-trips by batching operations

**Pattern**:
```lisp
(mc-with-connection (s)
  ;; Send N operations with :quiet t
  (dotimes (i 100)
    (mc-meta-set (format nil "key~a" i) "data" :stream s :quiet t))

  ;; Flush pipeline
  (mc-meta-noop :stream s)

  ;; All 100 sets complete with 2 round-trips (send all, recv noop)
  ;; vs 100 round-trips without pipelining
  )
```

**Confidence**: 0.95 (tested in tests.lisp:255-277)

---

## Error Handling

**Unknown response code**: Raises `cl-mc-error` with message "Unknown meta response: <line>"

**Connection errors**: Same as classic protocol (raises `memcached-server-unreachable`)

**Invalid flags**: Server may return error response

**Confidence**: 0.90

---

## Performance Characteristics

**Pipelining speedup**: O(N) operations → O(2) round-trips (send all + receive batch)

**Flag overhead**: Minimal (single-character flags)

**Response parsing**: Hash table allocation per VA response (slightly more expensive than classic protocol)

**Confidence**: 0.85 (inferred from architecture)

---

## Observations

**obs-meta-001** (convergent):
- Meta protocol is well-integrated with classic protocol
- Shares connection pooling infrastructure
- Consistent error handling model

**obs-meta-002** (convergent):
- Recent addition (commits 95087f9, 0141acf) shows active development
- Bug fixes applied quickly (same development cycle)
- Comprehensive test coverage (13 tests)

**obs-meta-003** (design excellence):
- Pipelining support via `:stream` parameter is elegant
- Quiet mode + opaque tokens enable request/response correlation
- Advanced caching semantics (dogpile prevention, early recache) show deep protocol knowledge

**obs-meta-004** (documentation):
- README documentation is excellent
- Pipelining example is clear and comprehensive
- Advanced flags documented with use cases

---

_Extraction confidence: 0.94 (weighted average)_
_Triangulation: code ∩ docs ∩ tests_
_Test coverage: Excellent (13 dedicated tests)_
_Recent addition: 2026-01 (commits 95087f9, 0141acf)_
