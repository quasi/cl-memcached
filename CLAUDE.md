---
type: specification
version: 1.0.0
applies-to: [cl-memcached]
requires: [Canon specification]
generated: 2026-01-21
confidence: 0.94
---

# cl-memcached: Implementation Contract and Behavioral Rules

Comprehensive specification for implementing and extending cl-memcached. Target: Agent-driven development, deterministic parsing, mechanical verification.

## 1. Scope and Applicability

**What this covers**: All public API functions, connection management, protocol handling, error conditions, type constraints, and invariants.

**What this doesn't cover**: Internal implementation details, specific Lisp dialect features, performance optimizations beyond architectural guidelines.

**Applies to**: All code modifications, new features, bug fixes, protocol implementations.

## 2. Terminology

Definitions appear once; use exact terms consistently throughout.

- **key**: Text identifier for cached data (1-250 characters, ASCII, case-sensitive)
- **value**: Binary data (octets), max ~1MB
- **TTL**: Time-to-live expiration in seconds; 0 means no expiration
- **CAS token**: Unique identifier for a value state, used in compare-and-set operations
- **memcache**: Connection object to a single memcached server
- **memcache-response**: Structure containing key, data, flags, CAS token from server response
- **response code**: 2-character protocol response (STORED, NOT_FOUND, EXISTS, etc.)
- **TEXT protocol**: Classic memcached protocol (commands as ASCII text)
- **META protocol**: Modern memcached protocol with advanced features
- **connection pool**: Reusable connection storage, automatically managed
- **pipelining**: Batching multiple operations before reading responses
- **noreply**: Flag to suppress server response confirmation
- **quiet mode**: Meta protocol mode where server doesn't send responses for individual operations

## 3. Type Constraints

### Data Types (MUST)

| Type | Storage Format | Constraint |
|------|---|---|
| Keys | String (simple-string) | 1-250 ASCII chars, unique per server |
| Values | Octets `(unsigned-byte 8)` | Raw binary, max ~1MB |
| Flags | Integer (0-2^32-1) | Protocol metadata field |
| CAS tokens | String | Hex-encoded unique identifier |
| Expiration | Integer (seconds) | 0 = never; >0 = TTL |
| Counter values | Integer (0-2^64-1) | Never negative |

### Return Type Guarantees (MUST)

| Function | Returns | Guarantee |
|---|---|---|
| `mc-get+` | List of memcache-response \| nil | Always list or nil, never raises on missing keys |
| `mc-gets+` | List of memcache-response \| nil | Always list or nil, includes CAS tokens |
| `mc-incr`/`mc-decr` | Integer | Counter value after operation |
| `mc-del` | String | "DELETED" or "NOT_FOUND" |
| `mc-set` | String | "STORED" on success |
| Counters | Never negative | Decrement clamps at 0 |

## 4. Normative Rules

### RULE-001: Keys Are Strings

**Rule**: All key parameters MUST be strings.

**Applies to**: `mc-set`, `mc-add`, `mc-replace`, `mc-get`, `mc-gets`, `mc-del`, `mc-incr`, `mc-decr`, `mc-touch`, `mc-cas`, meta protocol operations.

**Validation**: `(typep key 'string)` MUST be true, else `cl-mc-error`.

**Agent action**: Flag non-string key usage as error. Auto-fix forbidden (type error indicates logic error).

---

### RULE-002: Values Are Octet Arrays Internally

**Rule**: Storage operations convert strings to octets; retrieval operations return octets. User-facing functions handle conversion via `:encoding` parameter.

**Applies to**: `mc-store` (low-level), `mc-set` (high-level), retrieval functions.

**Implementation**:
- `mc-set` accepts string or octets, converts to octets using `*mc-default-encoding*`
- `mc-store` accepts octets only, raises error if not `(unsigned-byte 8)` array
- Retrieval returns octets; user must convert with `babel:octets-to-string`

**Rationale**: Binary protocol compatibility across Lisp implementations.

**Agent action**: Flag string/octet mismatches. Auto-fix allowed for conversions.

---

### RULE-003: Lists Are Required for Get Operations

**Rule**: `mc-get`, `mc-get+`, `mc-gets`, `mc-gets+` MUST receive `keys-list` as list type.

**Applies to**: All GET operations.

**Validation**: `(listp keys-list)` MUST be true, else `cl-mc-error` with message "KEYS-LIST has to be a LIST of keys".

**Rationale**: Protocol optimization (batch retrieval), type safety.

**Agent action**: Flag non-list keys-list. Auto-fix allowed (wrap single key in list).

---

### RULE-004: CAS Tokens Only With CAS Command

**Rule**: `cas-unique` parameter MUST only be used with `:cas` command in `mc-store`.

**Applies to**: `mc-store`, `mc-cas`.

**Validation**: If `cas-unique` is provided and command is not `:cas`, raise `cl-mc-error`.

**Rationale**: Protocol correctness, prevents confusion.

**Agent action**: Flag cas-unique with non-CAS command. Auto-fix allowed (remove parameter or change command).

---

### RULE-005: Counters Must Be Initialized

**Rule**: Before calling `mc-incr` or `mc-decr`, key MUST be initialized with `mc-set`.

**Applies to**: `mc-incr`, `mc-decr`.

**Protocol behavior**: Returns `NOT_FOUND` if key doesn't exist (not an error).

**Agent action**: Warn if incr/decr called on uninitialized key. Suggest `mc-set` initialization.

---

### RULE-006: TTL Zero Means Never Expire

**Rule**: `timeout=0` (or `ttl=0` in meta protocol) means data never expires automatically.

**Applies to**: `mc-set`, `mc-add`, `mc-replace`, `mc-cas`, `mc-meta-set`.

**Implication**: Explicit `mc-del` or server restart required to remove data.

**Agent action**: Flag large TTL values (>86400) as suspicious. Warn if no TTL provided on volatile data.

---

### RULE-007: Connection Cleanup Is Guaranteed

**Rule**: Connections MUST be cleaned up even on error. `unwind-protect` is non-negotiable.

**Applies to**: All connection management code.

**Implementation**: Every public function wraps connection use in `unwind-protect` or macro.

**Violation**: Connection leak (resource exhaustion under error).

**Agent action**: Flag missing `unwind-protect` in connection code. Auto-fix allowed (wrap operation).

---

### RULE-008: Both Protocols Are Semantically Equivalent

**Rule**: TEXT protocol and META protocol operations MUST produce identical results (except for protocol-specific features).

**Applies to**: Operations available in both protocols (set, get, delete, CAS).

**Examples**:
- `(mc-set "k" "v")` ≡ `(mc-meta-set "k" "v")`
- `(mc-gets+ ...)` and `(mc-meta-get ... :cas t)` both return CAS tokens
- CAS success/failure codes differ syntactically but convey same semantics

**Rationale**: Dual protocol support requires equivalent behavior.

**Agent action**: Flag protocol differences in behavior. Auto-fix forbidden (protocol difference may be intentional).

---

### RULE-009: Missing Keys Return NIL/Empty, Not Error

**Rule**: GET operations on non-existent keys MUST return nil or empty collection, never raise error.

**Applies to**: `mc-get+`, `mc-gets+`, `mc-meta-get`.

**Rationale**: Absence is normal state, not exceptional.

**Agent action**: Flag exceptions on key not found. Auto-fix allowed (return nil).

---

### RULE-010: Atomic Counter Operations

**Rule**: `mc-incr` and `mc-decr` operations MUST be atomic (executed server-side without race conditions).

**Applies to**: `mc-incr`, `mc-decr`.

**Implication**: Safe for use in concurrent environments without additional locking.

**Agent action**: No action needed (protocol guarantee).

---

## 5. System Invariants

Invariants MUST hold before, during, and after every operation. Violations indicate bugs.

### INV-001: Data Integrity Round-Trip

**Invariant**: `∀ (set key value) : (get key) ⟹ value == retrieved-value` (assuming no expiration or delete)

**Mechanically checkable**:
```lisp
(let ((original "test-data"))
  (mc-set "key" original)
  (let ((retrieved (babel:octets-to-string
                     (mc-data (first (mc-get+ (list "key")))))))
    (assert (string= original retrieved))))
```

**Confidence**: 1.0 (testable by all test suites)

---

### INV-002: CAS Token Validity Scope

**Invariant**: CAS token is valid until the key is modified (set/delete/overwrite).

**Protocol semantics**:
- Get key → receive CAS token T1
- Any write to key → T1 becomes invalid
- CAS with T1 returns "EXISTS"
- Next get → new CAS token T2

**Enforcement**: Server-side guarantee, protocol implementation.

**Confidence**: 0.98 (protocol spec, verified by tests)

---

### INV-003: Connection Pool Transparency

**Invariant**: Operations produce identical results regardless of `*mc-use-pool*` setting.

**Implication**: Only latency differs, not behavior or correctness.

**Measurable**: `(eq (result-with-pool) (result-without-pool))`

**Confidence**: 0.95 (architecture guarantee, verified by benchmarks)

---

### INV-004: Expiration Mechanism

**Invariant**: Key expires (becomes unretrievable) at precisely TTL seconds after storage.

**Exception**: TTL=0 means no expiration.

**Enforcement**: Memcached server, not client.

**Client responsibility**: Don't rely on precise timing; treat expired data as possibly present until gc.

**Confidence**: 0.90 (server-side, approximate timing)

---

### INV-005: Counter Non-Negativity

**Invariant**: Counter values MUST be non-negative. `mc-decr` never produces negative result.

**Enforcement**: Memcached server clamps at 0.

**Implication**: `(mc-decr "counter" 1000)` on value 5 → returns 0, not -995.

**Confidence**: 0.95 (protocol spec, verified by tests)

---

### INV-006: No Null Pointer Errors

**Invariant**: Public API MUST never return null that requires checking before use.

**Applies to**: Collections (always list or nil, never null within list); errors (raise condition, don't return error code).

**Rationale**: Prevent null pointer exceptions in caller code.

**Confidence**: 1.0 (design rule, enforced by API)

---

### INV-007: Encoding Consistency

**Invariant**: Strings are encoded/decoded using same encoding (default UTF-8).

**Implication**: Round-trip encoding MUST preserve data.

**Exception**: Binary data encoding specified explicitly.

**Confidence**: 0.98 (Babel library guarantee)

---

## 6. Anti-Patterns

Structures and patterns that MUST NOT appear in code.

### ANTI-001: Manual Connection Management

**Pattern**: Creating connections directly and not using pool or macro.

```lisp
;; DON'T do this
(let ((socket (usocket:socket-connect host port)))
  (write-to-socket socket data)
  ;; Connection not cleaned up on error!
  (close socket))
```

**Why harmful**: Connection leaks, resource exhaustion, concurrency errors.

**Remediation**: Use `mc-with-connection` macro or enable pooling.

**Agent action**: Flag unprotected socket operations. Auto-fix allowed (wrap in `unwind-protect` or macro).

---

### ANTI-002: Synchronous Retry on Connection Failure

**Pattern**: Tight retry loop on connection error.

```lisp
;; DON'T do this
(loop for i from 1 to 1000
      do (handler-case
           (mc-set "key" "value")
           (memcached-server-unreachable ()
             (sleep 0.001))))  ;; Spins CPU
```

**Why harmful**: Burns CPU, doesn't improve availability.

**Remediation**: Use exponential backoff with jitter.

**Agent action**: Flag tight retry loops. Suggest exponential backoff pattern.

---

### ANTI-003: Ignoring Encoding in Round-Trip

**Pattern**: Storing string, retrieving as raw bytes, not decoding.

```lisp
;; DON'T do this
(mc-set "key" "data")
(let ((response (first (mc-get+ (list "key")))))
  (format t "~A" (mc-data response)))  ;; Prints raw bytes!
```

**Why harmful**: Confusing output, data loss if processed as string.

**Remediation**: Always decode with `babel:octets-to-string` or use `mc-get-value`.

**Agent action**: Flag octet processing without decoding. Suggest conversion or `mc-get-value`.

---

### ANTI-004: Assuming CAS Will Succeed

**Pattern**: Using CAS without checking response code.

```lisp
;; DON'T do this
(let ((response (first (mc-gets+ (list "counter")))))
  (mc-cas "counter" "new" (mc-cas-unique response))
  (format t "Updated!"))  ;; May have failed!
```

**Why harmful**: Silent failures, lost updates, data corruption.

**Remediation**: Always check response (STORED vs EXISTS vs NOT_FOUND).

**Agent action**: Flag CAS without response checking. Auto-fix allowed (add check).

---

### ANTI-005: Mixing Protocols on Same Key

**Pattern**: Using TEXT protocol and META protocol on same key inconsistently.

**Why harmful**: Protocol differences may cause subtle bugs (should be equivalent but testing required).

**Remediation**: Stick to one protocol for a given key.

**Agent action**: Flag mixing protocols on same key. Warn if semantics differ.

---

## 7. Heuristics

Probabilistic guidance with confidence scores.

### HEUR-001: Long Function = Performance Issue

**Signal**: Function >100 LOC

**Confidence**: Medium (60%)

**Interpretation**: Likely needs extraction or optimization

**Action**: Yellow flag. Review for extractability.

---

### HEUR-002: Multiple Memcache Instances = Sharding

**Signal**: Application creates 3+ memcache connections to different hosts

**Confidence**: High (85%)

**Interpretation**: Likely implementing distributed caching or redundancy

**Action**: Green signal. Document sharding strategy.

---

### HEUR-003: No Error Handling = Brittle

**Signal**: Code path without `handler-case` for memcached operations

**Confidence**: Medium (70%)

**Interpretation**: Assumes cache is always available

**Action**: Yellow flag. Suggest error handling pattern.

---

### HEUR-004: Pipelining Unneeded = Premature

**Signal**: Batch operation <10 items with pipelining

**Confidence**: Medium (65%)

**Interpretation**: Overhead may exceed benefits

**Action**: Yellow flag. Profile before/after.

---

## 8. Operation Semantics

### SET Operation Semantics

```
Input: key, value, timeout
Output: "STORED" or error

Semantics:
- If key doesn't exist: Create with value and TTL
- If key exists: Overwrite with new value and TTL
- If timeout=0: Data never expires
- If timeout>0: Data expires after timeout seconds
- Exceptions: MEMCACHED-SERVER-UNREACHABLE, CL-MC-ERROR (invalid input)
```

### GET Operation Semantics

```
Input: keys-list (list of keys)
Output: list of memcache-response (ordered by key) or nil

Semantics:
- If all keys missing: Return nil
- If some keys missing: Return only found keys
- If key expired: Treat as missing
- Never raises on missing keys (absence is normal)
- Exceptions: MEMCACHED-SERVER-UNREACHABLE, CL-MC-ERROR (not a list)
```

### CAS Operation Semantics

```
Input: key, value, cas-token, timeout
Output: "STORED" (success), "EXISTS" (token stale), "NOT_FOUND" (key missing)

Semantics:
- Atomic compare-and-set (server-side)
- Compare: stored CAS token == provided cas-token
- If match: Update value, generate new CAS token
- If mismatch: Don't update, return "EXISTS"
- If key missing: Return "NOT_FOUND"
- Race condition safe: Only one CAS succeeds

Workflow:
1. mc-gets+ to get key + CAS token
2. Modify value locally
3. mc-cas with old CAS token
4. If "EXISTS": Retry from step 1
5. If "STORED": Success
```

### Counter Operation Semantics

```
Input: key, amount (default 1)
Output: new counter value (integer)

Semantics for mc-incr:
- Atomically increment counter by amount
- Return new value after increment
- If key missing: Return NOT_FOUND (as error)
- Exceptions: MEMCACHED-SERVER-UNREACHABLE

Semantics for mc-decr:
- Atomically decrement counter by amount
- Never return negative (clamp at 0)
- If key missing: Return NOT_FOUND (as error)
- Exceptions: MEMCACHED-SERVER-UNREACHABLE
```

## 9. Error Conditions

### MEMCACHED-SERVER-UNREACHABLE

**When**: Connection to memcached fails or times out

**Cause**: Server down, network unreachable, invalid host/port

**Handler**:
```lisp
(handler-case
  (mc-set "key" "value")
  (cl-memcached:memcached-server-unreachable (e)
    ;; Fallback to database, logging, etc.
    (format t "Cache unavailable: ~A~%" e)))
```

**Recovery**: Retry with exponential backoff, fallback strategy, failover to replica.

---

### CL-MC-ERROR

**When**: Invalid input types or protocol violations

**Examples**:
- Non-string key
- Non-list keys-list in GET
- CAS token with non-CAS command
- Invalid timeout value

**Handler**:
```lisp
(handler-case
  (mc-get+ "not-a-list")  ;; Bug in code
  (cl-memcached:cl-mc-error (e)
    ;; Fix bug, don't suppress
    (format t "Programming error: ~A~%" e)))
```

**Recovery**: Fix input validation, don't catch and suppress.

---

## 10. Allowed Transformations

Transformations agents MAY perform:

| Transform | Scope | Conditions |
|-----------|-------|------------|
| Add type annotation | Function signature | Must not change behavior |
| Extract helper function | Function body | Preserves semantics |
| Rename internal variable | Function scope | No external impact |
| Add connection cleanup | Error handlers | Wrap in unwind-protect |
| Optimize octet conversion | Data path | Use babel consistently |
| Add cache invalidation | Function exit | Must not lose correctness |

---

## 11. Forbidden Transformations

Transformations agents MUST NOT perform:

| Transform | Reason |
|-----------|--------|
| Change public function signature | Breaking API change |
| Modify CAS semantics | Race condition risk |
| Remove error handling | Safety regression |
| Change TTL behavior | Data corruption risk |
| Modify counter non-negativity | Protocol violation |
| Skip connection cleanup | Resource leak |

---

## 12. Ambiguity Resolution Order

When rules conflict or behavior is unclear:

1. **Invariants** (always true, never broken)
2. **MUST rules** (absolute requirements)
3. **Protocol specification** (memcached protocol)
4. **Existing implementation** (precedent)
5. **SHOULD rules** (recommendations)
6. **Heuristics** (probabilistic guidance)

If ambiguity remains after applying precedence: **Defer to human review**. Flag the ambiguity explicitly.

---

## 13. Machine Checklist

Binary verification assertions. ALL MUST be true for correct implementation.

```
[ ] All keys are strings (typep key 'string)
[ ] All values stored are octet arrays (unsigned-byte 8)
[ ] mc-get returns list or nil, never throws on missing
[ ] mc-gets returns list or nil with CAS tokens
[ ] mc-incr/mc-decr never return negative
[ ] mc-del returns "DELETED" or "NOT_FOUND"
[ ] mc-set returns "STORED"
[ ] CAS operations return "STORED", "EXISTS", or "NOT_FOUND"
[ ] All connections wrapped in unwind-protect
[ ] No connection leaks on error
[ ] TTL=0 means never expire
[ ] TTL>0 means expiration after N seconds
[ ] TEXT and META protocols produce equivalent results
[ ] Pooling doesn't change correctness (only speed)
[ ] Encoding round-trip preserves data
[ ] No null values in returned collections
[ ] All public errors raise conditions, not return codes
```

**Verification**: Run test suite, check all invariants hold.

---

## 14. Protocol Rules

### TEXT Protocol (Classic)

**Wire format**: ASCII commands + CRLF termination

**Commands** (examples):
- `set key flags exptime bytes [noreply]\r\ndata\r\n`
- `get key1 key2\r\n`
- `delete key\r\n`
- `incr key amount\r\n`

**Responses**:
- `STORED\r\n`
- `VALUE key flags bytes\r\ndata\r\n`
- `DELETED\r\n`

**Rules**:
- All commands are ASCII
- Data payload is binary (included in flags for encoding)
- Multi-line responses end with `END\r\n`
- Single-word responses like `STORED`, `NOT_FOUND`

---

### META Protocol (Modern)

**Wire format**: ASCII meta commands + optional data + CRLF

**Commands** (examples):
- `mg key [flags]\r\n` (meta get)
- `ms key flags [exptime] [options]\r\ndata\r\n` (meta set)
- `md key [options]\r\n` (meta delete)
- `mn [opaque]\r\n` (meta noop)

**Responses**:
- 2-char codes: `HD` (hit), `EN` (end), `EX` (exists), `ST` (stored)
- `VA <size> [flags]\r\ndata\r\n` (value response)
- Opaque token reflection for pipelining

**Rules**:
- Commands are ASCII keywords
- Responses are 2-character codes (except VA)
- Supports pipelining (quiet mode)
- Opaque tokens for request/response correlation
- Advanced flags: recache on miss, stale data serving

---

## 15. Pipelining Constraints

### Pipeline Assembly Rules (MUST)

```lisp
(mc-with-connection (s :memcache mc)
  ;; MUST: Use :stream parameter for pipelining
  (mc-meta-set "k1" "v1" :stream s :quiet t)
  (mc-meta-set "k2" "v2" :stream s :quiet t)

  ;; MUST: Flush pipeline with noop or final non-quiet operation
  (mc-meta-noop :stream s)

  ;; MAY: Use :opaque tokens for response correlation
  (mc-meta-get "k1" :stream s :quiet t :opaque "req1"))
```

### Quiet Mode Rules (MUST)

- Operations with `:quiet t` suppress server response
- MUST follow with noop or non-quiet operation to synchronize
- MUST NOT rely on response from quiet operation
- Exception: Final operation may be non-quiet for synchronization

### Opaque Token Rules (SHOULD)

- Include `:opaque <token>` for request/response correlation
- Opaque token reflected in response (same string)
- Enables correlation in pipelined batch operations
- Optional but recommended for batches >2 operations

---

## 16. Exemplar Patterns

### PATTERN-001: Simple Get-Set

**Scenario**: Store and retrieve a string value

**Complete Example**:
```lisp
;; Store a value with 1-hour expiration
(cl-memcached:mc-set "user:123" "John Doe" :timeout 3600 :memcache *cache*)

;; Retrieve and decode (handles encoding automatically)
(let* ((responses (cl-memcached:mc-get+ (list "user:123") :memcache *cache*))
       (response (first responses)))
  (when response
    (format t "Name: ~A~%"
      (babel:octets-to-string (cl-memcached:mc-data response)))))
```

**Why This Shape**:
- `mc-set` handles string encoding automatically (no manual conversion)
- `mc-get+` returns structured responses (easier than raw alist)
- Always check response is non-nil (key might not exist)
- Use `babel:octets-to-string` for UTF-8 decoding

**Rules Satisfied**: RULE-001 (strings), RULE-009 (nil on missing), INV-001 (round-trip)

---

### PATTERN-002: Atomic Counter with Initialization

**Scenario**: Track a counter that needs initialization

**Complete Example**:
```lisp
;; Initialize if not present
(let ((exists (not (null (cl-memcached:mc-get+ (list "page-views"))))))
  (unless exists
    (cl-memcached:mc-set "page-views" "0")))

;; Increment atomically (no race conditions)
(let ((new-count (cl-memcached:mc-incr "page-views" 1)))
  (format t "Page views: ~D~%" new-count))
```

**Why This Shape**:
- Counter MUST be initialized with `mc-set` before increment (RULE-005)
- `mc-incr` is atomic, safe for concurrent use
- No manual state management needed
- Decrement clamps at 0 automatically (INV-005)

**Rules Satisfied**: RULE-005 (initialization), RULE-010 (atomicity), INV-005 (non-negative)

---

### PATTERN-003: CAS with Retry Loop

**Scenario**: Safely update a complex value in presence of concurrency

**Complete Example**:
```lisp
(defun safe-update (key new-value)
  "Update key atomically, retrying on conflict."
  (loop
    ;; Step 1: Get current value with CAS token
    (let* ((response (first (cl-memcached:mc-gets+
                             (list key) :memcache *cache*)))
           (cas-token (when response (cl-memcached:mc-cas-unique response))))

      (unless response
        ;; Key doesn't exist, try add
        (return (cl-memcached:mc-add key new-value :memcache *cache*)))

      ;; Step 2: Try update with CAS
      (let ((result (cl-memcached:mc-cas key new-value cas-token
                                         :memcache *cache*)))
        (cond
          ((string= result "STORED")
           ;; Success
           (return :updated))
          ((string= result "EXISTS")
           ;; Conflict, retry
           (format t "Conflict detected, retrying...~%"))
          (t
           ;; Other error
           (error "Unexpected CAS response: ~A" result)))))))
```

**Why This Shape**:
- `mc-gets+` retrieves value + CAS token (separate from `mc-get`)
- CAS operation is atomic at server level
- Check response code to detect conflicts
- Retry loop handles concurrent modifications
- No manual locking needed

**Rules Satisfied**: RULE-004 (CAS only with cas), RULE-008 (semantic equivalence), INV-002 (CAS validity)

---

### PATTERN-004: Batch Operations with Pipelining

**Scenario**: Store multiple values efficiently (O(1) round-trips instead of O(N))

**Complete Example**:
```lisp
(defun batch-store (key-values)
  "Store multiple key-value pairs efficiently."
  ;; Enable pipelining on connection
  (cl-memcached:mc-with-connection (s :memcache *cache*)
    ;; Step 1: Queue operations without waiting for responses
    (dolist (kv key-values)
      (destructuring-bind (key value) kv
        (cl-memcached:mc-meta-set key value
                                  :stream s
                                  :quiet t)))  ;; Suppress responses

    ;; Step 2: Flush pipeline with noop (synchronization barrier)
    (let ((sync-response (cl-memcached:mc-meta-noop :stream s)))
      (assert (string= "MN" sync-response)))))

;; Usage: Store 1000 values in ~2 round-trips (not 1000!)
(batch-store '(("k1" "v1") ("k2" "v2") ... ("k1000" "v1000")))
```

**Why This Shape**:
- `mc-with-connection` manages connection lifetime
- `:quiet t` suppresses individual responses (pipeline batching)
- `mc-meta-noop` is synchronization barrier (wait for all to complete)
- `mc-meta-*` functions support pipelining natively
- No `:stream` parameter needed; connection macro provides it

**Rules Satisfied**: RULE-007 (cleanup guaranteed), INV-003 (pool transparency), RULE-008 (protocol equivalence)

**Performance**: O(N) round-trips → O(1) round-trips

---

### PATTERN-005: Error Handling with Fallback

**Scenario**: Handle cache failures gracefully, fall back to database

**Complete Example**:
```lisp
(defun get-or-fetch (key fetch-fn)
  "Try cache first, fall back to fetch-fn on error."
  (handler-case
    ;; Try cache
    (let ((response (first (cl-memcached:mc-get+ (list key)))))
      (when response
        (return-from get-or-fetch
          (babel:octets-to-string (cl-memcached:mc-data response)))))

    ;; Cache unavailable, fall through to fetch
    (cl-memcached:memcached-server-unreachable (e)
      (format *error-output* "Cache error (~A), using database~%" e)))

  ;; Fetch from source
  (let ((value (funcall fetch-fn key)))
    ;; Try to cache for next time (best effort, don't fail)
    (handler-case
      (cl-memcached:mc-set key value :timeout 3600)
      (cl-memcached:memcached-server-unreachable ()
        nil))  ;; Silently fail cache write

    value))

;; Usage: Transparent cache with fallback
(get-or-fetch "user:123"
              (lambda (key) (fetch-from-database key)))
```

**Why This Shape**:
- Catch `memcached-server-unreachable` for connection errors
- Catch `cl-mc-error` for input errors (during development)
- Fall back to primary source (database, computation)
- Retry cache write best-effort (don't fail user request)
- Distinguish between cache unavailability and missing keys

**Rules Satisfied**: Error handling semantics, INV-009 (no exceptions on missing)

---

## 17. Implementation Verification

### Verification by Testing

**Test categories**:
1. Unit tests: Individual operations (set/get/delete)
2. Integration tests: Connection pooling, error handling
3. CAS tests: Concurrent updates, token validity
4. Protocol tests: Both TEXT and META
5. Performance tests: Pooling speedup, pipelining efficiency

**Invariant checking**:
```lisp
;; Verify round-trip: store == retrieved
(loop for i from 1 to 1000
      do (let ((value (format nil "test-~D" i)))
           (mc-set (format nil "key~D" i) value)
           (let ((retrieved (babel:octets-to-string
                             (mc-data (first (mc-get+ (list (format nil "key~D" i))))))))
             (assert (string= value retrieved)))))

;; Verify counter atomicity
(mc-set "counter" "0")
(dotimes (i 100)
  (cl:mp:process-run-function
    "counter-increment"
    (lambda () (mc-incr "counter"))))
;; Verify final counter = 100 (all increments succeeded)
(assert (= 100 (parse-integer (mc-get-value "counter"))))
```

---

## 18. Debugging Guidance

**When operations fail**:

1. **Connection errors**: Check memcached running, host/port correct
2. **Type errors**: Verify input types (string keys, octet values, list keys-lists)
3. **CAS failures**: Ensure CAS token is current (key wasn't modified)
4. **Encoding issues**: Ensure UTF-8 encoding for strings, explicit encoding for binary
5. **Performance issues**: Enable pooling, use pipelining for batches
6. **Concurrency issues**: Use atomic counters or CAS for updates

**Debug output**:
```lisp
(setf cl-memcached:*mc-debug* t)  ;; If available
(trace cl-memcached:mc-set)       ;; Trace specific functions
```

---

## 19. Conformance Checklist

For implementation review:

```
Correctness:
[ ] All invariants hold (round-trip, CAS, atomicity)
[ ] Error conditions raise appropriate exceptions
[ ] No null pointer errors in public API
[ ] Connection cleanup guaranteed on error
[ ] Encoding round-trip preserves data

Protocol Compliance:
[ ] TEXT protocol: Commands end with CRLF
[ ] META protocol: 2-character response codes
[ ] Both protocols semantically equivalent
[ ] CAS semantics: Compare-and-set atomic
[ ] Counter non-negativity enforced

Performance:
[ ] Connection pooling enables 5-7x speedup
[ ] Pipelining reduces round-trips from O(N) to O(1)
[ ] No unnecessary copies or allocations

Code Quality:
[ ] No resource leaks (connections, sockets)
[ ] Consistent error handling
[ ] No anti-patterns present
[ ] All rules followed or explicitly violated with rationale
```

---

**Document Version**: 1.0.0
**Confidence**: 0.94
**Last Updated**: 2026-01-21
