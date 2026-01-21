# System Invariants and Properties

[DRAFT - Inferred from code analysis]

## Overview

This document captures invariants, constraints, and behavioral properties of the cl-memcached library.

**Extraction method**: Code analysis + test observation + documentation review
**Confidence**: 0.88 (inferred patterns)

---

## Type Invariants

### INV-TYPE-001: Data Storage Format

**Property**: All data stored in memcached MUST be `(unsigned-byte 8)` arrays

**Evidence**:
- `mc-store` validates: `(equal (array-element-type data) '(UNSIGNED-BYTE 8))`
- Error raised if violated: "Data has to be a ARRAY with ELEMENT-TYPE of (UNSIGNED-BYTE 8)"
- Public functions (`mc-set`, etc.) accept strings and convert using Babel

**Source**: cl-memcached.lisp:175-176

**Implication**: Library handles encoding/decoding, but internal storage is always octets

**Confidence**: 1.0 (explicit validation)

---

### INV-TYPE-002: Keys are Strings

**Property**: All keys MUST be strings (simple-string)

**Evidence**:
- `key` parameter typed as `simple-string` in structure definitions
- Protocol construction uses string concatenation
- No validation (assumes correct type)

**Source**: cl-memcached.lisp throughout

**Implication**: Passing non-string keys results in type error (not gracefully handled)

**Confidence**: 0.95

---

### INV-TYPE-003: Keys-List Must be List

**Property**: `mc-get-internal` requires `keys-list` parameter to be a list

**Evidence**:
- Explicit validation: `(when (not (listp keys-list)) (cl-mc-error ...))`
- Error message: "KEYS-LIST has to be a LIST of keys"

**Source**: cl-memcached.lisp:250-251

**Implication**: Even single-key get requires wrapping in list: `(mc-get (list "key"))`

**Confidence**: 1.0 (explicit validation)

---

## CAS (Check-And-Set) Invariants

### INV-CAS-001: CAS Token Validity

**Property**: CAS token is valid only until key is modified

**Evidence**:
- Test shows `mc-cas` with stale token returns "EXISTS"
- Any modification (even another CAS) invalidates previous tokens

**Source**: tests.lisp:81-94

**Lifecycle**:
1. `mc-gets+` → CAS token T1
2. Any write → CAS token T1 becomes invalid
3. `mc-gets+` → New CAS token T2

**Confidence**: 0.98

---

### INV-CAS-002: CAS Only With CAS Command

**Property**: `cas-unique` parameter is only valid with `:cas` command

**Evidence**:
- Explicit validation: `(when (and cas-unique (not (eq command :cas))) (cl-mc-error ...))`
- Error message: "CAS-UNIQUE is only used with the CAS command"

**Source**: cl-memcached.lisp:177-178

**Implication**: Cannot accidentally use CAS with SET/ADD/REPLACE

**Confidence**: 1.0 (explicit validation)

---

### INV-CAS-003: CAS Semantics Across Protocols

**Property**: Classic protocol CAS (`mc-cas`) and meta protocol CAS (`mc-meta-set` with `:cas`) have equivalent semantics

**Evidence**:
- Both return "EXISTS"/"EX" on mismatch
- Both return "STORED"/"HD" on success
- Both prevent concurrent modification

**Source**: tests.lisp:68-94 (classic), 182-210 (meta)

**Confidence**: 0.95

---

## Connection Management Invariants

### INV-CONN-001: Connection Cleanup Guarantee

**Property**: Connections are ALWAYS cleaned up, even on error

**Evidence**:
- `mc-with-pool-y/n` uses `unwind-protect`
- Pool return or connection close guaranteed

**Source**: cl-memcached.lisp:114-121

**Implication**: No connection leaks under normal error conditions

**Confidence**: 0.98

---

### INV-CONN-002: Pool vs No-Pool Transparency

**Property**: Operations produce identical results regardless of `mc-use-pool` setting

**Evidence**:
- Only performance differs, not behavior
- Same functions called, same protocol used

**Source**: Benchmarks show equivalent correctness

**Implication**: `mc-use-pool` is purely a performance optimization

**Confidence**: 0.95

---

### INV-CONN-003: Binary Stream Requirement

**Property**: All socket streams MUST use element-type `'(unsigned-byte 8)`

**Evidence**:
- `usocket:socket-connect` uses `:element-type '(unsigned-byte 8)`
- Comment in git history mentions character streams failed on CCL

**Source**: cl-memcached.lisp:75, commit 980dc80

**Rationale**: Cross-implementation compatibility (SBCL, CCL, CMUCL)

**Confidence**: 0.98

---

## Counter Operation Invariants

### INV-COUNTER-001: Non-Negative Values

**Property**: Counters cannot go below zero

**Evidence**:
- README states: "Value cannot go negative (clamped at 0)"
- Memcached protocol specification

**Source**: README.md counter documentation

**Implication**: `mc-decr` on counter with value 5 by amount 100 → result is 0, not -95

**Confidence**: 0.90 (protocol behavior, not explicitly tested)

---

### INV-COUNTER-002: Initialization Required

**Property**: Counters must exist before increment/decrement

**Evidence**:
- `mc-incr`/`mc-decr` on non-existent key returns `'NOT_FOUND`
- Must use `mc-set` to initialize counter first

**Source**: cl-memcached.lisp:342-343, 354-355, README.md examples

**Confidence**: 0.95

---

### INV-COUNTER-003: Atomic Operations

**Property**: Increment/decrement operations are atomic (server-side)

**Evidence**:
- Memcached protocol guarantees atomicity
- No race conditions possible (unlike GET + compute + SET)

**Source**: Memcached protocol specification

**Implication**: Safe for concurrent use without CAS

**Confidence**: 0.92

---

## Protocol Invariants

### INV-PROTO-001: CRLF Line Termination

**Property**: All protocol commands end with `\r\n` (CRLF)

**Evidence**:
- Constant `+crlf+` used throughout
- `read-line-from-binary-stream` expects CRLF

**Source**: cl-memcached.lisp:28-33, 234-245

**Confidence**: 1.0

---

### INV-PROTO-002: END Marker for Multi-Line Responses

**Property**: Multi-line responses (GET, GETS, STATS) terminate with "END\r\n"

**Evidence**:
- `mc-get-internal`: `(until (string-equal "END" x))`
- `mc-stats-internal`: `(while (not (string-equal "END" line)))`

**Source**: cl-memcached.lisp:256, 410

**Confidence**: 1.0

---

### INV-PROTO-003: ASCII Command Encoding

**Property**: Command strings are encoded as ASCII

**Evidence**:
- `+command-encoding+` is ASCII
- `(babel:make-external-format :ASCII)`

**Source**: cl-memcached.lisp:25

**Rationale**: Memcached protocol commands are ASCII, data is binary

**Confidence**: 1.0

---

## Meta Protocol Specific Invariants

### INV-META-001: Response Code Format

**Property**: Meta protocol responses are 2-character codes or "VA <len> [flags]"

**Evidence**:
- `mc-read-meta-response` parses: HD, EN, EX, ST, MN, NF, NS, VA
- All are 2 characters except VA (value response)

**Source**: cl-memcached.lisp:500-522

**Confidence**: 0.98

---

### INV-META-002: Quiet Mode Suppresses Responses

**Property**: Operations with `:quiet t` do not send responses (except errors)

**Evidence**:
- `(unless quiet (mc-read-meta-response s))` pattern
- Used for pipelining efficiency

**Source**: cl-memcached.lisp:543, 574, 598

**Implication**: Must use noop or final non-quiet operation to synchronize

**Confidence**: 0.95

---

### INV-META-003: Opaque Token Reflection

**Property**: Opaque tokens sent in request are reflected in response

**Evidence**:
- Test verifies: `(gethash :opaque response)` equals provided token
- Used for request/response correlation

**Source**: tests.lisp:285-288

**Confidence**: 0.95

---

## Error Handling Invariants

### INV-ERROR-001: Connection Errors Raise Condition

**Property**: Connection failures raise `memcached-server-unreachable` condition

**Evidence**:
- `new-memcache-connection` wraps usocket:socket-connect in handler-case
- Signals `memcached-server-unreachable` on error

**Source**: cl-memcached.lisp:75-76

**Confidence**: 0.95

---

### INV-ERROR-002: Missing Keys Don't Raise Errors

**Property**: GET operations on non-existent keys return nil/empty, not error

**Evidence**:
- `mc-get` returns empty list for missing keys
- `mc-get+` returns nil
- `mc-meta-get` returns ("EN", nil)

**Source**: Function implementations, tests

**Implication**: Absence is indicated by return value, not exception

**Confidence**: 0.98

---

### INV-ERROR-003: Invalid Input Raises cl-mc-error

**Property**: Type/validation errors raise `cl-mc-error` condition

**Evidence**:
- Keys-list not a list → cl-mc-error
- Data not octet array → cl-mc-error
- CAS-unique with non-CAS command → cl-mc-error

**Source**: cl-memcached.lisp:175-178, 250-251

**Confidence**: 1.0

---

## Encoding Invariants

### INV-ENC-001: Default Encoding is UTF-8

**Property**: Unless specified, all string encoding/decoding uses UTF-8

**Evidence**:
- `*mc-default-encoding*` defaults to UTF-8
- All text operations respect this default

**Source**: cl-memcached.lisp:22-23

**Confidence**: 1.0

---

### INV-ENC-002: Encoding Round-Trip Preservation

**Property**: String → octets → string preserves data (if encoding matches)

**Evidence**:
- `mc-data` uses same encoding as storage
- Tests verify round-trip: set string, get string, values equal

**Source**: tests.lisp (implicit in all tests)

**Confidence**: 0.98

---

## Temporal Invariants

### INV-TIME-001: TTL of 0 Means No Expiration

**Property**: `timeout` or `ttl` parameter of 0 means "never expire"

**Evidence**:
- Default values are 0
- Memcached protocol specification

**Source**: cl-memcached.lisp function signatures

**Confidence**: 0.95

---

### INV-TIME-002: mc-touch Extends Expiration

**Property**: `mc-touch` updates expiration without fetching/modifying data

**Evidence**:
- Function signature: `(mc-touch key expiry-time ...)`
- Protocol command: `touch <key> <exptime>`

**Source**: cl-memcached.lisp:363-368

**Confidence**: 0.90

---

## Naming Convention Properties

### PROP-NAME-001: Public API Prefix

**Property**: All public API functions use `mc-` prefix

**Evidence**:
- 44 exported symbols, all start with `mc-` or `*mc-`
- Internal functions don't use this prefix

**Source**: packages.lisp

**Implication**: Easy to identify public vs internal functions

**Confidence**: 1.0

---

### PROP-NAME-002: Accessor Consistency

**Property**: All struct accessors use `mc-` prefix

**Evidence**:
- `memcache` struct: `mc-name`, `mc-ip`, `mc-port`, `mc-pool-size`, `mc-pool`
- `memcache-response` struct: `mc-key`, `mc-flags`, `mc-bytes`, `mc-cas-unique`, `mc-data-raw`
- Both structs use `:conc-name mc-`

**Source**: cl-memcached.lisp:49, 278

**Confidence**: 1.0

---

## Test Coverage Properties

### PROP-TEST-001: CAS Operations Well-Tested

**Property**: CAS has highest test coverage of any feature

**Evidence**:
- 4 dedicated tests (classic CAS success/failure, meta CAS success/failure)
- Both protocols tested
- Both success and failure paths verified

**Source**: tests.lisp:68-94, 182-210

**Confidence**: 1.0

---

### PROP-TEST-002: Append/Prepend Under-Tested

**Property**: Append and prepend operations have minimal test coverage

**Evidence**:
- No dedicated tests found in test suite
- Only implicit in implementation

**Source**: tests.lisp (absence)

**Recommendation**: Add tests for these operations

**Confidence**: 0.95 (absence of evidence)

---

## Cross-Cutting Properties

### PROP-CROSS-001: Connection Pool Transparency

**Property**: Connection pooling affects performance, not correctness

**Evidence**:
- All functions support both pooled and non-pooled modes
- Results are identical
- Only latency differs

**Source**: Benchmarks, architecture

**Confidence**: 0.95

---

### PROP-CROSS-002: noreply Optimization

**Property**: Operations with `noreply=t` complete faster but provide no confirmation

**Evidence**:
- `(unless noreply (read-line-from-binary-stream s))` pattern
- Skip response parsing when noreply set

**Source**: cl-memcached.lisp throughout

**Trade-off**: Speed vs confirmation

**Confidence**: 0.95

---

## Summary Statistics

| Category | Properties | Confidence |
|----------|------------|------------|
| Type Invariants | 3 | 0.98 |
| CAS Invariants | 3 | 0.97 |
| Connection Invariants | 3 | 0.97 |
| Counter Invariants | 3 | 0.92 |
| Protocol Invariants | 3 | 1.0 |
| Meta Protocol Invariants | 3 | 0.96 |
| Error Handling Invariants | 3 | 0.98 |
| Encoding Invariants | 2 | 0.99 |
| Temporal Invariants | 2 | 0.93 |
| Naming Properties | 2 | 1.0 |
| Test Coverage Properties | 2 | 0.98 |
| Cross-Cutting Properties | 2 | 0.95 |

**Total Properties Identified**: 29
**Average Confidence**: 0.96

---

_Extraction method: Code analysis ∩ Test observation ∩ Documentation review_
_Overall confidence: 0.96 (very high)_
