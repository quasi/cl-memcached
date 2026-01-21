# Storage Operations Contract

[DRAFT - Extracted via triangulation]

## Overview

Storage operations write data to memcached with various semantics (unconditional, conditional, append/prepend).

**Source files**: cl-memcached.lisp:152-216
**Confidence**: 0.95 (convergent: code ∩ docs ∩ tests)

---

## mc-set

**Signature**:
```lisp
(mc-set key data
        &key (memcache *memcache*)
             (timeout 0)
             (flags 0)
             (noreply nil)
             (external-format *mc-default-encoding*)
             (mc-use-pool *mc-use-pool*))
```

**Purpose**: Unconditionally store data for a key

**Parameters**:
- `key` (simple-string): Cache key
- `data` (string or (unsigned-byte 8) array): Data to store
- `memcache`: Memcache instance (defaults to `*memcache*`)
- `timeout` (fixnum): Expiration in seconds (0 = never expire)
- `flags` (fixnum): Application-specific flags stored with data
- `noreply` (boolean): If t, server doesn't send response
- `external-format`: Babel encoding for string data
- `mc-use-pool`: Use connection pool

**Returns**:
- `"STORED"` on success
- `:INTERNAL` (second value) indicates internal formatting

**Side effects**: Creates or overwrites key in memcached

**Source**:
- Implementation: cl-memcached.lisp:211
- Documentation: README.md:34-37
- Tests: tests.lisp:46, 73, 109, 159

**Confidence**: 0.98

---

## mc-add

**Signature**: Same as `mc-set`

**Purpose**: Store data ONLY if key does NOT already exist

**Returns**:
- `"STORED"` if key didn't exist
- `"NOT_STORED"` if key already exists

**Source**:
- Implementation: cl-memcached.lisp:212
- Documentation: README.md:37
- Tests: (inferred from mc-set tests)

**Confidence**: 0.90

---

## mc-replace

**Signature**: Same as `mc-set`

**Purpose**: Store data ONLY if key DOES already exist

**Returns**:
- `"STORED"` if key existed
- `"NOT_STORED"` if key didn't exist

**Source**:
- Implementation: cl-memcached.lisp:213
- Documentation: README.md:37

**Confidence**: 0.90

---

## mc-append

**Signature**: Same as `mc-set`

**Purpose**: Append data to existing value (raw concatenation)

**Returns**:
- `"STORED"` if key existed and data appended
- `"NOT_STORED"` if key didn't exist

**Implementation note**: Server performs byte-level concatenation (existing_data + new_data)

**Source**:
- Implementation: cl-memcached.lisp:214
- Documentation: README.md:37

**Confidence**: 0.88

---

## mc-prepend

**Signature**: Same as `mc-set`

**Purpose**: Prepend data to existing value (raw concatenation)

**Returns**:
- `"STORED"` if key existed and data prepended
- `"NOT_STORED"` if key didn't exist

**Implementation note**: Server performs byte-level concatenation (new_data + existing_data)

**Source**:
- Implementation: cl-memcached.lisp:215
- Documentation: README.md:37

**Confidence**: 0.88

---

## mc-cas (Check-And-Set)

**Signature**:
```lisp
(mc-cas key data cas-unique
        &key (memcache *memcache*)
             (timeout 0)
             (flags 0)
             (noreply nil)
             (external-format *mc-default-encoding*)
             (mc-use-pool *mc-use-pool*))
```

**Purpose**: Store data ONLY if CAS token matches (no concurrent modification)

**Parameters**:
- `key` (simple-string): Cache key
- `data`: Data to store
- `cas-unique`: CAS token from previous `mc-gets`
- Other parameters same as `mc-set`

**Returns**:
- `"STORED"` if CAS token matched
- `"EXISTS"` if CAS token didn't match (concurrent modification)
- `"NOT_FOUND"` if key doesn't exist

**Typical usage pattern**:
```lisp
;; 1. Get with CAS token
(let* ((response (mc-gets+ "mykey"))
       (cas (mc-cas-unique response))
       (old-value (mc-data response)))
  ;; 2. Modify value
  (let ((new-value (compute-new-value old-value)))
    ;; 3. Try to store with CAS
    (mc-cas "mykey" new-value cas)))
```

**Source**:
- Implementation: cl-memcached.lisp:221-226
- Documentation: README.md:41-43
- Tests: tests.lisp:68-94, 182-210

**Confidence**: 0.98 (comprehensive test coverage)

---

## Internal: mc-store

**Signature**:
```lisp
(mc-store key data
          &key (memcache *memcache*)
               (command :set)
               (timeout 0)
               (flags 0)
               (noreply nil)
               (cas-unique nil)
               (mc-use-pool *mc-use-pool*))
```

**Purpose**: Internal function implementing all storage commands

**Parameter notes**:
- `data` MUST be (unsigned-byte 8) array (not string)
- `command` is keyword: :set, :add, :replace, :append, :prepend, :cas

**Implementation details**:
- Constructs memcached protocol command string
- Uses `mc-with-pool-y/n` macro for connection management
- Sends: `<command> <key> <flags> <timeout> <bytes>\r\n<data>\r\n`
- Reads response line (unless noreply=t)

**Source**: cl-memcached.lisp:152-196

**Confidence**: 0.95

---

## Protocol Wire Format

Storage commands use this format:
```
<command> <key> <flags> <exptime> <bytes> [cas-unique] [noreply]\r\n
<data block>\r\n
```

**Example**:
```
set mykey 0 3600 11\r\n
hello world\r\n
```

**Source**: memcached protocol specification, cl-memcached.lisp:190-194

---

## Error Handling

**Connection errors**: Raises `memcached-server-unreachable` condition

**Invalid data type**: Raises `cl-mc-error` with message "Data has to be a ARRAY with ELEMENT-TYPE of (UNSIGNED-BYTE 8)"

**CAS misuse**: Raises `cl-mc-error` with message "CAS-UNIQUE is only used with the CAS command"

**Source**: cl-memcached.lisp:175-178

**Confidence**: 0.92

---

## Performance Characteristics

**With pooling** (`*mc-use-pool*` = t):
- SBCL: ~0.713s per 10,000 operations (SET)
- CCL: ~0.847s per 10,000 operations
- CMUCL: ~0.970s per 10,000 operations

**Without pooling**:
- SBCL: ~4.942s per 10,000 operations
- **Speedup**: ~7x with pooling

**Source**: README.md:209-216, benchmark data
**Test configuration**: 1KB payload per operation

**Confidence**: 0.95 (verified benchmarks)

---

## Observations

**obs-contract-001** (convergent):
- All five basic storage commands implemented via macro `mc-make-command`
- Macro ensures consistent parameter handling
- Clean separation: public functions handle encoding, internal `mc-store` handles protocol

**obs-contract-002** (convergent):
- CAS operation is well-tested (4 comprehensive tests)
- Tests verify both success and failure cases
- Documentation matches implementation exactly

**obs-contract-003** (inferred from code):
- Append/prepend operations have minimal test coverage
- Documentation is brief
- Confidence lower than set/cas operations

---

_Extraction confidence: 0.94 (weighted average)_
_Triangulation: code ∩ docs ∩ tests_
