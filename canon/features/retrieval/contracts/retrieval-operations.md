# Retrieval Operations Contract

[DRAFT - Extracted via triangulation]

## Overview

Retrieval operations fetch data from memcached, with variants for raw access, structured responses, and convenience wrappers.

**Source files**: cl-memcached.lisp:249-321
**Confidence**: 0.95 (convergent: code ∩ docs ∩ tests)

---

## mc-get

**Signature**:
```lisp
(mc-get keys-list
        &key (memcache *memcache*)
             (mc-use-pool *mc-use-pool*))
```

**Purpose**: Retrieve values for one or more keys (low-level, returns raw lists)

**Parameters**:
- `keys-list` (list of strings): Keys to retrieve (MUST be a list, even for single key)
- `memcache`: Memcache instance
- `mc-use-pool`: Use connection pool

**Returns**: List of lists, one per found key:
```lisp
((key flags bytes cas-unique data-raw) ...)
```
Where:
- `key` (string): The cache key
- `flags` (string): Server-stored flags
- `bytes` (integer): Data length
- `cas-unique`: Always nil for mc-get (use mc-gets for CAS)
- `data-raw` ((unsigned-byte 8) array): Raw octet data

**Not-found behavior**: Keys not found are simply omitted from result list

**Example**:
```lisp
(mc-get (list "key1" "key2"))
;; => (("key1" "0" 5 NIL #(104 101 108 108 111))
;;     ("key2" "0" 5 NIL #(119 111 114 108 100)))
```

**Source**:
- Implementation: cl-memcached.lisp:267-268
- Internal: mc-get-internal (cl-memcached.lisp:249-265)
- Documentation: README.md:47-49
- Tests: tests.lisp (multiple uses)

**Confidence**: 0.95

---

## mc-gets

**Signature**:
```lisp
(mc-gets keys-list
         &key (memcache *memcache*)
              (mc-use-pool *mc-use-pool*))
```

**Purpose**: Retrieve values WITH CAS (Check-And-Set) unique identifiers

**Parameters**: Same as `mc-get`

**Returns**: Same structure as `mc-get`, but `cas-unique` field is populated:
```lisp
((key flags bytes cas-unique data-raw) ...)
```

**Key difference from mc-get**: Includes CAS token for subsequent `mc-cas` operations

**Example**:
```lisp
(mc-gets (list "mykey"))
;; => (("mykey" "0" 11 "12345678" #(104 101 108 108 111 ...)))
;;                       ^^^^^^^^^
;;                       CAS token (not nil)
```

**Source**:
- Implementation: cl-memcached.lisp:270-271
- Documentation: README.md (implicit in mc-gets+ docs)
- Tests: tests.lisp:40-54, 56-66

**Confidence**: 0.95

**Note**: Added in commit 71fd866 as "missing memcached command"

---

## mc-get+

**Signature**:
```lisp
(mc-get+ key-or-list-of-keys
         &key (memcache *memcache*)
              (mc-use-pool *mc-use-pool*))
```

**Purpose**: Retrieve values as structured `memcache-response` objects (convenience wrapper)

**Parameters**:
- `key-or-list-of-keys`: Single key (string) OR list of keys
- Other parameters same as `mc-get`

**Returns**:
- If single key provided: Single `memcache-response` structure (or nil if not found)
- If list provided: List of `memcache-response` structures

**Return type**: `memcache-response` structure with slots:
- `(mc-key response)` → key (string)
- `(mc-flags response)` → flags (string)
- `(mc-bytes response)` → data length (fixnum)
- `(mc-cas-unique response)` → CAS token (nil for mc-get+)
- `(mc-data-raw response)` → raw octets

**Example**:
```lisp
(mc-get+ "mykey")
;; => #<MEMCACHED-RESPONSE Key:mykey Data-Length:11>

(mc-get+ '("key1" "key2"))
;; => (#<MEMCACHED-RESPONSE Key:key1 Data-Length:5>
;;     #<MEMCACHED-RESPONSE Key:key2 Data-Length:5>)
```

**Source**:
- Implementation: cl-memcached.lisp:297-305
- Documentation: README.md:53-57
- Tests: tests.lisp:56-66, 148-169

**Confidence**: 0.98 (excellent documentation and tests)

---

## mc-gets+

**Signature**:
```lisp
(mc-gets+ key-or-list-of-keys
          &key (memcache *memcache*)
               (mc-use-pool *mc-use-pool*))
```

**Purpose**: Like `mc-get+` but includes CAS tokens for concurrent updates

**Parameters**: Same as `mc-get+`

**Returns**: Same as `mc-get+`, but `mc-cas-unique` slot is populated

**Key difference from mc-get+**: Includes CAS token in response structure

**Typical usage pattern**:
```lisp
;; 1. Get value with CAS token
(let* ((response (mc-gets+ "counter"))
       (cas (mc-cas-unique response))
       (old-value (parse-integer (mc-data response))))
  ;; 2. Compute new value
  (let ((new-value (1+ old-value)))
    ;; 3. Update with CAS (safe from concurrent modifications)
    (mc-cas "counter" (princ-to-string new-value) cas)))
```

**Source**:
- Implementation: cl-memcached.lisp:307-315
- Documentation: README.md:55-57
- Tests: tests.lisp:56-66, 68-94

**Confidence**: 0.98

---

## mc-data

**Signature**:
```lisp
(mc-data response
         &key (external-format *mc-default-encoding*))
```

**Purpose**: Convert raw octets from `memcache-response` to string

**Parameters**:
- `response`: A `memcache-response` structure
- `external-format`: Babel encoding (default UTF-8)

**Returns**: String decoded from `mc-data-raw`, or nil if response is not a `memcache-response`

**Example**:
```lisp
(let ((response (mc-get+ "mykey")))
  (mc-data response))
;; => "hello world"

;; With custom encoding
(mc-data response :external-format (babel:make-external-format :latin-1))
```

**Source**:
- Implementation: cl-memcached.lisp:292-294
- Documentation: README.md:61-63
- Tests: tests.lisp:163-168

**Confidence**: 0.95

---

## mc-get-value

**Signature**:
```lisp
(mc-get-value key
              &key (memcache *memcache*)
                   (mc-use-pool *mc-use-pool*)
                   (external-format *mc-default-encoding*))
```

**Purpose**: One-step convenience: get key and return decoded string value

**Parameters**:
- `key` (string): Single key to retrieve
- Other parameters as above

**Returns**: String value, or nil if key not found

**Implementation**: Combines `mc-get+` and `mc-data`:
```lisp
(mc-data (mc-get+ key ...) :external-format external-format)
```

**Example**:
```lisp
(mc-get-value "mykey")
;; => "hello world"
```

**Warning**: Docstring incorrectly says "macro" - it's a function

**Source**:
- Implementation: cl-memcached.lisp:318-320
- Documentation: README.md:67-69
- Tests: tests.lisp:166-167

**Confidence**: 0.92 (minor documentation error in docstring)

---

## Internal: mc-get-internal

**Signature**:
```lisp
(mc-get-internal command keys-list
                 &key (memcache *memcache*)
                      (mc-use-pool *mc-use-pool*))
```

**Purpose**: Internal function implementing both `mc-get` and `mc-gets`

**Parameters**:
- `command`: Protocol command string ("get" or "gets")
- `keys-list`: Must be a list (validated with error)
- Other parameters as above

**Implementation details**:
- Validates `keys-list` is a list (raises `cl-mc-error` if not)
- Constructs protocol command: `get key1 key2 key3\r\n`
- Parses response lines until "END"
- For "gets" command, extracts CAS token from 5th field

**Protocol wire format**:
```
get key1 key2\r\n
VALUE key1 0 11 12345678\r\n
hello world\r\n
VALUE key2 0 5\r\n
world\r\n
END\r\n
```

**Error handling**: Raises `cl-mc-error` if `keys-list` is not a list

**Source**: cl-memcached.lisp:249-265

**Confidence**: 0.95

---

## Response Structure Details

### memcache-response Structure

**Definition**: cl-memcached.lisp:277-289

**Slots**:
```lisp
(key ""        :type simple-string :read-only t)
(flags ""      :read-only t)
(bytes 0       :type fixnum :read-only t)
(cas-unique "" :read-only t)  ; populated by mc-gets+, nil for mc-get+
(data-raw nil  :type (array (unsigned-byte 8)) :read-only t)
```

**Accessor prefix**: All accessors use `mc-` prefix
- `(mc-key response)`
- `(mc-flags response)`
- `(mc-bytes response)`
- `(mc-cas-unique response)`
- `(mc-data-raw response)`

**Print representation**:
```lisp
#<MEMCACHED-RESPONSE Key:mykey Data-Length:11>
```

**Confidence**: 0.98 (well-tested structure)

---

## Function Family Comparison

| Function | Input | Output | CAS Token | Encoding | Use Case |
|----------|-------|--------|-----------|----------|----------|
| `mc-get` | List | Raw lists | No | Manual | Low-level access |
| `mc-gets` | List | Raw lists | Yes | Manual | Low-level with CAS |
| `mc-get+` | Key or list | Structure(s) | No | Manual | Structured access |
| `mc-gets+` | Key or list | Structure(s) | Yes | Manual | Structured with CAS |
| `mc-data` | Response | String | N/A | Auto | Decode helper |
| `mc-get-value` | Key | String | No | Auto | One-step convenience |

**Recommendation**: Use `mc-get-value` for simple cases, `mc-gets+` when CAS is needed

---

## Performance Characteristics

**Batch retrieval**: `mc-get` and `mc-get+` support retrieving multiple keys in a single network round-trip:

```lisp
;; EFFICIENT: Single network call
(mc-get+ '("key1" "key2" "key3" "key4" "key5"))

;; INEFFICIENT: 5 network calls
(mapcar #'mc-get+ '("key1" "key2" "key3" "key4" "key5"))
```

**Connection pooling impact**: Same as storage operations (~7x speedup on SBCL)

**Confidence**: 0.90 (inferred from architecture)

---

## Error Handling

**Invalid input**:
```lisp
(mc-get "not-a-list")  ; ERROR: cl-mc-error "KEYS-LIST has to be a LIST of keys"
```

**Key not found**: Returns empty list or nil (no error raised)

**Connection errors**: Raises `memcached-server-unreachable` condition

**Confidence**: 0.92

---

## Protocol Semantics

### Key Not Found Behavior

```lisp
;; Request non-existent key
(mc-get (list "nosuchkey"))
;; => NIL

(mc-get+ "nosuchkey")
;; => NIL

(mc-get-value "nosuchkey")
;; => NIL
```

**No exception raised** - absence indicated by nil/empty result

---

### Multiple Keys with Some Missing

```lisp
(mc-get (list "exists1" "missing" "exists2"))
;; => (("exists1" "0" 5 NIL #(...))
;;     ("exists2" "0" 5 NIL #(...)))
;;
;; Note: "missing" is simply omitted, no error or placeholder
```

**Confidence**: 0.95 (matches memcached protocol spec)

---

## Observations

**obs-retrieval-001** (convergent):
- Elegant function family design
- Low-level (`mc-get`) → structured (`mc-get+`) → convenience (`mc-get-value`)
- Clear separation of concerns

**obs-retrieval-002** (convergent):
- `mc-gets` added in commit 71fd866 as "missing command"
- Integrated seamlessly with existing API
- Test coverage excellent (4 dedicated tests)

**obs-retrieval-003** (minor issue):
- `mc-get-value` docstring says "macro" but it's a function
- Does not affect behavior, only documentation

**obs-retrieval-004** (design note):
- Smart return type: single response for single key, list for multiple keys
- Makes API ergonomic for common cases

---

_Extraction confidence: 0.95 (weighted average)_
_Triangulation: code ∩ docs ∩ tests_
_Test coverage: Excellent (retrieval operations heavily tested)_
