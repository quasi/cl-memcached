# Counter Operations Contract

[DRAFT - Extracted via triangulation]

## Overview

Atomic counter operations for incrementing and decrementing numeric values stored in memcached.

**Source files**: cl-memcached.lisp:336-357
**Confidence**: 0.90 (code ∩ docs, limited test coverage)

---

## mc-incr

**Signature**:
```lisp
(mc-incr key
         &key (value 1)
              (noreply nil)
              (memcache *memcache*)
              (mc-use-pool *mc-use-pool*))
```

**Purpose**: Atomically increment a numeric counter

**Parameters**:
- `key` (string): Counter key
- `value` (integer, default 1): Amount to increment
- `noreply` (boolean): Suppress server response
- `memcache`, `mc-use-pool`: Standard connection parameters

**Returns**:
- New value (integer) on success
- `'NOT_FOUND` (symbol) if key doesn't exist
- Value cannot go negative (clamped at 0)

**Protocol**: Sends `incr <key> <value>\r\n`, receives `<newvalue>\r\n` or `NOT_FOUND\r\n`

**Example**:
```lisp
(mc-set "counter" "0")  ; Initialize counter
(mc-incr "counter")     ; => 1
(mc-incr "counter")     ; => 2
(mc-incr "counter" :value 10)  ; => 12
(mc-incr "nosuchkey")   ; => NOT_FOUND
```

**Behavior notes**:
- Key MUST exist before incrementing (use mc-set to initialize)
- Key value must be a decimal number string
- Atomic operation (thread-safe)
- Underflow protection: decrementing to below 0 sets value to 0

**Source**:
- Implementation: cl-memcached.lisp:336-344
- Documentation: README.md:79-82
- Tests: README.md:173-182 (examples, not formal tests)

**Confidence**: 0.90

---

## mc-decr

**Signature**:
```lisp
(mc-decr key
         &key (value 1)
              (noreply nil)
              (memcache *memcache*)
              (mc-use-pool *mc-use-pool*))
```

**Purpose**: Atomically decrement a numeric counter

**Parameters**: Same as `mc-incr`

**Returns**: Same as `mc-incr`
- New value (integer) on success
- `'NOT_FOUND` if key doesn't exist
- Clamped at 0 (cannot go negative)

**Protocol**: Sends `decr <key> <value>\r\n`, receives `<newvalue>\r\n` or `NOT_FOUND\r\n`

**Example**:
```lisp
(mc-set "counter" "10")  ; Initialize counter
(mc-decr "counter")      ; => 9
(mc-decr "counter")      ; => 8
(mc-decr "counter" :value 5)  ; => 3
(mc-decr "counter" :value 100) ; => 0 (clamped, not negative)
```

**Underflow behavior**: Server prevents negative values, returns 0

**Source**:
- Implementation: cl-memcached.lisp:348-356
- Documentation: README.md:85-87
- Tests: README.md:184-187 (examples)

**Confidence**: 0.90

---

## Usage Patterns

### Initialize Before Use

```lisp
;; CORRECT:
(mc-set "visits" "0")
(mc-incr "visits")  ; => 1

;; WRONG:
(mc-incr "visits")  ; => NOT_FOUND (key doesn't exist)
```

### Check for NOT_FOUND

```lisp
(let ((result (mc-incr "maybe-exists")))
  (if (eq result 'NOT_FOUND)
      ;; Initialize and retry
      (progn
        (mc-set "maybe-exists" "0")
        (mc-incr "maybe-exists"))
      ;; Use result
      result))
```

### Atomic Read-Modify-Write

Counters provide atomic updates without CAS:

```lisp
;; NO RACE CONDITIONS - atomic
(dotimes (i 100)
  (spawn-thread
    (lambda () (mc-incr "shared-counter"))))
;; Counter will be exactly 100

;; RACE CONDITIONS - non-atomic
(dotimes (i 100)
  (spawn-thread
    (lambda ()
      (let ((val (parse-integer (mc-get-value "shared-counter"))))
        (mc-set "shared-counter" (princ-to-string (1+ val)))))))
;; Counter will be < 100 due to lost updates
```

---

## Error Handling

**Key not found**: Returns `'NOT_FOUND` symbol (not an error condition)

**Non-numeric value**: Server behavior undefined (may return error or CLIENT_ERROR)

**Connection errors**: Raises `memcached-server-unreachable`

**Confidence**: 0.85

---

## Performance Characteristics

**Atomicity**: Server-side atomic operation (no race conditions)

**Overhead**: Similar to GET/SET operations

**Use case**: Ideal for counters, statistics, rate limiting

**Confidence**: 0.90

---

## Limitations

1. **Initialization required**: Key must exist before incr/decr
2. **Numeric values only**: Key value must be parseable as decimal number
3. **64-bit unsigned**: Server typically uses 64-bit unsigned integer
4. **No negative values**: Decrement clamped at 0

---

## Observations

**obs-counter-001** (coverage gap):
- Implementation and documentation are solid
- Limited formal test coverage (only examples in README)
- Recommendation: Add dedicated tests

**obs-counter-002** (convergent):
- Atomic behavior matches memcached protocol spec
- Simple, focused implementation
- Clear error semantics

---

_Extraction confidence: 0.90_
_Triangulation: code ∩ docs_
_Test coverage: Low (examples only, no formal tests)_
