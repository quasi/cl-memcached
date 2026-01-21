# Meta Protocol Basic Operation Scenarios

[DRAFT - Extracted from tests]

## Scenario: Basic mc-meta-set and mc-meta-get

**Source**: tests.lisp:152-169

**Given**:
- Key "meta-basic-test" does not exist
- Memcache connection is established

**When**:
1. Set key using `mc-meta-set "meta-basic-test" "hello meta protocol"`
2. Get key using `mc-meta-get "meta-basic-test"`

**Then**:
- `mc-meta-set` returns `"HD"` (success)
- `mc-meta-get` returns two values:
  1. Hash table with response data
  2. `t` (foundp = true)
- Hash table contains `:value` key with octet data
- Decoded value matches "hello meta protocol"

**Example code**:
```lisp
(let ((set-result (mc-meta-set "key" "data")))
  (assert (string= "HD" set-result)))

(multiple-value-bind (response foundp) (mc-meta-get "key")
  (assert foundp)
  (assert (hash-table-p response))
  (let ((value (gethash :value response)))
    (assert (string= "data" (babel:octets-to-string value)))))
```

**Confidence**: 0.95

---

## Scenario: mc-meta-get returns CAS when requested

**Source**: tests.lisp:171-180

**Given**:
- Key "meta-cas-test" is set to "cas-test-data"

**When**:
- Call `mc-meta-get "meta-cas-test" :cas t`

**Then**:
- `foundp` is `t`
- Response hash table contains `:cas` key
- CAS value is NOT nil

**Comparison**:
- Without `:cas t`: `:cas` key absent from response
- With `:cas t`: `:cas` key present with token

**Confidence**: 0.95

---

## Scenario: mc-meta-delete removes key

**Source**: tests.lisp:212-228

**Given**:
- Key "meta-delete-test" exists with value "to-be-deleted"

**When**:
1. Verify key exists with `mc-meta-get`
2. Delete key with `mc-meta-delete "meta-delete-test"`
3. Attempt to get key again

**Then**:
- Initial get: `foundp = t`
- Delete returns `"HD"` (success)
- Second get: `foundp = nil`

**Confidence**: 0.95

---

## Scenario: mc-meta-delete on nonexistent key

**Source**: tests.lisp:230-238

**Given**:
- Key "meta-delete-nonexistent-test" does NOT exist

**When**:
- Call `mc-meta-delete "meta-delete-nonexistent-test"`

**Then**:
- Returns `"NF"` (Not Found) or `"EN"` (Error/Not stored)
- No error raised
- Operation completes successfully

**Confidence**: 0.93

---

## Scenario: mc-meta-noop returns MN

**Source**: tests.lisp:240-244

**Given**:
- Memcache connection is established

**When**:
- Call `(mc-meta-noop)`

**Then**:
- Returns `"MN"` (Meta Noop response code)

**Use case**: Pipeline flush marker

**Confidence**: 0.95

---

## Scenario: mc-meta-get on nonexistent key

**Source**: tests.lisp:246-253

**Given**:
- Key "meta-nonexistent-key-12345" does NOT exist

**When**:
- Call `mc-meta-get "meta-nonexistent-key-12345"`

**Then**:
- Returns two values:
  1. `"EN"` (error/not found string)
  2. `nil` (foundp = false)

**Pattern for checking results**:
```lisp
(multiple-value-bind (response foundp) (mc-meta-get "nosuchkey")
  (if foundp
      ;; Hash table with data
      (gethash :value response)
      ;; String error code
      response))  ; => "EN"
```

**Confidence**: 0.95

---

## Behavioral Patterns

**Response codes**:
- `"HD"`: Success (Hit/Deleted/Stored)
- `"EN"`: Not found / Error
- `"EX"`: Exists (CAS conflict)
- `"NF"`: Not found (delete)
- `"MN"`: Meta noop

**Dual return values**:
- Found: `(hash-table, t)`
- Not found: `(error-string, nil)`

**Hash table keys**:
- `:value` - Data octets (if requested)
- `:cas` - CAS token (if requested)
- `:opaque` - Client token (if provided)
- `:key` - Key (if return-key=t)
- `:win`, `:stale`, `:already-won` - Advanced flags

**Encoding**: Values are always octet arrays, use `babel:octets-to-string` to decode

---

_Extraction confidence: 0.94_
_Source: tests.lisp:152-253_
_Test coverage: Excellent (basic operations well-tested)_
