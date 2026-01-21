# Statistics Command Scenarios

[DRAFT - Extracted from tests]

## Scenario: mc-stats-items returns alist

**Source**: tests.lisp:105-115

**Given**:
- At least one item exists in cache ("stats-test-key" set to "some-data")
- Memcache connection is established

**When**:
- Call `(mc-stats-items :memcache *test-memcache*)`

**Then**:
- Returns a list (may be empty if no items)
- Each element is a cons cell `(stat-name . stat-value)`
- All stat names are strings
- All stat values are strings

**Example data structure**:
```lisp
(("items:1:number" . "1")
 ("items:1:age" . "5")
 ("items:1:evicted" . "0"))
```

**Confidence**: 0.90

---

## Scenario: mc-stats-slabs returns alist

**Source**: tests.lisp:117-123

**Given**:
- Memcache server is running

**When**:
- Call `(mc-stats-slabs :memcache *test-memcache*)`

**Then**:
- Returns a list
- Each element is a cons cell
- Contains slab allocation statistics

**Confidence**: 0.90

---

## Scenario: mc-stats-sizes returns alist

**Source**: tests.lisp:125-129

**Given**:
- Memcache server is running

**When**:
- Call `(mc-stats-sizes :memcache *test-memcache*)`

**Then**:
- Returns a list
- Contains item size distribution data

**Note**: May be expensive operation on production servers

**Confidence**: 0.88

---

## Scenario: mc-stats returns expected fields

**Source**: tests.lisp:131-141

**Given**:
- Memcache server is running

**When**:
- Call `(mc-stats :memcache *test-memcache*)`

**Then**:
- Returns a list with length > 0
- Contains expected field "version"
- Contains expected field "pid"
- All fields are alist entries `(name . value)`

**Example verification**:
```lisp
(let ((result (mc-stats :memcache *test-memcache*)))
  (assert (not (null (assoc "version" result :test #'string=))))
  (assert (not (null (assoc "pid" result :test #'string=)))))
```

**Confidence**: 0.95

---

## Behavioral Patterns

**Consistency**: All stats functions return alist format

**String keys**: All stat names are strings (not keywords)

**String values**: All stat values are strings (parse-integer when needed)

**Empty results**: Empty list is valid (no error raised)

**Use pattern**:
```lisp
(let ((stats (mc-stats)))
  (cdr (assoc "version" stats :test #'string=)))
;; => "1.6.17"
```

---

_Extraction confidence: 0.91_
_Source: tests.lisp:105-141_
_Test coverage: Good (4 dedicated tests)_
