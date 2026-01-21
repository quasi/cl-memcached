# GETS Operation Scenarios (CAS Support)

[DRAFT - Extracted from tests]

## Scenario: mc-gets returns CAS unique value

**Source**: tests.lisp:40-54

**Given**:
- Key "test-gets-cas" does not exist
- Memcache connection is established

**When**:
1. Set key to "test-data" using `mc-set`
2. Retrieve key using `mc-gets` (not `mc-get`)

**Then**:
- `mc-gets` returns a list with one result
- Result is a list: `(key flags bytes cas-unique data-raw)`
- `cas-unique` (4th element) is NOT nil
- Key matches "test-gets-cas"

**Confidence**: 0.95 (direct test evidence)

---

## Scenario: mc-gets+ returns response with CAS

**Source**: tests.lisp:56-66

**Given**:
- Key "test-gets-plus-cas" is set to "hello-world"

**When**:
- Call `mc-gets+ "test-gets-plus-cas"`

**Then**:
- Returns a `memcache-response` structure
- `(mc-cas-unique response)` is NOT nil
- Structure type is `'cl-memcached::memcache-response`

**Comparison with mc-get+**:
- `mc-get+` would return `cas-unique = nil`
- `mc-gets+` returns populated `cas-unique` field

**Confidence**: 0.95

---

## Behavioral Patterns

**Key insight**: The 's' in `mc-gets` / `mc-gets+` stands for "CAS support"

**Naming convention**:
- `mc-get` / `mc-get+`: No CAS token
- `mc-gets` / `mc-gets+`: With CAS token

**Use cases**:
- Use `mc-get`/`mc-get+` for read-only access
- Use `mc-gets`/`mc-gets+` when planning to update with CAS

**Added**: Commit 71fd866 (feat: Implement missing memcached commands)

---

_Extraction confidence: 0.95_
_Source: tests.lisp:40-66_
_Related: CAS operation scenarios_
