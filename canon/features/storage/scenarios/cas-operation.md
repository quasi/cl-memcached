# CAS (Check-And-Set) Operation Scenarios

[DRAFT - Extracted from tests]

## Scenario: CAS succeeds with correct token

**Source**: tests.lisp:68-79

**Given**:
- Key "test-cas-correct" does not exist

**When**:
1. Set key to "initial-data" using `mc-set`
2. Get key with `mc-gets+` to retrieve CAS token
3. Attempt to update with `mc-cas` using the correct CAS token

**Then**:
- `mc-cas` returns "STORED"
- Data is updated in cache

**Confidence**: 0.95 (direct test evidence)

---

## Scenario: CAS fails with stale token

**Source**: tests.lisp:81-94

**Given**:
- Key "test-cas-incorrect" is set to "initial-data"

**When**:
1. Get key with `mc-gets+` to retrieve CAS token
2. Another client modifies the key (using `mc-set`)
3. Attempt to update with `mc-cas` using the now-stale CAS token

**Then**:
- `mc-cas` returns "EXISTS"
- Data is NOT updated (concurrent modification detected)

**Confidence**: 0.95 (direct test evidence)

---

## Scenario: Meta protocol CAS succeeds

**Source**: tests.lisp:182-194

**Given**:
- Key "meta-cas-set-test" is set

**When**:
1. Get key with `mc-meta-get :cas t` to retrieve CAS token
2. Attempt to update with `mc-meta-set` using correct CAS token

**Then**:
- `mc-meta-set` returns "HD" (success)
- Data is updated

**Confidence**: 0.95

---

## Scenario: Meta protocol CAS fails with wrong token

**Source**: tests.lisp:196-210

**Given**:
- Key is set to "initial"

**When**:
1. Get CAS token
2. Modify value (invalidating CAS)
3. Attempt to set with old CAS token

**Then**:
- `mc-meta-set` returns "EX" (exists/conflict)
- Data is NOT updated

**Confidence**: 0.95

---

**Behavioral Patterns**:
- CAS provides optimistic concurrency control
- Classic and meta protocols have equivalent semantics
- Error responses clearly distinguish "not found" from "concurrent modification"

**Coverage**: Excellent (4 comprehensive tests for success and failure cases)

_Extracted via Pass 3: Behavioral Capture_
_Confidence: 0.95 (tests ∩ code)_
