# Meta Protocol Pipelining Scenarios

[DRAFT - Extracted from tests]

## Scenario: Pipelining with mc-with-connection

**Source**: tests.lisp:255-277

**Given**:
- Keys "pipe-test-1" and "pipe-test-2" do not exist
- Memcache connection is established

**When**:
```lisp
(mc-with-connection (s :memcache *test-memcache*)
  ;; Send quiet sets (no immediate response)
  (mc-meta-set "pipe-test-1" "data1" :stream s :quiet t)
  (mc-meta-set "pipe-test-2" "data2" :stream s :quiet t)

  ;; Flush pipeline with noop
  (let ((result (mc-meta-noop :stream s)))
    (assert (string= "MN" result))))
```

**Then**:
- Noop returns `"MN"`
- Both keys are successfully stored
- Subsequent `mc-meta-get` retrieves both values correctly
- Total network round-trips: 2 (send batch + receive noop)

**Verification**:
```lisp
(multiple-value-bind (r1 f1) (mc-meta-get "pipe-test-1")
  (assert f1)
  (assert (string= "data1" (babel:octets-to-string (gethash :value r1)))))

(multiple-value-bind (r2 f2) (mc-meta-get "pipe-test-2")
  (assert f2)
  (assert (string= "data2" (babel:octets-to-string (gethash :value r2)))))
```

**Performance benefit**: 2 round-trips vs 4 round-trips (without pipelining)

**Confidence**: 0.95

---

## Scenario: Pipelining with opaque tokens

**Source**: tests.lisp:279-289

**Given**:
- Key "meta-opaque-test" exists with value "opaque-data"

**When**:
- Call `mc-meta-get "meta-opaque-test" :opaque "mytoken123"`

**Then**:
- `foundp` is `t`
- Response hash table contains `:opaque` key
- `(gethash :opaque response)` returns `"mytoken123"`

**Use case**: Correlate requests and responses in pipelined operations

**Example pipelining with correlation**:
```lisp
(mc-with-connection (s)
  ;; Send multiple gets with different opaque tokens
  (mc-meta-get "key1" :stream s :quiet t :opaque "req1")
  (mc-meta-get "key2" :stream s :quiet t :opaque "req2")
  (mc-meta-get "key3" :stream s :quiet t :opaque "req3")
  (mc-meta-noop :stream s)

  ;; Read responses - opaque tokens identify which response is which
  (let ((r1 (mc-read-meta-response s))
        (r2 (mc-read-meta-response s))
        (r3 (mc-read-meta-response s)))
    (list (gethash :opaque r1)  ; => "req1"
          (gethash :opaque r2)  ; => "req2"
          (gethash :opaque r3)))) ; => "req3"
```

**Confidence**: 0.95

---

## Scenario: Setting TTL with mc-meta-set

**Source**: tests.lisp:291-303

**Given**:
- Key "meta-ttl-test" may or may not exist

**When**:
- Call `mc-meta-set "meta-ttl-test" "ttl-data" :ttl 60`

**Then**:
- Returns `"HD"` (success)
- Key exists and can be retrieved
- Key will expire after 60 seconds

**Verification**:
```lisp
(let ((result (mc-meta-set "key" "data" :ttl 60)))
  (assert (string= "HD" result)))

(multiple-value-bind (response foundp) (mc-meta-get "key")
  (assert foundp))
```

**Confidence**: 0.93

---

## Pipelining Patterns

### Pattern 1: Batch Writes

```lisp
(mc-with-connection (s)
  (dotimes (i 100)
    (mc-meta-set (format nil "key~a" i) "data" :stream s :quiet t))
  (mc-meta-noop :stream s))
;; 100 sets with 2 round-trips
```

### Pattern 2: Batch Reads with Correlation

```lisp
(mc-with-connection (s)
  (loop for key in '("key1" "key2" "key3")
        do (mc-meta-get key :stream s :quiet t :opaque key))
  (mc-meta-noop :stream s)

  (loop repeat 3
        collect (mc-read-meta-response s)))
```

### Pattern 3: Mixed Operations

```lisp
(mc-with-connection (s)
  (mc-meta-set "key1" "new-value" :stream s :quiet t)
  (mc-meta-delete "key2" :stream s :quiet t)
  (mc-meta-get "key3" :stream s :quiet t :opaque "get-key3")
  (mc-meta-noop :stream s)

  (mc-read-meta-response s))  ; Get response for key3
```

---

## Key Requirements for Pipelining

1. **Use :stream parameter**: Pass connection stream to operations
2. **Use :quiet t**: Suppress individual responses
3. **Use :opaque tokens**: Correlate requests/responses (optional but recommended)
4. **Flush with noop**: Send `mc-meta-noop` to get final sync point
5. **Read responses**: Call `mc-read-meta-response` for non-quiet operations

---

## Performance Characteristics

**Without pipelining** (N operations):
- Network round-trips: N
- Latency: N × RTT

**With pipelining** (N operations):
- Network round-trips: 2 (send batch + receive noop/responses)
- Latency: 2 × RTT

**Speedup**: O(N) → O(1) round-trips for batch operations

**Confidence**: 0.90 (inferred from architecture)

---

## Error Handling in Pipelines

**Connection errors**: Macro ensures cleanup via `unwind-protect`

**Individual operation failures**: Responses indicate success/failure per operation

**Pipeline flush**: `mc-meta-noop` acts as synchronization barrier

**Best practice**: Check response codes even in pipelines

---

## Observations

**obs-pipeline-001** (design excellence):
- `:stream` parameter enables pipelining without API changes
- `:quiet` flag suppresses unnecessary responses
- `:opaque` tokens enable request/response correlation
- Clean integration with existing connection management

**obs-pipeline-002** (test coverage):
- Pipelining is well-tested
- Tests verify both quiet and opaque modes
- Verification after pipeline completes

---

_Extraction confidence: 0.94_
_Source: tests.lisp:255-303_
_Performance impact: High (reduces round-trips from O(N) to O(1))_
