# Statistics Commands Contract

[DRAFT - Extracted via triangulation]

## Overview

Commands for retrieving memcached server statistics and monitoring data.

**Source files**: cl-memcached.lisp:404-436
**Confidence**: 0.92 (code ∩ docs ∩ tests)
**Documentation updated**: 2026-01-21 (this session)

---

## mc-stats

**Signature**:
```lisp
(mc-stats &key (memcache *memcache*)
               (mc-use-pool *mc-use-pool*))
```

**Purpose**: Get general server statistics

**Returns**: Alist of `(stat-name . value)` pairs

**Common statistics**:
- `"version"` - Server version
- `"pid"` - Process ID
- `"uptime"` - Seconds since server start
- `"curr_connections"` - Current open connections
- `"total_connections"` - Total connections since start
- `"cmd_get"` - GET commands received
- `"cmd_set"` - SET commands received
- `"get_hits"` - Successful GETs
- `"get_misses"` - Failed GETs
- `"bytes"` - Current bytes stored
- `"curr_items"` - Current item count
- `"evictions"` - Items evicted

**Protocol**: `stats\r\n`, receives `STAT <name> <value>\r\n` lines, ends with `END\r\n`

**Example**:
```lisp
(mc-stats)
;; => (("version" . "1.6.17")
;;     ("pid" . "12345")
;;     ("uptime" . "86400")
;;     ("curr_connections" . "10")
;;     ...)
```

**Source**:
- Implementation: cl-memcached.lisp:414-416
- Documentation: README.md:115-117
- Tests: tests.lisp:131-141

**Confidence**: 0.95

---

## mc-stats-items

**Signature**:
```lisp
(mc-stats-items &key (memcache *memcache*)
                     (mc-use-pool *mc-use-pool*))
```

**Purpose**: Get per-slab-class item statistics

**Returns**: Alist of item-level statistics grouped by slab class

**Common statistics**:
- `"items:N:number"` - Number of items in slab class N
- `"items:N:age"` - Age of oldest item in slab class N
- `"items:N:evicted"` - Items evicted from slab class N
- `"items:N:outofmemory"` - Out of memory count for slab N

**Protocol**: `stats items\r\n`

**Use case**: Understanding item distribution across slab classes, identifying hotspots

**Example**:
```lisp
(mc-stats-items)
;; => (("items:1:number" . "42")
;;     ("items:1:age" . "3600")
;;     ("items:2:number" . "128")
;;     ...)
```

**Source**:
- Implementation: cl-memcached.lisp:418-420
- Documentation: README.md:127-129 (added this session)
- Tests: tests.lisp:105-115

**Confidence**: 0.90

---

## mc-stats-slabs

**Signature**:
```lisp
(mc-stats-slabs &key (memcache *memcache*)
                     (mc-use-pool *mc-use-pool*))
```

**Purpose**: Get slab allocator statistics

**Returns**: Alist of slab allocation statistics

**Common statistics**:
- `"N:chunk_size"` - Chunk size for slab class N
- `"N:chunks_per_page"` - Chunks per page for slab N
- `"N:total_pages"` - Total pages allocated to slab N
- `"N:total_chunks"` - Total chunks in slab N
- `"N:used_chunks"` - Used chunks in slab N
- `"N:free_chunks"` - Free chunks in slab N
- `"N:mem_requested"` - Memory requested for slab N

**Protocol**: `stats slabs\r\n`

**Use case**: Memory allocation analysis, understanding slab utilization

**Example**:
```lisp
(mc-stats-slabs)
;; => (("1:chunk_size" . "96")
;;     ("1:chunks_per_page" . "10922")
;;     ("1:total_pages" . "1")
;;     ...)
```

**Source**:
- Implementation: cl-memcached.lisp:422-424
- Documentation: README.md:133-135 (added this session)
- Tests: tests.lisp:117-123

**Confidence**: 0.90

---

## mc-stats-sizes

**Signature**:
```lisp
(mc-stats-sizes &key (memcache *memcache*)
                     (mc-use-pool *mc-use-pool*))
```

**Purpose**: Get item size distribution statistics

**Returns**: Alist showing distribution of item sizes

**Format**: Each entry shows how many items fall into size buckets

**Protocol**: `stats sizes\r\n`

**Use case**: Understanding item size distribution for capacity planning

**Warning**: This command may be expensive on production servers (requires iterating all items)

**Example**:
```lisp
(mc-stats-sizes)
;; => (("96" . "42")      ; 42 items ~96 bytes
;;     ("128" . "15")     ; 15 items ~128 bytes
;;     ("256" . "8")      ; 8 items ~256 bytes
;;     ...)
```

**Source**:
- Implementation: cl-memcached.lisp:426-428
- Documentation: README.md:139-141 (added this session)
- Tests: tests.lisp:125-129

**Confidence**: 0.88 (less commonly used)

---

## mc-stats-summary

**Signature**:
```lisp
(mc-stats-summary &key (memcache *memcache*))
```

**Purpose**: Print formatted summary of general stats (human-readable output)

**Returns**: NIL (prints to standard output)

**Output format**: Formatted table with stat names and values

**Example**:
```lisp
(mc-stats-summary)
;; Prints:
;; Memcached Server Stats
;; ----------------------
;; Version              : 1.6.17
;; Pid                  : 12345
;; Uptime               : 86400
;; ...
```

**Note**: This is a convenience function for interactive use, not for programmatic access

**Source**:
- Implementation: cl-memcached.lisp:432-436
- Documentation: README.md:121-123

**Confidence**: 0.92

---

## Internal: mc-stats-internal

**Signature**:
```lisp
(mc-stats-internal command
                   &key (memcache *memcache*)
                        (mc-use-pool *mc-use-pool*))
```

**Purpose**: Internal function implementing all stats variants

**Parameters**:
- `command`: Protocol command string ("stats", "stats items", "stats slabs", "stats sizes")

**Protocol parsing**:
```
STAT <name> <value>\r\n
STAT <name> <value>\r\n
...
END\r\n
```

**Returns**: Alist of `(name . value)` pairs extracted from STAT lines

**Source**: cl-memcached.lisp:404-412

**Confidence**: 0.95

---

## Usage Patterns

### Monitoring Server Health

```lisp
(let ((stats (mc-stats)))
  (list :version (cdr (assoc "version" stats :test #'string=))
        :uptime (parse-integer (cdr (assoc "uptime" stats :test #'string=)))
        :hit-rate (let ((hits (parse-integer (cdr (assoc "get_hits" stats :test #'string=))))
                        (misses (parse-integer (cdr (assoc "get_misses" stats :test #'string=)))))
                    (if (> (+ hits misses) 0)
                        (float (/ hits (+ hits misses)))
                        0.0))))
;; => (:VERSION "1.6.17" :UPTIME 86400 :HIT-RATE 0.87)
```

### Memory Analysis

```lisp
(let ((slabs (mc-stats-slabs)))
  ;; Analyze slab utilization
  (loop for (key . val) in slabs
        when (search "used_chunks" key)
        collect (list key val)))
```

---

## Observations

**obs-stats-001** (resolved):
- Documentation gap identified during initiation
- mc-stats-items, mc-stats-slabs, mc-stats-sizes were implemented and tested
- But not documented in README
- **Resolution**: Added documentation this session (2026-01-21)

**obs-stats-002** (convergent):
- All stats functions follow consistent pattern
- Internal implementation is clean (mc-stats-internal)
- Test coverage is good (tests.lisp:105-141)

**obs-stats-003** (design note):
- Alist return format is idiomatic for Common Lisp
- Easy to use with `assoc` for lookups
- String keys (not keywords) match server format

---

_Extraction confidence: 0.92_
_Triangulation: code ∩ docs ∩ tests_
_Documentation status: NOW COMPLETE (updated this session)_
