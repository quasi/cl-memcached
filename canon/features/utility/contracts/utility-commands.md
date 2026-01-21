# Utility Commands Contract

[DRAFT - Extracted via triangulation]

## Overview

Administrative and maintenance commands for memcached server management.

**Source files**: cl-memcached.lisp:326-331, 363-397
**Confidence**: 0.88 (code ∩ docs, limited test coverage)

---

## mc-del

**Signature**:
```lisp
(mc-del key
        &key (noreply nil)
             (memcache *memcache*)
             (mc-use-pool *mc-use-pool*))
```

**Purpose**: Delete a key from the cache

**Returns**:
- `"DELETED"` if key existed and was deleted
- `"NOT_FOUND"` if key didn't exist

**Protocol**: `delete <key>\r\n`

**Source**: cl-memcached.lisp:326-331, README.md:73-75, tests.lisp:29
**Confidence**: 0.95

---

## mc-touch

**Signature**:
```lisp
(mc-touch key expiry-time
          &key (noreply nil)
               (memcache *memcache*)
               (mc-use-pool *mc-use-pool*))
```

**Purpose**: Update expiration time without fetching the value

**Parameters**:
- `key`: Key to touch
- `expiry-time` (integer): New expiration in seconds

**Returns**:
- `"TOUCHED"` on success
- `"NOT_FOUND"` if key doesn't exist

**Protocol**: `touch <key> <exptime>\r\n`

**Use case**: Extend TTL of frequently accessed items without the overhead of GET/SET

**Source**: cl-memcached.lisp:363-368, README.md:91-93
**Confidence**: 0.88

---

## mc-flush-all

**Signature**:
```lisp
(mc-flush-all &key (delay 0)
                   (noreply nil)
                   (memcache *memcache*)
                   (mc-use-pool *mc-use-pool*))
```

**Purpose**: Invalidate all keys (expire all current items)

**Parameters**:
- `delay` (integer, default 0): Seconds to wait before flushing

**Returns**: `"OK"`

**Protocol**: `flush_all <delay>\r\n`

**Warning**: This affects ALL keys in the memcached instance (not scoped to namespace)

**Source**: cl-memcached.lisp:373-378, README.md:97-99
**Confidence**: 0.90

---

## mc-version

**Signature**:
```lisp
(mc-version &key (memcache *memcache*)
                 (mc-use-pool *mc-use-pool*))
```

**Purpose**: Get memcached server version string

**Returns**: String like `"VERSION 1.6.17"`

**Protocol**: `version\r\n`

**Use case**: Version compatibility checks, debugging

**Source**: cl-memcached.lisp:383-387, README.md:103-105
**Confidence**: 0.90

---

## mc-verbosity

**Signature**:
```lisp
(mc-verbosity &key (level 1)
                   (noreply nil)
                   (memcache *memcache*)
                   (mc-use-pool *mc-use-pool*))
```

**Purpose**: Set server logging verbosity level

**Parameters**:
- `level` (integer): Verbosity level (typically 0-3)

**Returns**: `"OK"`

**Protocol**: `verbosity <level>\r\n`

**Use case**: Debugging, troubleshooting server issues

**Source**: cl-memcached.lisp:392-397, README.md:109-111
**Confidence**: 0.85

---

_Extraction confidence: 0.88_
_Note: These utility commands have limited test coverage_
