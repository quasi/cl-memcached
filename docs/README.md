# cl-memcached Documentation

**cl-memcached** is a fast, thread-safe Common Lisp library for interfacing with memcached servers. It supports both the classic TEXT protocol and the modern meta protocol, with built-in connection pooling for superior performance.

## What is cl-memcached?

A Lisp client library that connects your application to memcached—a distributed memory caching system. Store and retrieve data quickly, use atomic counters, and batch operations efficiently.

## Why Use It?

- **Fast**: Connection pooling provides 5-7x speedup over unpooled connections
- **Reliable**: Thread-safe, tested on SBCL, CCL, and CMUCL
- **Complete**: Supports all memcached commands including modern meta protocol
- **Simple**: Single-file library with minimal dependencies
- **Battle-tested**: Active development with comprehensive test coverage

## Getting Started

**New to cl-memcached?** Start here:
- [Quickstart Guide](quickstart.md) — Get a working example in 5 minutes
- [Core Concepts](concepts/core-concepts.md) — Understand what memcached does and why

**Ready to use it?** Pick your path:
- [Tutorials](tutorials/) — Learn by example (beginner to advanced)
- [How-To Guides](how-to/) — Solve specific problems
- [API Reference](reference/api-reference.md) — Lookup function details

**Advanced users?**
- [Performance Tuning](performance.md) — Optimize for your workload
- [Pipelining](how-to/pipelining.md) — Batch operations efficiently
- [Architecture](architecture.md) — Understand the design

## Installation

Add to your `.asd` file:

```lisp
:depends-on (:cl-memcached)
```

Then load it:

```lisp
(ql:quickload :cl-memcached)
```

For manual installation, see [Setup Guide](setup.md).

## Quick Example

```lisp
;; Connect to memcached
(setq mc (cl-memcached:make-memcache :host "localhost" :port 11211))

;; Store data
(cl-memcached:mc-set "user:123" "John Doe")

;; Retrieve data
(cl-memcached:mc-get+ (list "user:123"))
;; => (#<MEMCACHE-RESPONSE key="user:123" data="John Doe">)

;; Use a counter
(cl-memcached:mc-set "page-views" "0")
(cl-memcached:mc-incr "page-views")
;; => 1
```

It works! Now explore the [Quickstart](quickstart.md) or jump to a [Tutorial](tutorials/).

## Common Tasks

| Task | Documentation |
|------|----------------|
| Cache HTTP responses | [Caching Tutorial](tutorials/02-caching.md) |
| Count events with atomic operations | [Counters How-To](how-to/counters.md) |
| Handle concurrent updates safely | [CAS Operations](how-to/cas-operations.md) |
| Monitor cache health | [Stats and Monitoring](how-to/monitoring.md) |
| Speed up batch operations | [Pipelining](how-to/pipelining.md) |
| Handle connection failures | [Error Handling](how-to/error-handling.md) |

## Core Concepts Glossary

| Term | What It Does |
|------|--------------|
| **Key** | Text identifier for your data (max 250 chars) |
| **Value** | Binary data you store (max ~1MB) |
| **TTL** | How long to keep data before auto-delete (seconds) |
| **CAS** | Check-and-set: update only if unchanged (prevents race conditions) |
| **Pool** | Reusable connections for speed (5-7x faster) |
| **Meta Protocol** | Modern memcached protocol with pipelining support |

Full glossary: [Core Concepts](concepts/core-concepts.md)

## System Requirements

- **Common Lisp**: SBCL, CCL, or CMUCL (2013+)
- **Memcached**: 1.4+ server
- **Dependencies**: usocket, split-sequence, babel, pooler (included via quicklisp)

## Troubleshooting

**Can't connect to memcached?**
- Check memcached is running: `echo "stats" | nc localhost 11211`
- Verify host/port match your server
- See [Troubleshooting Guide](troubleshooting.md)

**Performance issues?**
- Enable connection pooling: `(setf cl-memcached:*mc-use-pool* t)`
- Check [Performance Tuning](performance.md)

**Type errors with data?**
- Data must be byte arrays: `(babel:string-to-octets "my data")`
- Use `mc-set` instead of `mc-store` (handles conversion automatically)
- See [Data Types](concepts/data-types.md)

More help: [Troubleshooting Guide](troubleshooting.md)

## Project Status

**Active Development**: Regular updates and bug fixes
- Latest: Meta protocol bugs fixed, comprehensive test suite added (Jan 2026)
- Stable: 1.0.0 release with MIT license
- Repository: [github.com/quasi/cl-memcached](https://github.com/quasi/cl-memcached)

## License

MIT License. Free for commercial and private use.

## Next Steps

1. **First time?** → [Quickstart](quickstart.md)
2. **Learn concepts** → [Core Concepts](concepts/core-concepts.md)
3. **Try a tutorial** → [Tutorials](tutorials/)
4. **Solve a problem** → [How-To Guides](how-to/)
5. **Look up details** → [API Reference](reference/api-reference.md)

---

**Questions?** Check [FAQ](faq.md) or file an issue on GitHub.
