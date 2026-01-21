# CL-MEMCACHED
============

CL-MEMCACHED is a **simple**, **fast** & **thread-safe** library to interface with the [memcached](http://www.danga.com/memcached/) object caching system. It implements both the classic TEXT protocol and the modern META protocol.

According to the home page :
> *memcached* is a high-performance, distributed memory object caching system, generic in nature, but intended for use in speeding up dynamic web applications by alleviating database load.

Tested on SBCL, CCL & CMUCL.

**Documentation**: See [DOCUMENTATION.md](DOCUMENTATION.md) for complete guides
- **Users**: Start with [docs/README.md](docs/README.md) or [docs/quickstart.md](docs/quickstart.md)
- **Agents/Developers**: Read [CLAUDE.md](CLAUDE.md) and [SKILL.md](SKILL.md)
- **Complete Spec**: [canon/canon.yaml](canon/canon.yaml)

-----
Global variables

`*memcache*`

Most commands have this as a fallback binding. Useful if we are only using one cache or if we want to bind it to a cache and then use it multiple places.

`*mc-use-pool*`

If this is true then the connection pool will be used. On SBCL this is about 3x faster.

`*mc-default-encoding*`

Babel is used for encodeing/decoding the data. Memcached expects octets. Default encoding is UTF-8.

-----

**make-memcache** &key (ip "127.0.0.1") (port 11211) (name "Memcache") (pool-size 2)

Makes a memcached data-structure. We use this for further transactions. This has a inbuilt pool and know how to make new pool items.

-----

**mc-set** key data &key (memcache `*memcache*`) (timeout 0) (flags 0) (noreply nil) (cas-unique nil) (mc-use-pool `*mc-use-pool*`)

Stores `data` for the `key` in the `memcache`. The parameters have same value as the memcached server commands.
We have similar **mc-add**, **mc-replace**, **mc-append**, **mc-prepend** functions available.

-----

**mc-cas** key data cas-unique &key (memcache `*memcache*`) (timeout 0) (flags 0) (noreply nil) (external-format `*mc-default-encoding*`) (mc-use-pool `*mc-use-pool*`)

This is a Check & Store operation.

-----

**mc-get** keys-list &key (memcache `*memcache*`) (mc-use-pool `*mc-use-pool*`)

Returns a list of lists corresponding to responses to found keys in the keys-list.

----

**mc-get+** key-or-list-of-keys &key (memcache `*memcache*`) (mc-use-pool `*mc-use-pool*`)

This is a wrapper around `mc-get`. It accepts 1 or many keys. Returns 1 or many `memcache-response` type structures containing all the pieces of the response.

The `memcache-response` structure has these slots : `key`, `flags`, `bytes`, `cas-unique`, `data-raw`. All the slot accessors start with `mc-`

-----

**mc-data** response &key (external-format `*mc-default-encoding*`)

Takes the data-raw, which is in octets, and converts it to string using the external-format.

-----

**mc-get-value** key &key (memcache `*memcache*`) (mc-use-pool `*mc-use-pool*`) (external-format `*mc-default-encoding*`)

A wrapper around `mc-data` and `mc-get+`. Give it a key and it gets a string value in return. Misuse is entierly the users responsibility. :)

-----

**mc-del** key &key (memcache `*memcache*`) (noreply nil) (mc-use-pool `*mc-use-pool*`)

Deletes `key` from the cache.

-----

**mc-incr** key &key (value 1) (noreply nil) (memcache `*memcache*`) (mc-use-pool `*mc-use-pool*`)

Increments `key` in place by value. If `key` not found then will return `NOT_FOUND`.

------

**mc-decr** key &key (value 1) (noreply nil) (memcache `*memcache*`) (mc-use-pool `*mc-use-pool*`)

Decrements `key` by value. If `key` not found then will return `NOT_FOUND`.

------

**mc-touch** key expiry-time &key ( *noreply* nil ) ( *memcache* `*memcache*` ) ( *mc-use-pool* `*mc-use-pool*` )

Change expiry time of `key`.

-------

**mc-flush-all** &key ( *delay* 0 ) ( *noreply* nil) ( *memcache* `*memcache*` ) ( *mc-use-pool* `*mc-use-pool*` )

expires all the current keys.

-------

**mc-version** &key  ( *memcache* `*memcache*` ) ( *mc-use-pool* `*mc-use-pool*` )

Returns a text string with the version of the memcached server

-------

**mc-verbosity** &key ( *level* 1 ) ( *noreply* nil) ( *memcache* `*memcache*` ) ( *mc-use-pool* `*mc-use-pool*` )

Sets the verbosity level of the logging output

-------

**mc-stats** &key (memcache `*memcache*`) (noreply nil) (mc-use-pool `*mc-use-pool*`)

Returns a `alist` of the stats.

------

**mc-stats-summary** &key (memcache `*memcache*`)

Prints all the details from the alist. ;) Not too hot, but hey.

------

**mc-stats-items** &key (memcache `*memcache*`) (mc-use-pool `*mc-use-pool*`)

Returns an `alist` of per-slab-class item statistics from the memcached server.

------

**mc-stats-slabs** &key (memcache `*memcache*`) (mc-use-pool `*mc-use-pool*`)

Returns an `alist` of slab statistics from the memcached server.

------

**mc-stats-sizes** &key (memcache `*memcache*`) (mc-use-pool `*mc-use-pool*`)

Returns an `alist` of item size distribution statistics from the memcached server.


## Examples
----

Example Usage for testing.
```
CL-USER> (require 'cl-memcached)
NIL

CL-MEMCACHED> (in-package :cl-memcached)
#<PACKAGE "CL-MEMCACHED">

CL-MEMCACHED> (setf *memcache* (make-memcache))
#<MEMCACHED-SERVER Name:Memcache IP:127.0.0.1 Port:11211 >

CL-MEMCACHED> (mc-quick-test "foo" "bar")
Success SET
Success GET
NIL

CL-MEMCACHED> (mc-set "t1" "oooooooooooooooooooooo")
STORED
:INTERNAL

CL-MEMCACHED> (mc-get+ "t1")
#<MEMCACHED-RESPONSE Key:t1 Data-Length:22 >

CL-MEMCACHED> (describe *)
#<MEMCACHED-RESPONSE Key:t1 Data-Length:22 >
  [structure-object]

Slots with :INSTANCE allocation:
  KEY         = "t1"
  FLAGS       = "0"
  BYTES       = 22
  CAS-UNIQUE  = NIL
  DATA-RAW    = #(111 111 111 111 111 111 111 111 111 111 111 111 111 111 111 111 111..
; No value

CL-MEMCACHED> (mc-data (mc-get+ "t1"))
"oooooooooooooooooooooo"

CL-MEMCACHED> (mc-get-value "t1")
"oooooooooooooooooooooo"

CL-MEMCACHED> (mc-set "t2" "0")
STORED
:INTERNAL

CL-MEMCACHED> (mc-incr "t3")
NOT_FOUND

CL-MEMCACHED> (mc-incr "t2")
1
1

CL-MEMCACHED> (mc-incr "t2")
2
1

CL-MEMCACHED> (mc-decr "t2")
1
1

```




## Meta Protocol


This library also supports the modern memcached "meta protocol". This is a text-based protocol that is more efficient and feature-rich than the classic text protocol. It is recommended for all new applications.

The meta protocol functions are prefixed with `mc-meta-`.

**mc-meta-get** key &key (value t) (cas nil) (recache-on-miss-ttl nil) (early-recache-ttl nil) (quiet nil) (opaque nil) (return-key nil) (key-is-base64 nil)

Retrieves a key using the `mg` meta command. Returns two values: a hash table with the response data, and `t` if the key was found. The response hash table may contain keys like `:cas`, `:win`, `:stale`, `:opaque`, `:key`, and `:value`.

**mc-meta-set** key data &key (ttl 0) (client-flags 0) (cas nil) (quiet nil) (keep-stale nil) (key-is-base64 nil) (opaque nil) (return-key nil)

Stores a key-value pair using the `ms` meta command.

**mc-meta-delete** key &key (cas nil) (quiet nil) (mark-stale nil) (stale-ttl nil) (key-is-base64 nil) (opaque nil) (return-key nil)

Deletes a key using the `md` meta command.

### Pipelining

The meta protocol is designed for pipelining, which can significantly improve performance by sending multiple commands to the server before reading responses. This library supports pipelining via the `mc-with-connection` macro and the `:stream` and `:quiet` keyword arguments.

```lisp
(mc-with-connection (s)
  ;; Send two set commands without waiting for a response
  (mc-meta-set "key1" "data1" :stream s :quiet t)
  (mc-meta-set "key2" "data2" :stream s :quiet t)

  ;; Send two get commands, also without waiting for a response.
  ;; Use opaque tokens to correlate requests and responses.
  (mc-meta-get "key1" :stream s :quiet t :opaque "req1")
  (mc-meta-get "key2" :stream s :quiet t :opaque "req2")

  ;; Send a no-op command to flush the pipeline and get a final response.
  (mc-meta-noop :stream s)

  ;; Now, read the responses from the stream.
  (let ((resp1 (mc-read-meta-response s))
        (resp2 (mc-read-meta-response s))
        (resp3 (mc-read-meta-response s)))
    ;; resp1 will be the response for "key1" (opaque "req1")
    ;; resp2 will be the response for "key2" (opaque "req2")
    ;; resp3 will be the response for the noop ("MN")
    ))
```

### Advanced Flags

The meta protocol functions support a variety of flags for advanced caching strategies:

*   `:recache-on-miss-ttl` (Get): `N` flag. If the item is not found, create a new item with this TTL to prevent dogpiling. The response will contain a `:win` flag.
*   `:early-recache-ttl` (Get): `R` flag. If the item's remaining TTL is less than this value, the response will contain a `:win` flag, signaling the client to recache it.
*   `:mark-stale` (Delete): `I` flag. Marks an item as stale instead of deleting it.
*   `:keep-stale` (Set): `I` flag. When used with `:cas`, updates a stale item while keeping it marked as stale.
*   `:quiet` (All): `q` flag. Suppresses nominal responses (e.g., "HD", "ST").
*   `:opaque` (All): `O` flag. A token that is reflected in the response, useful for pipelining.
*   `:return-key` (All): `k` flag. The key is returned in the response.
*   `:key-is-base64` (All): `b` flag. Indicates that the key is base64-encoded.


## AUTHOR

Abhijit 'quasi' Rao <quasi@quasilabs.in>


### DEPENDENCIES:

* pooler https://github.com/quasi/pooler
* usocket http://www.cliki.net/usockes
* split-sequence http://www.cliki.net/SPLIT-SEQUENCE
* babel http://common-lisp.net/project/babel/



### Benchmark

Host OS : OSX 10.8.4
Dataset: 1024 bytes (1kb) text string. Repeat 10000 times.
```
|-------------------+------------------+---------------+------------------+---------------|
| implementation    | SET without pool | SET with pool | GET without pool | GET with pool |
|-------------------+------------------+---------------+------------------+---------------|
| SBCL 1.1.10       |            4.942 |         0.713 |            4.905 |         0.690 |
| CCL 1.9-r15759    |            4.711 |         0.847 |            4.506 |         0.648 |
| CMUCL 20D Unicode |            4.460 |         0.970 |            4.290 |         0.810 |
|-------------------+------------------+---------------+------------------+---------------|
| Dalli on Ruby 1.9 |                  |         0.957 |                  |         1.033 |
|-------------------+------------------+---------------+------------------+---------------|
```
When we do not use the pool we make a new socket connection every time.

The Ruby 'dalli' client, which implements the binary protocol, uses the same socket (I think) so this should be comparable with our with-pool.

