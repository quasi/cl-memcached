CL-MEMCACHED
============

CL-MEMCACHED is a **simple**, **fast** & **thread-safe** library to interface with the [memcached](http://www.danga.com/memcached/) object caching system. It implements the Memcached TEXT protocol.

According to the home page :
> *memcached* is a high-performance, distributed memory object caching system, generic in nature, but intended for use in speeding up dynamic web applications by alleviating database load.

Tested on SBCL, CCL & CMUCL.

-----
Global variables

`*memcache*`

Most command have this as a fallback binding. Useful if we are only using one cache or if we want to bind it to a cache and then use it multiple places.

`*mc-use-pool*`

If this is true then the connection pool will be used. On SBCL this is about 3x faster.

`*mc-default-encoding*`

Babel external format. Default is UTF-8.

-----

**make-memcache** &key (ip "127.0.0.1") (port 11211) (name "Memcache") (pool-size 2)

Makes a memcached data-structure. We use this for further transactions. This has a inbuilt pool and know how to pool items.

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

**mc-touch** key expiry-time &key (noreply nil) (memcache `*memcache*`) (mc-use-pool `*mc-use-pool*`)

Change expiry time of `key`.

-------

**mc-stats** &key (memcache `*memcache*`) (noreply nil) (mc-use-pool `*mc-use-pool*`)

Returns raw stats response string. Pretty much useless.

------

**mc-stats-alist** &key (memcache `*memcache*`)

Returns a Alist. A bit more usefull.

-----

**mc-stats-summary** &key (memcache `*memcache*`)

Prints all the details from the alist. ;) Not too hot, but hey.


-----

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

AUTHORS:

Abhijit 'quasi' Rao <quasi@quasilabs.in>


DEPENDENCIES:

* usocket http://www.cliki.net/usockes
* split-sequence http://www.cliki.net/SPLIT-SEQUENCE
* babel http://common-lisp.net/project/babel/
* pooler https://github.com/quasi/pooler

Note :
The http://common-lisp.net/project/cl-memcached/ is the homepage. But the version there is older and the documentation out of date. I have lost the creds, :-). Till I manage to set that right please ignore that one.

Benchmark
---------
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


