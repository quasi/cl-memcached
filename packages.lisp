;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

;;; Copyright (c) 2005-2013, quasi. All rights reserved.


(in-package #:cl-user)

(defpackage #:cl-memcached
  (:use #:cl)
  (:export #:*memcache*
	   #:*mc-use-pool*
	   #:*mc-default-encoding*
	   #:make-memcache
	   #:mc-store
	   #:mc-cas
	   #:mc-set
	   #:mc-add
	   #:mc-replace
	   #:mc-append
	   #:mc-prepend
           #:mc-get
	   #:mc-get+
	   #:mc-get-value
	   #:mc-key
	   #:mc-flags
	   #:mc-bytes
	   #:mc-cas-unique
	   #:mc-data-raw
	   #:mc-data
	   #:mc-del
	   #:mc-incr
	   #:mc-decr
	   #:mc-touch
	   #:mc-stats
	   #:mc-stats-summary
	   ))


