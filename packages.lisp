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
           #:mc-get
	   #:mc-get+
	   #:mc-get-value
	   #:mr-key
	   #:mr-flags
	   #:mr-bytes
	   #:mr-cas-unique
	   #:mr-data-raw
	   #:mr-data
	   #:mc-del
	   #:mc-incr
	   #:mc-decr
	   #:mc-touch
	   #:mc-stats
	   #:mc-stats-alist
	   #:mc-stats-summary
	   ))


