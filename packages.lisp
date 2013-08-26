;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

;;; Copyright (c) 2005-2013, quasi. All rights reserved.


(in-package #:cl-user)

(defpackage #:cl-memcached
  (:use #:cl)
  (:export #:*memcache*
	   #:memcache
	   #:mc-store
           #:mc-get
	   #:mc-get+
	   #:mc-del
	   #:mc-incr
	   #:mc-decr
	   #:mc-stats
           #:make-memcache
	   #:mc-server-check
	   #:*use-pool*))


