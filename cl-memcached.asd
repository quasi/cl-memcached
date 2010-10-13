;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;
;;; Copyright (c) 2005-2006, quasi.  All rights reserved.
;;;               Cleartrip.
;


(in-package #:cl-user)

(defpackage #:cl-memcached-system
  (:nicknames "CLMC")
  (:use #:cl
	#:asdf))

(in-package #:cl-memcached-system)

(defsystem :cl-memcached
  :version "0.5.0"
  :author "quasi <quasi@cleartrip.com>"
  :serial t
  :depends-on (:usocket
	       :split-sequence
	       :bordeaux-threads
	       :flexi-streams)
  :components ((:file "packages")
	       (:file "util")
	       (:file "compat")
	       (:file "cl-memcached")))
 
