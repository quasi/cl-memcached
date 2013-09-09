;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;;
;;; Copyright (c) 2005-2013, quasi.  All rights reserved.
;;;


(asdf:defsystem :cl-memcached
  :version "0.9.0"
  :author "quasi <quasi@quasilabs.in>"
  :serial t
  :license "MIT"
  :depends-on (:usocket
	       :split-sequence
	       :babel
	       :pooler)
  :components ((:file "packages")
	       (:file "cl-memcached")))

