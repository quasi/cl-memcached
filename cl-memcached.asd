;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;;
;;; Copyright (c) 2005-2013, Abhijit 'quasi' Rao.  All rights reserved.
;;;


(asdf:defsystem :cl-memcached
  :version "1.0.0"
  :author "quasi <quasi@quasilabs.in>"
  :description "Fast, thread-safe library to interface with the Memcached Object Cache."
  :license "MIT"
  :serial t
  :depends-on (:usocket
	       :split-sequence
	       :babel
	       :pooler)
  :components ((:file "packages")
	       (:file "cl-memcached")))

