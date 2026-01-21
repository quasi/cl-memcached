;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;;
;;; Test system for cl-memcached

(asdf:defsystem :cl-memcached-test
  :version "1.0.0"
  :author "quasi <quasi@quasilabs.in>"
  :description "Tests for cl-memcached"
  :license "MIT"
  :serial t
  :depends-on (:cl-memcached
               :fiveam)
  :components ((:file "tests")))
