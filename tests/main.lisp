;;;; -*- Mode: lisp; Syntax: Ansi-Common-Lisp; Base: 10; Package: GEMINI-TESTS; Encoding: utf-8; -*-

(in-package "GEMINI-TESTS")

;;; Main entry point for running all tests. This is what the .asd file calls.
(def-suite all-tests
  :description "The master suite of all tests for the Gemini system.")

(defun run! ()
  "Run all test suites."
  (fiveam:run! 'all-tests))

;;; Test suite for utility functions in misc.lisp
(def-suite misc-utils
  :description "Tests for miscellaneous utility functions."
  :in all-tests)

(in-suite misc-utils)

(test keyword-string-conversion
  "Test the conversion between keystrings and keywords."
  (is (eq :foo (gemini::keystring->keyword "foo")))
  (is (equal "foo" (gemini::keyword->keystring :foo)))
  (is (eq :foo-bar (gemini::keystring->keyword "fooBar")))
  (is (equal "fooBar" (gemini::keyword->keystring :foo-bar)))
  )
