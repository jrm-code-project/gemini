;;;; -*- Mode: lisp; Syntax: Ansi-Common-Lisp; Base: 10; Package: GEMINI-TESTS; Encoding: utf-8; -*-

(in-package "GEMINI-TESTS")

;;; Main entry point for running all tests. This is what the .asd file calls.
(def-suite all-tests
  :description "The master suite of all tests for the Gemini system.")

(defun run! ()
  "Run all test suites."
  (fiveam:run! 'all-tests))
