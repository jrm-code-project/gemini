;;;; -*- Mode: lisp; Syntax: Ansi-Common-Lisp; Base: 10; Package: GEMINI-TESTS; Encoding: utf-8; -*-

(in-package "GEMINI-TESTS")

;;; Suite declarations

;;; Test suite for utility functions in misc.lisp
(def-suite misc-utils
  :description "Tests for miscellaneous utility functions."
  :in all-tests)


;;; Test suite for concurrency functions in gemini.lisp
(def-suite concurrency-tests
  :description "Tests for concurrent utility functions."
  :in all-tests)


;;; Test suite for Predator Reader v4.0
(def-suite predator-tests
  :description "Tests for the hardened Predator Reader v4.0."
  :in all-tests)


;;; Test suite for gemini-print functions
(def-suite gemini-print-tests
  :description "Tests for gemini-print functions."
  :in all-tests)


;;; Test suite for gemini-core functions
(def-suite gemini-core-tests
  :description "Tests for gemini-core functions."
  :in all-tests)


;;; Test suite for gemini-iridium functions
(def-suite gemini-iridium-tests
  :description "Tests for gemini-iridium functions."
  :in all-tests)

(def-suite interaction-payload-tests
  :description "Focused tests for Interactions and LM Studio payload normalization."
  :in all-tests)

(def-suite interaction-live-tests
  :description "Opt-in live tests for Interactions and LM Studio backends."
  :in all-tests)

(def-suite interaction-stream-tests
  :description "Hermetic stream and event-processing tests for Interactions and LM Studio."
  :in all-tests)

(def-suite lmstudio-backend-tests
  :description "Hermetic LM Studio backend and bridge tests."
  :in all-tests)

(def-suite interaction-backend-tests
  :description "Hermetic Interactions backend, invocation, and tool-translation tests."
  :in all-tests)

(def-suite gemini-chatbot-tests
  :description "Integration-flavored tests for chatbot state and conversation isolation."
  :in all-tests)

(def-suite sse-tests
  :description "Tests for SSE transport and streaming integration."
  :in all-tests)
