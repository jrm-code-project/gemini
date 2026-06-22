;;;; -*- Mode: lisp; Syntax: Ansi-Common-Lisp; Base: 10; Package: GEMINI-TESTS; Encoding: utf-8; -*-

(in-package "GEMINI-TESTS")

(in-suite concurrency-tests)

(test map-parallel-normal
  "Test that map-parallel returns expected results under normal conditions."
  (let ((res (gemini:map-parallel (lambda (x) (* x x)) '(1 2 3 4))))
    (is (equal '(1 4 9 16) res))))

(test map-parallel-timeout
  "Test that map-parallel handles timed out tasks gracefully."
  (let ((res (gemini:map-parallel (lambda (x)
                                   (if (or (= x 2) (= x 3))
                                       (sleep 1.5)
                                       x))
                                 '(1 2 3)
                                 :timeout-ms 200)))
    (is (equal 1 (car res)))
    (is (equal "[TIMEOUT]" (cadr res)))
    (is (equal "[TIMEOUT]" (caddr res)))))

(test map-parallel-error
  "Test that map-parallel catches and formats errors gracefully."
  (let ((res (gemini:map-parallel (lambda (x)
                                   (if (= x 2)
                                       (error "test error")
                                       x))
                                 '(1 2 3))))
    (is (equal 1 (car res)))
    (is (str:starts-with? "[ERROR:" (cadr res)))
    (is (equal 3 (caddr res)))))
