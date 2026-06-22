;;;; -*- Mode: lisp; Syntax: Ansi-Common-Lisp; Base: 10; Package: GEMINI-TESTS; Encoding: utf-8; -*-

(in-package "GEMINI-TESTS")

(in-suite gemini-print-tests)

(test thought-stripping
  "Test stripping of thoughts from parts, content, candidates, and results."
  (let* ((normal-part (part "normal text"))
         (thought-part (part "this is a thought" :thought t))
         (parts (list normal-part thought-part))
         (content (content :parts parts :role "user"))
         (candidate (gemini::object :content content :index 0 :finish-reason "STOP"))
         (results (gemini::object :candidates (list candidate) :response-id "test-id")))
    
    ;; 1. strip-thoughts-from-part
    (is (eq normal-part (gemini::strip-thoughts-from-part normal-part)))
    (is (null (gemini::strip-thoughts-from-part thought-part)))
    
    ;; 2. strip-thoughts-from-parts
    (let ((stripped-parts (gemini::strip-thoughts-from-parts parts)))
      (is (= 1 (length stripped-parts)))
      (is (equal "normal text" (get-text (car stripped-parts)))))
      
    ;; 3. strip-thoughts-from-content
    (let ((stripped-content (gemini::strip-thoughts-from-content content)))
      (is (not (null stripped-content)))
      (is (equal "user" (get-role stripped-content)))
      (is (= 1 (length (coerce (get-parts stripped-content) 'list)))))
      
    ;; 4. strip-thoughts-from-candidate
    (let ((stripped-candidate (gemini::strip-thoughts-from-candidate candidate)))
      (is (not (null stripped-candidate)))
      (is (equal 0 (gemini::get-index stripped-candidate)))
      (is (equal "STOP" (get-finish-reason stripped-candidate))))
      
    ;; 5. strip-and-print-thoughts
    (let* ((trace-output (with-output-to-string (*trace-output*)
                           (let ((stripped-results (gemini::strip-and-print-thoughts results)))
                             (is (not (null stripped-results)))
                             (is (equal "test-id" (gemini::get-response-id stripped-results)))))))
      ;; Verify that the thought was printed to *trace-output*
      (is (search "this is a thought" trace-output)))))

(test print-text-formatting
  "Test print-text formatting, paragraph reflow, blockquotes, and bowdlerization."
  (let* ((text-with-newlines (format nil "Line 1.~%Line 2.~%~%Line 3 after blank line."))
         (results (gemini::object :candidates (list (gemini::object :content (content :parts (list (part text-with-newlines)) :role "model"))))))
    (let ((output (with-output-to-string (*trace-output*)
                    (gemini::print-text nil results))))
      ;; Should reflow lines 1 & 2 into paragraph with leading indentation "  "
      (is (search "  Line 1. Line 2." output))
      (is (search "..." output))
      (is (not (search "  Line 3 after blank line." output)))))
  
  ;; Bowdlerization
  (let* ((censored-results (gemini::object :candidates (list (gemini::object :content (content :parts (list (part "Hello world secret!")) :role "model"))))))
    (let ((output (with-output-to-string (*trace-output*)
                    (gemini::print-text "secret" censored-results))))
      (is (search "Hello world" output))
      (is (not (search "secret" output))))))

(test print-text-strips-thought-tags
  "Test that print-text removes <thought>...</thought> tags during paragraph reflow."
  (let* ((response (gemini::object
                    :candidates
                    (list (gemini::object
                           :content (gemini::content
                                     :role "model"
                                     :parts (list (part "Line 1 with <thought>internal reasoning here</thought> continues.\nLine 2.")))))))
         (output (with-output-to-string (*trace-output*)
                   (gemini::print-text nil response))))
    ;; Should have "Line 1" and "Line 2"
    (is (search "Line 1" output))
    (is (search "Line 2" output))
    ;; Should NOT have the thought content or tags
    (is (not (search "thought" output)))
    (is (not (search "internal reasoning" output)))
    (is (not (search "<thought>" output)))
    (is (not (search "</thought>" output)))))
