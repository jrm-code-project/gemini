;;;; -*- Mode: lisp; Syntax: Ansi-Common-Lisp; Base: 10; Package: GEMINI-TESTS; Encoding: utf-8; -*-

(in-package "GEMINI-TESTS")

(in-suite misc-utils)

(test keyword-string-conversion
  "Test the conversion between keystrings and keywords."
  (is (eq :foo (gemini::keystring->keyword "foo")))
  (is (equal "foo" (gemini::keyword->keystring :foo)))
  (is (eq :foo-bar (gemini::keystring->keyword "fooBar")))
  (is (equal "fooBar" (gemini::keyword->keystring :foo-bar)))
  )

(test logging-facade-level-filtering
  "Test that log level threshold suppresses lower-priority messages."
  (let ((gemini::*log-level* :warn))
    (let ((output (with-output-to-string (*trace-output*)
                    (gemini::log-info "info hidden")
                    (gemini::log-warn "warn shown"))))
      (is (not (search "info hidden" output)))
      (is (search "warn shown" output)))))

(test logging-facade-formatting
  "Test that facade emits level prefix and formatted payload."
  (let ((gemini::*log-level* :debug))
    (let ((output (with-output-to-string (*trace-output*)
                    (gemini::log-error "boom ~a" 42))))
      (is (search ";; ERROR:" output))
      (is (search "boom 42" output)))))

(test report-elapsed-time-logging-facade-integration
  "Test that report-elapsed-time macro correctly routes timing output through log-info and respects log levels."
  ;; When log-level is :info, it should show the invoking and finished info
  (let ((gemini::*log-level* :info))
    (let ((output (with-output-to-string (*trace-output*)
                    (gemini::report-elapsed-time "test-action"
                      (sleep 0.01)))))
      (is (search ";; INFO:" output))
      (is (search "Invoking test-action..." output))
      (is (search "test-action finished in" output))))
  ;; When log-level is :warn, info logs should be suppressed
  (let ((gemini::*log-level* :warn))
    (let ((output (with-output-to-string (*trace-output*)
                    (gemini::report-elapsed-time "test-action"
                      (sleep 0.01)))))
      (is (equal "" output)))))

(test conversation-uses-runtime-session-prior-context
  "Test that evolution conversation mutates the active runtime session rather than relying on legacy prior-context globals."
  (let ((orig-begin-evolve (fdefinition 'gemini::begin-evolve))
        (orig-continue-evolve (fdefinition 'gemini::continue-evolve))
        (session (gemini:make-runtime-session
                  :prior-context
                  (list (gemini:content :role "user" :parts (list (part "alpha")))
                        (gemini:content :role "model" :parts (list (part "beta")))))))
    (unwind-protect
         (progn
           (setf (fdefinition 'gemini::begin-evolve)
                 (lambda (prompt)
                   prompt))
           (setf (fdefinition 'gemini::continue-evolve)
                 (lambda (prompt)
                   prompt))
           (gemini:with-runtime-session (session)
             (gemini::conversation 2 "hello"))
           (let ((prior-context (gemini:runtime-session-prior-context session)))
             (is (equal "model" (gemini:get-role (first prior-context))))
             (is (equal "user" (gemini:get-role (second prior-context))))))
      (setf (fdefinition 'gemini::begin-evolve) orig-begin-evolve)
      (setf (fdefinition 'gemini::continue-evolve) orig-continue-evolve))))

(test filesystem-write-tools-propagate-errors
  "Test that filesystem write handlers do not swallow write failures."
  (let* ((handler (cdr (assoc "writeFileLines"
                             (gemini::filesystem-tools-and-handlers)
                             :key #'gemini:get-name
                             :test #'equal))))
    (uiop:with-temporary-file (:stream temp-stream :pathname temp-path :direction :output)
      (close temp-stream)
      (signals error
        (funcall handler
                :file (format nil "~a\\child.txt" temp-path)
                :lines #("alpha"))))))

(test blob-helpers-log-and-return-nil-on-failure
  "Test that blob helpers preserve NIL-on-failure behavior while routing diagnostics through the logging facade."
  (let ((gemini::*log-level* :warn))
    (is (null (gemini::file->blob "Z:\\definitely-missing\\file.bin")))
    (let ((write-output (with-output-to-string (*trace-output*)
                         (is (null (gemini::blob->file "Z:\\definitely-missing\\file.bin"
                                                       "%%%not-base64%%%"))))))
      (is (search "Error decoding blob" write-output)))))

(test asdfx-warning-paths-use-logging-facade
  "Test that ASDF registry warning paths emit through the logging facade."
  (let ((gemini::*log-level* :warn))
    (let ((read-output (with-output-to-string (*trace-output*)
                         (is (null (gemini::parse-source-registry-config
                                   #p"Z:\\definitely-missing\\source-registry.conf"))))))
      (is (search "Could not read ASDF config file" read-output)))
    (let ((directive-output (with-output-to-string (*trace-output*)
                             (multiple-value-bind (dirs trees excludes)
                                 (gemini::interpret-source-registry-forms '((:mystery "x")))
                               (declare (ignore dirs trees excludes))))))
      (is (search "Unknown ASDF source registry directive" directive-output)))))

(test macroexpand-tools-log-through-debug-facade
  "Test that macroexpand tool handlers use debug logging for result and parse diagnostics."
  (let* ((handlers (gemini::lisp-introspection-tools-and-handlers))
         (macroexpand-handler (cdr (assoc "macroexpand" handlers :key #'gemini:get-name :test #'equal))))
    (let ((gemini::*log-level* :debug))
      (let ((output (with-output-to-string (*trace-output*)
                      (let* ((values (funcall macroexpand-handler :expression "(when t :ok)"))
                             (expanded (first values))
                             (expanded-p (second values)))
                        (is (equal '(if t :ok) expanded))
                        (is (not (null expanded-p)))))))
        (is (search "Macroexpand values" output)))
      (let ((output (with-output-to-string (*trace-output*)
                      (signals error
                        (funcall macroexpand-handler :expression "   ")))))
        (is (search "Incomplete expression" output))))))

(test test-filesystem-tools-sandboxed
  "Test filesystem tools inside a safe temporary sandbox directory to verify no leakage."
  (let* ((tools (gemini::filesystem-tools-and-handlers))
         (create-dir-handler (cdr (assoc "createDirectory" tools :key #'gemini:get-name :test #'equal)))
         (write-file-handler (cdr (assoc "writeFileLines" tools :key #'gemini:get-name :test #'equal)))
         (list-dir-handler (cdr (assoc "listDirectory" tools :key #'gemini:get-name :test #'equal)))
         (read-file-handler (cdr (assoc "readFileLines" tools :key #'gemini:get-name :test #'equal))))
    (let* ((temp-root (uiop:temporary-directory))
           (sandbox-dir-name (format nil "gemini-test-sandbox-~a" (get-universal-time)))
           (sandbox-path (namestring (merge-pathnames (make-pathname :directory (list :relative sandbox-dir-name)) temp-root)))
           (test-subdir (format nil "~a/nested/dir/" sandbox-path))
           (test-file (format nil "~a/nested/dir/test.txt" sandbox-path)))
      (unwind-protect
           (progn
             ;; 1. Create a directory inside sandbox
             (funcall create-dir-handler :directory test-subdir)
             (is (not (null (probe-file (parse-namestring test-subdir)))))

             ;; 2. Write file lines
             (funcall write-file-handler :file test-file :lines #("line 1" "line 2"))
             (is (not (null (probe-file (parse-namestring test-file)))))

             ;; 3. Read file lines
             (let ((lines (funcall read-file-handler :pathname test-file)))
               (is (equal "line 1" (elt lines 0)))
               (is (equal "line 2" (elt lines 1))))

             ;; 4. List directory contents
             (let ((contents (coerce (funcall list-dir-handler :directory test-subdir) 'list)))
               (is (some (lambda (item) (search "test.txt" item)) contents))))
        (when (probe-file (parse-namestring sandbox-path))
          (uiop:delete-directory-tree (parse-namestring sandbox-path) :validate t))))))

(test test-git-tool-isolated
  "Test that the git tool works seamlessly with fully-mocked run-program, avoiding any host VCS side effects."
  (let* ((tools (gemini::git-tools-and-handlers))
         (git-handler (cdr (assoc "git" tools :key #'gemini:get-name :test #'equal)))
         (orig-run-program #'uiop:run-program)
         (called-cmd nil)
         (called-args nil))
    (unwind-protect
         (progn
           (setf (fdefinition 'uiop:run-program)
                 (lambda (cmd-args &key output error-output ignore-error-status)
                   (declare (ignore output error-output ignore-error-status))
                   (setf called-cmd (first cmd-args))
                   (setf called-args (cdr cmd-args))
                   "On branch main\nYour branch is up to date."))
           (let ((res (funcall git-handler :arguments #("status"))))
             (is (equal "git" called-cmd))
             (is (equal '("status") called-args))
             (is (search "On branch main" res))))
      (setf (fdefinition 'uiop:run-program) orig-run-program))))

(test test-shell-tools-isolated
  "Test that shell tools like bash and grep execute correctly through a mocked run-program to guarantee no host CLI leakage."
  (let* ((tools (gemini::shell-tools-and-handlers))
         (bash-handler (cdr (assoc "bash" tools :key #'gemini:get-name :test #'equal)))
         (grep-handler (cdr (assoc "grep" tools :key #'gemini:get-name :test #'equal)))
         (orig-run-program #'uiop:run-program)
         (called-args nil))
    (unwind-protect
         (progn
           ;; 1. Test bash tool
           (setf (fdefinition 'uiop:run-program)
                 (lambda (cmd-args &key output error-output ignore-error-status)
                   (declare (ignore output error-output ignore-error-status))
                   (setf called-args cmd-args)
                   "hello shell"))
           (let ((res (funcall bash-handler :command "echo" :arguments #("hello"))))
             (is (equal '("echo" "hello") called-args))
             (is (equal "hello shell" res)))

           ;; 2. Test grep tool
           (setf (fdefinition 'uiop:run-program)
                 (lambda (cmd-args &key output error-output ignore-error-status)
                   (declare (ignore output error-output ignore-error-status))
                   (setf called-args cmd-args)
                   "matched-line"))
           (let ((res (funcall grep-handler :arguments #("-r" "pattern" "."))))
             (is (equal '("grep" "-r" "pattern" ".") called-args))
             (is (equal "matched-line" res))))
      (setf (fdefinition 'uiop:run-program) orig-run-program))))
