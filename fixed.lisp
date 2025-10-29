(defun improve-prompt ()
  (let ((prompt (uiop:read-file-lines (asdf/system:system-relative-pathname
                                       (asdf/system:find-system "gemini")
                                       "prompt.txt"))))
    (without-personality
      (invoke-gemini (format nil "Read a prompt from ~a, improve it as per the instructions in ~a, and write the improved prompt to ~a."
                             (asdf/system:system-relative-pathname
                              (asdf/system:find-system "gemini")
                              "prompt.txt")
                             (asdf/system:system-relative-pathname
                              (asdf/system:find-system "gemini")
                              "prompt.txt")
                             (asdf/system:system-relative-pathname
                              (asdf/system:find-system "gemini")
                              "prompt.txt"))))))

