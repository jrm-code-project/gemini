;;; -*- mode: Lisp; coding: utf-8 -*-

(in-package "GEMINI")

(defun process-thought (thought)
  "Processes a thought part object.
   If the thought is a text part, it formats the text and outputs it to *trace-output*."
  (format *trace-output* "~&~%~{;; ~a~%~}~%"
          (reverse
           (let ((rev (reverse (mappend #'reflow-line (str:split #\newline (get-text thought))))))
                (if (and rev (string= "" (car rev)))
                    (cdr rev)
                    rev)))))

(defun strip-thoughts-from-part (part)
  "If PART is a thought part, processes it and returns NIL to exclude it from the output.
   Otherwise, returns PART unchanged."
  (if (thought-part? part)
      (progn (process-thought part)
             nil)
      part))

(defun strip-thoughts-from-parts (parts)
  "Processes a list of PARTS, stripping out any thought parts and processing them for output.
   Returns a new list of parts with thoughts removed."
  (remove nil (map 'list #'strip-thoughts-from-part parts)))

(defun strip-thoughts-from-content (content)
  "Processes the CONTENT object, stripping out any thought parts from its parts.
   Returns a new content object with thoughts removed, or NIL if all parts were thoughts."
  (let* ((parts* (strip-thoughts-from-parts (get-parts content))))
    (when parts*
      (let ((stripped (object :parts parts*)))
        (when (get-role content)
          (setf (get-role stripped) (get-role content)))
        stripped))))

(defun strip-thoughts-from-candidate (candidate)
  "Processes a candidate object, stripping out any thought parts from its content.
   Returns a new candidate object with thoughts removed, or NIL if the content was entirely thoughts."
  (let ((content* (strip-thoughts-from-content (get-content candidate))))
    (when content*
      (let ((stripped (object :content content*)))
        (when (get-finish-reason candidate)
          (setf (get-finish-reason stripped) (get-finish-reason candidate)))
        (when (get-index candidate)
          (setf (get-index stripped) (get-index candidate)))
        (when (get-citation-metadata candidate)
          (setf (get-citation-metadata stripped) (get-citation-metadata candidate)))
        stripped))))

(defun strip-thoughts-from-candidates (candidates)
  "Processes a list or vector of candidates, stripping out any thought parts from their content.
   Returns a new list of candidates with thoughts removed, excluding any candidates that were entirely thoughts."
  (remove nil (map 'list #'strip-thoughts-from-candidate candidates)))

(defun strip-and-print-thoughts (results)
  "Processes the RESULTS object, stripping out any thought parts from its candidates and printing them to *trace-output*.
   Returns a new results object with thoughts removed from candidates, or NIL if all candidates were entirely thoughts."
  (let ((candidates* (strip-thoughts-from-candidates (get-candidates results))))
    (when candidates*
      (let ((stripped (object :candidates candidates*)))
        (when (get-model-version results)
          (setf (get-model-version stripped) (get-model-version results)))
        (when (get-response-id results)
          (setf (get-response-id stripped) (get-response-id results)))
        (when (get-usage-metadata results)
          (setf (get-usage-metadata stripped) (get-usage-metadata results)))
        stripped))))

(defun print-text (bowdlerize results)
  "Prints the text parts from the results to *trace-output*.
   Reflows prose into 80-column paragraphs with a leading space indent, 
   and intelligently wraps list items and blockquotes."
  (labels ((smart-wrap (text width &key (first-prefix "") (rest-prefix ""))
             ;; Joins words and wraps them, respecting the prefixes for first and subsequent lines.
             (let* ((words (remove "" (str:split " " (cl-ppcre:regex-replace-all "\\s+" text " ")) :test #'string=))
                    (current-line first-prefix)
                    (col (length first-prefix))
                    (first-word-p t))
               (if (null words)
                   (format *trace-output* "~&~a~%" (str:trim-right first-prefix))
                   (progn
                     (dolist (word words)
                       (let ((word-len (length word)))
                         (cond (first-word-p
                                (setf current-line (concatenate 'string current-line word)
                                      col (+ col word-len)
                                      first-word-p nil))
                               ((> (+ col word-len 1) width)
                                (format *trace-output* "~&~a~%" (str:trim-right current-line))
                                (setf current-line (concatenate 'string rest-prefix word)
                                      col (+ (length rest-prefix) word-len)))
                               (t
                                (setf current-line (concatenate 'string current-line " " word)
                                      col (+ col word-len 1))))))
                     (format *trace-output* "~&~a~%" (str:trim-right current-line))))))

           (reflow-paragraph (lines)
             ;; Standard prose reflow - Now with the leading space indent!
             (when lines
               (smart-wrap (str:join " " lines) 80 :first-prefix "  ")))

           (process-text-buffer (lines)
             ;; Group buffered lines into paragraphs (separated by blank lines)
             (let next ((remaining lines) (para-acc nil))
               (cond ((null remaining)
                      (when para-acc (reflow-paragraph (reverse para-acc))))
                     ((str:emptyp (str:trim (car remaining)))
                      (when para-acc (reflow-paragraph (reverse para-acc)))
                      (format *trace-output* "~%")
                      (next (cdr remaining) nil))
                     (t
                      (next (cdr remaining) (cons (car remaining) para-acc))))))

           (get-prefix (regex line)
             (multiple-value-bind (start end reg-start reg-end)
                 (cl-ppcre:scan regex line)
               (declare (ignore end))
               (when start
                 (subseq line (aref reg-start 0) (aref reg-end 0)))))

           (is-header-or-rule (line)
             (let ((t-line (str:trim line)))
               (or (str:starts-with? "#" t-line)
                   (cl-ppcre:scan "^[-*_]{3,}\\s*$" t-line)))))

    (let ((candidates (get-candidates results)))
      (when candidates
        (dolist (candidate (if (consp candidates)
                               candidates
                               (and (vectorp candidates)
                                    (> (length candidates) 0)
                                    (coerce candidates 'list))))
          (let ((content (get-content candidate)))
            (when content
              (let next-part ((parts (coerce (get-parts content) 'list)))
                (when parts
                  (if (not (text-part? (car parts)))
                      (next-part (cdr parts))
                      (let* ((text (get-text (car parts)))
                             (clean-text (if bowdlerize
                                             (cl-ppcre:regex-replace-all bowdlerize text "")
                                             text)))
                        (let process-lines ((lines (str:split #\Newline clean-text))
                                            (in-code-p nil)
                                            (text-buffer nil))
                          (cond
                            ((null lines)
                             (when text-buffer (process-text-buffer (reverse text-buffer)))
                             (next-part (cdr parts)))
                            (t
                             (let* ((line (car lines))
                                    (is-fence (str:starts-with? "```" (str:trim line)))
                                    (lp (unless in-code-p (get-prefix "^(\\s*(?:[-*+]|\\d+\\.)\\s+)" line)))
                                    (qp (unless in-code-p (get-prefix "^(\\s*>\\s*)" line)))
                                    (sr (unless in-code-p (is-header-or-rule line))))
                               (cond
                                 (is-fence
                                  (when text-buffer (process-text-buffer (reverse text-buffer)))
                                  (format *trace-output* "~&~a~%" line)
                                  (process-lines (cdr lines) (not in-code-p) nil))
                                 (in-code-p
                                  (format *trace-output* "~&~a~%" line)
                                  (process-lines (cdr lines) t nil))
                                 (sr
                                  (when text-buffer (process-text-buffer (reverse text-buffer)))
                                  (smart-wrap line 80)
                                  (process-lines (cdr lines) nil nil))
                                 (lp
                                  (when text-buffer (process-text-buffer (reverse text-buffer)))
                                  (let ((content (subseq line (length lp)))
                                        (indent (make-string (length lp) :initial-element #\Space)))
                                    (smart-wrap content 80 :first-prefix lp :rest-prefix indent))
                                  (process-lines (cdr lines) nil nil))
                                 (qp
                                  (when text-buffer (process-text-buffer (reverse text-buffer)))
                                  (let ((content (subseq line (length qp))))
                                    (smart-wrap content 80 :first-prefix qp :rest-prefix qp))
                                  (process-lines (cdr lines) nil nil))
                                 (t
                                  (process-lines (cdr lines) nil (cons line text-buffer))))))))))))))))))
  results)

(defun extract-function-calls-from-candidate (candidate)
  (let ((content (get-content candidate)))
    (when content
      (remove-if-not #'function-call-part? (coerce (get-parts content) 'list)))))

(defun extract-function-calls-from-results (results)
  "Extracts function calls from the results.
   Returns a list of function call parts if present, otherwise NIL."
  (let ((candidates (get-candidates results)))
    (cond ((and (consp candidates)
                (null (cdr candidates)))
           (extract-function-calls-from-candidate (car candidates)))
          ((and (vectorp candidates)
                (= (length candidates) 1))
           (extract-function-calls-from-candidate (svref candidates 0))))))
