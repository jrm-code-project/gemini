;;; -*- Lisp -*-

(in-package "GEMINI")

(defconstant +fence+
  (if (boundp '+fence+)
      (symbol-value '+fence+)
      "```"))

(defun fence-start (language)
  (declare (optimizable-series-function))
  (scan (list (concatenate 'string +fence+ language))))

(defun fence-end ()
  (declare (optimizable-series-function))
  (scan (list +fence+)))

(defun file->part (filename language)
  (part
   (str:join #\Newline
             (collect 'list
               (catenate
                (fence-start language)
                (scan-file filename #'read-line)
                (fence-end))))))

(defun lisp->part (filename)
  (file->part filename "lisp"))

(defparameter
 *analyze-system-instructions*
 (concatenate 'string
              "You are an expert Common Lisp developer with many years experience."
              "  You will be given a file from a Common Lisp system based on"
              " the following system definition."))

(defparameter
 *analyze-instructions*
 (concatenate 'string
              "Analyze the following Common Lisp file."
              "  Ensure the file follows best practices, look for bugs"
              " and ambiguities, check that the file uses ideomatic"
              " Common Lisp, note any problem areas, and give concrete,"
              " actionable suggestions for improvements."))

(defun analyze-file (system filename)
  (let ((*system-instruction*
         (content :parts
                  (list (part *analyze-system-instructions*) 
                        (lisp->part (asdf/system:system-source-file system)))))
        (*include-thoughts* t))
    (invoke-gemini
     (list
      (part *analyze-instructions*)
      (lisp->part (asdf/system:system-relative-pathname system filename))))))
