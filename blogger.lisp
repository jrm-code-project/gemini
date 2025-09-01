;;; -*- Lisp -*-

(in-package "GEMINI")

(defun summarize-blogger-post (string)
  (let ((*max-output-tokens* 1000)
        (*temperature* 0.5))
    (without-personality
      (invoke-gemini
       (list (part "Summarize the following blog post in a concise manner, focusing on the main points and key takeaways. Use clear and simple language suitable for a general audience. Avoid technical jargon unless absolutely necessary.")
             (part string))))))

(defun blog-summaries (blog-id)
  (declare (optimizable-series-function))
  (map-fn 'string #'summarize-blogger-post (google:scan-blogger-posts blog-id)))

(defun summarize-blog (blog-id &key (max-posts 5))
  (let ((post-summaries
          (collect 'list
            (map-fn 't (lambda (summary) (part summary))
                    (subseries (blog-summaries blog-id) 0 max-posts)))))
    (without-personality
      (invoke-gemini
       (cons (part "Summarize the following blog posts into a single concise summary, highlighting the main themes and key insights. Use clear and simple language suitable for a general audience. Avoid technical jargon unless absolutely necessary.")
             (reverse post-summaries))))))
