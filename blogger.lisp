;;; -*- Lisp -*-

(in-package "GEMINI")

(defun blogger-get (blog-id &optional next-page-token)
  (let ((response (dexador:get (format nil "https://www.googleapis.com/blogger/v3/blogs/~d/posts~@[?pageToken=~a~]" blog-id next-page-token)
                               :headers `(("Accept" . "application/json")
                                          ("x-goog-api-key" . ,(blogger-api-key))))))
    (if (stringp response)
        (with-decoder-jrm-semantics
          (cl-json:decode-json-from-string response))
        (with-decoder-jrm-semantics
          (cl-json:decode-json-from-string
           (flex:octets-to-string response :external-format :utf-8))))))

(defun %scan-blogger-posts (blog-id)
  (declare (optimizable-series-function))
  (map-fn 'string
          (lambda (post) (gethash :content post))
          (choose-if #'hash-table-p
                     (map-fn 't #'car
                             (scan-fn t
                                      (lambda () (let ((page (blogger-get blog-id)))
                                                   (append (coerce (gethash :items page) 'list)
                                                           (list  (gethash :next-page-token page)))))
                                      (lambda (stack)
                                        (if (stringp (car stack))
                                            (let ((page (blogger-get blog-id (car stack))))
                                              (append (coerce (gethash :items page) 'list)
                                                      (list (gethash :next-page-token page))))
                                            (cdr stack)))
                                      (lambda (stack) (null stack)))))))

(defun summarize-blogger-post (string)
  (let ((*max-output-tokens* 1000)
        (*temperature* 0.5))
    (without-personality
      (invoke-gemini
       (list (part "Summarize the following blog post in a concise manner, focusing on the main points and key takeaways. Use clear and simple language suitable for a general audience. Avoid technical jargon unless absolutely necessary.")
             (part string))))))

(defun blog-summaries (blog-id)
  (declare (optimizable-series-function))
  (map-fn 'string #'summarize-blogger-post (%scan-blogger-posts blog-id)))

(defun summarize-blog (blog-id &key (max-posts 5))
  (let ((post-summaries
          (collect 'list
            (map-fn 't (lambda (summary) (part summary))
                    (subseries (blog-summaries blog-id) 0 max-posts)))))
    (without-personality
      (invoke-gemini
       (cons (part "Summarize the following blog posts into a single concise summary, highlighting the main themes and key insights. Use clear and simple language suitable for a general audience. Avoid technical jargon unless absolutely necessary.")
             (reverse post-summaries))))))
