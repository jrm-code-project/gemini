;;; -*- Lisp -*-

(in-package "GEMINI")

(defparameter +summarize-blogger-post-prompt+
  "Summarize the following blog post in a concise manner, focusing on the main points and key takeaways. Use clear and simple language suitable for a general audience. Avoid technical jargon unless absolutely necessary."
  "Prompt for summarizing a blog post.")

(defun summarize-blogger-post (string)
  "Take a blog post STRING and return a concise summary of the post as a string."
  (let ((*max-output-tokens* 1000)
        (*temperature* 0.25))
    (without-personality
      (invoke-gemini
       (list (part +summarize-blogger-post-prompt+)
             (part string))))))

(defun blog-summaries (blog-id)
  "Return a series of summaries of the posts in the Blogger blog with the given BLOG-ID."
  (declare (optimizable-series-function))
  (map-fn 'string (compose #'summarize-blogger-post #'get-content) (google:scan-blogger-posts blog-id)))

(defparameter +summarize-blog-prompt+
  "Summarize the following blog summaries into a single concise summary, highlighting the main themes and key insights. Use clear and simple language suitable for a general audience. Avoid technical jargon unless absolutely necessary."
  "Prompt for summarizing a blog from its post summaries.")

(defun summarize-blog (blog-id &key (max-posts 5))
  "Summarize the blog with the given BLOG-ID by summarizing up to MAX-POSTS recent posts and then combining those summaries into a single summary of the blog."
  (let ((post-summaries
          (collect 'list
            (map-fn 't #'part
                    (subseries (blog-summaries blog-id) 0 max-posts)))))
    (without-personality
      (invoke-gemini
       (cons (part +summarize-blog-prompt+)
             ;; oldest first
             (reverse post-summaries))))))
