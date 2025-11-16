;;; -*- Lisp -*-

(in-package "GEMINI")

(defun web-tools-and-handlers ()
  "Return a list of web-related functions and their handlers."
  (list
   (cons
    (function-declaration
     :name "httpGet"
     :description "Performs an HTTP GET request to the specified URL and returns the response body as a string."
     :behavior :blocking
     :parameters (schema :type :object
                         :properties (object :url
                                             (schema :type :string
                                                     :description "The URL to send the GET request to."))
                         :required (vector :url))
     :response (schema :type :string))
    (lambda (&key url)
      (dexador:get url)))

   (when (and (google:hyperspec-search-engine-id)
              (google:search-engine-api-key))
     (cons
      (function-declaration
       :name "hyperspecSearch"
       :description "Search the Common Lisp Hyperspec for pages about a topic."
       :behavior :blocking
       :parameters (schema :type :object
                           :properties (object :search-terms
                                               (schema :type :string
                                                       :description "The search terms to use for the hyperspec search.  Use spaces to separate terms."))
                           :required (vector :search-terms))
       :response (schema :type :object
                         :properties (object :items
                                             (schema :type :array
                                                     :items (schema :type :object
                                                                    :properties (object 
                                                                                 :title (schema :type :string)
                                                                                 :link (schema :type :string)
                                                                                 :snippet (schema :type :string))
                                                                    :required (vector :link :snippet :title))))
                         :required (vector :items)))
      (lambda (&key search-terms)
        (format *trace-output* "~&;; Search Terms: ~{~a~^ ~}~%" (str:split " " search-terms :omit-nulls t))
        (finish-output *trace-output*)
        (object :items
                (map 'list (lambda (item)
                             (object :title (get-title item)
                                     :link (get-link item)
                                     :snippet (get-snippet item))
                             )
                     (get-items
                      (google:hyperspec-search
                       (str:join "+" (str:split " " search-terms :omit-nulls t)))))))))

   (when (and (google:google-search-engine-id)
              (google:search-engine-api-key))
     (cons
      (function-declaration
       :name "webSearch"
       :description "Search the Web for pages about a topic."
       :behavior :blocking
       :parameters (schema :type :object
                           :properties (object :search-terms
                                               (schema :type :string
                                                       :description "The search terms to use for the web search.  Use spaces to separate terms."))
                           :required (vector :search-terms))
       :response (schema :type :object
                         :properties (object :items
                                             (schema :type :array
                                                     :items (schema :type :object
                                                                    :properties (object 
                                                                                 :title (schema :type :string)
                                                                                 :link (schema :type :string)
                                                                                 :snippet (schema :type :string))
                                                                    :required (vector :link :snippet :title))))
                         :required (vector :items)))
      (lambda (&key search-terms)
        (format *trace-output* "~&;; Search Terms: ~{~a~^ ~}~%" (str:split " " search-terms :omit-nulls t))
        (finish-output *trace-output*)
        (object :items
                (map 'list (lambda (item)
                             (object :title (get-title item)
                                     :link (get-link item)
                                     :snippet (get-snippet item))
                             )
                     (get-items
                      (google:web-search
                       (str:join "+" (str:split " " search-terms :omit-nulls t)))))))))))
