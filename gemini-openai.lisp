;;; -*- mode: Lisp; coding: utf-8 -*-

(in-package "GEMINI")

(defun resolve-openai-authorization (&key (authorization-header nil authorization-supplied-p))
  "Resolves the OpenAI-compatible Authorization header value.
Resolution order:
1) Explicit AUTHORIZATION-HEADER argument when supplied.
2) *OPENAI-AUTHORIZATION* runtime/config value.
3) OPENAI_AUTHORIZATION environment variable.
4) OPENAI_API_KEY environment variable (wrapped as Bearer).
5) Optional local fallback Bearer lm-studio when enabled."
  (if authorization-supplied-p
      authorization-header
      (or (and (boundp '*openai-authorization*)
               *openai-authorization*)
          (let ((auth-from-env (uiop:getenv "OPENAI_AUTHORIZATION")))
            (when (and auth-from-env (> (length auth-from-env) 0))
              auth-from-env))
          (let ((api-key (uiop:getenv "OPENAI_API_KEY")))
            (when (and api-key (> (length api-key) 0))
              (format nil "Bearer ~a" api-key)))
          (when (and (boundp '*openai-use-lm-studio-default-authorization*)
                     *openai-use-lm-studio-default-authorization*)
            "Bearer lm-studio"))))

(defun openai-request-headers (&key (authorization-header nil authorization-supplied-p))
  "Builds request headers for OpenAI-compatible chat completion calls."
  (let ((headers (list (cons "Content-Type" "application/json")))
        (resolved-auth (if authorization-supplied-p
                           authorization-header
                           (resolve-openai-authorization))))
    (when (and resolved-auth (> (length resolved-auth) 0))
      (setf headers (append headers (list (cons "Authorization" resolved-auth)))))
    headers))

(defun %%invoke-openai (model-id payload &key (url "http://localhost:1234/v1/chat/completions") (read-timeout 300) (connect-timeout 60))
  "Invoke a local or OpenAI-compatible backend. No Google-tax required."
  (let ((response 
          (report-elapsed-time (format nil "OpenAI model `~a`" model-id)
                (dex:post url
                          :headers (openai-request-headers)
                          :content (cl-json:encode-json-to-string payload)
                          :read-timeout (or read-timeout 300)
                          :connect-timeout (or connect-timeout 60)))))
    response))

(defun parse-openai-response (json-string)
  "Reach into the OpenAI choices array and grab the goddamn signal."
  (let* ((data (cl-json:decode-json-from-string json-string))
         (choice (elt (gethash "choices" data) 0))
         (message (gethash "message" choice))
         (content (gethash "content" message)))
    content))

;; Compatibility shims retained while adapter normalization settles.

(defun openai-field (object &rest keys)
  (apply #'adapter-field object keys))

(defun part->openai-text (part)
  (gemini-part->openai-text part))

(defun content->openai-message (content)
  (gemini-content->openai-message content))

(defun build-openai-payload (model-id payload)
  "Converts a Gemini payload to an OpenAI chat-completions payload."
  (validate-gemini-payload-shape payload)
  (let* ((contents (get-contents payload))
         (system-instruction (get-system-instruction payload)) ;; Grab the soul
         (content-list (adapter-as-list contents))
         (messages (remove nil (mapcar #'gemini-content->openai-message content-list)))
         (generation-config (get-generation-config payload))
         (openai-tools (gemini-tools->openai-tools (get-tools payload))))

    ;; Prepend the system instruction if it exists
    (when system-instruction
      (push (object :role "system" 
                    :content (content->text system-instruction)) 
            messages))
            
    (let ((openai-payload (object :model model-id :messages messages)))

      (when openai-tools
        (setf (get-tools openai-payload) (coerce openai-tools 'vector))
        (setf (gethash "tool_choice" openai-payload) "auto"))

      (apply-gemini-generation-config-to-openai-payload openai-payload generation-config)
      openai-payload)))

(defun openai-response->gemini-response (json-string)
  "Converts an OpenAI chat response JSON string into Gemini-style response objects."
  (handler-case
      (with-decoder-jrm-semantics
        (openai-response-hash->gemini-response
         (cl-json:decode-json-from-string json-string)))
    (error (c)
      (log-warn "Failed to decode OpenAI response: ~a" c)
      (values (object :candidates (list (object :content (content :role "model"
                                                                  :parts (list (part json-string))))))
              nil))))
