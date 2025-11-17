;;; -*- Mode: lisp; Coding: utf-8; -*-

(in-package "GEMINI")

(defun standard-functions-and-handlers (content-generator)
  "Return a list of standard functions and their handlers based on the content-generator's configuration."
  (let ((config (get-config content-generator)))
    (append
     (mcp-functions-and-handlers content-generator)
     (let ((diary-tool (persona-diary-tool content-generator)))
       (when diary-tool (list diary-tool)))
     (when (get-enable-evolution-tools config) (evolution-tools-and-handlers))
     (when (get-enable-filesystem-tools config) (filesystem-tools-and-handlers))
     (when (get-enable-git-tools config) (git-tools-and-handlers))
     (when (get-enable-interaction-tools config) (interaction-tools-and-handlers))
     (when (get-enable-lisp-introspection-tools config) (lisp-introspection-tools-and-handlers))
     (when (get-enable-misc-tools config) (misc-tools-and-handlers))
     (when (get-enable-shell-tools config) (shell-tools-and-handlers))
     (when (get-enable-web-tools config) (web-tools-and-handlers))
     )))




