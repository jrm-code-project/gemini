;;; -*- mode: Lisp; coding: utf-8 -*-

(in-package "GEMINI")

(defun update-interaction-stream-session (session event)
  (let ((interaction-id (adapter-field event "interactionId" "interaction_id" :interaction-id :interaction_id :interaction--id))
        (environment-id (adapter-field event "environmentId" "environment_id" :environment-id :environment_id :environment--id)))
    (when interaction-id
      (setf (runtime-session-interaction-id session) interaction-id))
    (when environment-id
      (setf (runtime-session-environment-id session) environment-id))
    (let ((interaction (adapter-field event "interaction" :interaction)))
      (when interaction
        (let ((id (adapter-field interaction "id" :id))
              (env-id (adapter-field interaction "environmentId" "environment_id" :environment-id :environment_id :environment--id)))
          (when id
            (setf (runtime-session-interaction-id session) id))
          (when env-id
            (setf (runtime-session-environment-id session) env-id)))))))

(defun update-lmstudio-stream-session (session event)
  (let ((event-type (adapter-field event "type" :type)))
    (when (and event-type (string-equal event-type "chat.end"))
      (let* ((result (adapter-field event "result" :result))
             (response-id (and result
                               (adapter-field result
                                              "response_id"
                                              :response_id
                                              :response-id
                                              :response--id))))
        (when response-id
          (setf (runtime-session-interaction-id session) response-id))))))
