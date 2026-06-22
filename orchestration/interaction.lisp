;;; -*- mode: lisp; coding: utf-8 -*-

(in-package "GEMINI")

;;;; =========================================================================
;;;; Global JSON SSE [DONE] Terminal Shield
;;;; =========================================================================

(eval-when (:load-toplevel :execute)
  (let ((orig-decode (fdefinition 'cl-json:decode-json-from-string)))
    (unless (get 'cl-json:decode-json-from-string 'custom-sse-wrapped)
      (setf (fdefinition 'cl-json:decode-json-from-string)
            (lambda (string &rest args)
              (if (and (stringp string)
                       (or (string-equal string "[DONE]")
                           (string-equal (str:trim string) "[DONE]")))
                  nil
                  (apply orig-decode string args))))
      (setf (get 'cl-json:decode-json-from-string 'custom-sse-wrapped) t))))

(deftype model-option ()
  '(member
    :gemini-2.5-computer-use-preview-10-2025
    :gemini-3.1-flash-tts-preview
    :gemini-2.5-flash-preview-tts
    :gemini-2.5-pro-preview-tts
    :lyria-3-pro-preview
    :gemini-2.5-flash
    :gemini-3.1-pro-preview
    :lyria-3-clip-preview
    :gemini-3.1-flash-lite
    :gemini-3.1-flash-lite-preview
    :gemini-3-flash-preview
    :gemini-3.5-flash
    :gemini-3-pro-preview
    :gemini-2.5-flash-native-audio-preview-12-2025
    :gemini-2.5-flash-image
    :gemini-2.5-flash-lite
    :gemini-2.5-pro
    :gemini-3.1-flash-image-preview
    :gemini-3-pro-image-preview
    :gemini-2.5-flash-lite-preview-09-2025
    :gemini-2.5-flash-preview-09-2025
    :gemini-2.5-computer-use-preview-10-2025
    :gemini-3.1-flash-tts-preview
    :gemini-2.5-flash-preview-tts
    :gemini-2.5-pro-preview-tts
    :lyria-3-pro-preview
    :gemini-2.5-flash
    :gemini-3.1-pro-preview
    :lyria-3-clip-preview
    :gemini-3.1-flash-lite
    :gemini-3.1-flash-lite-preview
    :gemini-3-flash-preview
    :gemini-3.5-flash
    :gemini-3-pro-preview
    :gemini-2.5-flash-native-audio-preview-12-2025
    :gemini-2.5-flash-image
    :gemini-2.5-flash-lite
    :gemini-2.5-pro
    :gemini-3.1-flash-image-preview
    :gemini-3-pro-image-preview
    :gemini-2.5-flash-lite-preview-09-2025
    :gemini-2.5-flash-preview-09-2025))

(deftype agent-option ()
  `(member
    :deep-research-preview-04-2026
    :deep-research-pro-preview-12-2025
    :deep-research-max-preview-04-2026
    :antigravity-preview-05-2026
    :deep-research-preview-04-2026
    :deep-research-pro-preview-12-2025
    :deep-research-max-preview-04-2026
    :antigravity-preview-05-2026))

(defclass request-body ()
  ((model :initarg :model :accessor get-model :type model-option)
   (agent :initarg :agent :accessor get-agent :type agent-option)
   (input :initarg :input :accessor get-input)))

(defmethod initialize-instance :after ((instance request-body) &rest args)
  ;; assert that either the model or agent are specified, but not both
  (assert (or (getf args :model)
              (getf args :agent))))

(deftype content-type ()
  '(member
    :audio
    :document
    :image
    :text
    :video))

(defclass content ()
  ((content-type :initarg :type :type content-type :reader get-content-type)))

(deftype audio-mime-type ()
  '(member
    :audio/aac
    :audio/aiff
    :audio/alaw
    :audio/flac
    :audio/l16
    :audio/m4a
    :audio/mp3
    :audio/mpeg
    :audio/mulaw
    :audio/ogg
    :audio/opus
    :audio/wav))

(deftype document-mime-type ()
  '(member
    :application/pdf
    :text/csv))

(deftype image-mime-type ()
  '(member
    :image/bmp
    :image/gif
    :image/heic
    :image/heif
    :image/jpeg
    :image/png
    :image/tiff
    :image/webp))

(deftype video-mime-type ()
  '(member
    :video/3gpp
    :video/avi
    :video/mov
    :video/mp4
    :video/mpeg
    :video/mpg
    :video/webm
    :video/wmv
    :video/x-flv))

(deftype model-resolution ()
  `(member
    :low
    :medium
    :high
    :ultra-high))

(deftype annotation-type ()
  '(member
    :file-citation
    :place-citation
    :uri-citation))

(defclass audio-content (content)
  ((data :initarg :data :reader get-data :type string)
   (uri  :initarg :uri  :reader get-uri  :type string)
   (mime-type :initarg :mime-type :reader get-mime-type :type audio-mime-type)
   (channels :initarg :channels :reader get-channels :type integer)
   (sample-rate :initarg :sample-rate :reader get-sample-rate :type integer))
  (:default-initargs :type :audio))

(defclass document-content (content)
  ((data :initarg :data :reader get-data :type string)
   (uri  :initarg :uri  :reader get-uri  :type string)
   (mime-type :initarg :mime-type :reader get-mime-type :type document-mime-type))
  (:default-initargs :type :document))

(defclass image-content (content)
  ((data :initarg :data :reader get-data :type string)
   (uri  :initarg :uri  :reader get-uri  :type string)
   (mime-type :initarg :mime-type :reader get-mime-type :type document-mime-type)
   (resolution :initarg :resolution :reader get-resolution :type model-resolution))
  (:default-initargs :type :image))

(defclass annotation ()
  ((type :initarg :type :reader get-annotation-type :type string)))

(defclass file-citation (annotation)
  ((document-uri :initarg :document-uri :reader get-document-uri :type string)
   (file-name :initarg :file-name :reader get-file-name :type string)
   (source :initarg :source :reader get-source :type string)
   (custom-metadata :initarg :custom-metadata :reader get-custom-metadata)
   (page-number :initarg :page-number :reader get-page-number :type integer)
   (media-id :initarg :media-id :reader get-media-id :type string)
   (start-index :initarg :start-index :reader get-start-index :type integer)
   (end-index :initarg :end-index :reader get-end-index :type integer))
  (:default-initargs :type :file-citation))

(defclass place-citation (annotation)
  ((place-id :initarg :place-id :reader get-place-id :type string)
   (name :initarg :name :reader get-name :type string)
   (uri :initarg :uri :reader get-uri :type string)
   (review-snippet :initarg :review-snippet :reader get-review-snippet :type review-snippet)
   (start-index :initarg :start-index :reader get-start-index :type integer)
   (end-index :initarg :end-index :reader get-end-index :type integer))
  (:default-initargs :type :place-citation))

(defclass uri-citation (annotation)
  ((url :initarg :url :reader get-url :type string)
   (title :initarg :title :reader get-title :type string)
   (start-index :initarg :start-index :reader get-start-index :type integer)
   (end-index :initarg :end-index :reader get-end-index :type integer))
  (:default-initargs :type :uri-citation))

(defclass text-content (content)
  ((text :initarg :text :reader get-text :type string)
   (annotation :initarg :annotation :reader get-annotation :type annotation))
  (:default-initargs :type :text))

(defmethod print-object ((obj text-content) stream)
  (format stream "#<TEXT-CONTENT: ~s>"
          (get-text obj)))

(defclass video-content (content)
  ()
  (:default-initargs :type :video))
