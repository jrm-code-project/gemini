;;; -*- Lisp -*-
(in-package "GEMINI")

(require :sb-posix)

(defparameter *lm-studio-models* (merge-pathnames #p".lmstudio/models/" (user-homedir-pathname)))

(defun list-local-lm-studio-models ()
  (mapcar (lambda (pathname)
            (format nil "~a/~a.~a"
                    (car (last (pathname-directory pathname)))
                    (pathname-name pathname)
                    (pathname-type pathname)))
          (directory (merge-pathnames #p"**/*.gguf" *lm-studio-models*))))

(defparameter *uncensored-model*
  (make-pathname :directory (list :relative "HauhauCS" "Gemma-4-E4B-Uncensored-HauhauCS-Aggressive")
                 :name "Gemma-4-E4B-Uncensored-HauhauCS-Aggressive-Q4_K_M"
                 :type "gguf"))

(sb-alien:load-shared-object "kernel32.dll")

(sb-alien:define-alien-routine ("CreateFileMappingA" create-file-mapping) (* t)
  (h-file (* t))
  (lp-attributes (* t))
  (fl-protect sb-alien:unsigned-int)
  (dw-maximum-size-high sb-alien:unsigned-int)
  (dw-maximum-size-low sb-alien:unsigned-int)
  (lp-name (* sb-alien:char)))

(sb-alien:define-alien-routine ("MapViewOfFile" map-view-of-file) (* t)
  (h-file-mapping-object (* t))
  (dw-desired-access sb-alien:unsigned-int)
  (dw-file-offset-high sb-alien:unsigned-int)
  (dw-file-offset-low sb-alien:unsigned-int)
  (dw-number-of-bytes-to-map sb-alien:size-t))

(sb-alien:define-alien-routine ("UnmapViewOfFile" unmap-view-of-file) sb-alien:int
  (lp-base-address (* t)))

(sb-alien:define-alien-routine ("CloseHandle" close-handle) sb-alien:int
  (h-object (* t)))

(defstruct mapped-model
  sap      ; The System Area Pointer (the raw address)
  size     ; Total size in bytes
  h-file   ; The Windows file handle
  h-map)   ; The Windows mapping handle

(defun load-model (path)
  ;; native-namestring converts the #P object into a string Windows understands
  (let* ((filename (sb-ext:native-namestring path))
         (h-file (create-file filename
                             #x80000000 ; GENERIC_READ
                             #x00000001 ; FILE_SHARE_READ
                             nil 
                             3          ; OPEN_EXISTING
                             #x00000080 ; FILE_ATTRIBUTE_NORMAL
                             nil))
         (size (get-file-size-h h-file)))
    
    (when (sb-sys:sap= h-file (sb-sys:int-sap -1))
      (error "Fuck! Could not open file: ~A (Windows Error: ~A)" 
             filename 
             (sb-alien:alien-funcall (sb-alien:extern-alien "GetLastError" (function sb-alien:unsigned-long)))))

    (let ((h-map (create-file-mapping h-file nil #x02 0 0 nil))) ; PAGE_READONLY
      (if (not (sb-sys:sap= h-map (sb-sys:int-sap 0)))
          (let ((sap (map-view-of-file h-map #x04 0 0 0))) ; FILE_MAP_READ
            (if (not (sb-sys:sap= sap (sb-sys:int-sap 0)))
                (make-mapped-model :sap sap :size size :h-file h-file :h-map h-map)
                (progn
                  (close-handle h-map)
                  (close-handle h-file)
                  (error "Mapping failed. View creation choked."))))
          (progn
            (close-handle h-file)
            (error "Mapping failed. CreateFileMapping returned NIL."))))))

;; You'll need these extra definitions to make it work
(sb-alien:define-alien-routine ("CreateFileA" create-file) (* t)
  (lp-file-name (* sb-alien:char))
  (dw-desired-access sb-alien:unsigned-int)
  (dw-share-mode sb-alien:unsigned-int)
  (lp-security-attributes (* t))
  (dw-creation-disposition sb-alien:unsigned-int)
  (dw-flags-and-attributes sb-alien:unsigned-int)
  (h-template-file (* t)))

(defun get-file-size-h (h-file)
  (sb-alien:with-alien ((size sb-alien:long-long))
    (sb-alien:alien-funcall 
     (sb-alien:extern-alien "GetFileSizeEx" (function sb-alien:int (* t) (* sb-alien:long-long)))
     h-file (sb-alien:addr size))
    size))
