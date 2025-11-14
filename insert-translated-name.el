;;; insert-translated-name.el --- Insert translated string as variable or function name  -*- lexical-binding: t; -*-

;; Filename: insert-translated-name.el
;; Description: Insert translated string as variable or function name
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2018, Andy Stewart, all rights reserved.
;; Created: 2018-09-22 10:54:16
;; Version: 3.0

;;; Require
(require 'json)
(require 'subr-x)
(require 'cl-seq)
(require 'url) ;; for url-retrieve

;;; Code:

;;;;;;;;;;;;;;;;;;;;; Customize options ;;;;;;;;;;;;;;;;;;;;;
(defgroup insert-translated-name nil
  "Search and refacotry code base on ripgrep."
  :group 'insert-translated-name)

(defcustom insert-translated-name-program "libretranslate"
  "Use `crow', `ollama', `llm' or `libretranslate' to translate input.
Set to \"libretranslate\" to use a LibreTranslate HTTP API (default localhost:5000)."
  :group 'insert-translated-name
  :type 'string)

(defcustom insert-translated-name-crow-engine "google"
  "the crow app engine"
  :group 'insert-translated-name
  :type 'string)

(defcustom insert-translated-name-ollama-model-name "llama3:8b"
  "The model name of ollama."
  :group 'insert-translated-name
  :type 'string)

(defcustom insert-translated-name-prompt
  "You are a helpful, professional translator. Translate the given text to English and return only the translated text."
  "The prompt sent to an LLM when using the `llm' backend."
  :group 'insert-translated-name
  :type 'string)

;; LibreTranslate related settings
(defcustom insert-translated-name-libretranslate-url "http://localhost:5000/translate"
  "The LibreTranslate API endpoint used for translations."
  :group 'insert-translated-name
  :type 'string)

(defcustom insert-translated-name-libretranslate-source "zh"
  "Source language code for LibreTranslate."
  :group 'insert-translated-name
  :type 'string)

(defcustom insert-translated-name-libretranslate-target "en"
  "Target language code for LibreTranslate."
  :group 'insert-translated-name
  :type 'string)

(defface insert-translated-name-font-lock-mark-word
  '((t (:foreground "White" :background "#007aff" :bold t)))
  "Face for keyword match."
  :group 'insert-translated-name)

(defvar insert-translated-name-ollama-file (expand-file-name "ollama.py" (if load-file-name
                                                                             (file-name-directory load-file-name)
                                                                           default-directory)))

(defvar insert-translated-name-origin-style-mode-list
  '(text-mode erc-mode rcirc-mode))

(defvar insert-translated-name-line-style-mode-list
  '(web-mode emacs-lisp-mode inferior-emacs-lisp-mode css-mode))

(defvar insert-translated-name-camel-style-mode-list
  '(js-mode go-mode))

(defvar insert-translated-name-underline-style-mode-list
  '(ruby-mode))

(defvar insert-translated-name-llm-provider nil
  "The privoder of llm.")

(defvar insert-translated-name-default-style "underline"
  "The default translation style, which can be set to \"origin\", \"line\", \"camel\" or \"underline\".")
;;;;;;;;;;;;;;;;;;;;; Interactive functions ;;;;;;;;;;;;;;;;;;;;;
(defun insert-translated-name-insert (arg)
  (interactive "p")
  (if (or
       (equal arg 4)
       (and (boundp 'insert-translated-name-original-translation)
            insert-translated-name-original-translation)
       (insert-translated-name-in-string-p)
       (insert-translated-name-in-comment-p)
       (insert-translated-name-in-commit-buffer-p)
       (minibuffer-window-active-p (get-buffer-window)))
      (insert-translated-name-insert-original-translation)
    (insert-translated-name-active
     (cond
      ((insert-translated-name-match-modes insert-translated-name-origin-style-mode-list)
       "origin")
      ((insert-translated-name-match-modes insert-translated-name-line-style-mode-list)
       "line")
      ((insert-translated-name-match-modes insert-translated-name-camel-style-mode-list)
       "camel")
      ((insert-translated-name-match-modes insert-translated-name-underline-style-mode-list)
       "underline")
      (t
       insert-translated-name-default-style)))))

(defun insert-translated-name-insert-original-translation ()
  (interactive)
  (insert-translated-name-active "comment"))

(defun insert-translated-name-insert-with-line ()
  (interactive)
  (insert-translated-name-active "line"))

(defun insert-translated-name-insert-with-underline ()
  (interactive)
  (insert-translated-name-active "underline"))

(defun insert-translated-name-insert-with-camel ()
  (interactive)
  (insert-translated-name-active "camel"))

(defun insert-translated-name-replace ()
  (interactive)
  (insert-translated-name-replace-symbol
   (cond ((insert-translated-name-match-modes insert-translated-name-line-style-mode-list)
          "line")
         ((insert-translated-name-match-modes insert-translated-name-camel-style-mode-list)
          "camel")
         ((insert-translated-name-match-modes insert-translated-name-underline-style-mode-list)
          "underline")
         (t
          insert-translated-name-default-style))))

(defun insert-translated-name-replace-with-line ()
  (interactive)
  (insert-translated-name-replace-symbol "line"))

(defun insert-translated-name-replace-with-underline ()
  (interactive)
  (insert-translated-name-replace-symbol "underline"))

(defun insert-translated-name-replace-with-camel ()
  (interactive)
  (insert-translated-name-replace-symbol "camel"))

;;;;;;;;;;;;;;;;;;;;; Helper functions ;;;;;;;;;;;;;;;;;;;;;
(defun insert-translated-name-replace-symbol (style)
  (let ((word (if (use-region-p)
                  (buffer-substring-no-properties (region-beginning) (region-end))
                (thing-at-point 'symbol))))
    (if (use-region-p)
        (kill-region (region-beginning) (region-end))
      (kill-region (beginning-of-thing 'symbol) (end-of-thing 'symbol)))
    (insert-translated-name-query-translation word style)))

(defun insert-translated-name-match-modes (mode-list)
  (cl-remove-if 'null (mapcar #'(lambda (mode) (derived-mode-p mode)) mode-list)))

(defun insert-translated-name-use-original-translation ()
  (set (make-local-variable 'insert-translated-name-original-translation) t))

(defun insert-translated-name-active (style)
  ;; Enable input method if user has load it.
  (activate-input-method default-input-method)

  ;; Add monitor hook.
  (add-hook 'after-change-functions 'insert-translated-name-monitor-after-change nil t)

  ;; Make sure build hash to contain placeholder.
  (unless (boundp 'insert-translated-name-placeholder-hash)
    (set (make-local-variable 'insert-translated-name-placeholder-hash) (make-hash-table :test 'equal)))

  ;; Make sure clean active overlay first.
  (when (and (boundp 'insert-translated-name-active-overlay)
             insert-translated-name-active-overlay)
    (delete-overlay insert-translated-name-active-overlay))

  ;; Reset active local variables
  (set (make-local-variable 'insert-translated-name-active-point) (point))
  (set (make-local-variable 'insert-translated-name-active-style) style)
  (set (make-local-variable 'insert-translated-name-active-overlay) (make-overlay (point) (point)))

  ;; Active new overlay from current point.
  (overlay-put insert-translated-name-active-overlay 'face 'insert-translated-name-font-lock-mark-word)

  ;; Print play hint.
  (unless (minibuffer-window-active-p (get-buffer-window))
    (message "Type Chinese and press SPACE to translate.")))

(defun insert-translated-name-inactive (&optional keep-style)
  (interactive)
  ;; Disable input method if user has load it.
  (deactivate-input-method)

  ;; Delete active overlay.
  (when (and (boundp 'insert-translated-name-active-overlay)
             insert-translated-name-active-overlay)
    (delete-overlay insert-translated-name-active-overlay))

  ;; Clean active local variables.
  (set (make-local-variable 'insert-translated-name-active-point) nil)
  (when (and (boundp 'insert-translated-name-active-overlay)
             insert-translated-name-active-overlay)
    (set (make-local-variable 'insert-translated-name-active-overlay) nil))

  ;; Clean style.
  (unless keep-style
    (set (make-local-variable 'insert-translated-name-active-style) nil)))

(defun insert-translated-name-monitor-after-change (start end len)
  (when (and (boundp 'insert-translated-name-active-point))
    (if insert-translated-name-active-point
        (let ((translate-start insert-translated-name-active-point)
              (translate-end (point)))
          (cond
           ;; Translate current Chinese words after press SPACE.
           ((string-equal (buffer-substring-no-properties start end) " ")
            (let ((word (buffer-substring-no-properties translate-start translate-end)))
              ;; Delete Chinese words.
              (kill-region translate-start translate-end)

              ;; Query translation.
              (insert-translated-name-query-translation word insert-translated-name-active-style)

              ;; Inactive.
              (insert-translated-name-inactive nil)))
           ;; Update active overlay bound if user press any other non-SPACE character.
           (t
            (move-overlay insert-translated-name-active-overlay translate-start translate-end)))))))

(defun insert-translated-name-current-parse-state ()
  "Return parse state of point from beginning of defun."
  (let ((point (point)))
    (beginning-of-defun)
    (when (equal point (point))
      (beginning-of-line))
    (parse-partial-sexp (point) point)))

(defun insert-translated-name-in-commit-buffer-p ()
  (and (string-equal (buffer-name) "COMMIT_EDITMSG")
       (save-excursion
         (goto-char (point-min))
         (search-forward-regexp "#\\s-Please\\s-enter\\s-the\\s-commit\\s-message\\s-for\\s-your\\s-changes." nil t))))

(defun insert-translated-name-in-string-p (&optional state)
  (or (nth 3 (or state (insert-translated-name-current-parse-state)))
      (and
       (eq (get-text-property (point) 'face) 'font-lock-string-face)
       (eq (get-text-property (- (point) 1) 'face) 'font-lock-string-face))
      (and
       (eq (get-text-property (point) 'face) 'font-lock-doc-face)
       (eq (get-text-property (- (point) 1) 'face) 'font-lock-doc-face))
      ))

(defun insert-translated-name-in-comment-p (&optional state)
  (or (nth 4 (or state (insert-translated-name-current-parse-state)))
      (eq (get-text-property (point) 'face) 'font-lock-comment-face)))

(defun insert-translated-name-convert-translation (translation style)
  (let ((words (cl-remove-if #'string-empty-p (split-string translation " "))))
    (cond ((string-equal style "line")
           (string-join (mapcar 'downcase words) "-"))
          ((string-equal style "underline")
           (string-join (mapcar 'downcase words) "_"))
          ((string-equal style "camel")
           (concat (downcase (car words)) (string-join (mapcar 'capitalize (cdr words)))))
          ((or
            (string-equal style "comment")
            (string-equal style "origin"))
           translation))))

(defun insert-translated-name-update-translation-in-buffer (word style translation insert-buffer placeholder)
  (let ((result (insert-translated-name-convert-translation translation style)))
    (save-excursion
      (with-current-buffer insert-buffer
        (let ((placeholder-point (gethash placeholder insert-translated-name-placeholder-hash)))
          (if placeholder-point
              (progn
                ;; Insert result at placeholder point .
                (goto-char placeholder-point)
                (insert result)

                ;; Remove placeholder from hash.
                (remhash placeholder insert-translated-name-placeholder-hash))
            (message (format "Something wrong that we can't found placeholder for %s: %s" word translation))))))))

(defun insert-translated-name-generate-uuid ()
  "Generate a 32 character UUID."
  (md5 (number-to-string (float-time))))

(defun insert-translated-name-query-translation (word style)
  (if (string-equal word "")
      (message "Nothing input, cancel translate.")
    (let ((placeholder (insert-translated-name-generate-uuid)))
      ;; Store placeholder in hash.
      (unless (boundp 'insert-translated-name-placeholder-hash)
        (set (make-local-variable 'insert-translated-name-placeholder-hash) (make-hash-table :test 'equal)))

      (puthash placeholder (point) insert-translated-name-placeholder-hash)

      ;; Query translation.
      (insert-translated-name-retrieve-translation word style placeholder)
      )))

(defvar insert-translated-name-word nil)
(defvar insert-translated-name-style nil)
(defvar insert-translated-name-buffer-name nil)
(defvar insert-translated-name-placeholder nil)

(defun insert-translated-name-process-sentinel (process event)
  (when (string= event "finished\n")
    (with-current-buffer (process-buffer process)
      (let* ((output (buffer-string))
             (first-line (substring-no-properties output 0 (or (string-match "\n" output) (length output)))))
        (insert-translated-name-update-translation-in-buffer
         insert-translated-name-word
         insert-translated-name-style
         (pcase insert-translated-name-program
           ("crow" (alist-get 'translation (json-read-from-string output)))
           ("ollama"
            (pcase insert-translated-name-style
              ("origin" (replace-regexp-in-string "\"" "" (string-trim first-line)))
              (_ (replace-regexp-in-string "\"\\|'\\|‘\\|\\.\\|,\\|，\\|。\\|\\?\\|\\!" "" (string-trim first-line)))
              )))
         insert-translated-name-buffer-name
         insert-translated-name-placeholder)
        ))))

(defun insert-translated-name-api-key-from-auth-source (host &optional user)
  "Lookup api key in the auth source.
By default, the LLM host for the active backend is used as HOST,
and \"apikey\" as USER."
  (if-let ((secret
            (plist-get
             (car (auth-source-search
                   :host host
                   :user (or user "apikey")
                   :require '(:secret)))
             :secret)))
      (if (functionp secret)
          (encode-coding-string (funcall secret) 'utf-8)
        secret)
    (user-error "No `api-key' found in the auth source")))

;; LibreTranslate callback
(defun insert-translated-name--libretranslate-callback (status)
  "Callback for url-retrieve when calling LibreTranslate.
STATUS is the status plist provided by url-retrieve."
  (let ((err (plist-get status :error)))
    (if err
        (message "LibreTranslate request error: %s" err)
      (goto-char (point-min))
      (if (search-forward "\n\n" nil t)
          (let* ((json-object-type 'alist)
                 (data (condition-case nil
                           (json-read)
                         (error nil)))
                 (translation (or (and (listp data) (alist-get 'translatedText data))
                                  (and (listp data) (alist-get 'translation data))
                                  ;; if server returns string directly
                                  (and (stringp data) data))))
            (if translation
                (insert-translated-name-update-translation-in-buffer
                 insert-translated-name-word
                 insert-translated-name-style
                 (string-trim translation)
                 insert-translated-name-buffer-name
                 insert-translated-name-placeholder)
              (message "LibreTranslate: unexpected response: %s" (buffer-string))))
        (message "LibreTranslate: failed to find response body."))))
  ;; kill the temporary response buffer
  (when (buffer-name)
    (kill-buffer (current-buffer))))

(defun insert-translated-name-retrieve-translation (word style placeholder)
  (setq insert-translated-name-word word)
  (setq insert-translated-name-style style)
  (setq insert-translated-name-buffer-name (buffer-name))
  (setq insert-translated-name-placeholder placeholder)
  (when (get-buffer " *insert-translated-name*")
    (kill-buffer " *insert-translated-name*"))
  (if (and (string= insert-translated-name-program "llm") insert-translated-name-llm-provider)
      ;;If the LLM is activated
      (progn
        ;;If packages are not imported they will be imported automatically
        (unless (featurep 'llm)
          (require 'llm))
        (llm-chat-async
         insert-translated-name-llm-provider
         (llm-make-simple-chat-prompt (format insert-translated-name-prompt word))
         (lambda (text)
           (let* ((translate (string-trim text))
                  (translate
                   (pcase insert-translated-name-style
                     ("origin" (replace-regexp-in-string "\"" "" (string-trim translate)))
                     (_ (replace-regexp-in-string "\"\\|'\\|‘\\|\\.\\|,\\|，\\|。\\|\\?\\|\\!" "" (string-trim translate))))))
             (insert-translated-name-update-translation-in-buffer
              insert-translated-name-word
              insert-translated-name-style
              translate
              insert-translated-name-buffer-name
              insert-translated-name-placeholder)))
         (lambda (msg)
           (user-error (format "LLM Interface call failed:%s" msg)))))
    ;; If not using LLM
    (pcase insert-translated-name-program
      ("libretranslate"
       ;; Use url-retrieve to POST JSON to LibreTranslate endpoint asynchronously.
       (let* ((url insert-translated-name-libretranslate-url)
              (payload (json-encode `(("q" . ,word)
                                      ("source" . ,insert-translated-name-libretranslate-source)
                                      ("target" . ,insert-translated-name-libretranslate-target)
                                      ("format" . "text"))))
              ;; Encode payload as UTF-8 bytes to avoid "Multibyte text in HTTP request"
              (url-request-data (encode-coding-string payload 'utf-8))
              (url-request-method "POST")
              (url-request-extra-headers
               '(("Content-Type" . "application/json; charset=utf-8")
                 ("Accept" . "application/json"))))
         (url-retrieve url #'insert-translated-name--libretranslate-callback nil t)))
      ("crow"
       (let ((process
              (start-process
               "insert-translated-name"
               " *insert-translated-name*"
               "crow" "-t" "en" "--json" "-e" insert-translated-name-crow-engine word)))
         (set-process-sentinel process 'insert-translated-name-process-sentinel)))
      ("ollama"
       (let ((process
              (start-process
               "insert-translated-name"
               " *insert-translated-name*"
               "python"
               insert-translated-name-ollama-file
               insert-translated-name-ollama-model-name
               (format "'%s'" word))))
         (set-process-sentinel process 'insert-translated-name-process-sentinel)))
      (_
       ;; default fallback to crow behavior
       (let ((process
              (start-process
               "insert-translated-name"
               " *insert-translated-name*"
               "crow" "-t" "en" "--json" "-e" insert-translated-name-crow-engine word)))
         (set-process-sentinel process 'insert-translated-name-process-sentinel))))))


(provide 'insert-translated-name)
