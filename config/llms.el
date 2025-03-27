;; Copyright 2025 Mauro Calderara
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are met:
;;
;; 1. Redistributions of source code must retain the above copyright notice,
;;    this list of conditions and the following disclaimer.
;;
;; 2. Redistributions in binary form must reproduce the above copyright notice
;;    this list of conditions and the following disclaimer in the documentation
;;    and/or other materials provided with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;; ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
;; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;; POSSIBILITY OF SUCH DAMAGE.

;; config/llms.el - Settings specific to using gptel/llms

(defun llm-read-api-key-from-file (file-path)
  "Read API key from file-path, trimming whitespace."
  (with-temp-buffer
    (insert-file-contents file-path)
    (string-trim (buffer-string))))

;; Set the default model
(setq
 gptel-model 'claude-3-5-sonnet-20241022
 gptel-backend (
                gptel-make-anthropic "Claude 3.5 Sonnet"
                :stream t
                :key (llm-read-api-key-from-file "~/.llms/api_keys/anthropic")
                ))

;; Use org mode
(setq gptel-default-mode 'org-mode)

(setq gptel-prompt-prefix-alist
      '((org-mode . "* 🤪: ")
        (markdown-mode . "# 🤪: ")
        (text-mode . "🤪: "))
      gptel-prompt-prefix "* 🤪: ")  ; Fallback

(setq gptel-response-prefix-alist
      '((org-mode . "** 🧠\n")
        (markdown-mode . "## 🧠\n")
        (text-mode . "🧠\n"))
      gptel-response-prefix "** 🧠\n")  ; Fallback

(setq gptel-system
      (concat
       "You are a helpful AI assistant embedded in an editor. "

       "Please respond using in org mode syntax. "

       "When writing code, write idiomatic and concise code that is easily readable "
       "by a programmer experienced in the respective language. "

       "Avoid lengthy explanations and context in your response, but feel free to add "
       "comments on things that are not otherwise clear. "

       "If you rewrite or change a function, write a comment containing 'MARKER' close to "
       "it, drawing attention to that change. Also, a short itemized list of changes can be "
       "very helpful. "

       "Please don't take my being terse in my promtps as rudeness, I do sincerely "
       "appreciate your efforts and help."
       ))

;; When using gptel-org-set-properties we should use the local settings, so we
;; monkeypatch (advise) the function
(defun llm-gptel-org-set-properties (&rest _)
  "Set gptel properties using current settings"
  (interactive)
  (org-with-point-at 1
    (org-insert-property-drawer)
    (org-set-property "GPTEL_MODEL" (symbol-name gptel-model))
    (org-set-property "GPTEL_BACKEND" (gptel-backend-name gptel-backend))
    (org-set-property "GPTEL_SYSTEM" gptel-system)))

(advice-add 'gptel-org-set-properties :override #'llm-gptel-org-set-properties)

(defun llm-create-session (buffer-name)
  "Create a new llm/gptel session"
  (with-current-buffer (get-buffer-create buffer-name)
    (org-mode)
    (gptel-mode)
    (insert "# -*- eval: (gptel-mode 1) -*-\n\n")
    (llm-gptel-org-set-properties)
    (goto-char (point-max))
    (insert (concat "\n" gptel-prompt-prefix))
    (current-buffer)))

(defun llm-send ()
  "Move to end of buffer and send gptel message."
  (interactive)
  (goto-char (point-max))
  (gptel-send))

(define-key gptel-mode-map (kbd "s-<return>") #'llm-send)

(add-hook 'gptel-mode-hook (lambda()
                             ;; Don't break prompt lines automatically
                             (visual-line-mode 1)
                             (setq-local markdown-header-scaling t)
                             (use-local-map gptel-mode-map)))

(add-hook 'gptel-post-stream-hook 'gptel-auto-scroll)

(defun llm-redisplay-images (&rest _)
  "Wrapper for org-redisplay-inline-images that handles hook arguments"
  (org-redisplay-inline-images))
(add-hook 'gptel-post-response-functions #'llm-redisplay-images)
;;(add-hook 'gptel-post-response-functions #'org-redisplay-inline-images) ;; triger syntax highlighting
;;(add-hook 'gptel-post-response-functions #'gptel-end-of-response)

(defun llm-default-session ()
  "Switch to *llm* buffer if it exists, or create a new session."
  (interactive)
  (if (get-buffer "*llm*")
      (switch-to-buffer "*llm*")
    (switch-to-buffer (llm-create-session "*llm*"))))

(spacemacs/set-leader-keys "," 'llm-default-session)

(defun llm-named-session ()
  "Switch to named buffer buffer if it exists, or create it"
  (interactive)
  (let ((llm-session-name (read-string "LLM session name: " nil)))
    (if (get-buffer llm-session-name)
        (switch-to-buffer llm-session-name)
      (switch-to-buffer (llm-create-session llm-session-name)))))

(spacemacs/set-leader-keys "<" 'llm-named-session)

;; Don't change the background of buffers that are in a context
(defun llm-fix-context-background (&rest _)
  "Clear background highlighting from context buffers"
  (when (facep 'gptel-context-highlight-face)
    (set-face-background 'gptel-context-highlight-face nil nil)))
(advice-add 'gptel-context-add :after #'llm-fix-context-background)

(defun llm-save-context ()
  "Save current context as org properties."
  (when (bound-and-true-p gptel-mode)
    (let ((context-items
           (seq-filter
            (lambda (path)
              (and path (file-exists-p path)))
            (mapcar (lambda (item)
                      (if (bufferp (car item))
                          (buffer-file-name (car item))
                        (car item)))
                    gptel-context--alist))))
      (when context-items
        (save-excursion
          (goto-char (point-min))
          (org-set-property
           "GPTEL_CONTEXT"
           (string-join context-items "\n")))))))

(defun llm-save ()
  "Ensure file opens with gptel-mode enabled."
  (save-excursion
    (let ((enable-local-variables t))
      (goto-char (point-min))
      (if (looking-at ".*-\\*-")
          (modify-file-local-variable-prop-line 'eval nil 'delete))
      (add-file-local-variable-prop-line
       'eval '(gptel-mode 1)))))

(add-hook 'gptel-save-state-hook #'llm-save-context)
(add-hook 'gptel-save-state-hook #'llm-save)

(defun llm-load-context ()
  "Load context from org properties."
  (when (bound-and-true-p gptel-mode)
    (save-excursion
      (goto-char (point-min))
      (when-let ((context-str (org-entry-get (point) "GPTEL_CONTEXT")))
        (let ((missing-files '()))
          (dolist (path (split-string context-str "\n"))
            (if (file-exists-p path)
                (gptel-context-add-file path)
              (push path missing-files)))
          (when missing-files
            (message "Some context files were not found: %s"
                     (string-join missing-files ", "))))))))

(add-hook 'gptel-mode-hook #'llm-load-context)

(defvar-local llm-associated-session nil
  "Buffer-local variable to store associated session name.")

(defun llm-set-associated-session ()
  "Set associated session for buffer."
  (interactive)
  (let ((sessions (seq-filter
                   (lambda (buf)
                     (with-current-buffer buf (bound-and-true-p gptel-mode)))
                   (buffer-list))))
    (let ((session-buffer
           (if sessions
               (get-buffer
                (helm-comp-read "Choose LLM session: "
                                (mapcar #'buffer-name sessions)
                                :allow-nest t))
             (progn
               (llm-default-session)
               (current-buffer)))))
      (setq-local llm-associated-session (buffer-name session-buffer))
      session-buffer)))

;; TODO This doesn't yet work:
(defun llm-update-modeline (&rest _)
  "Update modeline to show associated LLM session"
  (setq mode-line-format
        (append mode-line-format
                '((:eval (when llm-associated-session
                           (format " 🧠=%s "
                                   (buffer-name llm-associated-session))))))))

(add-hook 'find-file-hook #'llm-update-modeline)
(add-hook 'after-change-major-mode-hook #'llm-update-modeline)
(add-variable-watcher 'llm-associated-session
                      (lambda (&rest _)
                        (llm-update-modeline)
                        (force-mode-line-update)))

(spacemacs/set-leader-keys "$gb" 'llm-set-associated-session)

(defun llm-major-mode-to-src-lang ()
  "Convert major mode to src block language name."
  (let ((mode-name (symbol-name major-mode)))
    (replace-regexp-in-string
     "-mode$" ""
     (replace-regexp-in-string "\\=.*-" "" mode-name))))

(defun llm-send-selection (begin end)
  "Send selected region to the associated LLM session."
  (interactive "r")
  (let* ((code-buffer (current-buffer))
         (code-text (buffer-substring-no-properties begin end))
         (lang (llm-major-mode-to-src-lang))
         (session-buffer
          (or (and llm-associated-session
                   (get-buffer llm-associated-session))
              (llm-set-associated-session))))

    ;; Switch to session and prepare prompt
    (pop-to-buffer session-buffer '((display-buffer-reuse-window)))
    (goto-char (point-max))
    (insert "\n\n* Can you help me with this code:\n\n")
    (insert (format "#+BEGIN_SRC %s\n%s\n#+END_SRC" lang code-text))
    (gptel-context-add)))

(global-set-key (kbd "M-s-<return>") #'llm-send-selection)

;; For now gptel doesn't seem to maintain a context per session/gptel buffer,
;; otherwise these here might be useful
;;(defun llm-get-gptel-buffers ()
;;  "Get list of active gptel buffers."
;;  (seq-filter
;;   (lambda (buf)
;;     (with-current-buffer buf (bound-and-true-p gptel-mode)))
;;   (buffer-list)))
;;
;;(defun llm-pick-target-session ()
;;  "Prompt user to select a gptel session buffer."
;;  (let ((gptel-buffers (llm-get-gptel-buffers)))
;;    (get-buffer
;;     (helm-comp-read "Add context to session: "
;;                     (mapcar #'buffer-name gptel-buffers)))))
;;
;;(defun llm-ctx-push (target-buffer buffers files)
;;  "Add buffers and files as context to target-buffer."
;;  (with-current-buffer target-buffer
;;    (dolist (buf buffers)
;;      (gptel-context-add buf))
;;    (dolist (file files)
;;      (gptel-context-add-file file))))
;;
;;(defun llm-ctx-push-buf ()
;;  "Add current buffer as context to an llm session"
;;  (when-let ((target-buffer (llm-pick-target-session)))
;;    (llm-ctx-push target-buffer (list (current-buffer)) nil)))
;;
;;(spacemacs/set-leader-keys "$ga" 'llm-ctx-push-buf)

(defun llm-ctx-push-proj ()
  "Add selected project files/directories to llm context."
  (interactive)
  (when-let* ((project (project-current))
              (root (project-root project))
              (default-directory root)
              (files (project-files project))
              (dirs (delete-dups
                     (mapcar #'file-name-directory files)))
              (items (mapcar (lambda (f) (file-relative-name f root))
                             (append files dirs)))
              (selected (helm-comp-read "Add to context: "
                                        items
                                        :marked-candidates t)))
    (dolist (item selected)
      (let ((full-path (expand-file-name item root)))
        (if (file-directory-p full-path)
            (progn
              (let ((contained-files
                     (seq-filter
                      (lambda (f) (string-prefix-p full-path f))
                      files)))
                (dolist (file contained-files)
                  (gptel-context-add-file file))))
          (gptel-context-add-file full-path))))))

(spacemacs/set-leader-keys "$gP" 'llm-ctx-push-proj)
