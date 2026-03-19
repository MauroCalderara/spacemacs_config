;; Copyright 2026 Mauro Calderara  -*- lexical-binding: t; -*-
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

;; config/cyborg.el - Settings related to cyborg

;; Only try load the cyborg code review plugin if it is available (in my
;; standard location, other users might have to change the path)
(defvar cyborg-plugin-directory
  (expand-file-name "~/projects.d/cyborg/main/dist/emacs-plugin")
  "Directory containing cyborg Emacs plugins.")

(when (file-directory-p cyborg-plugin-directory)
  (add-to-list 'load-path cyborg-plugin-directory)
  (require 'cyborg-review)
  (require 'cyborg-implementation-agents))

;; ---------------------------------------------------------------------------
;; Lint rule browser (helm)
;; ---------------------------------------------------------------------------

(defvar cyborg-lint--rule-cache nil
  "Cached alist of (display-string . rule-id) from `cyborg-lint --list'.")

(defun cyborg-lint--refresh-rules ()
  "Parse `cyborg-lint --list' into an alist of (display . id)."
  (let ((lines (split-string
                (shell-command-to-string "cyborg-lint --list")
                "\n" t))
        result)
    ;; Skip header + separator (first two lines)
    (dolist (line (cddr lines))
      (when (string-match "^\\([A-Z0-9]+\\)" line)
        (push (cons (string-trim line) (match-string 1 line)) result)))
    (setq cyborg-lint--rule-cache (nreverse result))))

(defun cyborg-lint--rule-body (id)
  "Extract the body of rule ID from `cyborg-lint --dump' output."
  (let ((output (shell-command-to-string "cyborg-lint --dump 2>/dev/null")))
    (when (string-match (concat "^=== [^\n]*(" (regexp-quote id) ")") output)
      (let ((start (match-beginning 0))
            (end (if (string-match "^=== " output (+ (match-end 0) 1))
                     (match-beginning 0)
                   (length output))))
        (string-trim (substring output start end))))))

(defun cyborg-lint-browse ()
  "Browse cyborg lint rules with helm."
  (interactive)
  (unless cyborg-lint--rule-cache
    (cyborg-lint--refresh-rules))
  (helm :sources
        (helm-build-sync-source "Cyborg Lint Rules"
          :candidates cyborg-lint--rule-cache
          :persistent-action
          (lambda (id)
            (with-current-buffer (get-buffer-create "*cyborg-lint-rule*")
              (let ((inhibit-read-only t))
                (erase-buffer)
                (insert (or (cyborg-lint--rule-body id)
                            (format "No body found for %s" id)))
                (goto-char (point-min))
                (special-mode)))
            (display-buffer "*cyborg-lint-rule*"))
          :action
          (helm-make-actions
           "View rule"
           (lambda (id)
             (with-current-buffer (get-buffer-create "*cyborg-lint-rule*")
               (let ((inhibit-read-only t))
                 (erase-buffer)
                 (insert (or (cyborg-lint--rule-body id)
                             (format "No body found for %s" id)))
                 (goto-char (point-min))
                 (special-mode)))
             (switch-to-buffer "*cyborg-lint-rule*"))
           "Insert rule ID at point"
           (lambda (id) (insert id)))
          :fuzzy-match t)
        :buffer "*helm cyborg-lint*"))

(defun cyborg-lint-refresh ()
  "Force-refresh the lint rule cache."
  (interactive)
  (cyborg-lint--refresh-rules)
  (message "Refreshed %d lint rules" (length cyborg-lint--rule-cache)))

(spacemacs/set-leader-keys "$dRl" #'cyborg-lint-browse)
