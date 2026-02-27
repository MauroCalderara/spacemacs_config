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

;; config/org-mode.el - Settings related to:
;;
;; * Org mode :)
;;

;; Wrap paragraph text at 100 columns
(add-hook 'org-mode-hook
          (lambda ()
            (setq-local fill-column 100)
            (auto-fill-mode 1)))

;; Since org is loaded lazily we set these only after org mode is loaded
(with-eval-after-load 'org

  ;; ToDo keywords
  (setq org-todo-keywords
        '((sequence "TODO" "IN PROGRESS" "BLOCKED" "|" "DONE" "RECURRING")))

  ;; Disable leading stars in orgmode
  (setq org-hide-leading-stars nil)
  (setq org-superstar-leading-bullet ?\s)

  ;; Visually align the paragraphs with the corresponding header (doesn't
  ;; insert actual whitespace)
  (setq org-startup-indented t)

  ;; Insert (empty) child
  (bind-key "M-o"
            (lambda ()
              (interactive)
              (move-end-of-line nil)
              (org-insert-heading)
              (org-metaright)))

  ;; Insert child at position
  (bind-key "M-i"
            (lambda ()
              (interactive)
              (org-insert-heading)
              (org-metaright)))

  )
