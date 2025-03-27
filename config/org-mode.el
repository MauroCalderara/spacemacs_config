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

;; config/org-mode.el - Settings related to:
;;
;; * Org mode :)
;;

;; Since org is loaded lazily we set these only after org mode is loaded
(with-eval-after-load 'org

  ;; ToDo keywords
  (setq org-todo-keywords
        '((sequence "TODO" "IN PROGRESS" "BLOCKED" "|" "DONE")))

  ;; Disable leading stars in orgmode
  (setq org-hide-leading-stars nil)
  (setq org-superstar-leading-bullet ?\s)

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

  ;; Admittedly, source blocks are easier in markdown, but this might be too
  ;; slow + it doesn't work
  ;;(defun custom/org-electric-src-block ()
  ;;  "Replace ``` with org src block and position cursor"
  ;;  (interactive)
  ;;  (when (looking-back "`" 3)
  ;;    (delete-backward-char 3)
  ;;    (insert "#+BEGIN_SRC\n\n#+END_SRC")
  ;;    (forward-line -1)))

  ;;(add-hook 'org-mode-hook
  ;;          (lambda ()
  ;;            (local-set-key (kbd "`") #'custom/org-electric-src-block)))

  )
