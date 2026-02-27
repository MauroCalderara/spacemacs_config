;; Copyright 2025 Mauro Calderara  -*- lexical-binding: t; -*-
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

;; config/shell-and-term.el - Settings related to:
;;
;; * Shell
;; * Terminals
;;

(setq vterm-max-scrollback 10000)

(defvar custom/shell-counter 0
  "Counter for auto-naming shell buffers s0, s1, s2, ...")

(defun custom/next-shell-name ()
  "Return the next available shell name (s0, s1, ...)."
  (let ((name (format "s%d" custom/shell-counter)))
    (setq custom/shell-counter (1+ custom/shell-counter))
    name))

(defun custom/new-shell ()
  "Create a new vterm with an auto-generated name (s0, s1, ...)."
  (interactive)
  (let* ((name (custom/next-shell-name))
         (buf (vterm name)))
    (with-current-buffer buf
      (rename-buffer name t))))

(defun custom/default-shell ()
  "Switch to *s0* if it exists, otherwise create it.  Opens in a
horizontal split of the current window and moves focus there."
  (interactive)
  (let ((buf (get-buffer "s0")))
    (unless buf
      ;; Create the vterm buffer without displaying it
      (save-window-excursion
        (setq buf (vterm "s0"))
        (with-current-buffer buf
          (rename-buffer "s0" t)))
      ;; Bump counter past 0 so new-shell starts at s1+
      (when (< custom/shell-counter 1)
        (setq custom/shell-counter 1)))
    (select-window (split-window-below))
    (switch-to-buffer buf)))

(defun custom/create-shell-and-rename ()
  "Create a new term instance with a given name."
  (interactive)
  (let ((shell-name (read-string "Shell name: " nil)))
    (vterm shell-name)
    (rename-buffer shell-name t)))

;; SPC .   - always create a new numbered shell in the current window
(spacemacs/set-leader-keys "." #'custom/new-shell)

;; SPC RET - go to s0 (create if needed) in a horizontal split
(spacemacs/set-leader-keys "RET" #'custom/default-shell)

;; SPC >   - create a shell with a custom name (existing behavior)
(spacemacs/set-leader-keys ">" #'custom/create-shell-and-rename)

(with-eval-after-load 'vterm
  ;; Send ctrl-d through to the shell
  (evil-define-key '(normal insert) vterm-mode-map (kbd "C-d") 'vterm-send-C-d)
  ;; For "inner escape" we use key-chord so it behaves the same as "fd"
  (key-chord-define vterm-mode-map "jk" #'vterm-send-escape))
