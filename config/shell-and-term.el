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

(defun custom/next-shell-name ()
  "Return the lowest available shell name (s0, s1, ...).
Scans existing buffers and picks the first gap."
  (let ((i 0))
    (while (get-buffer (format "s%d" i))
      (setq i (1+ i)))
    (format "s%d" i)))

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
          (rename-buffer "s0" t))))
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
  (key-chord-define vterm-mode-map "jk" #'vterm-send-escape)

  ;; Pin scroll position in evil normal state so terminal output doesn't
  ;; yank the viewport away, without resorting to `vterm-copy-mode' (which
  ;; makes the buffer read-only and blocks paste).
  ;;
  ;; We advise `vterm--delayed-redraw' rather than `vterm--filter': the
  ;; filter only feeds bytes into libvterm's state machine via
  ;; `vterm--write-input', it does not touch the Emacs buffer.  The actual
  ;; text rewrite and point movement happen in `vterm--delayed-redraw',
  ;; which is scheduled from the C module via `vterm--invalidate' (usually
  ;; through a `vterm-timer-delay' timer; see vterm.el).  Also, in a
  ;; process filter `(current-buffer)' is whatever happened to be current
  ;; when the output arrived, not the vterm buffer, so advising the filter
  ;; and looking up the window from `current-buffer' was a no-op.
  (defun custom/vterm-pin-scroll-a (orig-fn buffer &rest args)
    "Around advice for `vterm--delayed-redraw': freeze viewport in normal state.
Records window-start, window-point, and buffer point as markers before the
redraw so Emacs redisplay cannot chase new output and yank us to the prompt.
Markers track any position shifts caused by scrollback trimming during the
redraw itself."
    (if (not (and (buffer-live-p buffer)
                  (with-current-buffer buffer
                    (and (bound-and-true-p evil-mode)
                         (evil-normal-state-p)))))
        (apply orig-fn buffer args)
      (let ((saved (mapcar
                    (lambda (w)
                      (list w
                            (copy-marker (window-start w))
                            (copy-marker (window-point w) t)))
                    (get-buffer-window-list buffer nil t))))
        (unwind-protect
            ;; `save-excursion' pins buffer point across the redraw;
            ;; without it, Emacs redisplay would move window-start to
            ;; follow point (which vterm yanks to the cursor).
            (with-current-buffer buffer
              (save-excursion
                (apply orig-fn buffer args)))
          (dolist (entry saved)
            (pcase-let ((`(,w ,ws ,wp) entry))
              (when (window-live-p w)
                (set-window-start w (marker-position ws))
                (set-window-point w (marker-position wp)))
              (set-marker ws nil)
              (set-marker wp nil)))))))

  (advice-add 'vterm--delayed-redraw :around #'custom/vterm-pin-scroll-a))
