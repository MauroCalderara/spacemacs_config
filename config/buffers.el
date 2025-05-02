;; Copyright 2021 Mauro Calderara
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

;; config/buffers.el - Settings related to managing buffers

;; Shortcut to allow renaming of buffers
(spacemacs/set-leader-keys "b$" 'rename-buffer "Rename buffer")

;; The spacemacs defaults don't feel intuitive to me:
(spacemacs/set-leader-keys "bq"
  (lambda () (interactive)
    (kill-buffer (current-buffer)))
  "Kill buffer")

(spacemacs/set-leader-keys "bQ"
  (lambda () (interactive)
    (kill-buffer-and-window))
  "Kill buffer and close window")

(spacemacs/set-leader-keys "bx"
  (lambda () (interactive)
    (save-buffer)
    (kill-buffer (current-buffer)))
  "Save and kill buffer")

(spacemacs/set-leader-keys "bX"
  (lambda () (interactive)
    (save-buffer)
    (kill-buffer-and-window))
  "Save and kill buffer and close window")

(setq scroll-conservatively 101)
(setq scroll-margin 5)
