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

;; config/shell-and-eterm.el - Settings related to:
;;
;; * Shell
;; * Terminals
;;

;; Custom function to open shell in vsplit and bind it to SPC '.
;; NOTE: This assumes
;;
;;   dotspacemacs-configuration-layers '(
;;     ...
;;     (shell :variables
;;       shell-default-position 'right
;;       shell-default-shell 'vterm)
;;     ...
;;   )
;;
(defun custom/create-shell-split()
  (spacemacs/default-pop-shell)
  (enlarge-window-horizontally (- 80 (window-width))))

(defun custom/create-shell-split-and-rename()
  "Create pop a new shell with a given name"
  (interactive)
  (let
    ((shell-name (read-string "Shell name: " nil)))
    (custom/create-shell-split)
    (rename-buffer shell-name)))

(spacemacs/set-leader-keys "'"
  (lambda () (interactive) (custom/create-shell-split))
  "open terminal")

;; We steal that one from spacemacs
(spacemacs/set-leader-keys "\""
  (lambda () (interactive) (custom/create-shell-split-and-rename))
  "open terminal + rename")

(defun custom/create-shell-and-rename ()
  "Create a new term instance with a given name"
  (interactive)
  (let
    ((shell-name (read-string "Shell name: " nil)))
    (vterm)
    (rename-buffer shell-name)))

;; Create a new terminal in the current window
(spacemacs/set-leader-keys "."
  (lambda () (interactive) (vterm)))

(spacemacs/set-leader-keys ">"
  (lambda () (interactive) (custom/create-shell-and-rename)))

;; Send ctrl-d
(evil-define-key 'normal vterm-mode-map "C-d" 'vterm-send-C-d)
(evil-define-key 'insert vterm-mode-map "C-d" 'vterm-send-C-d)
