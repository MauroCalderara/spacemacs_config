;; Copyright 2023 Mauro Calderara  -*- lexical-binding: t; -*-
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

;; config/c-c++-mode.el - Settings specific (and common) to C and C++ modes

;; ---------------------------------------------------------------------------
;; Remote clangd via SSH
;;
;; When the buffer-local environment (set via direnv/envrc) contains
;; LSP_REMOTE_HOST, use the remote-clangd wrapper script instead of a
;; local clangd.  The wrapper handles SSH multiplexing and passes
;; --path-mappings to clangd so file URIs are rewritten transparently.
;;
;; To activate for a project, add to its .envrc:
;;   export LSP_REMOTE_HOST=dev@rhel9
;;   export LSP_REMOTE_ROOT=/home/dev/projects.d/myproject
;;   export LSP_LOCAL_ROOT="$(pwd)"
;;   # export LSP_SSH_CONTROL=/path/to/.ssh-control-socket  # optional
;; ---------------------------------------------------------------------------

(with-eval-after-load 'lsp-mode
  (let ((script (expand-file-name "~/.emacs.d/private/mmc/scripts/remote-clangd")))
    (lsp-register-client
     (make-lsp-client
      :new-connection (lsp-stdio-connection
                       (lambda ()
                         (if (getenv "LSP_REMOTE_HOST")
                             (list script)
                           (list (or (executable-find "clangd")
                                     "clangd")))))
      :activation-fn (lsp-activate-on "c" "cpp" "objective-c")
      :major-modes '(c-mode c++-mode objc-mode)
      :priority 1
      :server-id 'clangd-auto))))

(defun custom-c-mode-common-hook()

  ;; Works for both my personal preference for C as well as Google's C++ style
  ;; guide:
  (auto-fill-mode)
  (setq fill-column 80)

  )

(add-hook 'c-mode-common-hook 'custom-c-mode-common-hook)
