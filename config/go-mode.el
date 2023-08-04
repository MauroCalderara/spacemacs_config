;; Copyright 2023 Mauro Calderara
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

;; config/go-mode.el - Settings specific to go mode

(defun custom-go-mode-hook()

  ;; The go style guide has no maximal line length, so we go with the defaults
  ;; from prog-mode.el
  ;;(setq fill-column -1)
  ;;(setq whitespace-line-column nil)
  ;;(auto-fill-mode -1)

  ;; Make use of the helper tools that are available
  (setq gofmt-command "goimports")
  (setq go-use-golangci-lint t)

  ;; The above are global settings. Project-specifc settings should go into
  ;; a .dir-locals.el file. Example .dir-locals.el content for go:
  ;;
  ;; ((go-mode
  ;;   (setq go-tab-width 4)
  ;; ))

)

(add-hook 'go-mode-hook 'custom-go-mode-hook)
