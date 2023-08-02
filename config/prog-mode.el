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

;; config/prog-mode.el - Settings common to all programming modes


(defun custom-prog-mode-hook()
  ;; Default auto wrapping for all programming modes. See specific programming
  ;; mode hooks (e.g. python-mode.el) for overrides.

  ;; These are disabled globally and just here for reference. It seems most
  ;; modern languages are more generous with line length, so this should be set
  ;; in the language specific config (e.g. c-c++-mode.el) whenever the style
  ;; guide of the language restricts line length.
  ;;(auto-fill-mode)
  ;;(setq fill-column 80)

  )

(add-hook 'prog-mode-hook 'custom-prog-mode-hook)

(setq whitespace-style '(trailing face lines space-before-tab indentation
                                  space-after-tab)
      ;; Setting this to nil means that the value for the fill-column is used.
      ;; Not sure whether this works still if you only override fill-column in a
      ;; more specific programming environment (say, python-mode or rust-mode)
      whitespace-line-column nil)

(add-hook 'prog-mode-hook 'whitespace-mode)

