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

;; config/python-mode.el - Settings specific to python-mode


;; This assumes that python and lsp are in dotspacemacs-configuration-layers
;; and that python-language-server[all] is installed (via pip or otherwise).
;; Also see https://develop.spacemacs.org/layers/+lang/python/README.html

;; A lot of important settings for python are actually set as variables on
;; they python layer (again, see dotspacemacs-configuration-layers).

(defun custom-python-mode-hook()

  (setq python-fill-column 100)
  (setq python-test-runner 'pytest)
  (setq python-formatter 'black)
  (setq python-sort-imports-on-save t)

  ;; $PROJECT_ROOT/.venv is loaded by default through pyvenv. Use one of these
  ;; in a .direnv.el file if you want to use pipenv or poetry
  ;;(setq python-pipenv-activate t)
  ;;(setq python-poetry-activate t)

)

(add-hook 'python-mode-hook 'custom-python-mode-hook)

