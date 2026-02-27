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

;; config/claude-code.el - Settings for Claude Code IDE integration

;; Register Emacs MCP tools (xref, imenu, tree-sitter, project-info) so
;; they are available to Claude Code sessions started from Emacs.
(when (fboundp 'claude-code-ide-emacs-tools-setup)
  (claude-code-ide-emacs-tools-setup))

;; Send selection to the current Claude Code session.
(global-set-key (kbd "M-<return>") #'claude-code-ide-insert-at-mentioned)

;; ---------------------------------------------------------------------------
;; Pin the Claude Code window to the right side.
;;
;; The package sets side-window parameters only at creation time, so other
;; operations (ediff, help, completions) can displace it.  A persistent
;; display-buffer-alist entry ensures the buffer always lands in a dedicated
;; right side window with a minimum width.
;; ---------------------------------------------------------------------------
(add-to-list 'display-buffer-alist
             `("\\*claude-code\\[.*\\]\\*"
               (display-buffer-in-side-window)
               (side . right)
               (slot . 0)
               (window-width . 90)
               (dedicated . t)
               (window-parameters . ((no-delete-other-windows . t)
                                     (no-other-window . nil)))))

;; ---------------------------------------------------------------------------
;; Non-interrupting mode
;;
;; When claude-code-ide-interrupting-mode is nil, openDiff is unregistered from
;; the MCP tool list so Claude falls back to showing diffs inline in its
;; terminal.  Desktop notifications are triggered by a Claude Code
;; PermissionRequest hook that calls back into Emacs via emacsclient --eval.
;; This means notifications fire only when actual approval is needed (not for
;; pre-approved edits) and degrade gracefully when Claude Code is not running
;; inside Emacs.
;;
;; Hook configuration (in ~/.claude/settings.json):
;;
;;   "hooks": {
;;     "PermissionRequest": [{
;;       "matcher": "",
;;       "hooks": [{
;;         "type": "command",
;;         "command": "emacsclient --eval '(claude-code-ide--hook-maybe-notify)' >/dev/null 2>&1 || true"
;;       }]
;;     }]
;;   }
;; ---------------------------------------------------------------------------

(defvar claude-code-ide-interrupting-mode t
  "When non-nil, Claude Code opens ediff for proposed changes (default).
When nil, diffs are shown inline in the Claude terminal and a desktop
notification is sent via a PermissionRequest hook if the Claude window
is not focused.")

(defvar claude-code-ide--last-notify-time 0
  "Timestamp of last desktop notification, used for debouncing.")

(defvar claude-code-ide--notify-debounce-seconds 5
  "Minimum seconds between desktop notifications.")

(defun claude-code-ide--notify (title message)
  "Send a macOS desktop notification with TITLE and MESSAGE."
  (start-process "claude-notify" nil
                 "osascript" "-e"
                 (format "display notification %S with title %S"
                         message title)))

(defun claude-code-ide--claude-buffer-focused-p ()
  "Return non-nil if a Claude Code session buffer is currently focused."
  (claude-code-ide--session-buffer-p (window-buffer (selected-window))))

(defun claude-code-ide--hook-maybe-notify ()
  "Entry point for the Claude Code PermissionRequest hook.
Called via `emacsclient --eval'.  Sends a desktop notification when
`claude-code-ide-interrupting-mode' is nil and the Claude IDE buffer
is not the selected window.  Returns nil so emacsclient produces no
meaningful stdout."
  (when (and (not claude-code-ide-interrupting-mode)
             (not (claude-code-ide--claude-buffer-focused-p))
             (> (- (float-time) claude-code-ide--last-notify-time)
                claude-code-ide--notify-debounce-seconds))
    (setq claude-code-ide--last-notify-time (float-time))
    (let ((project-name
           (cl-loop for session being the hash-values of claude-code-ide-mcp--sessions
                    thereis (file-name-nondirectory
                             (directory-file-name
                              (claude-code-ide-mcp-session-project-dir session))))))
      (claude-code-ide--notify
       (format "CC: %s" (or project-name "Claude Code"))
       "Waiting for approval")))
  nil)

(defun claude-code-ide--send-tools-list-changed-all-sessions ()
  "Send notifications/tools/list_changed to every active MCP session."
  (maphash
   (lambda (_dir session)
     (when-let ((client (claude-code-ide-mcp-session-client session)))
       (let ((msg `((jsonrpc . "2.0")
                    (method . "notifications/tools/list_changed")
                    (params . ,(make-hash-table :test 'equal)))))
         (condition-case nil
             (websocket-send-text client (json-encode msg))
           (error nil)))))
   claude-code-ide-mcp--sessions))

(defun claude-code-ide-toggle-interrupting-mode ()
  "Toggle between interrupting (ediff) and non-interrupting (inline) modes."
  (interactive)
  (setq claude-code-ide-interrupting-mode
        (not claude-code-ide-interrupting-mode))
  ;; Sync with the upstream variable that controls openDiff registration
  (setq claude-code-ide-use-ide-diff claude-code-ide-interrupting-mode)
  ;; Tell all sessions to re-query the tool list
  (claude-code-ide--send-tools-list-changed-all-sessions)
  (message "Claude Code interrupting mode %s"
           (if claude-code-ide-interrupting-mode
               "ON (ediff)" "OFF (inline + notify)")))

(spacemacs/set-leader-keys "$dm" 'claude-code-ide-toggle-interrupting-mode)
