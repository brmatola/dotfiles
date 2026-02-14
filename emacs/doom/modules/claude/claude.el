;;; claude.el --- Claude workspace management -*- lexical-binding: t; -*-

;;; Commentary:
;; Entry point for Claude workflow system.
;; Orchestrates multiple Claude Code sessions via grove CLI.
;; Dashboard-centric: SPC = opens mission control, all actions from there.

;;; Code:

;; Core modules
(require 'claude-grove)
(require 'claude-monitor)
(require 'claude-dashboard)

;;; Customization Group

(defgroup claude nil
  "Manage multiple Claude Code sessions."
  :group 'tools
  :prefix "claude-")

;;; Main Entry Point

;;;###autoload
(defun claude-goto-main ()
  "Switch to the main Claude workspace and focus the dashboard."
  (interactive)
  (unless (+workspace-exists-p "claude:main")
    (+workspace/new "claude:main"))
  (+workspace/switch-to "claude:main")
  (let ((buf (get-buffer-create "*claude:dashboard*")))
    (unless (eq (buffer-local-value 'major-mode buf) 'claude-dashboard-mode)
      (with-current-buffer buf
        (claude-dashboard-mode)))
    (switch-to-buffer buf)
    (claude-dashboard-refresh)))

;; Forward declarations for Doom workspace functions
(declare-function +workspace-exists-p "~/.config/emacs/modules/ui/workspaces/autoload/workspaces")
(declare-function +workspace/new "~/.config/emacs/modules/ui/workspaces/autoload/workspaces")
(declare-function +workspace/switch-to "~/.config/emacs/modules/ui/workspaces/autoload/workspaces")

;;; Keybindings

(map! :leader
      :desc "Claude dashboard" "=" #'claude-goto-main)

;;; vterm ESC Key Support

(after! vterm
  (defun claude-vterm-send-escape ()
    "Send ESC key to vterm."
    (interactive)
    (vterm-send-key "<escape>"))
  (map! :map vterm-mode-map
        :i "C-c C-c" #'claude-vterm-send-escape))

;;; Startup

(defun claude--startup-hook ()
  "Run at Doom startup to start monitor if Claude buffers exist."
  (when (seq-some (lambda (buf)
                    (string-match-p "^\\*claude:[^:]+:[^*]+\\*$"
                                    (buffer-name buf)))
                  (buffer-list))
    (claude-monitor-start)))

(add-hook 'doom-after-init-hook #'claude--startup-hook)

(provide 'claude)
;;; claude.el ends here
