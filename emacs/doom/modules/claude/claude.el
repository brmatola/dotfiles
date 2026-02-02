;;; claude.el --- Multi-Claude workspace management -*- lexical-binding: t; -*-

;;; Commentary:
;; Entry point for Claude workflow system.
;; Manages multiple Claude Code sessions in parallel across different repos
;; and worktrees.
;;
;; This module uses a state machine (claude-state) for workspace lifecycle
;; tracking with automatic recovery via reconciliation (claude-reconcile).

;;; Code:

;; Core modules (order matters)
(require 'claude-state)
(require 'claude-vterm)
(require 'claude-worktree)
(require 'claude-reconcile)
(require 'claude-workspace)
(require 'claude-cleanup)
(require 'claude-monitor)
(require 'claude-dashboard)

;;; Customization Group

(defgroup claude-workflow nil
  "Manage multiple Claude Code sessions."
  :group 'tools
  :prefix "claude-")

(defcustom claude-worktree-dir "~/worktrees"
  "Directory for git worktrees."
  :type 'directory
  :group 'claude-workflow)

(defcustom claude-metadata-dir "~/worktrees/metadata"
  "Directory for worktree metadata."
  :type 'directory
  :group 'claude-workflow)

;;; Keybindings

(map! :leader
      (:prefix ("C" . "claude")
       :desc "Create workspace" "c" #'claude-create-workspace
       :desc "Dashboard" "d" #'claude-dashboard
       :desc "Home workspace" "h" #'claude-home-workspace
       :desc "Jump to Claude" "j" #'claude-jump-to-buffer
       :desc "New terminal" "t" #'claude-new-terminal
       :desc "Close workspace" "x" #'claude-close-workspace
       :desc "Toggle monitor" "m" #'claude-monitor-toggle
       :desc "Magit status" "g" #'claude-magit-status
       :desc "Repair workspace" "r" #'claude-repair-workspace))

;;; vterm ESC Key Support

;; Send ESC to vterm with C-g (useful for Claude's "go back" action)
(after! vterm
  (defun claude-vterm-send-escape ()
    "Send ESC key to vterm."
    (interactive)
    (vterm-send-key "<escape>"))
  (map! :map vterm-mode-map
        :i "C-g" #'claude-vterm-send-escape))

;;; Repair Command

;;;###autoload
(defun claude-repair-workspace (&optional workspace-name)
  "Repair WORKSPACE-NAME or current workspace.
Attempts to fix broken workspaces by recreating missing components."
  (interactive
   (list (when current-prefix-arg
           (completing-read "Repair workspace: "
                            (mapcar (lambda (ws)
                                      (claude--workspace-name (car ws) (cdr ws)))
                                    (claude--list-workspaces-by-status "broken"))))))
  (let* ((ws (or workspace-name (claude-workspace-current)))
         (parsed (and ws (claude--parse-workspace-name ws))))
    (if parsed
        (let* ((repo-name (car parsed))
               (branch-name (cdr parsed))
               (metadata (claude-metadata-read repo-name branch-name)))
          (if metadata
              (let ((result (claude--reconcile-safe metadata)))
                (if (eq result 'active)
                    (message "Workspace %s repaired" ws)
                  (message "Could not repair %s (result: %s)" ws result)))
            (user-error "No metadata found for %s" ws)))
      (user-error "Not in a Claude workspace"))))

;;; Startup Reconciliation

(defun claude--startup-hook ()
  "Run at Doom startup to reconcile workspaces and start monitor."
  ;; Run reconciliation
  (claude--startup-reconcile)
  ;; Auto-start monitor if there are active workspaces
  (when (claude--list-active-workspaces)
    (claude-monitor-start)))

;; Add to Doom's after-init hook
(add-hook 'doom-after-init-hook #'claude--startup-hook)

(provide 'claude)
;;; claude.el ends here
