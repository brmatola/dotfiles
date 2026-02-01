;;; claude.el --- Multi-Claude workspace management -*- lexical-binding: t; -*-

;;; Commentary:
;; Entry point for Claude workflow system.
;; Manages multiple Claude Code sessions in parallel across different repos
;; and worktrees.

;;; Code:

(require 'claude-worktree)
(require 'claude-workspace)
(require 'claude-monitor)
(require 'claude-dashboard)
(require 'claude-cleanup)

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

;; Keybindings
(map! :leader
      (:prefix ("C" . "claude")
       :desc "Create workspace" "c" #'claude-create-workspace
       :desc "Dashboard" "d" #'claude-dashboard
       :desc "Home workspace" "h" #'claude-home-workspace
       :desc "Jump to Claude" "j" #'claude-jump-to-buffer
       :desc "New terminal" "t" #'claude-new-terminal
       :desc "Close workspace" "x" #'claude-close-workspace
       :desc "Toggle monitor" "m" #'claude-monitor-toggle
       :desc "Magit status" "g" #'claude-magit-status))

(provide 'claude)
;;; claude.el ends here
