# File Structure

## Doom Config Organization

```
~/.config/doom/                    # Symlinked from dotfiles/emacs/doom/
├── init.el                        # Doom module declarations (no changes needed)
├── packages.el                    # Extra packages (none needed for this)
├── config.el                      # Loads our modules
└── modules/
    ├── defaults.el                # UI preferences, theme, fonts
    ├── keybindings.el             # Non-Claude custom bindings
    └── claude/
        ├── claude.el              # Entry point
        ├── claude-worktree.el     # Git worktree operations
        ├── claude-workspace.el    # Doom workspace management
        ├── claude-monitor.el      # Attention detection
        ├── claude-dashboard.el    # Dashboard UI
        └── claude-cleanup.el      # Cleanup workflow
```

## Module Responsibilities

### claude.el (~80 lines)
Entry point and configuration.

```elisp
;;; claude.el --- Multi-Claude workspace management -*- lexical-binding: t; -*-

;;; Commentary:
;; Entry point for Claude workflow system.
;; Loads submodules and defines configuration.

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
  :type 'directory)

(defcustom claude-metadata-dir "~/worktrees/metadata"
  "Directory for worktree metadata."
  :type 'directory)

;; Keybindings
(map! :leader
      (:prefix ("C" . "claude")
       :desc "Create workspace" "c" #'claude-create-workspace
       :desc "Dashboard" "d" #'claude-dashboard
       :desc "Jump to Claude" "j" #'claude-jump-to-buffer
       :desc "Close workspace" "x" #'claude-close-workspace
       :desc "Toggle monitor" "m" #'claude-monitor-toggle
       :desc "Magit status" "g" #'claude-magit-status))

(provide 'claude)
;;; claude.el ends here
```

### claude-worktree.el (~120 lines)
Git worktree and metadata operations.

```elisp
;;; claude-worktree.el --- Git worktree management -*- lexical-binding: t; -*-

;;; Code:

(require 'json)

;; Worktree operations
(defun claude-worktree-create (repo-path branch-name parent-branch)
  "Create worktree for BRANCH-NAME from REPO-PATH.")

(defun claude-worktree-remove (repo-name branch-name)
  "Remove worktree for BRANCH-NAME in REPO-NAME.")

(defun claude-worktree-path (repo-name branch-name)
  "Return path to worktree.")

(defun claude-worktree-list ()
  "List all worktrees across all repos.")

;; Metadata operations
(defun claude-metadata-write (repo-name branch-name data)
  "Write metadata for worktree.")

(defun claude-metadata-read (repo-name branch-name)
  "Read metadata for worktree.")

(defun claude-metadata-delete (repo-name branch-name)
  "Delete metadata file.")

;; Git operations
(defun claude-git-commits-ahead (worktree-path parent-branch)
  "Count commits ahead of PARENT-BRANCH.")

(defun claude-git-merge-branch (repo-path target-branch source-branch)
  "Merge SOURCE-BRANCH into TARGET-BRANCH.")

(defun claude-git-delete-branch (repo-path branch-name)
  "Delete BRANCH-NAME from repo.")

(defun claude-repo-name (repo-path)
  "Extract repo name from path.")

(provide 'claude-worktree)
;;; claude-worktree.el ends here
```

### claude-workspace.el (~100 lines)
Doom workspace and vterm session management.

```elisp
;;; claude-workspace.el --- Workspace management -*- lexical-binding: t; -*-

;;; Code:

(require 'vterm)

(defun claude-workspace-create (repo-name branch-name worktree-path)
  "Create Doom workspace and start Claude session.")

(defun claude-workspace-switch (workspace-name)
  "Switch to workspace.")

(defun claude-workspace-delete (workspace-name)
  "Delete workspace and kill associated buffers.")

(defun claude-workspace-list ()
  "List all claude workspaces.")

(defun claude-workspace-current ()
  "Get current workspace if it's a Claude workspace.")

(defun claude-buffer-name (repo-name branch-name)
  "Generate Claude buffer name.")

(defun claude-jump-to-buffer ()
  "Jump to Claude buffer in current workspace.")

(defun claude-create-workspace ()
  "Interactive command to create new Claude workspace.")

(defun claude-magit-status ()
  "Open magit in current worktree.")

(provide 'claude-workspace)
;;; claude-workspace.el ends here
```

### claude-monitor.el (~100 lines)
Attention detection and modeline.

```elisp
;;; claude-monitor.el --- Attention monitoring -*- lexical-binding: t; -*-

;;; Code:

(defcustom claude-monitor-interval 2
  "Seconds between attention checks."
  :type 'integer)

(defcustom claude-attention-idle-threshold 3
  "Seconds of idle before checking patterns."
  :type 'integer)

(defcustom claude-attention-patterns
  '("Allow .* \\[y/n\\]" "\\[Y/n\\]" "(y/n)"
    "Do you want to" "Proceed\\?" "Continue\\?")
  "Patterns indicating Claude needs attention."
  :type '(repeat string))

(defvar claude-monitor-timer nil)

;; Buffer-local state
(defvar-local claude--last-output-time nil)
(defvar-local claude--last-content-hash nil)
(defvar-local claude--needs-attention nil)

(defun claude-monitor-start ()
  "Start attention monitoring.")

(defun claude-monitor-stop ()
  "Stop attention monitoring.")

(defun claude-monitor-toggle ()
  "Toggle monitoring on/off.")

(defun claude--check-all-buffers ()
  "Check all Claude buffers for attention.")

(defun claude--check-buffer-attention (buffer)
  "Check if BUFFER needs attention.")

(defun claude--any-needs-attention-p ()
  "Return t if any Claude buffer needs attention.")

(defun claude-modeline-segment ()
  "Modeline segment showing Claude status.")

(provide 'claude-monitor)
;;; claude-monitor.el ends here
```

### claude-dashboard.el (~120 lines)
Dashboard buffer and UI.

```elisp
;;; claude-dashboard.el --- Dashboard UI -*- lexical-binding: t; -*-

;;; Code:

(defvar claude-dashboard-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "j" #'claude-dashboard-next)
    (define-key map "k" #'claude-dashboard-prev)
    (define-key map (kbd "RET") #'claude-dashboard-select)
    (define-key map "c" #'claude-create-workspace)
    (define-key map "x" #'claude-dashboard-close)
    (define-key map "g" #'claude-dashboard-refresh)
    (define-key map "/" #'claude-dashboard-filter)
    (define-key map "q" #'quit-window)
    map))

(define-derived-mode claude-dashboard-mode special-mode "Claude"
  "Major mode for Claude workspace dashboard.")

(defun claude-dashboard ()
  "Open Claude workspace dashboard.")

(defun claude-dashboard-refresh ()
  "Refresh dashboard contents.")

(defun claude-dashboard--render ()
  "Render dashboard buffer contents.")

(defun claude-dashboard-next ()
  "Move to next entry.")

(defun claude-dashboard-prev ()
  "Move to previous entry.")

(defun claude-dashboard-select ()
  "Jump to workspace under cursor.")

(defun claude-dashboard-close ()
  "Close workspace under cursor.")

(defun claude-dashboard-filter ()
  "Filter by repo.")

(provide 'claude-dashboard)
;;; claude-dashboard.el ends here
```

### claude-cleanup.el (~100 lines)
Cleanup workflow and status display.

```elisp
;;; claude-cleanup.el --- Cleanup workflow -*- lexical-binding: t; -*-

;;; Code:

(defun claude-close-workspace (&optional workspace-name)
  "Close WORKSPACE-NAME with merge-aware cleanup.")

(defun claude-cleanup--show-status (workspace-info)
  "Show status buffer for WORKSPACE-INFO.")

(defun claude-cleanup--view-diff (workspace-info)
  "Show diff in magit.")

(defun claude-cleanup--merge-and-cleanup (workspace-info)
  "Merge branch and clean up workspace.")

(defun claude-cleanup--delete-without-merge (workspace-info)
  "Delete workspace without merging.")

(defun claude-cleanup--do-cleanup (workspace-info)
  "Perform actual cleanup steps.")

(provide 'claude-cleanup)
;;; claude-cleanup.el ends here
```

## Loading in config.el

```elisp
;;; config.el -*- lexical-binding: t; -*-

;; Load custom modules
(add-to-list 'load-path (expand-file-name "modules" doom-user-dir))
(add-to-list 'load-path (expand-file-name "modules/claude" doom-user-dir))

(load! "modules/defaults")
(load! "modules/keybindings")
(load! "modules/claude/claude")
```
