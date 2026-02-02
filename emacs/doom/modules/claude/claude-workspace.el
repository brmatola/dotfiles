;;; claude-workspace.el --- Workspace management -*- lexical-binding: t; -*-

;;; Commentary:
;; Doom workspace and vterm session management for Claude.
;; Now uses claude-state for state management and naming utilities.

;;; Code:

(require 'vterm)
(require 'projectile nil t)
(require 'claude-state)
(require 'claude-vterm)
(require 'claude-worktree)

;; Forward declarations
(declare-function +workspace/new "~/.config/emacs/modules/ui/workspaces/autoload/workspaces")
(declare-function +workspace/switch-to "~/.config/emacs/modules/ui/workspaces/autoload/workspaces")
(declare-function +workspace-kill "~/.config/emacs/modules/ui/workspaces/autoload/workspaces")
(declare-function +workspace-list-names "~/.config/emacs/modules/ui/workspaces/autoload/workspaces")
(declare-function +workspace-current-name "~/.config/emacs/modules/ui/workspaces/autoload/workspaces")
(declare-function +workspace-exists-p "~/.config/emacs/modules/ui/workspaces/autoload/workspaces")
(declare-function magit-status "magit-status")
(declare-function treemacs-add-and-display-current-project-exclusively "treemacs")
(declare-function claude-monitor-start "claude-monitor")

;;; Backwards Compatibility (re-export from claude-state)

(defalias 'claude-workspace-name #'claude--workspace-name)
(defalias 'claude-buffer-name #'claude--buffer-name)
(defalias 'claude-parse-workspace-name #'claude--parse-workspace-name)
(defalias 'claude-home-workspace-p #'claude--home-workspace-p)

;;; Workspace Queries

(defun claude-workspace-list ()
  "List all Claude workspaces.
Returns list of workspace names matching pattern 'repo:branch'."
  (seq-filter (lambda (name)
                (string-match-p "^[^:]+:[^:]+$" name))
              (+workspace-list-names)))

(defun claude-workspace-current ()
  "Get current workspace if it's a Claude workspace.
Returns workspace name or nil."
  (let ((current (+workspace-current-name)))
    (when (and current (string-match-p "^[^:]+:[^:]+$" current))
      current)))

(defun claude-home-exists-p (repo-name)
  "Return non-nil if home workspace exists for REPO-NAME."
  (let ((ws-name (claude--workspace-name repo-name claude-home-branch-name)))
    (+workspace-exists-p ws-name)))

(defun claude-workspace-path (workspace-name)
  "Get working directory for WORKSPACE-NAME.
For home workspaces, returns the main repo path.
For worktree workspaces, returns the worktree path."
  (let ((parsed (claude--parse-workspace-name workspace-name)))
    (when parsed
      (let* ((repo-name (car parsed))
             (branch-name (cdr parsed))
             (metadata (claude-metadata-read repo-name branch-name)))
        (or (plist-get metadata :worktree_path)
            (plist-get metadata :parent_repo))))))

;;; Repository Utilities

(defun claude--get-repo-path ()
  "Get the current repository path.
Returns expanded path or nil if not in a repo."
  (or (and (fboundp 'projectile-project-root)
           (projectile-project-root))
      (locate-dominating-file default-directory ".git")))

(defun claude-get-repo-from-worktree ()
  "Get parent repo path when in a Claude-managed worktree.
Returns the :parent_repo from worktree metadata, or nil if not found."
  (let* ((git-dir (string-trim (shell-command-to-string
                                "git rev-parse --git-dir 2>/dev/null")))
         (common-dir (string-trim (shell-command-to-string
                                   "git rev-parse --git-common-dir 2>/dev/null"))))
    ;; If git-dir equals common-dir, we're in the main repo, not a worktree
    (unless (or (string-empty-p git-dir)
                (string-empty-p common-dir)
                (string= (expand-file-name git-dir)
                         (expand-file-name common-dir)))
      ;; We're in a worktree, try to get parent repo from metadata
      (let* ((toplevel (string-trim (shell-command-to-string
                                     "git rev-parse --show-toplevel 2>/dev/null")))
             (worktree-name (file-name-nondirectory (directory-file-name toplevel)))
             (parent-dir (file-name-directory (directory-file-name toplevel)))
             (repo-name (file-name-nondirectory (directory-file-name parent-dir))))
        ;; Try to read metadata
        (let ((metadata (claude-metadata-read repo-name worktree-name)))
          (when metadata
            (plist-get metadata :parent_repo)))))))

;;; Workspace Creation with Rollback

(defun claude--create-workspace-ui (repo-name branch-name worktree-path)
  "Create Doom workspace and vterm buffer for REPO-NAME/BRANCH-NAME.
WORKTREE-PATH is the working directory.
Returns workspace name on success, signals error on failure.
Performs rollback of UI components on failure."
  (let ((workspace-name (claude--workspace-name repo-name branch-name))
        (buffer-name (claude--buffer-name repo-name branch-name))
        (workspace-created nil)
        (buffer-created nil))
    (condition-case err
        (progn
          ;; Create new Doom workspace
          (+workspace/new workspace-name)
          (+workspace/switch-to workspace-name)
          (setq workspace-created t)
          ;; Create vterm buffer
          (claude--create-vterm-in-dir buffer-name worktree-path)
          (setq buffer-created t)
          ;; Start Claude
          (claude--send-command buffer-name "claude")
          ;; Open treemacs rooted at workspace directory
          (let ((default-directory worktree-path))
            (treemacs-add-and-display-current-project-exclusively))
          ;; Return focus to Claude buffer
          (switch-to-buffer buffer-name)
          workspace-name)
      (error
       ;; Rollback on failure - reverse order of creation
       (when buffer-created
         (when-let ((buf (get-buffer buffer-name)))
           (let ((kill-buffer-query-functions nil))
             (kill-buffer buf))))
       (when workspace-created
         (ignore-errors (+workspace-kill workspace-name)))
       ;; Re-raise error
       (signal (car err) (cdr err))))))

(defun claude--create-worktree-workspace (repo-path branch-name parent-branch)
  "Finish workspace setup after worktree already created.
REPO-PATH is the parent repository.
BRANCH-NAME is the branch (worktree already exists).
PARENT-BRANCH is the branch it was created from.
Returns workspace name on success, signals error on failure.
Note: Caller must have already created the git worktree."
  (let* ((repo-name (claude--repo-name repo-path))
         (worktree-path (claude--worktree-path repo-name branch-name)))
    ;; Check for collision
    (claude--check-repo-collision repo-name branch-name repo-path)
    ;; Create metadata in 'creating' state
    (claude--create-metadata repo-name branch-name parent-branch repo-path "worktree")
    (condition-case err
        (progn
          ;; Verify worktree exists (caller should have created it)
          (unless (file-directory-p worktree-path)
            (error "Worktree not found at %s" worktree-path))
          ;; Create UI components
          (claude--create-workspace-ui repo-name branch-name worktree-path)
          ;; Mark as active
          (claude--update-status repo-name branch-name "active")
          ;; Start monitor
          (claude-monitor-start)
          (claude--workspace-name repo-name branch-name))
      (error
       ;; Rollback: mark as failed (don't delete metadata - allows inspection)
       (claude--update-status repo-name branch-name "failed")
       ;; Also try to clean up worktree if it was created
       (when (file-directory-p worktree-path)
         (ignore-errors (claude-worktree-remove-force repo-name branch-name)))
       (signal (car err) (cdr err))))))

(defun claude--create-worktree-from-existing (repo-path branch-name)
  "Create a workspace for an existing branch.
REPO-PATH is the parent repository.
BRANCH-NAME is the existing branch to check out.
Returns workspace name on success, signals error on failure."
  (let* ((repo-name (claude--repo-name repo-path))
         (worktree-path (claude--worktree-path repo-name branch-name))
         ;; Detect parent branch for existing branch
         (parent-branch (claude--detect-parent-branch repo-path branch-name)))
    ;; Check for collision
    (claude--check-repo-collision repo-name branch-name repo-path)
    ;; Create metadata in 'creating' state
    (claude--create-metadata repo-name branch-name parent-branch repo-path "worktree")
    (condition-case err
        (progn
          ;; Create worktree for existing branch
          (let ((result (claude-worktree-create-from-existing repo-path branch-name)))
            (unless (car result)
              (error "Worktree creation failed: %s" (cdr result))))
          ;; Create UI components
          (claude--create-workspace-ui repo-name branch-name worktree-path)
          ;; Mark as active
          (claude--update-status repo-name branch-name "active")
          ;; Start monitor
          (claude-monitor-start)
          (claude--workspace-name repo-name branch-name))
      (error
       ;; Rollback
       (claude--update-status repo-name branch-name "failed")
       (when (file-directory-p worktree-path)
         (ignore-errors (claude-worktree-remove-force repo-name branch-name)))
       (signal (car err) (cdr err))))))

(defun claude--create-home-workspace (repo-path)
  "Create a home workspace for REPO-PATH.
Returns workspace name on success, signals error on failure."
  (let* ((repo-name (claude--repo-name repo-path))
         (expanded-repo (expand-file-name repo-path)))
    ;; Create metadata in 'creating' state
    (claude--create-metadata repo-name claude-home-branch-name nil repo-path "home")
    (condition-case err
        (progn
          ;; Create UI components (no worktree needed - use repo directly)
          (claude--create-workspace-ui repo-name claude-home-branch-name expanded-repo)
          ;; Mark as active
          (claude--update-status repo-name claude-home-branch-name "active")
          ;; Start monitor
          (claude-monitor-start)
          (claude--workspace-name repo-name claude-home-branch-name))
      (error
       ;; Rollback
       (claude--update-status repo-name claude-home-branch-name "failed")
       (signal (car err) (cdr err))))))

;;; Workspace Navigation

(defun claude-workspace-switch (workspace-name)
  "Switch to WORKSPACE-NAME and focus its Claude buffer."
  (+workspace/switch-to workspace-name)
  ;; Focus the Claude buffer for this workspace
  (let ((parsed (claude--parse-workspace-name workspace-name)))
    (when parsed
      (let* ((buffer-name (claude--buffer-name (car parsed) (cdr parsed)))
             (buffer (get-buffer buffer-name)))
        (when (and buffer (buffer-live-p buffer))
          (switch-to-buffer buffer)
          ;; Jump to end and redraw to avoid scrollback replay
          (when (eq major-mode 'vterm-mode)
            (goto-char (point-max))
            (vterm-reset-cursor-point)))))))

;;; Interactive Commands

;;;###autoload
(defun claude-create-workspace ()
  "Interactive command to create new Claude workspace.
Detects current repo, prompts for branch name, creates worktree and workspace."
  (interactive)
  ;; Check for claude command
  (unless (executable-find "claude")
    (user-error "Claude CLI not found. Run: npm install -g @anthropic-ai/claude-code"))
  ;; Get repo path
  (let ((repo-path (claude--get-repo-path)))
    (unless repo-path
      (user-error "Not in a git repository"))
    (let* ((repo-path (expand-file-name repo-path))
           (current-branch (claude-git-current-branch repo-path))
           (branch-name (read-string
                         (format "Branch name (from %s): " current-branch))))
      ;; Validate branch name
      (when-let ((err (claude--validate-branch-name branch-name)))
        (user-error err))
      ;; Try to create worktree
      (let ((result (claude-worktree-create repo-path branch-name current-branch)))
        (cond
         ;; Success
         ((car result)
          (condition-case err
              (progn
                (claude--create-worktree-workspace repo-path branch-name current-branch)
                (message "Created workspace %s:%s"
                         (claude--repo-name repo-path) branch-name))
            (error
             (user-error "Workspace creation failed: %s" (error-message-string err)))))
         ;; Branch already exists
         ((eq (cdr result) 'branch-exists)
          (if (y-or-n-p (format "Branch '%s' exists. Reuse it? " branch-name))
              (condition-case err
                  (progn
                    (claude--create-worktree-from-existing repo-path branch-name)
                    (message "Created workspace %s:%s (existing branch)"
                             (claude--repo-name repo-path) branch-name))
                (error
                 (user-error "Workspace creation failed: %s" (error-message-string err))))
            (message "Cancelled")))
         ;; Directory already exists
         ((eq (cdr result) 'dir-exists)
          (user-error "Worktree directory already exists at %s"
                      (claude--worktree-path (claude--repo-name repo-path) branch-name)))
         ;; Other error
         (t
          (user-error "Failed to create worktree: %s" (cdr result))))))))

;;;###autoload
(defun claude-home-workspace ()
  "Jump to home workspace for current repo, creating if needed.
From main repo: creates/jumps to home workspace.
From Claude-managed worktree: jumps to parent repo's home.
From non-Claude worktree: shows error with guidance."
  (interactive)
  ;; Check for claude command
  (unless (executable-find "claude")
    (user-error "Claude CLI not found. Run: npm install -g @anthropic-ai/claude-code"))
  ;; Determine if we're in a worktree or main repo
  (let* ((git-dir (string-trim (shell-command-to-string
                                "git rev-parse --git-dir 2>/dev/null")))
         (common-dir (string-trim (shell-command-to-string
                                   "git rev-parse --git-common-dir 2>/dev/null"))))
    (cond
     ;; Not in a git repo
     ((or (string-empty-p git-dir) (string-empty-p common-dir))
      (user-error "Not in a git repository"))
     ;; In a worktree (git-dir != common-dir)
     ((not (string= (expand-file-name git-dir)
                    (expand-file-name common-dir)))
      (let ((parent-repo (claude-get-repo-from-worktree)))
        (if parent-repo
            (claude--home-workspace-for-repo parent-repo)
          (user-error "Not in a Claude-managed worktree. Use SPC C h from the main repo"))))
     ;; In main repo
     (t
      (let ((repo-path (claude--get-repo-path)))
        (if repo-path
            (claude--home-workspace-for-repo (expand-file-name repo-path))
          (user-error "Not in a git repository")))))))

(defun claude--home-workspace-for-repo (repo-path)
  "Create or switch to home workspace for REPO-PATH."
  (let* ((repo-name (claude--repo-name repo-path))
         (workspace-name (claude--workspace-name repo-name claude-home-branch-name)))
    (if (claude-home-exists-p repo-name)
        ;; Switch to existing home workspace
        (progn
          (claude-workspace-switch workspace-name)
          (message "Switched to home workspace for %s" repo-name))
      ;; Create new home workspace
      (condition-case err
          (progn
            (claude--create-home-workspace repo-path)
            (message "Created home workspace for %s" repo-name))
        (error
         (user-error "Failed to create home workspace: %s" (error-message-string err)))))))

;;;###autoload
(defun claude-jump-to-buffer ()
  "Jump to Claude buffer in current workspace."
  (interactive)
  (let ((workspace (claude-workspace-current)))
    (if workspace
        (let* ((parsed (claude--parse-workspace-name workspace))
               (buffer-name (claude--buffer-name (car parsed) (cdr parsed)))
               (buffer (get-buffer buffer-name)))
          (if buffer
              (switch-to-buffer buffer)
            (user-error "Claude buffer not found: %s" buffer-name)))
      (user-error "Not in a Claude workspace"))))

;;;###autoload
(defun claude-magit-status ()
  "Open magit in current workspace (works for both home and worktree)."
  (interactive)
  (let ((workspace (claude-workspace-current)))
    (if workspace
        (let ((workspace-path (claude-workspace-path workspace)))
          (if (and workspace-path (file-directory-p workspace-path))
              (let ((default-directory workspace-path))
                (magit-status))
            (user-error "Workspace path not found")))
      (user-error "Not in a Claude workspace"))))

;;; Terminal Utilities

;;;###autoload
(defun claude-new-terminal ()
  "Spawn a new terminal in the current Claude workspace."
  (interactive)
  (let ((workspace (claude-workspace-current)))
    (if workspace
        (let* ((parsed (claude--parse-workspace-name workspace))
               (repo-name (car parsed))
               (branch-name (cdr parsed))
               (workspace-path (claude-workspace-path workspace))
               (buffer-name (claude-terminal-buffer-name repo-name branch-name)))
          (if (and workspace-path (file-directory-p workspace-path))
              (progn
                (claude--create-vterm-in-dir buffer-name workspace-path)
                (message "Created terminal %s" buffer-name))
            (user-error "Workspace path not found")))
      (user-error "Not in a Claude workspace"))))

(provide 'claude-workspace)
;;; claude-workspace.el ends here
