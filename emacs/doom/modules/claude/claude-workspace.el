;;; claude-workspace.el --- Workspace management -*- lexical-binding: t; -*-

;;; Commentary:
;; Doom workspace and vterm session management for Claude.

;;; Code:

(require 'vterm)
(require 'projectile nil t)

;; Forward declarations
(declare-function +workspace/new "~/.config/emacs/modules/ui/workspaces/autoload/workspaces")
(declare-function +workspace/switch-to "~/.config/emacs/modules/ui/workspaces/autoload/workspaces")
(declare-function +workspace/delete "~/.config/emacs/modules/ui/workspaces/autoload/workspaces")
(declare-function +workspace-list-names "~/.config/emacs/modules/ui/workspaces/autoload/workspaces")
(declare-function +workspace-current-name "~/.config/emacs/modules/ui/workspaces/autoload/workspaces")
(declare-function magit-status "magit-status")

;; Forward declarations for claude-worktree functions
(declare-function claude-worktree-create "claude-worktree")
(declare-function claude-worktree-create-from-existing "claude-worktree")
(declare-function claude-worktree-remove "claude-worktree")
(declare-function claude-worktree-path "claude-worktree")
(declare-function claude-repo-name "claude-worktree")
(declare-function claude-git-current-branch "claude-worktree")
(declare-function claude-monitor-start "claude-monitor")

;;; Naming utilities

(defun claude-workspace-name (repo-name branch-name)
  "Generate workspace name for REPO-NAME and BRANCH-NAME."
  (format "%s:%s" repo-name branch-name))

(defun claude-buffer-name (repo-name branch-name)
  "Generate Claude buffer name for REPO-NAME and BRANCH-NAME."
  (format "*claude:%s:%s*" repo-name branch-name))

(defun claude-parse-workspace-name (workspace-name)
  "Parse WORKSPACE-NAME into (repo-name . branch-name) cons cell.
Returns nil if not a valid Claude workspace name."
  (when (and workspace-name
             (string-match "^\\([^:]+\\):\\([^:]+\\)$" workspace-name))
    (cons (match-string 1 workspace-name)
          (match-string 2 workspace-name))))

;;; Workspace queries

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

;;; Workspace operations

(defun claude-workspace-create (repo-name branch-name worktree-path)
  "Create Doom workspace and start Claude session.
WORKTREE-PATH is the path to the git worktree.
Returns the workspace name on success, nil on failure.
On failure, cleans up any partially created resources."
  (let ((workspace-name (claude-workspace-name repo-name branch-name))
        (buffer-name (claude-buffer-name repo-name branch-name))
        (workspace-created nil)
        (buffer-created nil))
    (condition-case err
        (progn
          ;; Create new workspace
          (+workspace/new workspace-name)
          (+workspace/switch-to workspace-name)
          (setq workspace-created t)
          ;; Create vterm buffer
          (let ((vterm-buffer (get-buffer-create buffer-name)))
            (setq buffer-created t)
            (with-current-buffer vterm-buffer
              (unless (eq major-mode 'vterm-mode)
                (vterm-mode)))
            (switch-to-buffer vterm-buffer)
            ;; Send commands to vterm: cd to worktree and run claude
            (vterm-send-string (format "cd %s && clear && claude\n"
                                       (shell-quote-argument worktree-path))))
          workspace-name)
      (error
       ;; Cleanup on failure - reverse order of creation
       (when buffer-created
         (when-let ((buf (get-buffer buffer-name)))
           (kill-buffer buf)))
       (when workspace-created
         (ignore-errors (+workspace/delete workspace-name)))
       ;; Re-raise error
       (signal (car err) (cdr err))))))

(defun claude-workspace-switch (workspace-name)
  "Switch to WORKSPACE-NAME."
  (+workspace/switch-to workspace-name))

(defun claude-workspace-delete (workspace-name)
  "Delete WORKSPACE-NAME and kill associated buffers."
  (let ((parsed (claude-parse-workspace-name workspace-name)))
    (when parsed
      (let ((buffer-name (claude-buffer-name (car parsed) (cdr parsed))))
        ;; Kill the claude buffer if it exists
        (when-let ((buffer (get-buffer buffer-name)))
          (kill-buffer buffer))))
    ;; Delete the workspace
    (+workspace/delete workspace-name)))

;;; Interactive commands

(defun claude--get-repo-path ()
  "Get the current repository path.
Returns expanded path or nil if not in a repo."
  (or (and (fboundp 'projectile-project-root)
           (projectile-project-root))
      (locate-dominating-file default-directory ".git")))

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
           (repo-name (claude-repo-name repo-path))
           (current-branch (claude-git-current-branch repo-path))
           (branch-name (read-string
                         (format "Branch name (from %s): " current-branch)))
           (parent-branch current-branch))
      ;; Validate branch name
      (when (string-empty-p branch-name)
        (user-error "Branch name cannot be empty"))
      ;; Create worktree
      (let ((result (claude-worktree-create repo-path branch-name parent-branch)))
        (cond
         ;; Success
         ((car result)
          (let ((worktree-path (cdr result)))
            ;; Create workspace and start Claude, with cleanup on failure
            (condition-case err
                (progn
                  (claude-workspace-create repo-name branch-name worktree-path)
                  ;; Start monitor
                  (claude-monitor-start)
                  (message "Created workspace %s:%s" repo-name branch-name))
              (error
               ;; Cleanup worktree and metadata on workspace creation failure
               (claude-worktree-remove repo-name branch-name)
               (user-error "Workspace creation failed: %s" (error-message-string err))))))
         ;; Branch already exists
         ((eq (cdr result) 'branch-exists)
          (if (y-or-n-p (format "Branch '%s' exists. Reuse it? " branch-name))
              (let ((reuse-result (claude-worktree-create-from-existing
                                   repo-path branch-name)))
                (if (car reuse-result)
                    (let ((worktree-path (cdr reuse-result)))
                      (condition-case err
                          (progn
                            (claude-workspace-create repo-name branch-name worktree-path)
                            (claude-monitor-start)
                            (message "Created workspace %s:%s (existing branch)"
                                     repo-name branch-name))
                        (error
                         (claude-worktree-remove repo-name branch-name)
                         (user-error "Workspace creation failed: %s" (error-message-string err)))))
                  (user-error "Failed to create worktree: %s" (cdr reuse-result))))
            (message "Cancelled")))
         ;; Directory already exists
         ((eq (cdr result) 'dir-exists)
          (user-error "Worktree directory already exists at %s"
                      (claude-worktree-path repo-name branch-name)))
         ;; Other error
         (t
          (user-error "Failed to create worktree: %s" (cdr result))))))))

;;;###autoload
(defun claude-jump-to-buffer ()
  "Jump to Claude buffer in current workspace."
  (interactive)
  (let ((workspace (claude-workspace-current)))
    (if workspace
        (let* ((parsed (claude-parse-workspace-name workspace))
               (buffer-name (claude-buffer-name (car parsed) (cdr parsed)))
               (buffer (get-buffer buffer-name)))
          (if buffer
              (switch-to-buffer buffer)
            (user-error "Claude buffer not found: %s" buffer-name)))
      (user-error "Not in a Claude workspace"))))

;;;###autoload
(defun claude-magit-status ()
  "Open magit in current worktree."
  (interactive)
  (let ((workspace (claude-workspace-current)))
    (if workspace
        (let* ((parsed (claude-parse-workspace-name workspace))
               (worktree-path (claude-worktree-path (car parsed) (cdr parsed))))
          (if (file-directory-p worktree-path)
              (let ((default-directory worktree-path))
                (magit-status))
            (user-error "Worktree not found: %s" worktree-path)))
      (user-error "Not in a Claude workspace"))))

(provide 'claude-workspace)
;;; claude-workspace.el ends here
