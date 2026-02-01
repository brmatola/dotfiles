;;; claude-cleanup.el --- Cleanup workflow -*- lexical-binding: t; -*-

;;; Commentary:
;; Cleanup workflow and merge status display for Claude workspaces.

;;; Code:

(require 'magit nil t)

;; Forward declarations
(declare-function claude-metadata-read "claude-worktree")
(declare-function claude-metadata-delete "claude-worktree")
(declare-function claude-worktree-path "claude-worktree")
(declare-function claude-worktree-remove "claude-worktree")
(declare-function claude-git-commits-ahead "claude-worktree")
(declare-function claude-git-merge-branch "claude-worktree")
(declare-function claude-git-delete-branch "claude-worktree")
(declare-function claude-workspace-current "claude-workspace")
(declare-function claude-workspace-delete "claude-workspace")
(declare-function claude-workspace-list "claude-workspace")
(declare-function claude-buffer-name "claude-workspace")
(declare-function claude-parse-workspace-name "claude-workspace")
(declare-function claude-home-workspace-p "claude-workspace")
(declare-function claude-workspace-path "claude-workspace")
(declare-function claude-monitor-stop "claude-monitor")

(defvar claude-cleanup-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "v" #'claude-cleanup-view-diff)
    (define-key map "m" #'claude-cleanup-merge)
    (define-key map "d" #'claude-cleanup-delete)
    (define-key map "c" #'claude-cleanup-cancel)
    (define-key map "q" #'claude-cleanup-cancel)
    map)
  "Keymap for Claude cleanup mode.")

(define-derived-mode claude-cleanup-mode special-mode "Claude-Cleanup"
  "Major mode for Claude cleanup status buffer.

\\{claude-cleanup-mode-map}")

;; Evil bindings for cleanup mode (Doom overrides the base keymap)
(after! evil
  (evil-set-initial-state 'claude-cleanup-mode 'normal)
  (map! :map claude-cleanup-mode-map
        :n "v" #'claude-cleanup-view-diff
        :n "m" #'claude-cleanup-merge
        :n "d" #'claude-cleanup-delete
        :n "c" #'claude-cleanup-cancel
        :n "q" #'claude-cleanup-cancel))

(defvar-local claude-cleanup--workspace-info nil
  "Current workspace info for cleanup buffer.")

;;;###autoload
(defun claude-close-workspace (&optional workspace-name)
  "Close WORKSPACE-NAME with merge-aware cleanup.
If WORKSPACE-NAME is nil, uses current workspace.
Home workspaces get a simpler flow (dirty check, no merge)."
  (interactive)
  (let* ((ws (or workspace-name (claude-workspace-current)))
         (parsed (and ws (claude-parse-workspace-name ws))))
    (if parsed
        (if (claude-home-workspace-p ws)
            (claude-cleanup--close-home-workspace parsed)
          (claude-cleanup--show-status parsed))
      (user-error "Not in a Claude workspace"))))

(defun claude-cleanup--close-home-workspace (parsed)
  "Close home workspace for PARSED (repo-name . branch-name).
Checks for uncommitted changes and prompts if dirty."
  (let* ((repo-name (car parsed))
         (branch-name (cdr parsed))
         (workspace-name (format "%s:%s" repo-name branch-name))
         (workspace-path (claude-workspace-path workspace-name)))
    (if (not workspace-path)
        (progn
          ;; No path found, just close the workspace
          (claude-cleanup--do-home-cleanup repo-name branch-name)
          (message "Home workspace closed"))
      ;; Check for dirty state
      (let* ((default-directory workspace-path)
             (status (string-trim (shell-command-to-string
                                   "git status --porcelain 2>/dev/null")))
             (is-dirty (not (string-empty-p status))))
        (if (and is-dirty
                 (not (y-or-n-p "Uncommitted changes. Close anyway? ")))
            (message "Cancelled")
          (claude-cleanup--do-home-cleanup repo-name branch-name)
          (message "Home workspace closed"))))))

(defun claude-cleanup--show-status (parsed)
  "Show status buffer for PARSED workspace (repo-name . branch-name)."
  (let* ((repo-name (car parsed))
         (branch-name (cdr parsed))
         (metadata (claude-metadata-read repo-name branch-name))
         (worktree-path (claude-worktree-path repo-name branch-name))
         (parent-branch (plist-get metadata :parent_branch))
         (commits-ahead (if (file-directory-p worktree-path)
                            (claude-git-commits-ahead worktree-path parent-branch)
                          0))
         (buffer (get-buffer-create "*Claude Cleanup*"))
         (info (list :repo-name repo-name
                     :branch-name branch-name
                     :worktree-path worktree-path
                     :parent-branch parent-branch
                     :parent-repo (plist-get metadata :parent_repo)
                     :commits-ahead commits-ahead)))
    ;; Fast path: already merged
    (if (and (= commits-ahead 0) parent-branch)
        (when (y-or-n-p "Branch already merged. Clean up? ")
          (claude-cleanup--do-cleanup info))
      ;; Show status buffer
      (with-current-buffer buffer
        (claude-cleanup-mode)
        (setq claude-cleanup--workspace-info info)
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (propertize "Claude Workspace Cleanup\n"
                              'face 'doom-modeline-buffer-major-mode)
                  "\n"
                  (format "Workspace: %s:%s\n" repo-name branch-name)
                  (format "Parent: %s\n" (or parent-branch "unknown"))
                  (format "Status: %s\n"
                          (cond
                           ((= commits-ahead 0) "No commits ahead (merged)")
                           ((= commits-ahead 1) "1 commit ahead")
                           (t (format "%d commits ahead" commits-ahead))))
                  "\n"
                  (propertize "[v]" 'face 'font-lock-keyword-face)
                  " View diff\n"
                  (propertize "[m]" 'face 'font-lock-keyword-face)
                  " Merge & cleanup\n"
                  (propertize "[d]" 'face 'font-lock-keyword-face)
                  (if (> commits-ahead 0)
                      " Delete (lose changes)\n"
                    " Delete\n")
                  (propertize "[c]" 'face 'font-lock-keyword-face)
                  " Cancel\n"))
        (goto-char (point-min)))
      (switch-to-buffer buffer))))

(defun claude-cleanup-view-diff ()
  "Show diff in magit."
  (interactive)
  (when-let* ((info claude-cleanup--workspace-info)
              (worktree-path (plist-get info :worktree-path)))
    (when (file-directory-p worktree-path)
      (let ((default-directory worktree-path))
        (magit-status)))))

(defun claude-cleanup-merge ()
  "Merge branch and clean up workspace."
  (interactive)
  (when-let ((info claude-cleanup--workspace-info))
    (let* ((parent-repo (plist-get info :parent-repo))
           (parent-branch (plist-get info :parent-branch))
           (branch-name (plist-get info :branch-name)))
      (if (not (and parent-repo parent-branch))
          (user-error "Missing parent repo or branch info")
        (let ((result (claude-git-merge-branch parent-repo
                                               parent-branch
                                               branch-name)))
          (if (car result)
              (progn
                (claude-cleanup--do-cleanup info)
                (message "Merged and cleaned up successfully"))
            (user-error "Merge failed: %s\nResolve conflicts in magit, then try again"
                        (cdr result))))))))

(defun claude-cleanup-delete ()
  "Delete workspace without merging."
  (interactive)
  (when-let ((info claude-cleanup--workspace-info))
    (let ((commits-ahead (plist-get info :commits-ahead)))
      (when (or (= commits-ahead 0)
                (yes-or-no-p (format "Delete %d unmerged commits? "
                                     commits-ahead)))
        (claude-cleanup--do-cleanup info)
        (message "Workspace deleted")))))

(defun claude-cleanup-cancel ()
  "Cancel cleanup."
  (interactive)
  (quit-window t))

(defun claude-cleanup--do-cleanup (info)
  "Perform actual cleanup steps for INFO."
  (let ((repo-name (plist-get info :repo-name))
        (branch-name (plist-get info :branch-name))
        (parent-repo (plist-get info :parent-repo))
        (workspace-name (format "%s:%s"
                                (plist-get info :repo-name)
                                (plist-get info :branch-name))))
    ;; 1. Kill Claude buffer
    (when-let ((buffer (get-buffer (claude-buffer-name repo-name branch-name))))
      (kill-buffer buffer))
    ;; 2. Kill all terminal buffers for this workspace
    (let ((term-pattern (format "\\*term:%s:%s:[0-9]+\\*"
                                (regexp-quote repo-name)
                                (regexp-quote branch-name))))
      (dolist (buf (buffer-list))
        (when (string-match-p term-pattern (buffer-name buf))
          (kill-buffer buf))))
    ;; 3. Delete Doom workspace
    (claude-workspace-delete workspace-name)
    ;; 4. Remove git worktree
    (claude-worktree-remove repo-name branch-name)
    ;; 5. Delete branch from parent repo
    (when parent-repo
      (claude-git-delete-branch parent-repo branch-name))
    ;; 6. Delete metadata
    (claude-metadata-delete repo-name branch-name)
    ;; 7. Stop monitor if no more workspaces
    (when (null (claude-workspace-list))
      (claude-monitor-stop))
    ;; Close status buffer
    (when-let ((buffer (get-buffer "*Claude Cleanup*")))
      (kill-buffer buffer))))

(defun claude-cleanup--do-home-cleanup (repo-name branch-name)
  "Perform cleanup steps for home workspace REPO-NAME:BRANCH-NAME.
Simpler than worktree cleanup: no worktree removal, no metadata, no merge."
  (let ((workspace-name (format "%s:%s" repo-name branch-name)))
    ;; 1. Kill Claude buffer
    (when-let ((buffer (get-buffer (claude-buffer-name repo-name branch-name))))
      (kill-buffer buffer))
    ;; 2. Kill all terminal buffers for this home workspace
    (let ((term-pattern (format "\\*term:%s:%s:[0-9]+\\*"
                                (regexp-quote repo-name)
                                (regexp-quote branch-name))))
      (dolist (buf (buffer-list))
        (when (string-match-p term-pattern (buffer-name buf))
          (kill-buffer buf))))
    ;; 3. Delete Doom workspace
    (claude-workspace-delete workspace-name)
    ;; 4. Stop monitor if no more workspaces
    (when (null (claude-workspace-list))
      (claude-monitor-stop))))

(provide 'claude-cleanup)
;;; claude-cleanup.el ends here
