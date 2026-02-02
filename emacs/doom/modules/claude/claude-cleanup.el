;;; claude-cleanup.el --- Cleanup workflow -*- lexical-binding: t; -*-

;;; Commentary:
;; Cleanup workflow and merge status display for Claude workspaces.
;; Now uses claude-state for state tracking with cleanup_progress.

;;; Code:

(require 'magit nil t)
(require 'claude-state)
(require 'claude-vterm)

;; Forward declarations
(declare-function +workspace-current-name "~/.config/emacs/modules/ui/workspaces/autoload/workspaces")
(declare-function +workspace-list-names "~/.config/emacs/modules/ui/workspaces/autoload/workspaces")
(declare-function +workspace/switch-to "~/.config/emacs/modules/ui/workspaces/autoload/workspaces")
(declare-function +workspace-kill "~/.config/emacs/modules/ui/workspaces/autoload/workspaces")
(declare-function claude-worktree-remove "claude-worktree")
(declare-function claude-worktree-remove-force "claude-worktree")
(declare-function claude-git-commits-ahead "claude-worktree")
(declare-function claude-git-merge-branch "claude-worktree")
(declare-function claude-git-delete-branch "claude-worktree")
(declare-function claude-git-has-uncommitted-changes "claude-worktree")
(declare-function claude-git-fetch "claude-worktree")
(declare-function claude-workspace-current "claude-workspace")
(declare-function claude-workspace-list "claude-workspace")
(declare-function claude-monitor-stop "claude-monitor")

;;; Cleanup Mode for Status Buffer

(defvar claude-cleanup-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "v" #'claude-cleanup-view-diff)
    (define-key map "m" #'claude-cleanup-merge)
    (define-key map "p" #'claude-cleanup-push-pr)
    (define-key map "d" #'claude-cleanup-delete)
    (define-key map "c" #'claude-cleanup-cancel)
    (define-key map "q" #'claude-cleanup-cancel)
    map)
  "Keymap for Claude cleanup mode.")

(define-derived-mode claude-cleanup-mode special-mode "Claude-Cleanup"
  "Major mode for Claude cleanup status buffer.

\\{claude-cleanup-mode-map}")

;; Evil bindings for cleanup mode
(with-eval-after-load 'evil
  (add-hook 'claude-cleanup-mode-hook
            (lambda ()
              (when (fboundp 'evil-local-set-key)
                (evil-local-set-key 'normal (kbd "v") #'claude-cleanup-view-diff)
                (evil-local-set-key 'normal (kbd "m") #'claude-cleanup-merge)
                (evil-local-set-key 'normal (kbd "p") #'claude-cleanup-push-pr)
                (evil-local-set-key 'normal (kbd "d") #'claude-cleanup-delete)
                (evil-local-set-key 'normal (kbd "c") #'claude-cleanup-cancel)
                (evil-local-set-key 'normal (kbd "q") #'claude-cleanup-cancel)
                (evil-local-set-key 'normal (kbd "RET") #'claude-cleanup-merge)))))

(defvar-local claude-cleanup--workspace-info nil
  "Current workspace info for cleanup buffer.")

;;; Entry Point

;;;###autoload
(defun claude-close-workspace (&optional workspace-name)
  "Close WORKSPACE-NAME with merge-aware cleanup.
If WORKSPACE-NAME is nil, uses current workspace.
Home workspaces get a simpler flow (dirty check, no merge)."
  (interactive)
  (let* ((ws (or workspace-name (claude-workspace-current)))
         (parsed (and ws (claude--parse-workspace-name ws))))
    (if parsed
        (let ((repo-name (car parsed))
              (branch-name (cdr parsed)))
          (if (claude--home-workspace-p ws)
              (claude-cleanup--close-home-workspace repo-name branch-name)
            (claude-cleanup--show-status repo-name branch-name)))
      (user-error "Not in a Claude workspace"))))

;;; Home Workspace Cleanup (Simpler Flow)

(defun claude-cleanup--close-home-workspace (repo-name branch-name)
  "Close home workspace for REPO-NAME/BRANCH-NAME.
Checks for uncommitted changes and prompts if dirty."
  (let ((metadata (claude-metadata-read repo-name branch-name)))
    (if (not metadata)
        (progn
          ;; No metadata - just try to close what we can
          (claude-cleanup--do-home-cleanup repo-name branch-name)
          (message "Home workspace closed"))
      ;; Check for dirty state
      (let* ((parent-repo (plist-get metadata :parent_repo))
             (is-dirty (and parent-repo
                            (file-directory-p parent-repo)
                            (claude-git-has-uncommitted-changes parent-repo))))
        (if (and is-dirty
                 (not (y-or-n-p "Uncommitted changes. Close anyway? ")))
            (message "Cancelled")
          (claude-cleanup--do-home-cleanup repo-name branch-name)
          (message "Home workspace closed"))))))

(defun claude-cleanup--do-home-cleanup (repo-name branch-name)
  "Perform cleanup steps for home workspace REPO-NAME/BRANCH-NAME."
  (let ((workspace-name (claude--workspace-name repo-name branch-name)))
    ;; 1. Switch away from this workspace before deleting it
    (when (equal (+workspace-current-name) workspace-name)
      (let ((other-workspaces (remove workspace-name (+workspace-list-names))))
        (if other-workspaces
            (+workspace/switch-to (car other-workspaces))
          (+workspace/switch-to "+workspace--last"))))
    ;; 2. Kill all buffers
    (claude--kill-workspace-buffers repo-name branch-name)
    ;; 3. Delete Doom workspace
    (ignore-errors (+workspace-kill workspace-name))
    ;; 4. Delete metadata
    (claude-metadata-delete repo-name branch-name)
    ;; 5. Stop monitor if no more workspaces
    (when (null (claude-workspace-list))
      (claude-monitor-stop))))

;;; Worktree Workspace Cleanup (Full Flow with Status Buffer)

(defun claude-cleanup--show-status (repo-name branch-name)
  "Show status buffer for REPO-NAME/BRANCH-NAME workspace."
  (let* ((metadata (claude-metadata-read repo-name branch-name))
         (worktree-path (plist-get metadata :worktree_path))
         (parent-branch (plist-get metadata :parent_branch))
         (workflow-phase (claude--workflow-phase repo-name branch-name))
         (workflow (plist-get metadata :workflow))
         (workflow-plan (and workflow (plist-get workflow :plan)))
         (commits-ahead (if (and worktree-path (file-directory-p worktree-path))
                            (claude-git-commits-ahead worktree-path parent-branch)
                          0))
         (buffer (get-buffer-create "*Claude Cleanup*"))
         (info (list :repo-name repo-name
                     :branch-name branch-name
                     :worktree-path worktree-path
                     :parent-branch parent-branch
                     :parent-repo (plist-get metadata :parent_repo)
                     :commits-ahead commits-ahead)))
    ;; Fast path: already merged (only if we could determine status)
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
                  ;; Show workflow info if present
                  (if workflow-phase
                      (format "Workflow: %s (phase: %s)\n"
                              (or workflow-plan "unknown")
                              workflow-phase)
                    "")
                  (format "Status: %s\n"
                          (cond
                           ((< commits-ahead 0) "Unknown (couldn't read git status)")
                           ((= commits-ahead 0) "No commits ahead (merged)")
                           ((= commits-ahead 1) "1 commit ahead")
                           (t (format "%d commits ahead" commits-ahead))))
                  "\n"
                  (propertize "[v]" 'face 'font-lock-keyword-face)
                  " View diff\n"
                  (propertize "[m]" 'face 'font-lock-keyword-face)
                  (if (< commits-ahead 0)
                      " Merge & cleanup (may fail)\n"
                    " Merge & cleanup\n")
                  (propertize "[p]" 'face 'font-lock-keyword-face)
                  " Push & create PR\n"
                  (propertize "[d]" 'face 'font-lock-keyword-face)
                  (cond
                   ((< commits-ahead 0) " Delete (status unknown)\n")
                   ((> commits-ahead 0) " Delete (lose changes)\n")
                   (t " Delete\n"))
                  (propertize "[c]" 'face 'font-lock-keyword-face)
                  " Cancel\n"))
        (goto-char (point-min)))
      (switch-to-buffer buffer))))

;;; Status Buffer Actions

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
        ;; Fetch first to ensure we have latest
        (message "Fetching latest changes...")
        (claude-git-fetch parent-repo)
        ;; Try merge
        (let ((result (claude-git-merge-branch parent-repo
                                               parent-branch
                                               branch-name)))
          (if (car result)
              ;; Merge succeeded - cleanup runs async
              (claude-cleanup--do-cleanup info)
            (if (eq (cdr result) 'conflict)
                (user-error "Merge conflicts detected. Resolve in magit, then try again")
              (user-error "Merge failed: %s" (cdr result)))))))))

(defun claude-cleanup-delete ()
  "Delete workspace without merging."
  (interactive)
  (when-let ((info claude-cleanup--workspace-info))
    (let ((commits-ahead (plist-get info :commits-ahead)))
      (when (or (<= commits-ahead 0)
                (yes-or-no-p (format "Delete %d unmerged commits? " commits-ahead)))
        ;; Cleanup runs async - message is shown by do-cleanup
        (claude-cleanup--do-cleanup info)))))

(defun claude-cleanup-push-pr ()
  "Push branch and create PR."
  (interactive)
  (when-let* ((info claude-cleanup--workspace-info)
              (branch-name (plist-get info :branch-name))
              (worktree-path (plist-get info :worktree-path)))
    (if (not (and worktree-path (file-directory-p worktree-path)))
        (user-error "Worktree path not found")
      (let ((default-directory worktree-path))
        ;; Push
        (message "Pushing %s to origin..." branch-name)
        (let ((push-result (shell-command-to-string
                            (format "git push -u origin %s 2>&1" branch-name))))
          (if (string-match-p "error\\|fatal" push-result)
              (user-error "Push failed: %s" push-result)
            ;; Create PR
            (message "Creating PR...")
            (let ((pr-result (shell-command-to-string "gh pr create --fill 2>&1")))
              (if (string-match-p "error\\|fatal" pr-result)
                  (user-error "PR creation failed: %s" pr-result)
                ;; Success - close status buffer but keep worktree
                (when-let ((buffer (get-buffer "*Claude Cleanup*")))
                  (quit-window t (get-buffer-window buffer)))
                (message "PR created for %s" branch-name)))))))))

(defun claude-cleanup-cancel ()
  "Cancel cleanup."
  (interactive)
  (quit-window t))

;;; Main Cleanup Implementation

(defun claude-cleanup--do-cleanup (info)
  "Perform actual cleanup steps for INFO.
UI cleanup is sync, git operations run async in background."
  (let* ((repo-name (plist-get info :repo-name))
         (branch-name (plist-get info :branch-name))
         (parent-repo (plist-get info :parent-repo))
         (workspace-name (claude--workspace-name repo-name branch-name)))
    ;; Set status to closing
    (claude--update-status repo-name branch-name "closing")
    ;; Initialize cleanup progress
    (claude--init-cleanup-progress repo-name branch-name)

    ;; 0. Close status buffer first
    (when-let ((buffer (get-buffer "*Claude Cleanup*")))
      (kill-buffer buffer))

    (condition-case err
        (progn
          ;; === SYNC PART: UI cleanup (fast) ===

          ;; 1. Switch away from this workspace before deleting it
          (when (equal (+workspace-current-name) workspace-name)
            (let ((other-workspaces (remove workspace-name (+workspace-list-names))))
              (if other-workspaces
                  (+workspace/switch-to (car other-workspaces))
                (+workspace/switch-to "+workspace--last"))))

          ;; 2. Kill all buffers
          (claude--kill-workspace-buffers repo-name branch-name)
          (claude--mark-cleanup-step repo-name branch-name :buffers_killed)

          ;; 3. Kill Doom workspace
          (ignore-errors (+workspace-kill workspace-name))
          (claude--mark-cleanup-step repo-name branch-name :workspace_removed)

          ;; === ASYNC PART: Git cleanup (slow) ===
          ;; Run in background so UI stays responsive
          (claude-cleanup--async-git-cleanup info)

          (message "Closing %s (cleanup running in background)" workspace-name))
      (error
       ;; Mark as stuck for manual intervention
       (claude--update-status repo-name branch-name "stuck")
       (user-error "Cleanup failed: %s" (error-message-string err))))))

(defun claude-cleanup--async-git-cleanup (info)
  "Run git cleanup operations asynchronously for INFO."
  (let* ((repo-name (plist-get info :repo-name))
         (branch-name (plist-get info :branch-name))
         (parent-repo (plist-get info :parent-repo))
         (worktree-path (plist-get info :worktree-path))
         (workspace-name (claude--workspace-name repo-name branch-name)))
    ;; Build the shell command to run all git operations
    (let* ((worktree-cmd (when worktree-path
                           (format "cd %s && git worktree remove --force %s 2>/dev/null || true"
                                   (shell-quote-argument parent-repo)
                                   (shell-quote-argument worktree-path))))
           (branch-cmd (when parent-repo
                         (format "cd %s && git branch -d %s 2>/dev/null || git branch -D %s 2>/dev/null || true"
                                 (shell-quote-argument parent-repo)
                                 (shell-quote-argument branch-name)
                                 (shell-quote-argument branch-name))))
           (full-cmd (string-join (delq nil (list worktree-cmd branch-cmd)) " && ")))
      (when (and full-cmd (not (string-empty-p full-cmd)))
        (make-process
         :name (format "claude-cleanup-%s" workspace-name)
         :buffer nil
         :command (list "sh" "-c" full-cmd)
         :sentinel (lambda (proc event)
                     (when (string-match-p "finished" event)
                       ;; Mark steps complete and finalize
                       (claude--mark-cleanup-step repo-name branch-name :worktree_removed)
                       (claude--mark-cleanup-step repo-name branch-name :branch_deleted)
                       (claude-metadata-delete repo-name branch-name)
                       ;; Stop monitor if no more workspaces
                       (when (null (claude-workspace-list))
                         (claude-monitor-stop))
                       (message "Cleanup complete: %s" workspace-name)))))))))

;;; Cleanup Progress Tracking

(defun claude--init-cleanup-progress (repo-name branch-name)
  "Initialize cleanup progress tracking for REPO-NAME/BRANCH-NAME."
  (let* ((metadata (claude-metadata-read repo-name branch-name))
         (updated (plist-put (copy-sequence metadata)
                             :cleanup_progress
                             (list :started_at (claude--timestamp)))))
    (claude-metadata-write repo-name branch-name updated)))

(defun claude--mark-cleanup-step (repo-name branch-name step)
  "Mark cleanup STEP as complete for REPO-NAME/BRANCH-NAME."
  (let* ((metadata (claude-metadata-read repo-name branch-name))
         (progress (plist-get metadata :cleanup_progress))
         (updated-progress (plist-put progress step t))
         (updated (plist-put (copy-sequence metadata)
                             :cleanup_progress updated-progress)))
    (claude-metadata-write repo-name branch-name updated)))

;;; Force Cleanup for Broken/Stuck Workspaces

;;;###autoload
(defun claude-force-cleanup (workspace-name)
  "Force cleanup a broken or stuck workspace.
WORKSPACE-NAME can be repo:branch format."
  (interactive
   (list (completing-read "Force cleanup workspace: "
                          (mapcar (lambda (ws)
                                    (claude--workspace-name (car ws) (cdr ws)))
                                  (append (claude--list-workspaces-by-status "broken")
                                          (claude--list-workspaces-by-status "stuck")
                                          (claude--list-workspaces-by-status "failed"))))))
  (let ((parsed (claude--parse-workspace-name workspace-name)))
    (if parsed
        (when (yes-or-no-p (format "Force cleanup %s? This may lose data. " workspace-name))
          (let* ((repo-name (car parsed))
                 (branch-name (cdr parsed))
                 (metadata (claude-metadata-read repo-name branch-name))
                 (worktree-path (plist-get metadata :worktree_path))
                 (parent-repo (plist-get metadata :parent_repo)))
            ;; Kill buffers
            (claude--kill-workspace-buffers repo-name branch-name)
            ;; Kill Doom workspace
            (ignore-errors (+workspace-kill workspace-name))
            ;; Force remove worktree
            (when worktree-path
              (claude-worktree-remove-force repo-name branch-name))
            ;; Delete branch
            (when parent-repo
              (ignore-errors (claude-git-delete-branch parent-repo branch-name)))
            ;; Delete metadata
            (claude-metadata-delete repo-name branch-name)
            ;; Stop monitor if no more workspaces
            (when (null (claude-workspace-list))
              (claude-monitor-stop))
            (message "Force cleaned up %s" workspace-name)))
      (user-error "Invalid workspace name: %s" workspace-name))))

(provide 'claude-cleanup)
;;; claude-cleanup.el ends here
