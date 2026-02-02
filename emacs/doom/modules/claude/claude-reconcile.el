;;; claude-reconcile.el --- Reconciliation layer -*- lexical-binding: t; -*-

;;; Commentary:
;; Reconciliation layer that ensures git worktrees, Doom workspaces, and vterm
;; buffers stay in sync with the metadata source of truth.
;;
;; Reconciliation runs at:
;; - Emacs startup (all workspaces)
;; - Navigation/switch (target workspace)
;; - Dashboard open/refresh (all workspaces)
;; - State change hook (affected workspace)

;;; Code:

(require 'claude-state)
(require 'claude-vterm)

;; Forward declarations for Doom workspace functions
(declare-function +workspace-exists-p "~/.config/emacs/modules/ui/workspaces/autoload/workspaces")
(declare-function +workspace/new "~/.config/emacs/modules/ui/workspaces/autoload/workspaces")
(declare-function +workspace/switch-to "~/.config/emacs/modules/ui/workspaces/autoload/workspaces")
(declare-function +workspace-kill "~/.config/emacs/modules/ui/workspaces/autoload/workspaces")

;;; Customization

(defcustom claude-creation-timeout 120
  "Seconds before creation is considered stalled.
Two minutes allows for slow git operations on large repos."
  :type 'integer
  :group 'claude-workflow)

(defcustom claude-failed-cleanup-hours 24
  "Hours after which failed workspaces are auto-cleaned."
  :type 'integer
  :group 'claude-workflow)

;;; Component Checks

(defun claude--check-worktree (metadata)
  "Return t if worktree exists and is valid.
Home workspaces (no worktree) always return t."
  (let ((path (plist-get metadata :worktree_path)))
    (if (null path)
        t  ; Home workspace - no worktree needed
      (and (file-directory-p path)
           (file-exists-p (expand-file-name ".git" path))))))

(defun claude--check-doom-workspace (metadata)
  "Return t if Doom workspace exists."
  (let* ((repo-name (plist-get metadata :repo_name))
         (branch-name (plist-get metadata :branch_name))
         (name (claude--workspace-name repo-name branch-name)))
    (+workspace-exists-p name)))

(defun claude--check-vterm-buffer (metadata)
  "Return t if Claude vterm buffer exists."
  (let* ((repo-name (plist-get metadata :repo_name))
         (branch-name (plist-get metadata :branch_name))
         (name (claude--buffer-name repo-name branch-name)))
    (buffer-live-p (get-buffer name))))

;;; Repair Functions

(defun claude--repair-doom-workspace (metadata)
  "Recreate Doom workspace from metadata.
Returns t on success."
  (let* ((repo-name (plist-get metadata :repo_name))
         (branch-name (plist-get metadata :branch_name))
         (name (claude--workspace-name repo-name branch-name)))
    (condition-case err
        (progn
          (+workspace/new name)
          t)
      (error
       (message "Failed to repair Doom workspace %s: %s" name err)
       nil))))

(defun claude--repair-vterm-buffer (metadata)
  "Recreate vterm buffer and start Claude session.
Note: This loses the previous session context.
Returns t on success."
  (let* ((repo-name (plist-get metadata :repo_name))
         (branch-name (plist-get metadata :branch_name))
         (workspace-name (claude--workspace-name repo-name branch-name))
         (buffer-name (claude--buffer-name repo-name branch-name))
         (dir (or (plist-get metadata :worktree_path)
                  (plist-get metadata :parent_repo))))
    (condition-case err
        (progn
          ;; Clear any stale attention state
          (when (boundp 'claude-monitor--attention-state)
            (remhash workspace-name claude-monitor--attention-state))
          ;; Create new buffer
          (claude--create-vterm-in-dir buffer-name dir)
          (claude--send-command buffer-name "claude")
          ;; Notify user
          (message "Claude session restarted in %s (previous context lost)" workspace-name)
          t)
      (error
       (message "Failed to repair vterm buffer for %s: %s" workspace-name err)
       nil))))

;;; Main Reconciliation Logic

(defun claude--reconcile (metadata)
  "Reconcile workspace state with metadata.
Returns the resulting status symbol (active, broken, etc.)."
  (catch 'claude--reconcile-early
    (let* ((repo-name (plist-get metadata :repo_name))
           (branch-name (plist-get metadata :branch_name))
           (status-str (plist-get metadata :status))
           (status (and status-str (intern status-str))))
      ;; Transient states - don't interfere
      (when (memq status '(creating closing failed))
        (throw 'claude--reconcile-early status))

      ;; Check all components
      (let ((worktree-ok (claude--check-worktree metadata))
            (doom-ok (claude--check-doom-workspace metadata))
            (vterm-ok (claude--check-vterm-buffer metadata)))
        (cond
         ;; All healthy
         ((and worktree-ok doom-ok vterm-ok)
          (when (eq status 'broken)
            ;; Was broken, now healthy - update to active
            (claude--update-status repo-name branch-name "active"))
          'active)

         ;; Worktree missing - can't repair, mark broken
         ((not worktree-ok)
          (unless (eq status 'broken)
            (claude--update-status repo-name branch-name "broken")
            (message "Claude workspace %s:%s marked broken (worktree missing)"
                     repo-name branch-name))
          'broken)

         ;; Worktree exists but UI is missing - repair silently
         (t
          (when (not doom-ok)
            (claude--repair-doom-workspace metadata))
          (when (not vterm-ok)
            (claude--repair-vterm-buffer metadata))
          ;; Re-check after repair
          (if (and (claude--check-doom-workspace metadata)
                   (claude--check-vterm-buffer metadata))
              (progn
                (when (not (eq status 'active))
                  (claude--update-status repo-name branch-name "active"))
                'active)
            'broken)))))))

(defun claude--reconcile-safe (metadata)
  "Reconcile with error handling.
Returns the resulting status or `error' on failure."
  (condition-case err
      (claude--reconcile metadata)
    (error
     (message "Claude reconcile error for %s:%s: %s"
              (plist-get metadata :repo_name)
              (plist-get metadata :branch_name)
              (error-message-string err))
     'error)))

;;; Stalled Creation Detection

(defun claude--check-stalled-creations ()
  "Handle workspaces stuck in creating state."
  (dolist (ws (claude--list-workspaces-by-status "creating"))
    (let* ((repo-name (car ws))
           (branch-name (cdr ws))
           (metadata (claude-metadata-read repo-name branch-name))
           (created-at (plist-get metadata :created_at)))
      (when created-at
        (let ((elapsed (float-time (time-subtract nil (date-to-time created-at)))))
          (when (> elapsed claude-creation-timeout)
            ;; Check if worktree actually exists
            (if (and (plist-get metadata :worktree_path)
                     (file-directory-p (plist-get metadata :worktree_path)))
                ;; Worktree exists - creation probably succeeded
                (progn
                  (claude--update-status repo-name branch-name "active")
                  (message "Stalled workspace %s:%s marked active (worktree exists)"
                           repo-name branch-name))
              ;; No worktree - mark failed
              (claude--update-status repo-name branch-name "failed")
              (message "Stalled workspace %s:%s marked failed (creation timeout)"
                       repo-name branch-name))))))))

;;; Partial Cleanup Recovery

(defun claude--recover-partial-cleanup (repo-name branch-name metadata)
  "Resume cleanup from where it left off.
Called when workspace is in closing state with cleanup_progress."
  (let ((progress (plist-get metadata :cleanup_progress)))
    (if (not progress)
        ;; No progress tracking - mark stuck for manual intervention
        (progn
          (claude--update-status repo-name branch-name "stuck")
          nil)
      ;; Has progress - try to resume
      (condition-case err
        (progn
          ;; Resume from last incomplete step
          (unless (plist-get progress :buffers_killed)
            (claude--kill-workspace-buffers repo-name branch-name)
            (claude--update-cleanup-progress repo-name branch-name :buffers_killed t))

          (unless (plist-get progress :workspace_removed)
            (let ((ws-name (claude--workspace-name repo-name branch-name)))
              (when (+workspace-exists-p ws-name)
                (+workspace-kill ws-name)))
            (claude--update-cleanup-progress repo-name branch-name :workspace_removed t))

          (unless (plist-get progress :worktree_removed)
            (let ((worktree-path (plist-get metadata :worktree_path)))
              (when (and worktree-path (file-directory-p worktree-path))
                (require 'claude-worktree)
                (claude-worktree-remove repo-name branch-name)))
            (claude--update-cleanup-progress repo-name branch-name :worktree_removed t))

          ;; All done - delete metadata
          (claude-metadata-delete repo-name branch-name)
          (message "Recovered cleanup for %s:%s" repo-name branch-name)
          t)

        (error
         ;; Cleanup failed again - mark stuck
         (message "Cleanup recovery failed for %s:%s: %s"
                  repo-name branch-name (error-message-string err))
         (claude--update-status repo-name branch-name "stuck")
         nil)))))

(defun claude--update-cleanup-progress (repo-name branch-name key value)
  "Update cleanup progress KEY to VALUE in metadata."
  (let* ((metadata (claude-metadata-read repo-name branch-name))
         (progress (or (plist-get metadata :cleanup_progress)
                       (list :started_at (claude--timestamp))))
         (updated-progress (plist-put progress key value))
         (updated-metadata (plist-put (copy-sequence metadata)
                                       :cleanup_progress updated-progress)))
    (claude-metadata-write repo-name branch-name updated-metadata)))

;;; In-Flight Workspace Recovery

(defun claude--recover-in-flight-workspaces ()
  "Handle workspaces stuck in transient states at startup."
  (dolist (ws (claude--list-all-workspaces))
    (let* ((repo-name (car ws))
           (branch-name (cdr ws))
           (metadata (claude-metadata-read repo-name branch-name))
           (status (plist-get metadata :status)))
      (cond
       ((equal status "creating")
        ;; Already handled by check-stalled-creations
        nil)

       ((equal status "closing")
        (if (plist-get metadata :cleanup_progress)
            (claude--recover-partial-cleanup repo-name branch-name metadata)
          ;; No progress - mark stuck for manual intervention
          (claude--update-status repo-name branch-name "stuck")))))))

;;; Stale Failed Workspace Cleanup

(defun claude--cleanup-stale-failed ()
  "Auto-cleanup failed workspaces older than threshold."
  (dolist (ws (claude--list-workspaces-by-status "failed"))
    (let* ((repo-name (car ws))
           (branch-name (cdr ws))
           (metadata (claude-metadata-read repo-name branch-name))
           (created-at (plist-get metadata :created_at)))
      (when created-at
        (let ((hours-old (/ (float-time (time-subtract nil (date-to-time created-at)))
                            3600.0)))
          (when (> hours-old claude-failed-cleanup-hours)
            (message "Auto-cleaning stale failed workspace: %s:%s" repo-name branch-name)
            (claude--cleanup-failed-workspace repo-name branch-name)))))))

(defun claude--cleanup-failed-workspace (repo-name branch-name)
  "Clean up a failed workspace - delete metadata and any partial resources."
  (let ((metadata (claude-metadata-read repo-name branch-name)))
    ;; Kill any buffers that might exist
    (when-let ((buf (get-buffer (claude--buffer-name repo-name branch-name))))
      (let ((kill-buffer-query-functions nil))
        (kill-buffer buf)))
    ;; Kill terminal buffers
    (dolist (buf (claude--terminal-buffers repo-name branch-name))
      (let ((kill-buffer-query-functions nil))
        (kill-buffer buf)))
    ;; Remove Doom workspace if it exists
    (let ((ws-name (claude--workspace-name repo-name branch-name)))
      (when (+workspace-exists-p ws-name)
        (+workspace-kill ws-name)))
    ;; Remove worktree if it exists (force, since state is unknown)
    (when-let ((path (plist-get metadata :worktree_path)))
      (when (file-directory-p path)
        (ignore-errors
          (let ((default-directory (or (plist-get metadata :parent_repo)
                                        "~")))
            (shell-command-to-string
             (format "git worktree remove --force %s 2>&1"
                     (shell-quote-argument path)))))))
    ;; Delete metadata
    (claude-metadata-delete repo-name branch-name)))

;;; Startup Reconciliation

(defun claude--startup-reconcile ()
  "Reconcile all workspaces at Emacs startup."
  ;; First check for stalled creations
  (claude--check-stalled-creations)
  ;; Then recover any in-flight operations
  (claude--recover-in-flight-workspaces)
  ;; Then reconcile all active workspaces
  (dolist (metadata (claude--list-all-metadata))
    (let ((status (plist-get metadata :status)))
      (when (equal status "active")
        (let ((result (claude--reconcile-safe metadata)))
          (when (eq result 'broken)
            (message "Claude workspace %s:%s is broken (worktree missing)"
                     (plist-get metadata :repo_name)
                     (plist-get metadata :branch_name))))))))

;; Hook for startup reconciliation
;; Note: This will be added in claude.el to ensure proper load order

;;; Reconcile All

(defun claude--reconcile-all ()
  "Reconcile all workspaces.
Used by dashboard refresh."
  (dolist (metadata (claude--list-all-metadata))
    (claude--reconcile-safe metadata)))

;;; Get Workspace Status with Reconciliation

(defun claude--get-workspace-status (repo-name branch-name)
  "Get current status of workspace, running reconciliation if needed.
Returns status symbol or nil if workspace doesn't exist."
  (when-let ((metadata (claude-metadata-read repo-name branch-name)))
    (let ((status-str (plist-get metadata :status)))
      (if (equal status-str "active")
          ;; For active workspaces, reconcile to ensure status is accurate
          (claude--reconcile-safe metadata)
        (and status-str (intern status-str))))))

(provide 'claude-reconcile)
;;; claude-reconcile.el ends here
