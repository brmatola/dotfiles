# Claude Module: Reconciliation

Last updated: 2026-02-01 (reviewed)

## Purpose

The reconciliation layer ensures the three systems (git worktrees, Doom workspaces, vterm buffers) stay in sync with the metadata source of truth.

## When Reconciliation Runs

| Event | Scope | Purpose |
|-------|-------|---------|
| Emacs startup | All workspaces | Detect crashed sessions, repair UI |
| Navigation (switch) | Target workspace | Ensure healthy before switching |
| Dashboard open/refresh | All workspaces | Show accurate state |
| State change hook | Affected workspace | Propagate changes |

## Reconciliation Logic

```
reconcile(workspace-metadata):
    if metadata.status in (failed, creating, closing):
        # Transient states - don't interfere
        return metadata.status

    checks = {
        worktree: check_worktree(metadata),
        doom: check_doom_workspace(metadata),
        vterm: check_vterm_buffer(metadata)
    }

    if all(checks.values()):
        return "active"

    if not checks.worktree:
        # Can't repair git state - mark broken
        update_metadata(status="broken")
        return "broken"

    # Worktree exists but UI is missing - repair
    if not checks.doom:
        repair_doom_workspace(metadata)

    if not checks.vterm:
        repair_vterm_buffer(metadata)

    return "active"
```

## Component Checks

### Git Worktree Check

```elisp
(defun claude--check-worktree (metadata)
  "Return t if worktree exists and is valid."
  (let ((path (plist-get metadata :worktree_path)))
    (if (null path)
        t  ; Home workspace - no worktree needed
      (and (file-directory-p path)
           (file-exists-p (expand-file-name ".git" path))))))
```

### Doom Workspace Check

```elisp
(defun claude--check-doom-workspace (metadata)
  "Return t if Doom workspace exists."
  (let ((name (claude--workspace-name metadata)))
    (+workspace-exists-p name)))
```

### vterm Buffer Check

```elisp
(defun claude--check-vterm-buffer (metadata)
  "Return t if Claude vterm buffer exists."
  (let ((name (claude--buffer-name metadata)))
    (get-buffer name)))
```

## Repair Actions

### Repair Doom Workspace

```elisp
(defun claude--repair-doom-workspace (metadata)
  "Recreate Doom workspace from metadata."
  (let ((name (claude--workspace-name metadata)))
    (+workspace/new name)
    (+workspace/switch-to name)))
```

### Repair vterm Buffer

```elisp
(defun claude--repair-vterm-buffer (metadata)
  "Recreate vterm buffer and start Claude session."
  (let* ((name (claude--buffer-name metadata))
         (dir (or (plist-get metadata :worktree_path)
                  (plist-get metadata :parent_repo))))
    (claude--create-vterm-in-dir name dir)
    (claude--send-command name (format "cd %s && claude" dir))))
```

## Repair Strategy: Moderate

| Component | Missing Action | Rationale |
|-----------|---------------|-----------|
| Git worktree | Mark broken | Contains code state, can't safely recreate |
| Doom workspace | Auto-repair | Pure UI, safe to recreate |
| vterm buffer | Auto-repair | Session can be restarted |

**Auto-repair is silent** — the reconciler repairs Doom/vterm in the background without user interaction. Status stays `active`.

**Manual repair (`SPC C r`)** — for workspaces already marked `broken`. User invokes after worktree is somehow restored, or to retry after transient failure.

## Event-Driven Updates

```elisp
(defvar claude-state-change-hook nil
  "Hook run when any workspace state changes.
Called with (workspace-name old-status new-status).")

;; Components subscribe to state changes
(add-hook 'claude-state-change-hook #'claude-dashboard--on-state-change)
(add-hook 'claude-state-change-hook #'claude-modeline--on-state-change)

;; Attention changes fire their own hook
(defun claude-monitor--attention-changed (workspace-name needs-attention)
  "Called when attention state changes."
  (run-hook-with-args 'claude-attention-change-hook
                      workspace-name
                      needs-attention))
```

## Reconciliation at Startup

```elisp
(defun claude--startup-reconcile ()
  "Reconcile all workspaces at Emacs startup."
  (dolist (metadata (claude--list-all-metadata))
    (let ((result (claude--reconcile metadata)))
      (when (eq result 'broken)
        (message "Claude workspace %s is broken (worktree missing)"
                 (claude--workspace-name metadata))))))

(add-hook 'doom-after-init-hook #'claude--startup-reconcile)
```

## Error Handling

Reconciliation never throws. All errors are caught and logged:

```elisp
(defun claude--reconcile-safe (metadata)
  "Reconcile with error handling."
  (condition-case err
      (claude--reconcile metadata)
    (error
     (message "Claude reconcile error for %s: %s"
              (claude--workspace-name metadata)
              (error-message-string err))
     'error)))
```

## Transient State Recovery

Transient states (`creating`, `closing`) can become stuck if Emacs crashes. The reconciler handles these specially.

### Stalled Creation

```elisp
(defcustom claude-creation-timeout 120
  "Seconds before creation is considered stalled.
Two minutes allows for slow git operations on large repos."
  :type 'integer
  :group 'claude-workflow)

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
                (claude--update-status repo-name branch-name "active")
              ;; No worktree - mark failed
              (claude--update-status repo-name branch-name "failed"))))))))
```

### Partial Cleanup Recovery

When cleanup is interrupted, `cleanup_progress` tracks what was done:

```elisp
(defun claude--recover-partial-cleanup (repo-name branch-name metadata)
  "Resume cleanup from where it left off."
  (let ((progress (plist-get metadata :cleanup_progress)))
    (unless progress
      ;; No progress tracking - can't safely resume
      (claude--update-status repo-name branch-name "stuck")
      (return))

    (condition-case err
        (progn
          ;; Resume from last incomplete step
          (unless (plist-get progress :buffers_killed)
            (claude--kill-workspace-buffers repo-name branch-name)
            (claude--update-cleanup-progress repo-name branch-name :buffers_killed t))

          (unless (plist-get progress :workspace_removed)
            (when (+workspace-exists-p (claude--workspace-name repo-name branch-name))
              (+workspace-kill (claude--workspace-name repo-name branch-name)))
            (claude--update-cleanup-progress repo-name branch-name :workspace_removed t))

          (unless (plist-get progress :worktree_removed)
            (claude-worktree-remove repo-name branch-name)
            (claude--update-cleanup-progress repo-name branch-name :worktree_removed t))

          (unless (plist-get progress :branch_deleted)
            (let ((parent-repo (plist-get metadata :parent_repo)))
              (claude-git-delete-branch parent-repo branch-name))
            (claude--update-cleanup-progress repo-name branch-name :branch_deleted t))

          ;; All done - delete metadata
          (claude-metadata-delete repo-name branch-name))

      (error
       ;; Cleanup failed again - mark stuck
       (message "Cleanup recovery failed for %s:%s: %s"
                repo-name branch-name (error-message-string err))
       (claude--update-status repo-name branch-name "stuck")))))

(defun claude--recover-in-flight-workspaces ()
  "Handle workspaces in transient states at startup."
  (dolist (ws (claude-worktree-list))
    (let* ((repo-name (car ws))
           (branch-name (cdr ws))
           (metadata (claude-metadata-read repo-name branch-name))
           (status (plist-get metadata :status)))
      (cond
       ((equal status "creating")
        (claude--check-stalled-creations))

       ((equal status "closing")
        (if (plist-get metadata :cleanup_progress)
            (claude--recover-partial-cleanup repo-name branch-name metadata)
          ;; No progress - mark stuck for manual intervention
          (claude--update-status repo-name branch-name "stuck")))))))

;; Run on startup
(add-hook 'doom-after-init-hook #'claude--recover-in-flight-workspaces)
```

### Stale Failed Workspace Cleanup

Failed workspaces older than 24 hours are auto-cleaned at startup:

```elisp
(defcustom claude-failed-cleanup-hours 24
  "Hours after which failed workspaces are auto-cleaned."
  :type 'integer
  :group 'claude-workflow)

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

;; Run after in-flight recovery
(add-hook 'doom-after-init-hook #'claude--cleanup-stale-failed 90)
```
