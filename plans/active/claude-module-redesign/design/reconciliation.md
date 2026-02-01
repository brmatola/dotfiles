# Claude Module: Reconciliation

Last updated: 2026-02-01

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
  (let ((path (alist-get 'worktree_path metadata)))
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
         (dir (or (alist-get 'worktree_path metadata)
                  (alist-get 'parent_repo metadata))))
    (claude--create-vterm-in-dir name dir)
    (claude--send-command name (format "cd %s && claude" dir))))
```

## Repair Strategy: Moderate

| Component | Missing Action | Rationale |
|-----------|---------------|-----------|
| Git worktree | Mark broken | Contains code state, can't safely recreate |
| Doom workspace | Auto-repair | Pure UI, safe to recreate |
| vterm buffer | Auto-repair | Session can be restarted |

## Event-Driven Updates

```elisp
(defvar claude-state-change-hook nil
  "Hook run when any workspace state changes.
Called with (workspace-name old-status new-status).")

;; Components subscribe to state changes
(add-hook 'claude-state-change-hook #'claude-dashboard--on-state-change)
(add-hook 'claude-state-change-hook #'claude-modeline--on-state-change)

;; Attention changes also fire the hook
(defun claude-monitor--attention-changed (workspace-name needs-attention)
  "Called when attention state changes."
  (run-hook-with-args 'claude-state-change-hook
                      workspace-name
                      (if needs-attention 'attention 'no-attention)
                      nil))
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
