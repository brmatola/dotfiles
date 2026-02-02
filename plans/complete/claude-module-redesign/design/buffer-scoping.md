# Claude Module: Buffer Scoping

Last updated: 2026-02-01

## Problem

Emacs buffers are global by default. Without scoping, buffers from one Claude workspace can appear in another, causing confusion:

- `SPC b b` shows buffers from all workspaces
- Switching to a file might jump you to the wrong workspace
- Closing a workspace might kill buffers from another

## Solution: Doom Workspace Integration

Doom workspaces (`+workspace`) provide buffer isolation. Each workspace maintains its own buffer list.

### How Doom Workspaces Scope Buffers

1. **Buffer association** - Buffers opened while in a workspace are associated with it
2. **Buffer filtering** - `SPC b b` only shows buffers in current workspace
3. **Real buffers** - Special buffers (like vterm) can be explicitly associated

### Our Naming Convention

Buffers are named to identify their workspace:

```
*claude:{repo}:{branch}*     - Primary Claude session
*term:{repo}:{branch}:{n}*   - Extra terminals
```

This provides:
1. **Visual clarity** - Buffer name shows what workspace it belongs to
2. **Programmatic filtering** - Code can find buffers by pattern
3. **Uniqueness** - No conflicts between workspaces

## Implementation

### Associating Buffers with Workspaces

When creating a buffer, ensure it's associated with the current workspace:

```elisp
(defun claude--create-buffer-in-workspace (buffer-name)
  "Create buffer and associate with current Doom workspace."
  (let ((buf (get-buffer-create buffer-name)))
    ;; persp-mode (underlying Doom workspaces) auto-associates
    ;; buffers created while in a workspace, but we can be explicit:
    (when (bound-and-true-p persp-mode)
      (persp-add-buffer buf (get-current-persp) nil nil))
    buf))
```

### Buffer Belongs to Workspace Check

```elisp
(defun claude--buffer-in-workspace-p (buffer workspace-name)
  "Return t if BUFFER belongs to WORKSPACE-NAME."
  (let ((parsed (claude--parse-workspace-name workspace-name)))
    (when parsed
      (let* ((repo-name (car parsed))
             (branch-name (cdr parsed))
             (buf-name (buffer-name buffer)))
        (or
         ;; Claude buffer
         (string= buf-name (claude--buffer-name repo-name branch-name))
         ;; Terminal buffer
         (string-prefix-p (format "*term:%s:%s:" repo-name branch-name) buf-name)
         ;; File in worktree
         (when-let* ((file (buffer-file-name buffer))
                     (metadata (claude-metadata-read repo-name branch-name))
                     (worktree (plist-get metadata :worktree_path)))
           (string-prefix-p (expand-file-name worktree)
                            (expand-file-name file))))))))
```

### Switching Workspaces

When switching to a Claude workspace, Doom automatically:
1. Switches to that workspace's buffer list
2. Restores window configuration
3. Shows only associated buffers in `SPC b b`

We hook in to ensure Claude buffers are properly focused:

```elisp
(defun claude--on-workspace-switch ()
  "Handle switching to a Claude workspace."
  (when-let* ((ws-name (+workspace-current-name))
              (parsed (claude--parse-workspace-name ws-name)))
    (let* ((repo-name (car parsed))
           (branch-name (cdr parsed))
           (claude-buf (claude--buffer-name repo-name branch-name)))
      ;; If Claude buffer exists and we're not already showing it, switch to it
      (when (and (get-buffer claude-buf)
                 (not (string= (buffer-name) claude-buf)))
        ;; Focus the claude buffer in one window
        (when-let ((win (get-buffer-window claude-buf)))
          (select-window win))))))

(add-hook '+workspace-switch-hook #'claude--on-workspace-switch)
```

## File Buffer Isolation

File buffers visiting files in a worktree should be associated with that workspace:

```elisp
(defun claude--associate-file-buffer ()
  "Associate current file buffer with appropriate Claude workspace."
  (when-let ((file (buffer-file-name)))
    (dolist (ws (claude-worktree-list))
      (let* ((repo-name (car ws))
             (branch-name (cdr ws))
             (metadata (claude-metadata-read repo-name branch-name))
             (worktree (plist-get metadata :worktree_path)))
        (when (and worktree
                   (string-prefix-p (expand-file-name worktree)
                                    (expand-file-name file)))
          ;; This file belongs to this workspace
          (let ((ws-name (claude--workspace-name repo-name branch-name)))
            (when (+workspace-exists-p ws-name)
              (persp-add-buffer (current-buffer)
                                (persp-get-by-name ws-name)
                                nil nil))))))))

(add-hook 'find-file-hook #'claude--associate-file-buffer)
```

## Dashboard Buffer List

The dashboard shows all workspaces, but when selecting one to switch to, we need to consider buffer state:

```elisp
(defun claude-dashboard--workspace-buffer-count (repo-name branch-name)
  "Count buffers associated with workspace."
  (let ((count 0))
    (dolist (buf (buffer-list))
      (when (claude--buffer-in-workspace-p
             buf
             (claude--workspace-name repo-name branch-name))
        (setq count (1+ count))))
    count))
```

## Cleanup: Kill Only Workspace Buffers

When closing a workspace, kill buffers in this order:

1. **vterm buffers** (Claude and terminals) — killed explicitly by name pattern
2. **File buffers** — killed if visiting files in the worktree path
3. **Doom workspace** — `+workspace-kill` handles any remaining associated buffers

```elisp
(defun claude--kill-workspace-buffers (repo-name branch-name)
  "Kill all buffers belonging to this workspace.
Kills vterm buffers and file buffers in the worktree.
Does NOT kill buffers that happen to be associated with the Doom workspace
but aren't Claude-specific - those are handled by +workspace-kill."
  (let ((ws-name (claude--workspace-name repo-name branch-name))
        (metadata (claude-metadata-read repo-name branch-name))
        (worktree-path (plist-get metadata :worktree_path)))
    ;; 1. Kill Claude buffer
    (when-let ((buf (get-buffer (claude--buffer-name repo-name branch-name))))
      (kill-buffer buf))
    ;; 2. Kill terminal buffers
    (dolist (buf (buffer-list))
      (when (string-prefix-p (format "*term:%s:%s:" repo-name branch-name)
                             (buffer-name buf))
        (kill-buffer buf)))
    ;; 3. Kill file buffers visiting worktree files
    (when worktree-path
      (let ((expanded-path (expand-file-name worktree-path)))
        (dolist (buf (buffer-list))
          (when-let ((file (buffer-file-name buf)))
            (when (string-prefix-p expanded-path (expand-file-name file))
              (kill-buffer buf))))))))
```

**Important:** We explicitly kill file buffers visiting the worktree because:
- `+workspace-kill` only removes buffer association, doesn't kill buffers
- Leaving stale file buffers causes confusion when worktree is deleted
- Modified buffers will prompt for save (standard Emacs behavior)

## Edge Cases

### Buffer Opened from Dashboard

If user opens a file from dashboard (which is outside any Claude workspace), the buffer goes to default workspace. This is fine - when they switch to the Claude workspace, they can open the file again and it will be associated.

### Buffer Shared Between Workspaces

This shouldn't happen with proper scoping, but if it does:
- Each workspace has its own window configuration
- Buffer might appear in multiple workspaces' buffer lists
- Closing one workspace doesn't kill shared buffers
- Our cleanup code only kills buffers that match naming patterns or are in worktree paths

### Magit Buffers

Magit buffers follow their own naming convention (`*magit: repo*`). They're associated with whatever workspace was current when they were created. When opening magit from a Claude workspace, it's automatically associated.

```elisp
(defun claude-magit-status ()
  "Open magit status in the current workspace's directory."
  (interactive)
  (let* ((ws-name (+workspace-current-name))
         (parsed (claude--parse-workspace-name ws-name)))
    (if parsed
        (let* ((repo-name (car parsed))
               (branch-name (cdr parsed))
               (dir (claude--workspace-directory repo-name branch-name)))
          (magit-status dir))
      ;; Not in Claude workspace - use default behavior
      (call-interactively #'magit-status))))
```

## Summary

Buffer scoping is achieved through:

1. **Doom workspaces** - Provide native buffer list isolation
2. **Naming conventions** - Enable programmatic identification
3. **find-file hook** - Associates file buffers with correct workspace
4. **Cleanup filters** - Only kill buffers belonging to closing workspace

This combination prevents buffer bleed between workspaces while maintaining standard Emacs/Doom behavior.
