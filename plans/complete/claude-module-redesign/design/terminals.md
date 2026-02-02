# Claude Module: Terminal Management

Last updated: 2026-02-01

## Overview

Each Claude workspace has one primary Claude buffer and can have multiple extra terminal buffers for running commands, builds, tests, etc.

## Buffer Naming Scheme

| Buffer Type | Name Pattern | Example |
|-------------|--------------|---------|
| Claude session | `*claude:{repo}:{branch}*` | `*claude:dotfiles:feature-auth*` |
| Extra terminal | `*term:{repo}:{branch}:{n}*` | `*term:dotfiles:feature-auth:1*` |

Where `{n}` is an incrementing number starting at 1.

## Terminal Tracking

Terminals are tracked ephemerally (not in metadata) since they're easily recreated:

```elisp
(defun claude--terminal-buffers (repo-name branch-name)
  "Return list of extra terminal buffers for workspace."
  (let ((prefix (format "*term:%s:%s:" repo-name branch-name))
        (result nil))
    (dolist (buf (buffer-list))
      (when (string-prefix-p prefix (buffer-name buf))
        (push buf result)))
    (nreverse result)))

(defun claude--terminal-count (repo-name branch-name)
  "Return count of extra terminals for workspace."
  (length (claude--terminal-buffers repo-name branch-name)))
```

## Gap Reuse

When creating a new terminal, find the lowest unused number:

```elisp
(defun claude--next-terminal-number (repo-name branch-name)
  "Find the next available terminal number.
Reuses gaps in the sequence (e.g., if 1,3 exist, returns 2)."
  (let* ((buffers (claude--terminal-buffers repo-name branch-name))
         (numbers (mapcar (lambda (buf)
                            (when (string-match ":\\([0-9]+\\)\\*$" (buffer-name buf))
                              (string-to-number (match-string 1 (buffer-name buf)))))
                          buffers))
         (numbers (delq nil numbers))
         (n 1))
    ;; Find first gap
    (while (memq n numbers)
      (setq n (1+ n)))
    n))
```

### Example

```
State: *term:dotfiles:feature:1*, *term:dotfiles:feature:3* exist

User runs SPC C t
  → claude--next-terminal-number returns 2
  → Creates *term:dotfiles:feature:2*

User closes terminal 1
State: *term:dotfiles:feature:2*, *term:dotfiles:feature:3*

User runs SPC C t
  → claude--next-terminal-number returns 1
  → Creates *term:dotfiles:feature:1*
```

## Creating Terminals

```elisp
(defun claude-new-terminal ()
  "Create a new terminal in the current Claude workspace."
  (interactive)
  (let* ((ws-name (+workspace-current-name))
         (parsed (claude--parse-workspace-name ws-name)))
    (unless parsed
      (user-error "Not in a Claude workspace"))
    (let* ((repo-name (car parsed))
           (branch-name (cdr parsed))
           (n (claude--next-terminal-number repo-name branch-name))
           (buffer-name (format "*term:%s:%s:%d*" repo-name branch-name n))
           (dir (claude--workspace-directory repo-name branch-name)))
      (claude--create-vterm-in-dir buffer-name dir)
      (message "Created terminal %d" n))))

(defun claude--workspace-directory (repo-name branch-name)
  "Get working directory for workspace."
  (let ((metadata (claude-metadata-read repo-name branch-name)))
    (or (plist-get metadata :worktree_path)
        (plist-get metadata :parent_repo))))
```

## Cleanup

When closing a workspace, all associated terminals are killed:

```elisp
(defun claude--kill-workspace-buffers (repo-name branch-name)
  "Kill all buffers associated with workspace."
  ;; Kill Claude buffer
  (when-let ((buf (get-buffer (claude--buffer-name repo-name branch-name))))
    (kill-buffer buf))

  ;; Kill all terminal buffers
  (dolist (buf (claude--terminal-buffers repo-name branch-name))
    (kill-buffer buf))

  ;; Kill file buffers visiting files in worktree
  (when-let* ((metadata (claude-metadata-read repo-name branch-name))
              (worktree-path (plist-get metadata :worktree_path)))
    (dolist (buf (buffer-list))
      (when-let ((file (buffer-file-name buf)))
        (when (string-prefix-p (expand-file-name worktree-path) (expand-file-name file))
          (kill-buffer buf))))))
```

## vterm Creation Helper

```elisp
(defun claude--create-vterm-in-dir (buffer-name dir)
  "Create vterm buffer with name BUFFER-NAME in directory DIR."
  (let ((default-directory (expand-file-name dir)))
    (with-current-buffer (vterm buffer-name)
      ;; Ensure we're in the right directory
      (vterm-send-string (format "cd %s && clear\n" (shell-quote-argument dir)))
      (current-buffer))))

(defun claude--send-command (buffer-name command)
  "Send COMMAND to vterm BUFFER-NAME."
  (with-current-buffer buffer-name
    (vterm-send-string (concat command "\n"))))
```

## Workspace Scoping

Terminals are scoped to their workspace through:

1. **Naming convention** - Buffer name includes repo:branch
2. **Doom workspace buffer list** - Each Doom workspace has its own buffer list
3. **Directory** - Each terminal starts in the workspace's working directory

When switching Doom workspaces, only buffers belonging to that workspace appear in buffer lists and switching commands.

## Jump to Claude Buffer

```elisp
(defun claude-jump-to-buffer ()
  "Jump to the Claude buffer in the current workspace."
  (interactive)
  (let* ((ws-name (+workspace-current-name))
         (parsed (claude--parse-workspace-name ws-name)))
    (unless parsed
      (user-error "Not in a Claude workspace"))
    (let* ((repo-name (car parsed))
           (branch-name (cdr parsed))
           (buffer-name (claude--buffer-name repo-name branch-name)))
      (if-let ((buf (get-buffer buffer-name)))
          (switch-to-buffer buf)
        (user-error "Claude buffer not found: %s" buffer-name)))))
```

## Terminal Limits (Optional)

Optionally limit max terminals per workspace:

```elisp
(defcustom claude-max-terminals-per-workspace 10
  "Maximum extra terminals per workspace, or nil for unlimited."
  :type '(choice (const nil) integer)
  :group 'claude-workflow)

(defun claude-new-terminal ()
  "Create a new terminal in the current Claude workspace."
  (interactive)
  ;; ... parse workspace name ...
  (when (and claude-max-terminals-per-workspace
             (>= (claude--terminal-count repo-name branch-name)
                 claude-max-terminals-per-workspace))
    (user-error "Maximum terminals (%d) reached for this workspace"
                claude-max-terminals-per-workspace))
  ;; ... create terminal ...
  )
```
