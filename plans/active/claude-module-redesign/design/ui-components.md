# Claude Module: UI Components

Last updated: 2026-02-01

## Dashboard

### Purpose

Central view of all Claude workspaces with their status. Reactive - updates automatically on state changes.

### Layout

```
┌─────────────────────────────────────────────────────────────┐
│ Claude Workspaces                              [g] refresh  │
├─────────────────────────────────────────────────────────────┤
│                                                             │
│  ⌂ dotfiles:__home__              ○ active                  │
│  > dotfiles:feature-auth          ● attention               │
│    dotfiles:fix-cleanup           ○ active                  │
│    other-repo:refactor            ⚠ broken                  │
│    other-repo:experiment          ◌ creating                │
│                                                             │
├─────────────────────────────────────────────────────────────┤
│ j/k:navigate  RET:switch  x:close  c:create  g:refresh  q:quit
└─────────────────────────────────────────────────────────────┘
```

### Visual Elements

| Symbol | Meaning |
|--------|---------|
| `⌂` | Home workspace |
| `>` | Currently selected |
| `○` | Active (healthy) |
| `●` | Needs attention |
| `◌` | Creating (in progress) |
| `⚠` | Broken (component missing) |
| `⊘` | Stuck (cleanup failed) |
| `✗` | Failed (creation failed) |

### Keybindings

| Key | Action |
|-----|--------|
| `j` / `n` | Move down |
| `k` / `p` | Move up |
| `RET` | Switch to workspace |
| `c` | Create new workspace |
| `x` | Close workspace |
| `g` | Refresh (manual) |
| `/` | Filter by repo |
| `q` | Quit dashboard |

### Auto-Refresh

Dashboard refreshes automatically when:
- `claude-state-change-hook` fires
- Monitor detects attention change
- Workspace created or closed

```elisp
(defun claude-dashboard--setup-auto-refresh ()
  "Set up automatic refresh on state changes."
  (add-hook 'claude-state-change-hook
            #'claude-dashboard--refresh-if-visible nil t))

(defun claude-dashboard--refresh-if-visible ()
  "Refresh dashboard if it's currently visible."
  (when-let ((buf (get-buffer "*claude-dashboard*")))
    (when (get-buffer-window buf)
      (claude-dashboard--render))))
```

### Rendering

```elisp
(defun claude-dashboard--render ()
  "Render the dashboard buffer."
  (let ((inhibit-read-only t)
        (workspaces (claude--list-all-workspaces-with-status)))
    (erase-buffer)
    (insert (propertize "Claude Workspaces" 'face 'claude-dashboard-title))
    (insert "                              [g] refresh\n")
    (insert (make-string 61 ?─) "\n\n")

    (if (null workspaces)
        (insert "  No workspaces. Press 'c' to create one.\n")
      (dolist (ws workspaces)
        (claude-dashboard--render-workspace ws)))

    (insert "\n" (make-string 61 ?─) "\n")
    (insert "j/k:navigate  RET:switch  x:close  c:create  g:refresh  q:quit")))

(defun claude-dashboard--render-workspace (ws)
  "Render a single workspace entry."
  (let* ((name (alist-get 'name ws))
         (status (alist-get 'status ws))
         (attention (alist-get 'attention ws))
         (home-p (alist-get 'home ws))
         (current-p (alist-get 'current ws)))
    (insert (if home-p "  ⌂ " (if current-p "  > " "    ")))
    (insert (propertize (format "%-30s" name) 'face 'claude-dashboard-workspace))
    (insert (claude-dashboard--status-indicator status attention))
    (insert "\n")))
```

## Modeline Segment

### Purpose

Global indicator showing if any Claude workspace needs attention.

### States

| Display | Condition |
|---------|-----------|
| (hidden) | No Claude workspaces exist |
| `○ Claude` | Workspaces exist, none need attention |
| `● Claude` | At least one needs attention |
| `⚠ Claude` | At least one is broken/stuck |

### Implementation

```elisp
(defvar claude-modeline--state 'none
  "Current modeline state: none, ok, attention, broken.")

(defun claude-modeline-segment ()
  "Return modeline segment string."
  (pcase claude-modeline--state
    ('none "")
    ('ok (propertize "○ Claude" 'face 'claude-modeline-ok))
    ('attention (propertize "● Claude" 'face 'claude-modeline-attention))
    ('broken (propertize "⚠ Claude" 'face 'claude-modeline-broken))))

(defun claude-modeline--update ()
  "Update modeline state from current workspaces."
  (let ((workspaces (claude--list-all-workspaces-with-status)))
    (setq claude-modeline--state
          (cond
           ((null workspaces) 'none)
           ((cl-some (lambda (ws) (alist-get 'attention ws)) workspaces) 'attention)
           ((cl-some (lambda (ws) (memq (alist-get 'status ws) '(broken stuck))) workspaces) 'broken)
           (t 'ok)))
    (force-mode-line-update t)))

;; Subscribe to state changes
(add-hook 'claude-state-change-hook #'claude-modeline--update)
```

### Click Behavior

Click on modeline → jump to first workspace needing attention, or open dashboard if multiple.

```elisp
(defvar claude-modeline-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1] #'claude-modeline--click)
    map))

(defun claude-modeline--click ()
  "Handle click on modeline segment."
  (interactive)
  (let ((needy (claude--workspaces-needing-attention)))
    (pcase (length needy)
      (0 (claude-dashboard))
      (1 (claude-workspace-switch (car needy)))
      (_ (claude-dashboard)))))
```

## Status Buffer (Cleanup)

### Purpose

Shows workspace state during cleanup, presents options.

### Layout

```
┌─────────────────────────────────────────────────────────────┐
│ Close Workspace: dotfiles:feature-auth                      │
├─────────────────────────────────────────────────────────────┤
│                                                             │
│ Branch:    feature-auth → main                              │
│ Commits:   3 ahead, 0 behind                                │
│ Uncommitted changes: none                                   │
│                                                             │
│ Commits to merge:                                           │
│   abc1234 Add authentication middleware                     │
│   def5678 Add login endpoint                                │
│   ghi9012 Add logout endpoint                               │
│                                                             │
├─────────────────────────────────────────────────────────────┤
│ [m] Merge & cleanup  [p] Create PR  [d] Delete  [c] Cancel  │
└─────────────────────────────────────────────────────────────┘
```

### Keybindings

| Key | Action |
|-----|--------|
| `m` | Merge to parent and cleanup |
| `p` | Create PR (opens browser via `gh pr create`) |
| `d` | Delete without merging (confirms first) |
| `c` / `q` | Cancel |
| `v` | View full diff in magit |
| `g` | Open magit status |

## Attention Patterns

Patterns that trigger "needs attention" state:

```elisp
(defcustom claude-attention-patterns
  '(;; Permission prompts
    "Allow .* to \\(run\\|read\\|write\\|edit\\)"
    "needs your permission"

    ;; Yes/no prompts
    "\\[y/n\\]"
    "\\[Y/n\\]"
    "\\[yes/no\\]"

    ;; Waiting for input (prompt at end of buffer)
    "^> *$"
    "^❯ *$"

    ;; Claude-specific
    "Would you like"
    "Do you want"
    "Should I")
  "Patterns indicating Claude needs user attention.")
```

## Faces (Colors/Styles)

```elisp
(defface claude-dashboard-title
  '((t :inherit bold :height 1.2))
  "Face for dashboard title.")

(defface claude-dashboard-workspace
  '((t :inherit default))
  "Face for workspace names.")

(defface claude-modeline-ok
  '((t :inherit success))
  "Face for modeline when all workspaces ok.")

(defface claude-modeline-attention
  '((t :inherit warning :weight bold))
  "Face for modeline when attention needed.")

(defface claude-modeline-broken
  '((t :inherit error))
  "Face for modeline when workspace broken.")

(defface claude-status-active
  '((t :inherit success))
  "Face for active status.")

(defface claude-status-attention
  '((t :inherit warning :weight bold))
  "Face for attention status.")

(defface claude-status-broken
  '((t :inherit error))
  "Face for broken status.")
```
