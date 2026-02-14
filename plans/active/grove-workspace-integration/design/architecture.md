# Grove Workspace Integration: Module Architecture

Last updated: 2026-02-14

## Module Map (After)

```
claude-grove.el          # NEW — grove CLI wrapper (no deps)
    ↓
claude-monitor.el        # MODIFY — attention detection (depends: nothing)
    ↓
claude-dashboard.el      # REWRITE — mission control (depends: claude-grove, claude-monitor)
    ↓
claude.el                # MODIFY — entry point, global keybindings
```

### Removed Modules

| Module | Reason |
|--------|--------|
| `claude-state.el` | State machine + metadata → grove owns this |
| `claude-reconcile.el` | Drift detection → grove owns state, no drift possible |
| `claude-worktree.el` | Git worktree ops → grove owns this |
| `claude-cleanup.el` | Merge/close orchestration → grove owns this |
| `claude-workspace.el` | Doom workspace creation → absorbed into dashboard |
| `claude-vterm.el` | vterm buffer creation → absorbed into dashboard |

### What Remains in Emacs

| Concern | Owner | Why |
|---------|-------|-----|
| Doom workspace create/switch/delete | `claude-dashboard.el` | Doom-specific API |
| vterm buffer create/kill | `claude-dashboard.el` | Emacs-specific |
| Attention detection | `claude-monitor.el` | Reads vterm buffer output |
| Dashboard rendering | `claude-dashboard.el` | Emacs UI |
| Grove CLI calls | `claude-grove.el` | Thin async wrapper |
| Global keybindings | `claude.el` | Doom keybinding API |

## claude-grove.el — Grove CLI Wrapper

Thin layer: shell out, parse JSON, return elisp data. All grove interaction goes through here.

### Interface

```elisp
;; Synchronous (for quick reads)
(claude-grove-repo-list)           ; → parsed JSON from `grove repo list --json`
(claude-grove-workspace-status id) ; → parsed JSON from `grove workspace status --json`

;; Asynchronous (for mutations that take time)
(claude-grove-workspace-create repo-path branch callback)
(claude-grove-workspace-sync branch callback)
(claude-grove-workspace-close branch mode callback)  ; mode = 'merge | 'discard
(claude-grove-repo-add path callback)

;; Callback signature: (lambda (ok data error-msg) ...)
```

### Implementation Notes

- Sync calls use `shell-command-to-string` — fine for reads that return in <100ms
- Async calls use `make-process` with a sentinel — for creates/syncs that may take seconds
- JSON parsing via `json-parse-string` with `:object-type 'plist`
- Error handling: check exit code, parse error envelope, pass to callback or signal
- Binary location: check `grove` in PATH, configurable via `claude-grove-executable`
- If grove not found: all functions return nil / call callback with error, dashboard shows install instructions

## claude-monitor.el — Attention Detection

Kept modular. Dashboard doesn't know how detection works — just queries status.

### Interface

```elisp
(claude-workspace-attention workspace-name)  ; → 'working | 'idle | 'error | nil

;; Hook for event-driven refresh
(defvar claude-attention-change-hook nil
  "Hook run when any workspace's attention status changes.
Called with (workspace-name new-status).")
```

### Current Detection Mechanism

Watches vterm buffer output for patterns (prompt appearing, output stopping). This stays as-is. If it proves unreliable, the implementation swaps without touching dashboard code.

## claude-dashboard.el — Mission Control

The big module. Handles:
- Buffer rendering (tree layout, faces, buttons)
- Doom workspace management (create/switch/delete)
- vterm buffer management (create/kill)
- User interaction (keybindings, mouse, buttons)
- Auto-refresh (timer + event-driven)

### Dashboard Mode

```elisp
(define-derived-mode claude-dashboard-mode special-mode "Claude"
  "Major mode for the Claude mission control dashboard."
  :group 'claude
  (setq buffer-read-only t
        truncate-lines t
        cursor-type nil)  ; hide cursor, navigation by highlight
  (claude-dashboard--setup-refresh-timer))
```

### Main Workspace

The "main" workspace is a Doom workspace named `claude:main`. It's not tied to any directory — it exists purely to host the dashboard buffer.

```elisp
(defun claude-goto-main ()
  "Switch to the main Claude workspace and focus the dashboard."
  (interactive)
  ;; Create Doom workspace if needed
  (unless (+workspace-exists-p "claude:main")
    (+workspace/new "claude:main"))
  (+workspace/switch-to "claude:main")
  ;; Create or switch to dashboard buffer
  (let ((buf (get-buffer-create "*claude:dashboard*")))
    (unless (eq (buffer-local-value 'major-mode buf) 'claude-dashboard-mode)
      (with-current-buffer buf
        (claude-dashboard-mode)))
    (switch-to-buffer buf)
    (claude-dashboard-refresh)))
```

## Lifecycle Management

### Opening a Home Workspace (from dashboard)

```
User hits RET on "acorn"
  → Check if Doom workspace "acorn:home" exists
  → If not: create it, set default-directory to acorn path
  → Switch to "acorn:home"
  → Open dired or find-file in that workspace
```

### Creating a Worktree (from dashboard)

```
User hits 'c' on "acorn", types "feature-auth"
  → claude-grove-workspace-create (async)
  → On success:
    → Create Doom workspace "acorn:feature-auth"
    → Set default-directory to worktree root
    → Create vterm buffer, launch Claude
    → Refresh dashboard
  → On error:
    → Show error in minibuffer or popup
    → Refresh dashboard (may show failed state)
```

### Closing a Worktree (from dashboard)

```
User hits 'x' on "feature-auth"
  → Prompt: [m]erge or [d]iscard?
  → claude-grove-workspace-close (async)
  → On success:
    → Kill vterm buffers for that workspace
    → Delete Doom workspace "acorn:feature-auth"
    → Refresh dashboard (entry disappears)
  → On error:
    → Show error message
    → Refresh dashboard (may show failed state)
```

### Syncing (from dashboard)

```
User hits 's' on "feature-auth"
  → claude-grove-workspace-sync (async)
  → On success: refresh dashboard (sync status clears)
  → On conflict:
    → Show conflict details in minibuffer/popup
    → Jump to the conflicted worktree workspace
    → User/Claude resolves, re-syncs from dashboard
```
