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

Thin layer: shell out, parse JSON, return elisp data. All grove interaction goes through here. **All calls are async** — no synchronous shell-outs, so Emacs never blocks.

### Interface

```elisp
;; All calls are async with callbacks
(claude-grove-repo-list callback)                    ; `grove repo list --json`
(claude-grove-workspace-status branch callback)      ; `grove workspace status <branch> --json`
(claude-grove-workspace-create repo-path branch callback)
(claude-grove-workspace-sync branch callback)
(claude-grove-workspace-close branch mode callback)  ; mode = 'merge | 'discard
(claude-grove-workspace-switch branch callback)      ; `grove workspace switch <branch> --json`
(claude-grove-repo-add path callback)
(claude-grove-repo-remove name callback)

;; Callback signature: (lambda (ok data error-msg) ...)

;; Guard against overlapping calls
(claude-grove--request-pending-p command)  ; → t if a call of this type is in-flight
```

### Implementation Notes

- All calls use `make-process` with a sentinel — Emacs never blocks on grove
- JSON parsing via `json-parse-string` with `:object-type 'plist`
- Error handling: check exit code, parse error envelope, pass to callback
- **In-flight guard**: `claude-grove-repo-list` tracks whether a call is pending. If the refresh timer fires while a previous `repo list` is still running, the new call is skipped. This prevents queuing up slow calls.
- **Timeout**: Each process gets a kill timer (default 10s, configurable via `claude-grove-timeout`). If grove doesn't respond, the process is killed and the callback is invoked with an error. Prevents stuck dashboard on grove hang.
- Binary location: check `grove` in PATH, configurable via `claude-grove-executable`
- If grove not found: callback called with `(nil nil "grove not found")`, dashboard shows install instructions

## claude-monitor.el — Attention Detection

Kept modular. Dashboard doesn't know how detection works — just queries status. **No modeline** — the dashboard is the sole UI for workspace status.

### Interface

```elisp
(claude-workspace-attention workspace-name)  ; → 'working | 'idle | 'error | nil

;; Hook for event-driven refresh
(defvar claude-attention-change-hook nil
  "Hook run when any workspace's attention status changes.
Called with (workspace-name new-status).")
```

### vterm Buffer Naming Convention

vterm buffers follow the pattern `*claude:<repo>:<branch>*` (e.g., `*claude:acorn:feature-auth*`). This is the contract between the dashboard (creates buffers) and the monitor (finds them by name to check output). For home workspaces, the buffer is `*claude:<repo>:home*`.

### Current Detection Mechanism

Watches vterm buffer output for patterns (prompt appearing, output stopping). This stays as-is. If it proves unreliable, the implementation swaps without touching dashboard code.

## claude-dashboard.el — Mission Control

The big module. Handles:
- Buffer rendering (tree layout, faces, buttons)
- Doom workspace management (create/switch/delete)
- vterm buffer management (create/kill)
- User interaction (keybindings, mouse, buttons)
- Auto-refresh (timer + event-driven)
- Orphan Doom workspace cleanup (grove worktree gone → clean up Doom workspace)

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
  → If not: create it, set default-directory to repo path (from grove cache)
  → Switch to "acorn:home"
  → Open dired or find-file in that workspace
```

### Creating a Worktree (from dashboard)

```
User hits 'c' on "acorn", types "feature-auth"
  → claude-grove-workspace-create (async)
  → On success:
    → Create Doom workspace "acorn:feature-auth"
    → Set default-directory to worktree root (from grove response `data.root`)
    → Create vterm buffer "*claude:acorn:feature-auth*"
    → Launch `claude` in vterm with default-directory set to worktree root
      (No arguments — user gives Claude instructions interactively)
    → Refresh dashboard
  → On error:
    → message "Grove: create failed — %s" in minibuffer
    → Refresh dashboard (may show failed state)
```

### Closing a Worktree (from dashboard)

```
User hits 'x' on "feature-auth"
  → Prompt: [m]erge or [d]iscard?
  → claude-grove-workspace-close (async)
  → On success:
    → Kill vterm buffer "*claude:acorn:feature-auth*"
    → Delete Doom workspace "acorn:feature-auth"
    → Refresh dashboard (entry disappears)
  → On error:
    → message "Grove: close failed — %s" in minibuffer
    → Refresh dashboard (may show failed state)
```

### Syncing (from dashboard)

```
User hits 's' on "feature-auth"
  → claude-grove-workspace-sync (async)
  → On success: refresh dashboard (sync status clears)
  → On conflict:
    → message "Grove: conflicts in %s — %s" in minibuffer
    → Jump to the conflicted worktree workspace
    → User/Claude resolves, re-syncs from dashboard
```

### Orphan Cleanup (on refresh)

```
On every dashboard refresh:
  → Compare active Doom workspaces matching "repo:branch" pattern
    against grove repo list data
  → If a Doom workspace exists but no matching grove worktree:
    → Kill associated vterm buffers
    → Delete the orphaned Doom workspace
    → (Handles worktrees closed outside Emacs via CLI)
  → Skip workspaces in transient states (creating, closing):
    → These are mid-operation — not orphans
    → Only treat as orphan if workspace is completely absent from grove data
```

### Error Display

All grove errors use `message` (minibuffer). Format: `"Grove: <action> failed — <error>"`. No popups, no dedicated error area. The minibuffer is visible, non-intrusive, and consistent with Emacs conventions.
