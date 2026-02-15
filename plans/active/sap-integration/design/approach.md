# sap Integration — Approach

Last updated: 2026-02-14

## claude-monitor.el Rewrite

### What Gets Removed

- `claude-attention-patterns` — regex list (replaced by exact hook events)
- `claude-attention-idle-threshold` — timing heuristic (replaced by idle_prompt hook)
- `claude--last-content-hash`, `claude--last-output-time` — buffer-local hash tracking
- `claude--check-buffer-attention` — the vterm scraping function
- `vterm--filter-buffer-substring` dependency — no more buffer content reading

### What Gets Added

#### sap CLI wrapper (following grove.el pattern)

```elisp
(defun claude-sap-status (callback)
  "Query sap for current session states.
Call CALLBACK with (ok data error-msg)."
  ;; Async process call to: sap status --json
  ;; Parse JSON response
  ;; Return via callback
  )

(defun claude-sap-latest (workspace callback)
  "Query sap for latest session in WORKSPACE.
Call CALLBACK with (ok data error-msg)."
  ;; Async process call to: sap latest --workspace WORKSPACE --json
  )
```

#### Polling loop

```elisp
(defvar claude-sap-poll-interval 2
  "Seconds between sap status polls.")

(defun claude--poll-sap ()
  "Poll sap for current session states and update internal state."
  (claude-sap-status
    (lambda (ok data _err)
      (when ok
        (let ((sessions (plist-get data :sessions)))
          (dolist (session sessions)
            (let* ((ws (plist-get session :workspace))
                   (state (intern (plist-get session :state)))
                   (old-state (claude--get-workspace-state ws)))
              (claude--set-workspace-state ws session)
              (when (not (eq state old-state))
                (run-hook-with-args 'claude-attention-change-hook
                                    ws state)))))))))
```

#### State model

Old three-state model:
- `working` — buffer content changing
- `idle` — content stable + attention pattern matched
- `nil` — no buffer

New state model (from sap):
- `active` — Claude is working (session-start, tool-use, user-prompt events)
- `attention` — Claude needs input (permission_prompt, idle_prompt events)
- `stopped` — session ended
- `nil` — no session for this workspace

#### Public API (preserved for dashboard compatibility)

```elisp
(defun claude-workspace-attention (workspace)
  "Return attention state for WORKSPACE.
Returns `active', `attention', `stopped', or nil."
  ;; Look up workspace in internal state table (populated by polling)
  )

(defun claude-workspace-session (workspace)
  "Return full session plist for WORKSPACE, or nil."
  ;; Returns the full sap session data including:
  ;; :session-id, :state, :started-at, :last-event-at,
  ;; :last-tool, :last-tool-detail
  )
```

### What Stays

- `claude-attention-change-hook` — same hook signature, dashboard still listens
- `claude-monitor-start` / `claude-monitor-stop` — start/stop the poll timer
- `claude-monitor-interval` — renamed from poll interval, same purpose
- Buffer naming convention (`*claude:REPO:BRANCH*`) — still used for vterm management

## claude-dashboard.el Changes

### Status Indicators

Current indicator logic (lines 537-561 of dashboard) maps to new states:

```
active    → ⚡ working (success face)
attention → ● waiting (warning face, bold)
stopped   → ◌ stopped (footer face)
```

### Last Tool Display

When sap reports `last_tool` and `last_tool_detail`, the dashboard can show what Claude is doing:

```
⚡ editing src/app.ts
⚡ running tests
● waiting for permission
◌ stopped (2m ago)
```

This replaces the generic "working" indicator with actionable context.

### Resume Action

New action on workspaces with stopped sessions:

```elisp
(defun claude-dashboard--resume-workspace (workspace)
  "Resume the latest stopped session in WORKSPACE."
  (claude-sap-latest workspace
    (lambda (ok data _err)
      (when (and ok (equal (plist-get data :state) "stopped"))
        (let ((session-id (plist-get data :session_id)))
          ;; Open/switch to workspace, then launch claude --resume
          (claude-dashboard--open-workspace workspace)
          (vterm-send-string
            (format "claude --resume %s\n" session-id)))))))
```

The dashboard shows "Resume" instead of "Open" when a stopped session exists for a workspace.

### Session Duration

With `started_at` available, the dashboard can show session duration:

```
⚡ working (12m)
● waiting (12m, idle 30s)
◌ stopped (ran for 45m)
```

## claude-grove.el — No Changes

grove.el is unaffected. It manages workspace lifecycle (create, sync, close). sap manages session awareness. They're orthogonal.

## Testing Strategy

### claude-monitor tests (rewrite)

- Stub `claude-sap-status` to return canned JSON
- Test state transitions: active → attention → active → stopped
- Test hook firing on state changes
- Test graceful handling of sap not installed (fall back to nil states)
- Test multiple workspaces with different states

### claude-dashboard tests (additions)

- Test new status indicator rendering (active/attention/stopped)
- Test last-tool display in status line
- Test resume action availability (stopped session → show Resume button)
- Test duration display formatting

### No vterm dependency in tests

The rewritten monitor has zero vterm dependency — it's pure async process calls and state management. All testable in batch mode.

## Migration

### Backward compatibility

The old monitor can coexist during migration:
1. Deploy sap + hooks first
2. Rewrite monitor to poll sap
3. If sap is not installed, monitor returns nil for all workspaces (dashboard shows no status, but doesn't crash)
4. Remove old pattern-matching code once sap is confirmed working

### Hooks installation

The Claude Code hooks configuration lives in `claude/settings.json` in this dotfiles repo. Adding hooks there means they're active for anyone using these dotfiles (i.e., just you). The hooks are no-ops if sap isn't installed (Claude Code reports hook command failure but continues normally).
