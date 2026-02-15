# sap Integration — Approach

Last updated: 2026-02-14

## claude-monitor.el Rewrite

### What Gets Removed

- `claude-attention-patterns` — regex list (replaced by exact hook events)
- `claude-attention-idle-threshold` — timing heuristic (replaced by attention-idle hook)
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

#### Internal state table

```elisp
(defvar claude--workspace-states (make-hash-table :test 'equal)
  "Hash table mapping workspace name to (STATE . SESSION-PLIST).
Populated by polling loop, queried by public API.")

(defun claude--get-workspace-state (workspace)
  "Return state symbol for WORKSPACE, or nil."
  (car (gethash workspace claude--workspace-states)))

(defun claude--set-workspace-state (workspace session state)
  "Set STATE and SESSION data for WORKSPACE."
  (puthash workspace (cons state session) claude--workspace-states))

(defun claude--set-workspace-stopped (workspace)
  "Mark WORKSPACE as stopped, preserving session data."
  (when-let ((entry (gethash workspace claude--workspace-states)))
    (puthash workspace (cons 'stopped (cdr entry))
             claude--workspace-states)))

(defun claude--each-workspace-state (fn)
  "Call FN with (workspace session) for each tracked workspace."
  (maphash (lambda (ws entry) (funcall fn ws (cdr entry)))
           claude--workspace-states))
```

#### Polling loop

`sap status` only returns non-stopped sessions. To detect `active` → `stopped` transitions, the poll loop compares the returned workspace set against its internal state table. Any previously-known workspace absent from the response has stopped.

Stale sessions (sap annotates `"stale": true` when `last_event_at` > 10 minutes) are treated as stopped — they represent crashed/orphaned Claude processes.

```elisp
(defvar claude-sap-poll-interval 2
  "Seconds between sap status polls.")

(defun claude--poll-sap ()
  "Poll sap for current session states and update internal state.
Detects stopped sessions by absence from sap status response.
Treats stale sessions as stopped."
  (claude-sap-status
    (lambda (ok data _err)
      (when ok
        (let ((sessions (plist-get data :sessions))
              (seen (make-hash-table :test 'equal)))
          ;; Process sessions present in response
          (dolist (session sessions)
            (let* ((ws (plist-get session :workspace))
                   (stale (eq (plist-get session :stale) t))
                   (state (if stale 'stopped
                            (intern (plist-get session :state))))
                   (old-state (claude--get-workspace-state ws)))
              (puthash ws t seen)
              (claude--set-workspace-state ws session state)
              (when (not (eq state old-state))
                (run-hook-with-args 'claude-attention-change-hook
                                    ws state))))
          ;; Detect stopped: known workspaces absent from response
          (claude--each-workspace-state
            (lambda (ws _session)
              (unless (or (gethash ws seen)
                          (eq (claude--get-workspace-state ws) 'stopped))
                (claude--set-workspace-stopped ws)
                (run-hook-with-args 'claude-attention-change-hook
                                    ws 'stopped)))))))))
```

#### State model

Old three-state model:
- `working` — buffer content changing
- `idle` — content stable + attention pattern matched (confusingly named — actually means "needs attention")
- `nil` — no buffer

New state model (from sap):
- `active` — Claude is working (session-start, tool-use, user-prompt events)
- `idle` — Claude finished its turn, awaiting user input (turn-complete event)
- `attention` — Claude is blocked, needs user action (attention-permission, attention-idle events)
- `stopped` — session ended
- `nil` — no session for this workspace

**Naming collision:** Old `idle` meant "needs attention." New `idle` means the opposite — Claude is done, your turn. What was `idle` is now `attention`. All callsites in the dashboard that check for `'idle` must change to `'attention` (or `'idle` with new semantics).

#### Public API (preserved for dashboard compatibility)

WORKSPACE parameter uses sap's `repo:branch` format (e.g., `dotfiles:main`). See "Workspace identity bridge" below for how the dashboard maps its naming conventions to this format.

```elisp
(defun claude-workspace-attention (workspace)
  "Return attention state for WORKSPACE.
WORKSPACE is in sap format: \"repo:branch\".
Returns `active', `idle', `attention', `stopped', or nil."
  (car (gethash workspace claude--workspace-states)))

(defun claude-workspace-session (workspace)
  "Return full session plist for WORKSPACE, or nil.
WORKSPACE is in sap format: \"repo:branch\"."
  ;; Returns the full sap session data including:
  ;; :session_id, :state, :started_at, :last_event_at,
  ;; :last_tool, :last_tool_detail
  ;; (key names match JSON — underscored, not hyphenated)
  (cdr (gethash workspace claude--workspace-states)))
```

### What Stays

- `claude-attention-change-hook` — same hook signature, dashboard still listens
- `claude-monitor-start` / `claude-monitor-stop` — start/stop the poll timer
- `claude-monitor-interval` — renamed from poll interval, same purpose
- Buffer naming convention (`*claude:REPO:BRANCH*`) — still used for vterm management

## Workspace Identity Bridge

sap identifies workspaces as `repo:branch` (resolved from cwd via git). The dashboard uses two naming conventions:

| Dashboard context | Dashboard name | sap name | Match? |
|-------------------|---------------|----------|--------|
| Worktree entry | `dotfiles:feature-x` | `dotfiles:feature-x` | Yes |
| Repo home session | `dotfiles:home` | `dotfiles:main` | **No** |

Worktrees match directly — the branch name is the same in both systems. Home sessions don't — the dashboard uses the synthetic `:home` suffix while sap uses the repo's actual branch.

### Resolution

The dashboard resolves the repo's current branch when querying home session attention. The Doom workspace name (`repo:home`) and vterm buffer name (`*claude:repo:home*`) stay as-is — those are UI labels, not sap queries.

```elisp
;; Worktree attention — direct match, no change needed
(claude-workspace-attention (format "%s:%s" repo-name branch))

;; Home attention — resolve actual branch first
(claude-workspace-attention (format "%s:%s" repo-name (claude-dashboard--repo-branch path)))
```

`claude-dashboard--repo-branch` resolves the repo's current branch:

```elisp
(defun claude-dashboard--repo-branch (repo-path)
  "Return the current branch name for REPO-PATH.
Synchronous — called once per repo per refresh cycle."
  (let ((default-directory (file-name-as-directory repo-path)))
    (string-trim
     (shell-command-to-string "git rev-parse --abbrev-ref HEAD"))))
```

This is one `git` call per repo per refresh cycle (~5 second interval). With a small number of registered repos, the overhead is negligible.

### Caching

The resolved branch can be cached alongside the grove data in the dashboard's tier 1 pipeline. Invalidation: re-resolve on each full refresh (tier 1). Branch switches on the main repo are uncommon in worktree-based workflows.

## Claude Code Hooks Configuration

sap is fed by Claude Code hooks. Each hook event pipes its stdin JSON (which includes `session_id` and `cwd`) directly to `sap record`. sap ignores unknown fields, so the extra hook metadata (`hook_event_name`, `permission_mode`, etc.) passes through harmlessly. sap also picks up `transcript_path` when present.

### Hook-to-sap event mapping

| Claude Code Hook | sap Event | sap State After |
|------------------|-----------|-----------------|
| `SessionStart` | `session-start` | `active` |
| `SessionEnd` | `session-end` | `stopped` |
| `PreToolUse` | `tool-use` | `active` |
| `Stop` | `turn-complete` | `idle` |
| `PermissionRequest` | `attention-permission` | `attention` |
| `UserPromptSubmit` | `user-prompt` | `active` |
| `Notification` | `attention-idle` | `attention` |

`PreToolUse` is used over `PostToolUse` so that sap shows what Claude is *currently doing*, not what it just finished. Both fire with `tool_name` and `tool_input` on stdin.

`Notification` maps to `attention-idle` — when Claude sends a desktop notification, it means the session is idle and wants user attention.

### Configuration

Lives in `claude/settings.json` in this dotfiles repo (symlinked to `~/.claude/settings.json`). Hooks are no-ops if sap isn't installed — Claude Code logs the hook failure but continues normally.

```json
{
  "hooks": {
    "SessionStart": [
      { "hooks": [{ "type": "command", "command": "sap record --event session-start" }] }
    ],
    "SessionEnd": [
      { "hooks": [{ "type": "command", "command": "sap record --event session-end" }] }
    ],
    "PreToolUse": [
      { "hooks": [{ "type": "command", "command": "sap record --event tool-use" }] }
    ],
    "Stop": [
      { "hooks": [{ "type": "command", "command": "sap record --event turn-complete" }] }
    ],
    "PermissionRequest": [
      { "hooks": [{ "type": "command", "command": "sap record --event attention-permission" }] }
    ],
    "UserPromptSubmit": [
      { "hooks": [{ "type": "command", "command": "sap record --event user-prompt" }] }
    ],
    "Notification": [
      { "hooks": [{ "type": "command", "command": "sap record --event attention-idle" }] }
    ]
  }
}
```

Each hook is synchronous (default). `sap record` writes to SQLite and exits in <50ms, so there's no perceptible delay. If sap fails (exit code 2), Claude Code logs the error and continues — hooks don't block the session.

## claude-dashboard.el Changes

### Repo Header Status (Home Sessions)

The repo header line shows sap status for the home session — same indicators as worktrees. This gives at-a-glance state for everything on the dashboard:

```
dotfiles  ◇ ready                              [ Open ]
├  fix-auth  3 commits  ⚡ editing auth.ts      [ Jump ] [ Close ]
└  add-cache  1 commit  ● waiting              [ Jump ] [ Close ]

sap  ⚡ running tests                           [ Open ]
  (no active workspaces)
```

Every repo with an active Claude session shows its state inline on the header. The resolution uses `claude-dashboard--repo-branch` to map the dashboard's `repo:home` naming to sap's `repo:<actual-branch>` (see Workspace Identity Bridge above).

### Status Indicators

Current indicator logic (lines 537-561 of dashboard) maps to new states:

```
active    → ⚡ working (success face)
idle      → ◇ ready (shadow face) — Claude finished, your turn
attention → ● waiting (warning face, bold) — Claude is blocked
stopped   → ◌ stopped (footer face)
nil       → (no indicator — no session for this workspace)
```

`idle` and `attention` both mean "you should interact," but attention is urgent (Claude can't continue without you) while idle is informational (Claude finished, ball is in your court).

The `nil` case replaces the current fallback that shows "working" for unknown states. A grove-active workspace with no Claude session should show no status indicator.

#### Callsite migration

The return value of `claude-workspace-attention` changes from `working/idle/nil` to `active/idle/attention/stopped/nil`. Dashboard callsites that need updating:

- `claude-dashboard--insert-status` — add `idle` case, rename `'idle` → `'attention` for waiting indicator
- `claude-dashboard--sort-workspaces` — change `(memq ... '(idle error))` to `(memq ... '(idle attention))`
- `claude-dashboard--insert-repo-section` — home attention check (already works, just returns new symbols)
- All test assertions that check for `'working` or `'idle` (old semantics)

### Last Tool Display

When sap reports `last_tool` and `last_tool_detail`, the dashboard can show what Claude is doing:

```
⚡ editing src/app.ts           (active — tool context)
⚡ running tests                (active — tool context)
◇ ready                        (idle — no tool context, Claude is done)
● waiting for permission       (attention — blocked)
◌ stopped (2m ago)             (stopped — duration since stop)
```

Last-tool context is shown for `active` state only. `idle` means Claude finished — showing the last tool it used isn't useful.

### Resume Action

New action on workspaces whose monitor state is `stopped`. The dashboard renders "Resume" instead of "Open"/"Jump" when the monitor reports a stopped session.

On resume, the dashboard calls `sap latest` to get the session ID (the monitor's internal state may not include it), then launches `claude --resume` in the appropriate vterm:

```elisp
(defun claude-dashboard--resume-session (data)
  "Resume the stopped session in workspace described by DATA.
DATA is a dashboard entry plist with :repo-name, :branch, :path, :root."
  (let* ((repo-name (plist-get data :repo-name))
         (branch (plist-get data :branch))
         (sap-ws (format "%s:%s" repo-name branch)))
    (claude-sap-latest sap-ws
      (lambda (ok latest _err)
        (when (and ok (equal (plist-get latest :state) "stopped"))
          (let ((session-id (plist-get latest :session_id))
                (buf-name (format "*claude:%s:%s*" repo-name branch)))
            ;; Switch to workspace (reuse existing open-home / jump-to-worktree)
            (if (equal branch "home")
                (claude-dashboard--open-home
                  (list :name repo-name :path (plist-get data :path)))
              (claude-dashboard--jump-to-worktree data))
            ;; Send resume command to vterm
            (when-let ((buf (get-buffer buf-name)))
              (with-current-buffer buf
                (vterm-send-string
                  (format "claude --resume %s\n" session-id))))))))))
```

Resume is only available when the monitor has detected a stopped session (via the polling loop's absence-detection). If Emacs restarts and the monitor hasn't seen the session, no resume button appears — acceptable tradeoff vs polling `sap latest` for every workspace on every refresh.

### Session Duration

With `started_at` and `last_event_at` available, the dashboard can show contextual timing:

```
⚡ working (12m)               (session duration)
◇ ready (30s)                  (time since Claude finished — how long it's been your turn)
● waiting (45s)                (time since attention event — how long Claude has been blocked)
◌ stopped (2m ago)             (time since session ended)
```

## claude-grove.el — No Changes

grove.el is unaffected. It manages workspace lifecycle (create, sync, close). sap manages session awareness. They're orthogonal.

## Testing Strategy

### claude-monitor tests (rewrite)

- Stub `claude-sap-status` to return canned JSON
- Test state transitions: active → idle → active → attention → active → stopped
- Test hook firing on state changes (including idle transitions)
- Test graceful handling of sap not installed (fall back to nil states)
- Test multiple workspaces with different states simultaneously
- **Test stopped detection by absence**: stub returns session on first poll, omits it on second poll → state transitions to `stopped`, hook fires
- **Test stopped sessions don't re-fire**: once marked stopped, subsequent polls without the session don't fire the hook again
- **Test stale sessions treated as stopped**: stub returns session with `"stale": true` → state is `stopped`, not the underlying state
- **Test idle state**: stub returns session with `"state": "idle"` → `claude-workspace-attention` returns `'idle`

### claude-dashboard tests (additions)

- Test new status indicator rendering (active/idle/attention/stopped/nil)
- Test nil attention renders no indicator (not "working")
- Test idle renders ◇ ready (not working, not waiting)
- Test last-tool display in status line (active only, not idle)
- Test resume action availability (stopped session → show Resume button)
- Test duration display formatting for each state
- **Test home attention uses resolved branch**: stub `claude-dashboard--repo-branch` to return "main", verify dashboard queries `repo:main` not `repo:home`
- **Test workspace sorting**: both `idle` and `attention` sort above `active`; `attention` sorts above `idle`

### No vterm dependency in tests

The rewritten monitor has zero vterm dependency — it's pure async process calls and state management. All testable in batch mode.

## Migration

### Atomic swap, not phased

The monitor rewrite and dashboard callsite migration must land as a single change. The old monitor returns `'working` / `'idle` (meaning attention) / `nil`. The new monitor returns `'active` / `'idle` / `'attention` / `'stopped` / `nil`. If the dashboard is updated to expect new symbols but the monitor still returns old ones (or vice versa), status indicators will silently break.

Specifically, the home attention query (`(format "%s:home" name)`) must change to use the resolved branch at the same time the monitor switches to sap — otherwise the dashboard queries `repo:home` against sap's `repo:main` and gets nil.

### Steps

1. Install sap CLI and configure hooks in `claude/settings.json` (can be done before the Emacs changes — hooks are no-ops without sap, sap records events without Emacs consuming them)
2. Rewrite `claude-monitor.el` + update all dashboard callsites + update all tests in a single commit
3. If sap is not installed, monitor returns nil for all workspaces (dashboard shows no status, but doesn't crash)

### Hooks installation

The Claude Code hooks configuration lives in `claude/settings.json` in this dotfiles repo (see "Claude Code Hooks Configuration" section above). Adding hooks there means they're active for anyone using these dotfiles (i.e., just you). The hooks are no-ops if sap isn't installed — Claude Code logs the hook failure but continues normally.
