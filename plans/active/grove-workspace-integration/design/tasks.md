# Grove Workspace Integration: Implementation Tasks

Last updated: 2026-02-14

## Overview

5 phases. Phase 1 depends on grove repo registry being implemented. Phases 2-5 build sequentially.

## File Map

| File | Status | Description |
|------|--------|-------------|
| `claude-grove.el` | NEW | Grove CLI wrapper (all async, with timeout) |
| `claude-monitor.el` | MODIFY | Attention detection only (no modeline, updated in Phase 1) |
| `claude-dashboard.el` | REWRITE | Mission control dashboard |
| `claude.el` | MODIFY | Simplified entry point + global keybindings |
| `claude-state.el` | DELETE | Replaced by grove |
| `claude-reconcile.el` | DELETE | Replaced by grove |
| `claude-worktree.el` | DELETE | Replaced by grove |
| `claude-cleanup.el` | DELETE | Replaced by grove |
| `claude-workspace.el` | DELETE | Absorbed into dashboard |
| `claude-vterm.el` | DELETE | Absorbed into dashboard |
| `test/claude-grove-test.el` | NEW | Tests for grove wrapper |
| `test/claude-dashboard-test.el` | NEW | Tests for dashboard rendering |

---

## Phase 0: Prerequisites

**Goal:** Grove repo registry is implemented and working.

- [ ] Implement `grove repo add/remove/list` (see grove repo-registry plan)
- [ ] Verify `grove repo list --json` returns enriched data (repos + workspaces + status)
- [ ] Install grove globally (`npm link` or add to PATH)

---

## Phase 1: Grove CLI Wrapper + Monitor Update

**Goal:** Emacs can call grove and parse results. Monitor adapted to new naming convention. No UI changes yet.

### Task 1.1: Create claude-grove.el

- [ ] `claude-grove-repo-list` — async with callback, calls `grove repo list --json`
- [ ] `claude-grove-workspace-status` — async with callback, calls `grove workspace status <branch> --json`
- [ ] `claude-grove-workspace-create` — async with callback
- [ ] `claude-grove-workspace-sync` — async with callback
- [ ] `claude-grove-workspace-close` — async with callback
- [ ] `claude-grove-workspace-switch` — async with callback, calls `grove workspace switch <branch> --json`
- [ ] `claude-grove-repo-add` — async with callback
- [ ] `claude-grove-repo-remove` — async with callback
- [ ] In-flight guard: skip `repo-list` call if previous one is still pending (debounce)
- [ ] Timeout: kill process after `claude-grove-timeout` seconds (default 10), invoke callback with error
- [ ] Error handling: grove not found, non-zero exit, JSON parse failure
- [ ] Configurable `claude-grove-executable` (default: "grove" in PATH)

### Task 1.2: Update claude-monitor.el for new buffer naming

The dashboard (Phase 2) depends on `claude-workspace-attention` using the new vterm buffer naming convention. This must happen before Phase 2.

- [ ] Update buffer name matching to use `*claude:<repo>:<branch>*` pattern
- [ ] Remove modeline integration (dashboard is the sole status UI)
- [ ] Remove any dependencies on modules that will be deleted (claude-state, etc.)
- [ ] Clean public interface: `claude-workspace-attention` function + `claude-attention-change-hook`

### Task 1.3: Tests for grove wrapper and monitor

- [ ] Mock shell commands, verify JSON parsing
- [ ] Error cases: grove not found, bad JSON, non-zero exit, timeout
- [ ] Async callback invocation
- [ ] Monitor: buffer name matching with new naming convention

**Exit criteria:** `(claude-grove-repo-list #'callback)` invokes callback with parsed data from grove. `(claude-workspace-attention "repo:branch")` returns status using new buffer names.

---

## Phase 2: Dashboard Foundation

**Goal:** Dashboard buffer renders with data from grove. No actions yet.

### Task 2.1: Dashboard mode and buffer

- [ ] Define `claude-dashboard-mode` derived from `special-mode`
- [ ] Create `*claude:dashboard*` buffer
- [ ] Define all custom faces (repo header, branch, status, lifecycle, failed, current-entry, buttons, tree, footer)
- [ ] Faces inherit from doom-themes for theme compatibility
- [ ] Test faces work in both GUI and terminal

### Task 2.2: Data cache and progressive rendering pipeline

- [ ] `claude-dashboard--grove-cache` — buffer-local var holding merged grove data
- [ ] `claude-dashboard-refresh` — tier 1 fetch, paint, then fire tier 2 per workspace
- [ ] `claude-dashboard--merge-workspace-status` — merge tier 2 data into cache by workspace ID
- [ ] `claude-dashboard--paint` — sync buffer paint from cache (no I/O)
- [ ] `claude-dashboard--render-error` — error/missing-grove state (install instructions)
- [ ] `claude-dashboard--insert-title` — styled title with nerd-icon
- [ ] `claude-dashboard--insert-repo-section` — repo header with background band
- [ ] `claude-dashboard--insert-worktree-entry` — tree lines, branch, status (attention + lifecycle); commit count and sub-repo details omitted until tier 2 data arrives
- [ ] `claude-dashboard--insert-subrepo-entry` — dimmed sub-repo details for grouped workspaces (only rendered when tier 2 data present)
- [ ] `claude-dashboard--insert-button` — clickable text with box face and action property
- [ ] `claude-dashboard--insert-footer` — hint bar with keybinding summary
- [ ] Text properties on all entries (`claude-dashboard-entry-type`, `claude-dashboard-entry-data`)

### Task 2.3: Column alignment

- [ ] Right-align commit counts
- [ ] Right-align status indicators
- [ ] Right-align buttons
- [ ] Handle variable-width content gracefully (long branch names, etc.)

### Task 2.4: Auto-refresh

- [ ] Timer-based refresh (5s default, configurable via `claude-dashboard-refresh-interval`)
- [ ] Timer only active when dashboard is in a visible window
- [ ] Skips refresh if previous grove call is still in-flight (debounce via grove wrapper)
- [ ] Preserve cursor position across refreshes (remember entry under point by ID)
- [ ] Preserve collapse/expand state across refreshes (`claude-dashboard--collapsed-repos` buffer-local var)
- [ ] Event-driven refresh via `claude-attention-change-hook`

### Task 2.5: Orphan Doom workspace cleanup

- [ ] On each refresh, compare active Doom workspaces (matching `<repo>:<branch>` pattern) against grove data
- [ ] If a Doom workspace exists with no matching grove worktree: kill vterm buffers, delete Doom workspace
- [ ] Skip workspaces in transient states (`creating`, `closing`) — these are mid-operation, not orphans
- [ ] Handles worktrees closed outside Emacs (via CLI `grove workspace close`)

**Exit criteria:** Dashboard renders immediately from tier 1 data, progressively fills in commit counts and sub-repo details from tier 2, auto-refreshes, looks good. Orphaned Doom workspaces are cleaned up automatically (skipping transient states).

---

## Phase 3: Navigation

**Goal:** Can jump between main, home workspaces, and worktree workspaces.

### Task 3.1: Main workspace

- [ ] `claude-goto-main` — create/switch to `claude:main` Doom workspace, focus dashboard
- [ ] Bind to `SPC ;` globally (single global keybinding, no prefix map)
- [ ] Main workspace has no default-directory (or uses home)

### Task 3.2: Home workspace navigation

- [ ] `RET` on repo header → open/create Doom workspace `<repo>:home`
- [ ] Set `default-directory` to repo path
- [ ] If workspace already exists, just switch to it

### Task 3.3: Worktree workspace navigation

- [ ] `RET` on worktree entry → open/create Doom workspace `<repo>:<branch>`
- [ ] Set `default-directory` to worktree root
- [ ] If workspace doesn't exist but worktree does (e.g., created outside Emacs), create workspace + vterm
- [ ] If workspace exists, just switch to it

### Task 3.4: Entry navigation within dashboard

- [ ] `n` / `p` — move to next/previous actionable entry (skip decorative lines)
- [ ] `TAB` — collapse/expand repo section (toggles repo name in `claude-dashboard--collapsed-repos`)
- [ ] Highlight current entry with `claude-dashboard-current-face` (subtle background overlay on the line at point, updated via `post-command-hook`)

**Exit criteria:** Full navigation loop works: main → home → worktree → main.

---

## Phase 4: Actions

**Goal:** All workspace operations work from the dashboard.

### Task 4.1: Create worktree

- [ ] `c` on repo section → prompt for branch name in minibuffer
- [ ] Call `claude-grove-workspace-create` async
- [ ] On success: create Doom workspace, launch vterm + Claude, refresh dashboard
- [ ] On error: show error, refresh dashboard
- [ ] Show spinner/progress in dashboard while creating

### Task 4.2: Sync worktree

- [ ] `s` on worktree entry → call `claude-grove-workspace-sync` async
- [ ] On success: refresh dashboard
- [ ] On conflict: show conflict info, optionally jump to workspace
- [ ] Show sync status in dashboard while running

### Task 4.3: Close worktree

- [ ] `x` on worktree entry → prompt merge/discard (single key: m or d)
- [ ] Call `claude-grove-workspace-close` async
- [ ] On success: kill vterm + buffers, delete Doom workspace, refresh dashboard
- [ ] On error: show error, refresh dashboard

### Task 4.4: Add repo

- [ ] `a` anywhere → prompt for directory path (with completion)
- [ ] Call `claude-grove-repo-add` async
- [ ] Refresh dashboard (new repo appears)

### Task 4.5: Remove repo

- [ ] `r` on repo header → confirm with y-or-n-p
- [ ] Call `claude-grove-repo-remove` async
- [ ] Refresh dashboard (repo disappears)

### Task 4.6: Mouse support

- [ ] All buttons clickable via mouse
- [ ] Repo headers clickable (same as RET)
- [ ] Worktree entries clickable (same as RET)

**Exit criteria:** Full lifecycle from dashboard: add repo → create worktree → sync → close → remove repo.

---

## Phase 5: Cleanup & Polish

**Goal:** Remove old code, update docs, final polish.

### Task 5.1: Remove old modules

- [ ] Delete `claude-state.el` and `test/claude-state-test.el`
- [ ] Delete `claude-reconcile.el` and `test/claude-reconcile-test.el`
- [ ] Delete `claude-worktree.el`
- [ ] Delete `claude-cleanup.el` and `test/claude-cleanup-test.el`
- [ ] Delete `claude-workspace.el`
- [ ] Delete `claude-vterm.el`
- [ ] Update `claude.el` requires — only needs `claude-grove`, `claude-monitor`, `claude-dashboard`

### Task 5.2: Verify claude-monitor.el

- [ ] Verify attention detection works end-to-end with new workspace naming (updated in Phase 1)
- [ ] Remove any remaining references to deleted modules

### Task 5.3: Update tests

- [ ] Remove tests for deleted modules
- [ ] New tests mock grove CLI output, not git
- [ ] Dashboard rendering tests (verify buffer contents for known grove data)
- [ ] Update `test/run-tests.sh` and `test/lint.sh`

### Task 5.4: Documentation

- [ ] Update `CLAUDE.md` module structure section
- [ ] Update dependency graph
- [ ] Update testing instructions
- [ ] Document grove dependency and install instructions

**Exit criteria:** Clean module set, all tests pass, docs accurate.
