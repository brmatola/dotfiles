# Grove Workspace Integration: Implementation Tasks

Last updated: 2026-02-14

## Overview

5 phases. Phase 1 depends on grove repo registry being implemented. Phases 2-5 build sequentially.

## File Map

| File | Status | Description |
|------|--------|-------------|
| `claude-grove.el` | NEW | Grove CLI wrapper (sync + async) |
| `claude-monitor.el` | MODIFY | Clean interface for attention queries |
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

## Phase 1: Grove CLI Wrapper

**Goal:** Emacs can call grove and parse results. No UI changes yet.

### Task 1.1: Create claude-grove.el

- [ ] `claude-grove-repo-list` — sync, calls `grove repo list --json`, parses to plist
- [ ] `claude-grove-workspace-status` — sync, calls `grove workspace status --json`
- [ ] `claude-grove-workspace-create` — async with callback
- [ ] `claude-grove-workspace-sync` — async with callback
- [ ] `claude-grove-workspace-close` — async with callback
- [ ] `claude-grove-repo-add` — async with callback
- [ ] Error handling: grove not found, non-zero exit, JSON parse failure
- [ ] Configurable `claude-grove-executable` (default: "grove" in PATH)

### Task 1.2: Tests for grove wrapper

- [ ] Mock shell commands, verify JSON parsing
- [ ] Error cases: grove not found, bad JSON, non-zero exit
- [ ] Async callback invocation

**Exit criteria:** `(claude-grove-repo-list)` returns parsed data from grove.

---

## Phase 2: Dashboard Foundation

**Goal:** Dashboard buffer renders with data from grove. No actions yet.

### Task 2.1: Dashboard mode and buffer

- [ ] Define `claude-dashboard-mode` derived from `special-mode`
- [ ] Create `*claude:dashboard*` buffer
- [ ] Define all custom faces (repo header, branch, status, buttons, tree, footer)
- [ ] Faces inherit from doom-themes for theme compatibility
- [ ] Test faces work in both GUI and terminal

### Task 2.2: Rendering pipeline

- [ ] `claude-dashboard-render` — main render function
- [ ] `claude-dashboard--insert-title` — styled title with nerd-icon
- [ ] `claude-dashboard--insert-repo-section` — repo header with background band
- [ ] `claude-dashboard--insert-worktree-entry` — tree lines, branch, commits, status
- [ ] `claude-dashboard--insert-subrepo-entry` — dimmed sub-repo details for grouped workspaces
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
- [ ] Preserve cursor position across refreshes (remember entry under point by ID)
- [ ] Event-driven refresh via `claude-attention-change-hook`
- [ ] `g` key for manual refresh (keep as backup)

**Exit criteria:** Dashboard renders correctly with grove data, auto-refreshes, looks good.

---

## Phase 3: Navigation

**Goal:** Can jump between main, home workspaces, and worktree workspaces.

### Task 3.1: Main workspace

- [ ] `claude-goto-main` — create/switch to `claude:main` Doom workspace, focus dashboard
- [ ] Bind to `SPC C m` globally
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
- [ ] `TAB` — collapse/expand repo section
- [ ] Highlight current entry (subtle background or fringe indicator)

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

### Task 4.5: Mouse support

- [ ] All buttons clickable via mouse
- [ ] Repo headers clickable (same as RET)
- [ ] Worktree entries clickable (same as RET)

**Exit criteria:** Full lifecycle from dashboard: add repo → create worktree → sync → close.

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

### Task 5.2: Update claude-monitor.el

- [ ] Clean public interface: `claude-workspace-attention` function
- [ ] Remove any dependencies on deleted modules
- [ ] Verify attention detection still works with new workspace naming

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
