# Claude Module Redesign: Implementation Tasks

Last updated: 2026-02-01 (reviewed, issues fixed)

## Overview

This plan is implemented in 4 phases. Each phase builds on the previous and can be tested independently.

## File Dependency Map

| File | Status | Depends On | Provides |
|------|--------|------------|----------|
| `claude-state.el` | NEW | - | State machine, metadata ops, naming utils |
| `claude-worktree.el` | MODIFY | claude-state | Git worktree ops (remove metadata ops) |
| `claude-reconcile.el` | NEW | claude-state | Reconciliation logic |
| `claude-vterm.el` | NEW | claude-state | vterm creation, buffer management |
| `claude-workspace.el` | MODIFY | claude-state, claude-vterm | Doom workspace ops |
| `claude-monitor.el` | MODIFY | claude-state | Attention detection (uses state hook) |
| `claude-dashboard.el` | MODIFY | claude-state, claude-reconcile | Dashboard UI |
| `claude-cleanup.el` | MODIFY | claude-state, claude-worktree, claude-vterm | Merge/cleanup flow |
| `claude.el` | MODIFY | all above | Entry point, keybindings |

---

## Phase 1: State Foundation

**Goal:** Single source of truth for workspace state. Tests pass in batch mode.

**Success Criteria:**
- All 6 state transitions work correctly (verified by unit tests)
- v0 metadata files are auto-migrated on read
- `claude--workspace-name` and `claude--parse-workspace-name` are inverses
- Home workspace detection works for both metadata and workspace names

### Task 1.1: Create claude-state.el

Extract and enhance from current claude-worktree.el:

```elisp
;; Provides:
;; - claude-state-create, claude-state-transition (state machine)
;; - claude-metadata-read, claude-metadata-write, claude-metadata-delete
;; - claude--workspace-name, claude--buffer-name, claude--parse-workspace-name
;; - claude--home-workspace-p
;; - claude-state-change-hook
```

- [ ] Define all state constants (creating, active, closing, failed, broken, stuck)
- [ ] Implement state transition validation
- [ ] Move metadata operations from claude-worktree.el
- [ ] Add v0→v1 migration in metadata-read
- [ ] Add `__home__` sentinel constant
- [ ] Define hook with documented signature

### Task 1.2: Create claude-state-test.el

- [ ] Test all valid state transitions
- [ ] Test invalid transition rejection
- [ ] Test metadata read/write roundtrip
- [ ] Test v0→v1 migration
- [ ] Test naming convention functions
- [ ] Test home workspace detection

### Task 1.3: Update claude-worktree.el

- [ ] Remove metadata operations (now in claude-state)
- [ ] Require claude-state
- [ ] Keep only git worktree operations
- [ ] Update all plist access to use keywords

**Exit criteria:** `./test/run-unit-tests.sh` passes

---

## Phase 2: Reconciliation Layer

**Goal:** Automatic detection and repair of drift between components.

**Success Criteria:**
- Missing worktree → status becomes `broken`
- Missing Doom workspace → auto-repaired, status stays `active`
- Missing vterm buffer → auto-repaired, Claude session restarted
- Stalled creations (>120s) marked `failed`
- Startup reconciliation runs without errors on existing workspaces

### Task 2.1: Create claude-vterm.el

Extract from claude-workspace.el (needed by reconciler for repairs):

- [ ] `claude--create-vterm-in-dir`
- [ ] `claude--send-command`
- [ ] `claude--terminal-buffers`
- [ ] `claude--next-terminal-number`
- [ ] `claude--kill-workspace-buffers`

### Task 2.2: Create claude-reconcile.el

```elisp
;; Provides:
;; - claude--reconcile (main entry)
;; - claude--check-worktree, claude--check-doom-workspace, claude--check-vterm-buffer
;; - claude--repair-doom-workspace, claude--repair-vterm-buffer
;; - claude--startup-reconcile
;; - claude--check-stalled-creations
;; - claude--recover-in-flight-workspaces
```

- [ ] Implement component checks
- [ ] Implement repair functions (uses claude-vterm)
- [ ] Handle transient states (skip creating/closing)
- [ ] Add startup reconciliation hook
- [ ] Add stalled creation detection (120s timeout)
- [ ] Add partial cleanup recovery
- [ ] Add auto-cleanup for failed workspaces > 24 hours old (no warning)

### Task 2.3: Create claude-reconcile-test.el

- [ ] Test healthy workspace detection
- [ ] Test missing worktree → broken
- [ ] Test missing doom workspace → repair
- [ ] Test missing vterm → repair
- [ ] Test transient state skipping
- [ ] Test stalled creation timeout

**Exit criteria:** `./test/run-reconciler-tests.sh` passes

---

## Phase 3: Creation and Cleanup Flows

**Goal:** Atomic operations with rollback and progress tracking.

**Success Criteria:**
- Creation failure rolls back all components (no orphaned worktrees/workspaces)
- Merge conflict → status becomes `stuck`, user can resolve and retry
- Dashboard `x` works on all states including `failed`, `broken`, `stuck`
- Home workspace creation/cleanup works without git operations
- Post-cleanup navigates to home workspace (or stays put if none)

### Task 3.1: Update claude-workspace.el for creation

- [ ] Require claude-state, claude-vterm
- [ ] Implement `claude--create-workspace-with-rollback`
- [ ] Add metadata status=creating at start
- [ ] Implement rollback stack
- [ ] Fire state-change-hook on completion
- [ ] Handle branch-exists flow (reuse prompt)

### Task 3.2: Update claude-cleanup.el

- [ ] Implement status buffer UI
- [ ] Add cleanup_progress tracking in metadata
- [ ] Implement safe merge protocol (fetch → forward merge → reverse merge)
- [ ] Handle stuck state on conflict
- [ ] Implement force cleanup for broken/stuck workspaces
- [ ] Fire state-change-hook on transitions

### Task 3.3: Add home workspace support

- [ ] `claude-home-workspace` command
- [ ] Create with type=home, branch=__home__
- [ ] Skip worktree creation for home
- [ ] Simpler cleanup (no merge flow)

### Task 3.4: Integration tests

- [ ] Test full lifecycle (create → work → merge → cleanup)
- [ ] Test broken detection after manual worktree delete
- [ ] Test conflict handling → stuck → resolve → retry
- [ ] Test home workspace lifecycle
- [ ] Test crash recovery scenarios

**Exit criteria:** `./test/run-integration-tests.sh` passes

---

## Phase 4: Monitor and UI

**Goal:** Event-driven UI updates, attention detection.

**Success Criteria:**
- Dashboard shows correct symbols for all 6 states plus attention
- Dashboard auto-refreshes on state/attention changes (no manual `g` needed)
- Modeline shows `●` when any workspace needs attention
- `SPC C r` repairs broken workspaces (recreates Doom/vterm)
- Monitor only checks idle buffers (no false positives during output)

### Task 4.1: Update claude-monitor.el

- [ ] Subscribe to state-change-hook
- [ ] Skip attention checks for non-active workspaces
- [ ] Clear attention on workspace switch
- [ ] Track buffer activity per workspace
- [ ] Auto-start on Doom init if workspaces exist

### Task 4.2: Update claude-dashboard.el

- [ ] Subscribe to state-change-hook for auto-refresh
- [ ] Show all workspace states with correct symbols
- [ ] Allow close (x) on broken/stuck workspaces
- [ ] Add repair (r) keybinding in dashboard

### Task 4.3: Update claude.el

- [ ] Add `SPC C r` for repair command
- [ ] Update require statements for new files
- [ ] Verify all keybindings work

### Task 4.4: Add modeline segment

- [ ] Implement `claude-modeline-segment`
- [ ] Subscribe to state-change-hook
- [ ] Click → jump to needy workspace or dashboard

**Exit criteria:** Manual testing of full workflow

---

## Known Limitations (Document, Don't Fix Now)

1. **Repo name collisions** — Same-named repos from different paths conflict
2. **vterm activity tracking** — May need adjustment based on testing
3. **Safe merge race condition** — Remote could change between forward/reverse merge
4. **No remote tracking check** — Fetch may fail if branch has no upstream

---

## Testing Commands

```bash
# Phase 1
emacs --batch -l ert -l claude-state.el -l test/claude-state-test.el -f ert-run-tests-batch-and-exit

# Phase 2
emacs --batch -l ert -l claude-state.el -l claude-reconcile.el -l test/claude-reconcile-test.el -f ert-run-tests-batch-and-exit

# Phase 3+4 (requires Doom)
doom run -l test/claude-integration-test.el --eval "(ert-run-tests-batch-and-exit)"
```
