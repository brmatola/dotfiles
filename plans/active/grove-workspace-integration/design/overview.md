# Grove Workspace Integration: Overview

Last updated: 2026-02-13

## Problem Statement

The Emacs Claude module (`emacs/doom/modules/claude/`) currently owns the full worktree lifecycle: creation, state management, reconciliation, merge/cleanup. This is ~1200 lines of elisp handling git operations, state machines, and error recovery.

Grove is gaining a `workspace` subcommand that handles all of this as a CLI tool with `--json` output. The Emacs module needs to become a thin UI layer over grove instead of reimplementing everything.

## Dependency

**This plan blocks on:** Grove workspace management being implemented and working.
See: `~/repos/twiglylabs/grove/plans/active/workspace-management/`

## Scope

Replace the Emacs Claude module's worktree/state management with calls to `grove workspace`. Keep the Emacs-specific UI (Doom workspaces, vterm buffers, dashboard, modeline, keybindings).

## What Changes

| Component | Current | After |
|-----------|---------|-------|
| Worktree creation | `claude-worktree.el` calls git directly | Emacs calls `grove workspace create --json` |
| State management | `claude-state.el` reads/writes JSON metadata | Emacs calls `grove workspace status --json` |
| Merge/cleanup | `claude-cleanup.el` orchestrates git merge/worktree removal | Emacs calls `grove workspace close --json` |
| Sync | Not supported | Emacs calls `grove workspace sync --json` |
| Reconciliation | `claude-reconcile.el` detects drift, auto-repairs | Removed — grove owns state, Emacs just renders it |
| Dashboard | `claude-dashboard.el` reads metadata directly | Dashboard calls `grove workspace list --json` |

## What Stays the Same

- **Doom workspace management** — creating/switching Doom workspaces is Emacs-only
- **vterm buffer management** — creating/killing vterm buffers is Emacs-only
- **Modeline** — attention detection stays in Emacs (reads vterm buffer output)
- **Keybindings** — `SPC C` prefix stays
- **Monitor** — process monitoring stays in Emacs

## Architecture After

```
┌──────────────────────────────────────────────────┐
│                  Emacs (UI layer)                  │
│  Doom workspaces, vterm buffers, dashboard,       │
│  modeline, keybindings                            │
└────────────────────┬─────────────────────────────┘
                     │ shell-command / async-shell-command
                     │ grove workspace <cmd> --json
                     ▼
┌──────────────────────────────────────────────────┐
│              Grove CLI (source of truth)           │
│  Git worktrees, branch management, state,         │
│  sync, merge/discard                              │
└──────────────────────────────────────────────────┘
```

## Design Principles

1. **Grove is the source of truth** — Emacs never writes to `~/.grove/workspaces/` directly
2. **Emacs caches grove output** — Parse JSON once, cache in buffer-local or module-level vars, refresh on events
3. **Async where possible** — `grove workspace create` and `grove workspace sync` can take seconds; run async and update UI on completion
4. **Degrade gracefully** — If `grove` binary not found, show helpful error, don't crash

## Rough Task Breakdown

This is intentionally light — detailed implementation tasks will be written once grove workspace management is working and the CLI contract is finalized.

### Phase 1: Grove integration layer
- Create `claude-grove.el` — thin wrapper that shells out to `grove workspace --json` and parses responses
- Define elisp types/structs matching grove's JSON output
- Error handling (grove not found, command failed, parse error)

### Phase 2: Replace state management
- Remove `claude-state.el` state machine and metadata read/write
- Dashboard and status calls go through `claude-grove.el`
- Remove `claude-reconcile.el` entirely

### Phase 3: Replace worktree lifecycle
- `claude-create-workspace` calls `grove workspace create`
- `claude-close-workspace` calls `grove workspace close`
- New sync UI: `claude-sync-workspace` calls `grove workspace sync`
- Remove direct git operations from `claude-worktree.el` and `claude-cleanup.el`

### Phase 4: Multi-repo support in UI
- Dashboard shows grouped workspaces with per-repo status
- Create workspace from grouped repo auto-detects children
- Sync shows per-repo progress
- Close shows per-repo merge status

### Phase 5: Cleanup
- Remove dead code (old state machine, reconciliation, direct git ops)
- Update tests to mock grove CLI instead of git
- Update CLAUDE.md module documentation
