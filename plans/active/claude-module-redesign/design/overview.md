# Claude Module Redesign: Overview

Last updated: 2026-02-01

## Problem Statement

The current Claude module for Doom Emacs has fundamental state management issues:

1. **Buffer confusion** — Claude buffers/terminals from one workspace appear in another
2. **State leakage** — Monitor/attention state gets mixed between workspaces
3. **Navigation confusion** — Unclear which workspace you're in, dashboard doesn't reflect reality
4. **Cleanup failures** — Partial cleanups leave orphaned resources
5. **Merge unreliability** — No confidence that work actually gets merged back

Root cause: The three systems (git worktrees, Doom workspaces, vterm sessions) have different lifecycles and no clear synchronization model.

## Solution: Metadata-Driven State Machine

### Core Principle

**Metadata files are the single source of truth.** Git worktrees, Doom workspaces, and vterm buffers are views into that state, kept in sync by a reconciliation layer.

### Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                    Metadata Layer                           │
│         (Source of truth - JSON files with state)           │
└─────────────────────────────────────────────────────────────┘
                           │
                           ▼
┌─────────────────────────────────────────────────────────────┐
│                  Reconciliation Layer                       │
│    (Ensures git/doom/vterm match metadata state)            │
│    - Detects drift                                          │
│    - Auto-repairs ephemeral components (doom, vterm)        │
│    - Marks broken if git worktree missing                   │
└─────────────────────────────────────────────────────────────┘
                           │
          ┌────────────────┼────────────────┐
          ▼                ▼                ▼
    ┌──────────┐    ┌──────────┐    ┌──────────┐
    │   Git    │    │   Doom   │    │  vterm   │
    │ Worktree │    │Workspace │    │ Buffers  │
    └──────────┘    └──────────┘    └──────────┘
```

### Key Design Decisions

1. **Metadata with lifecycle state** — Each workspace has a status (creating/active/closing/broken/stuck/failed)
2. **Reconciliation on key events** — Startup, navigation, dashboard refresh
3. **Moderate repair strategy** — Auto-repair Doom/vterm (ephemeral), mark broken if git worktree missing
4. **Full workspace isolation** — Buffers are scoped to their workspace, no bleeding
5. **Safe merge flow** — Pre-flight conflict check, conflicts resolved in worktree
6. **Event-driven UI** — Dashboard/modeline refresh on state changes, not polling

## File Structure

```
claude/
├── claude.el                 # Entry point, keybindings, config
├── claude-state.el           # State machine, metadata operations
├── claude-reconcile.el       # Reconciliation logic
├── claude-worktree.el        # Git worktree operations only
├── claude-workspace.el       # Doom workspace operations only
├── claude-vterm.el           # vterm session management only
├── claude-monitor.el         # Attention detection
├── claude-dashboard.el       # Dashboard UI
├── claude-cleanup.el         # Merge and cleanup flow
└── test/
    ├── claude-state-test.el      # State machine tests
    ├── claude-reconcile-test.el  # Reconciler tests (mocked)
    └── claude-integration-test.el # Full flow tests
```

## Related Documents

- [State Model](./state-model.md) — State machine details
- [Reconciliation](./reconciliation.md) — How reconciliation works
- [Testing Strategy](./testing.md) — Test pyramid and approach
