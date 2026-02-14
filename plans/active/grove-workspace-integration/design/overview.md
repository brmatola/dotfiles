# Grove Workspace Integration: Overview

Last updated: 2026-02-14

## Problem Statement

Two problems to solve together:

1. **State ownership** — The Emacs Claude module owns the full worktree lifecycle (~1200 lines of elisp for git operations, state machines, error recovery). Grove now handles all of this via CLI. Emacs should become a thin UI layer.

2. **Navigation at scale** — `SPC TAB <number>` doesn't scale for multiple workspaces. No way to see at a glance what's live, what needs attention, or orchestrate from one place.

## Dependencies

- **Grove workspace management** — COMPLETE (implemented in grove)
- **Grove repo registry** — TODO (see `~/repos/twiglylabs/grove/plans/active/repo-registry/`)

## Solution: Mission Control Dashboard

### Three-Tier Workspace Model

```
Main (pure control plane — dashboard, no repo)
 │
 ├── dotfiles (home workspace — edit code, test, plan)
 │   ├── fix-zsh-path (worktree — Claude working)
 │   └── add-brew-packages (worktree — Claude working)
 │
 ├── acorn (home workspace — planning, manual testing)
 │   ├── feature-auth (worktree — Claude working)
 │   └── data-model-v2 (worktree — Claude working)
 │
 └── trellis (home workspace — edit code)
```

**Main** = Dashboard-only workspace. Not tied to a repo. Pure orchestration UI. This is where you live when you're not actively in a workspace. `SPC ;` always brings you here.

**Home workspaces** = Tied to a repo's main branch. For editing, planning, manual testing, running Claude for orchestration. Opened from the dashboard.

**Worktree workspaces** = Feature branches. Claude is working here. Only need you when Claude stops.

### Data Flow

```
claude-dashboard.el  →  claude-grove.el  →  grove repo list --json   (tier 1, instant)
       │                      │                     │
       │                      │                     │  Returns: repos + workspace summaries
       │                      │                     │           (no git stats)
       │                      │                     ▼
       │                      ├──────────→  grove workspace status --json
       │                      │              (tier 2, per workspace, has git stats)
       │                      │                     │
       │                      │                     │  Returns: per-repo dirty/commit counts
       │                      │                     ▼
       ├── Renders from cache (updated progressively by both tiers)
       └── Reads: claude-monitor.el (attention status per vterm buffer)
```

**Two-tier progressive loading.** Emacs never blocks on grove. Tier 1 (`grove repo list --json`) is instant — reads state files only, returns repo names and workspace summaries with no git stats. The dashboard renders immediately from tier 1 data (branch names, workspace status, attention indicators). Tier 2 (`grove workspace status <branch> --json`) fires in parallel for each active workspace and fills in commit counts and per-repo dirty/clean status as callbacks arrive. Each tier-2 callback updates the cache and triggers a re-render. The re-render is fast (no I/O — just repaints from cache), so multiple re-renders as tier-2 data trickles in are fine. Attention status (working/idle/error) comes from vterm buffer monitoring — always local and fast.

### Attention Model

- **Worktree workspaces** should always be chugging. If Claude is idle → needs attention.
- **Home workspaces** are user-driven. No attention signal.
- **Main** shows aggregate: "2 workspaces need attention" or all clear.

Detection stays in `claude-monitor.el` behind a clean interface. Dashboard calls `(claude-workspace-attention workspace-name)` → `working | idle | error | nil`. Module is swappable.
