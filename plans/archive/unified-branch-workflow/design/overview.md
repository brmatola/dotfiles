# Unified Branch Workflow

Last updated: 2026-02-01

## Problem Statement

Three related issues with the current setup:

1. **Treemacs doesn't refresh** when switching workspaces after merge
2. **Out-of-band merge errors** — branches merged externally cause `SPC C x` to fail
3. **Parallel systems** — gremlins skill and Emacs module do similar things independently

## Solution

One unified workflow with two entry points:

- **Claude (skill):** Prepares branch for integration (pull, rebase, resolve conflicts, run tests)
- **Emacs (SPC C x):** Executes the choice (merge/PR/keep/discard) and handles cleanup

```
┌─────────────────────────────────────────────────────────┐
│                    Workspace Metadata                    │
│         (source of truth - knows branch state)           │
└─────────────────────────────────────────────────────────┘
                           │
           ┌───────────────┴───────────────┐
           ▼                               ▼
   ┌──────────────────┐           ┌──────────────────┐
   │   Claude Skill   │           │   Emacs UI       │
   │   (prep work)    │           │   (execution)    │
   └──────────────────┘           └──────────────────┘
           │                               │
           │ "Branch ready"                │
           └──────────────►────────────────┘
                           │
              ┌─────────────────────────┐
              │   SPC C x presents:     │
              │   [m] Merge & cleanup   │
              │   [p] Push & create PR  │
              │   [k] Keep as-is        │
              │   [d] Discard           │
              └─────────────────────────┘
```

## Key Design Decisions

1. **Claude preps, user executes** — Claude handles tricky prep work (conflicts), user makes final call
2. **Defensive git checks** — Handle already-merged, deleted branches, missing worktrees gracefully
3. **Skills in dotfiles** — `dotfiles/claude/skills/` symlinked to `~/.claude/skills/`
4. **Treemacs refresh on switch** — Automatic, no manual intervention needed

## Related Documents

- [Options Behavior](./options.md) — The 4 options and their behavior
- [Emacs Changes](./emacs-changes.md) — UI and code changes
- [Skill Design](./skill.md) — The prep skill
- [Error Handling](./errors.md) — Edge cases and recovery
