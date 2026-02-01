# Doom Emacs Multi-Claude Workflow

## Problem

Running multiple Claude Code instances in parallel across different repos and worktrees, with no easy way to:
- Manage all sessions from one place
- Know when a Claude needs attention
- Safely merge and clean up when done

## Solution

A modular Doom Emacs package that provides:
- **Workspace management** - Create/switch/close Claude sessions tied to git worktrees
- **Attention monitoring** - Detect when Claude is waiting for input, show in modeline
- **Dashboard** - See all active sessions, their status, quick navigation
- **Safe cleanup** - Merge-aware workflow that prevents losing unmerged work

## Key Design Decisions

1. **Centralized worktrees** - All worktrees live in `~/worktrees/{repo}/{branch}/` regardless of source repo
2. **Metadata tracking** - Parent branch stored in `~/worktrees/metadata/{repo}/{branch}.json`
3. **Context-aware creation** - Detects current repo, branches from current HEAD
4. **Flat workspace model** - All sessions are peers named `repo:branch`, filterable by repo
5. **Pattern-based attention detection** - Scan last 15 lines for Claude prompts after 3s idle

## Related Documents

- [Data Model](./data-model.md) - Directory structure and metadata format
- [Commands](./commands.md) - Keybindings and command reference
- [Monitor](./monitor.md) - Attention detection system
- [Flows](./flows.md) - Creation and cleanup workflows
- [File Structure](./file-structure.md) - Elisp module organization
