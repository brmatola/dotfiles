# Gremlins Integration

Last updated: 2026-02-01

## Problem Statement

Gremlins lives in `~/repos/gremlins` and is installed as a Claude Code plugin. Every edit requires `claude plugins update gremlins`. This creates:

1. **Update friction** - Changes don't take effect until plugin update runs
2. **Split history** - Gremlins changes aren't in dotfiles git history
3. **Parallel systems** - Emacs module and gremlins both manage worktrees independently

## Solution

Two-phase integration:

### Phase 1: Import
Copy skills, agents, commands into dotfiles. Symlink to `~/.claude/`. Uninstall plugin.

### Phase 2: Emacs Harmony
Adjust skills to use Emacs conventions: `~/worktrees/` location, shared metadata, Emacs handles final merge/PR execution.

## Success Criteria

**Phase 1:**
- [ ] All skills in `dotfiles/claude/skills/`
- [ ] All agents in `dotfiles/claude/agents/`
- [ ] Commands in `dotfiles/claude/commands/`
- [ ] `install.sh` symlinks all three
- [ ] Skills invoke correctly (test with `/worktree status`)
- [ ] Plugin uninstalled

**Phase 2:**
- [ ] `using-git-worktrees` detects Emacs-managed worktrees, skips creation
- [ ] `worktree-workflow` writes state to Emacs metadata location
- [ ] `finishing-a-development-branch` reports readiness, tells user to use `SPC C x`
- [ ] Dashboard shows workflow phase
- [ ] Elisp tests pass

## Documents

**Design:**
- [Phase 1: Import](./phase-1-import.md)
- [Phase 2: Emacs Harmony](./phase-2-emacs-harmony.md)

**Implementation:**
- [Phase 1 Tasks](../implementation/phase-1-tasks.md)
- [Phase 2 Tasks](../implementation/phase-2-tasks.md)
