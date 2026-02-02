# Phase 1: Import Gremlins

Last updated: 2026-02-01

## Goal

Copy gremlins into dotfiles. No modifications - just move the code and verify it works.

## Tasks

### 1. Copy Content

```bash
# Skills (24 directories)
cp -r ~/repos/gremlins/skills/* ~/dotfiles/claude/skills/

# Agents (10 files)
mkdir -p ~/dotfiles/claude/agents
cp ~/repos/gremlins/agents/*.md ~/dotfiles/claude/agents/

# Commands (1 file)
mkdir -p ~/dotfiles/claude/commands
cp ~/repos/gremlins/commands/*.md ~/dotfiles/claude/commands/
```

### 2. Update install.sh

Add symlinks for agents and commands (skills already handled):

```bash
# After existing skills symlink
ln -sfn "$DOTFILES/claude/agents" "$HOME/.claude/agents"
ln -sfn "$DOTFILES/claude/commands" "$HOME/.claude/commands"
```

### 3. Run install.sh

```bash
./install.sh
```

### 4. Verify Before Uninstalling Plugin

**Critical: Test with both plugin AND symlinks active first.**

```bash
# Start claude in any repo
cd ~/some-repo
claude

# Test skill invocation
/worktree status
```

If skills work: proceed to uninstall.
If skills fail: debug before uninstalling (you can still rollback).

### 5. Uninstall Plugin

```bash
claude plugins uninstall gremlins
```

### 6. Verify After Uninstall

```bash
claude
/worktree status
```

## Rollback

If something breaks after uninstall:

```bash
# Reinstall plugin
claude plugins install ~/repos/gremlins

# Remove symlinks (install.sh will recreate on next run)
rm ~/.claude/skills ~/.claude/agents ~/.claude/commands
```

## Notes

- No skill modifications in this phase
- Phase 2 handles Emacs-specific adjustments
