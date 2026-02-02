# Phase 1 Implementation Tasks

## Prerequisites
- `~/repos/gremlins` exists with current skills/agents/commands
- `~/dotfiles` is the working directory

## Tasks

### Task 1: Create directory structure
```bash
mkdir -p ~/dotfiles/claude/skills
mkdir -p ~/dotfiles/claude/agents
mkdir -p ~/dotfiles/claude/commands
```

### Task 2: Copy skills
```bash
cp -r ~/repos/gremlins/skills/* ~/dotfiles/claude/skills/
```

Verify: `ls ~/dotfiles/claude/skills | wc -l` should show 24 directories.

### Task 3: Copy agents
```bash
cp ~/repos/gremlins/agents/*.md ~/dotfiles/claude/agents/
```

Verify: `ls ~/dotfiles/claude/agents | wc -l` should show 10 files.

### Task 4: Copy commands
```bash
cp ~/repos/gremlins/commands/*.md ~/dotfiles/claude/commands/
```

Verify: `ls ~/dotfiles/claude/commands` should show `worktree.md`.

### Task 5: Update install.sh

Add after existing skills symlink (around line 77):

```bash
# Only link agents if directory has content
if [ -d "$DOTFILES_DIR/claude/agents" ] && [ "$(ls -A "$DOTFILES_DIR/claude/agents" 2>/dev/null)" ]; then
    create_symlink "$DOTFILES_DIR/claude/agents" "$HOME/.claude/agents"
fi
# Only link commands if directory has content
if [ -d "$DOTFILES_DIR/claude/commands" ] && [ "$(ls -A "$DOTFILES_DIR/claude/commands" 2>/dev/null)" ]; then
    create_symlink "$DOTFILES_DIR/claude/commands" "$HOME/.claude/commands"
fi
```

### Task 6: Run install.sh
```bash
cd ~/dotfiles && ./install.sh
```

Verify symlinks:
```bash
ls -la ~/.claude/skills   # -> ~/dotfiles/claude/skills
ls -la ~/.claude/agents   # -> ~/dotfiles/claude/agents
ls -la ~/.claude/commands # -> ~/dotfiles/claude/commands
```

### Task 7: Test with plugin still installed
```bash
cd ~/some-test-repo
claude
# In Claude: /worktree status
```

Should work. If not, debug before proceeding.

### Task 8: Uninstall plugin
```bash
claude plugins uninstall gremlins
```

### Task 9: Final verification
```bash
cd ~/some-test-repo
claude
# In Claude: /worktree status
```

Should still work via symlinks.

### Task 10: Commit
```bash
cd ~/dotfiles
git add claude/skills claude/agents claude/commands install.sh
git commit -m "feat: import gremlins skills, agents, commands into dotfiles"
```

## Rollback (if needed)
```bash
claude plugins install ~/repos/gremlins
rm ~/.claude/skills ~/.claude/agents ~/.claude/commands
```
