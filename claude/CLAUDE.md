# Global Claude Instructions

## Principles

1. **Verify before claiming** - Run commands, see output, then claim results
2. **Test-first** - Write failing test before implementation
3. **Understand before fixing** - Find root cause, don't guess

## Skills Available

The skills in `~/.claude/skills/` are loaded automatically when relevant:

- `test-driven-development` - TDD workflow (red-green-refactor)
- `systematic-debugging` - Four-phase debugging framework
- `verification-before-completion` - Evidence before assertions

## Agents Available

Use these via the Task tool for delegation:

- `haiku-general-purpose` - Fast, cheap tasks (research, summarization)
- `sonnet-general-purpose` - Balanced tasks (analysis, implementation)
- `opus-general-purpose` - Complex reasoning tasks

## How to Expand

Add more skills: Copy SKILL.md files to `~/.claude/skills/<name>/SKILL.md`
Add more agents: Copy .md files to `~/.claude/agents/<name>.md`
Add hooks: Configure in `~/.claude/settings.json`

---

## Doom Emacs Multi-Claude Workflow

A system for managing multiple Claude Code sessions in parallel across different repos and worktrees.

### Concept

Each Claude session gets its own isolated git worktree and Doom workspace. A monitor watches all sessions and alerts you when any Claude needs input. A dashboard gives you a bird's-eye view of all active work.

### Directory Structure

```
~/worktrees/
├── {repo-name}/
│   └── {branch-name}/           # Git worktree checkout
└── metadata/
    └── {repo-name}/
        └── {branch-name}.json   # Parent branch, timestamps
```

### Keybindings

All commands under `SPC C` prefix:

| Key | Command | Description |
|-----|---------|-------------|
| `SPC C c` | Create workspace | New worktree + workspace + Claude session |
| `SPC C d` | Dashboard | See all sessions, status, navigate |
| `SPC C j` | Jump to Claude | Focus Claude buffer in current workspace |
| `SPC C x` | Close workspace | Merge-aware cleanup with confirmation |
| `SPC C m` | Toggle monitor | Start/stop attention detection |
| `SPC C g` | Magit status | Open magit in current worktree |

### Dashboard Keys

| Key | Action |
|-----|--------|
| `j/k` | Navigate up/down |
| `RET` | Jump to workspace |
| `c` | Create new workspace |
| `x` | Close workspace (with merge check) |
| `g` | Refresh |
| `/` | Filter by repo |
| `q` | Quit |

### Workflow

**Starting work:**
1. Open any file in the repo you want to work on
2. `SPC C c` to create a new workspace
3. Enter branch name when prompted
4. Claude session starts automatically in the new worktree

**Monitoring:**
- Modeline shows "Claude" indicator when any session needs attention
- Click indicator (or `SPC C d`) to jump to the needy session

**Finishing work:**
1. `SPC C x` to close the workspace
2. Review merge status (commits ahead/behind parent)
3. Choose: `[m]erge & cleanup`, `[d]elete`, `[v]iew diff`, or `[c]ancel`
4. Merge option: merges to parent branch, removes worktree, cleans up

### Modeline Status

The modeline shows Claude session status globally:

| Display | Meaning |
|---------|---------|
| (hidden) | No Claude sessions active |
| `○ Claude` | Sessions active, none need attention |
| `● Claude` | At least one session waiting for input |

Click the modeline indicator to jump to the first session needing attention.

### Troubleshooting

**"claude: command not found"**
- Run `npm install -g @anthropic-ai/claude-code`
- Or re-run `~/dotfiles/install.sh`

**"Not in a git repository"**
- `SPC C c` requires being in a git repo
- Navigate to any file in a repo first

**"Branch already exists"**
- Choose to reuse the existing branch or enter a different name

**Workspace not appearing in dashboard**
- Ensure the workspace was created with `SPC C c`, not manually
- Check `~/worktrees/metadata/` for the JSON file

**Monitor not detecting attention**
- Toggle monitor: `SPC C m` twice
- Patterns are checked after 3s of no output
- Check `claude-attention-patterns` variable for pattern list

**Merge conflicts during cleanup**
- Use `[v]iew diff` to see changes in magit
- Resolve conflicts in magit, then try cleanup again

### Files

The module lives in `~/.config/doom/modules/claude/`:

- `claude.el` - Entry point, keybindings, customization
- `claude-worktree.el` - Git worktree and metadata operations
- `claude-workspace.el` - Doom workspace management, vterm sessions
- `claude-monitor.el` - Attention detection, modeline segment
- `claude-dashboard.el` - Dashboard buffer UI
- `claude-cleanup.el` - Merge-aware cleanup workflow

### Customization

```elisp
;; Change worktree location (default: ~/worktrees)
(setq claude-worktree-dir "~/my-worktrees")

;; Change attention check interval (default: 2 seconds)
(setq claude-monitor-interval 3)

;; Change idle threshold before checking patterns (default: 3 seconds)
(setq claude-attention-idle-threshold 5)

;; Add custom attention patterns
(add-to-list 'claude-attention-patterns "Your pattern here")
```
