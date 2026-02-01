# Commands & Keybindings

## Primary Commands (SPC C prefix)

| Key | Command | Description |
|-----|---------|-------------|
| `SPC C c` | `claude-create-workspace` | Create new worktree + workspace + Claude session |
| `SPC C d` | `claude-dashboard` | Open dashboard showing all Claude workspaces |
| `SPC C j` | `claude-jump-to-buffer` | Jump to Claude buffer in current workspace |
| `SPC C x` | `claude-close-workspace` | Status-aware cleanup flow |
| `SPC C m` | `claude-monitor-toggle` | Toggle attention monitoring on/off |
| `SPC C g` | `claude-magit-status` | Open magit in current worktree |

## Workspace Switching

| Key | Command | Description |
|-----|---------|-------------|
| `SPC 1-9` | Built-in | Jump to workspace by number |
| `SPC TAB l` | Built-in | List all workspaces |

## Dashboard Mode Bindings

When in the Claude dashboard buffer:

| Key | Action |
|-----|--------|
| `j` / `k` | Move down / up |
| `RET` | Jump to workspace under cursor |
| `c` | Create new workspace |
| `x` | Close workspace under cursor (with merge check) |
| `g` | Refresh dashboard |
| `/` | Filter by repo (toggle) |
| `q` | Quit dashboard |

## Command Details

### claude-create-workspace

1. Detects repo from current buffer (or prompts if not in a repo)
2. Prompts for branch name
3. Optionally prompts for description
4. Creates git worktree at `~/worktrees/{repo}/{branch}`
5. Writes metadata JSON
6. Creates Doom workspace `{repo}:{branch}`
7. Opens vterm, starts `claude` command
8. Starts monitor if not running

### claude-close-workspace

1. Reads metadata for parent branch info
2. Compares current branch to parent (commits ahead/behind)
3. Shows status with options:
   - `[v]` View diff in magit
   - `[m]` Merge and cleanup
   - `[d]` Delete without merging (requires confirmation if unmerged)
   - `[c]` Cancel
4. On merge/delete: kills buffer, removes workspace, deletes worktree, removes metadata

### claude-dashboard

Opens a dedicated buffer showing all Claude workspaces:
- Status indicator (needs attention / active / idle)
- Workspace name
- Commits ahead of parent
- Parent branch name

### claude-monitor-toggle

- Starts/stops a 2-second timer that scans all `*claude:*` buffers
- Auto-starts when first workspace is created
- Auto-stops when last workspace is closed
