# Commands & Keybindings

## New Commands

| Key | Command | Description |
|-----|---------|-------------|
| `SPC C h` | `claude-home-workspace` | Jump to home for current repo. Creates if doesn't exist. |
| `SPC C t` | `claude-new-terminal` | Spawn new terminal in current workspace. |

## `SPC C h` Behavior

1. Detect if in worktree vs main repo (compare `git rev-parse --git-dir` vs `--git-common-dir`)
2. **If in main repo:** use current path as repo
3. **If in Claude-managed worktree:** read parent repo from metadata
4. **If in non-Claude worktree:** error "Not in a Claude-managed worktree. Use SPC C h from the main repo."
5. If home workspace `repo:__home__` exists → switch to it
6. If not → create workspace, open Claude in main repo, switch to it
7. Start monitor if not running

## `SPC C t` Behavior

1. Must be in a Claude workspace (home or worktree)
2. List existing terminal buffers matching `*term:repo:branch:*`
3. Find first gap in numbering starting from 1 (e.g., if 1,3,4 exist → use 2)
4. Create `*term:repo:branch:N*` where N is the gap (or max+1 if no gaps)
5. cd to workspace directory, switch to the new terminal buffer

## Updated `SPC C x` for Home

1. Detect if closing a home workspace (using `claude-home-workspace-p`)
2. If home → check `git status` for uncommitted changes
3. If dirty → prompt "Uncommitted changes. Close anyway? (y/n)"
4. If clean or confirmed → kill Claude buffer + all terminal buffers, delete workspace
5. No merge flow, no metadata cleanup (home has none)

## Naming Conventions

| Type | Workspace Name | Claude Buffer | Terminal Buffers |
|------|----------------|---------------|------------------|
| Home | `rithmly:__home__` | `*claude:rithmly:__home__*` | `*term:rithmly:__home__:1*`, `*term:rithmly:__home__:2*` |
| Worktree | `rithmly:feat-x` | `*claude:rithmly:feat-x*` | `*term:rithmly:feat-x:1*` |

Note: Home uses `__home__` namespace to avoid conflicts with branches literally named "home".
