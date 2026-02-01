# Commands & Keybindings

## New Commands

| Key | Command | Description |
|-----|---------|-------------|
| `SPC C h` | `claude-home-workspace` | Jump to home for current repo. Creates if doesn't exist. |
| `SPC C t` | `claude-new-terminal` | Spawn new terminal in current workspace. |

## `SPC C h` Behavior

1. Detect current repo (from current buffer or worktree)
2. If home workspace `repo:home` exists → switch to it
3. If not → create workspace, open Claude in main repo, switch to it
4. Start monitor if not running

**From a worktree:** Detects parent repo from worktree metadata and jumps to that repo's home.

## `SPC C t` Behavior

1. Must be in a Claude workspace (home or worktree)
2. Count existing terminals for this workspace
3. Create `*term:repo:branch:N*` where N is next number
4. Switch to the new terminal buffer

## Updated `SPC C x` for Home

1. If closing a home workspace → check `git status` for uncommitted changes
2. If dirty → prompt "Uncommitted changes. Close anyway? (y/n)"
3. If clean or confirmed → kill buffers, delete workspace (no merge flow)

## Naming Conventions

| Type | Workspace Name | Claude Buffer | Terminal Buffers |
|------|----------------|---------------|------------------|
| Home | `rithmly:home` | `*claude:rithmly:home*` | `*term:rithmly:home:1*`, `*term:rithmly:home:2*` |
| Worktree | `rithmly:feat-x` | `*claude:rithmly:feat-x*` | `*term:rithmly:feat-x:1*` |
