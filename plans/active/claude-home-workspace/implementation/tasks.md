# Implementation Tasks

## Phase 1: Home Workspace Core

- [ ] **1.1: Add home workspace utilities to claude-workspace.el**
  - `claude-home-workspace-p` - check if workspace name ends in `:home`
  - `claude-home-exists-p` - check if `repo:home` workspace exists
  - `claude-get-repo-from-worktree` - extract parent repo from worktree metadata
  - Success: Functions work in IELM

- [ ] **1.2: Implement claude-home-workspace command**
  - Detect current repo (handle both main repo and worktree cases)
  - If home exists, switch to it
  - If not, create workspace with Claude in main repo
  - Start monitor if not running
  - Add `SPC C h` keybinding
  - Success: `SPC C h` creates/jumps to home from anywhere

## Phase 2: Extra Terminals

- [ ] **2.1: Implement claude-new-terminal command**
  - `claude-terminal-buffer-name` - generate `*term:repo:branch:N*`
  - Count existing terminals, increment N
  - Create vterm buffer, cd to workspace directory
  - Add `SPC C t` keybinding
  - Success: `SPC C t` spawns new terminal in current workspace

## Phase 3: Cleanup & Dashboard

- [ ] **3.1: Update cleanup for home workspaces**
  - Detect home workspace in `claude-close-workspace`
  - Check `git status` for uncommitted changes
  - Prompt if dirty, close immediately if clean
  - Kill all terminal buffers for the workspace
  - No merge flow for home
  - Success: `SPC C x` on home warns if dirty, closes cleanly

- [ ] **3.2: Update dashboard for home workspaces**
  - Add `⌂` prefix for home workspaces in render
  - Home workspaces sort to top of their repo group
  - Success: Dashboard shows `⌂ rithmly:home` with indicator

## Phase 4: Polish

- [ ] **4.1: Update documentation**
  - Update `claude/CLAUDE.md` with new keybindings
  - Add home workspace section to workflow docs
  - Success: Docs reflect new functionality

## Success Criteria (Overall)

1. `SPC C h` jumps to home (creates if needed) from anywhere in repo
2. `SPC C h` from worktree jumps to parent repo's home
3. `SPC C t` spawns numbered terminals in any workspace
4. `SPC C x` on home checks for dirty state, no merge flow
5. Dashboard shows home workspaces with `⌂` indicator
