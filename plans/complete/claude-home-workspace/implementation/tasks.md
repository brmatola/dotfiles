# Implementation Tasks

## Phase 1: Home Workspace Core

- [x] **1.1: Add home workspace utilities to claude-workspace.el**
  - `claude-home-workspace-p` - check if workspace name ends in `:__home__`
  - `claude-home-exists-p` - check if `repo:__home__` workspace exists in workspace list
  - `claude-get-repo-from-worktree` - read `:parent_repo` from worktree metadata; if no metadata, return nil (fail gracefully)
  - `claude-workspace-path` - return working dir for workspace: main repo path for home, `claude-worktree-path` for others
  - Success: Functions work in IELM

- [x] **1.2: Implement claude-home-workspace command**
  - Detect if in worktree vs main repo:
    - Compare `git rev-parse --git-dir` vs `--git-common-dir`
    - If different → in worktree → get parent from metadata
    - If no metadata → error "Not in a Claude-managed worktree. Use SPC C h from the main repo."
    - If same → in main repo → use current path
  - If home exists (`repo:__home__`), switch to it
  - If not, create workspace with Claude in main repo (no worktree, no metadata)
  - Start monitor if not running
  - Add `SPC C h` keybinding
  - Success: `SPC C h` creates/jumps to home from anywhere

- [x] **1.3: Update claude-magit-status to use claude-workspace-path**
  - Replace `claude-worktree-path` call with `claude-workspace-path`
  - Works for both home and worktree workspaces
  - Success: `SPC C g` works in home workspace

## Phase 2: Extra Terminals

- [x] **2.1: Implement claude-new-terminal command**
  - `claude-terminal-buffer-name` - generate `*term:repo:branch:N*`
    - List buffers matching `*term:{repo}:{branch}:*`
    - Extract numbers, find first gap starting from 1
    - If no gaps, use max+1
  - Create vterm buffer with generated name
  - cd to workspace directory (use `claude-workspace-path`)
  - Add `SPC C t` keybinding
  - Success: `SPC C t` spawns new terminal in current workspace, reuses gaps in numbering

## Phase 3: Cleanup & Dashboard

- [x] **3.1: Update cleanup for home workspaces**
  - Detect home workspace in `claude-close-workspace` using `claude-home-workspace-p`
  - If home: check `git status` for uncommitted changes
  - Prompt if dirty ("Uncommitted changes. Close anyway?"), close immediately if clean
  - Kill all terminal buffers matching `*term:{repo}:__home__:*`
  - No merge flow, no metadata cleanup (home has none)
  - Success: `SPC C x` on home warns if dirty, closes cleanly

- [x] **3.2: Update dashboard for home workspaces**
  - Add `⌂` prefix for home workspaces in render
  - Display just repo name for homes (not `repo:__home__`)
  - Sort home workspaces to top of list
  - Success: Dashboard shows `⌂ rithmly` with attention indicator

## Phase 4: Polish

- [x] **4.1: Update documentation**
  - Update `claude/CLAUDE.md` with new keybindings
  - Add home workspace section to workflow docs
  - Success: Docs reflect new functionality

## Success Criteria (Overall)

1. [x] `SPC C h` jumps to home (creates if needed) from anywhere in main repo
2. [x] `SPC C h` from Claude-managed worktree jumps to parent repo's home
3. [x] `SPC C h` from non-Claude worktree shows helpful error message
4. [x] `SPC C t` spawns numbered terminals (reusing gaps) in any workspace
5. [x] `SPC C g` (magit) works in both home and worktree workspaces
6. [x] `SPC C x` on home checks for dirty state, no merge flow
7. [x] Dashboard shows home workspaces with `⌂` prefix, sorted to top
