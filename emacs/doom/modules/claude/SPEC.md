# Claude Module Specification

Last updated: 2026-02-01

## Overview

This module manages multiple Claude Code sessions in parallel using Doom workspaces and git worktrees.

---

## Core Concepts

### Workspace Types

| Type | Name Pattern | Directory | Metadata |
|------|--------------|-----------|----------|
| Home | `{repo}:__home__` | Main repo (e.g., `~/repos/rithmly`) | None |
| Worktree | `{repo}:{branch}` | `~/worktrees/{repo}/{branch}` | `~/worktrees/metadata/{repo}/{branch}.json` |

### Buffer Naming

| Buffer Type | Pattern | Example |
|-------------|---------|---------|
| Claude | `*claude:{repo}:{branch}*` | `*claude:rithmly:feat-auth*` |
| Terminal | `*term:{repo}:{branch}:{N}*` | `*term:rithmly:feat-auth:1*` |

---

## Commands

### `SPC C h` — Home Workspace

**Preconditions:**
- User is in a git repository (main repo or worktree)

**Behavior:**

| Context | Action |
|---------|--------|
| In main repo, home exists | Switch to home workspace |
| In main repo, home doesn't exist | Create home workspace |
| In Claude-managed worktree | Switch to parent repo's home |
| In non-Claude worktree | Error: "Not in a Claude-managed worktree" |
| Not in git repo | Error: "Not in a git repository" |

**Postconditions:**
- [ ] User is in `{repo}:__home__` workspace
- [ ] Treemacs shows main repo files
- [ ] Claude buffer is focused
- [ ] Monitor is running

---

### `SPC C c` — Create Worktree Workspace

**Preconditions:**
- User is in a git repository
- User provides a branch name

**Behavior:**

| Context | Action |
|---------|--------|
| Branch doesn't exist | Create branch + worktree + workspace |
| Branch exists, user confirms reuse | Create worktree from existing branch |
| Branch exists, user declines | Cancel |
| Worktree dir already exists | Error |

**Postconditions:**
- [ ] Git worktree exists at `~/worktrees/{repo}/{branch}`
- [ ] Metadata file exists at `~/worktrees/metadata/{repo}/{branch}.json`
- [ ] Metadata contains: `parent_branch`, `parent_repo`, `created`
- [ ] Doom workspace `{repo}:{branch}` exists
- [ ] User is in that workspace
- [ ] Treemacs shows worktree files
- [ ] Claude buffer exists and is focused
- [ ] Monitor is running

---

### `SPC C x` — Close Workspace

#### For Home Workspace (`{repo}:__home__`)

**Behavior:**
1. Check for uncommitted changes via `git status --porcelain`
2. If dirty: prompt "Uncommitted changes. Close anyway?"
3. If clean or confirmed: proceed with cleanup

**Cleanup steps:**
- [ ] Kill Claude buffer `*claude:{repo}:__home__*`
- [ ] Kill all terminal buffers `*term:{repo}:__home__:*`
- [ ] Switch to another workspace (if any) or default
- [ ] Kill the Doom workspace (remove from tab bar)

**Postconditions:**
- [ ] Workspace not in `(+workspace-list-names)`
- [ ] No buffers matching `*claude:{repo}:__home__*`
- [ ] No buffers matching `*term:{repo}:__home__:*`

#### For Worktree Workspace (`{repo}:{branch}`)

**Behavior:**
1. Read metadata to get parent branch/repo
2. Count commits ahead of parent
3. Show status buffer with options

**Status buffer shows:**
```
Workspace: {repo}:{branch}
Parent: {parent_branch}
Status: {N} commits ahead | No commits ahead (merged)

[v] View diff
[m] Merge & cleanup
[d] Delete
[c] Cancel
```

**Commits ahead calculation:**
```bash
git rev-list --count {parent_branch}..{branch}
```
- If 0: branch is merged (or has no unique commits)
- If >0: branch has unmerged commits

**[m] Merge & cleanup:**
1. In parent repo, merge the branch: `git merge {branch}`
2. If merge succeeds: proceed with cleanup
3. If merge fails: show error, user should resolve in magit

**[d] Delete:**
1. If commits ahead > 0: confirm "Delete N unmerged commits?"
2. Proceed with cleanup (no merge)

**Cleanup steps:**
- [ ] Kill Claude buffer
- [ ] Kill all terminal buffers
- [ ] Switch to another workspace
- [ ] Kill the Doom workspace
- [ ] Remove git worktree: `git worktree remove {path}`
- [ ] Delete branch from parent repo (optional, if merged)
- [ ] Delete metadata file

**Postconditions:**
- [ ] Workspace not in `(+workspace-list-names)`
- [ ] No buffers matching `*claude:{repo}:{branch}*`
- [ ] No buffers matching `*term:{repo}:{branch}:*`
- [ ] Worktree directory doesn't exist
- [ ] Metadata file doesn't exist

---

### `SPC C d` — Dashboard

**Behavior:**
- Show all Claude workspaces
- Home workspaces: `⌂ {repo}` (sorted to top)
- Worktree workspaces: `  {repo}:{branch}`
- Attention indicator: `●` if needs attention, `○` if idle

**Keybindings (in dashboard buffer):**

| Key | Action | Postcondition |
|-----|--------|---------------|
| `j` | Move down | Point on next workspace line |
| `k` | Move up | Point on previous workspace line |
| `RET` | Select | Switched to workspace, dashboard closed |
| `c` | Create | Prompts for new workspace |
| `x` | Close | Opens cleanup for workspace at point |
| `g` | Refresh | Dashboard re-rendered |
| `/` | Filter | Prompts for repo filter |
| `q` | Quit | Dashboard closed |

---

### `SPC C t` — New Terminal

**Preconditions:**
- User is in a Claude workspace

**Behavior:**
1. Find existing terminal buffers for this workspace
2. Find first gap in numbering (1, 2, 3...) or use max+1
3. Create vterm buffer with that name
4. cd to workspace directory

**Example:**
- Existing: `*term:rithmly:feat:1*`, `*term:rithmly:feat:3*`
- Next created: `*term:rithmly:feat:2*` (fills gap)

**Postconditions:**
- [ ] New terminal buffer exists
- [ ] Buffer is in vterm-mode
- [ ] Working directory is workspace path
- [ ] Buffer is focused

---

### `SPC C j` — Jump to Claude

**Preconditions:**
- User is in a Claude workspace

**Behavior:**
- Switch to the Claude buffer for current workspace

**Postconditions:**
- [ ] Claude buffer is focused

---

### `SPC C g` — Magit Status

**Preconditions:**
- User is in a Claude workspace

**Behavior:**
- Open magit-status in the workspace's directory
- Home workspace: main repo
- Worktree workspace: worktree directory

---

## Workspace Switching

When switching between workspaces (via dashboard, `SPC TAB TAB`, etc.):

**Postconditions:**
- [ ] Doom workspace is active
- [ ] Claude buffer for that workspace is focused
- [ ] Point is at end of buffer (no scrollback replay)
- [ ] Treemacs shows correct project (if visible)

---

## Edge Cases

### Orphaned Worktrees

A worktree directory exists but:
- No metadata file, OR
- Git doesn't recognize it (`git worktree list` doesn't show it)

**Behavior:**
- Cleanup should still work (delete what exists, ignore what doesn't)
- `SPC C h` from orphaned worktree: error with guidance

### Orphaned Workspaces

A Doom workspace exists but:
- No corresponding worktree directory, OR
- No Claude buffer

**Behavior:**
- Dashboard should still show it
- Cleanup should remove it cleanly

### Concurrent Sessions

Multiple Emacs frames/sessions editing same repo:
- Each manages its own workspaces
- Worktree conflicts possible (first-come-first-served)

---

## Test Checklist

### Unit Tests (batch mode, no Doom)

- [x] `claude-workspace-name` generates correct format
- [x] `claude-buffer-name` generates correct format
- [x] `claude-parse-workspace-name` parses valid names
- [x] `claude-parse-workspace-name` returns nil for invalid
- [x] `claude-home-workspace-p` detects home workspaces
- [x] `claude-repo-name` extracts repo from path
- [x] `claude-metadata-path` generates correct path
- [x] `claude-worktree-path` generates correct path
- [x] Metadata write/read/delete roundtrip
- [x] Terminal buffer naming with gap reuse

### Integration Tests (need Doom)

- [ ] Create home workspace from main repo
- [ ] Create home workspace from worktree
- [ ] Create worktree workspace (new branch)
- [ ] Create worktree workspace (existing branch)
- [ ] Switch workspace focuses correct buffer
- [ ] Close home workspace (clean)
- [ ] Close home workspace (dirty, confirm)
- [ ] Close worktree workspace (merged)
- [ ] Close worktree workspace (unmerged, merge)
- [ ] Close worktree workspace (unmerged, delete)
- [ ] Dashboard navigation works
- [ ] Dashboard select switches and closes
- [ ] New terminal fills gaps
- [ ] Keybindings work in dashboard
- [ ] Keybindings work in cleanup buffer

---

## Current Known Issues

1. **"Branch already merged" false positive** — Need to verify git logic
2. **Keybindings intermittent** — Evil local bindings may not always take effect
3. **Treemacs not always rooted correctly** — Need to verify project detection
4. **UI locks during cleanup** — No feedback while operations run, should show progress
