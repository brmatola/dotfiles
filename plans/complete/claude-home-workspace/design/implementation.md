# Implementation Details

## Files to Modify

| File | Changes |
|------|---------|
| `claude.el` | Add keybindings for `SPC C h` and `SPC C t` |
| `claude-workspace.el` | Add home workspace and terminal functions |
| `claude-cleanup.el` | Handle home workspace cleanup (dirty check, no merge flow) |
| `claude-dashboard.el` | Show home workspaces with `⌂` indicator |

## New Functions

### claude-workspace.el

```elisp
claude-home-workspace          ; Interactive: jump/create home
claude-home-workspace-p (name) ; Predicate: is this a home workspace? (checks for :__home__ suffix)
claude-home-exists-p (repo)    ; Check if home exists for repo
claude-get-repo-from-worktree  ; Get parent repo when in a worktree (from metadata)
claude-workspace-path (ws)     ; Get working dir for workspace (main repo for home, worktree path otherwise)
claude-new-terminal            ; Interactive: spawn terminal in current workspace
claude-terminal-buffer-name    ; Generate terminal buffer name (finds first gap in numbering)
```

### Key Logic

**Detecting repo from worktree:**
```elisp
;; 1. Get current git toplevel: git rev-parse --show-toplevel
;; 2. Check if we're in a worktree by comparing:
;;    - git rev-parse --git-dir (e.g. /path/to/worktree/.git)
;;    - git rev-parse --git-common-dir (e.g. /path/to/main/.git)
;;    If different, we're in a worktree
;; 3. If in worktree, read metadata from ~/worktrees/metadata/{repo}/{branch}.json
;;    The :parent_repo field has the main repo path
;; 4. If no metadata (worktree created outside system), fail gracefully:
;;    "Not in a Claude-managed worktree. Use SPC C h from the main repo."
```

**Home workspace creation:**
```elisp
;; Similar to worktree workspace but:
;; - No worktree creation
;; - No metadata file
;; - Working directory is the main repo itself
;; - Workspace name is always "repo:__home__" (namespaced to avoid branch conflicts)
```

**New helper - `claude-workspace-path`:**
```elisp
;; Returns the working directory for a workspace:
;; - For home workspaces (:__home__): return the main repo path (from projectile or locate-dominating-file)
;; - For worktree workspaces: return ~/worktrees/{repo}/{branch}
;; Used by claude-magit-status and anywhere we need the workspace's git root
```

**Terminal numbering:**
```elisp
;; When spawning new terminal:
;; 1. List existing buffers matching *term:{repo}:{branch}:*
;; 2. Extract numbers, find first gap starting from 1
;; 3. If no gaps, use max+1
;; Example: existing [1,3,4] → next is 2
```

## Edge Cases

| Situation | Behavior |
|-----------|----------|
| `SPC C h` not in any repo | Error: "Not in a git repository" |
| `SPC C h` from Claude-managed worktree | Detect parent repo from metadata, jump to its home |
| `SPC C h` from non-Claude worktree | Error: "Not in a Claude-managed worktree. Use SPC C h from the main repo." |
| `SPC C c` when home exists | Works normally - creates worktree workspace (they coexist) |
| Close home with running terminals | Kill all `*term:repo:__home__:*` buffers too |
| `SPC C t` not in Claude workspace | Error: "Not in a Claude workspace" |

## Home vs Worktree Comparison

| Aspect | Home | Worktree |
|--------|------|----------|
| Location | Main repo (`~/repos/rithmly`) | Worktree (`~/worktrees/rithmly/feat-x`) |
| Naming | `rithmly:__home__` | `rithmly:feat-x` |
| Metadata | None needed | JSON in `~/worktrees/metadata/` |
| Cleanup | Just close (warn if dirty) | Merge-aware flow |
| Purpose | Coordination, planning | Isolated feature work |
| Identification | Branch name is `__home__` | Has metadata file |

## Dashboard Display

```
Claude Workspaces

⌂ rithmly              ● (attention needed)
  rithmly:feat-auth    ○
  rithmly:fix-bug      ○
⌂ dotfiles             ○
```

- Home workspaces get `⌂` prefix and display just the repo name (not `repo:__home__`)
- Home workspaces sort to top of the list
- Visual distinction makes it easy to identify home vs worktree sessions
