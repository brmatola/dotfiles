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
claude-home-workspace-p (name) ; Predicate: is this a home workspace?
claude-home-exists-p (repo)    ; Check if home exists for repo
claude-get-repo-from-worktree  ; Get parent repo when in a worktree
claude-new-terminal            ; Interactive: spawn terminal in current workspace
claude-terminal-buffer-name    ; Generate terminal buffer name
```

### Key Logic

**Detecting repo from worktree:**
```elisp
;; When in a worktree, get the parent repo path
;; Option 1: Read from worktree metadata (:parent_repo)
;; Option 2: Parse `git worktree list --porcelain`
```

**Home workspace creation:**
```elisp
;; Similar to worktree workspace but:
;; - No worktree creation
;; - No metadata file
;; - Working directory is the main repo itself
;; - Workspace name is always "repo:home"
```

## Edge Cases

| Situation | Behavior |
|-----------|----------|
| `SPC C h` not in any repo | Error: "Not in a git repository" |
| `SPC C h` from worktree | Detect parent repo from metadata, jump to its home |
| `SPC C c` when home exists | Works normally - creates worktree workspace (they coexist) |
| Close home with running terminals | Kill all `*term:repo:home:*` buffers too |
| `SPC C t` not in Claude workspace | Error: "Not in a Claude workspace" |

## Home vs Worktree Comparison

| Aspect | Home | Worktree |
|--------|------|----------|
| Location | Main repo (`~/repos/rithmly`) | Worktree (`~/worktrees/rithmly/feat-x`) |
| Naming | `rithmly:home` | `rithmly:feat-x` |
| Metadata | None needed | JSON in `~/worktrees/metadata/` |
| Cleanup | Just close (warn if dirty) | Merge-aware flow |
| Purpose | Coordination, planning | Isolated feature work |

## Dashboard Display

```
Claude Workspaces

⌂ rithmly:home          ● (attention needed)
  rithmly:feat-auth     ○
  rithmly:fix-bug       ○
⌂ dotfiles:home         ○
```

Home workspaces get `⌂` prefix for visual distinction.
