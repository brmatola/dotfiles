# Error Handling & Edge Cases

Last updated: 2026-02-01

## Out-of-Band Changes

When `SPC C x` runs, it checks git state defensively before showing options.

| Situation | Detection | Behavior |
|-----------|-----------|----------|
| Branch already merged | `git merge-base --is-ancestor HEAD parent` | Skip merge, offer cleanup only |
| Branch deleted externally | `git rev-parse branch` fails | Skip branch deletion, continue cleanup |
| Worktree removed externally | Directory doesn't exist | Skip worktree removal, continue cleanup |
| Remote branch gone | `git ls-remote` returns empty | Skip remote operations, local cleanup only |

## Status Buffer Reflects Reality

When branch is already merged:

```
Claude Workspace Cleanup

Workspace: dotfiles:feature-auth
Parent: main
Status: Already merged ✓

[d] Clean up workspace
[k] Keep as-is
[q] Cancel
```

No merge/PR options shown since they don't apply.

## Conflict During Merge

If user selects `[m]` and merge hits conflict:

1. Mark workspace status as "stuck" in metadata
2. Open magit in the worktree
3. Message: "Merge conflict. Resolve in magit, then run SPC C x again."
4. Next `SPC C x` detects conflict state:
   - If resolved (no conflicts) → continue merge, complete cleanup
   - If still conflicted → show magit again
   - Offer "abort merge" option

## Stale Metadata

Reconciliation detects orphaned metadata on dashboard open:

```elisp
(defun claude--detect-orphaned-metadata ()
  "Find metadata with no corresponding worktree or branch."
  (seq-filter
   (lambda (ws)
     (let* ((repo (car ws))
            (branch (cdr ws))
            (meta (claude-metadata-read repo branch))
            (path (plist-get meta :worktree_path)))
       (and path
            (not (file-directory-p path))
            (not (claude-git-branch-exists-p
                  (plist-get meta :parent_repo) branch)))))
   (claude--list-all-workspaces)))
```

When found, offer to clean up:
- "Found orphaned workspace metadata for X. Clean up? [y/n]"
- If yes → delete metadata, remove from dashboard

## Recovery States

| State | Meaning | User Action |
|-------|---------|-------------|
| `active` | Normal operation | `SPC C x` shows all options |
| `stuck` | Merge conflict in progress | `SPC C x` shows resolve/abort |
| `broken` | Worktree missing | `SPC C x` offers cleanup only |
| `failed` | Creation failed | Dashboard `x` force-cleans |

## Git Command Failures

All git operations wrapped in error handling:

```elisp
(condition-case err
    (claude-git-merge ...)
  (error
   (pcase (claude--classify-git-error err)
     ('conflict (claude--handle-conflict ...))
     ('not-found (claude--handle-missing ...))
     (_ (user-error "Git error: %s" (error-message-string err))))))
```

Never leave workspace in undefined state — always update metadata to reflect reality.
