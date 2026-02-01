# Claude Module: Cleanup and Merge Flow

Last updated: 2026-02-01

## Overview

Cleanup is a critical flow that must be safe and reliable. The key principle: **never lose work**.

## Cleanup Flow Diagram

```
User: SPC C x
         │
         ▼
┌─────────────────────────────────────────┐
│  1. Set status → "closing"              │
│  2. Check for uncommitted changes       │
│     - If yes: warn, require confirm     │
└─────────────────────────────────────────┘
         │
         ▼
┌─────────────────────────────────────────┐
│  Show status buffer:                    │
│  - Commits ahead of parent: N           │
│  - Uncommitted changes: yes/no          │
│  - Branch: feature → main               │
│                                         │
│  [m] Merge to parent & cleanup          │
│  [p] Create PR (opens browser)          │
│  [d] Delete without merging             │
│  [c] Cancel                             │
└─────────────────────────────────────────┘
         │
         ▼ (if [m] merge)
┌─────────────────────────────────────────┐
│  Safe Merge Protocol:                   │
│  1. git fetch origin {parent}           │
│  2. git merge {parent} --no-commit      │
│     (pull parent changes into worktree) │
│  3. If conflict → abort, status="stuck" │
│  4. If clean → commit the merge         │
│  5. git checkout {parent}               │
│  6. git merge --no-ff {branch}          │
│  7. If conflict → status="stuck"        │
│  8. If clean → continue cleanup         │
└─────────────────────────────────────────┘
         │
         ▼
┌─────────────────────────────────────────┐
│  Cleanup sequence:                      │
│  1. Kill Claude buffer                  │
│  2. Kill terminal buffers               │
│  3. Kill file buffers in worktree       │
│  4. Remove Doom workspace               │
│  5. git worktree remove {path}          │
│  6. git branch -d {branch}              │
│  7. Delete metadata file                │
│  8. Switch to home workspace            │
└─────────────────────────────────────────┘
```

## Safe Merge Protocol

The merge must handle these scenarios:

### Scenario 1: Clean Merge

```
main:     A---B
               \
feature:        C---D

After merge:
main:     A---B-------M
               \     /
feature:        C---D
```

Steps:
1. In worktree: `git fetch origin main`
2. In worktree: `git merge main` (bring main changes into feature)
3. If clean, we know the reverse merge will also be clean
4. In main repo: `git checkout main && git merge --no-ff feature`
5. Cleanup worktree

### Scenario 2: Conflicts on Forward Merge

```
main:     A---B  (modified file.txt)
               \
feature:        C  (also modified file.txt)
```

Steps:
1. In worktree: `git fetch origin main`
2. In worktree: `git merge main` → CONFLICT
3. Set status → "stuck"
4. Open magit in worktree
5. User resolves, commits
6. User retries `SPC C x` → now clean

### Scenario 3: Nothing to Merge

```
main:     A---B
               \
feature:        (no commits)
```

Steps:
1. Detect 0 commits ahead
2. Show warning: "No commits to merge"
3. Offer: [d] Delete without merging, [c] Cancel

## Status Buffer

```
┌─────────────────────────────────────────────────────────────┐
│ Close Workspace: dotfiles:feature-auth                      │
├─────────────────────────────────────────────────────────────┤
│                                                             │
│ Branch:    feature-auth → main                              │
│ Commits:   3 ahead, 0 behind                                │
│ Uncommitted changes: none                                   │
│                                                             │
│ Commits to merge:                                           │
│   abc1234 Add authentication middleware                     │
│   def5678 Add login endpoint                                │
│   ghi9012 Add logout endpoint                               │
│                                                             │
├─────────────────────────────────────────────────────────────┤
│ [m] Merge & cleanup  [p] Create PR  [d] Delete  [c] Cancel  │
└─────────────────────────────────────────────────────────────┘
```

## Stuck State Recovery

When status = "stuck":

```
┌─────────────────────────────────────────────────────────────┐
│ Workspace Stuck: dotfiles:feature-auth                      │
├─────────────────────────────────────────────────────────────┤
│                                                             │
│ Merge conflicts detected. Resolve in magit, then retry.     │
│                                                             │
│ Conflicting files:                                          │
│   src/auth.ts                                               │
│   src/middleware.ts                                         │
│                                                             │
├─────────────────────────────────────────────────────────────┤
│ [g] Open magit  [r] Retry cleanup  [d] Delete  [c] Cancel   │
└─────────────────────────────────────────────────────────────┘
```

## Home Workspace Cleanup

Home workspaces have a simpler flow (no merge):

```
User: SPC C x (in home workspace)
         │
         ▼
┌─────────────────────────────────────────┐
│  Check for uncommitted changes          │
│  - If yes: warn "Uncommitted changes    │
│    in main repo. Commit or stash first."│
│    [c] Cancel only                      │
│  - If no: continue                      │
└─────────────────────────────────────────┘
         │
         ▼
┌─────────────────────────────────────────┐
│  Cleanup:                               │
│  1. Kill Claude buffer                  │
│  2. Kill terminal buffers               │
│  3. Remove Doom workspace               │
│  4. Delete metadata                     │
│  (No git operations)                    │
└─────────────────────────────────────────┘
```

## Implementation Notes

### Atomic Operations

Each step should be individually recoverable:

```elisp
(defun claude--cleanup-step (step-name action rollback)
  "Execute ACTION, save progress. On failure, run ROLLBACK."
  (condition-case err
      (progn
        (funcall action)
        (claude--record-cleanup-progress step-name))
    (error
     (when rollback (funcall rollback))
     (signal (car err) (cdr err)))))
```

### Cleanup Progress Tracking

Store progress in metadata to enable recovery:

```json
{
  "status": "closing",
  "cleanup_progress": {
    "buffers_killed": true,
    "workspace_removed": true,
    "worktree_removed": false,
    "branch_deleted": false,
    "started_at": "2026-02-01T14:30:00Z"
  }
}
```

### Post-Cleanup Navigation

After successful cleanup:
1. Switch to home workspace for that repo
2. If home workspace doesn't exist, create it
3. Focus the Claude buffer in home (if exists) or just the workspace

```elisp
(defun claude--post-cleanup-navigate (repo-name)
  "Navigate to appropriate location after cleanup."
  (let ((home-name (concat repo-name ":__home__")))
    (if (claude-metadata-read repo-name "__home__")
        (claude-workspace-switch repo-name "__home__")
      ;; No home workspace - just switch to an existing Doom workspace
      ;; or stay where we are
      (message "Workspace cleaned up. No home workspace to switch to."))))
```
