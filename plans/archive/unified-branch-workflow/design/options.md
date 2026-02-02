# The Four Options

Last updated: 2026-02-01

## Option 1: Merge & Cleanup `[m]`

**Steps:**
1. Fetch latest from remote
2. Merge branch into parent (fast-forward if possible)
3. If conflict → mark workspace "stuck", open magit, stop
4. If success → delete local branch, remove worktree, remove Doom workspace, delete metadata
5. Navigate to home workspace (or last workspace if no home)

**Edge cases:**
- Branch already merged → skip merge, go straight to cleanup
- Branch doesn't exist → skip deletion, continue cleanup

## Option 2: Push & Create PR `[p]`

**Steps:**
1. Push branch to origin with `-u` (set upstream)
2. Create PR via `gh pr create` with summary
3. Store `:pr_url` in metadata
4. Keep workspace alive (for addressing review feedback)
5. Show PR URL in minibuffer

**Edge cases:**
- Remote push fails → show error, don't modify workspace state
- PR creation fails → branch is pushed, show error, let user create PR manually

## Option 3: Keep As-Is `[k]`

**Steps:**
1. Close status buffer
2. Do nothing else

**Use case:** Want to keep working, or handle things manually.

## Option 4: Discard `[d]`

**Steps:**
1. Require confirmation (y-or-n prompt)
2. Force-delete branch (`git branch -D`)
3. Remove worktree, Doom workspace, metadata
4. Navigate away same as Option 1

**Edge cases:**
- Branch doesn't exist → skip deletion, continue cleanup
- Uncommitted changes → warn but allow (user confirmed)

## Home Workspaces

Home workspaces have no branch, so only show:
- `[k]` Keep as-is
- `[d]` Close workspace (no "discard" language since nothing to lose)
- `[v]` View changes in magit

Warn if uncommitted changes exist.
