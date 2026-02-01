# Workflows

## Workspace Creation Flow

```
User: SPC C c
         │
         ▼
┌─────────────────────────────────────┐
│ 1. Detect current repository        │
│    - project-root or projectile     │
│    - If not in repo: show recent    │
│      repos or prompt for path       │
└─────────────────────────────────────┘
         │
         ▼
┌─────────────────────────────────────┐
│ 2. Get current branch               │
│    - This becomes parent_branch     │
│    - Show in prompt for context     │
└─────────────────────────────────────┘
         │
         ▼
┌─────────────────────────────────────┐
│ 3. Prompt for new branch name       │
│    "Branch name (from main): "      │
│    Optional: prompt for description │
└─────────────────────────────────────┘
         │
         ▼
┌─────────────────────────────────────┐
│ 4. Validate                         │
│    - Branch doesn't already exist   │
│    - Worktree dir doesn't exist     │
│    - If conflicts: offer options    │
└─────────────────────────────────────┘
         │
         ▼
┌─────────────────────────────────────┐
│ 5. Create git worktree              │
│    git worktree add -b {branch}     │
│      ~/worktrees/{repo}/{branch}    │
└─────────────────────────────────────┘
         │
         ▼
┌─────────────────────────────────────┐
│ 6. Write metadata                   │
│    ~/worktrees/metadata/{repo}/     │
│      {branch}.json                  │
└─────────────────────────────────────┘
         │
         ▼
┌─────────────────────────────────────┐
│ 7. Create Doom workspace            │
│    Name: {repo}:{branch}            │
└─────────────────────────────────────┘
         │
         ▼
┌─────────────────────────────────────┐
│ 8. Set up workspace                 │
│    - Create vterm buffer            │
│    - cd to worktree directory       │
│    - Run `claude` command           │
│    - Name: *claude:{repo}:{branch}* │
│    - (Optional: open file browser)  │
└─────────────────────────────────────┘
         │
         ▼
┌─────────────────────────────────────┐
│ 9. Start monitor if needed          │
└─────────────────────────────────────┘
```

### Edge Cases

**Branch already exists (git exit code 255):**
- Error: `fatal: a branch named 'X' already exists`
- Prompt: "Branch {name} exists. [r]euse, [n]ew name, [c]ancel"
- Reuse: create worktree from existing branch (use `git worktree add {path} {branch}` without `-b`)
- New name: prompt again

**Worktree directory exists (git exit code 128):**
- Error: `fatal: '/path' already exists`
- Note: Empty directories succeed silently (git uses them)
- Prompt: "Worktree exists at {path}. [o]pen existing, [d]elete and recreate, [c]ancel"

**Not in a git repo:**
- Show completing-read with recently used repos
- Or prompt for path

**Partial failure cleanup:**
- If worktree created but workspace creation fails: remove worktree, delete metadata
- If workspace created but vterm fails: delete workspace, remove worktree, delete metadata
- Always clean up in reverse order of creation

---

## Cleanup Flow

```
User: SPC C x (or x in dashboard)
         │
         ▼
┌─────────────────────────────────────┐
│ 1. Identify workspace               │
│    - Current workspace, or          │
│    - Selected from dashboard        │
└─────────────────────────────────────┘
         │
         ▼
┌─────────────────────────────────────┐
│ 2. Read metadata                    │
│    - parent_branch                  │
│    - parent_repo                    │
└─────────────────────────────────────┘
         │
         ▼
┌─────────────────────────────────────┐
│ 3. Compute merge status             │
│    - git rev-list to count commits  │
│    - ahead/behind parent            │
└─────────────────────────────────────┘
         │
         ▼
┌─────────────────────────────────────┐
│ 4. Show status buffer               │
│                                     │
│   Workspace: webapp:auth-fix        │
│   Parent: feature/user-system       │
│   Status: 3 commits ahead           │
│                                     │
│   [v] View diff                     │
│   [m] Merge & cleanup               │
│   [d] Delete (lose changes)         │
│   [c] Cancel                        │
│                                     │
└─────────────────────────────────────┘
         │
         ├──────[v]──────► Show diff in magit
         │                 Return to status
         │
         ├──────[m]──────► Merge & Cleanup
         │                 (see below)
         │
         ├──────[d]──────► Delete without merge
         │                 (requires "yes" if unmerged)
         │
         └──────[c]──────► Cancel
```

### Merge & Cleanup Steps

```
┌─────────────────────────────────────┐
│ 1. In parent repo, checkout parent  │
│    git checkout {parent_branch}     │
└─────────────────────────────────────┘
         │
         ▼
┌─────────────────────────────────────┐
│ 2. Merge worktree branch            │
│    git merge {worktree_branch}      │
└─────────────────────────────────────┘
         │
         ├──── Success ────► Continue to cleanup
         │
         └──── Conflict ───► Abort merge, open magit
                             Show message: "Resolve conflicts
                             in magit, then run cleanup again"
                             (Do NOT auto-cleanup on conflict)
         │
         ▼
┌─────────────────────────────────────┐
│ 3. Cleanup                          │
│    - Kill *claude:{repo}:{branch}*  │
│    - Delete Doom workspace          │
│    - git worktree remove {path}     │
│    - git branch -d {branch}         │
│    - Delete metadata JSON           │
└─────────────────────────────────────┘
         │
         ▼
┌─────────────────────────────────────┐
│ 4. Stop monitor if no workspaces    │
└─────────────────────────────────────┘
```

### Conflict Handling

If merge fails due to conflicts:
1. Run `git merge --abort` to restore clean state
2. Open magit in parent repo via `magit-status`
3. Display message explaining how to proceed
4. User resolves conflicts manually in magit
5. User re-runs `SPC C x` after resolution (will show "0 commits ahead" since merged)

### Fast Path (Already Merged)

If worktree branch has 0 commits ahead of parent:
- Skip status buffer
- Prompt: "Branch already merged. Clean up? [y/n]"
- On yes: run cleanup steps directly
