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
│    - Open treemacs at worktree root │
│    - Create vterm buffer            │
│    - cd to worktree directory       │
│    - Run `claude` command           │
│    - Name: *claude:{repo}:{branch}* │
└─────────────────────────────────────┘
         │
         ▼
┌─────────────────────────────────────┐
│ 9. Start monitor if needed          │
└─────────────────────────────────────┘
```

### Edge Cases

**Branch already exists:**
- Prompt: "Branch {name} exists. [r]euse, [n]ew name, [c]ancel"
- Reuse: create worktree from existing branch
- New name: prompt again

**Worktree directory exists:**
- Prompt: "Worktree exists at {path}. [o]pen existing, [d]elete and recreate, [c]ancel"

**Not in a git repo:**
- Show completing-read with recently used repos
- Or prompt for path

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
│    Handle conflicts if any          │
└─────────────────────────────────────┘
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

### Fast Path (Already Merged)

If worktree branch has 0 commits ahead of parent:
- Skip status buffer
- Prompt: "Branch already merged. Clean up? [y/n]"
- On yes: run cleanup steps directly
