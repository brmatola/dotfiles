# Claude Module: State Model

Last updated: 2026-02-01

## Workspace States

```
                    ┌──────────────────────────────────────┐
                    │                                      │
                    ▼                                      │
┌─────────┐    ┌─────────┐    ┌─────────┐    ┌─────────┐  │
│ creating│───▶│  active │───▶│ closing │───▶│ (gone)  │  │
└─────────┘    └─────────┘    └─────────┘    └─────────┘  │
     │              │              │                       │
     │              │              │                       │
     ▼              ▼              ▼                       │
┌─────────┐    ┌─────────┐    ┌─────────┐                 │
│  failed │    │  broken │    │  stuck  │─────────────────┘
└─────────┘    └─────────┘    └─────────┘
                                (retry)
```

### State Definitions

| State | Meaning | Entry Condition |
|-------|---------|-----------------|
| `creating` | Setup in progress | User initiated create |
| `active` | All components healthy | Creation completed successfully |
| `closing` | Cleanup in progress | User initiated close |
| `failed` | Creation failed | Error during creation |
| `broken` | Component disappeared | Reconciler detected missing component |
| `stuck` | Cleanup blocked | Merge conflict or cleanup error |

### Valid Transitions

| From | To | Trigger |
|------|----|---------|
| `creating` | `active` | All components verified present |
| `creating` | `failed` | Any component creation failed |
| `active` | `closing` | User initiates close |
| `active` | `broken` | Reconciler detects missing component |
| `closing` | (deleted) | All components removed successfully |
| `closing` | `stuck` | Cleanup failed (merge conflict, etc.) |
| `stuck` | `closing` | User retries after resolving issue |
| `broken` | `closing` | User chooses to clean up |
| `failed` | (deleted) | Rollback complete |

## Metadata Schema

Location: `~/worktrees/metadata/{repo}/{branch}.json`

```json
{
  "version": 1,
  "status": "active",
  "type": "worktree",
  "repo_name": "dotfiles",
  "branch_name": "feature-auth",
  "parent_branch": "main",
  "parent_repo": "/Users/bmatola/dotfiles",
  "worktree_path": "/Users/bmatola/worktrees/dotfiles/feature-auth",
  "created_at": "2026-02-01T10:30:00Z",
  "updated_at": "2026-02-01T14:22:00Z"
}
```

### Field Definitions

| Field | Type | Description |
|-------|------|-------------|
| `version` | int | Schema version for migrations |
| `status` | string | Current state (see above) |
| `type` | string | `"worktree"` or `"home"` |
| `repo_name` | string | Repository name (for display) |
| `branch_name` | string | Branch name (`"__home__"` for home workspaces) |
| `parent_branch` | string | Branch to merge into (null for home) |
| `parent_repo` | string | Absolute path to main repo |
| `worktree_path` | string | Absolute path to worktree (null for home) |
| `created_at` | string | ISO timestamp |
| `updated_at` | string | ISO timestamp (updated on state change) |

## Home Workspaces

Home workspaces are special:

- `type: "home"`
- `branch_name: "__home__"`
- `worktree_path: null` (runs in main repo)
- `parent_branch: null` (no merge flow)
- Cleanup just kills buffers and removes workspace (no git operations)

## Derived Values

These are computed from metadata, not stored:

| Value | Derivation |
|-------|------------|
| Workspace name | `{repo_name}:{branch_name}` |
| Claude buffer name | `*claude:{repo_name}:{branch_name}*` |
| Terminal buffer name | `*term:{repo_name}:{branch_name}:{n}*` |
| Working directory | `worktree_path` or `parent_repo` (for home) |

## State Invariants

These must always be true:

1. **Metadata exists ⟹ workspace is tracked** — No workspace without metadata
2. **Status = active ⟹ all components exist** — Or reconciler marks broken
3. **Status = creating ⟹ creation in progress** — Will transition to active or failed
4. **Status = closing ⟹ cleanup in progress** — Will transition to deleted or stuck
5. **Home workspace has no worktree** — `type = home ⟹ worktree_path = null`
