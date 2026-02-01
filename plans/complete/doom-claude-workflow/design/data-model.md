# Data Model

## Directory Structure

```
~/worktrees/
├── {repo-name}/
│   └── {branch-name}/        # Git worktree checkout
└── metadata/
    └── {repo-name}/
        └── {branch-name}.json
```

**Examples:**
```
~/worktrees/
├── webapp/
│   ├── auth-fix/
│   └── api-refactor/
├── genealogy/
│   └── gedcom-parser/
└── metadata/
    ├── webapp/
    │   ├── auth-fix.json
    │   └── api-refactor.json
    └── genealogy/
        └── gedcom-parser.json
```

## Metadata Format

Each worktree has a JSON metadata file:

```json
{
  "parent_branch": "feature/user-system",
  "parent_repo": "/Users/bmatola/code/webapp",
  "created": "2025-02-01T10:30:00Z",
  "description": "Fix OAuth token refresh"
}
```

| Field | Type | Description |
|-------|------|-------------|
| `parent_branch` | string | Branch this worktree should merge back into |
| `parent_repo` | string | Absolute path to the source repository |
| `created` | string | ISO 8601 timestamp |
| `description` | string | Optional user-provided description |

## Naming Conventions

| Entity | Format | Example |
|--------|--------|---------|
| Worktree directory | `~/worktrees/{repo}/{branch}` | `~/worktrees/webapp/auth-fix` |
| Metadata file | `~/worktrees/metadata/{repo}/{branch}.json` | `~/worktrees/metadata/webapp/auth-fix.json` |
| Doom workspace | `{repo}:{branch}` | `webapp:auth-fix` |
| Claude buffer | `*claude:{repo}:{branch}*` | `*claude:webapp:auth-fix*` |

## Repo Name Derivation

The `{repo-name}` is derived from the source repository path:
- `/Users/bmatola/code/webapp` → `webapp`
- `/Users/bmatola/projects/my-cool-app` → `my-cool-app`

Uses the final path component (directory name).
