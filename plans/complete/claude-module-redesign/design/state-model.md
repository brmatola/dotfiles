# Claude Module: State Model

Last updated: 2026-02-01 (reviewed)

## Workspace States

```
                    ┌──────────────────────────────────────┐
                    │                                      │
                    ▼                                      │
┌─────────┐    ┌─────────┐    ┌─────────┐    ┌─────────┐  │
│ creating│───▶│  active │───▶│ closing │───▶│ (gone)  │  │
└─────────┘    └─────────┘    └─────────┘    └─────────┘  │
     │              │▲             │                       │
     │              ││             │                       │
     ▼              ▼│(repair)     ▼                       │
┌─────────┐    ┌─────────┐    ┌─────────┐                 │
│  failed │    │  broken │    │  stuck  │─────────────────┘
└─────────┘    └─────────┘    └─────────┘
  (timeout)                     (retry)
```

Note: `creating` can timeout to `failed` if stalled (see creation-flow.md).

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
| `creating` | `failed` | Creation timeout exceeded (see creation-flow.md) |
| `active` | `closing` | User initiates close |
| `active` | `broken` | Reconciler detects missing component |
| `closing` | (deleted) | All components removed successfully |
| `closing` | `stuck` | Cleanup failed (merge conflict, etc.) |
| `stuck` | `closing` | User retries after resolving issue |
| `broken` | `active` | User triggers repair and worktree is recovered |
| `broken` | `closing` | User chooses to clean up |
| `failed` | (deleted) | User deletes from dashboard or auto-cleanup |

### Cleanup from Failed

When a workspace is `failed` (creation didn't complete), cleanup is simple since there's nothing to merge:

```elisp
(defun claude--cleanup-failed-workspace (repo-name branch-name)
  "Clean up a failed workspace - just delete metadata and any partial resources."
  (let ((metadata (claude-metadata-read repo-name branch-name)))
    ;; Kill any buffers that might exist
    (when-let ((buf (get-buffer (claude--buffer-name repo-name branch-name))))
      (kill-buffer buf))
    ;; Remove Doom workspace if it exists
    (let ((ws-name (claude--workspace-name repo-name branch-name)))
      (when (+workspace-exists-p ws-name)
        (+workspace-kill ws-name)))
    ;; Remove worktree if it exists (force, since state is unknown)
    (when-let ((path (plist-get metadata :worktree_path)))
      (when (file-directory-p path)
        (ignore-errors
          (let ((default-directory (plist-get metadata :parent_repo)))
            (shell-command-to-string
             (format "git worktree remove --force %s 2>&1"
                     (shell-quote-argument path)))))))
    ;; Delete metadata
    (claude-metadata-delete repo-name branch-name)))
```

Failed workspaces can be cleaned up via:
1. Dashboard `x` key (same as other states)
2. Auto-cleanup at startup for workspaces failed > 24 hours ago

### Repair from Broken

When a workspace is `broken` (e.g., Doom workspace or vterm missing but worktree exists), the user can attempt repair via `SPC C r`:

```elisp
(defun claude-repair-workspace ()
  "Attempt to repair a broken workspace.
Can be called from dashboard or when in a Claude workspace.
Repair recreates missing ephemeral components (Doom workspace, vterm buffer)
but cannot recover a missing git worktree."
  (interactive)
  (let* ((ws-name (+workspace-current-name))
         (parsed (claude--parse-workspace-name ws-name)))
    (unless parsed (user-error "Not in a Claude workspace"))
    (let* ((repo-name (car parsed))
           (branch-name (cdr parsed))
           (metadata (claude-metadata-read repo-name branch-name))
           (status (plist-get metadata :status)))
      ;; Only repair broken workspaces
      (unless (equal status "broken")
        (user-error "Workspace is not broken (status: %s)" status))
      ;; Check if worktree exists - if not, can't repair
      (let ((worktree-path (plist-get metadata :worktree_path)))
        (when (and worktree-path (not (file-directory-p worktree-path)))
          (user-error "Cannot repair: git worktree is missing. Use 'x' to clean up"))
        ;; Reconciliation will auto-repair ephemeral components
        (let ((result (claude--reconcile metadata)))
          (if (eq result 'active)
              (progn
                (message "Workspace repaired successfully")
                ;; Switch to the repaired workspace
                (claude-workspace-switch (claude--workspace-name repo-name branch-name)))
            (message "Could not repair workspace")))))))
```

**Repair Flow:**

```
User: SPC C r (or 'r' in dashboard)
         │
         ▼
┌─────────────────────────────────────────┐
│  1. Check status == broken              │
│  2. Check worktree exists on disk       │
│     - If missing: error, suggest 'x'    │
└─────────────────────────────────────────┘
         │
         ▼
┌─────────────────────────────────────────┐
│  Call claude--reconcile:                │
│  - Recreate Doom workspace if missing   │
│  - Recreate vterm buffer if missing     │
│  - Restart Claude session               │
└─────────────────────────────────────────┘
         │
         ▼
┌─────────────────────────────────────────┐
│  Update status → active                 │
│  Switch to repaired workspace           │
└─────────────────────────────────────────┘
```

**Note:** Repair loses the previous Claude session context. The session starts fresh.

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
  "updated_at": "2026-02-01T14:22:00Z",
  "cleanup_progress": null
}
```

When status is `closing`, `cleanup_progress` tracks partial cleanup:

```json
{
  "cleanup_progress": {
    "buffers_killed": true,
    "workspace_removed": true,
    "worktree_removed": false,
    "branch_deleted": false,
    "started_at": "2026-02-01T14:30:00Z"
  }
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
| `cleanup_progress` | object/null | Tracks cleanup steps when status=closing (see above) |

## Home Workspaces

Home workspaces are special:

- `type: "home"`
- `branch_name: "__home__"`
- `worktree_path: null` (runs in main repo)
- `parent_branch: null` (no merge flow)
- Cleanup just kills buffers and removes workspace (no git operations)

### Why `__home__` for branch_name?

The `__home__` string is used because:
1. It's unlikely to conflict with real branch names (double underscore prefix)
2. It's not a valid git branch name (contains special meaning only in our system)
3. It provides a consistent lookup key for metadata

**Important:** Code must never attempt to create `__home__` as a git branch. Always check `type == "home"` before any git branch operations.

```elisp
(defconst claude-home-branch-name "__home__"
  "Sentinel value for home workspace branch_name field.
This is NOT a valid git branch name - never use with git commands.")

(defun claude--home-workspace-p (metadata-or-name)
  "Return t if this is a home workspace."
  (if (stringp metadata-or-name)
      ;; Workspace name - parse and check branch portion exactly
      (when-let ((parsed (claude--parse-workspace-name metadata-or-name)))
        (equal (cdr parsed) claude-home-branch-name))
    ;; Metadata plist
    (equal (plist-get metadata-or-name :type) "home")))
```

## State Change Hook

All state changes fire a hook for UI updates:

```elisp
(defvar claude-state-change-hook nil
  "Hook run when workspace state changes.
Called with three arguments:
  WORKSPACE-NAME - string like \"repo:branch\"
  OLD-STATUS - symbol: creating, active, closing, failed, broken, stuck
  NEW-STATUS - symbol (same options)

Note: Attention changes use a separate hook (claude-attention-change-hook)
to keep lifecycle and attention concerns separate.")

(defvar claude-attention-change-hook nil
  "Hook run when workspace attention state changes.
Called with two arguments:
  WORKSPACE-NAME - string like \"repo:branch\"
  NEEDS-ATTENTION - boolean")

;; Example subscriber
(add-hook 'claude-state-change-hook
          (lambda (ws-name old new)
            (message "Workspace %s: %s -> %s" ws-name old new)))
```

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

## Known Limitations

### Repo Name Collisions

The metadata path `~/worktrees/metadata/{repo}/{branch}.json` uses the repository basename. This means:

- `~/code/company-a/api` and `~/code/company-b/api` both use `api` as repo_name
- If both have a `feature-x` branch, metadata would conflict

**Mitigation:** The `parent_repo` field stores the full path, so we can detect this situation:

```elisp
(defun claude--check-repo-collision (repo-name branch-name parent-repo)
  "Check if metadata exists for different parent repo."
  (when-let ((existing (claude-metadata-read repo-name branch-name)))
    (unless (equal (plist-get existing :parent_repo)
                   (expand-file-name parent-repo))
      (user-error "Workspace %s:%s already exists for different repo: %s"
                  repo-name branch-name
                  (plist-get existing :parent_repo)))))
```

**Future consideration:** Use a hash of parent_repo path in the metadata filename to allow same-named repos from different locations.
