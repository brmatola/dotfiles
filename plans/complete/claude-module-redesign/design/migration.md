# Claude Module: Migration Strategy

Last updated: 2026-02-01

## Purpose

This document describes how to migrate from the current (v0) metadata schema to the redesigned (v1) schema while preserving existing workspaces.

## Schema Comparison

### v0 (Current)

```json
{
  "parent_branch": "main",
  "parent_repo": "/Users/bmatola/dotfiles",
  "created": "2026-02-01T10:30:00Z"
}
```

Location: `~/worktrees/metadata/{repo}/{branch}.json`

### v1 (Redesign)

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

Location: Same

## Migration Strategy: Lazy Upgrade

Migrate metadata files lazily (on first access) rather than eagerly (all at once). This is safer and handles edge cases better.

### Detection

```elisp
(defun claude--metadata-version (metadata)
  "Return version of METADATA, or 0 if unversioned."
  (or (plist-get metadata :version) 0))

(defun claude--needs-migration-p (metadata)
  "Return t if METADATA needs migration."
  (< (claude--metadata-version metadata) 1))
```

### Migration Function

```elisp
(defun claude--migrate-metadata (repo-name branch-name metadata)
  "Migrate METADATA from v0 to v1 format.
Returns migrated metadata."
  (let ((version (claude--metadata-version metadata)))
    (cond
     ;; Already current
     ((>= version 1) metadata)

     ;; v0 -> v1
     ((= version 0)
      (let* ((worktree-path (claude-worktree-path repo-name branch-name))
             (now (format-time-string "%Y-%m-%dT%H:%M:%SZ" nil t))
             (migrated
              (list :version 1
                    :status "active"  ; Assume active if metadata exists
                    :type "worktree"  ; v0 had no home workspaces
                    :repo_name repo-name
                    :branch_name branch-name
                    :parent_branch (plist-get metadata :parent_branch)
                    :parent_repo (plist-get metadata :parent_repo)
                    :worktree_path worktree-path
                    :created_at (or (plist-get metadata :created) now)
                    :updated_at now)))
        ;; Write migrated metadata
        (claude-metadata-write repo-name branch-name migrated)
        (message "Migrated metadata for %s:%s to v1" repo-name branch-name)
        migrated))

     ;; Unknown version
     (t
      (error "Unknown metadata version %s for %s:%s" version repo-name branch-name)))))
```

### Transparent Access

All metadata reads go through a wrapper that handles migration:

```elisp
(defun claude-metadata-read (repo-name branch-name)
  "Read metadata for REPO-NAME/BRANCH-NAME.
Automatically migrates old format if needed.
Returns plist or nil if not found."
  (let ((path (claude-metadata-path repo-name branch-name)))
    (when (file-exists-p path)
      (let* ((json-object-type 'plist)
             (json-key-type 'keyword)
             (raw (json-read-file path)))
        (if (claude--needs-migration-p raw)
            (claude--migrate-metadata repo-name branch-name raw)
          raw)))))
```

## Startup Verification

On startup, verify all workspaces and trigger migration:

```elisp
(defun claude--startup-migration-check ()
  "Check all metadata files and migrate as needed."
  (dolist (ws (claude-worktree-list))
    (let* ((repo-name (car ws))
           (branch-name (cdr ws))
           (metadata (claude-metadata-read repo-name branch-name)))
      ;; claude-metadata-read handles migration automatically
      ;; Just verify the workspace is in a good state
      (when metadata
        (claude--reconcile metadata)))))

(add-hook 'doom-after-init-hook #'claude--startup-migration-check)
```

## Edge Cases

### Orphaned Worktrees

Worktree exists but no metadata (shouldn't happen, but defensive):

```elisp
(defun claude--detect-orphaned-worktrees ()
  "Find worktrees without metadata and offer to import them."
  (let ((worktree-base (expand-file-name claude-worktree-dir)))
    (when (file-directory-p worktree-base)
      (dolist (repo-dir (directory-files worktree-base t "^[^.]"))
        (when (file-directory-p repo-dir)
          (let ((repo-name (file-name-nondirectory repo-dir)))
            (dolist (branch-dir (directory-files repo-dir t "^[^.]"))
              (when (and (file-directory-p branch-dir)
                         (file-exists-p (expand-file-name ".git" branch-dir)))
                (let ((branch-name (file-name-nondirectory branch-dir)))
                  (unless (claude-metadata-read repo-name branch-name)
                    (message "Orphaned worktree found: %s:%s" repo-name branch-name)
                    (claude--import-orphaned-worktree repo-name branch-name branch-dir)))))))))))

(defun claude--import-orphaned-worktree (repo-name branch-name worktree-path)
  "Create metadata for orphaned worktree."
  (let* ((default-directory worktree-path)
         (parent-repo (string-trim
                       (shell-command-to-string
                        "git rev-parse --git-common-dir 2>/dev/null | xargs dirname")))
         (parent-branch (or (claude--detect-parent-branch worktree-path branch-name)
                           "main")))
    (claude-metadata-write repo-name branch-name
                           (list :version 1
                                 :status "active"
                                 :type "worktree"
                                 :repo_name repo-name
                                 :branch_name branch-name
                                 :parent_branch parent-branch
                                 :parent_repo parent-repo
                                 :worktree_path worktree-path
                                 :created_at (format-time-string "%Y-%m-%dT%H:%M:%SZ" nil t)
                                 :updated_at (format-time-string "%Y-%m-%dT%H:%M:%SZ" nil t)))))
```

### Orphaned Metadata

Metadata exists but worktree is gone:

This is handled by normal reconciliation - metadata will be marked as `broken`.

### In-Flight Workspaces

Workspace was being created/closed when Emacs crashed:

```elisp
(defun claude--recover-in-flight-workspaces ()
  "Handle workspaces stuck in transient states."
  (dolist (ws (claude-worktree-list))
    (let* ((repo-name (car ws))
           (branch-name (cdr ws))
           (metadata (claude-metadata-read repo-name branch-name))
           (status (plist-get metadata :status)))
      (cond
       ;; Stuck in creating - check if worktree actually exists
       ((equal status "creating")
        (if (file-directory-p (plist-get metadata :worktree_path))
            ;; Worktree exists - probably creation succeeded, mark active
            (claude--update-status repo-name branch-name "active")
          ;; Worktree doesn't exist - mark failed
          (claude--update-status repo-name branch-name "failed")))

       ;; Stuck in closing - try to resume cleanup
       ((equal status "closing")
        (let ((progress (plist-get metadata :cleanup_progress)))
          (if progress
              ;; Has progress tracking - resume from where we left off
              (claude--resume-cleanup repo-name branch-name progress)
            ;; No progress - mark stuck for user intervention
            (claude--update-status repo-name branch-name "stuck"))))))))
```

## Future Migrations

### Adding New Fields (v1 -> v2)

```elisp
(defun claude--migrate-v1-to-v2 (repo-name branch-name metadata)
  "Migrate from v1 to v2."
  (let ((migrated (copy-sequence metadata)))
    (plist-put migrated :version 2)
    ;; Add new fields with defaults
    (plist-put migrated :new_field_name "default_value")
    (claude-metadata-write repo-name branch-name migrated)
    migrated))
```

### Extending Migration Function

```elisp
(defun claude--migrate-metadata (repo-name branch-name metadata)
  "Migrate METADATA through all versions to current."
  (let ((current metadata)
        (target-version 1))  ; Update when adding new versions
    ;; Chain migrations
    (when (< (claude--metadata-version current) 1)
      (setq current (claude--migrate-v0-to-v1 repo-name branch-name current)))
    ;; Future: add more migration steps here
    ;; (when (< (claude--metadata-version current) 2)
    ;;   (setq current (claude--migrate-v1-to-v2 repo-name branch-name current)))
    current))
```

## Rollback Plan

If v1 format causes problems, we can roll back:

1. The v0 fields (`parent_branch`, `parent_repo`, `created`) are preserved
2. Code can read either format (via migration function)
3. To rollback: deploy code that writes v0 format
4. Old metadata files continue to work

Migration is non-destructive - original data is preserved within the migrated format.
