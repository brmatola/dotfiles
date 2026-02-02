# Claude Module: Creation Flow

Last updated: 2026-02-01

## Overview

Workspace creation is a multi-step process that must be atomic: either all components are created successfully, or everything is rolled back.

## Creation Flow Diagram

```
User: SPC C c
         │
         ▼
┌─────────────────────────────────────────┐
│  1. Prompt for branch name              │
│  2. Validate branch name                │
│  3. Determine parent branch             │
└─────────────────────────────────────────┘
         │
         ▼
┌─────────────────────────────────────────┐
│  Create metadata with status="creating" │
│  (Makes workspace visible in dashboard) │
└─────────────────────────────────────────┘
         │
         ▼
┌─────────────────────────────────────────┐
│  Step 1: Create git worktree            │
│  - git worktree add -b {branch} ...     │
│  - On failure: check if branch exists   │
│    - Offer to reuse existing branch     │
│    - Or abort                           │
└─────────────────────────────────────────┘
         │
         ▼
┌─────────────────────────────────────────┐
│  Step 2: Create Doom workspace          │
│  - +workspace/new {repo}:{branch}       │
│  - +workspace/switch-to                 │
└─────────────────────────────────────────┘
         │
         ▼
┌─────────────────────────────────────────┐
│  Step 3: Create vterm buffer            │
│  - Create *claude:{repo}:{branch}*      │
│  - Set default-directory to worktree    │
│  - Start claude command                 │
└─────────────────────────────────────────┘
         │
         ▼
┌─────────────────────────────────────────┐
│  Update metadata: status="active"       │
│  Fire claude-state-change-hook          │
└─────────────────────────────────────────┘
```

## Rollback on Failure

Each step has a corresponding rollback action:

| Step | Action | Rollback |
|------|--------|----------|
| 0 | Create metadata | Delete metadata file |
| 1 | Create worktree | `git worktree remove --force` |
| 2 | Create Doom workspace | `+workspace-kill` |
| 3 | Create vterm | Kill buffer |

### Rollback Procedure

```elisp
(defun claude--create-workspace-with-rollback (repo-path branch-name parent-branch)
  "Create workspace with automatic rollback on failure."
  (let ((repo-name (claude-repo-name repo-path))
        (rollback-stack nil))
    (condition-case err
        (progn
          ;; Step 0: Create metadata
          (claude--create-metadata repo-name branch-name parent-branch repo-path)
          (push (lambda () (claude-metadata-delete repo-name branch-name)) rollback-stack)

          ;; Step 1: Create worktree
          (let ((result (claude-worktree-create repo-path branch-name parent-branch)))
            (unless (car result)
              (error "Worktree creation failed: %s" (cdr result))))
          (push (lambda () (claude--worktree-remove-force repo-name branch-name)) rollback-stack)

          ;; Step 2: Create Doom workspace
          (claude--create-doom-workspace repo-name branch-name)
          (push (lambda () (+workspace-kill (claude--workspace-name repo-name branch-name))) rollback-stack)

          ;; Step 3: Create vterm
          (claude--create-vterm repo-name branch-name)
          ;; No rollback needed - workspace deletion handles buffers

          ;; Success: update status
          (claude--update-status repo-name branch-name "active")
          (run-hook-with-args 'claude-state-change-hook
                              (claude--workspace-name repo-name branch-name)
                              'creating 'active))

      ;; On any error: rollback (stack is already in reverse order from push)
      (error
       (dolist (rollback-fn rollback-stack)
         (ignore-errors (funcall rollback-fn)))
       (claude--update-status repo-name branch-name "failed")
       (signal (car err) (cdr err))))))
```

## Branch Already Exists

When git reports branch exists, offer choices:

```
┌─────────────────────────────────────────────────────────────┐
│ Branch 'feature-auth' already exists                        │
├─────────────────────────────────────────────────────────────┤
│                                                             │
│ [r] Reuse existing branch (create worktree for it)          │
│ [n] Enter a different branch name                           │
│ [c] Cancel                                                  │
└─────────────────────────────────────────────────────────────┘
```

### Reuse Existing Branch

```elisp
(defun claude--create-from-existing-branch (repo-path branch-name)
  "Create workspace for existing branch."
  ;; Determine parent by looking at branch's merge base with common branches
  (let* ((parent-branch (claude--detect-parent-branch repo-path branch-name))
         (repo-name (claude-repo-name repo-path)))
    (claude--create-metadata repo-name branch-name parent-branch repo-path)
    ;; Use git worktree add without -b flag
    (let ((result (claude-worktree-create-from-existing repo-path branch-name)))
      (if (car result)
          (progn
            (claude--create-doom-workspace repo-name branch-name)
            (claude--create-vterm repo-name branch-name)
            (claude--update-status repo-name branch-name "active"))
        (claude-metadata-delete repo-name branch-name)
        (error "Failed to create worktree: %s" (cdr result))))))

(defun claude--detect-parent-branch (repo-path branch-name)
  "Detect likely parent branch for existing BRANCH-NAME."
  (let ((default-directory repo-path))
    ;; Try upstream first
    (let ((upstream (string-trim
                     (shell-command-to-string
                      (format "git rev-parse --abbrev-ref %s@{upstream} 2>/dev/null"
                              (shell-quote-argument branch-name))))))
      (if (not (string-empty-p upstream))
          upstream
        ;; Fall back to merge-base with main/master
        (let ((main-exists (= 0 (shell-command "git rev-parse --verify main 2>/dev/null"))))
          (if main-exists "main" "master"))))))
```

## Home Workspace Creation

Home workspaces are simpler (no worktree):

```
User: SPC C h
         │
         ▼
┌─────────────────────────────────────────┐
│  Check if home workspace exists         │
│  - If yes: switch to it                 │
│  - If no: create it                     │
└─────────────────────────────────────────┘
         │ (create)
         ▼
┌─────────────────────────────────────────┐
│  Create metadata:                       │
│  - type: "home"                         │
│  - branch_name: "__home__"              │
│  - worktree_path: null                  │
│  - status: "creating"                   │
└─────────────────────────────────────────┘
         │
         ▼
┌─────────────────────────────────────────┐
│  Create Doom workspace                  │
│  - Name: {repo}:__home__                │
└─────────────────────────────────────────┘
         │
         ▼
┌─────────────────────────────────────────┐
│  Create vterm in repo root              │
│  - *claude:{repo}:__home__*             │
│  - default-directory = repo path        │
└─────────────────────────────────────────┘
         │
         ▼
┌─────────────────────────────────────────┐
│  Update status: "active"                │
└─────────────────────────────────────────┘
```

## Validation

### Branch Name Validation

```elisp
(defun claude--validate-branch-name (name)
  "Validate NAME is a valid git branch name.
Returns nil if valid, error string if invalid."
  (cond
   ((string-empty-p name)
    "Branch name cannot be empty")
   ((string-match-p "^-" name)
    "Branch name cannot start with dash")
   ((string-match-p "\\.\\." name)
    "Branch name cannot contain '..'")
   ((string-match-p "[\000-\037\177 ~^:?*\\[]" name)
    "Branch name contains invalid characters")
   ((string-match-p "\\.$" name)
    "Branch name cannot end with '.'")
   ((string-match-p "@{" name)
    "Branch name cannot contain '@{'")
   ((string= name "__home__")
    "Branch name '__home__' is reserved")
   (t nil)))
```

### Pre-Creation Checks

```elisp
(defun claude--pre-create-checks (repo-path branch-name)
  "Run pre-creation checks. Signal error if any fail."
  ;; Check we're in a git repo
  (unless (claude--git-repo-p repo-path)
    (error "Not in a git repository"))

  ;; Validate branch name
  (when-let ((err (claude--validate-branch-name branch-name)))
    (error "Invalid branch name: %s" err))

  ;; Check workspace doesn't already exist
  (let ((repo-name (claude-repo-name repo-path)))
    (when (claude-metadata-read repo-name branch-name)
      (error "Workspace already exists for %s:%s" repo-name branch-name)))

  ;; Check we have write access to worktree dir
  (unless (file-writable-p claude-worktree-dir)
    (error "Cannot write to worktree directory: %s" claude-worktree-dir)))
```

## Creation Timeout

To prevent indefinitely stuck `creating` state:

```elisp
(defcustom claude-creation-timeout 120
  "Seconds before creation is considered stalled.
Two minutes allows for slow git operations on large repos."
  :type 'integer
  :group 'claude-workflow)

(defun claude--check-stalled-creations ()
  "Check for and handle stalled workspace creations."
  (dolist (ws (claude--list-workspaces-by-status "creating"))
    (let* ((metadata (claude-metadata-read (car ws) (cdr ws)))
           (created-at (plist-get metadata :created_at))
           (elapsed (float-time (time-subtract nil (date-to-time created-at)))))
      (when (> elapsed claude-creation-timeout)
        (message "Workspace %s:%s creation stalled, marking failed" (car ws) (cdr ws))
        (claude--update-status (car ws) (cdr ws) "failed")))))

;; Run on startup and periodically
(add-hook 'doom-after-init-hook #'claude--check-stalled-creations)
```

## Error Reporting

Creation errors are reported via:

1. **Minibuffer message** for quick errors
2. **Popup buffer** for detailed errors with git output

```elisp
(defun claude--report-creation-error (repo-name branch-name error-info)
  "Report creation error to user."
  (let ((buf (get-buffer-create "*claude-error*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert (format "Failed to create workspace: %s:%s\n\n" repo-name branch-name))
      (insert (format "Error: %s\n\n" (if (stringp error-info)
                                          error-info
                                        (error-message-string error-info))))
      (insert "The workspace has been rolled back.\n")
      (insert "\nPress 'q' to close this window."))
    (pop-to-buffer buf)
    (special-mode)
    (local-set-key "q" #'quit-window)))
```
