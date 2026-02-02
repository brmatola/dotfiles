;;; claude-state.el --- State machine and metadata operations -*- lexical-binding: t; -*-

;;; Commentary:
;; Single source of truth for Claude workspace state.
;; Provides:
;; - State machine with 6 states and validated transitions
;; - Metadata operations with automatic v0→v1 migration
;; - Naming utilities for workspaces and buffers
;; - Hooks for state and attention changes

;;; Code:

(require 'json)

;; Forward declarations for customizable variables defined in claude.el
(defvar claude-worktree-dir)
(defvar claude-metadata-dir)

;;; Constants

(defconst claude-home-branch-name "__home__"
  "Sentinel value for home workspace branch_name field.
This is NOT a valid git branch name - never use with git commands.")

(defconst claude-metadata-version 1
  "Current metadata schema version.")

;;; State Machine

(defconst claude-valid-states
  '(creating active closing failed broken stuck)
  "All valid workspace states.")

(defconst claude-state-transitions
  '((creating . (active failed))
    (active . (closing broken))
    (closing . (stuck))  ; closing -> (deleted) is handled by removal, not transition
    (failed . ())        ; failed -> (deleted) is handled by removal
    (broken . (active closing))
    (stuck . (closing)))
  "Valid state transitions.
Each entry is (FROM-STATE . (TO-STATE ...)).
Transitions to `deleted' are handled by metadata removal, not state change.")

(defun claude-state-valid-p (status)
  "Return t if STATUS is a valid state symbol."
  (memq status claude-valid-states))

(defun claude-state-transition-valid-p (from-status to-status)
  "Return t if transition from FROM-STATUS to TO-STATUS is valid."
  (let ((valid-targets (cdr (assq from-status claude-state-transitions))))
    (memq to-status valid-targets)))

(defun claude-state-transition (repo-name branch-name new-status)
  "Transition workspace REPO-NAME/BRANCH-NAME to NEW-STATUS.
Validates the transition and updates metadata.
Fires `claude-state-change-hook' on success.
Signals error if transition is invalid."
  (unless (claude-state-valid-p new-status)
    (error "Invalid state: %s" new-status))
  (let* ((metadata (claude-metadata-read repo-name branch-name))
         (old-status (intern (or (plist-get metadata :status) "creating"))))
    (unless (claude-state-transition-valid-p old-status new-status)
      (error "Invalid state transition: %s -> %s" old-status new-status))
    ;; Update metadata
    (let ((updated (plist-put (copy-sequence metadata)
                              :status (symbol-name new-status))))
      (plist-put updated :updated_at (claude--timestamp))
      (claude-metadata-write repo-name branch-name updated)
      ;; Fire hook
      (run-hook-with-args 'claude-state-change-hook
                          (claude--workspace-name repo-name branch-name)
                          old-status
                          new-status)
      new-status)))

(defun claude--update-status (repo-name branch-name new-status)
  "Update status without transition validation (for internal use).
NEW-STATUS can be a string or symbol."
  (let* ((metadata (claude-metadata-read repo-name branch-name))
         (old-status-str (plist-get metadata :status))
         (old-status (and old-status-str (intern old-status-str)))
         (new-status-sym (if (symbolp new-status) new-status (intern new-status)))
         (new-status-str (if (stringp new-status) new-status (symbol-name new-status))))
    (let ((updated (plist-put (copy-sequence metadata) :status new-status-str)))
      (plist-put updated :updated_at (claude--timestamp))
      (claude-metadata-write repo-name branch-name updated)
      ;; Fire hook if status actually changed
      (when (not (eq old-status new-status-sym))
        (run-hook-with-args 'claude-state-change-hook
                            (claude--workspace-name repo-name branch-name)
                            old-status
                            new-status-sym)))))

;;; Hooks

(defvar claude-state-change-hook nil
  "Hook run when workspace state changes.
Called with three arguments:
  WORKSPACE-NAME - string like \"repo:branch\"
  OLD-STATUS - symbol: creating, active, closing, failed, broken, stuck, or nil
  NEW-STATUS - symbol (same options)

Note: Attention changes use a separate hook (`claude-attention-change-hook')
to keep lifecycle and attention concerns separate.")

(defvar claude-attention-change-hook nil
  "Hook run when workspace attention state changes.
Called with two arguments:
  WORKSPACE-NAME - string like \"repo:branch\"
  NEEDS-ATTENTION - boolean")

;;; Naming Utilities

(defun claude--workspace-name (repo-name branch-name)
  "Generate workspace name for REPO-NAME and BRANCH-NAME."
  (format "%s:%s" repo-name branch-name))

(defun claude--buffer-name (repo-name branch-name)
  "Generate Claude buffer name for REPO-NAME and BRANCH-NAME."
  (format "*claude:%s:%s*" repo-name branch-name))

(defun claude--terminal-buffer-name (repo-name branch-name n)
  "Generate terminal buffer name for REPO-NAME/BRANCH-NAME with number N."
  (format "*term:%s:%s:%d*" repo-name branch-name n))

(defun claude--parse-workspace-name (workspace-name)
  "Parse WORKSPACE-NAME into (repo-name . branch-name) cons cell.
Returns nil if not a valid Claude workspace name."
  (when (and workspace-name
             (string-match "^\\([^:]+\\):\\(.+\\)$" workspace-name))
    (cons (match-string 1 workspace-name)
          (match-string 2 workspace-name))))

(defun claude--home-workspace-p (metadata-or-name)
  "Return t if this is a home workspace.
METADATA-OR-NAME can be a metadata plist or a workspace name string."
  (if (stringp metadata-or-name)
      ;; Workspace name - parse and check branch portion
      (when-let ((parsed (claude--parse-workspace-name metadata-or-name)))
        (equal (cdr parsed) claude-home-branch-name))
    ;; Metadata plist - check type field
    (equal (plist-get metadata-or-name :type) "home")))

;;; Path Utilities

(defun claude--worktree-path (repo-name branch-name)
  "Return path to worktree for REPO-NAME and BRANCH-NAME."
  (expand-file-name (format "%s/%s" repo-name branch-name)
                    (expand-file-name claude-worktree-dir)))

(defun claude--metadata-path (repo-name branch-name)
  "Return path to metadata file for REPO-NAME and BRANCH-NAME."
  (expand-file-name (format "%s/%s.json" repo-name branch-name)
                    (expand-file-name claude-metadata-dir)))

(defun claude--repo-name (repo-path)
  "Extract repo name from REPO-PATH.
Uses the final path component."
  (file-name-nondirectory (directory-file-name (expand-file-name repo-path))))

;;; Timestamp Utilities

(defun claude--timestamp ()
  "Return current time as ISO 8601 string."
  (format-time-string "%Y-%m-%dT%H:%M:%SZ" nil t))

;;; Metadata Operations

(defun claude-metadata-write (repo-name branch-name data)
  "Write DATA (plist) as metadata for REPO-NAME/BRANCH-NAME."
  (let* ((path (claude--metadata-path repo-name branch-name))
         (dir (file-name-directory path))
         ;; Ensure version and updated_at are set
         (data-with-version (if (plist-get data :version)
                                data
                              (plist-put (copy-sequence data) :version claude-metadata-version)))
         (data-with-timestamp (if (plist-get data-with-version :updated_at)
                                  data-with-version
                                (plist-put (copy-sequence data-with-version)
                                           :updated_at (claude--timestamp))))
         (json-data (json-encode data-with-timestamp)))
    (make-directory dir t)
    (with-temp-file path
      (insert json-data))
    t))

(defun claude-metadata-read (repo-name branch-name)
  "Read metadata for REPO-NAME/BRANCH-NAME.
Automatically migrates old format if needed.
Returns plist or nil if not found."
  (let ((path (claude--metadata-path repo-name branch-name)))
    (when (file-exists-p path)
      (let* ((json-object-type 'plist)
             (json-key-type 'keyword)
             (raw (json-read-file path)))
        (if (claude--needs-migration-p raw)
            (claude--migrate-metadata repo-name branch-name raw)
          raw)))))

(defun claude-metadata-delete (repo-name branch-name)
  "Delete metadata file for REPO-NAME/BRANCH-NAME."
  (let ((path (claude--metadata-path repo-name branch-name)))
    (when (file-exists-p path)
      (delete-file path)
      ;; Clean up empty parent directories
      (let ((dir (file-name-directory path)))
        (when (and (file-directory-p dir)
                   (null (directory-files dir nil "^[^.]")))
          (delete-directory dir))))))

;;; Migration

(defun claude--metadata-version (metadata)
  "Return version of METADATA, or 0 if unversioned."
  (or (plist-get metadata :version) 0))

(defun claude--needs-migration-p (metadata)
  "Return t if METADATA needs migration."
  (< (claude--metadata-version metadata) claude-metadata-version))

(defun claude--migrate-metadata (repo-name branch-name metadata)
  "Migrate METADATA from v0 to v1 format.
Returns migrated metadata."
  (let ((version (claude--metadata-version metadata)))
    (cond
     ;; Already current
     ((>= version claude-metadata-version) metadata)

     ;; v0 -> v1
     ((= version 0)
      (let* ((worktree-path (claude--worktree-path repo-name branch-name))
             (now (claude--timestamp))
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

;;; Workspace Listing

(defun claude--list-all-metadata ()
  "List all workspace metadata.
Returns list of metadata plists."
  (let ((metadata-dir (expand-file-name claude-metadata-dir))
        (result nil))
    (when (file-directory-p metadata-dir)
      (dolist (repo-dir (directory-files metadata-dir t "^[^.]"))
        (when (file-directory-p repo-dir)
          (let ((repo-name (file-name-nondirectory repo-dir)))
            (dolist (json-file (directory-files repo-dir t "\\.json$"))
              (let* ((branch-name (file-name-sans-extension
                                   (file-name-nondirectory json-file)))
                     (metadata (claude-metadata-read repo-name branch-name)))
                (when metadata
                  (push metadata result))))))))
    (nreverse result)))

(defun claude--list-all-workspaces ()
  "List all workspaces.
Returns list of (repo-name . branch-name) pairs."
  (let ((metadata-dir (expand-file-name claude-metadata-dir))
        (result nil))
    (when (file-directory-p metadata-dir)
      (dolist (repo-dir (directory-files metadata-dir t "^[^.]"))
        (when (file-directory-p repo-dir)
          (let ((repo-name (file-name-nondirectory repo-dir)))
            (dolist (json-file (directory-files repo-dir t "\\.json$"))
              (let ((branch-name (file-name-sans-extension
                                  (file-name-nondirectory json-file))))
                (push (cons repo-name branch-name) result)))))))
    (nreverse result)))

(defun claude--list-workspaces-by-status (status)
  "List workspaces with STATUS.
Returns list of (repo-name . branch-name) pairs."
  (let ((status-str (if (symbolp status) (symbol-name status) status)))
    (seq-filter (lambda (ws)
                  (let ((metadata (claude-metadata-read (car ws) (cdr ws))))
                    (equal (plist-get metadata :status) status-str)))
                (claude--list-all-workspaces))))

(defun claude--list-active-workspaces ()
  "List active workspaces.
Returns list of (repo-name . branch-name) pairs."
  (claude--list-workspaces-by-status "active"))

;;; Validation

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
   ((string= name claude-home-branch-name)
    "Branch name '__home__' is reserved")
   (t nil)))

(defun claude--check-repo-collision (repo-name branch-name parent-repo)
  "Check if metadata exists for different parent repo.
Signals error if collision detected."
  (when-let ((existing (claude-metadata-read repo-name branch-name)))
    (unless (equal (plist-get existing :parent_repo)
                   (expand-file-name parent-repo))
      (user-error "Workspace %s:%s already exists for different repo: %s"
                  repo-name branch-name
                  (plist-get existing :parent_repo)))))

;;; Initial Metadata Creation

(defun claude--create-metadata (repo-name branch-name parent-branch parent-repo &optional type)
  "Create initial metadata for a new workspace.
TYPE is \"worktree\" (default) or \"home\"."
  (let* ((workspace-type (or type "worktree"))
         (worktree-path (if (equal workspace-type "home")
                            nil
                          (claude--worktree-path repo-name branch-name)))
         (now (claude--timestamp)))
    (claude-metadata-write repo-name branch-name
                           (list :version claude-metadata-version
                                 :status "creating"
                                 :type workspace-type
                                 :repo_name repo-name
                                 :branch_name branch-name
                                 :parent_branch parent-branch
                                 :parent_repo (expand-file-name parent-repo)
                                 :worktree_path worktree-path
                                 :created_at now
                                 :updated_at now))))

;;; Workspace Directory

(defun claude--workspace-directory (repo-name branch-name)
  "Get working directory for workspace REPO-NAME/BRANCH-NAME."
  (let ((metadata (claude-metadata-read repo-name branch-name)))
    (or (plist-get metadata :worktree_path)
        (plist-get metadata :parent_repo))))

;;; Workflow Integration

(defun claude--workflow-phase (repo-name branch-name)
  "Get workflow phase for REPO-NAME/BRANCH-NAME, or nil if no workflow."
  (when-let* ((metadata (claude-metadata-read repo-name branch-name))
              (workflow (plist-get metadata :workflow)))
    (plist-get workflow :phase)))

(provide 'claude-state)
;;; claude-state.el ends here
