;;; claude-worktree.el --- Git worktree management -*- lexical-binding: t; -*-

;;; Commentary:
;; Git worktree operations for Claude workspaces.
;; Metadata operations have been moved to claude-state.el.

;;; Code:

(require 'claude-state)

;; Forward declarations for customizable variables defined in claude.el
(defvar claude-worktree-dir)

;;; Path utilities (re-exported from claude-state for backwards compatibility)

(defalias 'claude-worktree-path #'claude--worktree-path)
(defalias 'claude-metadata-path #'claude--metadata-path)
(defalias 'claude-repo-name #'claude--repo-name)

;; Note: Metadata operations (claude-metadata-write, claude-metadata-read,
;; claude-metadata-delete) are now in claude-state.el and exported directly.

;;; Worktree operations

(defun claude-worktree-create (repo-path branch-name parent-branch)
  "Create worktree for BRANCH-NAME from REPO-PATH.
Branches from PARENT-BRANCH.  Returns (success . path-or-error) cons.
Error can be symbol `branch-exists', `dir-exists', or error string.
Does NOT create metadata - caller should use `claude--create-metadata'."
  (let* ((repo-name (claude--repo-name repo-path))
         (worktree-path (claude--worktree-path repo-name branch-name))
         (worktree-dir (file-name-directory worktree-path)))
    ;; Ensure parent directory exists
    (make-directory worktree-dir t)
    ;; Run git worktree add
    (let* ((default-directory (expand-file-name repo-path))
           (cmd (format "git worktree add -b %s %s %s 2>&1"
                        (shell-quote-argument branch-name)
                        (shell-quote-argument worktree-path)
                        (shell-quote-argument parent-branch)))
           (output (shell-command-to-string cmd)))
      (cond
       ;; Check for specific error patterns in output
       ((string-match-p "branch named '.*' already exists" output)
        (cons nil 'branch-exists))
       ((string-match-p "already exists" output)
        (cons nil 'dir-exists))
       ;; Success if worktree directory exists
       ((file-directory-p worktree-path)
        (cons t worktree-path))
       ;; Unknown error
       (t
        (cons nil output))))))

(defun claude-worktree-create-from-existing (repo-path branch-name)
  "Create worktree for existing BRANCH-NAME from REPO-PATH.
Use when branch already exists and user wants to reuse it.
Returns (success . path-or-error) cons cell.
Note: Does NOT create metadata - caller should use claude--create-metadata."
  (let* ((repo-name (claude--repo-name repo-path))
         (worktree-path (claude--worktree-path repo-name branch-name))
         (worktree-dir (file-name-directory worktree-path)))
    (make-directory worktree-dir t)
    (let* ((default-directory (expand-file-name repo-path))
           (cmd (format "git worktree add %s %s 2>&1"
                        (shell-quote-argument worktree-path)
                        (shell-quote-argument branch-name)))
           (output (shell-command-to-string cmd)))
      (cond
       ((string-match-p "already exists" output)
        (cons nil 'dir-exists))
       ((file-directory-p worktree-path)
        (cons t worktree-path))
       (t
        (cons nil output))))))

(defun claude-worktree-remove (repo-name branch-name)
  "Remove worktree for BRANCH-NAME in REPO-NAME.
Returns (success . nil-or-error) cons cell.
Note: Does NOT delete metadata - caller should use claude-metadata-delete."
  (let* ((worktree-path (claude--worktree-path repo-name branch-name))
         (metadata (claude-metadata-read repo-name branch-name))
         (parent-repo (plist-get metadata :parent_repo)))
    (if (not parent-repo)
        (cons nil "Cannot find parent repo in metadata")
      (let* ((default-directory parent-repo)
             (cmd (format "git worktree remove --force %s 2>&1"
                          (shell-quote-argument worktree-path)))
             (output (shell-command-to-string cmd)))
        (if (or (string-match-p "^$" output)
                (not (file-directory-p worktree-path)))
            (cons t nil)
          (cons nil output))))))

(defun claude-worktree-remove-force (repo-name branch-name)
  "Force remove worktree for REPO-NAME/BRANCH-NAME.
Removes even if there are uncommitted changes.
Returns (success . nil-or-error) cons cell."
  (let* ((worktree-path (claude--worktree-path repo-name branch-name))
         (metadata (claude-metadata-read repo-name branch-name))
         (parent-repo (plist-get metadata :parent_repo)))
    (cond
     ((not parent-repo)
      ;; No parent repo - try to remove directory directly
      (when (file-directory-p worktree-path)
        (delete-directory worktree-path t))
      (cons t nil))
     (t
      (let* ((default-directory parent-repo)
             (cmd (format "git worktree remove --force %s 2>&1"
                          (shell-quote-argument worktree-path)))
             (output (shell-command-to-string cmd)))
        ;; Even if git fails, try direct removal
        (when (file-directory-p worktree-path)
          (ignore-errors (delete-directory worktree-path t)))
        (if (not (file-directory-p worktree-path))
            (cons t nil)
          (cons nil output)))))))

(defun claude-worktree-list ()
  "List all worktrees across all repos.
Returns list of (repo-name . branch-name) pairs."
  (claude--list-all-workspaces))

;;; Git operations

(defun claude-git-current-branch (repo-path)
  "Get current branch name in REPO-PATH."
  (let ((default-directory (expand-file-name repo-path)))
    (string-trim
     (shell-command-to-string "git rev-parse --abbrev-ref HEAD 2>/dev/null"))))

(defun claude-git-commits-ahead (worktree-path parent-branch)
  "Count commits ahead of PARENT-BRANCH in WORKTREE-PATH.
Returns -1 if unable to determine (missing parent-branch, not a git repo, etc.)."
  (cond
   ;; No worktree path
   ((not (file-directory-p worktree-path)) -1)
   ;; No parent branch
   ((or (null parent-branch) (string-empty-p parent-branch)) -1)
   ;; Try to count commits
   (t (let* ((default-directory worktree-path)
             (result (string-trim
                      (shell-command-to-string
                       (format "git rev-list --count %s..HEAD 2>&1"
                               (shell-quote-argument parent-branch))))))
        ;; Check if result is a number
        (if (string-match-p "^[0-9]+$" result)
            (string-to-number result)
          -1)))))

(defun claude-git-commits-behind (worktree-path parent-branch)
  "Count commits behind PARENT-BRANCH in WORKTREE-PATH.
Returns -1 if unable to determine."
  (cond
   ((not (file-directory-p worktree-path)) -1)
   ((or (null parent-branch) (string-empty-p parent-branch)) -1)
   (t (let* ((default-directory worktree-path)
             (result (string-trim
                      (shell-command-to-string
                       (format "git rev-list --count HEAD..%s 2>&1"
                               (shell-quote-argument parent-branch))))))
        (if (string-match-p "^[0-9]+$" result)
            (string-to-number result)
          -1)))))

(defun claude-git-has-uncommitted-changes (worktree-path)
  "Return t if WORKTREE-PATH has uncommitted changes."
  (when (file-directory-p worktree-path)
    (let* ((default-directory worktree-path)
           (status (string-trim (shell-command-to-string
                                 "git status --porcelain 2>/dev/null"))))
      (not (string-empty-p status)))))

(defun claude-git-merge-branch (repo-path target-branch source-branch)
  "Merge SOURCE-BRANCH into TARGET-BRANCH in REPO-PATH.
Returns (success . nil-or-error) cons cell."
  (let ((default-directory (expand-file-name repo-path)))
    ;; First checkout target branch
    (let ((checkout-result (shell-command-to-string
                            (format "git checkout %s 2>&1"
                                    (shell-quote-argument target-branch)))))
      (if (string-match-p "error\\|fatal" checkout-result)
          (cons nil (format "Checkout failed: %s" checkout-result))
        ;; Then merge
        (let ((merge-result (shell-command-to-string
                             (format "git merge --no-ff %s 2>&1"
                                     (shell-quote-argument source-branch)))))
          (cond
           ((string-match-p "CONFLICT" merge-result)
            ;; Abort the merge
            (shell-command-to-string "git merge --abort")
            (cons nil 'conflict))
           ((string-match-p "error\\|fatal" merge-result)
            (cons nil merge-result))
           (t
            (cons t nil))))))))

(defun claude-git-delete-branch (repo-path branch-name)
  "Delete BRANCH-NAME from REPO-PATH.
Returns (success . nil-or-error) cons cell."
  (let* ((default-directory (expand-file-name repo-path))
         (result (shell-command-to-string
                  (format "git branch -d %s 2>&1"
                          (shell-quote-argument branch-name)))))
    (if (string-match-p "error\\|fatal" result)
        ;; Try force delete if regular delete fails
        (let ((force-result (shell-command-to-string
                             (format "git branch -D %s 2>&1"
                                     (shell-quote-argument branch-name)))))
          (if (string-match-p "error\\|fatal" force-result)
              (cons nil force-result)
            (cons t nil)))
      (cons t nil))))

(defun claude-git-fetch (repo-path &optional remote)
  "Fetch from REMOTE (default origin) in REPO-PATH.
Returns (success . nil-or-error) cons cell."
  (let* ((default-directory (expand-file-name repo-path))
         (remote-name (or remote "origin"))
         (result (shell-command-to-string
                  (format "git fetch %s 2>&1"
                          (shell-quote-argument remote-name)))))
    (if (string-match-p "fatal:" result)
        (cons nil result)
      (cons t nil))))

(defun claude-git-repo-p (path)
  "Return t if PATH is inside a git repository."
  (when (file-directory-p path)
    (let ((default-directory path))
      (= 0 (shell-command "git rev-parse --git-dir >/dev/null 2>&1")))))

(defun claude--detect-parent-branch (repo-path branch-name)
  "Detect likely parent branch for existing BRANCH-NAME in REPO-PATH."
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

(provide 'claude-worktree)
;;; claude-worktree.el ends here
