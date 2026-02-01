;;; claude-worktree.el --- Git worktree management -*- lexical-binding: t; -*-

;;; Commentary:
;; Git worktree operations and metadata management for Claude workspaces.

;;; Code:

(require 'json)

;; Forward declarations for customizable variables defined in claude.el
(defvar claude-worktree-dir)
(defvar claude-metadata-dir)

;;; Path utilities

(defun claude-worktree-path (repo-name branch-name)
  "Return path to worktree for REPO-NAME and BRANCH-NAME."
  (expand-file-name (format "%s/%s" repo-name branch-name)
                    (expand-file-name claude-worktree-dir)))

(defun claude-metadata-path (repo-name branch-name)
  "Return path to metadata file for REPO-NAME and BRANCH-NAME."
  (expand-file-name (format "%s/%s.json" repo-name branch-name)
                    (expand-file-name claude-metadata-dir)))

(defun claude-repo-name (repo-path)
  "Extract repo name from REPO-PATH.
Uses the final path component."
  (file-name-nondirectory (directory-file-name (expand-file-name repo-path))))

;;; Metadata operations

(defun claude-metadata-write (repo-name branch-name data)
  "Write DATA (plist) as metadata for REPO-NAME/BRANCH-NAME."
  (let* ((path (claude-metadata-path repo-name branch-name))
         (dir (file-name-directory path))
         (json-object-type 'plist)
         (json-data (json-encode data)))
    (make-directory dir t)
    (with-temp-file path
      (insert json-data))
    t))

(defun claude-metadata-read (repo-name branch-name)
  "Read metadata for REPO-NAME/BRANCH-NAME.
Returns plist or nil if not found."
  (let ((path (claude-metadata-path repo-name branch-name)))
    (when (file-exists-p path)
      (let ((json-object-type 'plist)
            (json-key-type 'keyword))
        (json-read-file path)))))

(defun claude-metadata-delete (repo-name branch-name)
  "Delete metadata file for REPO-NAME/BRANCH-NAME."
  (let ((path (claude-metadata-path repo-name branch-name)))
    (when (file-exists-p path)
      (delete-file path)
      ;; Clean up empty parent directories
      (let ((dir (file-name-directory path)))
        (when (and (file-directory-p dir)
                   (null (directory-files dir nil "^[^.]")))
          (delete-directory dir))))))

;;; Worktree operations

(defun claude-worktree-create (repo-path branch-name parent-branch)
  "Create worktree for BRANCH-NAME from REPO-PATH, branching from PARENT-BRANCH.
Returns (success . path-or-error) cons cell.
Error can be symbol `branch-exists' or `dir-exists', or string with error message."
  (let* ((repo-name (claude-repo-name repo-path))
         (worktree-path (claude-worktree-path repo-name branch-name))
         (worktree-dir (file-name-directory worktree-path)))
    ;; Ensure parent directory exists
    (make-directory worktree-dir t)
    ;; Run git worktree add
    (let* ((default-directory (expand-file-name repo-path))
           (cmd (format "git worktree add -b %s %s %s 2>&1"
                        (shell-quote-argument branch-name)
                        (shell-quote-argument worktree-path)
                        (shell-quote-argument parent-branch)))
           (output (shell-command-to-string cmd))
           (exit-code (process-exit-status (get-buffer-process (current-buffer)))))
      (cond
       ;; Check for specific error patterns in output
       ((string-match-p "branch named '.*' already exists" output)
        (cons nil 'branch-exists))
       ((string-match-p "already exists" output)
        (cons nil 'dir-exists))
       ;; Success if worktree directory exists
       ((file-directory-p worktree-path)
        ;; Write metadata
        (claude-metadata-write repo-name branch-name
                               (list :parent_branch parent-branch
                                     :parent_repo (expand-file-name repo-path)
                                     :created (format-time-string "%Y-%m-%dT%H:%M:%SZ" nil t)))
        (cons t worktree-path))
       ;; Unknown error
       (t
        (cons nil output))))))

(defun claude-worktree-create-from-existing (repo-path branch-name)
  "Create worktree for existing BRANCH-NAME from REPO-PATH.
Use when branch already exists and user wants to reuse it.
Returns (success . path-or-error) cons cell."
  (let* ((repo-name (claude-repo-name repo-path))
         (worktree-path (claude-worktree-path repo-name branch-name))
         (worktree-dir (file-name-directory worktree-path)))
    (make-directory worktree-dir t)
    (let* ((default-directory (expand-file-name repo-path))
           ;; Get parent branch info before creating worktree
           (parent-branch (string-trim
                           (shell-command-to-string
                            (format "git rev-parse --abbrev-ref %s@{upstream} 2>/dev/null || git rev-parse --abbrev-ref HEAD"
                                    (shell-quote-argument branch-name)))))
           (cmd (format "git worktree add %s %s 2>&1"
                        (shell-quote-argument worktree-path)
                        (shell-quote-argument branch-name)))
           (output (shell-command-to-string cmd)))
      (cond
       ((string-match-p "already exists" output)
        (cons nil 'dir-exists))
       ((file-directory-p worktree-path)
        (claude-metadata-write repo-name branch-name
                               (list :parent_branch parent-branch
                                     :parent_repo (expand-file-name repo-path)
                                     :created (format-time-string "%Y-%m-%dT%H:%M:%SZ" nil t)))
        (cons t worktree-path))
       (t
        (cons nil output))))))

(defun claude-worktree-remove (repo-name branch-name)
  "Remove worktree for BRANCH-NAME in REPO-NAME.
Returns (success . nil-or-error) cons cell."
  (let* ((worktree-path (claude-worktree-path repo-name branch-name))
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
            (progn
              (claude-metadata-delete repo-name branch-name)
              (cons t nil))
          (cons nil output))))))

(defun claude-worktree-list ()
  "List all worktrees across all repos.
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
                             (format "git merge %s 2>&1"
                                     (shell-quote-argument source-branch)))))
          (cond
           ((string-match-p "CONFLICT" merge-result)
            ;; Abort the merge
            (shell-command-to-string "git merge --abort")
            (cons nil "Merge conflicts detected"))
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

(provide 'claude-worktree)
;;; claude-worktree.el ends here
