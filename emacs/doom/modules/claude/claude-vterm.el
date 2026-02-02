;;; claude-vterm.el --- vterm buffer management -*- lexical-binding: t; -*-

;;; Commentary:
;; vterm buffer creation, management, and cleanup for Claude workspaces.
;; Handles the primary Claude buffer and extra terminal buffers.

;;; Code:

;; vterm is loaded lazily - only required when actually creating terminals
(declare-function vterm "vterm")
(declare-function vterm-send-string "vterm")

(require 'claude-state)

;;; vterm Creation

(defun claude--create-vterm-in-dir (buffer-name dir)
  "Create vterm buffer with BUFFER-NAME in directory DIR.
Returns the buffer."
  (let ((default-directory (expand-file-name dir)))
    (with-current-buffer (vterm buffer-name)
      ;; Ensure we're in the right directory
      (vterm-send-string (format "cd %s && clear\n" (shell-quote-argument dir)))
      (current-buffer))))

(defun claude--send-command (buffer-name command)
  "Send COMMAND to vterm BUFFER-NAME."
  (when-let ((buf (get-buffer buffer-name)))
    (with-current-buffer buf
      (vterm-send-string (concat command "\n")))))

;;; Terminal Buffer Operations

(defun claude--terminal-buffers (repo-name branch-name)
  "Return list of extra terminal buffers for workspace.
Does not include the primary Claude buffer."
  (let ((prefix (format "*term:%s:%s:" repo-name branch-name))
        (result nil))
    (dolist (buf (buffer-list))
      (when (string-prefix-p prefix (buffer-name buf))
        (push buf result)))
    (nreverse result)))

(defun claude--terminal-count (repo-name branch-name)
  "Return count of extra terminals for workspace."
  (length (claude--terminal-buffers repo-name branch-name)))

(defun claude--next-terminal-number (repo-name branch-name)
  "Find the next available terminal number.
Reuses gaps in the sequence (e.g., if 1,3 exist, returns 2)."
  (let* ((buffers (claude--terminal-buffers repo-name branch-name))
         (numbers (mapcar (lambda (buf)
                            (when (string-match ":\\([0-9]+\\)\\*$" (buffer-name buf))
                              (string-to-number (match-string 1 (buffer-name buf)))))
                          buffers))
         (numbers (delq nil numbers))
         (n 1))
    ;; Find first gap
    (while (memq n numbers)
      (setq n (1+ n)))
    n))

;;; Buffer Cleanup

(defun claude--kill-workspace-buffers (repo-name branch-name)
  "Kill all buffers belonging to this workspace.
Kills Claude buffer, terminal buffers, and file buffers in the worktree."
  (let ((claude-buf-name (claude--buffer-name repo-name branch-name))
        (metadata (claude-metadata-read repo-name branch-name)))
    ;; 1. Kill Claude buffer
    (when-let ((buf (get-buffer claude-buf-name)))
      (let ((kill-buffer-query-functions nil))  ; Skip "process running" prompt
        (kill-buffer buf)))

    ;; 2. Kill all terminal buffers
    (dolist (buf (claude--terminal-buffers repo-name branch-name))
      (let ((kill-buffer-query-functions nil))
        (kill-buffer buf)))

    ;; 3. Kill file buffers visiting worktree files
    (when-let ((worktree-path (plist-get metadata :worktree_path)))
      (let ((expanded-path (expand-file-name worktree-path)))
        (dolist (buf (buffer-list))
          (when-let ((file (buffer-file-name buf)))
            (when (string-prefix-p expanded-path (expand-file-name file))
              (kill-buffer buf))))))))

;;; Buffer Queries

(defun claude--get-claude-buffer (repo-name branch-name)
  "Get the Claude buffer for workspace, or nil if not found."
  (get-buffer (claude--buffer-name repo-name branch-name)))

(defun claude--claude-buffer-exists-p (repo-name branch-name)
  "Return t if Claude buffer exists for workspace."
  (buffer-live-p (claude--get-claude-buffer repo-name branch-name)))

;;; Terminal Buffer Name (for backwards compatibility)

(defun claude-terminal-buffer-name (repo-name branch-name)
  "Generate next available terminal buffer name for REPO-NAME and BRANCH-NAME.
Finds the first available number, reusing gaps in the sequence."
  (let ((n (claude--next-terminal-number repo-name branch-name)))
    (claude--terminal-buffer-name repo-name branch-name n)))

(provide 'claude-vterm)
;;; claude-vterm.el ends here
