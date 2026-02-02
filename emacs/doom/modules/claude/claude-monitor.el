;;; claude-monitor.el --- Attention monitoring -*- lexical-binding: t; -*-

;;; Commentary:
;; Monitors Claude buffers for attention needs and updates modeline.
;; Subscribes to state-change-hook to skip non-active workspaces.

;;; Code:

(require 'claude-state)

;; Forward declarations
(declare-function +workspace/switch-to "~/.config/emacs/modules/ui/workspaces/autoload/workspaces")

(defcustom claude-monitor-interval 2
  "Seconds between attention checks."
  :type 'integer
  :group 'claude-workflow)

(defcustom claude-attention-idle-threshold 3
  "Seconds of idle before checking patterns."
  :type 'integer
  :group 'claude-workflow)

(defcustom claude-attention-patterns
  '(;; Permission prompts (Claude Code specific)
    "Allow .* to \\(run\\|read\\|write\\|edit\\)"
    "\\[y/n\\]"
    "\\[Y/n\\]"
    "\\[y/N\\]"
    ;; Tool confirmations
    "Do you want to proceed"
    "Would you like"
    "Proceed\\?"
    "Continue\\?"
    ;; Empty prompt (Claude waiting for input)
    "^> *$"
    "^❯ *$"
    ;; MCP permission prompts
    "needs your permission")
  "Patterns indicating Claude needs attention."
  :type '(repeat string)
  :group 'claude-workflow)

(defvar claude-monitor-timer nil
  "Timer for attention monitoring.")

(defvar claude-monitor--attention-state (make-hash-table :test 'equal)
  "Hash table mapping workspace-name -> attention boolean.")

;; Buffer-local state
(defvar-local claude--last-output-time nil
  "Timestamp of last detected output change.")

(defvar-local claude--last-content-hash nil
  "Hash of recent buffer content to detect changes.")

(defvar-local claude--needs-attention nil
  "Non-nil if this buffer needs user attention.")

;; Faces
(defface claude-attention-face
  '((t :foreground "#ff6c6b" :weight bold))
  "Face for Claude attention indicator.")

(defface claude-idle-face
  '((t :foreground "#5B6268"))
  "Face for Claude idle indicator.")

;;; Content Analysis

(defun claude--get-last-n-lines (buffer n)
  "Get last N lines from vterm BUFFER, filtering fake newlines."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-max))
        (forward-line (- n))
        (let ((text (buffer-substring (point) (point-max))))
          (if (fboundp 'vterm--filter-buffer-substring)
              (vterm--filter-buffer-substring text)
            text))))))

(defun claude--content-hash (buffer)
  "Calculate MD5 hash of last 15 lines of BUFFER."
  (let ((content (claude--get-last-n-lines buffer 15)))
    (when content
      (md5 content))))

(defun claude--matches-attention-pattern-p (text)
  "Return non-nil if TEXT matches any attention pattern."
  (when text
    (seq-some (lambda (pattern)
                (string-match-p pattern text))
              claude-attention-patterns)))

;;; Buffer Attention Checking

(defun claude--check-buffer-attention (buffer)
  "Check if BUFFER needs attention and update its state."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((current-hash (claude--content-hash buffer))
            (now (float-time)))
        (cond
         ;; Content changed - update timestamp, clear attention
         ((not (equal current-hash claude--last-content-hash))
          (setq claude--last-content-hash current-hash
                claude--last-output-time now
                claude--needs-attention nil))
         ;; Content stable long enough - check patterns
         ((and claude--last-output-time
               (> (- now claude--last-output-time) claude-attention-idle-threshold))
          (let ((content (claude--get-last-n-lines buffer 15))
                (old-attention claude--needs-attention))
            (setq claude--needs-attention
                  (claude--matches-attention-pattern-p content))
            ;; Fire hook if attention state changed
            (when (not (eq old-attention claude--needs-attention))
              (when-let ((ws-name (claude--workspace-for-buffer buffer)))
                (run-hook-with-args 'claude-attention-change-hook
                                    ws-name claude--needs-attention))))))))))

(defun claude--workspace-for-buffer (buffer)
  "Get workspace name for BUFFER based on buffer name pattern."
  (let ((name (buffer-name buffer)))
    (when (string-match "^\\*claude:\\([^:]+\\):\\([^*]+\\)\\*$" name)
      (format "%s:%s" (match-string 1 name) (match-string 2 name)))))

(defun claude--should-check-workspace-p (repo-name branch-name)
  "Return t if workspace REPO-NAME/BRANCH-NAME should be checked for attention.
Only active workspaces should be checked."
  (let ((metadata (claude-metadata-read repo-name branch-name)))
    (equal (plist-get metadata :status) "active")))

(defun claude--check-all-buffers ()
  "Check all Claude buffers for attention needs.
Skips non-active workspaces."
  (dolist (buffer (buffer-list))
    (let ((name (buffer-name buffer)))
      (when (string-match "^\\*claude:\\([^:]+\\):\\([^*]+\\)\\*$" name)
        (let ((repo-name (match-string 1 name))
              (branch-name (match-string 2 name)))
          (when (claude--should-check-workspace-p repo-name branch-name)
            (claude--check-buffer-attention buffer))))))
  ;; Force modeline update
  (force-mode-line-update t))

;;; Public Query Functions

(defun claude--any-needs-attention-p ()
  "Return t if any Claude buffer needs attention."
  (seq-some (lambda (buffer)
              (when (string-match-p "^\\*claude:" (buffer-name buffer))
                (buffer-local-value 'claude--needs-attention buffer)))
            (buffer-list)))

(defun claude--get-attention-buffer ()
  "Return first Claude buffer needing attention, or nil."
  (seq-find (lambda (buffer)
              (and (string-match-p "^\\*claude:" (buffer-name buffer))
                   (buffer-local-value 'claude--needs-attention buffer)))
            (buffer-list)))

;;; Monitor Control

;;;###autoload
(defun claude-monitor-start ()
  "Start attention monitoring."
  (interactive)
  (unless claude-monitor-timer
    (setq claude-monitor-timer
          (run-with-timer claude-monitor-interval
                          claude-monitor-interval
                          #'claude--check-all-buffers))
    (message "Claude monitor started")))

;;;###autoload
(defun claude-monitor-stop ()
  "Stop attention monitoring."
  (interactive)
  (when claude-monitor-timer
    (cancel-timer claude-monitor-timer)
    (setq claude-monitor-timer nil)
    (message "Claude monitor stopped")))

;;;###autoload
(defun claude-monitor-toggle ()
  "Toggle monitoring on/off."
  (interactive)
  (if claude-monitor-timer
      (claude-monitor-stop)
    (claude-monitor-start)))

;;;###autoload
(defun claude-jump-to-attention ()
  "Jump to first Claude buffer needing attention."
  (interactive)
  (if-let ((buffer (claude--get-attention-buffer)))
      (progn
        ;; Extract workspace name from buffer name
        (when (string-match "^\\*claude:\\(.+\\)\\*$" (buffer-name buffer))
          (let ((workspace-name (match-string 1 (buffer-name buffer))))
            (+workspace/switch-to workspace-name)))
        (switch-to-buffer buffer))
    (message "No Claude buffers need attention")))

;;; State Change Handler

(defun claude-monitor--on-state-change (workspace-name _old-status new-status)
  "Handle workspace state changes.
Clear attention state when workspace becomes non-active."
  (unless (eq new-status 'active)
    ;; Clear attention state for this workspace
    (remhash workspace-name claude-monitor--attention-state)
    ;; Clear buffer-local state if buffer exists
    (let ((parsed (claude--parse-workspace-name workspace-name)))
      (when parsed
        (let ((buffer-name (claude--buffer-name (car parsed) (cdr parsed))))
          (when-let ((buffer (get-buffer buffer-name)))
            (with-current-buffer buffer
              (setq claude--needs-attention nil))))))))

;; Subscribe to state changes
(add-hook 'claude-state-change-hook #'claude-monitor--on-state-change)

;;; Modeline Segment

(with-eval-after-load 'doom-modeline
  (doom-modeline-def-segment claude-status
    "Display Claude session status."
    (let* ((workspaces (claude--list-active-workspaces))
           (needs-attention (claude--any-needs-attention-p))
           (map (make-sparse-keymap)))
      (when workspaces
        (define-key map [mode-line mouse-1] #'claude-jump-to-attention)
        (concat
         (doom-modeline-spc)
         (propertize
          (if needs-attention "● Claude" "○ Claude")
          'face (if needs-attention 'claude-attention-face 'claude-idle-face)
          'mouse-face 'doom-modeline-highlight
          'local-map map
          'help-echo (format "%d Claude session(s)%s - click to jump"
                            (length workspaces)
                            (if needs-attention " (attention needed)" ""))))))))

;;; Auto-start on Doom Init

(defun claude-monitor--maybe-start ()
  "Start monitor if there are existing Claude workspaces."
  (when (claude--list-active-workspaces)
    (claude-monitor-start)))

;; Hook to be called from claude.el after Doom is ready
(defvar claude-monitor--init-hook-added nil
  "Non-nil if init hook has been added.")

(provide 'claude-monitor)
;;; claude-monitor.el ends here
