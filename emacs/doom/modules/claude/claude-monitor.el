;;; claude-monitor.el --- Attention monitoring -*- lexical-binding: t; -*-

;;; Commentary:
;; Monitors Claude vterm buffers for attention needs.
;; No dependencies on other claude modules.
;; Dashboard is the sole UI for workspace status — no modeline.

;;; Code:

;;; Customization

(defcustom claude-monitor-interval 2
  "Seconds between attention checks."
  :type 'integer
  :group 'claude)

(defcustom claude-attention-idle-threshold 3
  "Seconds of idle output before checking attention patterns."
  :type 'integer
  :group 'claude)

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
  :group 'claude)

;;; Hooks

(defvar claude-attention-change-hook nil
  "Hook run when any workspace's attention status changes.
Called with (workspace-name new-status).
NEW-STATUS is a symbol: `working', `idle', `error', or nil.")

;;; Internal State

(defvar claude-monitor-timer nil
  "Timer for attention monitoring.")

;; Buffer-local state
(defvar-local claude--last-output-time nil
  "Timestamp of last detected output change.")

(defvar-local claude--last-content-hash nil
  "Hash of recent buffer content to detect changes.")

(defvar-local claude--needs-attention nil
  "Non-nil if this buffer needs user attention.")

;;; Buffer Name Matching

(defun claude-monitor--workspace-for-buffer (buffer)
  "Get workspace name for BUFFER based on buffer name pattern.
Buffer names follow `*claude:REPO:BRANCH*'.
Returns \"REPO:BRANCH\" or nil."
  (let ((name (buffer-name buffer)))
    (when (string-match "^\\*claude:\\([^:]+\\):\\([^*]+\\)\\*$" name)
      (format "%s:%s" (match-string 1 name) (match-string 2 name)))))

(defun claude-monitor--claude-buffers ()
  "Return list of all live claude vterm buffers."
  (seq-filter (lambda (buf)
                (and (buffer-live-p buf)
                     (string-match-p "^\\*claude:[^:]+:[^*]+\\*$"
                                     (buffer-name buf))))
              (buffer-list)))

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
         ;; Content changed — update timestamp, clear attention
         ((not (equal current-hash claude--last-content-hash))
          (setq claude--last-content-hash current-hash
                claude--last-output-time now
                claude--needs-attention nil))
         ;; Content stable long enough — check patterns
         ((and claude--last-output-time
               (> (- now claude--last-output-time) claude-attention-idle-threshold))
          (let ((content (claude--get-last-n-lines buffer 15))
                (old-attention claude--needs-attention))
            (setq claude--needs-attention
                  (claude--matches-attention-pattern-p content))
            ;; Fire hook if attention state changed
            (when (not (eq old-attention claude--needs-attention))
              (when-let ((ws-name (claude-monitor--workspace-for-buffer buffer)))
                (run-hook-with-args 'claude-attention-change-hook
                                    ws-name
                                    (claude-workspace-attention ws-name)))))))))))

(defun claude--check-all-buffers ()
  "Check all Claude vterm buffers for attention needs."
  (dolist (buffer (claude-monitor--claude-buffers))
    (claude--check-buffer-attention buffer)))

;;; Public Query Interface

(defun claude-workspace-attention (workspace-name)
  "Return attention status for WORKSPACE-NAME.
Returns symbol: `working', `idle', `error', or nil.
WORKSPACE-NAME is \"REPO:BRANCH\"."
  (let ((buffer-name (format "*claude:%s*" workspace-name)))
    (if-let ((buffer (get-buffer buffer-name)))
        (if (buffer-local-value 'claude--needs-attention buffer)
            'idle
          'working)
      nil)))

;;; Monitor Control

;;;###autoload
(defun claude-monitor-start ()
  "Start attention monitoring."
  (interactive)
  (unless claude-monitor-timer
    (setq claude-monitor-timer
          (run-with-timer claude-monitor-interval
                          claude-monitor-interval
                          #'claude--check-all-buffers))))

;;;###autoload
(defun claude-monitor-stop ()
  "Stop attention monitoring."
  (interactive)
  (when claude-monitor-timer
    (cancel-timer claude-monitor-timer)
    (setq claude-monitor-timer nil)))

;;;###autoload
(defun claude-monitor-toggle ()
  "Toggle monitoring on/off."
  (interactive)
  (if claude-monitor-timer
      (claude-monitor-stop)
    (claude-monitor-start)))

(provide 'claude-monitor)
;;; claude-monitor.el ends here
