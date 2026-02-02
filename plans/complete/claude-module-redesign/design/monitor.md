# Claude Module: Monitor

Last updated: 2026-02-01 (reviewed)

## Purpose

The monitor detects when Claude sessions need user attention (permission prompts, questions, errors) and updates the UI accordingly.

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                    Monitor Timer                             │
│         (Runs every claude-monitor-interval seconds)         │
└─────────────────────────────────────────────────────────────┘
                           │
                           ▼
┌─────────────────────────────────────────────────────────────┐
│              For each active workspace:                      │
│  1. Check if buffer has been idle > threshold                │
│  2. If idle, scan last N lines for attention patterns        │
│  3. Update attention state                                   │
└─────────────────────────────────────────────────────────────┘
                           │
                           ▼
┌─────────────────────────────────────────────────────────────┐
│              On attention state change:                      │
│  - Fire claude-attention-change-hook                         │
│  - Dashboard auto-refreshes                                  │
│  - Modeline auto-updates                                     │
└─────────────────────────────────────────────────────────────┘
```

## Attention Detection Strategy

### Why Timer-Based?

Alternatives considered:

| Approach | Pros | Cons |
|----------|------|------|
| Process filter | Immediate detection | Complex, can miss patterns split across chunks |
| Timer + idle check | Simple, reliable | Slight delay (acceptable) |
| After-change hook | Immediate | Performance concern, complexity |

**Decision:** Timer with idle threshold. Simple, reliable, and the 2-3 second delay is acceptable for the use case.

### Idle Threshold

We only check for attention patterns when the buffer has been "idle" (no new output) for a threshold period. This prevents false positives from matching partial output.

```elisp
(defcustom claude-monitor-interval 2
  "Seconds between attention checks."
  :type 'number
  :group 'claude-workflow)

(defcustom claude-attention-idle-threshold 3
  "Seconds of no output before checking for attention patterns.
This prevents matching partial output mid-stream."
  :type 'number
  :group 'claude-workflow)
```

### Buffer Activity Tracking

Track last activity time per buffer:

```elisp
(defvar claude-monitor--last-activity (make-hash-table :test 'equal)
  "Hash table mapping buffer names to last activity timestamp.")

(defun claude-monitor--record-activity (buffer-name)
  "Record activity in BUFFER-NAME."
  (puthash buffer-name (current-time) claude-monitor--last-activity))

(defun claude-monitor--idle-p (buffer-name)
  "Return t if BUFFER-NAME has been idle longer than threshold."
  (let ((last (gethash buffer-name claude-monitor--last-activity)))
    (or (null last)
        (> (float-time (time-subtract nil last))
           claude-attention-idle-threshold))))

;; Hook into vterm to track activity
(defun claude-monitor--vterm-output-filter (output)
  "Track output activity in vterm buffers."
  (when (and (bound-and-true-p vterm-buffer-name)
             (string-prefix-p "*claude:" (buffer-name)))
    (claude-monitor--record-activity (buffer-name)))
  output)

(add-hook 'vterm-output-filter-functions #'claude-monitor--vterm-output-filter)
```

## Attention Patterns

Patterns that indicate Claude needs user input:

```elisp
(defcustom claude-attention-patterns
  '(;; Permission prompts
    "Allow .* to \\(run\\|read\\|write\\|edit\\|execute\\)"
    "Allow once\\|Allow always"
    "needs your permission"
    "Press Enter to allow"

    ;; Yes/no prompts
    "\\[y/n\\]"
    "\\[Y/n\\]"
    "\\[n/Y\\]"
    "\\[yes/no\\]"
    "(y/n)"

    ;; Interactive prompts
    "^> *$"
    "^❯ *$"
    "Press Enter"
    "Press any key"

    ;; Questions from Claude
    "Would you like"
    "Do you want"
    "Should I"
    "Which \\(one\\|option\\)"
    "Please \\(choose\\|select\\|confirm\\)"

    ;; Errors that need attention
    "Error:"
    "error\\[E"
    "FAILED"
    "fatal:"

    ;; Waiting states
    "Waiting for"
    "^\\$ *$")
  "Patterns indicating Claude needs user attention.
Matched against the last `claude-attention-scan-lines` lines of output."
  :type '(repeat regexp)
  :group 'claude-workflow)

(defcustom claude-attention-scan-lines 50
  "Number of lines from end of buffer to scan for attention patterns."
  :type 'integer
  :group 'claude-workflow)
```

### Pattern Matching

```elisp
(defun claude-monitor--check-attention (buffer-name)
  "Check if BUFFER-NAME contains attention patterns.
Returns t if attention needed, nil otherwise."
  (when-let ((buf (get-buffer buffer-name)))
    (with-current-buffer buf
      (save-excursion
        (goto-char (point-max))
        (let* ((start (save-excursion
                        (forward-line (- claude-attention-scan-lines))
                        (point)))
               (text (buffer-substring-no-properties start (point-max))))
          (cl-some (lambda (pattern)
                     (string-match-p pattern text))
                   claude-attention-patterns))))))
```

## Per-Workspace Attention State

Attention is tracked per workspace in a hash table (not in metadata, since it's ephemeral):

```elisp
(defvar claude-monitor--attention-state (make-hash-table :test 'equal)
  "Hash table mapping workspace names to attention state (t/nil).")

(defun claude-monitor--set-attention (workspace-name needs-attention)
  "Set attention state for WORKSPACE-NAME."
  (let ((old-state (gethash workspace-name claude-monitor--attention-state)))
    (unless (eq old-state needs-attention)
      (puthash workspace-name needs-attention claude-monitor--attention-state)
      ;; Fire attention hook (separate from lifecycle state-change-hook)
      (run-hook-with-args 'claude-attention-change-hook
                          workspace-name
                          needs-attention))))

(defun claude-monitor--get-attention (workspace-name)
  "Get attention state for WORKSPACE-NAME."
  (gethash workspace-name claude-monitor--attention-state))

(defun claude-monitor--clear-attention (workspace-name)
  "Clear attention state when user switches to workspace."
  (claude-monitor--set-attention workspace-name nil))
```

## Monitor Timer

```elisp
(defvar claude-monitor--timer nil
  "Timer for periodic attention checks.")

(defun claude-monitor--tick ()
  "Run one monitor check cycle."
  (dolist (ws (claude--list-active-workspaces))
    (let* ((repo-name (car ws))
           (branch-name (cdr ws))
           (workspace-name (claude--workspace-name repo-name branch-name))
           (buffer-name (claude--buffer-name repo-name branch-name))
           (metadata (claude-metadata-read repo-name branch-name))
           (status (plist-get metadata :status)))
      ;; Skip workspaces in transient states (creating, closing)
      ;; No point detecting attention during cleanup
      (when (equal status "active")
        ;; Only check if buffer exists and is idle
        (when (and (get-buffer buffer-name)
                   (claude-monitor--idle-p buffer-name))
          (let ((needs-attention (claude-monitor--check-attention buffer-name)))
            (claude-monitor--set-attention workspace-name needs-attention)))))))

(defun claude-monitor-start ()
  "Start the monitor timer."
  (interactive)
  (claude-monitor-stop)  ; Ensure no duplicate timers
  (setq claude-monitor--timer
        (run-with-timer claude-monitor-interval
                        claude-monitor-interval
                        #'claude-monitor--tick))
  (message "Claude monitor started"))

(defun claude-monitor-stop ()
  "Stop the monitor timer."
  (interactive)
  (when claude-monitor--timer
    (cancel-timer claude-monitor--timer)
    (setq claude-monitor--timer nil)
    (message "Claude monitor stopped")))

(defun claude-monitor-toggle ()
  "Toggle monitor on/off."
  (interactive)
  (if claude-monitor--timer
      (claude-monitor-stop)
    (claude-monitor-start)))

(defun claude-monitor-running-p ()
  "Return t if monitor is running."
  (and claude-monitor--timer t))
```

## Auto-Clear on Focus

When user switches to a workspace, clear its attention state:

```elisp
(defun claude-monitor--on-workspace-switch ()
  "Clear attention when switching to a Claude workspace."
  (when-let ((ws-name (+workspace-current-name)))
    (when (string-match "^\\([^:]+\\):\\(.+\\)$" ws-name)
      (claude-monitor--clear-attention ws-name))))

(add-hook '+workspace-switch-hook #'claude-monitor--on-workspace-switch)
```

## Attention After Repair

When reconciliation repairs a vterm buffer (restarts Claude session):

1. The session context is lost (Claude starts fresh)
2. Attention state is cleared
3. User is notified that session was restarted

```elisp
(defun claude--repair-vterm-buffer (metadata)
  "Recreate vterm buffer and start Claude session.
Note: This loses the previous session context."
  (let* ((repo-name (plist-get metadata :repo_name))
         (branch-name (plist-get metadata :branch_name))
         (workspace-name (claude--workspace-name repo-name branch-name))
         (buffer-name (claude--buffer-name repo-name branch-name))
         (dir (or (plist-get metadata :worktree_path)
                  (plist-get metadata :parent_repo))))
    ;; Clear old attention state
    (claude-monitor--clear-attention workspace-name)

    ;; Reset activity tracking
    (remhash buffer-name claude-monitor--last-activity)

    ;; Create new buffer
    (claude--create-vterm-in-dir buffer-name dir)
    (claude--send-command buffer-name "claude")

    ;; Notify user
    (message "Claude session restarted in %s (previous context lost)" workspace-name)))
```

## Workspaces Needing Attention

Query functions for UI:

```elisp
(defun claude--workspaces-needing-attention ()
  "Return list of workspace names that need attention."
  (let ((result nil))
    (maphash (lambda (name needs-attention)
               (when needs-attention
                 (push name result)))
             claude-monitor--attention-state)
    (nreverse result)))

(defun claude--any-needs-attention-p ()
  "Return t if any workspace needs attention."
  (let ((found nil))
    (maphash (lambda (_name needs-attention)
               (when needs-attention (setq found t)))
             claude-monitor--attention-state)
    found))
```

## Startup Behavior

Monitor starts automatically on Doom init if workspaces exist:

```elisp
(defun claude-monitor--maybe-start ()
  "Start monitor if Claude workspaces exist."
  (when (claude--list-all-workspaces)
    (claude-monitor-start)))

(add-hook 'doom-after-init-hook #'claude-monitor--maybe-start)
```

## Performance Considerations

- Timer runs every 2 seconds (configurable)
- Only checks idle buffers (avoids scanning during active output)
- Only scans last 50 lines (not entire buffer)
- Pattern matching uses short-circuit evaluation
- Hash table lookups are O(1)

For a typical setup with 3-5 workspaces, this adds negligible overhead.
