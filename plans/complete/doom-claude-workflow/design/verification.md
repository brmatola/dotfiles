# Verification Findings

Technical validation performed 2026-02-01 before implementation.

## 1. vterm Buffer Reading - CONFIRMED

**Finding:** `vterm--filter-buffer-substring` exists and works as documented.

**Correct approach for reading last N lines:**
```elisp
(defun claude--get-last-n-lines (buffer n)
  "Get last N lines from vterm BUFFER, filtering fake newlines."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-max))
      (forward-line (- n))
      (let ((text (buffer-substring (point) (point-max))))
        (vterm--filter-buffer-substring text)))))
```

**Buffer-local variables:** Survive persp-mode burial/restoration. Emacs buffer-local bindings are intrinsic to the buffer object.

## 2. Attention Detection - MAJOR FINDING

**Claude Code has a hooks system** that fires on notification events:

| notification_type | When it fires |
|-------------------|---------------|
| `permission_prompt` | User approval needed for tool use |
| `idle_prompt` | Claude finished responding (60s idle) |
| `elicitation_dialog` | Claude asking user for choices |

**Two approaches available:**

### Option A: Use Claude Code's Notification Hook (Recommended)
Configure a Notification hook in `~/.claude/settings.json` that writes to a file when Claude needs attention. Emacs monitors this file.

```json
{
  "hooks": {
    "Notification": [
      {
        "matcher": "permission_prompt|elicitation_dialog",
        "hooks": [{
          "type": "command",
          "command": "echo '{\"session_id\": \"$SESSION_ID\", \"type\": \"$NOTIFICATION_TYPE\"}' >> /tmp/claude-attention.jsonl"
        }]
      }
    ]
  }
}
```

**Pros:** Reliable, uses Claude's own detection, no regex fragility
**Cons:** Requires hook configuration, file monitoring adds complexity

### Option B: vterm Pattern Matching (Simpler, current plan)
Keep the regex approach but with updated patterns based on actual Claude output.

**Updated patterns for Claude Code CLI:**
```elisp
(defcustom claude-attention-patterns
  '(;; Permission prompts
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
    ;; MCP permission
    "needs your permission")
  "Patterns indicating Claude needs attention.")
```

**Pros:** Self-contained, no external file dependency
**Cons:** May miss new prompt types, regex can be fragile

**Decision:** Start with Option B (simpler), document Option A as future enhancement.

## 3. Doom Module Loading - MUST CHANGE

**Finding:** Plan's approach is incorrect.

**Wrong (current plan):**
```elisp
(add-to-list 'load-path (expand-file-name "modules/claude" doom-user-dir))
(require 'claude)
```

**Correct (Doom idiomatic):**
```elisp
;; In config.el
(add-load-path! "modules/claude")

;; Load after doom-modeline is available
(after! doom-modeline
  (load! "modules/claude/claude"))
```

**Key points:**
- Use `load!` not `require` for Doom modules
- Use `add-load-path!` not `add-to-list`
- Wrap modeline-dependent code in `after! doom-modeline`

## 4. Git Worktree Errors - DOCUMENTED

**Tested error scenarios:**

| Scenario | Exit Code | Error Message |
|----------|-----------|---------------|
| Branch exists | 255 | `fatal: a branch named 'X' already exists` |
| Non-empty dir exists | 128 | `fatal: '/path' already exists` |
| Empty dir exists | 0 | Success (uses existing dir) |
| Dirty working tree | 0 | Success (doesn't affect worktree creation) |

**Error handling strategy:**
```elisp
(defun claude-worktree-create (repo-path branch-name parent-branch)
  "Create worktree, returning (success . path-or-error)."
  (let* ((repo-name (claude-repo-name repo-path))
         (worktree-path (claude-worktree-path repo-name branch-name))
         (result (shell-command-to-string
                  (format "cd %s && git worktree add -b %s %s 2>&1"
                          (shell-quote-argument repo-path)
                          (shell-quote-argument branch-name)
                          (shell-quote-argument worktree-path)))))
    (cond
     ((string-match "already exists" result)
      (cons nil (if (string-match "branch" result)
                    'branch-exists
                  'dir-exists)))
     ((= 0 (process-exit-status))
      (cons t worktree-path))
     (t
      (cons nil result)))))
```

## 5. Concurrent Merge Risk - LOW

**Question:** What if two Claude sessions merge to same parent simultaneously?

**Finding:** Git handles this safely:
- First merge succeeds
- Second merge either: fast-forwards (if no conflicts) or requires manual resolution
- No data loss risk

**Mitigation:** Show clear error message if merge fails, suggest refreshing and retrying.

## 6. Performance at Scale - ACCEPTABLE

**Question:** Will 2-second polling across 10+ buffers cause lag?

**Analysis:**
- Each check: hash 15 lines of text (~1KB), compare hash
- Hashing 1KB: <1ms
- 10 buffers: <10ms per poll cycle
- 2-second interval: <0.5% CPU overhead

**Verdict:** Acceptable. Add escape hatch: `claude-monitor-max-buffers` to limit if needed.

## Summary: Changes Required

1. **monitor.md:** Update attention patterns, document hook alternative
2. **file-structure.md:** Fix module loading to use `load!` and `after!`
3. **flows.md:** Add git error handling with specific exit codes
4. **tasks.md:** Add Phase 0 spike task for vterm testing, update task details
