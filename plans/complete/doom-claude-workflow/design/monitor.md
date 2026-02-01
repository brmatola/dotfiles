# Attention Detection System

## Overview

A timer-based system that monitors all Claude buffers and updates the modeline when any Claude needs user input.

## Design Decisions

1. **Single attention state** - Just "needs attention" or not. No distinction between permission prompts, questions, etc.
2. **Always-visible modeline** - The Claude indicator stays visible even in non-Claude workspaces so you always know when to switch back
3. **Idle timer approach** - Use Emacs `run-with-idle-timer` for polling since vterm has no built-in idle detection

## Detection Strategy

### Two-phase check

1. **Staleness check** - Buffer must have no new output for 3+ seconds
2. **Pattern match** - Last 15 lines must contain an attention pattern

Both conditions must be true to trigger an alert. This prevents false positives from:
- Patterns appearing in code output (staleness prevents during active streaming)
- Old prompts in scroll history (only check last 15 lines)

### Attention Patterns

```elisp
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
  "Patterns indicating Claude needs attention.")
```

Patterns are case-insensitive and matched against each of the last 15 lines.

### Alternative: Claude Code Hooks (Future Enhancement)

Claude Code has a built-in Notification hook system that fires on `permission_prompt` and `elicitation_dialog` events. A more robust future approach:

1. Configure hook in `~/.claude/settings.json` to write attention events to a file
2. Have Emacs monitor that file with `filenotify`
3. Update attention state based on hook events

This eliminates regex fragility but adds external dependency. Starting with pattern matching for simplicity.

## State Tracking

Each Claude buffer tracks:

```elisp
;; Buffer-local variables
(defvar-local claude--last-output-time nil
  "Timestamp of last detected output change.")

(defvar-local claude--last-content-hash nil
  "Hash of recent buffer content to detect changes.")

(defvar-local claude--needs-attention nil
  "Non-nil if this buffer needs user attention.")
```

## Timer Operation

```elisp
(defvar claude-monitor-timer nil
  "Timer for attention monitoring.")

(defcustom claude-monitor-interval 2
  "Seconds between attention checks.")
```

### Check cycle (every 2 seconds):

1. Get list of all `*claude:*` buffers
2. For each buffer:
   - Hash last 15 lines of content
   - If hash changed from last check → update `last-output-time`, clear `needs-attention`
   - If hash unchanged AND `last-output-time` > 3s ago → run pattern match
   - If pattern matches → set `needs-attention`
3. Update modeline segment

## Modeline Integration

### Custom Segment Definition

```elisp
(doom-modeline-def-segment claude-status
  "Display Claude session status."
  (let* ((workspaces (claude-workspace-list))
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
                          (if needs-attention " (attention needed)" "")))))))
```

### Global Modeline Setup

Must define a custom modeline that includes our segment:

```elisp
(after! doom-modeline
  ;; Define custom modeline with claude-status
  (doom-modeline-def-modeline 'claude-main
    '(eldoc bar workspace-name window-number modals matches buffer-info remote-host buffer-position parrot selection-info)
    '(misc-info minor-modes input-method buffer-encoding major-mode process vcs check claude-status))

  ;; Set as default
  (doom-modeline-set-main-modeline 'claude-main t))
```

### Display States

| State | Display | Meaning |
|-------|---------|---------|
| No workspaces | Hidden | No Claude sessions active |
| All working/idle | `○ Claude` (dim) | Sessions active, none need attention |
| Any needs attention | `● Claude` (red) | At least one session waiting for input |

Clicking the modeline segment jumps to the first buffer needing attention.

## Auto-management

- **Auto-start**: Monitor starts when first Claude workspace is created
- **Auto-stop**: Monitor stops when last Claude workspace is closed
- **Manual toggle**: `SPC C m` to start/stop manually
