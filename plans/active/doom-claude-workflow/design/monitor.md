# Attention Detection System

## Overview

A timer-based system that monitors all Claude buffers and updates the modeline when any Claude needs user input.

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
  '("Allow .* \\[y/n\\]"
    "\\[Y/n\\]"
    "\\[y/N\\]"
    "(y/n)"
    "Do you want to"
    "Would you like to"
    "Proceed\\?"
    "Continue\\?"
    "Overwrite\\?"
    "^> *$"           ;; Empty prompt
    "^\\? *$")        ;; Question prompt
  "Patterns indicating Claude needs attention.")
```

Patterns are case-insensitive and matched against each of the last 15 lines.

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

```elisp
(defun claude-modeline-segment ()
  "Return modeline string for Claude status."
  (when (claude--any-needs-attention-p)
    (propertize " Claude" 'face 'claude-attention-face)))
```

Display states:
- Any buffer needs attention: `Claude` with red indicator
- All buffers working/idle: segment hidden (or plain `Claude`)

Clicking the modeline segment jumps to the first buffer needing attention.

## Auto-management

- **Auto-start**: Monitor starts when first Claude workspace is created
- **Auto-stop**: Monitor stops when last Claude workspace is closed
- **Manual toggle**: `SPC C m` to start/stop manually
