#!/bin/bash
# Run Claude module tests in batch mode

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
MODULE_DIR="$(dirname "$SCRIPT_DIR")"
DOOM_DIR="$HOME/.config/emacs"
DOOM_LOCAL="$DOOM_DIR/.local"

echo "Running Claude module tests..."
echo ""

# Find emacs binary - prefer Doom's if available
if [ -x "$DOOM_LOCAL/emacs/bin/emacs" ]; then
    EMACS="$DOOM_LOCAL/emacs/bin/emacs"
elif command -v emacs &> /dev/null; then
    EMACS="emacs"
else
    echo "Error: Emacs not found"
    exit 1
fi

# Run with minimal init - just load what we need
$EMACS --batch \
    --eval "(setq claude-worktree-dir \"/tmp/test-worktrees\")" \
    --eval "(setq claude-metadata-dir \"/tmp/test-metadata\")" \
    -L "$MODULE_DIR" \
    -L "$DOOM_LOCAL/straight/build/vterm" \
    -L "$DOOM_LOCAL/straight/build/json" \
    --eval "(require 'json)" \
    --eval "(provide 'vterm)" \
    --eval "(defun vterm-mode () nil)" \
    --eval "(defun vterm-send-string (s) nil)" \
    --eval "(defun vterm-reset-cursor-point () nil)" \
    --eval "(provide 'projectile)" \
    --eval "(defun projectile-project-root () nil)" \
    -l ert \
    -l claude-worktree \
    -l "$SCRIPT_DIR/claude-test.el" \
    -f ert-run-tests-batch-and-exit 2>&1

echo ""
echo "Tests complete!"
