#!/bin/bash
# Lint Claude module elisp files
# Usage: ./test/lint.sh [--fix]
#
# Checks:
# - Byte compilation warnings (excluding expected runtime-only warnings)
# - Lexical binding header
# - Provides statement matches filename

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
MODULE_DIR="$(dirname "$SCRIPT_DIR")"

cd "$MODULE_DIR"

echo "=== Claude Module Lint ==="
echo ""

ERRORS=0

# Files to lint (excluding test files for byte-compile)
FILES=(
    "claude-state.el"
    "claude-vterm.el"
    "claude-reconcile.el"
    "claude-worktree.el"
    "claude-workspace.el"
    "claude-monitor.el"
    "claude-dashboard.el"
    "claude-cleanup.el"
    "claude.el"
)

# Check lexical-binding header
echo "Checking lexical-binding headers..."
for f in "${FILES[@]}"; do
    if ! head -1 "$f" | grep -q "lexical-binding: t"; then
        echo "  ERROR: $f missing lexical-binding header"
        ERRORS=$((ERRORS + 1))
    fi
done
echo ""

# Check provides statement
echo "Checking provides statements..."
for f in "${FILES[@]}"; do
    base=$(basename "$f" .el)
    if ! grep -q "(provide '$base)" "$f"; then
        echo "  ERROR: $f missing or incorrect (provide '$base)"
        ERRORS=$((ERRORS + 1))
    fi
done
echo ""

# Byte-compile check (standalone files only)
# Note: Some files require vterm/Doom at runtime - we check what we can
echo "Checking byte compilation..."

# These files can be byte-compiled standalone
STANDALONE_FILES=(
    "claude-state.el"
    "claude-vterm.el"
)

for f in "${STANDALONE_FILES[@]}"; do
    echo "  $f"
    output=$(emacs --batch -Q \
        --eval "(setq byte-compile-error-on-warn t)" \
        --eval "(setq load-path (cons \".\" load-path))" \
        --eval "(byte-compile-file \"$f\")" 2>&1 || true)

    # Filter out expected warnings (runtime dependencies)
    filtered=$(echo "$output" | grep -E "Error:" | grep -v "not known to be defined" || true)

    if [[ -n "$filtered" ]]; then
        echo "$filtered"
        ERRORS=$((ERRORS + 1))
    fi
done

# Files that need claude-state loaded first
DEPENDENT_FILES=(
    "claude-reconcile.el"
    "claude-worktree.el"
)

for f in "${DEPENDENT_FILES[@]}"; do
    echo "  $f (with dependencies)"
    output=$(emacs --batch -Q \
        --eval "(setq byte-compile-error-on-warn t)" \
        --eval "(setq load-path (cons \".\" load-path))" \
        -l claude-state.el \
        --eval "(byte-compile-file \"$f\")" 2>&1 || true)

    # Filter expected runtime-only warnings
    filtered=$(echo "$output" | grep -E "Error:" | \
        grep -v "not known to be defined" | \
        grep -v "reference to free variable" || true)

    if [[ -n "$filtered" ]]; then
        echo "$filtered"
        ERRORS=$((ERRORS + 1))
    fi
done

echo ""

# Clean up .elc files
rm -f *.elc

echo "=== Results ==="
if [[ $ERRORS -eq 0 ]]; then
    echo "No lint errors found!"
    exit 0
else
    echo "$ERRORS lint error(s) found"
    exit 1
fi
