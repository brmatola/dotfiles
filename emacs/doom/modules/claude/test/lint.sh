#!/bin/bash
# Lint Claude module elisp files
# Usage: ./test/lint.sh

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
MODULE_DIR="$(dirname "$SCRIPT_DIR")"

cd "$MODULE_DIR"

echo "=== Claude Module Lint ==="
echo ""

ERRORS=0

# Files to lint
FILES=(
    "claude-grove.el"
    "claude-monitor.el"
    "claude-dashboard.el"
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

# Byte-compile check
echo "Checking byte compilation..."

# Standalone files (no deps)
STANDALONE_FILES=(
    "claude-grove.el"
    "claude-monitor.el"
)

for f in "${STANDALONE_FILES[@]}"; do
    echo "  $f"
    output=$(emacs --batch -Q \
        --eval "(setq byte-compile-error-on-warn t)" \
        --eval "(setq load-path (cons \".\" load-path))" \
        --eval "(byte-compile-file \"$f\")" 2>&1 || true)

    filtered=$(echo "$output" | grep -E "Error:" | grep -v "not known to be defined" || true)

    if [[ -n "$filtered" ]]; then
        echo "$filtered"
        ERRORS=$((ERRORS + 1))
    fi
done

# Files that need dependencies
echo "  claude-dashboard.el (with dependencies)"
output=$(emacs --batch -Q \
    --eval "(setq byte-compile-error-on-warn t)" \
    --eval "(setq load-path (cons \".\" load-path))" \
    -l claude-grove.el \
    -l claude-monitor.el \
    --eval "(byte-compile-file \"claude-dashboard.el\")" 2>&1 || true)

filtered=$(echo "$output" | grep -E "Error:" | \
    grep -v "not known to be defined" | \
    grep -v "reference to free variable" || true)

if [[ -n "$filtered" ]]; then
    echo "$filtered"
    ERRORS=$((ERRORS + 1))
fi

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
