#!/bin/bash
# Run all Claude module tests
# Usage: ./test/run-tests.sh [--verbose]

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
MODULE_DIR="$(dirname "$SCRIPT_DIR")"

cd "$MODULE_DIR"

echo "=== Claude Module Tests ==="
echo ""

# Track results
TOTAL=0
PASSED=0

run_test_file() {
    local name="$1"
    local deps="$2"
    local test_file="$3"

    echo "Running: $name"
    TOTAL=$((TOTAL + 1))

    if emacs --batch \
        -l ert \
        $deps \
        -l "$test_file" \
        -f ert-run-tests-batch-and-exit 2>&1; then
        PASSED=$((PASSED + 1))
    else
        echo "  FAILED"
    fi
    echo ""
}

# Unit tests - claude-state
run_test_file "claude-state-test" "-l claude-state.el" "test/claude-state-test.el"

# Unit tests - claude-reconcile (needs vterm stubs)
run_test_file "claude-reconcile-test" "-l claude-state.el -l claude-vterm.el -l claude-reconcile.el" "test/claude-reconcile-test.el"

# Integration tests
run_test_file "claude-test" "-l claude-state.el -l claude-worktree.el" "test/claude-test.el"

echo "=== Results ==="
echo "$PASSED/$TOTAL test suites passed"

if [[ $PASSED -eq $TOTAL ]]; then
    echo "All tests passed!"
    exit 0
else
    echo "Some tests failed"
    exit 1
fi
