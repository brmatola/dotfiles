#!/bin/bash
# Run all Claude module tests
# Usage: ./test/run-tests.sh

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

# Grove wrapper + monitor tests
run_test_file "claude-grove-test" "-l claude-grove.el -l claude-monitor.el" "test/claude-grove-test.el"

# Dashboard rendering tests
run_test_file "claude-dashboard-test" "-l claude-grove.el -l claude-monitor.el -l claude-dashboard.el" "test/claude-dashboard-test.el"

echo "=== Results ==="
echo "$PASSED/$TOTAL test suites passed"

if [[ $PASSED -eq $TOTAL ]]; then
    echo "All tests passed!"
    exit 0
else
    echo "Some tests failed"
    exit 1
fi
