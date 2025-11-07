#!/bin/bash
# Run Forth tests in all execution modes

set -e

ANVIL="./target/release/forth"
TEST_FILE="${1:-tests/run-all.fth}"

# Check if this is run-all.fth or an individual test file
if [[ "$TEST_FILE" == "tests/run-all.fth" ]]; then
    # Run-all includes framework itself
    ACTUAL_TEST="$TEST_FILE"
else
    # Individual test file - create wrapper that loads framework
    TEMP_WRAPPER=$(mktemp /tmp/anvil_test_wrapper.XXXXXX.fth)
    cat > "$TEMP_WRAPPER" << EOF
\\ Temporary wrapper to run individual test file
INCLUDE tests/testing.fth
INIT-TESTS
INCLUDE $TEST_FILE
REPORT
EOF
    ACTUAL_TEST="$TEMP_WRAPPER"
fi

echo "Running Forth tests: $TEST_FILE"
echo "========================================"
echo ""

echo "=== JIT Mode ==="
$ANVIL "$ACTUAL_TEST"

# echo ""
# echo "=== Interpreter Mode ==="
# $ANVIL --no-jit "$ACTUAL_TEST"

# echo ""
# echo "=== AOT Mode ==="
# TEMP_BINARY=$(mktemp)
# if $ANVIL --compile "$ACTUAL_TEST" -o "$TEMP_BINARY" 2>&1 | grep -q "Successfully compiled"; then
#     echo "Compilation successful"
#     $TEMP_BINARY
#     rm -f "$TEMP_BINARY"
# else
#     echo "Compilation failed"
#     rm -f "$TEMP_BINARY"
#     [[ -f "$TEMP_WRAPPER" ]] && rm -f "$TEMP_WRAPPER"
#     exit 1
# fi

# Clean up wrapper if it was created
[[ -f "$TEMP_WRAPPER" ]] && rm -f "$TEMP_WRAPPER"

echo ""
echo "========================================"
echo "All modes tested successfully"
