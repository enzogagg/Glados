#!/bin/bash

BINARY="./glados"
RET=0
GREEN='\033[0;32m'
RED='\033[0;31m'
NC='\033[0m'

assert_output() {
    TEST_NAME=$1
    INPUT=$2
    EXPECTED_RET=$3
    EXPECTED_OUT=$4

    echo "$INPUT" | $BINARY > /tmp/out 2> /tmp/err
    ACTUAL_RET=$?

    if [ $ACTUAL_RET -ne $EXPECTED_RET ]; then
        echo -e "${RED}[FAIL] $TEST_NAME: Return $ACTUAL_RET, expected $EXPECTED_RET${NC}"
        RET=1
        return
    fi

    if [ ! -z "$EXPECTED_OUT" ]; then
        TRIMMED=$(cat /tmp/out | xargs)
        if [ "$TRIMMED" != "$EXPECTED_OUT" ]; then
            echo -e "${RED}[FAIL] $TEST_NAME: Output mismatch.${NC}"
            RET=1
            return
        fi
    fi
    echo -e "${GREEN}[PASS] $TEST_NAME${NC}"
}

# --- TESTS ---
assert_output "Addition" "(+ 2 3)" 0 "5"
assert_output "Syntax Error" "(+ 2" 84 ""
assert_output "Unknown Var" "(+ a 1)" 84 ""

rm -f /tmp/out /tmp/err
exit $RET