#!/bin/bash


# Ensure we run from the project root
cd "$(dirname "$0")/.." || exit 1

BINARY="./glados"
RET=0
GREEN='\033[0;32m'
RED='\033[0;31m'
BLUE='\033[0;34m'
NC='\033[0m'

# --- SETUP & TEARDOWN ---

setup() {
    echo -e "${BLUE}==> Building project...${NC}"
    make > /dev/null
    if [ $? -ne 0 ]; then
        echo -e "${RED}Build failed!${NC}"
        exit 1
    fi
    echo -e "${GREEN}Build successful.${NC}\n"
}

cleanup() {
    echo -e "\n${BLUE}==> Cleaning up...${NC}"
    make fclean > /dev/null
    rm -f /tmp/out /tmp/err
    echo -e "${GREEN}Cleanup done.${NC}"
}

trap cleanup EXIT

# --- ASSERTIONS ---

assert_output() {
    TEST_NAME=$1
    INPUT=$2
    EXPECTED_RET=$3
    EXPECTED_OUT=$4

    echo "$INPUT" > /tmp/test_input
    $BINARY < /tmp/test_input > /tmp/out 2> /tmp/err
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
            echo -e "Expected: '$EXPECTED_OUT'"
            echo -e "Got:      '$TRIMMED'"
            RET=1
            return
        fi
    fi
    echo -e "${GREEN}[PASS] $TEST_NAME${NC}"
}

assert_file() {
    TEST_NAME=$1
    FILE=$2
    EXPECTED_RET=$3
    EXPECTED_OUT=$4

    $BINARY < "$FILE" > /tmp/out 2> /tmp/err
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
            echo -e "Expected: '$EXPECTED_OUT'"
            echo -e "Got:      '$TRIMMED'"
            RET=1
            return
        fi
    fi
    echo -e "${GREEN}[PASS] $TEST_NAME${NC}"
}

# --- TEST SUITES ---

run_basic_tests() {
    echo -e "${BLUE}--- Basic Tests ---${NC}"
    assert_output "Addition" "(+ 2 3)" 0 "5"
    assert_output "Syntax Error" "(+ 2" 84 ""
    assert_output "Unknown Var" "(+ a 1)" 84 ""
}

run_lambda_tests() {
    echo -e "\n${BLUE}--- Lambda Tests ---${NC}"
    assert_file "Lambda Definition" "test/functional/lambdas/lambda1.scm" 0 "#<procedure>"
    assert_file "Lambda Application" "test/functional/lambdas/lambda2.scm" 0 "3"
    assert_file "Lambda Define & Call" "test/functional/lambdas/lambda3.scm" 0 "7"
}

run_function_tests() {
    echo -e "\n${BLUE}--- Function Tests ---${NC}"
    assert_file "Function Definition" "test/functional/function/function1.scm" 0 "7"
}

run_conditionnal_tests() {
    echo -e "\n${BLUE}--- Conditionnal Tests ---${NC}"
    assert_file "If True" "test/functional/conditionnal/if1.scm" 0 "1"
    assert_file "If False" "test/functional/conditionnal/if2.scm" 0 "2"
    assert_file "If Else" "test/functional/conditionnal/if3.scm" 0 "21"
}

run_builtin_tests() {
    echo -e "\n${BLUE}--- Builtin Tests ---${NC}"
    assert_file "Addition" "test/functional/builtin/builtins1.scm" 0 "11"
    assert_file "Equality" "test/functional/builtin/builtins2.scm" 0 "#t"
    assert_file "Less Than" "test/functional/builtin/builtins3.scm" 0 "#f"
}

run_general_tests() {
    echo -e "\n${BLUE}--- General Tests ---${NC}"
    assert_file "Factorial" "test/functional/general/factorial.scm" 0 "#t"
    assert_file "Superior" "test/functional/general/superior.scm" 0 "3628800"
}

# --- MAIN ---

setup
run_basic_tests


if [ $RET -eq 0 ]; then
    echo -e "\n${GREEN}All tests passed!${NC}"
else
    echo -e "\n${RED}Some tests failed.${NC}"
fi

exit $RET