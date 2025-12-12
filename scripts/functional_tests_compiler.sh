#!/bin/bash


# Ensure we run from the project root
cd "$(dirname "$0")/.." || exit 1

BINARY="./glados-compiler"
RET=0
GREEN='\033[0;32m'
RED='\033[0;31m'
BLUE='\033[0;34m'
NC='\033[0m'

# --- SETUP & TEARDOWN ---

setup() {
    echo -e "${BLUE}==> Building project...${NC}"
    make compiler > /dev/null
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
    assert_file "Lambda Definition" "test/compiler/functional/lambdas/lambda1.scm" 0 "#<procedure>"
    assert_file "Lambda Application" "test/compiler/functional/lambdas/lambda2.scm" 0 "3"
    assert_file "Lambda Define & Call" "test/compiler/functional/lambdas/lambda3.scm" 0 "7"
}

run_list_tests() {
    echo -e "\n${BLUE}--- List Tests ---${NC}"
    assert_file "List1" "test/compiler/functional/lists/list1.scm" 0 "(1 2 3 4 5)"
    assert_file "List2" "test/compiler/functional/lists/list2.scm" 0 "1"
}

run_function_tests() {
    echo -e "\n${BLUE}--- Function Tests ---${NC}"
    assert_file "Function Definition" "test/compiler/functional/function/function1.scm" 0 "7"
}

run_conditional_tests() {
    echo -e "\n${BLUE}--- Conditional Tests ---${NC}"
    assert_file "If True" "test/compiler/functional/conditional/if1.scm" 0 "1"
    assert_file "If False" "test/compiler/functional/conditional/if2.scm" 0 "2"
    assert_file "If Else" "test/compiler/functional/conditional/if3.scm" 0 "21"
}

run_builtin_tests() {
    echo -e "\n${BLUE}--- Builtin Tests ---${NC}"
    assert_file "Addition" "test/compiler/functional/builtin/builtins1.scm" 0 "11"
    assert_file "Equality" "test/compiler/functional/builtin/builtins2.scm" 0 "#t"
    assert_file "Less Than" "test/compiler/functional/builtin/builtins3.scm" 0 "#f"
}

run_general_tests() {
    echo -e "\n${BLUE}--- General Tests ---${NC}"
    assert_file "Factorial" "test/compiler/functional/general/factorial.scm" 0 "3628800"
    assert_file "Superior" "test/compiler/functional/general/superior.scm" 0 "#t"
}

run_float_tests() {
    echo -e "\n${BLUE}--- Float Tests ---${NC}"
    assert_file "Float1" "test/compiler/functional/float/float1.scm" 0 "2.0"
    assert_file "Float2" "test/compiler/functional/float/float2.scm" 0 "0.5"
    assert_file "Float3" "test/compiler/functional/float/float3.scm" 0 "0.3333333333333333"
}

run_string_tests() {
    echo -e "\n${BLUE}--- String Tests ---${NC}"
    assert_file "String1" "test/compiler/functional/string/string1.scm" 0 "Hello, World!"
    assert_file "String2" "test/compiler/functional/string/string2.scm" 0 "This is a test"
    assert_file "String3" "test/compiler/functional/string/string3.scm" 0 "Hello, Glados is the best project ever!"
}

run_tco_tests() {
    echo -e "\n${BLUE}--- TCO Tests ---${NC}"
    assert_file "TCO1" "test/compiler/functional/tco/tco1.scm" 0 "0"
}

# --- MAIN ---

setup
run_basic_tests
run_lambda_tests
run_list_tests
run_function_tests
run_conditional_tests
run_builtin_tests
run_general_tests
run_float_tests
run_string_tests
run_tco_tests

if [ $RET -eq 0 ]; then
    echo -e "\n${GREEN}All tests passed!${NC}"
else
    echo -e "\n${RED}Some tests failed.${NC}"
fi

exit $RET