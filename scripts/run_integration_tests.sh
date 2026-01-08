#!/bin/bash

# Configuration
TEST_DIR="test/integration/suites"
COMPILER="./glados-compiler"
VM="./glados-vm"
TEMP_CBC="/tmp/test_out.cbc"
TEMP_OUT="/tmp/test_out.txt"

# Colors
GREEN='\033[0;32m'
RED='\033[0;31m'
BLUE='\033[0;34m'
NC='\033[0m'

echo -e "${BLUE}=== Starting Integration Tests ===${NC}"

# 1. Build project
echo -e "${BLUE}[SETUP] Building project...${NC}"
make > /dev/null
if [ $? -ne 0 ]; then
    echo -e "${RED}[ERROR] Build failed.${NC}"
    exit 1
fi

TOTAL=0
PASSED=0
FAILED=0

# 2. Find and Run tests
TEST_FILES=$(find "$TEST_DIR" -name "*.clad" | sort)

for src_file in $TEST_FILES; do
    ((TOTAL++))
    test_name=$(basename "$src_file" .clad)
    expected_file="${src_file%.clad}.out"
    
    echo -n "Running $test_name... "

    # Check expectations exist
    if [ ! -f "$expected_file" ]; then
        echo -e "${RED}[SKIP] No .out file found for $test_name${NC}"
        continue
    fi

    # Compile
    $COMPILER "$src_file" > /dev/null 2>&1
    # Note: Assuming compiler outputs to 'out.cbc' by default or we need to flag it. 
    # If compiler doesn't support -o, we rename the output.
    # Let's assume default is 'out.cbc' based on previous context.
    
    if [ ! -f "out.cbc" ]; then
         echo -e "${RED}[FAIL] Compilation failed${NC}"
         ((FAILED++))
         continue
    fi
    mv out.cbc "$TEMP_CBC"

    # Execute VM
    $VM "$TEMP_CBC" > "$TEMP_OUT" 2>&1
    vm_ret=$?

    # Compare Output
    diff_out=$(diff -u --strip-trailing-cr "$expected_file" "$TEMP_OUT")
    
    if [ $? -eq 0 ]; then
        echo -e "${GREEN}[PASS]${NC}"
        ((PASSED++))
    else
        echo -e "${RED}[FAIL] Output mismatch${NC}"
        echo "$diff_out"
        ((FAILED++))
    fi
    
    # Cleanup loop
    rm -f "$TEMP_CBC" "$TEMP_OUT"
done

# 3. Summary
echo -e "\n${BLUE}=== Summary ===${NC}"
echo -e "Total:  $TOTAL"
echo -e "Passed: ${GREEN}$PASSED${NC}"
echo -e "Failed: ${RED}$FAILED${NC}"

if [ $FAILED -eq 0 ]; then
    exit 0
else
    exit 1
fi
