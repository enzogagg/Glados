#!/bin/bash

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

COMPILER="$SCRIPT_DIR/../../../../glados-compiler"
VM="$SCRIPT_DIR/../../../../glados-vm"
TEST_FILE="$SCRIPT_DIR/../simple/afficher.clad"
OUTPUT_FILE="afficher.cbc"

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m'

TESTS_PASSED=0
TESTS_FAILED=0

print_result() {
    local test_name="$1"
    local expected_exit="$2"
    local actual_output="$3"
    local actual_exit="$4"

    if [ "$actual_exit" -eq "$expected_exit" ]; then
        echo -e "${GREEN}✓${NC} $test_name"
        ((TESTS_PASSED++))
    else
        echo -e "${RED}✗${NC} $test_name"
        echo -e "  ${YELLOW}Attendu:${NC} exit code=$expected_exit"
        echo -e "  ${YELLOW}Obtenu:${NC}  exit code=$actual_exit"
        echo -e "  ${YELLOW}Sortie:${NC}  '$actual_output'"
        ((TESTS_FAILED++))
    fi
}

echo "=========================================="
echo "  Tests fonctionnels CLaD - Afficher"
echo "=========================================="
echo ""

for bin in "$COMPILER" "$VM"; do
    if [ ! -f "$bin" ]; then
        echo -e "${RED}Erreur:${NC} Binaire introuvable : $bin"
        exit 1
    fi
    if [ ! -x "$bin" ]; then
        chmod +x "$bin"
    fi
done

if [ ! -f "$TEST_FILE" ]; then
    echo -e "${RED}Erreur:${NC} Fichier de test introuvable : $TEST_FILE"
    exit 1
fi

echo "Test 1: Compilation du programme"
echo "---------------------------------"

compile_output=$("$COMPILER" -o "$OUTPUT_FILE" "$TEST_FILE" 2>&1)
compile_exit=$?

if [ $compile_exit -eq 0 ]; then
    echo -e "${GREEN}✓${NC} Compilation réussie"
    ((TESTS_PASSED++))
else
    echo -e "${RED}✗${NC} Échec de la compilation"
    echo "$compile_output"
    ((TESTS_FAILED++))
    exit 1
fi

echo ""
echo "Test 2: Exécution du bytecode"
echo "------------------------------"

vm_output=$("$VM" "$OUTPUT_FILE" 2>&1)
vm_exit=$?

print_result "Exécution sans erreur" 0 "$vm_output" "$vm_exit"

echo ""
echo "Test 3: Vérification de l'affichage (42)"
echo "-----------------------------------------"

if echo "$vm_output" | grep -Eq '(^|[^0-9])42([^0-9]|$)'; then
    echo -e "${GREEN}✓${NC} Le programme affiche bien 42"
    echo -e "  ${YELLOW}Sortie:${NC} $vm_output"
    ((TESTS_PASSED++))
else
    echo -e "${RED}✗${NC} Le programme devrait afficher 42"
    echo -e "  ${YELLOW}Sortie:${NC} $vm_output"
    ((TESTS_FAILED++))
fi

echo ""
echo "Nettoyage..."
rm -f "$OUTPUT_FILE"

echo ""
echo "=========================================="
echo "  Résumé des tests"
echo "=========================================="
echo -e "Tests réussis: ${GREEN}$TESTS_PASSED${NC}"
echo -e "Tests échoués: ${RED}$TESTS_FAILED${NC}"

if [ "$TESTS_FAILED" -eq 0 ]; then
    echo -e "${GREEN}Tous les tests sont passés avec succès !${NC}"
    exit 0
else
    echo -e "${RED}Certains tests ont échoué.${NC}"
    exit 1
fi