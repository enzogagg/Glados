#!/bin/bash

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

COMPILER="$SCRIPT_DIR/../../../../glados-compiler"
VM="$SCRIPT_DIR/../../../../glados-vm"
TEST_FILE="$SCRIPT_DIR/../controllers/si_sinon_si.clad"
OUTPUT_FILE="si_sinon_afficher.cbc"

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m'

TESTS_PASSED=0
TESTS_FAILED=0

print_ok() {
    echo -e "${GREEN}✓${NC} $1"
    ((TESTS_PASSED++))
}

print_fail() {
    echo -e "${RED}✗${NC} $1"
    ((TESTS_FAILED++))
}

echo "=========================================="
echo "  Tests fonctionnels CLaD - Si / Sinon"
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

if [ "$compile_exit" -eq 0 ]; then
    print_ok "Compilation réussie"
else
    print_fail "Échec de la compilation"
    echo "$compile_output"
    exit 1
fi

echo ""
echo "Test 2: Exécution du bytecode"
echo "------------------------------"

vm_output=$("$VM" "$OUTPUT_FILE" 2>&1)
vm_exit=$?

if [ "$vm_exit" -eq 0 ]; then
    print_ok "Exécution sans erreur"
else
    print_fail "Erreur à l'exécution (exit=$vm_exit)"
    echo "$vm_output"
fi

echo ""
echo "Test 3: Vérification du résultat affiché"
echo "----------------------------------------"

expected_output="1"

if echo "$vm_output" | grep -Fxq "$expected_output"; then
    print_ok "Le bon bloc 'sinon si' a été exécuté (affiche 1)"
    echo -e "  ${YELLOW}Sortie:${NC} $vm_output"
else
    print_fail "Sortie incorrecte"
    echo -e "  ${YELLOW}Attendu:${NC} $expected_output"
    echo -e "  ${YELLOW}Obtenu:${NC}  $vm_output"
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