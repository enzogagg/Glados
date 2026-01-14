#!/bin/bash

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

COMPILER="$SCRIPT_DIR/../../../../glados-compiler"
VM="$SCRIPT_DIR/../../../../glados-vm"
TEST_FILE="$SCRIPT_DIR/../fonction/addition.clad"
OUTPUT_FILE="addition.cbc"

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
        return 0
    else
        echo -e "${RED}✗${NC} $test_name"
        echo -e "  ${YELLOW}Attendu:${NC} exit code=$expected_exit"
        echo -e "  ${YELLOW}Obtenu:${NC}  exit code=$actual_exit"
        if [ -n "$actual_output" ]; then
            echo -e "  ${YELLOW}Sortie:${NC}  '$actual_output'"
        fi
        ((TESTS_FAILED++))
        return 1
    fi
}

echo "=========================================="
echo "  Tests fonctionnels CLaD - Addition"
echo "=========================================="
echo ""

if [ ! -f "$COMPILER" ]; then
    echo -e "${RED}Erreur:${NC} Le compilateur 'glados-compiler' est introuvable."
    echo "Chemin attendu : $COMPILER"
    echo ""
    echo "Veuillez compiler le projet avec 'make' avant de lancer les tests."
    exit 1
fi

if [ ! -f "$VM" ]; then
    echo -e "${RED}Erreur:${NC} La VM 'glados-vm' est introuvable."
    echo "Chemin attendu : $VM"
    echo ""
    echo "Veuillez compiler le projet avec 'make' avant de lancer les tests."
    exit 1
fi

if [ ! -x "$COMPILER" ]; then
    echo -e "${YELLOW}Attention:${NC} Le compilateur n'est pas exécutable."
    echo "Ajout des permissions d'exécution..."
    chmod +x "$COMPILER"
fi

if [ ! -x "$VM" ]; then
    echo -e "${YELLOW}Attention:${NC} La VM n'est pas exécutable."
    echo "Ajout des permissions d'exécution..."
    chmod +x "$VM"
fi

if [ ! -f "$TEST_FILE" ]; then
    echo -e "${RED}Erreur:${NC} Le fichier de test est introuvable."
    echo "Chemin attendu : $TEST_FILE"
    exit 1
fi

echo "Compilateur : $COMPILER"
echo "VM          : $VM"
echo "Fichier     : $TEST_FILE"
echo ""

echo -e "${YELLOW}Note:${NC} Le fichier addition.clad doit utiliser 'afficher' et non 'retourner' dans principal"
echo ""

echo "Test 1: Compilation du programme"
echo "---------------------------------"

compile_output=$("$COMPILER" -o "$OUTPUT_FILE" "$TEST_FILE" 2>&1)
compile_exit=$?

if [ $compile_exit -eq 0 ]; then
    echo -e "${GREEN}✓${NC} Compilation réussie"
    ((TESTS_PASSED++))
else
    echo -e "${RED}✗${NC} Échec de la compilation"
    echo -e "  ${YELLOW}Code de sortie:${NC} $compile_exit"
    echo -e "  ${YELLOW}Sortie:${NC}"
    echo "$compile_output"
    ((TESTS_FAILED++))
    
    echo ""
    echo -e "${RED}Les tests suivants sont annulés (compilation échouée).${NC}"
    exit 1
fi

if [ ! -f "$OUTPUT_FILE" ]; then
    echo -e "${RED}✗${NC} Le fichier bytecode n'a pas été généré"
    echo -e "  ${YELLOW}Fichier attendu:${NC} $OUTPUT_FILE"
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
echo "Test 3: Vérification du résultat (2 + 3 = 5)"
echo "---------------------------------------------"

vm_result=$("$VM" "$OUTPUT_FILE" 2>&1)
vm_exit_code=$?

if [[ "$vm_result" == *"5"* ]] || [ "$vm_exit_code" -eq 5 ]; then
    echo -e "${GREEN}✓${NC} Le programme calcule bien 5 (2 + 3)"
    echo -e "  ${YELLOW}Sortie:${NC} $vm_result"
    ((TESTS_PASSED++))
else
    echo -e "${RED}✗${NC} Le programme devrait calculer 5"
    echo -e "  ${YELLOW}Attendu:${NC} 5 (dans la sortie ou exit code)"
    echo -e "  ${YELLOW}Obtenu:${NC}  exit=$vm_exit_code, sortie='$vm_result'"
    ((TESTS_FAILED++))
fi

echo ""
echo "Nettoyage des fichiers temporaires..."
rm -f "$OUTPUT_FILE"

echo ""
echo "=========================================="
echo "  Résumé des tests"
echo "=========================================="
echo -e "Tests réussis: ${GREEN}$TESTS_PASSED${NC}"
echo -e "Tests échoués: ${RED}$TESTS_FAILED${NC}"
echo ""

if [ "$TESTS_FAILED" -eq 0 ]; then
    echo -e "${GREEN}Tous les tests sont passés avec succès !${NC}"
    exit 0
else
    echo -e "${RED}Certains tests ont échoué.${NC}"
    exit 1
fi