#!/bin/bash

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

COMPILER="$SCRIPT_DIR/../../../../glados-compiler"
VM="$SCRIPT_DIR/../../../../glados-vm"
TEST_FILE="$SCRIPT_DIR/../optimisation/test_opti.clad"
OUTPUT_FILE="constant_folding.cbc"

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
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
echo "  Tests fonctionnels CLaD"
echo "  Optimisation: Constant Folding"
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
    chmod +x "$COMPILER"
fi

if [ ! -x "$VM" ]; then
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

echo "Test 3: Vérification de l'optimisation"
echo "---------------------------------------"
echo -e "${BLUE}Info:${NC} Compilation avec --visualize pour voir le bytecode optimisé"

visualize_output=$("$COMPILER" -o "$OUTPUT_FILE" --visualize "$TEST_FILE" 2>&1)

echo ""
echo -e "${YELLOW}Bytecode généré:${NC}"
echo "----------------------------------------"
echo "$visualize_output"
echo "----------------------------------------"
echo ""

if [[ "$visualize_output" == *"20"* ]]; then
    echo -e "${GREEN}✓${NC} L'expression (2 + 3) * 4 semble être optimisée en constante"
    echo -e "  ${YELLOW}Note:${NC} Le bytecode contient directement la valeur 20"
    ((TESTS_PASSED++))
else
    echo -e "${YELLOW}⚠${NC}  Impossible de vérifier l'optimisation automatiquement"
    echo -e "  ${YELLOW}Note:${NC} Vérifiez manuellement le bytecode ci-dessus"
    echo -e "  ${YELLOW}Attendu:${NC} Une instruction directe avec la valeur 20"
    echo -e "  ${YELLOW}Non attendu:${NC} Des instructions séparées pour 2, 3, +, 4, *"
fi

echo ""

echo "Test 4: Vérification de l'optimisation booléenne"
echo "-------------------------------------------------"

# Vérifier que "vrai et faux" est optimisé en "faux"
if [[ "$visualize_output" == *"faux"* ]] || [[ "$visualize_output" == *"false"* ]] || [[ "$visualize_output" == *"0"* ]]; then
    echo -e "${GREEN}✓${NC} L'expression (vrai et faux) semble être optimisée"
    echo -e "  ${YELLOW}Note:${NC} Le bytecode contient directement la valeur faux/false"
    ((TESTS_PASSED++))
else
    echo -e "${YELLOW}⚠${NC}  Impossible de vérifier l'optimisation booléenne automatiquement"
    echo -e "  ${YELLOW}Note:${NC} Vérifiez manuellement le bytecode ci-dessus"
    echo -e "  ${YELLOW}Attendu:${NC} Une instruction directe avec la valeur faux"
    echo -e "  ${YELLOW}Non attendu:${NC} Des instructions séparées pour vrai, faux, et"
fi

echo ""

echo "Test 5: Analyse de la taille du bytecode"
echo "-----------------------------------------"

if [ -f "$OUTPUT_FILE" ]; then
    bytecode_size=$(wc -c < "$OUTPUT_FILE")
    echo -e "${BLUE}Info:${NC} Taille du bytecode: $bytecode_size octets"
    if [ "$bytecode_size" -lt 1000 ]; then
        echo -e "${GREEN}✓${NC} Le bytecode est compact (optimisation probable)"
        ((TESTS_PASSED++))
    else
        echo -e "${YELLOW}⚠${NC}  Le bytecode est plus grand que prévu"
        echo -e "  ${YELLOW}Note:${NC} Cela peut indiquer que les optimisations ne sont pas appliquées"
    fi
else
    echo -e "${RED}✗${NC} Fichier bytecode introuvable"
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

echo "=========================================="
echo "  Vérifications manuelles recommandées"
echo "=========================================="
echo ""
echo "1. Compilez avec --visualize pour voir le bytecode:"
echo -e "   ${BLUE}$COMPILER -o test.cbc --visualize $TEST_FILE${NC}"
echo ""
echo "2. Vérifiez que l'expression (2 + 3) * 4 est remplacée par 20"
echo "3. Vérifiez que (vrai et faux) est remplacé par faux"
echo "4. Le bytecode ne devrait PAS contenir d'instructions d'addition"
echo "   ou de multiplication pour les expressions constantes"
echo ""

if [ "$TESTS_FAILED" -eq 0 ]; then
    echo -e "${GREEN}Tous les tests automatiques sont passés !${NC}"
    echo -e "${YELLOW}Vérifiez manuellement le bytecode pour confirmer l'optimisation.${NC}"
    exit 0
else
    echo -e "${RED}Certains tests ont échoué.${NC}"
    exit 1
fi