---
id: Tests
title: Protocoles de Test
sidebar_position: 4
---

# Protocoles de Test

Le Centre d'Enrichissement accorde une importance capitale à la validation des données. Tout code non testé sera considéré comme une violation du protocole de sécurité.

## Tests Unitaires

Les tests unitaires vérifient le bon fonctionnement des composants individuels du noyau GLaDOS (parsing, évaluation, types).

### Localisation
Les sujets de test se trouvent dans le secteur `test/` :
- `ParseToExprSpec.hs` : Vérifie la transformation du texte en AST.
- `ParseValueSpec.hs` : Valide l'évaluation des expressions et les primitives.
- `MainSpec.hs` : Tests généraux du point d'entrée.

### Exécution
Pour lancer la séquence de tests unitaires via le terminal :

```bash
make tests_run
```
Ou directement avec Stack :
```bash
stack test
```

### Ajouter un Test
1. Créez ou modifiez un fichier `*Spec.hs` dans `test/`.
2. Utilisez la syntaxe `hspec` pour définir vos assertions.
3. Vérifiez que votre test ne provoque pas de paradoxe temporel.

---

## Tests Fonctionnels

Les tests fonctionnels valident le comportement global de l'interpréteur en conditions réelles.

### Localisation
Le script de contrôle est situé à : `scripts/functional_tests.sh`.

### Fonctionnement
Le script :
1. Compile le binaire `glados`.
2. Injecte des instructions (via `stdin` ou fichiers).
3. Compare la sortie standard et le code de retour avec les valeurs attendues.

### Exécution
```bash
./scripts/functional_tests.sh
```

### Ajouter un Test
Ouvrez `scripts/functional_tests.sh` et ajoutez une ligne `assert_output` :

```bash
# assert_output "Nom du Test" "Entrée" "Code Retour Attendu" "Sortie Attendue"
assert_output "Test Gâteau" "(eq? 'cake 'lie)" 0 "#t"
```

---

:::tip Note du Superviseur
Un code vert est un code joyeux. Un code rouge entraînera la libération de neurotoxine.
:::