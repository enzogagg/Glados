---
id: Tests
title: Lancer les Tests
sidebar_position: 10
---

# Lancer les Tests

Il est crucial de valider le bon fonctionnement du langage, que vous soyez développeur du noyau ou utilisateur.

## 1. Tests Unitaires du Projet

Ces tests valident le fonctionnement interne du Compilateur et de la VM (écrits en Haskell).

### Lancer tous les tests
```bash
make tests_run
```
Ou avec Stack directement :
```bash
stack test
```

Cela exécutera :
- Les tests du Parser (vérification de la syntaxe CLaD).
- Les tests de la VM (vérification des opérations sur la pile, gestion mémoire).

## 2. Tests Fonctionnels

Les tests fonctionnels vérifient que le langage "dans son ensemble" se comporte comme prévu. Ils compilent des programmes CLaD de référence et vérifient leur sortie.

### Le Script de Test
Utilisez le script fourni dans `scripts/` (ou à la racine selon configuration).

```bash
./scripts/functional_tests.sh
```

### Exemple de Test Fonctionnel

Les tests fonctionnels ressemblent à ceci :

**Fichier `test_fact.clad`** :
```clad
fonction factorielle(n)
    si n <= 1
        retourner 1
    sinon
        retourner n * factorielle(n - 1)
    fin
fin

principal
    afficher(factorielle(5))
fin
```

**Exécution attendue** :
1. Compilation de `test_fact.clad` -> `test_fact.cbc`
2. Exécution `./glados-vm test_fact.cbc`
3. Sortie attendue : `120`

## 3. Intégration Continue (CI)

Le projet dispose d'une pipeline CI (GitHub Actions) qui lance automatiquement ces tests à chaque Push. Voir la section [CI/CD](Ci-cd.md) pour plus de détails.