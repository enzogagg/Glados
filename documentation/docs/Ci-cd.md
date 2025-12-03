---
id: Ci-cd
title: CI/CD
sidebar_position: 5
---

# CI/CD

Le projet GLaDOS utilise une chaîne d'intégration et de déploiement continu (CI/CD) basée sur **GitHub Actions** pour garantir la qualité du code et automatiser les livraisons.

## Architecture du Pipeline

Le pipeline est orchestré par le fichier `.github/workflows/ci-orchestrator.yml`. Il coordonne l'exécution de plusieurs modules spécialisés.

### 1. Linting (Qualité du Code)

Cette étape est exécutée en premier pour vérifier la conformité du code et des messages de commit.

- **Commit Linting** : Vérifie que les messages de commit respectent la convention (ex: `feat:`, `fix:`, `docs:`).
- **Haskell Linting** : Analyse le code source avec `hlint` pour détecter les mauvaises pratiques.

### 2. Build & Test (Construction et Tests)

Si le linting passe, le projet est compilé et testé.

- **Compilation** : Utilise `make` pour construire le binaire `glados`.
- **Vérification du Nettoyage** : S'assure que `make clean` et `make fclean` fonctionnent correctement.
- **Tests Unitaires** : Exécutés via `stack test` avec rapport de couverture.
- **Tests Fonctionnels** : Lance une suite de scripts shell (`scripts/functional_tests.sh`) pour valider le comportement global de l'interpréteur.

### 3. Déploiement (Branche Main uniquement)

Ces étapes ne sont déclenchées que lors d'un push sur la branche `main`, et seulement si les étapes précédentes ont réussi.

#### Mirroring
Le code est automatiquement poussé vers le dépôt Epitech via `pixta-dev/repository-mirroring-action`.

#### Documentation
1. Génération de la documentation API Haskell avec `haddock`.
2. Construction du site de documentation utilisateur avec **Docusaurus**.
3. Déploiement automatique sur **GitHub Pages**.

## Configuration

Les workflows sont définis dans le dossier `.github/workflows/` :

- `ci-orchestrator.yml` : Point d'entrée principal.
- `linting.yml` : Vérifications de style.
- `build-and-test.yml` : Compilation et tests.
- `mirroring.yml` : Synchronisation avec Epitech.
- `documentation.yml` : Génération et publication de la doc.
