---
id: types
title: Système de Types
sidebar_position: 3
---

# Types de Données

Bien que CLaD soit dynamiquement typé à l'exécution (dans la machine virtuelle), il manipule des concepts de types précis. Voici les types que vous manipulerez dans votre code.

## Types Primitifs

| Type | Mot-clé (Doc) | Description | Exemple |
|---|---|---|---|
| **Entier** | `entier` | Nombre entier signé 64-bit. | `42`, `-7` |
| **Flottant** | `flottant` | Nombre décimal double précision. | `3.14`, `-0.01` |
| **Booléen** | `booleen` | Valeur de vérité. | `vrai`, `faux` |
| **Chaîne** | `phrase` | Séquence de caractères. | `"CLaD est génial"` |
| **Symbole**| `symbole` | Identifiant unique (rarement utilisé directement). | `atom` |
| **Rien** | `vide` / `unit` | Absence de valeur (retour de procédure). | (Résultat d'un `afficher`) |

## Structures Complexes

Ces types permettent d'organiser les données. Voir la section [Structures de Données](Builtins/DataStructures.md) pour leur manipulation détaillée.

### Liste
Collection chaînée d'éléments.
- Création : `(1 2 3)` (Via `cons` ou citation).
- Usage : Traitement séquentiel récursif.

### Tableau (`Array`)
Collection indexée mutable.
- Syntaxe : `[1, 2, 3]`
- Accès rapide par index `tab[0]`.

### Tuple
Collection fixe et immuable de valeurs hétérogènes.
- Syntaxe : `{1, "A", vrai}`
- Idéal pour retourner plusieurs valeurs d'une fonction.

### Dictionnaire (`Map`)
Association Clé -> Valeur.

### Structure (`Struct`)
Objet composé avec champs nommés.
