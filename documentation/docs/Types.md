---
id: Types
title: Système de Types
sidebar_position: 5
---

# Système de Types

Le langage Glados (et sa variante CLaD) repose sur un système de types interne robuste défini en Haskell.

## 1. Expressions (Expr)
Le type `Expr` représente l'AST brut issu du parsing initial.

- **Number** : Entiers (`Integer`).
- **FloatLiteral** : Nombres à virgule flottante (`Double`).
- **Boolean** : Booléens (`Bool`).
- **Symbol** : Identifiants ou symboles.
- **String** : Chaînes de caractères.
- **List** : Liste d'expressions S-expressions.

## 2. IAST (Intermediate Abstract Syntax Tree)
L'`IAST` est une représentation intermédiaire plus riche, utilisée après une phase de transformation pour faciliter la compilation ou l'interprétation.

### Types primitifs
- `IANumber`, `IAFloatLiteral`, `IABoolean`, `IASymbol`, `IAString`.

### Formes spéciales
- **IAIf** : Conditionnelle `if`.
- **IALambda** : Fonctions anonymes.
- **IADefine** : Définition de variables/fonctions.
- **IAQuote** : Quotation.

### Extensions CLaD
- **IAInfix** : Opérations infixes.
- **IACall** : Appels de fonctions.
- **IAFunctionDef** : Définition de fonctions typées.
- **IADeclare**, **IAAssign** : Gestion des variables.
- **IAWhile**, **IAFor** : Boucles.
- **IAProgram** : Programme complet.
- **IAUnit** : Type unit (void).

## 3. CladType (Types CLaD)
Types explicites utilisés dans la syntaxe CLaD pour le typage statique ou dynamique.

- `IntT`, `FloatT`, `BoolT`, `StringT`, `VoidT`
- `ListT CladType` : Liste typée.

## 4. Value (Valeurs d'Exécution)
Le type `Value` représente les données manipulées par la VM lors de l'exécution.

| Constructeur | Description |
|---|---|
| `IntVal` | Valeur entière. |
| `FloatVal` | Valeur flottante. |
| `BoolVal` | Valeur booléenne (`#t` / `#f`). |
| `FuncVal` | Fermeture (Closure) contenant arguments, corps et environnement. |
| `Primitive` | Fonction native (Haskell). |
| `ListVal` | Liste de valeurs. |
| `SymbolVal` | Symbole atomique. |
| `StringVal` | Chaîne de caractères. |
| `Void` | Absence de valeur. |

## 5. Environnement (Env)
L'environnement `Env` est une liste d'association `[(String, Value)]` stockant les variables et fonctions définies.
