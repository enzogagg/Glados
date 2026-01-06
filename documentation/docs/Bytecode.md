---
id: Bytecode
title: Bytecode Specification
sidebar_position: 7
---

# CLaD Bytecode Specification v1.0

## Table des matières

1. [Architecture générale](#1-architecture-générale)
   - 1.1 [Structure du fichier bytecode](#11-structure-du-fichier-bytecode)
   - 1.2 [Architecture de la machine virtuelle](#12-architecture-de-la-machine-virtuelle)

2. [Système de typage](#2-système-de-typage)
   - 2.1 [Tags de types](#21-tags-de-types)
   - 2.2 [Format des valeurs](#22-format-des-valeurs)
   - 2.3 [Exemples d'encodage](#23-exemples-dencodage)

3. [Jeu d'instructions](#3-jeu-dinstructions)
   - 3.1 [Gestion des valeurs](#31-gestion-des-valeurs)
   - 3.2 [Opérations arithmétiques](#32-opérations-arithmétiques)
   - 3.3 [Opérations de comparaison](#33-opérations-de-comparaison)
   - 3.4 [Opérations sur les listes](#34-opérations-sur-les-listes)
   - 3.5 [Symboles et évaluation](#35-symboles-et-évaluation)
   - 3.6 [Gestion des variables](#36-gestion-des-variables)
   - 3.7 [Contrôle de flux](#37-contrôle-de-flux)
   - 3.8 [Gestion des fonctions](#38-gestion-des-fonctions)
   - 3.9 [Entrées/Sorties](#39-entréessorties)
   - 3.10 [Terminaison](#310-terminaison)

4. [Caractéristiques de la VM](#4-caractéristiques-de-la-vm)
   - 4.1 [Architecture à pile](#41-architecture-à-pile)
   - 4.2 [Support multi-paradigmes](#42-support-multi-paradigmes)
   - 4.3 [Gestion des types](#43-gestion-des-types)

5. [Exemple d'exécution](#5-exemple-dexécution)
   - 5.1 [Code source CLaD](#51-code-source-clad)
   - 5.2 [Représentation en bytecode assembleur](#52-représentation-en-bytecode-assembleur)
   - 5.3 [Encodage binaire](#53-encodage-binaire)

6. [Format du fichier .cbc](#6-format-du-fichier-cbc)
   - 6.1 [Vue d'ensemble de la structure](#61-vue-densemble-de-la-structure)
   - 6.2 [Header](#62-header-10-bytes)
   - 6.3 [Constant Pool](#63-constant-pool)
   - 6.4 [Function Table](#64-function-table)
   - 6.5 [Instructions](#65-instructions)
   - 6.6 [Exemple complet de fichier .cbc](#66-exemple-complet-de-fichier-cbc)

---

## 1. Architecture générale

### 1.1 Structure du fichier bytecode

Le fichier `.cbc` (CLaD ByteCode) est structuré en quatre sections principales :

```
[ HEADER ]
[ CONSTANT POOL ]
[ FUNCTION TABLE ]
[ INSTRUCTIONS ]
```

### 1.2 Architecture de la machine virtuelle

La machine virtuelle CLaD repose sur une architecture à pile avec les composants suivants :

* **Stack** : Pile principale pour l'évaluation des expressions
* **Heap** : Gestion des structures dynamiques (listes, chaînes, objets)
* **Global Table** : Table des variables globales
* **Call Stack** : Pile d'exécution pour la gestion des appels de fonctions

Cette architecture s'inspire des machines virtuelles modernes (JVM, BEAM) tout en restant adaptée aux besoins du langage CLaD.

---

## 2. Système de typage

### 2.1 Tags de types

Le bytecode utilise un système de typage unifié basé sur des tags d'un octet :

| Tag (1 octet) | Type              | Description                    |
| ------------- | ----------------- | ------------------------------ |
| 00            | Int               | Entier signé                   |
| 01            | Float             | Nombre à virgule flottante     |
| 02            | Bool              | Booléen                        |
| 03            | Char              | Caractère                      |
| 04            | String            | Chaîne de caractères           |
| 05            | List              | Liste                          |
| 06            | Symbol            | Symbole                        |
| 07            | Nil               | Valeur nulle                   |
| 08            | Function          | Référence vers Function Table  |
| 09            | Tuple             | Tuple (liste fixe)             |
| 0A            | Array             | Tableau (vecteur)              |
| 0B            | Struct            | Structure (champs nommés)      |
| 0C            | Map               | Dictionnaire (Clé-Valeur)      |

### 2.2 Format des valeurs

Chaque valeur sur la pile respecte le format suivant :

```
[ TYPE_TAG ][ DATA... ]
```

### 2.3 Exemples d'encodage

**Note sur les flottants** : Ils suivent la norme **IEEE-754 simple précision (32 bits)**, format Big Endian.

```
00 00 00 00 2A     = Int 42
01 40 28 00 00     = Float 2.5  (0x40280000)
01 41 28 00 00     = Float 10.5 (0x41280000)
02 01              = Bool true
04 00 00 00 05 'H' 'e' 'l' 'l' 'o'  = String "Hello"
06 00 00 00 03 'f' 'o' 'o'          = Symbol 'foo'
```

---

## 3. Jeu d'instructions

### 3.1 Gestion des valeurs

| Opcode | Instruction      | Description                                |
| ------ | ---------------- | ------------------------------------------ |
| 01     | PUSH_CONST index | Pousse une constante (String, List, etc.)  |
| 02     | PUSH_INT value   | Pousse un entier immédiat (4 bytes)        |
| 03     | PUSH_FLOAT value | Pousse un flottant immédiat (4 bytes)      |
| 04     | PUSH_BOOL value  | Pousse un booléen immédiat (1 byte)        |
| 05     | PUSH_NIL         | Pousse la valeur nulle sur la pile         |
| 06     | POP              | Retire la valeur au sommet de la pile      |

**Note importante** : Les types complexes (Strings, Symboles, Listes) doivent être stockés dans le Constant Pool et chargés via `PUSH_CONST`. Les types immédiats (Int, Float, Bool) peuvent être encodés directement dans l'instruction.

---

### 3.2 Opérations arithmétiques

Les opérations arithmétiques supportent automatiquement les types Int et Float avec conversion implicite.

| Opcode | Instruction | Description                  |
| ------ | ----------- | ---------------------------- |
| 10     | ADD         | Addition                     |
| 11     | SUB         | Soustraction                 |
| 12     | MUL         | Multiplication               |
| 13     | DIV         | Division                     |
| 14     | MOD         | Modulo                       |
| 15     | NEG         | Négation                     |

---

### 3.3 Opérations de comparaison

| Opcode | Instruction | Description              |
| ------ | ----------- | ------------------------ |
| 20     | EQ          | Égalité                  |
| 21     | NEQ         | Inégalité                |
| 22     | LT          | Inférieur strict         |
| 23     | GT          | Supérieur strict         |
| 24     | LTE         | Inférieur ou égal        |
| 25     | GTE         | Supérieur ou égal        |

---

### 3.4 Opérations sur les listes

| Opcode | Instruction | Description                                    |
| ------ | ----------- | ---------------------------------------------- |
| 30     | CONS        | Construction d'une paire (cons cell)           |
| 31     | HEAD (CAR)  | Extraction de la tête d'une liste              |
| 32     | TAIL (CDR)  | Extraction de la queue d'une liste             |
| 33     | LIST size   | Crée une liste de n éléments depuis la pile    |
| 34     | LEN         | Retourne la longueur d'une liste               |

**Exemple** : `LIST 3` consomme les 3 valeurs au sommet de la pile pour créer `[a, b, c]`.

---

### 3.5 Symboles et évaluation

| Opcode | Instruction | Description                      |
| ------ | ----------- | -------------------------------- |
| 40     | MAKE_SYMBOL | Crée un symbole                  |
| 41     | QUOTE       | Quote une expression             |
| 42     | EVAL        | Évalue une expression quotée     |

Ces instructions permettent de supporter les mécanismes d'évaluation différée propres aux langages de type Lisp.

---

### 3.6 Gestion des variables

| Opcode | Instruction      | Description                              |
| ------ | ---------------- | ---------------------------------------- |
| 50     | LOAD nameIndex   | Charge la valeur d'une variable          |
| 51     | STORE nameIndex  | Stocke une valeur dans une variable      |
| 52     | DEFINE nameIndex | Définit une nouvelle variable            |

**Note** : Les identifiants de variables sont stockés dans le constant pool. L'index référence la position dans ce pool.

---

### 3.7 Contrôle de flux

| Opcode | Instruction          | Description                                     |
| ------ | -------------------- | ----------------------------------------------- |
| 60     | JMP address          | Saut inconditionnel à l'adresse spécifiée       |
| 61     | JMP_IF_TRUE address  | Saut conditionnel si la valeur au sommet est vraie |
| 62     | JMP_IF_FALSE address | Saut conditionnel si la valeur au sommet est fausse |

---

### 3.8 Gestion des fonctions

| Opcode | Instruction             | Description                                  |
| ------ | ----------------------- | -------------------------------------------- |
| 70     | CALL funcIndex argCount | Appel de fonction avec n arguments           |
| 71     | RETURN                  | Retour de fonction                           |
| 72     | CLOSURE funcIndex       | Création d'une closure                       |
| 73     | LOAD_ARG index          | Charge un argument de fonction               |

**Function Table** : Une table associe chaque index de fonction à son adresse dans le bytecode :

```
Function 0 → adresse 200
Function 1 → adresse 350
```

---

### 3.9 Entrées/Sorties

| Opcode | Instruction | Description                      |
| ------ | ----------- | -------------------------------- |
| 80     | PRINT       | Affiche la valeur au sommet      |
| 81     | INPUT       | Lit une entrée utilisateur       |

---

### 3.10 Structures de données complexes

| Opcode | Instruction       | Description                                      |
| ------ | ----------------- | ------------------------------------------------ |
| 90     | MAKE_TUPLE size   | Crée un tuple de n éléments depuis la pile       |
| 91     | TUPLE_GET index   | Accède à l'élément à l'index immédiat            |
| 92     | MAKE_ARRAY size   | Crée un tableau de n éléments depuis la pile     |
| 93     | ARRAY_GET         | Accès indexé (Stack: array, index)               |
| 94     | ARRAY_SET         | Modification (Stack: array, index, val)          |
| 95     | MAKE_MAP count    | Crée une map (Stack: k1, v1, k2, v2...)          |
| 96     | MAP_GET           | Accès par clé (Stack: map, key)                  |
| 97     | MAP_SET           | Ajout/Modif (Stack: map, key, val)               |
| 98     | MAKE_STRUCT count | Crée une struct de n champs (Stack: val1, ...)   |
| 99     | STRUCT_GET nameId | Accès champ par nom (arg: index string pool)     |
| 9A     | STRUCT_SET nameId | Modif champ par nom (arg: index string pool)     |

---

### 3.11 Terminaison

| Opcode | Instruction | Description                |
| ------ | ----------- | -------------------------- |
| FF     | HALT        | Arrête l'exécution de la VM |

---

## 4. Caractéristiques de la VM

### 4.1 Architecture à pile

L'architecture à pile offre plusieurs avantages :

* Simplicité de conception et d'implémentation
* Instructions indépendantes et modulaires
* Facilité de débogage (inspection de la pile)
* Compatibilité avec les paradigmes fonctionnels

### 4.2 Support multi-paradigmes

La VM CLaD est conçue pour supporter :

* **Programmation impérative** : variables, séquençage, boucles
* **Programmation fonctionnelle** : fonctions de première classe, closures
* **Programmation inspirée de Lisp** : symboles, quote/eval, manipulation de listes

### 4.3 Gestion des types

* Conversion automatique Int → Float dans les opérations arithmétiques
* Typage dynamique avec vérification à l'exécution
* Support des structures de données hétérogènes

---

## 5. Exemple d'exécution

### 5.1 Code source CLaD

```clad
liste = [1, 2, 3]
print(head(liste) + 10)
```

### 5.2 Représentation en bytecode assembleur

```
PUSH_INT 1
PUSH_INT 2
PUSH_INT 3
LIST 3
STORE "liste"

LOAD "liste"
HEAD
PUSH_INT 10
ADD
PRINT
HALT
```

### 5.3 Encodage binaire

```
02 00 00 00 01    ; PUSH_INT 1
02 00 00 00 02    ; PUSH_INT 2
02 00 00 00 03    ; PUSH_INT 3
51 00 00 00 00    ; STORE 0 ("liste")

50 00 00 00 00    ; LOAD 0 ("liste")
02 00 00 00 0A    ; PUSH_INT 10
10                ; ADD
80                ; PRINT
FF                ; HALT
```

---

## 6. Format du fichier .cbc

### 6.1 Vue d'ensemble de la structure

```
┌─────────────────────────────────────────┐
│              HEADER                     │
│  ┌────────────────────────────────┐    │
│  │ Magic Number    : 4 bytes      │    │
│  │ Version         : 2 bytes      │    │
│  │ Flags           : 1 byte       │    │
│  │ Reserved        : 3 bytes      │    │
│  └────────────────────────────────┘    │
├─────────────────────────────────────────┤
│          CONSTANT POOL                  │
│  ┌────────────────────────────────┐    │
│  │ Count           : 4 bytes      │    │
│  ├────────────────────────────────┤    │
│  │ Entry 0                        │    │
│  │  ├─ Type       : 1 byte        │    │
│  │  ├─ Length     : 4 bytes       │    │
│  │  └─ Data       : n bytes       │    │
│  ├────────────────────────────────┤    │
│  │ Entry 1                        │    │
│  │  ├─ Type       : 1 byte        │    │
│  │  ├─ Length     : 4 bytes       │    │
│  │  └─ Data       : n bytes       │    │
│  └────────────────────────────────┘    │
├─────────────────────────────────────────┤
│         FUNCTION TABLE                  │
│  ┌────────────────────────────────┐    │
│  │ Count           : 4 bytes      │    │
│  ├────────────────────────────────┤    │
│  │ Function 0                     │    │
│  │  ├─ Index      : 4 bytes       │    │
│  │  ├─ Address    : 4 bytes       │    │
│  │  └─ ArgCount   : 1 byte        │    │
│  ├────────────────────────────────┤    │
│  │ Function 1                     │    │
│  │  ├─ Index      : 4 bytes       │    │
│  │  ├─ Address    : 4 bytes       │    │
│  │  └─ ArgCount   : 1 byte        │    │
│  └────────────────────────────────┘    │
├─────────────────────────────────────────┤
│          INSTRUCTIONS                   │
│  ┌────────────────────────────────┐    │
│  │ Code Length     : 4 bytes      │    │
│  ├────────────────────────────────┤    │
│  │ Opcode 1        : 1 byte       │    │
│  │ Operands        : n bytes      │    │
│  ├────────────────────────────────┤    │
│  │ Opcode 2        : 1 byte       │    │
│  │ Operands        : n bytes      │    │
│  ├────────────────────────────────┤    │
│  │        ...                     │    │
│  └────────────────────────────────┘    │
└─────────────────────────────────────────┘
```

### 6.2 Header (10 bytes)

| Offset | Taille | Champ        | Valeur / Description                |
|--------|--------|--------------|-------------------------------------|
| 0x00   | 4      | Magic Number | `0x43 0x42 0x43 0x00` ("CBC\0")    |
| 0x04   | 2      | Version      | `0x01 0x00` (version 1.0)          |
| 0x06   | 1      | Flags        | `0x00` (réservé pour usage futur)  |
| 0x07   | 3      | Reserved     | `0x00 0x00 0x00` (padding)         |

**Exemple en hexadécimal :**
```
43 42 43 00 | 01 00 | 00 | 00 00 00
   Magic    | Ver.  |Flg | Reserved
```

### 6.3 Constant Pool

**Structure :**

```
┌──────────────────────────────────────┐
│ Count (4 bytes)                      │  ← Nombre total d'entrées
├──────────────────────────────────────┤
│ Entry 0                              │
│  ┌────────────────────────────────┐  │
│  │ Type Tag    : 1 byte           │  │  ← Type (voir section 2.1)
│  │ Length      : 4 bytes          │  │  ← Taille des données
│  │ Data        : Length bytes     │  │  ← Données brutes
│  └────────────────────────────────┘  │
├──────────────────────────────────────┤
│ Entry 1                              │
│  ...                                 │
└──────────────────────────────────────┘
```

**Exemples d'entrées :**

```
String "Hello":
04 | 00 00 00 05 | 48 65 6C 6C 6F
^    ^             ^
Type Length        Data

Symbol 'x':
06 | 00 00 00 01 | 78
^    ^             ^
Type Length        Data (ASCII 'x')

Int 42:
00 | 00 00 00 04 | 00 00 00 2A
^    ^             ^
Type Length        Data (32-bit int)
```

### 6.4 Function Table

**Structure :**

```
┌──────────────────────────────────────┐
│ Count (4 bytes)                      │  ← Nombre de fonctions
├──────────────────────────────────────┤
│ Function Entry 0                     │
│  ┌────────────────────────────────┐  │
│  │ Index       : 4 bytes          │  │  ← ID de la fonction
│  │ Address     : 4 bytes          │  │  ← Adresse dans le bytecode
│  │ ArgCount    : 1 byte           │  │  ← Nombre d'arguments
│  └────────────────────────────────┘  │
├──────────────────────────────────────┤
│ Function Entry 1                     │
│  ...                                 │
└──────────────────────────────────────┘
```

**Exemple :**

```
Count: 2 fonctions

Function 0: factorial (2 args, @ 0x00C8)
00 00 00 00 | 00 00 00 C8 | 02
   Index       Address      Args

Function 1: main (0 args, @ 0x0150)
00 00 00 01 | 00 00 01 50 | 00
   Index       Address      Args
```

### 6.5 Instructions

**Structure :**

```
┌──────────────────────────────────────┐
│ Code Length (4 bytes)                │  ← Taille totale du code
├──────────────────────────────────────┤
│ Instruction 0                        │
│  ┌────────────────────────────────┐  │
│  │ Opcode      : 1 byte           │  │
│  │ Operands    : variable         │  │
│  └────────────────────────────────┘  │
├──────────────────────────────────────┤
│ Instruction 1                        │
│  ...                                 │
├──────────────────────────────────────┤
│ ...                                  │
├──────────────────────────────────────┤
│ HALT (0xFF)                          │
└──────────────────────────────────────┘
```

**Exemple d'encodage :**

```
Instruction: PUSH_INT 42
┌────┬────────────────────┐
│ 02 │ 00 00 00 2A       │
└────┴────────────────────┘
  ^          ^
Opcode    Operand (4 bytes)

Instruction: ADD
┌────┐
│ 10 │
└────┘
  ^
Opcode (pas d'opérande)

Instruction: CALL 0 with 2 args
┌────┬────────────────────┬────┐
│ 70 │ 00 00 00 00       │ 02 │
└────┴────────────────────┴────┘
  ^          ^             ^
Opcode   Function Index  ArgCount
```

### 6.6 Exemple complet de fichier .cbc

Programme : `print(42)`

```
┌─────────── HEADER ───────────┐
│ 43 42 43 00                  │  Magic "CBC\0"
│ 01 00                        │  Version 1.0
│ 00                           │  Flags
│ 00 00 00                     │  Reserved
├────── CONSTANT POOL ─────────┤
│ 00 00 00 00                  │  Count: 0 entrées
├────── FUNCTION TABLE ────────┤
│ 00 00 00 00                  │  Count: 0 fonctions
├─────── INSTRUCTIONS ─────────┤
│ 00 00 00 06                  │  Code Length: 6 bytes
│ 02 00 00 00 2A               │  PUSH_INT 42
│ 80                           │  PRINT
│ FF                           │  HALT
└──────────────────────────────┘

Taille totale: 10 + 4 + 4 + 4 + 6 = 28 bytes
```

---
