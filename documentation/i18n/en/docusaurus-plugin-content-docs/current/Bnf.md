---
id: BNF
title: BNF Grammar
sidebar_position: 5
---

# BNF Grammar of the CLaD Language

This specification defines the formal syntax of the **CLaD** language (Cognitive Logic and Data) using BNF notation (Backus–Naur Form).

---

## Notation Conventions

| Symbol | Meaning |
|--------|---------|
| `<symbole>` | Non-terminal symbol |
| `"texte"` | Terminal (keyword or literal symbol) |
| `|` | Alternative (logical OR) |
| `[ ... ]` | Optional element |
| `{ ... }` | Repetition (zero or more times) |
| `( ... )` | Grouping |

---

## 1. General Program Structure

```bnf
<programme> ::= { <déclaration> | <fonction> } <principal>

<principal> ::= "principal" <bloc> "fin"
```

A CLaD program is composed of optional declarations and functions, followed by a mandatory main block.

## 2. Declarations

```bnf
<déclaration> ::= <déclaration_constante>
                | <déclaration_variable>

<déclaration_constante> ::= "constante" <identificateur> <expression>

<déclaration_variable> ::= "variable" <identificateur> <expression>
```

### Example
```clad
constante VITESSE_LUMIERE 299792458
variable compteur 0
```
---

3. Functions

```bnf
<fonction> ::= "fonction" <identificateur> "(" [ <paramètres> ] ")" <bloc> "fin"

<paramètres> ::= <identificateur> { "," <identificateur> }

<bloc> ::= { <instruction> }
```

### Example

```
fonction addition(a, b)
    retourner a + b
fin
```

## 4. Instructions

```bnf
<instruction> ::= <affectation>
                | <appel_fonction>
                | <retour>
                | <structure_contrôle>
                | <affichage>

<affectation> ::= <identificateur> "=" <expression>

<appel_fonction> ::= <identificateur> "(" [ <arguments> ] ")"

<arguments> ::= <expression> { "," <expression> }

<retour> ::= "retourner" <expression>

<affichage> ::= "afficher" "(" <expression> ")"

### Examples

```clad
x = 42
resultat = addition(3, 5)
retourner x * 2
afficher("Test en cours...")
```
---

## 5. Control Structures

### 5.1 Conditional

```bnf
<structure_contrôle> ::= <si>
                       | <tantque>
                       | <pour>

<si> ::= "si" <expression> <bloc> { <sinon_si> } [ <sinon> ] "fin"

<sinon_si> ::= "sinon" "si" <expression> <bloc>

<sinon> ::= "sinon" <bloc>
```

#### Example

```clad
si x > 10
    afficher("Grand")
sinon si x > 5
    afficher("Moyen")
sinon
    afficher("Petit")
fin
```

### 5.2 Loops

```bnf
<tantque> ::= "tantque" <expression> <bloc> "fin"

<pour> ::= "pour" <identificateur> "de" <expression> "à" <expression> <bloc> "fin"

Examples

tantque x < 10
    x = x + 1
fin

pour i de 0 à 10
    afficher(i)
fin
```

## 6. Expressions

```bnf

<expression> ::= <terme> { <opérateur_additif> <terme> }

<terme> ::= <facteur> { <opérateur_multiplicatif> <facteur> }

<facteur> ::= <nombre>
            | <chaîne>
            | <booléen>
            | <identificateur>
            | <appel_fonction>
            | <liste>
            | <lambda>
            | "(" <expression> ")"
            | <opérateur_unaire> <facteur>
```

Expressions follow standard mathematical operator precedence.

---

## 7. Operators

### 7.1 Arithmetic Operators

```bnf
<opérateur_additif> ::= "+" | "-"

<opérateur_multiplicatif> ::= "*" | "/" | "div" | "mod"

<opérateur_unaire> ::= "-" | "abs" | "arrondi"
```

### 7.2 Logical Operators
```bnf
<opérateur_logique> ::= "et" | "ou" | "non"
```

7.3 Comparison Operators
```bnf
<opérateur_comparaison> ::= "=" | ">" | "<" | ">=" | "<="
```

### Operator Precedence

Du plus prioritaire au moins prioritaire :

1. **Unary** : `-`, `abs`, `arrondi`, `non`
2. **Multiplicative** : `*`, `/`, `div`, `mod`
3. **Additive** : `+`, `-`
4. **Comparison** : `=`, `>`, `<`, `>=`, `<=`
5. **Logical** : `et`, `ou`

All binary operators are left-associative.

---

## 8. Basic Types

### 8.1 Numbers

```bnf
<nombre> ::= <entier> | <virgule>

<entier> ::= [ "-" ] <chiffre> { <chiffre> }

<virgule> ::= [ "-" ] <chiffre> { <chiffre> } "." <chiffre> { <chiffre> }

<chiffre> ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
```

#### Examples
```clad
42
-17
3.14159
-0.5
```

### 8.2 Strings

```bnf
<chaîne> ::= '"' { <caractère> } '"'

<caractère> ::= <tout_caractère_sauf_guillemet>
```

#### Example

```clad
"Bonjour, Sujet 17"
```

8.3 Booleans

```bnf
<booléen> ::= "vrai" | "faux"
```

8.4 Lists
```bnf
<liste> ::= "Liste" <expression> { <expression> }
          | "[" [ <éléments_liste> ] "]"

<éléments_liste> ::= <expression> { <expression> }
```

#### Examples
```clad
Liste 1 2 3
[1 2 3]
[]
```

---

## 9. First-Class Functions

### 9.1 Lambda (Anonymous Functions)
```bnf
<lambda> ::= "lambda" "(" [ <paramètres> ] ")" <expression>
```

#### Example

```clad
lambda (x) (* x 2)
```

### 9.2 Native List Functions

```bnf
<fonction_liste> ::= "Premier" "(" <expression> ")"
                   | "reste" "(" <expression> ")"
                   | "ajout" "(" <expression> "," <expression> ")"
                   | "map" "(" <expression> "," <expression> ")"
                   | "filtre" "(" <expression> "," <expression> ")"
                   | "réduis" "(" <expression> "," <expression> "," <expression> ")"
```

#### Examples

```clad
Premier([1 2 3])                 # => 1
map(lambda (x) (* x 2), [1 2 3]) # => [2 4 6]
```

### 9.3 Native String Functions

```bnf
<fonction_chaîne> ::= "phr-long" "(" <expression> ")"
                    | "phr-ajout" "(" <expression> "," <expression> ")"
                    | "phr-coupe" "(" <expression> "," <expression> ")"
                    | "phr-cherche?" "(" <expression> "," <expression> ")"
```

#### Examples

```clad
phr-long("CLaD")                    # => 4
phr-ajout("Bonjour, ", "Sujet 17")  # => "Bonjour, Sujet 17"
```
---

## 10. Identifiers

```bnf
<identificateur> ::= <lettre> { <lettre> | <chiffre> | "_" | "-" }

<lettre> ::= "a" | "b" | ... | "z" | "A" | "B" | ... | "Z"
           | "à" | "é" | "è" | "ê" | "ç" | ...
```

Identifiers:

    - Must start with a letter

    - May contain letters, digits, hyphens, and underscores

    - Support French accented characters

    - Are case-sensitive

#### Valid Examples

```clad
compteur
nom_utilisateur
vitesse-lumière
résultat
PI
```

---

## 11. Comments
```bnf
<commentaire> ::= "#" { <caractère> } <fin_de_ligne>

<commentaire_multiligne> ::= "###" { <caractère> | <fin_de_ligne> } "###"
```

Comments are ignored by the parser.

#### Examples

```clad
# Ceci est un commentaire

###
Ceci est un commentaire
sur plusieurs lignes
###
```
---

## 12. Special Values

```bnf
<valeur_spéciale> ::= "Neant"
                    | <erreur>

<erreur> ::= "Erreur:" <type_erreur>

<type_erreur> ::= "DivisionParZero"
                | "AccesHorsLimites"
                | "TypeInvalide"
                | <message_erreur>
```

### Examples

```clad
Neant                       # Absence of value
Erreur: DivisionParZero     # Division error
```

---
## Complete Annotated Example

Below is a complete program analyzed according to the grammar:

```clad
### Programme de démonstration ###

# Déclaration de constante
constante PI 3.14159

# Définition de fonction
fonction aire_cercle(rayon)
    retourner PI * rayon * rayon
fin

# Point d'entrée
principal
    afficher("Aire du cercle de rayon 5:")
    afficher(aire_cercle(5))

    # Boucle for
    pour i de 1 à 3
        # Structure conditionnelle
        si i = 2
            afficher("Deux!")
        sinon
            afficher(i)
        fin
    fin
fin
```

** Syntactic breakdown:**

1. `Constante PI 3.14159 → <déclaration_constante>`

2. `Fonction aire_cercle(rayon) ... fin → <fonction>`

3. `Principal ... fin → <principal>`

4. `Afficher(...) → <instruction> of type <affichage>`
5. `Pour i de 1 à 3 ... fin → <pour>`
6. `Si i = 2 ... fin` → `<si>`

---

## Implementation Notes

### Significant Indentation

Although not directly expressed in the formal BNF, indentation is used to improve readability. Blocks are delimited by start keywords (fonction, si, tantque, pour, principal) and the keyword fin.

### Case Sensitivity

Keywords are case-sensitive:

- ✓ si, fonction, principal

- ✗ Si, Fonction, Principal

### Whitespace

Spaces, tabs, and line breaks are separators but do not affect semantics (except for token separation).si i = 2 ... fin → <si>

---

| Catégorie | Mots-clés |
|-----------|-----------|
| **Declarations** | `constante`, `variable` |
| **Fonctions** | `fonction`, `fin`, `retourner`, `lambda` |
| **Control** | `si`, `sinon`, `tantque`, `pour`, `de`, `à` |
| **I/O** | `afficher` |
| **Booleans** | `vrai`, `faux` |
| **Logic** | `et`, `ou`, `non` |
| **Arithmetic** | `div`, `mod`, `abs`, `arrondi` |
| **Lists** | `Liste`, `Premier`, `reste`, `ajout`, `map`, `filtre`, `réduis` |
| **Strings** | `phr-long`, `phr-ajout`, `phr-coupe`, `phr-cherche?` |
| **Special** | `Neant`, `Erreur`, `principal` |

---


**This BNF grammar constitutes the complete formal specification of the CLaD language and serves as the reference for parser implementation.**

:::tip Compliance
Any program that conforms to this grammar is considered syntactically valid and can be processed by the GLaDOS interpreter.
:::