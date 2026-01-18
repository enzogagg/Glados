---
id: BNF
title: Grammaire BNF
sidebar_position: 5
---

# Grammaire BNF du langage CLaD

Cette spécification définit la syntaxe formelle du langage **CLaD** (Cognitive Logic and Data) en notation BNF (Backus-Naur Form).

---

## Conventions de notation

| Symbole | Signification |
|---------|---------------|
| `<symbole>` | Symbole non-terminal |
| `"texte"` | Terminal (mot-clé ou symbole littéral) |
| `|` | Alternative (OU logique) |
| `[ ... ]` | Élément optionnel |
| `{ ... }` | Répétition (zéro ou plusieurs fois) |
| `( ... )` | Groupement |

---

## 1. Structure générale d'un programme

```bnf
<programme> ::= { <déclaration> | <fonction> } <principal>

<principal> ::= "principal" <bloc> "fin"
```

Un programme CLaD est composé de déclarations et fonctions optionnelles, suivies d'un bloc principal obligatoire.

---

## 2. Déclarations

```bnf
<déclaration> ::= <déclaration_constante>
                | <déclaration_variable>

<déclaration_constante> ::= "constante" <identificateur> <expression>

<déclaration_variable> ::= "variable" <identificateur> <expression>
```

### Exemple

```clad
constante VITESSE_LUMIERE 299792458
variable compteur 0
```

---

## 3. Fonctions

```bnf
<fonction> ::= "fonction" <identificateur> "(" [ <paramètres> ] ")" <bloc> "fin"

<paramètres> ::= <identificateur> { "," <identificateur> }

<bloc> ::= { <instruction> }
```

### Exemple

```clad
fonction addition(a, b)
    retourner a + b
fin
```

---

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
```

### Exemples

```clad
x = 42
resultat = addition(3, 5)
retourner x * 2
afficher("Test en cours...")
```

---

## 5. Structures de contrôle

### 5.1 Conditionnel

```bnf
<structure_contrôle> ::= <si>
                       | <tantque>
                       | <pour>

<si> ::= "si" <expression> <bloc> { <sinon_si> } [ <sinon> ] "fin"

<sinon_si> ::= "sinon" "si" <expression> <bloc>

<sinon> ::= "sinon" <bloc>
```

#### Exemple

```clad
si x > 10
    afficher("Grand")
sinon si x > 5
    afficher("Moyen")
sinon
    afficher("Petit")
fin
```

### 5.2 Boucles

```bnf
<tantque> ::= "tantque" <expression> <bloc> "fin"

<pour> ::= "pour" <identificateur> "de" <expression> "à" <expression> <bloc> "fin"
```

#### Exemples

```clad
tantque x < 10
    x = x + 1
fin

pour i de 0 à 10
    afficher(i)
fin
```

---

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

Les expressions respectent la priorité standard des opérateurs mathématiques.

---

## 7. Opérateurs

### 7.1 Opérateurs arithmétiques

```bnf
<opérateur_additif> ::= "+" | "-"

<opérateur_multiplicatif> ::= "*" | "/" | "div" | "mod"

<opérateur_unaire> ::= "-" | "abs" | "arrondi"
```

### 7.2 Opérateurs logiques

```bnf
<opérateur_logique> ::= "et" | "ou" | "non"
```

### 7.3 Opérateurs de comparaison

```bnf
<opérateur_comparaison> ::= "=" | ">" | "<" | ">=" | "<="
```

### Priorité des opérateurs

Du plus prioritaire au moins prioritaire :

1. **Unaires** : `-`, `abs`, `arrondi`, `non`
2. **Multiplicatifs** : `*`, `/`, `div`, `mod`
3. **Additifs** : `+`, `-`
4. **Comparaisons** : `=`, `>`, `<`, `>=`, `<=`
5. **Logiques** : `et`, `ou`

Tous les opérateurs binaires sont associatifs à gauche.

---

## 8. Types de base

### 8.1 Nombres

```bnf
<nombre> ::= <entier> | <virgule>

<entier> ::= [ "-" ] <chiffre> { <chiffre> }

<virgule> ::= [ "-" ] <chiffre> { <chiffre> } "." <chiffre> { <chiffre> }

<chiffre> ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
```

#### Exemples

```clad
42
-17
3.14159
-0.5
```

### 8.2 Chaînes de caractères

```bnf
<chaîne> ::= '"' { <caractère> } '"'

<caractère> ::= <tout_caractère_sauf_guillemet>
```

#### Exemple

```clad
"Bonjour, Sujet 17"
```

### 8.3 Booléens

```bnf
<booléen> ::= "vrai" | "faux"
```

### 8.4 Listes

```bnf
<liste> ::= "Liste" <expression> { <expression> }
          | "[" [ <éléments_liste> ] "]"

<éléments_liste> ::= <expression> { <expression> }
```

#### Exemples

```clad
Liste 1 2 3
[1 2 3]
[]
```

---

## 9. Fonctions de première classe

### 9.1 Lambda (fonctions anonymes)

```bnf
<lambda> ::= "lambda" "(" [ <paramètres> ] ")" <expression>
```

#### Exemple

```clad
lambda (x) (* x 2)
```

### 9.2 Fonctions natives sur les listes

```bnf
<fonction_liste> ::= "Premier" "(" <expression> ")"
                   | "reste" "(" <expression> ")"
                   | "ajout" "(" <expression> "," <expression> ")"
                   | "map" "(" <expression> "," <expression> ")"
                   | "filtre" "(" <expression> "," <expression> ")"
                   | "réduis" "(" <expression> "," <expression> "," <expression> ")"
```

#### Exemples

```clad
Premier([1 2 3])              # => 1
map(lambda (x) (* x 2), [1 2 3])  # => [2 4 6]
```

### 9.3 Fonctions natives sur les chaînes

```bnf
<fonction_chaîne> ::= "phr-long" "(" <expression> ")"
                    | "phr-ajout" "(" <expression> "," <expression> ")"
                    | "phr-coupe" "(" <expression> "," <expression> ")"
                    | "phr-cherche?" "(" <expression> "," <expression> ")"
```

#### Exemples

```clad
phr-long("CLaD")                    # => 4
phr-ajout("Bonjour, ", "Sujet 17")  # => "Bonjour, Sujet 17"
```

---

## 10. Identificateurs

```bnf
<identificateur> ::= <lettre> { <lettre> | <chiffre> | "_" | "-" }

<lettre> ::= "a" | "b" | ... | "z" | "A" | "B" | ... | "Z" 
           | "à" | "é" | "è" | "ê" | "ç" | ...
```

Les identificateurs :
- Doivent commencer par une lettre
- Peuvent contenir des lettres, chiffres, tirets et underscores
- Supportent les caractères accentués français
- Sont sensibles à la casse

#### Exemples valides

```clad
compteur
nom_utilisateur
vitesse-lumière
résultat
PI
```

---

## 11. Commentaires

```bnf
<commentaire> ::= "#" { <caractère> } <fin_de_ligne>

<commentaire_multiligne> ::= "###" { <caractère> | <fin_de_ligne> } "###"
```

Les commentaires sont ignorés par le parseur.

#### Exemples

```clad
# Ceci est un commentaire

###
Ceci est un commentaire
sur plusieurs lignes
###
```

---

## 12. Valeurs spéciales

```bnf
<valeur_spéciale> ::= "Neant"
                    | <erreur>

<erreur> ::= "Erreur:" <type_erreur>

<type_erreur> ::= "DivisionParZero"
                | "AccesHorsLimites"
                | "TypeInvalide"
                | <message_erreur>
```

### Exemples

```clad
Neant                       # Absence de valeur
Erreur: DivisionParZero     # Erreur de division
```

---

## Exemple complet annoté

Voici un programme complet analysé selon la grammaire :

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

**Décomposition syntaxique :**

1. `constante PI 3.14159` → `<déclaration_constante>`
2. `fonction aire_cercle(rayon) ... fin` → `<fonction>`
3. `principal ... fin` → `<principal>`
4. `afficher(...)` → `<instruction>` de type `<affichage>`
5. `pour i de 1 à 3 ... fin` → `<pour>`
6. `si i = 2 ... fin` → `<si>`

---

## Notes d'implémentation

### Indentation significative

Bien que non exprimée directement dans la BNF formelle, l'indentation est utilisée pour améliorer la lisibilité. Les blocs sont délimités par les mots-clés de début (`fonction`, `si`, `tantque`, `pour`, `principal`) et le mot-clé `fin`.

### Sensibilité à la casse

Les mots-clés sont sensibles à la casse :
- ✓ `si`, `fonction`, `principal`
- ✖ `Si`, `Fonction`, `Principal`

### Espaces blancs

Les espaces, tabulations et retours à la ligne sont des séparateurs mais n'affectent pas la sémantique (sauf pour délimiter les tokens).

---

## Résumé des mots-clés

| Catégorie | Mots-clés |
|-----------|-----------|
| **Déclarations** | `constante`, `variable` |
| **Fonctions** | `fonction`, `fin`, `retourner`, `lambda` |
| **Contrôle** | `si`, `sinon`, `tantque`, `pour`, `de`, `à` |
| **I/O** | `afficher` |
| **Booléens** | `vrai`, `faux` |
| **Logique** | `et`, `ou`, `non` |
| **Arithmétique** | `div`, `mod`, `abs`, `arrondi` |
| **Listes** | `Liste`, `Premier`, `reste`, `ajout`, `map`, `filtre`, `réduis` |
| **Chaînes** | `phr-long`, `phr-ajout`, `phr-coupe`, `phr-cherche?` |
| **Spécial** | `Neant`, `Erreur`, `principal` |

---

**Cette grammaire BNF constitue la spécification formelle complète du langage CLaD et servira de référence pour l'implémentation du parseur.**

:::tip Conformité
Tout programme respectant cette grammaire est considéré comme syntaxiquement valide et peut être traité par l'interpréteur GLaDOS.
:::