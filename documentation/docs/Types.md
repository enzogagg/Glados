---

id: Types
title: Types
sidebar_position: 3
-------------------

# **CLaD – Types de base**

Ce document décrit l’ensemble des **types fondamentaux** du langage **CLaD**, leurs opérations natives ainsi que des exemples d’utilisation.

---

## 1. Nombres

Les nombres sont divisés en deux catégories :

* **Entiers** (`Entiers`)
* **Flottants** (`Virgule`)

### Fonctions natives

* `a + b` — Addition
* `a - b` — Soustraction
* `a * b` — Multiplication
* `a / b` — Division flottante
* `a div b` — Division entière
* `a mod b` — Modulo
* `abs x` — Valeur absolue
* `arrondi x` — Arrondi

### Exemples

```clad
(2 + 3)        ;; => 5
(7 / 2)        ;; => 3.5
(7 div 2)      ;; => 3
(7 mod 2)      ;; => 1
(abs -42)      ;; => 42
(arrondi 3.14) ;; => 3
```

---

## 2. Chaînes de caractères (`Phrase`)

Les chaînes permettent la manipulation de texte — utiles, par exemple, pour menacer des sujets de test.

### Fonctions natives

* `phr-long s` — Longueur de la chaîne
* `phr-ajout a b` — Concaténation
* `phr-coupe s sep` — Découpage (split)
* `phr-cherche? s sub` — Recherche d’une sous-chaîne

### Exemples

```clad
(phr-long "CLaD")                    ;; => 6
(phr-ajout "Bonjour, " "Sujet 17") ;; => "Bonjour, Sujet 17"
(phr-coupe "a,b,c" ",")            ;; => ["a" "b" "c"]
(phr-cherche? "neurotoxine" "tox") ;; => vrai
```

---

## 3. Booléens (`PileOuFace`)

Deux valeurs possibles :

* `vrai`
* `faux`

### Fonctions natives

* `a et b` — ET logique
* `a ou b` — OU logique
* `non a` — Négation
* `=` — Égalité
* `>` `<` `>=` `<=` — Comparaisons

### Exemples

```clad
(vrai et faux) ;; => faux
(vrai ou faux) ;; => vrai
(non vrai)     ;; => faux
(= 3 3)        ;; => vrai
(> 5 2)        ;; => vrai
```

---

## 4. Listes (`Liste`)

Les listes sont des collections ordonnées d’éléments, souvent utilisées pour stocker des résultats de tests ou des données de sujets.

### Fonctions natives

* `Liste a b c ...` — Création d’une liste
* `Premier lst` — Premier élément
* `reste lst` — Reste de la liste
* `ajout lst1 lst2` — Concaténation de listes
* `map fn lst` — Application d’une fonction
* `filtre fn lst` — Filtrage
* `réduis fn init lst` — Réduction

### Exemples

```clad
(Liste 1 2 3)                         ;; => [1 2 3]
(Premier [1 2 3])                     ;; => 1
(reste [1 2 3])                       ;; => [2 3]
(ajout [1 2] [3 4])                   ;; => [1 2 3 4]
(map (lambda (x) (* x 2)) [1 2 3])    ;; => [2 4 6]
(filtre (lambda (x) (> x 2)) [1 2 3]) ;; => [3]
(réduis + 0 [1 2 3])                  ;; => 6
```

---

## 5. Le type `Neant`

Type spécial indiquant l’absence de valeur. Comparable à `null`, `none` ou `void` dans d’autres langages.

### Exemple

```clad
(print "Test en cours...") ;; => :Neant
```

---

## 6. Le type `Erreur`

Toutes les exceptions internes (division par zéro, accès hors limites, surcharge de tourelle, etc.) retournent un objet de type `Erreur`.

### Exemple

```clad
(/ 1 0) ;; => Erreur: DivisionParZero
```

---

## Conclusion

Cette section présente l’ensemble des types fondamentaux disponibles dans le langage **CLaD**. Ils constituent les briques essentielles pour écrire des programmes fiables, reproductibles et scientifiquement cruels.

**Bonne expérimentation.**
