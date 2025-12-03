---
id: Types
title: Types
sidebar_position: 3
---

# Types de base de **GLaDOS**

Ce document décrit l’ensemble des **types fondamentaux** du langage GLaDOS, leurs opérations natives, ainsi que des exemples d’utilisation..

---

## 1. Nombres

Les nombres sont divisés en deux catégories :

* **Entiers** (`Entiers`)
* **Flottants** (`Virgule`)

### Fonctions natives

* `+ a b` — Addition
* `- a b` — Soustraction
* `* a b` — Multiplication
* `/ a b` — Division flottante
* `div a b` — Division entière
* `mod a b` — Modulo
* `abs x` — Valeur absolue
* `arrondi x` — Arrondi

### Exemples

```glados
(+ 2 3)        ;; => 5
(/ 7 2)        ;; => 3.5
(div 7 2)      ;; => 3
(mod 7 2)      ;; => 1
(abs -42)      ;; => 42
(arrondi 3.14)   ;; => 3
```

---

## 2. Chaînes de caractères (`Phrase`)

Les chaînes permettent de manipuler du texte – utile pour menacer les sujets de test.

### Fonctions natives

* `phr-long s` — Longueur
* `phr-ajout a b` — Concaténation
* `phr-coupe s sep` — Découpe
* `phr-cherche? s sub` — Recherche

### Exemples

```glados
(phr-long "GLaDOS")         ;; => 6
(phr-ajout "Bonjour, " "Sujet 17")  ;; => "Bonjour, Sujet 17"
(phr-coupe "a,b,c" ",")        ;; => ["a" "b" "c"]
(phr-cherche? "neurotoxine" "tox") ;; => #t
```

---

## 3. Booléens (`PileouFace`)

Deux valeurs possibles :

* `vrai` — vrai
* `faux` — faux

### Fonctions natives

* `et a b`
* `ou a b`
* `non a`
* `=` — Égalité
* `>` `<` `>=` `<=` — Comparaisons

### Exemples

```glados
(and #t #f)        ;; => #f
(or #t #f)         ;; => #t
(not #t)           ;; => #f
(= 3 3)            ;; => #t
(> 5 2)            ;; => #t
```

---

## 4. Listes (`Liste`)

Collections ordonnées d’éléments, souvent utilisées pour stocker des résultats de tests ou des données de sujets.

### Fonctions natives

* `Liste a b c ...` — Crée une liste
* `Premier lst` — Premier élément
* `rest lst` — Le reste
* `ajout lst1 lst2` — Concaténation de listes
* `map fn lst` — Application d’une fonction
* `filtre fn lst` — Filtrage
* `réduis fn init lst` — Réduction

### Exemples

```glados
(list 1 2 3)            ;; => [1 2 3]
(Premier [1 2 3])         ;; => 1
(rest [1 2 3])          ;; => [2 3]
(ajout [1 2] [3 4])     ;; => [1 2 3 4]
(map (lambda (x) (* x 2)) [1 2 3])      ;; => [2 4 6]
(filtre (lambda (x) (> x 2)) [1 2 3 4]) ;; => [3 4]
(réduis + 0 [1 2 3])      ;; => 6
```

---

## 5. Le type `Neant`

Type spécial indiquant l'absence de valeur.
Similaire à `null`, `none` ou `void` dans d'autres langages.

### Exemple

```glados
(print "Test en cours...")  ;; => retourne :Neant
```

---

## 6. Le type `Erreur`

Toutes les exceptions internes (ex : division par zéro, accès hors liste, surcharge de tourelles) retournent un objet de type `Erreur`.

### Exemple

```glados
(/ 1 0)  ;; => Erreur: DivisionParZero
```

---

## Conclusion

Cette section détaille tous les types fondamentaux disponibles dans le langage GLaDOS. Ils constituent les blocs essentiels pour l’écriture de programmes fiables, reproductibles et scientifiquement cruels.

Bonne expérimentation.
