---
id: Listes
title: Listes
sidebar_position: 5
---

# Protocoles de Manipulation des Listes

Les listes sont des structures fondamentales du CLaD.  
Toute tentative de manipulation incorrecte entraînera une perte irréversible de cohérence logique.  
Merci de coopérer.

---

## Primitives sur les Listes

Les primitives suivantes permettent d’inspecter, modifier et analyser des listes.

---

# Fonctions de manipulation de listes

## `tete(liste)`
Récupère le **premier élément** de la liste.
- Erreur si la liste est vide.
- Ne modifie pas la liste originale.

**Exemple :**
```lisp
(tete '(1 2 3))
=> 1
```

---

## `reste(liste)`
Récupère la **liste sans le premier élément**.
- Erreur si la liste est vide.
- Ne modifie pas la liste originale.

**Exemple :**
```lisp
(reste '(1 2 3))
=> (2 3)
```

---

## `est_vide(liste)`
Renvoie **vrai** si la liste est vide, **faux** sinon.

**Exemple :**
```lisp
(est_vide '())
=> vrai

(est_vide '(1 2))
=> faux
```

---

## `fusion(el, liste)`
Ajoute un **élément au début** de la liste.
- Retourne une nouvelle liste.
- Ne modifie pas la liste originale.

**Exemple :**
```lisp
(fusion 0 '(1 2 3))
=> (0 1 2 3)
```

---

## `ajouter(liste, element)`
Ajoute un **élément à la fin** de la liste.
- Retourne une nouvelle liste.
- Ne modifie pas la liste originale.

**Exemple :**
```lisp
(ajouter '(1 2 3) 4)
=> (1 2 3 4)
```

---

## `insere(liste, index, element)`
Insère un élément à une **position donnée**.
- Erreur si l'index est hors limites.
- Retourne une nouvelle liste.

**Exemple :**
```lisp
(insere '(1 2 4) 2 3)
=> (1 2 3 4)
```

---

## `taille(liste)`
Renvoie le **nombre d'éléments** dans la liste.
- Retourne 0 pour une liste vide.

**Exemple :**
```lisp
(taille '(1 2 3))
=> 3

(taille '())
=> 0
```

---

## `supprimer(liste, index)`
Enlève l'élément à l'**index X**.
- Erreur si l'index est hors limites.
- Retourne une nouvelle liste.

**Exemple :**
```lisp
(supprimer '(1 2 3 4) 2)
=> (1 2 4)
```

---

## `nieme(liste, index)`
Récupère l'élément à l'**index donné**.
- Erreur si l'index est hors limites.
- Équivalent de `liste[index]`.

**Exemple :**
```lisp
(nieme '(a b c d) 2)
=> c
```

---

## `contient(liste, element)`
Renvoie **vrai** si l'élément est dans la liste, **faux** sinon.

**Exemple :**
```lisp
(contient '(1 2 3) 2)
=> vrai

(contient '(1 2 3) 5)
=> faux
```

---

## `vider(liste)`
Efface **tous les éléments** de la liste.

**Exemple :**
```lisp
(vider '(1 2 3))
=> ()
```
