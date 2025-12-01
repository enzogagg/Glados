---
id: types-et-mots-cles
title: Types et mots-clés
sidebar_position: 2
---

# Types et mots-clés avec exemples LISP-like

## Types de base

**Booléen — pileouface** : représente un état binaire (1 = pile, 0 = face).  
Exemple : `(def resultat 1)` ; si `resultat` vaut 1, c’est pile, sinon face.

**String — phrase** : chaîne de caractères.  
Exemple : `(def message "Bonjour GLaDOS")` ; variable `message` contient du texte.

**Int — chiffre** : nombre entier.  
Exemple : `(def compteur 42)` ; variable `compteur` contient un entier.

**Float — flottant** : nombre décimal.  
Exemple : `(def temperature 3.14)` ; variable `temperature` contient un flottant.

**Char — car** : caractère unique.  
Exemple : `(def lettre #\G)` ; variable `lettre` contient le caractère `G`.

**Define — définis** : constante ou macro.  
Exemple : `(définis MAX_VIES 3)` ; définit une constante `MAX_VIES`.

**Void — vide** : fonction ne renvoyant aucune valeur.  
Exemple : `(defun afficher-message () (print "Message sans retour"))` ; fonction qui affiche un message.

## Commandes conditionnelles

**If — si** : condition simple.  
Exemple : `(si (= resultat 1) (print "Pile") (print "Face"))`

**Else — sinon** : alternative à une condition.  
Exemple : `(si (> score 100) (print "Niveau supérieur") sinon
 (print "Continuez à jouer"))`

**While — pendant** : boucle tant que.  
Exemple : `(pendant (> energie 0) (progn (dec energie) (print energie)))`

**For — pour** : boucle contrôlée.  
Exemple : `(pour (def i 0) (< i 5) (inc i) (print i))`

**Return — renvoie** : renvoie une valeur.  
Exemple : `(defun scoreMax (a b) (renvoie (if (> a b) a b)))`

**Break — coupe** : sort d’une boucle.  
Exemple : `(pendant (< i 10) (si erreur (coupe)))`

## Opérateurs

**+** : addition. Exemple : `(+ x y)`  
**-** : soustraction. Exemple : `(- x y)`  
**/** : division. Exemple : `(/ x y)`  
**= — vaut** : assignation. Exemple : `(- 2 3) vaut 1`
