---
id: standard-io
title: Entrées / Sorties Standard
sidebar_position: 2
---

# Entrées / Sorties Standard

Ces fonctions permettent d'interagir avec la console (terminal) pour afficher des informations ou récupérer des saisies utilisateur.

## `afficher(expression)`

*(Alias : `print`)*

Affiche la représentation textuelle d'une expression sur la sortie standard, suivie d'un saut de ligne.

- **Signature** : `afficher(Tout) -> Vide`
- **Exemple** :
  ```clad
  afficher("Hello World")
  afficher(42)
  
  variable liste [1, 2, 3]
  afficher(liste)
  ```

## `entree(message)`

*(Alias : `input`)*

Lit une ligne de texte depuis l'entrée standard (clavier).

- **Signature** : `entree(Chaîne) -> Chaîne`
- **Exemple** :
  ```clad
  variable nom "Inconnu"
  nom = entree("Quel est votre nom ? ")
  afficher("Bonjour " + nom)
  ```
