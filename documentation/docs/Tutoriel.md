---
id: tutoriel
title: "Tutoriel : Votre Premier Programme"
sidebar_position: 4
---

# Tutoriel : Votre Premier Programme CLaD

Ce tutoriel va vous guider pas à pas dans l'écriture d'un programme complet en CLaD. Nous allons développer un petit jeu de **"Devine un nombre"** (Guess the Number).

## Objectif

Le programme doit :
1. Définir un nombre secret.
2. Demander à l'utilisateur de deviner.
3. Dire si c'est "Plus" ou "Moins".
4. S'arrêter quand c'est trouvé.

*(Note : Comme la génération de nombres aléatoires complexes n'est pas encore native, nous utiliserons une constante pour le secret)*.

## Étape 1 : Structure de base

Créez un fichier `devinette.clad`. Commençons par le bloc principal.

```clad
principal
    afficher("Bienvenue au Juste Prix (Version CLaD) !")
fin
```

## Étape 2 : Variables et Boucle

Ajoutons la logique de jeu.

```clad
principal
    print("--- Jeu de Devinette ---")
    
    variable secret 42
    variable trouve faux
    variable essai 0
    
    tantque trouve == faux
        # Logique à venir...
        trouve = vrai # Pour éviter une boucle infinie de test
    fin
fin
```

## Étape 3 : Interaction Utilisateur

Utilisons `entree` (ou `input`) pour récupérer le choix du joueur. Attention, `input` retourne une chaîne, il faut la comparer ou la convertir si nécessaire (ici on compare des chaînes ou on suppose une conversion implicite selon la version du VM).

```clad
principal
    variable secret 42
    variable running vrai
    
    tantque running
        afficher("Entrez un nombre :")
        variable user_input input()
        
        # Supposons que l'entrée est convertie ou gérée comme entier
        # Note : Dans la version actuelle, input retourne une string.
        # Il faudrait idéalement une fonction 'toInt(str)'. 
        # Pour ce tutoriel, simplifions en affichant juste le principe.
        
        afficher("Vous avez saisi : " + user_input)
        running = faux
    fin
fin
```

## Étape 4 : Code Complet (Exemple Avancé)

Voici à quoi ressemble un programme structuré avec des fonctions :

```clad
fonction check(valeur, cible)
    si valeur < cible
        afficher("C'est plus !")
        retourner faux
    sinon si valeur > cible
        afficher("C'est moins !")
        retourner faux
    sinon
        afficher("Gagné !")
        retourner vrai
    fin
fin

principal
    variable cible 42
    variable gagne faux
    variable tentative 0
    
    tantque gagne == faux
        # Simulation d'une boucle de jeu
        # Dans un vrai cas, on utiliserait input() et conversion
        tentative = tentative + 10
        
        afficher("Essai avec " + tentative)
        gagne = check(tentative, cible)
        
        # Sécurité pour l'exemple
        si tentative > 50
            gagne = vrai
            afficher("Abandon...")
        fin
    fin
fin
```

## Compilation et Test

1. Compilez : `./glados-compiler devinette.clad`
2. Exécutez : `./glados-vm out.cbc`

Bravo ! Vous avez écrit votre premier algorithme en CLaD.
