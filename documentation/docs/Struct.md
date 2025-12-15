---
id: Struct
title: Structure du code
sidebar_position: 6
---

# Structure du code en **CLaD**

Ce document présente l’organisation générale d’un programme écrit en CLad, un langage conçu pour rester clair, cohérent et lisible, tout en conservant une syntaxe minimaliste et entièrement en français.

---

# 1. Organisation générale d’un fichier

Un fichier CLaD peut contenir :

1. **Les déclarations** : constantes, variables globales, imports.
2. **Les fonctions** : blocs de code réutilisables.
3. **Le bloc principal** : le point d’entrée exécuté lorsqu’un fichier est lancé.

Exemple minimal :

Ce petit programme montre comment déclarer une constante, définir une fonction simple et appeler cette fonction depuis le bloc principal.

```glados
# Ceci est un commentaire

constante flottant PI = 3.14

fonction Bonjour(nom)
    afficher("Bonjour, " + nom)
fin

principal
    Bonjour("A tous !")
fin
```

---

# 2. Commentaires

Les commentaires commencent par `#` et vont jusqu’à la fin de la ligne.

```clad
# Programme de test
```

Les blocs de documentation multi‑ligne utilisent `###` :

```clad
###
Ce test ne devrait PAS exploser.
Normalement.
###
```

---

# 3. Déclarations

## 3.1 Constantes

Les constantes permettent de définir des valeurs fixes qui ne changent jamais au cours de l’exécution.

Syntaxe déclarative, en français :

```clad
constante entier VITESSE_LUMIERE = 299792458
```

## 3.2 Variables

Les variables servent à stocker des valeurs modifiables tout au long de l’exécution du programme.

Déclaration facultative, mais possible pour clarifier le type :

```clad
variable entier compteur = 0
variable phrase nom = "CLaD"
```

---

# 4. Fonctions

## 4.1 Déclaration

Les fonctions regroupent du code réutilisable afin d’éviter les répétitions et d'organiser la logique du programme.
Une fonction se déclare comme suit :

```clad
fonction NomDeLaFonction(param1, param2)
    # corps
fin
```

Les parenthèses sont obligatoires.
Les paramètres sont séparés par des virgules.

### Exemple

```clad
fonction addition(a, b)
    retourner a + b
fin
```

## 4.2 Valeurs de retour

Les fonctions peuvent renvoyer une valeur. Si aucune valeur n’est retournée, la fonction retourne `:unit` par défaut.

```clad
fonction log(msg)
    afficher(msg)
fin
```

---

# 5. Indentation et blocs

L’indentation structure la logique du code et délimite les blocs, rendant la lecture claire et intuitive.
Chaque bloc commence implicitement après un mot‑clé (`fonction`, `si`, `tantque`, `principal`, etc.) et se termine par `fin`.

Exemple :

```clad
si x > 10
    afficher("Trop grand")
fin
```

---

# 6. Structures de contrôle

Les conditions permettent d’exécuter différentes parties du code selon les situations rencontrées.

## 6.1 Conditionnel

```clad
si (condition)
    ...
sinon si (autre)
    ...
sinon
    ...
fin
```

## 6.2 Boucle "tant que"

La boucle tant que répète un bloc d’instructions tant que la condition est vraie.

```clad
tantque (x < 10)
    afficher(x)
    x = x + 1
fin
```

## 6.3 Boucle "pour"

La boucle pour permet d’itérer facilement sur une plage de valeurs.

Exemple:

```clad
pour (i = 0; 0 < 10; i = i + 1)
    afficher(i)
fin
```

---

# 7. Programme principal

Le bloc principal est le point d’entrée du programme : c’est ici que l’exécution commence.
Un fichier peut contenir plusieurs fonctions, mais un seul bloc `principal`.

```clad
principal
    afficher("Test en cours...")
fin
```

---

# 8. Exemple complet

Cet exemple regroupe toutes les notions vues précédemment dans un programme complet et fonctionnel.

```clad
### Programme de test CLaD ###

constante BASE 10

fonction puissance(x, p)
    resultat = 1
    pour i de 0 à p
        resultat = resultat * x
    fin
    retourner resultat
fin

principal
    afficher("Résultat : ")
    afficher(puissance(2, 8))
fin
```

---

# Conclusion

Cette structure permet d’écrire des programmes lisibles, cohérents et pleinement adaptés aux tests.

Vous pouvez maintenant définir vos modules, fonctions et expériences en toute sécurité.
