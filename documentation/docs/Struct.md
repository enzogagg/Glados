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

```glados
# Ceci est un commentaire

constante PI 3.14

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

Syntaxe déclarative, en français :

```clad
constante VITESSE_LUMIERE 299792458
```

## 3.2 Variables

Déclaration facultative, mais possible pour clarifier le type :

```clad
variable compteur 0
variable nom "CLaD"
```

---

# 4. Fonctions

## 4.1 Déclaration

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

Si aucune valeur n’est retournée, la fonction retourne `:unit` par défaut.

```clad
fonction log(msg)
    afficher(msg)
fin
```

---

# 5. Indentation et blocs

L’indentation est **significative**.
Chaque bloc commence implicitement après un mot‑clé (`fonction`, `si`, `tantque`, `principal`, etc.) et se termine par `fin`.

Exemple :

```clad
si x > 10
    afficher("Trop grand")
fin
```

---

# 6. Structures de contrôle

## 6.1 Conditionnel

```clad
si condition
    ...
sinon si autre
    ...
sinon
    ...
fin
```

## 6.2 Boucle "tant que"

```clad
tantque x < 10
    afficher(x)
    x = x + 1
fin
```

## 6.3 Boucle "pour"

Exemple:

```clad
pour i de 0 à 10
    afficher(i)
fin
```

---

# 7. Programme principal

Il s’agit du point d’entrée du programme.
Un fichier peut contenir plusieurs fonctions, mais un seul bloc `principal`.

```clad
principal
    afficher("Test en cours...")
fin
```

---

# 8. Exemple complet

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
