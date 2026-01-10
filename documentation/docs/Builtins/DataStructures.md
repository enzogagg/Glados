---
id: data-structures
title: Structures de Données
sidebar_position: 3
---

# Structures de Données

Manipulation des collections avancées.

## Tableaux (`Array`)

Liste mutable indexée.

### Création
```clad
variable tab [10, 20, 30]
```

### Lecture
```clad
print(tab[1])  # Affiche 20
```

### Écriture
```clad
tab[1] = 99
print(tab)     # [10, 99, 30]
```

## Tuples (`Tuple`)

Collection fixe hétérogène.

### Création
```clad
variable t {1, "a", vrai}
```

### Lecture (via built-in)
Actuellement, l'accès se fait via extraction ou fonction native (selon implémentation).
*(Syntaxe d'accès indexé `t[0]` à venir)*.

## Dictionnaires (`Map`)

Clé-Valeur.

*(Fonctionnalité en cours de stabilisation, voir OpCodes VM)*
