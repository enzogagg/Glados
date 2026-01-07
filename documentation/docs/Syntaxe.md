---
id: syntaxe
title: Manuel de Syntaxe
sidebar_position: 2
---

# Manuel de Syntaxe CLaD

Ce document est la référence complète de la syntaxe du langage CLaD (Generic Language and Data Operand Syntax). CLaD est un langage impératif structuré utilisant une syntaxe francophone claire.

## 1. Structure d'un Programme

Un programme CLaD est composé de définitions (constantes, variables globales, fonctions) et d'un bloc principal obligatoire.

### Le Bloc Principal (`principal`)
C'est le point d'entrée exécuté au lancement du programme. Il doit être unique.

```clad
principal
    afficher("Bonjour Monde !")
fin
```

### Commentaires
- Ligne : `# Ceci est un commentaire`
- Bloc : `### Bloc de commentaire ###` (non standard, préférez `#` sur plusieurs lignes pour l'instant).

---

## 2. Variables et Constantes

### Constantes
Déclarées avec le mot-clé `constante`. Elles ne peuvent pas être modifiées.

```clad
constante PI 3.14159
constante NOM_APP "CLaD v1.0"
```

### Variables
Déclarées avec `variable`.

```clad
variable compteur 0
variable message "Initial"

compteur = compteur + 1
```

---

## 3. Types de Données

Les mots-clés de type (utilisés dans les futures versions typées ou pour la documentation) :
- `entier` : Nombres entiers (ex: `42`, `-10`).
- `flottant` : Nombres à virgule (ex: `3.14`).
- `phrase` : Chaînes de caractères (ex: `"Bonjour"`).
- `booleen` : `vrai` ou `faux`.

---

## 4. Structures de Contrôle

### Conditionnelle (`si` / `sinon`)

```clad
si score > 100
    afficher("Nouveau record !")
sinon si score > 50
    afficher("Bien joué")
sinon
    afficher("Essaie encore")
fin
```

### Boucle (`tantque`)

```clad
variable i 0
tantque i < 10
    afficher(i)
    i = i + 1
fin
```

### Boucle (`pour`)

```clad
pour i de 0 à 5
    afficher("Itération : " + i)
fin
```

---

## 5. Fonctions

Les fonctions permettent de structurer le code. Elles sont définies par `fonction` et terminées par `fin`.

### Déclaration

```clad
fonction addition(a, b)
    retourner a + b
fin

fonction dire_bonjour(nom)
    afficher("Salut " + nom)
fin
```

### Appel

```clad
resultat = addition(10, 20)
dire_bonjour("Alice")
```

---

## 6. Fonctions Anonymes (Avancé)

CLaD supporte la programmation fonctionnelle via des lambdas (syntaxe à confirmer selon implémentation, standard: `lambda`).

```clad
# Exemple théorique selon support compilateur
carre = lambda(x) (x * x)
```

*(Note : Référez-vous à la section Built-ins pour les fonctions natives comme `afficher`, `entree`...)*
