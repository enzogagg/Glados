---
id: Types
title: Types de Données
sidebar_position: 3
---

# Types de Données

GLaDOS supporte plusieurs types de données fondamentaux pour représenter l'information dans le Centre d'Enrichissement.

## Nombres

GLaDOS gère deux types de nombres :

- **Entiers (Integer)** : Nombres sans partie décimale.
  - Exemples : `42`, `-1`, `0`
- **Flottants (Float)** : Nombres à virgule flottante pour une précision accrue.
  - Exemples : `3.14`, `-0.001`, `2.0`

## Booléens

Les valeurs de vérité sont représentées par :

- `#t` : Vrai (True)
- `#f` : Faux (False)

## Symboles

Les symboles sont des identifiants uniques, souvent utilisés comme noms de variables ou comme étiquettes.

- Syntaxe : `'identifiant` ou simplement `identifiant` (si évalué).
- Exemples : `'x`, `'test-result`, `lambda`

## Chaînes de Caractères

Les chaînes de caractères sont des séquences de texte délimitées par des guillemets doubles.

- Syntaxe : `"texte"`
- Exemple : `"Le gâteau est un mensonge"`

## Listes

Les listes sont des collections ordonnées d'éléments, pouvant contenir n'importe quel type de données (y compris d'autres listes).

- Syntaxe : `(élément1 élément2 ...)`
- Construction : Via la fonction `list` ou `cons`.
- Exemple : `(1 2 "trois" #t)`

## Fonctions

Les fonctions sont des citoyens de première classe. Elles peuvent être :

- **Natives (Primitives)** : Intégrées au langage (ex: `+`, `car`).
- **Définies par l'utilisateur (Lambda)** : Créées avec le mot-clé `lambda`.
  - Syntaxe : `(lambda (paramètres) corps)`

---

:::warning Attention
Ne regardez pas directement le code opérationnel des fonctions primitives.
:::
