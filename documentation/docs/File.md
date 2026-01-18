---
id: File
title: Fichiers
sidebar_position: 6
---

# Protocoles de Manipulation des Fichiers

Les fichiers permettent la persistance et l'échange de données dans le CLaD.  
Toute tentative de manipulation incorrecte entraînera une perte irréversible de cohérence logique.  
Merci de coopérer.

---

## Primitives sur les Fichiers

Les primitives suivantes permettent d'ouvrir, lire, écrire et fermer des fichiers.

---

# Fonctions de manipulation de fichiers

## `ouvert(fichier)`

Ouvre un **fichier** pour lecture ou écriture.
- Équivalent de `open` en Python.
- Erreur si le fichier n'existe pas (en mode lecture).
- Retourne un descripteur de fichier.

**Exemple :**
```lisp
(ouvert "donnees.txt")
=> <fichier: donnees.txt>
```

---

## `fermer(fichier)`

Ferme un **fichier** ouvert.
- Équivalent de `close` en Python.
- Libère les ressources associées au fichier.
- Erreur si le fichier n'est pas ouvert.

**Exemple :**
```lisp
(fermer fichier)
=> vrai
```

---

## `ecrire(fichier, "texte")`

Écrit du **texte** dans un fichier.
- Équivalent de `write` en Python.
- Le fichier doit être ouvert en mode écriture.
- Erreur si le fichier n'est pas ouvert ou en lecture seule.

**Exemple :**
```lisp
(ecrire fichier "Bonjour CLaD")
=> vrai
```

---

## `lire(fichier)`

Lit le **contenu** d'un fichier.
- Équivalent de `read` en Python.
- Retourne une phrase (chaîne de caractères).
- Le fichier doit être ouvert en mode lecture.
- Erreur si le fichier n'est pas ouvert.

**Exemple :**
```lisp
(lire fichier)
=> "Bonjour CLaD"
```

---

## Exemple d'utilisation complète

```lisp
; Ouvrir un fichier en écriture
(def mon_fichier (ouvert "test.txt"))

; Écrire dans le fichier
(ecrire mon_fichier "Ligne 1")
(ecrire mon_fichier "Ligne 2")

; Fermer le fichier
(fermer mon_fichier)

; Ouvrir le fichier en lecture
(def mon_fichier (ouvert "test.txt"))

; Lire le contenu
(lire mon_fichier)
=> "Ligne 1Ligne 2"

; Fermer le fichier
(fermer mon_fichier)
```