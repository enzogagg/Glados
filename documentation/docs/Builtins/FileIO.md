---
id: file-io
title: Gestion de Fichiers
sidebar_position: 1
---

# Gestion de Fichiers

Le langage fournit des fonctions natives pour manipuler les fichiers.

## `open(chemin, mode)`

Ouvre un fichier.

- **Arguments** :
  - `chemin` : Chaîne (ex: `"fichier.txt"`).
  - `mode` : Chaîne (`"r"` lecture, `"w"` écriture/écrasement, `"a"` ajout).
- **Retour** : Un handle de fichier (`Fichier`).

```clad
variable f open("test.txt", "w")
```

## `write(fichier, contenu)`

Écrit dans un fichier ouvert.

```clad
write(f, "Bonjour CLaD")
```

## `read(fichier)`

Lit tout le contenu d'un fichier.

```clad
variable f2 open("test.txt", "r")
variable contenu read(f2)
print(contenu)
```

## `close(fichier)`

Ferme le fichier (libère la ressource).

```clad
close(f)
close(f2)
```
