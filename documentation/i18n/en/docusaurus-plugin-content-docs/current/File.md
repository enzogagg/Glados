---
id: File
title: Files
sidebar_position: 6
---

# File Manipulation Protocols

Files enable data persistence and data exchange in CLaD.  
Any incorrect manipulation attempt will result in an irreversible loss of logical consistency.  
Thank you for your cooperation.

---

## File Primitives

The following primitives allow files to be opened, read, written, and closed.

---

# File Manipulation Functions

## `ouvert(fichier)`

Opens a **file** for reading or writing.
- Equivalent to `open` in Python.
- Error if the file does not exist (in read mode).
- Returns a file descriptor.

**Example:**
```lisp
(ouvert "donnees.txt")
=> <fichier: donnees.txt>

fermer(fichier)

Closes an open file.

    Equivalent to close in Python.

    Releases the resources associated with the file.

    Error if the file is not open.

Example:

(fermer fichier)
=> vrai

ecrire(fichier, "texte")

Writes text to a file.

    Equivalent to write in Python.

    The file must be opened in write mode.

    Error if the file is not open or is read-only.

Example:

(ecrire fichier "Bonjour CLaD")
=> vrai

lire(fichier)

Reads the contents of a file.

    Equivalent to read in Python.

    Returns a sentence (string).

    The file must be opened in read mode.

    Error if the file is not open.

Example:

(lire fichier)
=> "Bonjour CLaD"

Complete Usage Example

; Open a file in write mode
(def mon_fichier (ouvert "test.txt"))

; Write to the file
(ecrire mon_fichier "Ligne 1")
(ecrire mon_fichier "Ligne 2")

; Close the file
(fermer mon_fichier)

; Open the file in read mode
(def mon_fichier (ouvert "test.txt"))

; Read the contents
(lire mon_fichier)
=> "Ligne 1Ligne 2"

; Close the file
(fermer mon_fichier)
