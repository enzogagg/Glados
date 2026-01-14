---
id: usage
title: Utilisation
sidebar_position: 3
---

# Guide d'Utilisation

CLaD est un langage compilé vers du bytecode, qui est ensuite interprété par une machine virtuelle. Le flux de travail se déroule donc en deux étapes.

## 1. Écrire le Code

Créez un fichier source avec l'extension `.clad`.

**Exemple : `hello.clad`**
```clad
principal
    afficher("Bonjour CLaD !")
fin
```

## 2. Compiler (`glados-compiler`)

Utilisez le compilateur pour transformer votre code source en fichier binaire (`.cbc`).

**Syntaxe :**
```bash
./glados-compiler [fichier_source]
```

**Exemple :**
```bash
./glados-compiler hello.clad
```
Cela générera un fichier `out.cbc` (ou le nom spécifié selon les options).

*(Note : Si le compilateur supporte des options comme `-o`, spécifiez-les ici. Par défaut, supposons `out.cbc`)*

## 3. Exécuter (`glados-vm`)

Lancez la machine virtuelle en lui passant le fichier bytecode généré.

**Syntaxe :**
```bash
./glados-vm [fichier_bytecode]
```

**Exemple :**
```bash
./glados-vm out.cbc
```
> Sortie : `Bonjour CLaD !`

## Résumé du Workflow

```bash
# 1. Édition
nano main.clad

# 2. Compilation
./glados-compiler main.clad

# 3. Exécution
./glados-vm out.cbc
```

## Options de Débogage

Si vous rencontrez des problèmes, la VM peut souvent fournir des détails sur l'erreur (pile vide, instruction inconnue...). Assurez-vous de lire les messages d'erreur standards.
