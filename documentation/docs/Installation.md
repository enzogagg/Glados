---
id: Installation
title: Installation
sidebar_position: 2
---

# Guide d'Installation

Ce guide vous explique comment configurer l'environnement CLaD et compiler les outils nécessaires (Compilateur et Machine Virtuelle).

## Prérequis

Assurez-vous d'avoir les outils suivants installés sur votre machine (Linux/macOS) :

- **Git** : Pour le versioning.
- **Stack** : L'outil de build Haskell (`curl -sSL https://get.haskellstack.org/ | sh`).
- **Make** : Pour l'automatisation.
- **GCC/Clang** : Pour les dépendances système bas niveau.

## Installation

### 1. Cloner le Projet

```bash
git clone https://github.com/enzogagg/Glados.git
cd Glados
```

### 2. Compiler la Suite CLaD

Le projet utilise un `Makefile` racine pour orchestrer la compilation du compilateur et de la VM.

```bash
make
```

Cette commande va effectuer les actions suivantes :
1. Compiler le compilateur Haskell (`glados-compiler`).
2. Compiler la Machine Virtuelle (`glados-vm`).
3. Copier les exécutables à la racine du projet.

**Note :** La première compilation peut être longue car Stack doit télécharger et compiler les dépendances (GHC, bibliothèques...).

### 3. Vérifier l'Installation

Une fois la compilation terminée, vous devriez voir deux exécutables à la racine :

- `glados-compiler` : Le compilateur (Source `.clad` -> Bytecode `.cbc`).
- `glados-vm` : La machine virtuelle pour exécuter le bytecode.

Vérifiez leur présence :

```bash
ls -l glados-compiler glados-vm
```

## Résolution de Problèmes

**Erreur "Command not found: stack"** :
Assurez-vous que Stack est dans votre PATH. Ajoutez `export PATH=$PATH:~/.local/bin` à votre fichier de configuration shell (`.zshrc` ou `.bashrc`).

**Erreur durant le build GHC** :
Vérifiez que vous avez les bibliothèques système nécessaires (comme `libgmp-dev` ou `libtinfo-dev` sur Linux).
