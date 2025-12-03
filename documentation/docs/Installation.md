---
id: Installation
title: Installation
sidebar_position: 2
---

# Installation

Bienvenue dans le programme d'installation du GLaDOS (Generic Language and Data Operand Syntax).
Veuillez suivre ces instructions avec une précision absolue.

## Prérequis

Avant de commencer, assurez-vous que votre terminal de test dispose des éléments suivants :

- **Stack** (Haskell Build Tool) : Pour compiler le noyau du système.
- **Make** : Pour exécuter les protocoles de construction standard.
- **Git** : Pour récupérer les données du sujet de test.

## Procédure d'Installation

1.  **Clonage du Dépôt**

    Récupérez le code source depuis les archives d'Aperture Science :

    ```bash
    git clone https://github.com/enzogagg/Glados.git
    cd Glados
    ```

2.  **Configuration de l'Environnement**

    Initialisez les protocoles de sécurité (hooks) :

    ```bash
    ./scripts/install-hooks.sh
    ```

3.  **Compilation**

    Compilez le binaire GLaDOS. Cette étape peut prendre un certain temps, profitez-en pour réfléchir à vos erreurs passées.

    ```bash
    make
    ```

    *Note : Si la compilation échoue, c'est probablement de votre faute.*

## Vérification

Pour vérifier que GLaDOS est opérationnel, lancez la suite de tests fonctionnels :

```bash
make tests_run
```

Si tous les tests passent (vert), vous êtes prêt à commencer l'enrichissement.
Si des tests échouent (rouge), veuillez ne pas toucher au matériel et attendre l'arrivée d'un associé.

## Lancement

Vous pouvez maintenant lancer l'interpréteur en mode interactif :

```bash
./glados
```

Ou exécuter un fichier de script :

```bash
./glados chemin/vers/votre/script.scm
```

:::info Note
Le binaire se trouve à la racine du projet après la compilation.
:::
