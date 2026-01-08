---
id: tutoriel
title: "Tutorial: Your First Program"
sidebar_position: 4
---

# Tutorial: Your First CLaD Program

This tutorial will guide you step-by-step in writing a complete program in CLaD. We will develop a small **"Guess the Number"** game.

## Objective

The program must:
1. Define a secret number.
2. Ask the user to guess.
3. Say if it is "Higher" or "Lower".
4. Stop when found.

*(Note: As complex random number generation is not yet native, we will use a constant for the secret)*.

## Step 1: Basic Structure

Create a file `guess.clad`. Let's start with the main block.

```clad
principal
    afficher("Welcome to The Price is Right (CLaD Version)!")
fin
```

## Step 2: Variables and Loop

Add the game logic.

```clad
principal
    print("--- Guessing Game ---")
    
    variable secret 42
    variable found faux
    variable attempt 0
    
    tantque found == faux
        # Logic coming soon...
        found = vrai # To avoid infinite loop testing
    fin
fin
```

## Step 3: User Interaction

Let's use `entree` (or `input`) to get the player's choice. Note that `input` returns a string.

```clad
principal
    variable secret 42
    variable running vrai
    
    tantque running
        afficher("Enter a number:")
        variable user_input input()
        
        afficher("You entered: " + user_input)
        running = faux
    fin
fin
```

## Step 4: Complete Code (Advanced Example)

Here is what a structured program with functions looks like:

```clad
fonction check(value, target)
    si value < target
        afficher("It's higher!")
        retourner faux
    sinon si value > target
        afficher("It's lower!")
        retourner faux
    sinon
        afficher("You won!")
        retourner vrai
    fin
fin

principal
    variable target 42
    variable won faux
    variable attempt 0
    
    tantque won == faux
        # Game loop simulation
        attempt = attempt + 10
        
        afficher("Try with " + attempt)
        won = check(attempt, target)
        
        # Safety for example
        si attempt > 50
            won = vrai
            afficher("Giving up...")
        fin
    fin
fin
```

## Compile and Test

1. Compile: `./glados-compiler guess.clad`
2. Execute: `./glados-vm out.cbc`

Congratulations! You have written your first algorithm in CLaD.
