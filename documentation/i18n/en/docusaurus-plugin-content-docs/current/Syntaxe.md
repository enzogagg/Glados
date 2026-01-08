---
id: syntaxe
title: Syntax Manual
sidebar_position: 2
---

# CLaD Syntax Manual

This document is the complete reference for the CLaD (Generic Language and Data Operand Syntax) language syntax. CLaD is a structured imperative language using distinct French keywords.

## 1. Program Structure

A CLaD program is composed of definitions (constants, global variables, functions) and a mandatory main block.

### The Main Block (`principal`)
This is the entry point executed when the program launches. It must be unique.

```clad
principal
    afficher("Bonjour Monde !")
fin
```

### Comments
- Line: `# This is a comment`
- Block: `### Comment block ###`

---

## 2. Variables and Constants

### Constants
Declared with the `constante` keyword. They cannot be modified.

```clad
constante PI 3.14159
constante APP_NAME "CLaD v1.0"
```

### Variables
Declared with `variable`.

```clad
variable counter 0
variable message "Initial"

counter = counter + 1
```

---

## 3. Data Types

Type keywords (used in documentation or future typed versions):
- `entier`: Integers (e.g., `42`, `-10`).
- `flottant`: Floats (e.g., `3.14`).
- `phrase`: Strings (e.g., `"Hello"`).
- `booleen`: `vrai` (true) or `faux` (false).

---

## 4. Control Structures

### Conditional (`si` / `sinon`)

```clad
si score > 100
    afficher("New record!")
sinon si score > 50
    afficher("Good game")
sinon
    afficher("Try again")
fin
```

### While Loop (`tantque`)

```clad
variable i 0
tantque i < 10
    afficher(i)
    i = i + 1
fin
```

### For Loop (`pour`)

```clad
pour i de 0 à 5
    afficher("Iteration: " + i)
fin
```

---

## 5. Functions

Functions help structure the code. They are defined by `fonction` and ended by `fin`.

### Declaration

```clad
fonction addition(a, b)
    retourner a + b
fin

fonction say_hello(name)
    afficher("Hello " + name)
fin
```

### Call

```clad
result = addition(10, 20)
say_hello("Alice")
```

---

## 6. Anonymous Functions (Advanced)

CLaD supports functional programming via lambdas.

```clad
# Theoretical example depending on compiler support
square = lambda(x) (x * x)
```

*(Note: Refer to the Built-ins section for native functions like `afficher`, `entree`...)*
