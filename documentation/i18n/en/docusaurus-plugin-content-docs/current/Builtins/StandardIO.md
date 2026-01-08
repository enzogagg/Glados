---
id: standard-io
title: Standard Input / Output
sidebar_position: 2
---

# Standard Input / Output

These functions allow interaction with the console (terminal) to display information or retrieve user input.

## `afficher(expression)`

*(Alias: `print`)*

Displays the textual representation of an expression on standard output, followed by a newline.

- **Signature**: `afficher(Any) -> Void`
- **Example**:
  ```clad
  afficher("Hello World")
  afficher(42)
  
  variable list [1, 2, 3]
  afficher(list)
  ```

## `entree(message)`

*(Alias: `input`)*

Reads a line of text from standard input (keyboard).

- **Signature**: `entree(String) -> String`
- **Example**:
  ```clad
  variable name "Unknown"
  name = entree("What is your name? ")
  afficher("Hello " + name)
  ```
