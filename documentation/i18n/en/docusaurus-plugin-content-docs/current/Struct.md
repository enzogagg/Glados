
---
id: Struct
title: Code Structure
sidebar_position: 6
---

# Code Structure in **CLaD**

This document presents the general organization of a program written in CLaD, a language designed to remain clear, consistent, and readable, while maintaining a minimalist syntax entirely in French.

---

# 1. General File Organization

A CLaD file can contain:

1. **Declarations**: constants, global variables, imports.
2. **Functions**: reusable code blocks.
3. **The main block**: the entry point executed when a file is launched.

Minimal example:

```glados
# This is a comment

constante PI 3.14

fonction Hello(name)
afficher("Hello, " + name)
fin

principal
Hello("Everyone!")
fin

2. Comments

Comments start with # and extend to the end of the line.
Extrait de code

# Test program

Multi-line documentation blocks use ###:
Extrait de code

###
This test should NOT explode.
Hopefully.
###

3. Declarations
3.1 Constants

Declarative syntax, in French:
Extrait de code

constante LIGHT_SPEED 299792458

3.2 Variables

Optional declaration, but possible to clarify the type:
Extrait de code

variable counter 0
variable name "CLaD"

4. Functions
4.1 Declaration

A function is declared as follows:
Extrait de code

fonction FunctionName(param1, param2)
# body
fin

Parentheses are mandatory. Parameters are separated by commas.
Example
Extrait de code

fonction addition(a, b)
retourner a + b
fin

4.2 Return Values

If no value is returned, the function returns :unit by default.
Extrait de code

fonction log(msg)
afficher(msg)
fin

5. Indentation and Blocks

Indentation is significant. Each block begins implicitly after a keyword (fonction, si, tantque, principal, etc.) and ends with fin.

Example:
Extrait de code

si x > 10
afficher("Too big")
fin

6. Control Structures
6.1 Conditional
Extrait de code

si condition
...
sinon si other
...
sinon
...
fin

6.2 "While" Loop
Extrait de code

tantque x < 10
afficher(x)
x = x + 1
fin

6.3 "For" Loop

Example:
Extrait de code

pour i de 0 à 10
afficher(i)
fin

7. Main Program

This is the entry point of the program. A file can contain multiple functions, but only one principal block.
Extrait de code

principal
afficher("Test in progress...")
fin

8. Complete Example
Extrait de code

### CLaD Test Program ###

constante BASE 10

fonction power(x, p)
result = 1
pour i de 0 à p
result = result * x
fin
retourner result
fin

principal
afficher("Result: ")
afficher(power(2, 8))
fin

Conclusion

This structure allows for writing readable, consistent programs fully adapted for testing.

You can now safely define your modules, functions, and experiments.

