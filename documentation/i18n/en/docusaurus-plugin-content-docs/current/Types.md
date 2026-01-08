---
id: types
title: Type System
sidebar_position: 3
---

# Data Types

Although CLaD is dynamically typed at runtime (in the virtual machine), it handles precise type concepts. Here are the types you will handle in your code.

## Primitive Types

| Type | Keyword (Doc) | Description | Example |
|---|---|---|---|
| **Integer** | `entier` | Signed 64-bit integer. | `42`, `-7` |
| **Float** | `flottant` | Double precision decimal number. | `3.14`, `-0.01` |
| **Boolean** | `booleen` | Truth value. | `vrai`, `faux` |
| **String** | `phrase` | Sequence of characters. | `"CLaD is great"` |
| **Symbol**| `symbole` | Unique identifier (rarely used directly). | `atom` |
| **Void** | `vide` / `unit` | Absence of value. | (Result of `afficher`) |

## Complex Structures

These types help organize data. See the [Data Structures](Builtins/DataStructures.md) section for detailed manipulation.

### List
Chained collection of elements.
- Creation: `(1 2 3)` (Via `cons` or quote).

### Array
Mutable indexed collection.
- Syntax: `[1, 2, 3]`
- Fast access by index `tab[0]`.

### Tuple
Fixed and immutable collection of heterogeneous values.
- Syntax: `{1, "A", vrai}`
- Ideal for returning multiple values from a function.

### Map
Key-Value association.

### Struct
Composite object with named fields.
