---
id: data-structures
title: Data Structures
sidebar_position: 3
---

# Data Structures

Advanced collection manipulation.

## Arrays (`Array`)

Mutable indexed list.

### Creation
```clad
variable tab [10, 20, 30]
```

### Read
```clad
print(tab[1])  # Prints 20
```

### Write
```clad
tab[1] = 99
print(tab)     # [10, 99, 30]
```

## Tuples (`Tuple`)

Fixed heterogeneous collection.

### Creation
```clad
variable t {1, "a", vrai}
```

### Read
Currently, access is done via extraction or native function.

## Maps (`Map`)

Key-Value pairs.

*(Feature currently stabilizing, see VM OpCodes)*
