---

id: Types
title: Types
sidebar_position: 3
-------------------

# **CLaD – Basic Types**

This document describes the set of **fundamental types** of the **CLaD** language, their native operations, and usage examples.

---

## 1. Numbers

Numbers are divided into two categories:

* **Integers** (`Entiers`)
* **Floating-point numbers** (`Virgule`)

### Native Functions

* `a + b` — Addition
* `a - b` — Subtraction
* `a * b` — Multiplication
* `a / b` — Floating-point division
* `a div b` — Integer division
* `a mod b` — Modulo
* `abs x` — Absolute value
* `arrondi x` — Rounding

### Examples

```clad
(2 + 3)        ;; => 5
(7 / 2)        ;; => 3.5
(7 div 2)      ;; => 3
(7 mod 2)      ;; => 1
(abs -42)      ;; => 42
(arrondi 3.14) ;; => 3
```

---

## 2. Strings (`Phrase`)

Strings allow text manipulation — useful, for instance, when threatening test subjects.

### Native Functions

* `phr-long s` — String length
* `phr-ajout a b` — Concatenation
* `phr-coupe s sep` — Split
* `phr-cherche? s sub` — Substring search

### Examples

```clad
(phr-long "CLaD")                    ;; => 6
(phr-ajout "Hello, " "Subject 17") ;; => "Hello, Subject 17"
(phr-coupe "a,b,c" ",")            ;; => ["a" "b" "c"]
(phr-cherche? "neurotoxin" "tox")  ;; => true
```

---

## 3. Booleans (`PileOuFace`)

Two possible values:

* `true`
* `false`

### Native Functions

* `a et b` — Logical AND
* `a ou b` — Logical OR
* `non a` — Negation
* `=` — Equality
* `>` `<` `>=` `<=` — Comparisons

### Examples

```clad
(true et false) ;; => false
(true ou false) ;; => true
(non true)      ;; => false
(= 3 3)         ;; => true
(> 5 2)         ;; => true
```

---

## 4. Lists (`Liste`)

Lists are ordered collections of elements, often used to store test results or subject data.

### Native Functions

* `Liste a b c ...` — Create a list
* `Premier lst` — First element
* `reste lst` — Rest of the list
* `ajout lst1 lst2` — List concatenation
* `map fn lst` — Function application
* `filtre fn lst` — Filtering
* `réduis fn init lst` — Reduction

### Examples

```clad
(Liste 1 2 3)                         ;; => [1 2 3]
(Premier [1 2 3])                     ;; => 1
(reste [1 2 3])                       ;; => [2 3]
(ajout [1 2] [3 4])                   ;; => [1 2 3 4]
(map (lambda (x) (* x 2)) [1 2 3])    ;; => [2 4 6]
(filtre (lambda (x) (> x 2)) [1 2 3]) ;; => [3]
(réduis + 0 [1 2 3])                  ;; => 6
```

---

## 5. The `Neant` Type

A special type indicating the absence of a value. Comparable to `null`, `none`, or `void` in other languages.

### Example

```clad
(print "Test in progress...") ;; => :Neant
```

---

## 6. The `Erreur` Type

All internal exceptions (division by zero, out-of-bounds access, turret overload, etc.) return an object of type `Erreur`.

### Example

```clad
(/ 1 0) ;; => Erreur: DivisionParZero
```
---

## Conclusion

This section details all the fundamental types available in the **CLaD** language. They form the essential building blocks for writing reliable, reproducible, and scientifically cruel programs.

**Happy experimenting.**
