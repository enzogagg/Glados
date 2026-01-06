---
id: Types
title: Types
sidebar_position: 3
---

# **CLaD** Basic Types

This document describes the set of **fundamental types** of the CLaD language, their native operations, as well as usage examples.

---

## 1. Numbers

Numbers are divided into two categories:

* **Integers** (`Entiers`)
* **Floats** (`Virgule`)

### Native Functions

* `+ a b` — Addition
* `- a b` — Subtraction
* `* a b` — Multiplication
* `/ a b` — Floating point division
* `div a b` — Integer division
* `mod a b` — Modulo
* `abs x` — Absolute value
* `arrondi x` — Round

### Examples

```clad
(+ 2 3) ;; => 5
(/ 7 2) ;; => 3.5
(div 7 2) ;; => 3
(mod 7 2) ;; => 1
(abs -42) ;; => 42
(arrondi 3.14) ;; => 3

2. Strings (Phrase)

Strings allow for text manipulation – useful for threatening test subjects.
Native Functions

phr-long s — Length

phr-ajout a b — Concatenation

phr-coupe s sep — Split

phr-cherche? s sub — Search

Examples
Extrait de code

(phr-long "CLaD") ;; => 6
(phr-ajout "Bonjour, " "Sujet 17") ;; => "Bonjour, Sujet 17"
(phr-coupe "a,b,c" ",") ;; => ["a" "b" "c"]
(phr-cherche? "neurotoxine" "tox") ;; => #t

3. Booleans (PileouFace)

Two possible values:

vrai — true

faux — false

Native Functions

et a b

ou a b

non a

= — Equality

> < >= <= — Comparisons

Examples
Extrait de code

(and #t #f) ;; => #f
(or #t #f) ;; => #t
(not #t) ;; => #f
(= 3 3) ;; => #t
(> 5 2) ;; => #t

4. Lists (Liste)

Ordered collections of elements, often used to store test results or subject data.
Native Functions

Liste a b c ... — Creates a list

Premier lst — First element

rest lst — The rest

ajout lst1 lst2 — List concatenation

map fn lst — Application of a function

filtre fn lst — Filtering

réduis fn init lst — Reduction

Examples
Extrait de code

(list 1 2 3) ;; => [1 2 3]
(Premier [1 2 3]) ;; => 1
(rest [1 2 3]) ;; => [2 3]
(ajout [1 2] [3 4]) ;; => [1 2 3 4]
(map (lambda (x) (* x 2)) [1 2 3]) ;; => [2 4 6]
(filtre (lambda (x) (> x 2)) [1 2 3 4]) ;; => [3 4]
(réduis + 0 [1 2 3]) ;; => 6

5. The Neant Type

A special type indicating the absence of a value. Similar to null, none, or void in other languages.
Example
Extrait de code

(print "Test in progress...") ;; => returns :Neant

6. The Erreur Type

All internal exceptions (e.g., division by zero, out-of-bounds access, turret overload) return an object of type Erreur.
Example
Extrait de code

(/ 1 0) ;; => Erreur: DivisionParZero

Conclusion

This section details all the fundamental types available in the CLaD language. They constitute the essential blocks for writing reliable, reproducible, and scientifically cruel programs.

Happy experimenting.
