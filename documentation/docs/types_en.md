---
id: types-and-keywords
title: Types and Keywords
sidebar_position: 2
---

# Types and Keywords with LISP-like Examples

## Basic Types

**Boolean — pileouface**: represents a binary state (1 = heads, 0 = tails).  
Example: `(def result 1)`; if `result` is 1, it's heads, otherwise it's tails.

**String — phrase**: a sequence of characters.  
Example: `(def message "Hello GLaDOS")`; the variable `message` stores text.

**Int — chiffre**: an integer number.  
Example: `(def counter 42)`; the variable `counter` holds an integer value.

**Float — flottant**: a decimal number.  
Example: `(def temperature 3.14)`; `temperature` contains a floating-point value.

**Char — car**: a single character.  
Example: `(def letter #\G)`; the variable `letter` holds the character `G`.

**Define — définis**: defines a constant or macro.  
Example: `(define MAX_LIVES 3)`; declares a constant named `MAX_LIVES`.

**Void — vide**: represents a function that returns no value.  
Example:  
`(defun show-message () (print "Message with no return value"))`; a function that only prints a message.

## Conditional Commands

**If — si**: evaluates a condition.  
Example: `(if (= result 1) (print "Heads") (print "Tails"))`

**Else — sinon**: alternate branch when the condition is false.  
Example:  (if (> score 100)
            (print "Level up!")
            sinon (print "Keep going"))



**While — pendant**: repeats as long as the condition is true.  
Example:  
`(while (> energy 0) (progn (dec energy) (print energy)))`

**For — pour**: controlled loop.  
Example:  
`(for (def i 0) (< i 5) (inc i) (print i))`

**Return — renvoie**: returns a value from a function.  
Example:  
`(defun maxScore (a b) (return (if (> a b) a b)))`

**Break — coupe**: exits a loop prematurely.  
Example:  
`(while (< i 10) (if error (break)))`

## Operators

**+**: addition. Example: `(+ x y)`  
**-**: subtraction. Example: `(- x y)`  
**/**: division. Example: `(/ x y)`  

**= — vaut**: assignment.  
Example:  
`(- 2 3) vaut 1`

