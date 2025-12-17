
---
id: Bytecode
title: Bytecode Specification
sidebar_position: 7
---

# CLaD Bytecode Specification v1.0

---

## 1. General Architecture

### 1.1 Bytecode File Structure

The `.cbc` (CLaD ByteCode) file is structured into four main sections:

[ HEADER ] [ CONSTANT POOL ] [ FUNCTION TABLE ] [ INSTRUCTIONS ]

### 1.2 Virtual Machine Architecture

The CLaD virtual machine relies on a stack-based architecture with the following components:

* **Stack**: Main stack for expression evaluation.
* **Heap**: Management of dynamic structures (lists, strings, objects).
* **Global Table**: Table of global variables.
* **Call Stack**: Execution stack for managing function calls.

This architecture is inspired by modern virtual machines (JVM, BEAM) while remaining adapted to the needs of the CLaD language.

---

## 2. Type System

### 2.1 Type Tags

The bytecode uses a unified typing system based on one-byte tags:

| Tag (1 byte) | Type | Description |
| ------------ | ----------------- | ------------------------------ |
| 00 | Int | Signed Integer |
| 01 | Float | Floating-point number |
| 02 | Bool | Boolean |
| 03 | Char | Character |
| 04 | String | String |
| 05 | List | List |
| 06 | Symbol | Symbol |
| 07 | Nil | Null value |
| 08 | Function/Closure | Function or closure |
| 09 | Object | Object (future extension) |

### 2.2 Value Format

Each value on the stack respects the following format:

[ TYPE_TAG ][ DATA... ]

### 2.3 Encoding Examples

00 00 00 00 2A = Int 42
01 40 28 00 00 = Float 2.5
02 01 = Bool true
04 00 00 00 05 'H' 'e' 'l' 'l' 'o' = String "Hello"
06 00 00 00 03 'f' 'o' 'o' = Symbol 'foo'

---

## 3. Instruction Set

### 3.1 Value Management

| Opcode | Instruction | Description |
| ------ | ---------------- | ---------------------------------------- |
| 01 | PUSH_CONST index | Pushes a constant from the pool |
| 02 | PUSH_INT value | Pushes an integer onto the stack |
| 03 | PUSH_FLOAT value | Pushes a float onto the stack |
| 04 | PUSH_BOOL value | Pushes a boolean onto the stack |
| 05 | PUSH_STRING | Pushes a string onto the stack |
| 06 | PUSH_NIL | Pushes the null value onto the stack |
| 07 | POP | Removes the value at the top of the stack|

**Note**: The constant pool allows for efficient storage of large values (strings, symbols, complex lists).

---

### 3.2 Arithmetic Operations

Arithmetic operations automatically support Int and Float types with implicit conversion.

| Opcode | Instruction | Description |
| ------ | ----------- | ---------------------------- |
| 10 | ADD | Addition |
| 11 | SUB | Subtraction |
| 12 | MUL | Multiplication |
| 13 | DIV | Division |
| 14 | MOD | Modulo |
| 15 | NEG | Negation |

---

### 3.3 Comparison Operations

| Opcode | Instruction | Description |
| ------ | ----------- | ------------------------ |
| 20 | EQ | Equality |
| 21 | NEQ | Inequality |
| 22 | LT | Strictly less than |
| 23 | GT | Strictly greater than |
| 24 | LTE | Less than or equal to |
| 25 | GTE | Greater than or equal to |

---

### 3.4 List Operations

| Opcode | Instruction | Description |
| ------ | ----------- | ---------------------------------------------- |
| 30 | CONS | Construct a pair (cons cell) |
| 31 | HEAD (CAR) | Extract the head of a list |
| 32 | TAIL (CDR) | Extract the tail of a list |
| 33 | LIST size | Create a list of n elements from the stack |
| 34 | LEN | Return the length of a list |

**Example**: `LIST 3` consumes the 3 values at the top of the stack to create `[a, b, c]`.

---

### 3.5 Symbols and Evaluation

| Opcode | Instruction | Description |
| ------ | ----------- | -------------------------------- |
| 40 | MAKE_SYMBOL | Creates a symbol |
| 41 | QUOTE | Quotes an expression |
| 42 | EVAL | Evaluates a quoted expression |

These instructions support deferred evaluation mechanisms typical of Lisp-like languages.

---

### 3.6 Variable Management

| Opcode | Instruction | Description |
| ------ | ---------------- | ---------------------------------------- |
| 50 | LOAD nameIndex | Loads the value of a variable |
| 51 | STORE nameIndex | Stores a value in a variable |
| 52 | DEFINE nameIndex | Defines a new variable |

**Note**: Variable identifiers are stored in the constant pool. The index refers to the position in this pool.

---

### 3.7 Control Flow

| Opcode | Instruction | Description |
| ------ | -------------------- | ----------------------------------------------- |
| 60 | JMP address | Unconditional jump to the specified address |
| 61 | JMP_IF_TRUE address | Conditional jump if the top value is true |
| 62 | JMP_IF_FALSE address | Conditional jump if the top value is false |

---

### 3.8 Function Management

| Opcode | Instruction | Description |
| ------ | ----------------------- | -------------------------------------------- |
| 70 | CALL funcIndex argCount | Call function with n arguments |
| 71 | RETURN | Return from function |
| 72 | CLOSURE funcIndex | Create a closure |
| 73 | LOAD_ARG index | Load a function argument |

**Function Table**: A table associates each function index with its address in the bytecode:

Function 0 → address 200
Function 1 → address 350

---

### 3.9 Input/Output

| Opcode | Instruction | Description |
| ------ | ----------- | -------------------------------- |
| 80 | PRINT | Prints the top value |
| 81 | INPUT | Reads user input |

---

### 3.10 Termination

| Opcode | Instruction | Description |
| ------ | ----------- | -------------------------- |
| FF | HALT | Stops VM execution |

---

## 4. VM Characteristics

### 4.1 Stack-Based Architecture

The stack-based architecture offers several advantages:

* Simplicity of design and implementation.
* Independent and modular instructions.
* Ease of debugging (stack inspection).
* Compatibility with functional paradigms.

### 4.2 Multi-Paradigm Support

The CLaD VM is designed to support:

* **Imperative Programming**: variables, sequencing, loops.
* **Functional Programming**: first-class functions, closures.
* **Lisp-inspired Programming**: symbols, quote/eval, list manipulation.

### 4.3 Type Management

* Automatic Int → Float conversion in arithmetic operations.
* Dynamic typing with runtime checking.
* Support for heterogeneous data structures.

---

## 5. Execution Example

### 5.1 CLaD Source Code

```clad
list = [1, 2, 3]
print(head(list) + 10)

5.2 Assembly Bytecode Representation
Extrait de code

PUSH_INT 1
PUSH_INT 2
PUSH_INT 3
LIST 3
STORE "list"

LOAD "list"
HEAD
PUSH_INT 10
ADD
PRINT
HALT

5.3 Binary Encoding
Plaintext

02 00 00 00 01 ; PUSH_INT 1
02 00 00 00 02 ; PUSH_INT 2
02 00 00 00 03 ; PUSH_INT 3
33 03 ; LIST 3
51 00 ; STORE 0 ("list")

50 00 ; LOAD 0 ("list")
31 ; HEAD
02 00 00 00 0A ; PUSH_INT 10
10 ; ADD
80 ; PRINT
FF ; HALT

6. .cbc File Format
6.1 Structure Overview

To clearly visualize the .cbc file structure, here is a Mermaid diagram:
Extrait de code

graph TD
A[File .cbc] --> B(HEADER);
A --> C(CONSTANT POOL);
A --> D(FUNCTION TABLE);
A --> E(INSTRUCTIONS);

subgraph HEADER
B1(Magic Number : 4 bytes)
B2(Version : 2 bytes)
B3(Flags : 1 byte)
B4(Reserved : 3 bytes)
end

subgraph CONSTANT_POOL
C1(Count : 4 bytes)
C2[Entry 0] --> C2_1(Type Tag : 1 byte);
C2_1 --> C2_2(Length : 4 bytes);
C2_2 --> C2_3(Data : n bytes);
C3[Entry 1] --> C3_1(Type Tag : 1 byte);
C3_1 --> C3_2(Length : 4 bytes);
C3_2 --> C3_3(Data : n bytes);
end

subgraph FUNCTION_TABLE
D1(Count : 4 bytes)
D2[Function 0] --> D2_1(Index : 4 bytes);
D2_1 --> D2_2(Address : 4 bytes);
D2_2 --> D2_3(ArgCount : 1 byte);
D3[Function 1] --> D3_1(Index : 4 bytes);
D3_1 --> D3_2(Address : 4 bytes);
D3_2 --> D3_3(ArgCount : 1 byte);
end

subgraph INSTRUCTIONS
E1(Code Length : 4 bytes)
E2[Instruction 0] --> E2_1(Opcode : 1 byte);
E2_1 --> E2_2(Operands : variable);
E3[Instruction 1] --> E3_1(Opcode : 1 byte);
E3_1 --> E3_2(Operands : variable);
end

6.2 Header (10 bytes)
Offset Size Field Value / Description
0x00 4 Magic Number 0x43 0x42 0x43 0x00 ("CBC\0")
0x04 2 Version 0x01 0x00 (version 1.0)
0x06 1 Flags 0x00 (reserved for future use)
0x07 3 Reserved 0x00 0x00 0x00 (padding)

Hexadecimal Example:
Plaintext

43 42 43 00 | 01 00 | 00 | 00 00 00
Magic | Ver. |Flg | Reserved

6.3 Constant Pool

Structure:
Field Size Description
Count 4 bytes Total number of entries
Entry N variable Pool entry
Type Tag 1 byte Entry type (see section 2.1)
Length 4 bytes Data size
Data Length bytes Raw data

Entry Examples:

String "Hello":
Plaintext

04 | 00 00 00 05 | 48 65 6C 6C 6F
Type | Length | Data

Symbol 'x':
Plaintext

06 | 00 00 00 01 | 78
Type | Length | Data (ASCII 'x')

Int 42:
Plaintext

00 | 00 00 00 04 | 00 00 00 2A
Type | Length | Data (32-bit int)

6.4 Function Table

Structure:
Field Size Description
Count 4 bytes Number of functions
Function Entry N variable Table entry
Index 4 bytes Function ID
Address 4 bytes Address in bytecode
ArgCount 1 byte Number of arguments

Example:

Count: 2 functions

Function 0: factorial (2 args, @ 0x00C8)
Plaintext

00 00 00 00 | 00 00 00 C8 | 02
Index Address Args

Function 1: main (0 args, @ 0x0150)
Plaintext

00 00 00 01 | 00 00 01 50 | 00
Index Address Args

6.5 Instructions

Structure:
Field Size Description
Code Length 4 bytes Total code size
Instruction N variable Sequence of Opcode and Operands
Opcode 1 byte Instruction
Operands variable Instruction arguments

Encoding Example:

Instruction: PUSH_INT 42
Plaintext

02 | 00 00 00 2A
Opcode | Operand (4 bytes)

Instruction: ADD
Plaintext

10
Opcode (no operand)

Instruction: CALL 0 with 2 args
Plaintext

70 | 00 00 00 00 | 02
Opcode | Function Index | ArgCount

6.6 Complete .cbc File Example

Program: print(42)
Plaintext

43 42 43 00 01 00 00 00 00 00 ; HEADER (10 bytes)
00 00 00 00 ; CONSTANT POOL (Count: 0)
00 00 00 00 ; FUNCTION TABLE (Count: 0)
00 00 00 06 ; INSTRUCTIONS (Code Length: 6 bytes)
02 00 00 00 2A ; PUSH_INT 42
80 ; PRINT
FF ; HALT

Total size: 28 bytes
