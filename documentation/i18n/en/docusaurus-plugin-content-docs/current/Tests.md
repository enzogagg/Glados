---
id: Tests
<<<<<<< HEAD
title: Test Protocols
sidebar_position: 4
---

# Test Protocols

The Enrichment Center places paramount importance on data validation. Any untested code will be considered a violation of safety protocols.

## Unit Tests

Unit tests verify the proper functioning of the individual components of the GLaDOS core (parsing, evaluation, types).

### Location
Test subjects are located in the `test/` sector:
- `ParseToExprSpec.hs`: Verifies the transformation of text into AST.
- `ParseValueSpec.hs`: Validates expression evaluation and primitives.
- `MainSpec.hs`: General entry point tests.

### Execution
To launch the unit test sequence via the terminal:

```bash
make tests_run

Or directly with Stack:
Bash

stack test

Adding a Test

Create or modify a *Spec.hs file in test/.

Use hspec syntax to define your assertions.

Ensure your test does not cause a time paradox.

Functional Tests

Functional tests validate the global behavior of the interpreter under real-world conditions.
Location

The control script is located at: scripts/functional_tests.sh.
Operation

The script:

Compiles the glados binary.

Injects instructions (via stdin or files).

Compares the standard output and return code with expected values.

Execution
Bash

./scripts/functional_tests.sh

Adding a Test

Open scripts/functional_tests.sh and add an assert_output line:
Bash

# assert_output "Test Name" "Input" "Expected Return Code" "Expected Output"
assert_output "Cake Test" "(eq? 'cake 'lie)" 0 "#t"

:::tip Supervisor Note Green code is happy code. Red code will result in the release of neurotoxin. :::

=======
title: Running Tests
sidebar_position: 10
---

# Running Tests

It is crucial to validate the correct functioning of the language, whether you are a core kernel developer or a user.

## 1. Project Unit Tests

These tests validate the internal functioning of the Compiler and the VM (written in Haskell).

### Run all tests
```bash
make tests_run
```
Or with Stack directly:
```bash
stack test
```

This will execute:
- Parser tests (verification of CLaD syntax).
- VM tests (verification of stack operations, memory management).

## 2. Functional Tests

Functional tests verify that the language "as a whole" behaves as expected. They compile reference CLaD programs and check their output.

### The Test Script
Use the script provided in `scripts/`.

```bash
./scripts/functional_tests.sh
```

### Functional Test Example

Functional tests look like this:

**File `test_fact.clad`**:
```clad
fonction factorielle(n)
    si n <= 1
        retourner 1
    sinon
        retourner n * factorielle(n - 1)
    fin
fin

principal
    afficher(factorielle(5))
fin
```

**Expected Execution**:
1. Compilation of `test_fact.clad` -> `test_fact.cbc`
2. Execution `./glados-vm test_fact.cbc`
3. Expected Output: `120`

## 3. Continuous Integration (CI)

The project has a CI pipeline (GitHub Actions) that automatically runs these tests on every Push. See the [CI/CD](Ci-cd.md) section for more details.
>>>>>>> dev
