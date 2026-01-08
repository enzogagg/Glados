---
id: Tests
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
