---
id: Tests
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

