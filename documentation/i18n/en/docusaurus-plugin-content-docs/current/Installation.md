---
id: Installation
title: Installation
sidebar_position: 2
---

# Installation

Welcome to the GLaDOS (Generic Language and Data Operand Syntax) installation program.
Please follow these instructions with absolute precision.

## Prerequisites

Before starting, ensure that your test terminal is equipped with the following:

- **Stack** (Haskell Build Tool): To compile the system core.
- **Make**: To execute standard build protocols.
- **Git**: To retrieve test subject data.

## Installation Procedure

1. **Repository Cloning**

Retrieve the source code from the Aperture Science archives:

```bash
git clone [https://github.com/enzogagg/Glados.git](https://github.com/enzogagg/Glados.git)
cd Glados
```

2. **Environment Configuration**

Initialize security protocols (hooks):

```bash
./scripts/install-hooks.sh
```

3. **Compilation**

Compile the GLaDOS binary. This step may take some time; use this opportunity to reflect on your past mistakes.

```bash
make
```

*Note: If compilation fails, it is probably your fault.*

## Verification

To verify that GLaDOS is operational, launch the functional test suite:

```bash
make tests_run

If all tests pass (green), you are ready to begin enrichment. If tests fail (red), please do not touch the equipment and wait for an associate to arrive.
Launch

You can now launch the interpreter in interactive mode:
Bash

./glados

Or execute a script file:
Bash

./glados path/to/your/script.scm

:::info Note The binary is located at the project root after compilation. :::

