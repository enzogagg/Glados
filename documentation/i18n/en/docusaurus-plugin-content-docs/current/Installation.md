---
id: Installation
title: Installation Guide
sidebar_position: 2
---

# Installation Guide

This guide explains how to configure the CLaD environment and compile the necessary tools (Compiler and Virtual Machine).

## Prerequisites

Ensure you have the following tools installed on your machine (Linux/macOS):

- **Git**: For versioning.
- **Stack**: The Haskell Build Tool (`curl -sSL https://get.haskellstack.org/ | sh`).
- **Make**: For automation.
- **GCC/Clang**: For low-level system dependencies.

## Installation

### 1. Clone the Repository

```bash
git clone https://github.com/enzogagg/Glados.git
cd Glados
```

### 2. Compile the CLaD Suite

The project uses a root `Makefile` to orchestrate the compilation of the compiler and the VM.

```bash
make
```

This command will perform the following actions:
1. Compile the Haskell compiler (`glados-compiler`).
2. Compile the Virtual Machine (`glados-vm`).
3. Copy the executables to the project root.

**Note:** The first compilation may take some time as Stack needs to download and compile dependencies (GHC, libraries...).

### 3. Verify Installation

Once compilation is complete, you should see two executables at the root:

- `glados-compiler`: The compiler (Source `.clad` -> Bytecode `.cbc`).
- `glados-vm`: The virtual machine to execute bytecode.

Verify their presence:

```bash
ls -l glados-compiler glados-vm
```

## Troubleshooting

**Error "Command not found: stack"**:
Ensure Stack is in your PATH. Add `export PATH=$PATH:~/.local/bin` to your shell configuration file (`.zshrc` or `.bashrc`).

**Error during GHC build**:
Check that you have the necessary system libraries (like `libgmp-dev` or `libtinfo-dev` on Linux).
