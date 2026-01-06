---
id: Ci-cd
title: CI/CD
sidebar_position: 5
---

# CI/CD

The GLaDOS project uses a Continuous Integration and Continuous Deployment (CI/CD) pipeline based on **GitHub Actions** to ensure code quality and automate deliveries.

## Pipeline Architecture

The pipeline is orchestrated by the `.github/workflows/ci-orchestrator.yml` file. It coordinates the execution of several specialized modules.

### 1. Linting (Code Quality)

This step is executed first to check code compliance and commit messages.

- **Commit Linting**: Checks that commit messages respect the convention (e.g., `feat:`, `fix:`, `docs:`).
- **Haskell Linting**: Analyzes source code with `hlint` to detect bad practices.

### 2. Build & Test

If linting passes, the project is compiled and tested.

- **Compilation**: Uses `make` to build the `glados` binary.
- **Cleanup Verification**: Ensures that `make clean` and `make fclean` work correctly.
- **Unit Tests**: Executed via `stack test` with coverage report.
- **Functional Tests**: Launches a suite of shell scripts (`scripts/functional_tests.sh`) to validate the interpreter's global behavior.

### 3. Deployment (Main Branch Only)

These steps are triggered only on a push to the `main` branch, and only if the previous steps succeeded.

#### Mirroring
The code is automatically pushed to the Epitech repository via `pixta-dev/repository-mirroring-action`.

#### Documentation
1. Generation of Haskell API documentation with `haddock`.
2. Construction of the user documentation site with **Docusaurus**.
3. Automatic deployment to **GitHub Pages**.

## Configuration

Workflows are defined in the `.github/workflows/` folder:

- `ci-orchestrator.yml`: Main entry point.
- `linting.yml`: Style checks.
- `build-and-test.yml`: Compilation and tests.
- `mirroring.yml`: Synchronization with Epitech.
- `documentation.yml`: Documentation generation and publication.

