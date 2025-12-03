# GLaDOS CI/CD Pipeline

This directory contains the Continuous Integration and Continuous Deployment (CI/CD) configuration for the GLaDOS project. The pipeline is built using **GitHub Actions**.

## Overview

The pipeline is orchestrated by `workflows/ci-orchestrator.yml`, which manages the execution order and dependencies of modular workflows.

### Workflow Structure

The pipeline consists of the following stages:

1.  **Linting** (`workflows/linting.yml`)
    -   **Commit Linting**: Validates commit messages against conventional commit standards using `commitlint`.
    -   **Haskell Linting**: Checks code style and potential errors using `hlint`.

2.  **Build & Test** (`workflows/build-and-test.yml`)
    -   **Build**: Compiles the project using `make` and verifies the binary generation.
    -   **Clean Check**: Ensures `make clean` and `make fclean` work correctly.
    -   **Unit Tests**: Runs Haskell unit tests via `stack test` with coverage.
    -   **Functional Tests**: Executes shell-based functional tests (`scripts/functional_tests.sh`).

3.  **Mirroring** (`workflows/mirroring.yml`)
    -   *Condition*: Runs only on pushes to the `main` branch.
    -   **Action**: Mirrors the repository to the Epitech remote repository using `pixta-dev/repository-mirroring-action`.

4.  **Documentation** (`workflows/documentation.yml`)
    -   *Condition*: Runs only on pushes to the `main` branch.
    -   **API Docs**: Generates Haskell API documentation using `haddock`.
    -   **User Manual**: Builds the Docusaurus documentation site.
    -   **Deployment**: Deploys the combined documentation (User Manual + API) to GitHub Pages.

## Secrets

The following secrets must be configured in the repository settings for the pipeline to function correctly:

-   `MIRROR_URL`: The URL of the target Epitech repository.
-   `GIT_SSH_PRIVATE_KEY`: SSH private key for accessing the mirror repository.
-   `GITHUB_TOKEN`: Automatically provided by GitHub Actions (used for documentation deployment).

## Local Testing

You can run parts of the pipeline locally:

-   **Linting**: `hlint .`
-   **Build**: `make`
-   **Unit Tests**: `stack test`
-   **Functional Tests**: `./scripts/functional_tests.sh`
