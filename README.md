# 🍰 GLaDOS (Generic Language and Data Operand Syntax)

> "The Enrichment Center promises to always provide a safe testing environment."

A minimalist LISP interpreter written in Haskell, developed for the Epitech **GLaDOS** project (Part 1 & 2). This repository features a strict CI/CD pipeline, automated documentation with Docusaurus, and enforced commit standards.

![CI Status](https://img.shields.io/github/actions/workflow/status/VOTRE_ORG/VOTRE_REPO/ci-orchestrator.yml?label=Pipeline&logo=github)
![Coverage](https://img.shields.io/badge/coverage-80%25-green)
![Haskell](https://img.shields.io/badge/language-Haskell-purple)
![Epitech](https://img.shields.io/badge/Epitech-Project-blue)

## 📋 Prerequisites

Ensure you have the following tools installed on your local machine:

* **Haskell Stack**: For building and testing the project.
* **Make**: For standard compilation rules.
* **Node.js & npm**: Required for Commitlint (git hooks) and Docusaurus.

## 🚀 Installation (Run this first!)

When cloning this repository for the first time, you **MUST** run the setup steps to configure your local environment and git hooks.

```bash
# 1. Install Node.js dependencies (for commit linting & docs)
npm install

# 2. Install Git Hooks (Pre-commit & Commit-msg)
./scripts/install-hooks.sh

# 3. Build the project to ensure everything is working
make