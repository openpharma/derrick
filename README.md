# crane.reporter

![GitHub forks](https://img.shields.io/github/forks/insightsengineering/crane.reporter?style=social)
![GitHub Repo stars](https://img.shields.io/github/stars/insightsengineering/crane.reporter?style=social)

![GitHub commit activity](https://img.shields.io/github/commit-activity/m/insightsengineering/crane.reporter)
![GitHub contributors](https://img.shields.io/github/contributors/insightsengineering/crane.reporter)
![GitHub last commit](https://img.shields.io/github/last-commit/insightsengineering/crane.reporter)
![GitHub pull requests](https://img.shields.io/github/issues-pr/insightsengineering/crane.reporter)
![GitHub repo size](https://img.shields.io/github/repo-size/insightsengineering/crane.reporter)
![GitHub language count](https://img.shields.io/github/languages/count/insightsengineering/crane.reporter)
[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Downloads](https://img.shields.io/github/downloads/insightsengineering/crane.reporter/latest/total)](https://tooomm.github.io/github-release-stats/?username=insightsengineering\&repository=crane.reporter)
[![Current Version](https://img.shields.io/github/r-package/v/insightsengineering/crane.reporter/main?color=purple\&label=package%20version)](https://github.com/insightsengineering/crane.reporter/tree/main)
[![Open Issues](https://img.shields.io/github/issues-raw/insightsengineering/crane.reporter?color=red\&label=open%20issues)](https://github.com/insightsengineering/crane.reporter/issues?q=is%3Aissue+is%3Aopen+sort%3Aupdated-desc)

[![Audit Dependencies](https://github.com/insightsengineering/crane.reporter/actions/workflows/audit.yaml/badge.svg)](https://github.com/insightsengineering/crane.reporter/actions/workflows/audit.yaml)
[![BiocCheck](https://github.com/insightsengineering/crane.reporter/actions/workflows/bioccheck.yaml/badge.svg)](https://github.com/insightsengineering/crane.reporter/actions/workflows/bioccheck.yaml)
[![Check URLs](https://github.com/insightsengineering/crane.reporter/actions/workflows/links.yaml/badge.svg)](https://github.com/insightsengineering/crane.reporter/actions/workflows/links.yaml)
[![Coverage](https://github.com/insightsengineering/crane.reporter/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/insightsengineering/crane.reporter/actions/workflows/test-coverage.yaml)
[![License report](https://github.com/insightsengineering/crane.reporter/actions/workflows/licenses.yaml/badge.svg)](https://github.com/insightsengineering/crane.reporter/actions/workflows/licenses.yaml)
[![Pkgdown Docs](https://github.com/insightsengineering/crane.reporter/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/insightsengineering/crane.reporter/actions/workflows/pkgdown.yaml)
[![R CMD Check](https://github.com/insightsengineering/crane.reporter/actions/workflows/build-check-install.yaml/badge.svg)](https://github.com/insightsengineering/crane.reporter/actions/workflows/build-check-install.yaml)
[![R Package Validation report](https://github.com/insightsengineering/crane.reporter/actions/workflows/validation.yaml/badge.svg)](https://github.com/insightsengineering/crane.reporter/actions/workflows/validation.yaml)
[![Release](https://github.com/insightsengineering/crane.reporter/actions/workflows/release.yaml/badge.svg)](https://github.com/insightsengineering/crane.reporter/actions/workflows/release.yaml)
[![Roxygen](https://github.com/insightsengineering/crane.reporter/actions/workflows/roxygen.yaml/badge.svg)](https://github.com/insightsengineering/crane.reporter/actions/workflows/roxygen.yaml)
[![Spelling](https://github.com/insightsengineering/crane.reporter/actions/workflows/spelling.yaml/badge.svg)](https://github.com/insightsengineering/crane.reporter/actions/workflows/spelling.yaml)
[![Style](https://github.com/insightsengineering/crane.reporter/actions/workflows/style.yaml/badge.svg)](https://github.com/insightsengineering/crane.reporter/actions/workflows/style.yaml)
[![SuperLinter](https://github.com/insightsengineering/crane.reporter/actions/workflows/linter.yaml/badge.svg)](https://github.com/insightsengineering/crane.reporter/actions/workflows/linter.yaml)
[![Version bump](https://github.com/insightsengineering/crane.reporter/actions/workflows/version-bump.yaml/badge.svg)](https://github.com/insightsengineering/crane.reporter/actions/workflows/version-bump.yaml)
[![Version check](https://github.com/insightsengineering/crane.reporter/actions/workflows/version.yaml/badge.svg)](https://github.com/insightsengineering/crane.reporter/actions/workflows/version.yaml)
[![gitleaks](https://github.com/insightsengineering/crane.reporter/actions/workflows/gitleaks.yaml/badge.svg)](https://github.com/insightsengineering/crane.reporter/actions/workflows/gitleaks.yaml)

<!-- links -->

[pre-commit]: https://pre-commit.com

[pre-commit installation]: https://pre-commit.com/#installation

[git hooks]: https://git-scm.com/book/en/v2/Customizing-Git-Git-Hooks

An R package template with built-in GitHub Actions-based CI/CD workflows.

## Usage

### Initialization

You could initialize this repository in one of two ways:

#### Clone this template

* Clone this repository:

  ```bash
  git clone https://github.com/insightsengineering/crane.reporter.git
  cd crane.reporter
  ```

* Run the initializer script:

  ```bash
  ./init.sh
  ```

#### Use GitHub's template importer

* Click [here](https://github.com/insightsengineering/crane.reporter/generate) to generate a copy of this template directly within GitHub.

* Clone the repository from your account/organization.

* Run the initializer script:

  ```bash
  ./init.sh
  ```

### CI/CD Configurations

All CI/CD jobs are defined in the [.github/workflows](./.github/workflows) directory in the form of GitHub Action workflows. These can be modified per your requirements, but are designed and implemented to follow best practices and to ensure the highest quality standards for your package.

All workflows originating from this repository can be repurposed by other R package GitHub repositories.

👉 For more information including detailed description and screenshots of workflows, please refer to the [Workflows documentation](./workflows.md).

### Pre-commit

This repository contains an example [pre-commit] configuration.

[pre-commit] is a tool that uses [Git hooks] to identify and resolve simple issues before submission for code review.
[Git hooks] run on every commit to automatically point out and solve issues such as missing semicolons, trailing whitespaces,
code formatting and spell checks.

### Setting up pre-commit for R project

* Install the `pre-commit` framework. Use the official [installation guide][pre-commit installation].

* Install R package `precommit`

  ```sh
  R -e 'install.packages("precommit")'
  ```

* Run the `use_precommit()` function to generate an example pre-commit configuration called `.pre-commit-config.yaml`:

  ```sh
  [ ! -f ".pre-commit-config.yaml" ] && R -e 'precommit::use_precommit()'
  ```

* Install the git hooks script:

  ```sh
  pre-commit install
  ```

* From this moment on, all scripts from `.pre-commit-config.yaml` will run before every `git commit` command. If you want to run them manually without committing you can use command `pre-commit run --all-files`. For more information, please refer to the official [pre-commit] documentation.

> NOTE: Frequently run `pre-commit autoupdate` to update all hooks in the `.pre-commit-config.yaml` configuration file.

### Example output from pre-commit

```sh
$ git add .
$ git commit -m "Add pre-commit configuration"
[INFO] Installing environment for https://github.com/pre-commit/pre-commit-hooks.
[INFO] Once installed this environment will be reused.
[INFO] This may take a few minutes...
style-files..............................................................Passed
roxygenize...........................................(no files to check)Skipped
use-tidy-description.....................................................Passed
spell-check..............................................................Failed
- hook id: spell-check
- exit code: 1
- files were modified by this hook

The following spelling errors were found:
  WORD          FOUND IN
commiting     README.md:77
indentify     README.md:49
informatoin   README.md:77
All spelling errors found were copied to inst/WORDLIST assuming they were not spelling errors and will be ignored in the future. Please  review the above list and for each word that is an actual typo:
 - fix it in the source code.
 - remove it again manually from inst/WORDLIST to make sure it's not
   ignored in the future.
 Then, try committing again.
Error: Spell check failed
Execution halted

lintr....................................................................Passed
readme-rmd-rendered......................................................Passed
parsable-R...............................................................Passed
no-browser-statement.....................................................Passed
deps-in-desc.............................................................Passed
prettier.................................................................Failed
- hook id: prettier
- files were modified by this hook

.pre-commit-config.yaml
README.md

Check for added large files..............................................Passed
Fix End of Files.........................................................Passed
Trim Trailing Whitespace.................................................Failed
- hook id: trailing-whitespace
- exit code: 1
- files were modified by this hook

Fixing .pre-commit-config.yaml
Fixing README.md

Check Yaml...............................................................Passed
Don't commit to branch...................................................Passed
Mixed line ending........................................................Passed
Don't commit common R artifacts......................(no files to check)Skipped
```

## Stargazers and Forkers

### Stargazers over time

[![Stargazers over time](https://starchart.cc/insightsengineering/crane.reporter.svg)](https://starchart.cc/insightsengineering/crane.reporter)

### Stargazers

[![Stargazers repo roster for @insightsengineering/crane.reporter](https://reporoster.com/stars/insightsengineering/crane.reporter)](https://github.com/insightsengineering/crane.reporter/stargazers)

### Forkers

[![Forkers repo roster for @insightsengineering/crane.reporter](https://reporoster.com/forks/insightsengineering/crane.reporter)](https://github.com/insightsengineering/crane.reporter/network/members)
