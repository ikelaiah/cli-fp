# Pull Request: Release v1.3.3 - Stabilize Before Expanding

**Target Release:** v1.3.3

**Release Date:** 2026-08-13

## Summary

This PR delivers the focused `v1.3.3` roadmap milestone. It makes example
cleanup safe, replaces placeholder help tests with behavioural assertions,
accepts separated negative integer and float values, and keeps all seven
canonical examples buildable in Windows and Linux CI.

The release adds no public command API, parameter kind, generator capability,
or completion feature.

## Type of Change

- [x] Bug fix
- [x] Regression test
- [x] CI coverage
- [x] Documentation
- [ ] Breaking change

## Safe Example Cleanup

- [x] Restrict Bash and PowerShell cleanup scripts to generated compiler
  artifacts.
- [x] Preserve tracked completion scripts, documentation, and other
  repository files.
- [x] Add isolated Bash and PowerShell cleanup smoke checks.
- [x] Compile all seven canonical examples before each cleanup check.
- [x] Verify generated binaries are removed and tracked files remain unchanged.

## Help Coverage

- [x] Add an internal output-capture seam for framework tests.
- [x] Keep output capture out of normal builds and retain the public
  `ICLIApplication` API unchanged.
- [x] Replace placeholder tests with assertions for usage, descriptions,
  required options, defaults, complete help, and subcommands.

## Parser Correctness

- [x] Accept `--count -1` for registered integer options.
- [x] Accept `--rate -2.5` for registered float options.
- [x] Retain existing equals syntax such as `--count=-1`.
- [x] Confirm unknown options still fail validation and prevent command
  execution.

## CI, Documentation, and Versioning

- [x] Configure the example build-and-cleanup smoke check in Windows and Linux CI.
- [x] Run that CI check when runtime, example, cleanup-script, test, or
  workflow files change.
- [x] Update the README and user manual with negative-value behaviour and the
  cleanup smoke command.
- [x] Add the dated v1.3.3 changelog entry and release notes.
- [x] Update Lazarus package metadata to `1.3.3`.
- [x] Add this pull request note.

## Compatibility

No migration is required. Existing command registration, validation,
completion, generator behaviour, and schema-version-1 projects remain
compatible.

The parser accepts a separated leading `-` only when it is a valid integer or
float value for the registered option. Values beginning with `-` for other
parameter kinds continue to require equals syntax.

## Verification

- [x] Framework suite: 41 tests, 0 errors, 0 failures.
- [x] Separated and equals-form negative integer and float regression coverage.
- [x] Unknown-option regression coverage.
- [x] Windows example build-and-cleanup smoke check covering all seven examples.
- [x] Windows generator unit, golden-output, compile-smoke, and operations
  suites.
- [x] Lazarus runtime package build in an isolated clean clone with version
  metadata at `1.3.3`.
- [x] `git diff --check` passes.
- [x] FPC 3.2.2.
- [ ] GitHub Actions on Windows and Linux after the PR is opened.

## Release Readiness

- [x] Release date finalized as 2026-08-13.
- [x] Version metadata updated to `1.3.3`.
- [x] Changelog and release notes prepared.
- [x] Pull request notes prepared.
- [ ] Confirm GitHub Actions on Windows and Linux.

After merge, create the `v1.3.3` tag and publish the prepared release notes.
