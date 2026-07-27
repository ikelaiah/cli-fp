# Pull Request: Release v1.3.0 - Optional Root Commands

**Target Release:** v1.3.0

**Release Date:** July 28, 2026

## Summary

This PR adds opt-in root-command execution so `cli-fp` applications can run
directly as `app [options]` without requiring a named command. It preserves
the existing `ICLIApplication` contract and two-argument factory behavior.

Closes #14.

## Type of Change

- [x] New backward-compatible feature
- [x] Documentation correction
- [x] Test and CI coverage
- [ ] Breaking change

## Framework

- [x] Add the backward-compatible three-argument `CreateCLIApplication`
  overload.
- [x] Execute the root command for an empty argument list or leading
  root-level option.
- [x] Preserve named command and nested-subcommand resolution.
- [x] Keep sole help/version requests and first-argument completion-script
  options authoritative.
- [x] Share parsing, validation, defaults, and exception handling between root
  and named commands.
- [x] Reset application parsing state between executions.

## Help and Completion

- [x] Show root description and parameters in general help.
- [x] Show root parameters in complete help.
- [x] Complete root flags in Bash and PowerShell.
- [x] Complete root boolean and enum values.
- [x] Preserve commands-first empty-token completion behavior.

## Generator

- [x] Add optional schema-v1 `rootCommand` parsing and serialization.
- [x] Generate a protected user-owned root command stub.
- [x] Generate root parameter registration and application wiring.
- [x] Preserve output for specifications without a root command.
- [x] Extend unit, golden, runtime, and compile-smoke coverage.

## Compatibility

Existing code remains valid:

```pascal
App := CreateCLIApplication('MyApp', '1.0.0');
```

Root behavior is enabled only through the new overload:

```pascal
App := CreateCLIApplication('MyApp', '1.0.0', RootCommand);
```

No migration is required for existing applications or schema-v1
`clifp.json` files.

## Examples and Release Metadata

- [x] Add `RootCommandDemo`.
- [x] Add v1.3.0 release notes and changelog entries.
- [x] Update the Lazarus package and README version to `1.3.0`.

## Documentation

- [x] Rework README onboarding for root, named, and combined CLI applications.
- [x] Correct public API signatures and compile-check key Pascal examples.
- [x] Fix code-generator instructions and case-sensitive Linux build commands.
- [x] Align Bash and PowerShell completion guides with current behavior.
- [x] Clarify date-time parsing, Boolean defaults, password handling, and
  custom-completion limitations.
- [x] Repair documentation links and label dated test reports as historical
  snapshots.

## Verification

- [x] Framework test suite: 38 tests, 0 failures.
- [x] RootCommandDemo Release build and runtime smoke tests.
- [x] Full Windows code-generator suite.
- [x] Generated root-command application compile and runtime smoke test.
- [x] GitHub Actions on Linux and Windows.
- [x] No conflicts with the base branch.

## Release Readiness

- [x] Changelog finalized for July 28, 2026.
- [x] Release notes finalized for July 28, 2026.
- [x] Version metadata targets `1.3.0`.
- [x] Documentation links and examples verified.

After merge, create the `v1.3.0` tag and publish the prepared release notes.
