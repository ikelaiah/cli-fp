# Pull Request: Release v1.3.0 - Optional Root Commands

## Summary

This PR adds opt-in root-command execution so `cli-fp` applications can run
directly as `app [options]` without requiring a named command. It preserves
the existing `ICLIApplication` contract and two-argument factory behavior.

Closes #14.

## Framework

- [x] Add the backward-compatible three-argument `CreateCLIApplication`
  overload.
- [x] Execute the root command for an empty argument list or leading
  root-level option.
- [x] Preserve named command and nested-subcommand resolution.
- [x] Keep terminal global help, version, and completion options authoritative.
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

## Examples and Documentation

- [x] Add `RootCommandDemo`.
- [x] Update README, API reference, user manual, technical documentation, and
  generator documentation.
- [x] Add v1.3.0 release notes and changelog entries.
- [x] Update the Lazarus package and README version to `1.3.0`.

## Verification

- [x] Framework test suite: 38 tests, 0 failures.
- [x] RootCommandDemo Release build and runtime smoke tests.
- [x] Full Windows code-generator suite.
- [x] Generated root-command application compile and runtime smoke test.
- [ ] GitHub Actions on Linux and Windows.

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
