# Release Notes - cli-fp v1.2.0

**Release Date:** July 27, 2026

## Overview

Version `1.2.0` introduces `cli-fp-gen`, a standalone project and command
scaffold generator for `cli-fp`. It turns a versioned `clifp.json`
specification into a compilable Free Pascal application while keeping generated
infrastructure separate from user-owned command implementations.

This is a backward-compatible minor release. Existing `cli-fp` applications do
not require migration.

## New: `cli-fp-gen`

The generator supports the complete initial project workflow:

```text
cli-fp-gen init <target-dir> [--name <app-name>] [--version <x.y.z>] [--dry-run] [--force]
cli-fp-gen generate [--project <dir-or-spec-file>] [--dry-run] [--force]
cli-fp-gen add command <name> [--parent <cmd/path>] [--description <text>] [--project <dir-or-spec-file>] [--dry-run] [--force]
cli-fp-gen remove command <cmd/path> [--cascade] [--project <dir-or-spec-file>] [--dry-run] [--force]
```

### Generated project structure

- A Pascal program entry point under `src/`
- A generated command registry under `src/generated/`
- User-owned command stubs under `src/commands/`
- A generated-file manifest for stale-file cleanup
- A versioned `clifp.json` project specification as the source of truth

### Supported parameter kinds

Generated command registration supports:

- String, integer, float, flag, and explicit boolean parameters
- Path, enum, date/time, array, password, and URL parameters
- Required values, defaults, descriptions, short flags, and long flags

### File ownership

Generated entry points and registry units are refreshed by `generate`. Command
stubs are created once and preserved on subsequent runs unless `--force` is
explicitly supplied.

## Safety and correctness

The generator includes safeguards for:

- Existing project specifications during `init`
- Project-relative generated program paths
- Manifest cleanup outside the project directory
- Case-sensitive path handling on Linux and other case-sensitive filesystems
- Invalid command tokens and missing parents
- Duplicate command paths and generated Pascal identifier collisions
- Reserved Pascal words used as application names
- Safe Pascal string escaping in generated source

`--dry-run` previews file operations without modifying the project.

## Framework fix

Boolean parameter lookups now report a configured default as an available value,
matching the documented `GetParameterValue` contract and the behavior of other
parameter kinds.

## Automated testing

GitHub Actions now verifies the framework and generator on Linux and Windows.
The automated suite includes:

- 30 framework unit tests
- Focused generator naming and validation unit tests
- Golden-output comparisons
- Generator lifecycle and file-ownership checks
- Program and manifest path-safety checks
- Compilation and execution of a generated application
- Lazarus package compilation
- Compilation of all six shipped example applications

Local test runners are available for Bash and PowerShell.

## Build and use the generator

Compile from the repository root:

```bash
fpc -Futools/cli-fp-gen/src tools/cli-fp-gen/cli_fp_gen.lpr
```

Then create a project:

```bash
tools/cli-fp-gen/cli_fp_gen init ./my-app --name my-app
```

On Windows, use `cli_fp_gen.exe`.

See [codegen.md](codegen.md) for the complete specification and
workflow reference.

## Upgrade notes

- Existing applications remain source compatible.
- The Lazarus package version is now `1.2.0`.
- The generator is distributed as source and must be compiled before use.
- No `clifp.json` migration is required; schema version 1 is the current format.

## License

This project is licensed under the MIT License. See [LICENSE](../LICENSE).

---

**Full Changelog:** [v1.1.6...v1.2.0](https://github.com/ikelaiah/cli-fp/compare/v1.1.6...v1.2.0)
