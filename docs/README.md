# cli-fp Documentation

[Project README](../README.md) · [Examples](../examples/) ·
[Changelog](../CHANGELOG.md) ·
[v1.3.3 release notes](RELEASE_NOTES_v1.3.3.md)

Use this page to choose the shortest path to the information you need. If this
is your first Free Pascal command-line project, begin with the
[five-minute generated CLI](../README.md#build-your-first-generated-cli).

## Choose a Guide

| Goal | Guide |
| --- | --- |
| Install FPC and compile a first native CLI | [README: Start Here](../README.md#start-here) |
| Learn the framework from application setup through advanced features | [User manual](user-manual.md) |
| Generate a project or change its command tree | [Code generator](codegen.md) |
| Look up a public type or method | [API reference](api-reference.md) |
| Understand ownership, parsing, execution, and internal design | [Technical documentation](technical-docs.md) |
| See small programs that compile | [Examples](../examples/) |
| Test generated shell completion | [Completion testing guides](completion-testing/) |
| Review changes between releases | [Changelog](../CHANGELOG.md) |
| Review the v1.3.3 release | [v1.3.3 release notes](RELEASE_NOTES_v1.3.3.md) |

## New to Free Pascal?

The [Free Pascal in two minutes](../README.md#free-pascal-in-two-minutes)
section explains the file types, compiler switches, and Object Pascal
conventions used throughout the project. The
[official Free Pascal manuals](https://www.freepascal.org/docs.html) are the
authoritative references for the language, compiler, runtime library, and Free
Component Library.

You do not need Lazarus to use `cli-fp`: an FPC installation with the standard
FCL units is enough. Lazarus users can instead install the supplied
[runtime package](../README.md#lazarus-setup).

## Examples by Goal

| Learn about... | Example |
| --- | --- |
| A default action plus a named command | [`RootCommandDemo`](../examples/RootCommandDemo/) |
| Basic commands, options, and output | [`SimpleDemo`](../examples/SimpleDemo/) |
| Nested command trees | [`SubCommandDemo`](../examples/SubCommandDemo/) |
| Colours and terminal presentation | [`ColorDemo`](../examples/ColorDemo/) |
| Spinners and progress bars | [`ProgressDemo`](../examples/ProgressDemo/) |
| Longer operations and cleanup | [`LongRunningOpDemo`](../examples/LongRunningOpDemo/) |
| Errors and exit behaviour | [`ErrorHandlingDemo`](../examples/ErrorHandlingDemo/) |

Each example is a normal `.lpr` program. From the repository root, compile one
on Linux or macOS with:

```bash
fpc -Fu./src ./examples/RootCommandDemo/RootCommandDemo.lpr
```

In PowerShell, quote the `-Fu` argument so it reaches FPC as one argument:

```powershell
fpc "-Fu.\src" .\examples\RootCommandDemo\RootCommandDemo.lpr
```

## Repository Map

| Path | Purpose |
| --- | --- |
| [`src/`](../src/) | Framework units used by applications |
| [`tools/cli-fp-gen/`](../tools/cli-fp-gen/) | JSON-driven project generator |
| [`examples/`](../examples/) | Focused runnable applications |
| [`tests/`](../tests/) | FPCUnit framework tests and generator integration tests |
| [`packages/lazarus/`](../packages/lazarus/) | Runtime-only Lazarus package |
| [`docs/`](./) | Guides, references, and release records |

## Current Guides and Historical Records

The README, user manual, code-generator guide, API reference, and technical
documentation describe the current source tree.

The v1.3.3 implementation is complete and its release is dated 2026-08-14. It
introduces hermetic test compilation, password-safe framework diagnostics, and
internal boundaries for help rendering, completion calculation, and
parameter-value semantics without changing the public application facade.

Files named `PULL_REQUEST_v*.md` and `RELEASE_NOTES_v*.md` are snapshots of a
particular release. `test-output.md` records the test environment and results
at the time stated in that file. Treat those files as release history rather
than as the primary usage documentation.

## Found a Documentation Problem?

Please open an issue with the page, section, command, and platform involved.
Small corrections are welcome as pull requests.
