# Release Notes - cli-fp v1.3.2

**Release Date:** 2026-07-30

## Overview

Version `1.3.2` is a contract-hardening patch release. It removes a hidden
runtime dependency on `TBaseCommand`, formally deprecates the non-functional
custom-completion callback registration methods, and establishes the
maintenance roadmap for the next releases.

Existing applications based on `TBaseCommand` remain source-compatible.

## `ICommand` Contract Fix

Earlier versions accepted commands through the `ICommand` interface but cast
the selected command to `TBaseCommand` before execution. A valid independent
implementation of `ICommand` could therefore register successfully and then
fail with an invalid type cast at runtime.

Version `1.3.2` removes that downcast. Parameter handoff now uses the optional
`ICommandParameterReceiver` capability:

- `TBaseCommand` implements `ICommandParameterReceiver`, preserving existing
  parameter lookup behaviour.
- Commands that implement only `ICommand` can execute without inheriting from
  `TBaseCommand`.
- Commands that need framework-managed parameter lookup can derive from
  `TBaseCommand` or implement the optional receiver contract.

The regression suite now includes an interface-only command and verifies its
execution and exit code.

## Deprecated Completion Callback APIs

The following concrete `TCLIApplication` methods are now marked deprecated:

- `RegisterFlagValueCompletion()`
- `RegisterPositionalCompletion()`

These methods have always been non-functional stubs. They remain as no-ops in
the 1.x line for source compatibility and are planned for removal in v2.0.0.

Built-in completion is unchanged. Registered commands, subcommands, flags,
Boolean values, and enum values continue to be completed from command
metadata.

## Maintenance Roadmap

The new project roadmap records the intended sequence:

- `v1.3.2`: correct current contracts;
- `v1.4.0`: add a simple callback API and typed argument access;
- `v1.5.0`: split help and completion out of `TCLIApplication`; and
- `v2.0.0`: adopt an explicit execution context and retire legacy shared-state
  plumbing.

Convenience APIs are expected to delegate to one underlying implementation so
that improved ergonomics do not create parallel parsing or validation paths.

## Verification

- Framework suite: 39 tests, 0 errors, 0 failures.
- Added regression coverage for an `ICommand` implementation that does not
  inherit from `TBaseCommand`.
- Verified that use of a deprecated callback registration method produces the
  intended FPC compiler warning.
- Full Windows generator suite passed:
  - generator unit tests;
  - golden-output test;
  - generated-project compile smoke test; and
  - generator operations test.
- All seven example applications compiled successfully.
- Lazarus runtime package compiled with version metadata at `1.3.2`.
- Local Markdown targets resolve and fenced code blocks are balanced across 39
  Markdown files.
- FPC version: 3.2.2.

## Compatibility

No migration is required for applications using `TBaseCommand`,
`CreateCLIApplication`, existing command registration, schema-version-1
`clifp.json` files, or generated projects.

Applications that call either deprecated custom-completion registration method
may receive a compiler warning. Removing those calls does not change runtime
behaviour because the methods did not register callbacks.

## Versioning

- The README release badge now targets `1.3.2`.
- The Lazarus package metadata now targets `1.3.2`.

**Full Changelog:** [v1.3.1...v1.3.2](https://github.com/ikelaiah/cli-fp/compare/v1.3.1...v1.3.2)
