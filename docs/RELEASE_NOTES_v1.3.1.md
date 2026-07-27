# Release Notes - cli-fp v1.3.1

**Release Date:** July 28, 2026

## Overview

Version `1.3.1` is a documentation-focused patch release that makes `cli-fp`
easier to learn, evaluate, and adopt. It adds a verified path from installing
Free Pascal to running a generated native CLI, gives newcomers a concise
Object Pascal orientation, and reorganizes the documentation around developer
goals.

The release also corrects dependency, compiler-command, platform-support,
console, and API descriptions found during a source-backed documentation
audit.

There are no framework runtime, public API, generator-schema, or generated
output changes in this release.

## A Faster First Success

The README now guides a new developer through:

1. installing and verifying Free Pascal;
2. compiling `cli-fp-gen`;
3. generating an application;
4. compiling the generated Pascal units into a native executable;
5. implementing the generated `greet` command; and
6. running the result as `Myapp greet --name Ada`.

The documented command implementation and its `Hello, Ada!` result were
compile- and runtime-verified with FPC 3.2.2.

## Free Pascal Orientation

New sections explain the project’s essential Pascal conventions, including:

- `.lpr` program entry points and `.pas` units;
- `uses` clauses and FPC `-Fu` unit-search paths;
- `{$mode objfpc}`;
- `TBaseCommand` inheritance and overridden `Execute` methods; and
- returning application exit codes through `Halt(App.Execute)`.

The README also highlights how the project uses classes, interfaces, generics,
exceptions, deterministic cleanup, conditional compilation, FPCUnit, native
binaries, and platform-aware console code.

## Documentation Navigation

A new `docs/README.md` documentation home provides:

- goal-based routes into the user, generator, API, and maintainer guides;
- an examples map showing which application demonstrates each feature;
- a repository-layout overview;
- guidance for developers new to Free Pascal; and
- a clear distinction between current documentation and historical release or
  test records.

The user manual, generator guide, API reference, technical documentation, and
completion guides now link back into this navigation structure.

## Manual and Generator Improvements

- Replaced the user manual’s duplicated opening reference material with
  prerequisites and focused learning paths.
- Retained a single API cheat sheet for experienced users.
- Clarified root-command selection, help scopes, parameter conversion, debug
  access, and console behaviour.
- Added verified cross-platform generator and generated-application commands.
- Explained how `clifp.json`, the `.lpr` entry point, generated registry units,
  and user-owned command units become one native executable.
- Removed the dated “Phase 1” framing from the current generator guide.

## Accuracy Corrections

- Described parameter types as metadata used for validation and help while
  making clear that command code retrieves values as strings.
- Documented the standard FCL JSON units required by `cli-fp-gen`.
- Added required PowerShell quoting around FPC `-Fu` arguments.
- Separated CI-tested Windows/Linux environments from expected but currently
  untested Unix targets.
- Corrected Windows console and ANSI cursor-control descriptions.
- Expanded selected public API declarations and identified intentionally
  omitted testing and disabled callback surfaces.
- Preserved the distinction between general help, command-level help,
  complete help, version output, and completion-script generation.

## Verification

- Framework suite: 38 tests, 0 failures.
- Full Windows generator suite passed.
- The documented generated project compiled and ran successfully.
- `RootCommandDemo` compiled and completed its root and named actions.
- The Lazarus runtime package compiled with version metadata at `1.3.1`.
- Local links and heading anchors across 30 Markdown files resolve.
- Fenced code blocks across 30 Markdown files are balanced.
- `git diff --check` passed.

## Compatibility

No migration is required. Existing applications, public API usage,
schema-version-1 `clifp.json` files, and generated projects continue to work as
they did in v1.3.0.

## Versioning

- The README release badge now targets `1.3.1`.
- The Lazarus package metadata now targets `1.3.1`.

**Full Changelog:** [v1.3.0...v1.3.1](https://github.com/ikelaiah/cli-fp/compare/v1.3.0...v1.3.1)
