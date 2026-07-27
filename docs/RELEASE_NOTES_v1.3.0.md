# Release Notes - cli-fp v1.3.0

**Release Date:** July 28, 2026

## Overview

Version `1.3.0` introduces optional root commands, allowing focused utilities
to run directly as `app [options]` without giving up named commands or nested
subcommands. It also adds root-command support to `cli-fp-gen` and completes a
documentation-correction pass for the public API, build commands, and shell
completion guidance.

This is a backward-compatible minor release. Existing applications and
schema-v1 generator specifications require no migration.

## Optional Root Commands

Applications can opt into a root command that runs without requiring a named
command.

```text
myapp
myapp --name Gus
```

Named commands and nested subcommands remain available:

```text
myapp about
myapp repo clone
```

Existing applications using the two-argument factory retain their
command-first behavior and continue to show general help when invoked without
a command.

## Framework API

The new overload accepts any existing `ICommand` implementation:

```pascal
RootCommand := TGreetCommand.Create('', 'Greet someone');
RootCommand.AddStringParameter('-n', '--name', 'Name to greet',
  False, 'World');

App := CreateCLIApplication('MyApp', '1.0.0', RootCommand);
Halt(App.Execute);
```

Root execution shares the same parsing, typed validation, defaults, help, and
exception handling as named commands. Sole help/version requests and
first-argument completion-script requests retain precedence over root
selection.

Root parameters are local to the root command. Persistent/inherited flags and
positional arguments are not introduced in this release.

## Help and Completion

- General help includes the root description and parameters.
- Complete help includes a dedicated root-options section.
- Bash and PowerShell completion include root parameter flags.
- Built-in boolean and enum value completion works at the root level.
- Root-level completion remains commands-first until the user starts an
  option with `-`.

## Generator Support

Schema version 1 now accepts an optional `rootCommand` object. The following
shows that member in isolation; a complete specification still requires
`schemaVersion`, `app`, and `commands`:

```json
{
  "rootCommand": {
    "description": "Run the default action",
    "parameters": []
  }
}
```

When configured, `cli-fp-gen`:

- creates a user-owned `<App>_RootCommand.pas` stub;
- generates root parameter registration;
- calls the three-argument application factory; and
- retains any root stub if `rootCommand` is later removed from the spec.

Existing specifications without `rootCommand` generate as before.

## Documentation

The documentation received a release-wide accuracy and onboarding pass:

- Reworked the README to help new developers choose between root commands,
  named commands, and combined applications.
- Corrected public API signatures and copy/paste Pascal examples, with key
  examples verified by compilation.
- Updated root-command and code-generator instructions, including
  case-sensitive Linux build commands.
- Aligned Bash and PowerShell completion guides with current behavior.
- Clarified date-time parsing, Boolean defaults, password handling, and current
  custom-completion limitations.
- Repaired local documentation links and labelled dated test reports as
  historical snapshots.

## Examples and Verification

- Added `examples/RootCommandDemo`.
- Corrected copy/paste API examples, generated-program filename casing, and
  shell-completion compatibility notes.
- Removed the generated password-help claim that values are masked; password
  values are raw strings and must be handled as sensitive by the application.
- Expanded the framework suite from 30 to 38 tests.
- Added generator parsing, round-trip, golden-output, runtime, and compile
  coverage for root commands.
- Verified the full framework and generator suites on Windows with FPC 3.2.2.
- GitHub Actions passed on Linux and Windows for both push and pull-request
  events.

## Versioning

- The Lazarus package version is now `1.3.0`.
- The README version badge is now `1.3.0`.

This work addresses
[`[SUGGESTION] Have a root command` #14](https://github.com/ikelaiah/cli-fp/issues/14).

**Full Changelog:** [v1.2.0...v1.3.0](https://github.com/ikelaiah/cli-fp/compare/v1.2.0...v1.3.0)
