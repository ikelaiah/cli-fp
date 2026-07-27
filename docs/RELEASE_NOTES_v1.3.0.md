# Release Notes - cli-fp v1.3.0

## Optional Root Commands

Version `1.3.0` adds an opt-in root command for applications that should run
without requiring a named command.

```text
myapp
myapp --name Gus
```

Named commands and nested subcommands remain available:

```text
myapp about
myapp repo clone
```

This is a backward-compatible minor release. Existing applications using the
two-argument factory retain their command-first behavior and continue to show
general help when invoked without a command.

## Framework API

The new overload accepts any existing `ICommand` implementation:

```pascal
RootCommand := TGreetCommand.Create('', 'Greet someone');
RootCommand.AddStringParameter('-n', '--name', 'Name to greet',
  False, 'World');

App := CreateCLIApplication('MyApp', '1.0.0', RootCommand);
ExitCode := App.Execute;
```

Root execution shares the same parsing, typed validation, defaults, help, and
exception handling as named commands. Terminal global options such as
`--help`, `--version`, and completion-script generation retain precedence.

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

Schema version 1 now accepts an optional `rootCommand` object:

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

## Examples and Verification

- Added `examples/RootCommandDemo`.
- Expanded the framework suite from 30 to 38 tests.
- Added generator parsing, round-trip, golden-output, runtime, and compile
  coverage for root commands.
- Verified the full framework and generator suites on Windows with FPC 3.2.2.

## Versioning

- The Lazarus package version is now `1.3.0`.
- The README version badge is now `1.3.0`.

This work addresses
[`[SUGGESTION] Have a root command` #14](https://github.com/ikelaiah/cli-fp/issues/14).

**Full Changelog:** [v1.2.0...v1.3.0](https://github.com/ikelaiah/cli-fp/compare/v1.2.0...v1.3.0)
