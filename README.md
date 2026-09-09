![cli-fp: A CLI framework for Free Pascal](docs/images/cli-fp-hero.svg)

# ⚡ cli-fp: Native CLIs for Free Pascal

[![License: MIT](https://img.shields.io/badge/License-MIT-1E3A8A.svg)](https://github.com/ikelaiah/cli-fp/blob/main/LICENSE)
[![Free Pascal](https://img.shields.io/badge/Free%20Pascal-3.2.2+-3B82F6.svg)](https://www.freepascal.org/)
[![Lazarus](https://img.shields.io/badge/Lazarus-package-60A5FA.svg)](https://github.com/ikelaiah/cli-fp/blob/main/packages/lazarus/cli_fp.lpk)
![Supports Windows](https://img.shields.io/badge/support-Windows-F59E0B?logo=Windows)
![Supports Linux](https://img.shields.io/badge/support-Linux-F59E0B?logo=Linux)
[![Version](https://img.shields.io/badge/version-1.4.1-8B5CF6.svg)](https://github.com/ikelaiah/cli-fp/blob/main/CHANGELOG.md)
[![Documentation](https://img.shields.io/badge/Docs-Website-brightgreen.svg)](https://ikelaiah.github.io/cli-fp/)
[![Tests](https://github.com/ikelaiah/cli-fp/actions/workflows/tests.yml/badge.svg)](https://github.com/ikelaiah/cli-fp/actions/workflows/tests.yml)

`cli-fp` is a small Free Pascal framework for native command-line programs. It
provides command trees, validated options, generated help and shell completion,
colours, spinners, and progress bars—without third-party runtime dependencies.

## Your first CLI

This complete root-command program is also
[QuickStartDemo](https://github.com/ikelaiah/cli-fp/blob/main/examples/QuickStartDemo/QuickStartDemo.lpr), compiled by the
Windows and Linux example smoke checks.

```pascal
program QuickStartDemo;

{$mode objfpc}{$H+}{$J-}

uses
  CLI.Interfaces, CLI.Application, CLI.Command;

type
  THelloCommand = class(TBaseCommand)
  public
    function Execute: Integer; override;
  end;

function THelloCommand.Execute: Integer;
var
  PersonName: string;
begin
  if not GetParameterValue('--name', PersonName) then
    PersonName := 'World';
  WriteLn('Hello, ', PersonName, '!');
  Result := 0;
end;

var
  App: ICLIApplication;
  Main: THelloCommand;
begin
  Main := THelloCommand.Create('', 'Print a greeting');
  Main.AddStringParameter('-n', '--name', 'Name to greet', False, 'World');
  App := CreateCLIApplication('hello', '1.0.0', Main);
  Halt(App.Execute);
end.
```

Compile from a clone of this repository:

```bash
fpc -Fu./src ./examples/QuickStartDemo/QuickStartDemo.lpr
./examples/QuickStartDemo/QuickStartDemo --name Ada
```

```console
$ ./examples/QuickStartDemo/QuickStartDemo --name Ada
Hello, Ada!
```

On PowerShell, use `fpc "-Fu.\src" .\examples\QuickStartDemo\QuickStartDemo.lpr`
and run `.\examples\QuickStartDemo\QuickStartDemo.exe --name Ada`.

## Choose a CLI shape

| Shape | Invocation | Start with |
| --- | --- | --- |
| One default action | `hello --name Ada` | [Root command](docs/commands.md#root-command) |
| Named commands | `tool greet --name Ada` | [Named command](docs/commands.md#named-command) |
| Nested commands | `tool repo clone --url …` | [Subcommand](docs/commands.md#subcommand) |

## Documentation

- [Documentation site](https://ikelaiah.github.io/cli-fp/) — navigate by goal.
- [Your first cli-fp program](docs/getting-started.md) — compile and understand
  the example above.
- [How do I...?](docs/how-to.md) — short recipes for options, output,
  completion, the generator, and debugging.
- [Current limitations](docs/limitations.md) — find the supported boundaries.
- [API reference](docs/api-reference.md) — look up public signatures.
- [Technical documentation](docs/technical-docs.md) — architecture and
  maintainer material.

## Examples and generator

The [runnable examples](docs/examples.md) progress from QuickStartDemo through
root, named, nested, error-handling, colour, and progress applications. They
are compiled by the cross-platform cleanup smoke checks.

Use [`cli-fp-gen`](docs/codegen.md) when a larger command tree benefits from a
scaffolded project layout. It is optional; the program above is the shortest
way to start.

## Requirements

- Free Pascal 3.2.2 is the tested compiler version.
- Windows and Linux run the repository's CI checks.
- Lazarus is optional; a runtime package is available in the
  [`packages/lazarus/`](https://github.com/ikelaiah/cli-fp/tree/main/packages/lazarus)
  directory.
- The runtime has no third-party dependencies. The generator uses FCL JSON
  units.

## Contributing

See the [contribution guide](https://github.com/ikelaiah/cli-fp/blob/main/CONTRIBUTING.md). On Windows, run:

```powershell
powershell -ExecutionPolicy Bypass -File tests\run_tests.ps1
powershell -ExecutionPolicy Bypass -File tests\codegen\run_all_tests.ps1
powershell -ExecutionPolicy Bypass -File tests\run_cleanup_smoke.ps1
```

The Bash equivalents and the full project map are in the contribution guide.

## License

`cli-fp` is available under the [MIT License](https://github.com/ikelaiah/cli-fp/blob/main/LICENSE).
