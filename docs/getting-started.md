# Your first cli-fp program

[Documentation home](../README.md) · [How do I...?](how-to.md) ·
[Command shapes](commands.md) · [Options](options.md)

Start with one root command when your program has one default action. The
complete program below is also the repository's
[QuickStartDemo](https://github.com/ikelaiah/cli-fp/tree/main/examples/QuickStartDemo),
which is compiled by the Windows and Linux example smoke checks.

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

```powershell
fpc "-Fu.\src" .\examples\QuickStartDemo\QuickStartDemo.lpr
.\examples\QuickStartDemo\QuickStartDemo.exe --name Ada
```

Both commands print `Hello, Ada!`. Run `--help` to see generated usage and
the option description.

## What the program does

- `TBaseCommand` supplies parameter registration and lookup.
- An empty command name creates the root (default) action, so users run
  `hello --name Ada`, not `hello greet --name Ada`.
- `CreateCLIApplication` parses arguments, validates registered options, shows
  help, and returns the exit code from `Execute`.

Next, choose whether your application should remain a root command or grow
into [named or nested commands](commands.md). For common variations, go to
[How do I...?](how-to.md).
