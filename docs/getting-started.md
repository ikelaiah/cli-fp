# Your first cli-fp program

[Documentation home](../README.md) · [How do I...?](how-to.md) ·
[Command shapes](commands.md) · [Options](options.md)

Start with one root command when your program has one default action. The
complete program below is also the repository's
[QuickStartDemo](https://github.com/ikelaiah/cli-fp/tree/main/examples/QuickStartDemo),
which is compiled by the Windows and Linux example smoke checks.

The program follows the normal 1.x ownership model: `THelloCommand`
descends from `TBaseCommand`, `Main` is its instance and owns `--name`, and
`App` is the `ICLIApplication` that parses the invocation and calls
`Main.Execute`.

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

Clone, enter the repository, then build and run. If you already have a clone,
start in its root and skip the first two lines.

```bash
git clone https://github.com/ikelaiah/cli-fp.git
cd cli-fp
fpc -Fu./src ./examples/QuickStartDemo/QuickStartDemo.lpr
./examples/QuickStartDemo/QuickStartDemo --name Ada
./examples/QuickStartDemo/QuickStartDemo --help
./examples/QuickStartDemo/QuickStartDemo --version
```

```powershell
git clone https://github.com/ikelaiah/cli-fp.git
Set-Location cli-fp
fpc "-Fu.\src" .\examples\QuickStartDemo\QuickStartDemo.lpr
.\examples\QuickStartDemo\QuickStartDemo.exe --name Ada
.\examples\QuickStartDemo\QuickStartDemo.exe --help
.\examples\QuickStartDemo\QuickStartDemo.exe --version
```

The greeting prints `Hello, Ada!`. `--help` shows generated usage and the
option description; `--version` prints `hello version 1.0.0`. Each returns 0.
`CreateCLIApplication`'s name is application metadata; it does not change the
binary filename. Use the `QuickStartDemo` path shown above for every self-check.

`THelloCommand.Create('', ...)` uses an empty name intentionally: it represents
the root/default command, so there is no `greet` token before `--name Ada`.

The application owns the registered command tree through its interfaces. Do
not manually free registered commands; the beginner-recommended program tail
is `Halt(App.Execute)`.

The `{$mode objfpc}{$H+}` directive selects Free Pascal's Object Pascal mode
and long strings. If the compiler reports syntax or class/interface errors,
check that the directive is present before the `uses` clause.

## What the program does

- `TBaseCommand` supplies parameter registration and lookup.
- An empty command name creates the root (default) action.
- `CreateCLIApplication` constructs the application and registers its root.
- `App.Execute` parses arguments, validates options, handles built-ins, and
  dispatches the command. `Halt` passes the returned exit code to the shell.

Next, choose whether your application should remain a root command or grow
into [named or nested commands](commands.md). For common variations, go to
[How do I...?](how-to.md).
