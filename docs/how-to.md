# How do I...?

Short, supported recipes for common `cli-fp` tasks. Start with
[your first program](getting-started.md) for the canonical compiled program.

## The command model used by every recipe

Normal cli-fp 1.x applications are class-based. You define a descendant, create
an object of that class, register its options on that object, then register the
object with the application:

```text
TBaseCommand
  └── TGreetCommand
      ├── Execute
      └── registered options
TGreetCommand instance
  └── registered with ICLIApplication
```

This complete **command pattern** defines the developer-owned command. It is
not a whole program; the setup fragment immediately below creates and
registers its `Greet` instance.

```pascal
uses
  CLI.Command;

type
  TGreetCommand = class(TBaseCommand)
  public
    function Execute: Integer; override;
  end;

function TGreetCommand.Execute: Integer;
var
  PersonName: string;
begin
  if not GetParameterValue('--name', PersonName) then
    PersonName := 'World';
  WriteLn('Hello, ', PersonName, '!');
  Result := 0;
end;
```

This **program-setup fragment** supplies the instance and application used by
the option recipes below. Put it in a program that also contains the command
pattern above and uses `CLI.Interfaces` and `CLI.Application`.

```pascal
var
  App: ICLIApplication;
  Greet: TGreetCommand;
begin
  App := CreateCLIApplication('hello', '1.0.0');
  Greet := TGreetCommand.Create('greet', 'Print a greeting');
  Greet.AddStringParameter('-n', '--name', 'Name to greet', False, 'World');
  App.RegisterCommand(Greet);
  Halt(App.Execute);
end.
```

The `TGreetCommand` instance owns `--name`; its `Execute` method retrieves
that value after the application has parsed and validated the command line.

## How do I create the smallest CLI?

Use the complete, compiled [QuickStartDemo](getting-started.md). It supplies
an unnamed root-command descendant and passes that object to the three-argument
`CreateCLIApplication` overload.

## How do I create a root/default command?

Define the root command exactly like `TGreetCommand`, but give its instance an
empty name. This setup fragment assumes `TRootCommand` is your declared
`TBaseCommand` descendant with an overridden `Execute` method:

```pascal
var
  App: ICLIApplication;
  Root: TRootCommand;
begin
  Root := TRootCommand.Create('', 'Run the default action');
  Root.AddFlag('-v', '--verbose', 'Show detailed output');
  App := CreateCLIApplication('myapp', '1.0.0', Root);
  Halt(App.Execute);
end.
```

Its `Execute` method runs for `myapp [options]`. Root options are not inherited
by named commands; see [command shapes](commands.md).

## How do I add a named command?

The `TGreetCommand` pattern and setup at the top of this page create
`hello greet --name Ada`. For a command-first application, call the two-argument
factory, create the named command object, register its options on that object,
then call `App.RegisterCommand(Greet)`.

## How do I add a subcommand?

A parent and child are both command objects. This **setup fragment** assumes
`TRepoCommand` and `TCloneCommand` are declared `TBaseCommand` descendants,
each with its own `Execute` override; it registers only the top-level object:

```pascal
var
  App: ICLIApplication;
  Repo: TRepoCommand;
  Clone: TCloneCommand;
begin
  App := CreateCLIApplication('tool', '1.0.0');
  Repo := TRepoCommand.Create('repo', 'Repository operations');
  Clone := TCloneCommand.Create('clone', 'Clone a repository');
  Clone.AddUrlParameter('-u', '--url', 'Repository URL', True);
  Repo.AddSubCommand(Clone);
  App.RegisterCommand(Repo);
  Halt(App.Execute);
end.
```

This creates `tool repo clone --url https://example.com/project.git`. The
`Clone` object owns `--url`; the parent is only the command group. See
[commands](commands.md) for the full root/named/nested comparison.

## How do I add options?

Each registration below is a **program-setup fragment** placed immediately
after `Greet := TGreetCommand.Create(...)` in the setup at the top of this
page. `Greet` is therefore the concrete `TGreetCommand` instance that owns
the registered option.

### String, integer, and float

```pascal
Greet.AddStringParameter('-n', '--name', 'Name to greet', False, 'World');
Greet.AddIntegerParameter('-c', '--count', 'Number of runs', True);
Greet.AddFloatParameter('-r', '--rate', 'Processing rate', False, '1.0');
```

### Required, default, flag, and enum

```pascal
Greet.AddStringParameter('-f', '--file', 'Input file', True);
Greet.AddStringParameter('-o', '--output', 'Output file', False, 'out.txt');
Greet.AddFlag('-v', '--verbose', 'Show detailed output');
Greet.AddEnumParameter('-l', '--level', 'Log level',
  'debug|info|warn|error', False, 'info');
```

Use `AddFlag` when presence alone enables a feature: `--verbose` becomes
`true`, while omission returns `false`. Use `AddBooleanParameter` when callers
need an explicit value such as `--verbose true` or `--verbose false`; bare
presence remains accepted as `true` for 1.x compatibility.
Option lookup is case-insensitive and repeated options use the last value.

### Path, URL, and password

```pascal
Greet.AddPathParameter('-p', '--path', 'Target directory', True);
Greet.AddUrlParameter('-u', '--url', 'Repository URL', True);
Greet.AddPasswordParameter('-k', '--api-key', 'API key', True);
```

The framework validates registered values before it calls the selected
command's `Execute`. Paths are still strings rather than existence checks;
never print a retrieved password. See [options](options.md) for every kind.

## How do I retrieve and convert a value?

Put lookup code inside the descendant that owns the option. This replacement
for `TGreetCommand.Execute` uses the `--count` and `--verbose` options
registered on the `Greet` instance above; it needs `SysUtils` for
`TryStrToInt` and `SameText`.

```pascal
function TGreetCommand.Execute: Integer;
var
  RawCount: string;
  RawVerbose: string;
  Count: Integer;
begin
  if GetParameterValue('--count', RawCount) and
     TryStrToInt(RawCount, Count) then
    WriteLn('Count: ', Count);

  if GetParameterValue('--verbose', RawVerbose) and
     SameText(RawVerbose, 'true') then
    WriteLn('Verbose mode');

  Result := 0;
end;
```

`GetParameterValue` is protected, so it belongs in the command class—not the
program setup. Values remain strings after validation; use `TryStrToFloat` for
a float. An absent `AddFlag` normally supplies `false`. `GetParameterValue`
returns `False` and clears its output string when the option has no value.

## How do I return a non-zero exit code?

Set `Result` in the command's `Execute`, then let the application return it to
the shell. This complete **command-method fragment** assumes
`TCheckCommand = class(TBaseCommand)` is declared in the same unit:

```pascal
function TCheckCommand.Execute: Integer;
var
  InputFile: string;
begin
  if not GetParameterValue('--file', InputFile) then
    Exit(1);
  WriteLn('Checking ', InputFile);
  Result := 0;
end;
```

At the program boundary, use `Halt(App.Execute)`—not `Halt` inside `Execute`.
Here `App` is the `ICLIApplication` variable created in a program-setup
fragment such as the one at the top of this page.

## How do I print coloured output?

Inside a command's `Execute`, import `CLI.Console` and call the `TConsole`
class directly; there is no console object to construct:

```pascal
uses
  CLI.Console;

TConsole.WriteLn('Created project', ccGreen);
TConsole.WriteLn('Could not create project', ccRed);
```

See [terminal output](terminal.md) and the runnable
[ColorDemo](https://github.com/ikelaiah/cli-fp/tree/main/examples/ColorDemo).

## How do I display a spinner?

This complete **`Execute`-method fragment** belongs to a declared
`TDownloadCommand = class(TBaseCommand)`. Its command unit needs
`CLI.Interfaces` and `CLI.Progress`:

```pascal
function TDownloadCommand.Execute: Integer;
var
  Spinner: IProgressIndicator;
begin
  Spinner := CreateSpinner(ssLine);
  Spinner.Start;
  try
    Spinner.Update(0, 'Downloading');
    // Perform the download here.
  finally
    Spinner.Stop;
  end;
  Result := 0;
end;
```

Always stop the indicator in `finally`.

## How do I display progress?

This complete **`Execute`-method fragment** belongs to a declared
`TBatchCommand = class(TBaseCommand)`. Its command unit needs `SysUtils`,
`CLI.Interfaces`, and `CLI.Progress`:

```pascal
function TBatchCommand.Execute: Integer;
var
  Bar: IProgressIndicator;
  Index: Integer;
  Total: Integer;
begin
  Total := 3;
  Bar := CreateProgressBar(Total);
  Bar.Start;
  try
    for Index := 1 to Total do
    begin
      // Process item Index here.
      Bar.Update(Index, Format('Processed %d of %d', [Index, Total]));
    end;
  finally
    Bar.Stop;
  end;
  Result := 0;
end;
```

Use a progress bar when the total is known; otherwise use a spinner.

## How do I generate Bash completion?

```bash
./myapp --completion-file > myapp-completion.bash
source ./myapp-completion.bash
```

The request must be the first argument. See [shell completion](completion.md)
for installation and behavior.

On Windows, quote the unit path and descriptions with PowerShell's normal
quoting rules, for example `fpc "-Fu.\src" .\src\Myapp.lpr`; do not paste Bash
line-continuation or variable syntax into PowerShell. On Linux, filename and
unit casing must match exactly.

## How do I generate PowerShell completion?

```powershell
.\myapp.exe --completion-file-pwsh > .\myapp-completion.ps1
. .\myapp-completion.ps1
```

The leading dot and space source the generated script in the current session.

## How do I create a Git-style nested CLI?

Build a command tree with `AddSubCommand`, then register only its top-level
command. [SubCommandDemo](https://github.com/ikelaiah/cli-fp/tree/main/examples/SubCommandDemo)
is the runnable reference.

## How do I scaffold a project with `cli-fp-gen`?

```bash
fpc -Futools/cli-fp-gen/src tools/cli-fp-gen/cli_fp_gen.lpr
./tools/cli-fp-gen/cli_fp_gen init ./build-temp/myapp --name myapp
```

Use `generate` after changing `clifp.json`; keep application logic in the
user-owned command units. See the [generator guide](codegen.md).

## How do I inspect/debug argument parsing?

`DebugMode` is on the concrete `TCLIApplication`, not `ICLIApplication`. This
**program-setup fragment** declares and creates the `App` it casts; it needs
`CLI.Interfaces` and `CLI.Application`:

```pascal
var
  App: ICLIApplication;
begin
  App := CreateCLIApplication('myapp', '1.0.0');
  (App as TCLIApplication).DebugMode := True;
  Halt(App.Execute);
end;
```

Use it only while diagnosing an invocation, and never use debug output as a
place to expose password values.

## How do I do something cli-fp does not support?

Read [current limitations](limitations.md) first. In particular, positional
arguments, the `--` terminator, inherited global options, typed parameter access, and dynamic
completion callbacks are not current features. Build a small application-level
adapter if it fits your program, or open an issue with the command line and
behavior you need. Planned roadmap work is not a released contract.
