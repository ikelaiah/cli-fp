# How do I...?

Short, supported recipes for common `cli-fp` tasks. Start with
[your first program](getting-started.md) if the class-based API is new to you.
Each recipe shows the smallest useful piece; linked guides supply the surrounding
program structure.

## How do I create the smallest CLI?

Use an unnamed root command and pass it to `CreateCLIApplication`:

```pascal
Main := THelloCommand.Create('', 'Print a greeting');
App := CreateCLIApplication('hello', '1.0.0', Main);
Halt(App.Execute);
```

See the complete, compiled [QuickStartDemo](getting-started.md).

## How do I create a root/default command?

Give a `TBaseCommand` descendant an empty name. Its `Execute` method runs for
`myapp [options]`:

```pascal
Root := TMyRootCommand.Create('', 'Run the default action');
App := CreateCLIApplication('myapp', '1.0.0', Root);
```

Root options are not inherited by named commands. See [command shapes](commands.md).

## How do I add a named command?

```pascal
Greet := TGreetCommand.Create('greet', 'Print a greeting');
App.RegisterCommand(Greet);
```

This creates `myapp greet`. Create the application without a root command for
a command-first CLI.

## How do I add a subcommand?

```pascal
Repo := TRepoCommand.Create('repo', 'Repository operations');
Clone := TCloneCommand.Create('clone', 'Clone a repository');
Repo.AddSubCommand(Clone);
App.RegisterCommand(Repo);
```

This creates `myapp repo clone`. See [commands](commands.md) for the complete
relationship.

## How do I add a string option?

```pascal
Command.AddStringParameter('-n', '--name', 'Name to greet', False, 'World');
```

The final argument is the default value. Omit it for an optional parameter
without a default.

## How do I add an integer or float option?

```pascal
Command.AddIntegerParameter('-c', '--count', 'Number of runs', True);
Command.AddFloatParameter('-r', '--rate', 'Processing rate', False, '1.0');
```

Values are validated before `Execute`; convert the retrieved string with
`TryStrToInt` or `TryStrToFloat`.

## How do I make an option required?

Pass `True` in the `Required` position:

```pascal
Command.AddStringParameter('-f', '--file', 'Input file', True);
```

The framework reports a missing required option and shows command help before
it calls `Execute`.

## How do I give an option a default?

```pascal
Command.AddStringParameter('-o', '--output', 'Output file', False, 'out.txt');
```

`GetParameterValue` returns the default string when the user omitted the
option.

## How do I add a Boolean flag?

```pascal
Command.AddFlag('-v', '--verbose', 'Show detailed output');
```

It is `false` by default and `true` when present. For an explicit
`--colour true|false` value, use `AddBooleanParameter` instead.

## How do I add an enum option?

```pascal
Command.AddEnumParameter('-l', '--level', 'Log level',
  'debug|info|warn|error', False, 'info');
```

Enum matching is case-insensitive and built-in completion suggests the listed
values.

## How do I accept a path?

```pascal
Command.AddPathParameter('-p', '--path', 'Target directory', True);
```

This accepts a path-shaped string; it does not check that the path exists.

## How do I accept a URL?

```pascal
Command.AddUrlParameter('-u', '--url', 'Repository URL', True);
```

The current validator accepts `http://`, `https://`, `git://`, and `ssh://`
prefixes.

## How do I accept a sensitive/password value?

```pascal
Command.AddPasswordParameter('-k', '--api-key', 'API key', True);
```

Do not print or log the retrieved string. Framework debug output redacts
registered password values, but your own output does not.

## How do I retrieve a parameter inside `Execute`?

```pascal
var Name: string;
begin
  if GetParameterValue('--name', Name) then
    WriteLn('Hello, ', Name);
end;
```

Use either the short or long flag registered on that command.

## How do I convert validated values to Pascal types?

```pascal
if GetParameterValue('--count', RawCount) and TryStrToInt(RawCount, Count) then
  WriteLn(Count);

if GetParameterValue('--verbose', RawVerbose) and
   SameText(RawVerbose, 'true') then
  WriteLn('Verbose mode');
```

The current public lookup API is string-based; [options](options.md) explains
why conversion remains necessary.

## How do I return a non-zero exit code?

Return it from `Execute`, then pass the application's result to `Halt`:

```pascal
function TCheckCommand.Execute: Integer;
begin
  if not CheckInputs then
    Exit(1);
  Result := 0;
end;

// Program body
Halt(App.Execute);
```

## How do I print coloured output?

```pascal
uses CLI.Console;

TConsole.WriteLn('Created project', ccGreen);
TConsole.WriteLn('Could not create project', ccRed);
```

See [terminal output](terminal.md) and the runnable
[ColorDemo](https://github.com/ikelaiah/cli-fp/tree/main/examples/ColorDemo).

## How do I display a spinner?

```pascal
Spinner := CreateSpinner(ssLine);
Spinner.Start;
try
  Work;
finally
  Spinner.Stop;
end;
```

Use an `IProgressIndicator` variable and always stop it in `finally`.

## How do I display progress?

```pascal
Bar := CreateProgressBar(Total);
Bar.Start;
try
  Bar.Update(Current, 'Working');
finally
  Bar.Stop;
end;
```

Use a progress bar when `Total` is known; otherwise use a spinner.

## How do I generate Bash completion?

```bash
./myapp --completion-file > myapp-completion.bash
source ./myapp-completion.bash
```

The request must be the first argument. See [shell completion](completion.md)
for installation and behavior.

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

`DebugMode` is on the concrete `TCLIApplication`, not `ICLIApplication`:

```pascal
App := CreateCLIApplication('myapp', '1.0.0');
(App as TCLIApplication).DebugMode := True;
```

Use it only while diagnosing an invocation, and never use debug output as a
place to expose password values.

## How do I do something cli-fp does not support?

Read [current limitations](limitations.md) first. In particular, positional
arguments, inherited global options, typed parameter access, and dynamic
completion callbacks are not current features. Build a small application-level
adapter if it fits your program, or open an issue with the command line and
behavior you need. Planned roadmap work is not a released contract.
