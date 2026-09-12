# cli-fp API reference

[How do I...?](how-to.md) · [Commands](commands.md) · [Options](options.md) ·
[Current limitations](limitations.md)

Use this page to look up the current public, application-facing API. For a
complete program, start with [your first CLI](getting-started.md). The public
units in [`src/`](https://github.com/ikelaiah/cli-fp/tree/main/src) are the
authoritative declarations. Signature blocks below are API declarations;
application code is always labelled as a program-setup fragment and names its
variables or states the command pattern it depends on.

## Units to use

| Unit | Use it for |
| --- | --- |
| `CLI.Interfaces` | `ICommand`, `ICLIApplication`, parameter and progress interfaces |
| `CLI.Application` | Application factory, concrete debugging/completion support |
| `CLI.Command` | `TBaseCommand` and parameter registration helpers |
| `CLI.Parameter` | Low-level `ICommandParameter` construction |
| `CLI.Console` | Coloured terminal output and cursor control |
| `CLI.Progress` | Spinners and progress bars |
| `CLI.Errors` | Framework exception type |

## Application

### Create and run

```pascal
function CreateCLIApplication(const Name, Version: string): ICLIApplication; overload;
function CreateCLIApplication(const Name, Version: string;
  const RootCommand: ICommand): ICLIApplication; overload;
```

Use the two-argument overload for a command-first application. Use the
three-argument overload when an unnamed root command should run for
`myapp [options]`.

The following **program-setup fragment** needs `CLI.Interfaces` and
`CLI.Application`. It assumes `TRootCommand` is a declared `TBaseCommand`
descendant with an overridden `Execute`; `Root` is the instance passed to the
factory.

```pascal
var
  App: ICLIApplication;
  Root: TRootCommand;
begin
  Root := TRootCommand.Create('', 'Run the default action');
  App := CreateCLIApplication('myapp', '1.0.0', Root);
  Halt(App.Execute);
end.
```

`ICLIApplication` exposes:

```pascal
procedure RegisterCommand(const Command: ICommand);
function Execute: Integer;
```

`Execute` parses and validates arguments, services built-in requests, and
returns the selected command's exit code.

### Concrete application features

`TCLIApplication` also exposes `DebugMode`, `Version`, `RootCommand`, and
`Commands`. It has test-oriented helpers and deprecated completion callback
methods; those are not needed for ordinary applications. The callback methods
are no-op compatibility members in 1.x—see [limitations](limitations.md).

## Commands

### `TBaseCommand`

```pascal
{ Public members }
constructor Create(const AName, ADescription: string);
function Execute: Integer; virtual; abstract;
procedure AddSubCommand(const Command: ICommand);
```

Protected members for descendants:

```pascal
protected
function GetParameterValue(const Flag: string; out Value: string): Boolean;
```

Subclass `TBaseCommand`, override `Execute`, register options during setup,
and return `0` on success. Use an empty `AName` for a root command.

`GetParameterValue` is a protected member for command descendants, so call it
from your descendant's `Execute`. It finds either registered flag spelling and
returns values as strings.
Lookup is case-insensitive; a missing value returns `False` and clears the
output string.

### Register parameters

| Purpose | Signature |
| --- | --- |
| Generic parameter | `AddParameter(ShortFlag, LongFlag, Description, Required, ParamType, DefaultValue, AllowedValues)` |
| String | `AddStringParameter(ShortFlag, LongFlag, Description, Required = False, DefaultValue = '')` |
| Integer | `AddIntegerParameter(ShortFlag, LongFlag, Description, Required = False, DefaultValue = '')` |
| Float | `AddFloatParameter(ShortFlag, LongFlag, Description, Required = False, DefaultValue = '')` |
| Presence flag | `AddFlag(ShortFlag, LongFlag, Description, DefaultValue = 'false')` |
| Explicit Boolean | `AddBooleanParameter(ShortFlag, LongFlag, Description, Required, DefaultValue)` |
| Path | `AddPathParameter(ShortFlag, LongFlag, Description, Required = False, DefaultValue = '')` |
| Enum | `AddEnumParameter(ShortFlag, LongFlag, Description, AllowedValues, Required = False, DefaultValue = '')` |
| Date/time | `AddDateTimeParameter(ShortFlag, LongFlag, Description, Required = False, DefaultValue = '')` |
| Comma-separated items | `AddArrayParameter(ShortFlag, LongFlag, Description, Required = False, DefaultValue = '')` |
| Password | `AddPasswordParameter(ShortFlag, LongFlag, Description, Required = False)` |
| URL | `AddUrlParameter(ShortFlag, LongFlag, Description, Required = False, DefaultValue = '')` |

All parameters use `string` flag and description arguments. `AllowedValues` is
a pipe-separated string, such as `debug|info|warn`. See [options](options.md)
for validation behavior and [How do I...?](how-to.md) for minimal examples.

### Parameter kinds

`TParameterType` contains `ptString`, `ptInteger`, `ptFloat`, `ptBoolean`,
`ptPath`, `ptEnum`, `ptDateTime`, `ptArray`, `ptPassword`, and `ptUrl`.

The canonical date/time text is `YYYY-MM-DD HH:MM`; existing v1.x values with
seconds remain accepted for compatibility.

`ICommandParameter` exposes `ShortFlag`, `LongFlag`, `Description`, `Required`,
`ParamType`, `DefaultValue`, and `AllowedValues`. For lower-level registration,
use:

```pascal
function CreateParameter(const ShortFlag, LongFlag, Description: string;
  Required: Boolean; ParamType: TParameterType;
  const DefaultValue: string = ''; const AllowedValues: string = ''): ICommandParameter;
```

Then pass the result to `AddParameter(const Parameter: ICommandParameter)`.

## Command contracts

`ICommand` is the minimal command contract:

```pascal
function GetName: string;
function GetDescription: string;
function GetParameters: specialize TArray<ICommandParameter>;
function GetSubCommands: specialize TArray<ICommand>;
function Execute: Integer;
```

`TBaseCommand` implements it and is the normal choice when your command needs
framework-managed parameter lookup. A custom `ICommand` may also implement
`ICommandParameterReceiver.SetParsedParams` to receive parsed values.

## Terminal output

### `TConsole`

```pascal
class procedure Write(const Text: string); overload;
class procedure Write(const Text: string; const FgColor: TConsoleColor); overload;
class procedure WriteLn(const Text: string); overload;
class procedure WriteLn(const Text: string; const FgColor: TConsoleColor); overload;
```

For cursor-oriented terminal work, `TConsole` also provides foreground and
background colour setters, `ResetColors`, `ClearLine`, cursor movement, and
save/restore cursor methods. `TConsoleColor` offers standard and bright colour
values, including `ccGreen`, `ccYellow`, and `ccRed`.

## Progress

```pascal
function CreateSpinner(const Style: TSpinnerStyle = ssLine): IProgressIndicator;
function CreateProgressBar(const Total: Integer;
  const Width: Integer = 10): IProgressIndicator;
```

`IProgressIndicator` has `Start`, `Stop`, and
`Update(const Progress: Integer; const ACaption: string = '')`. Spinner styles
are `ssDots`, `ssLine`, `ssCircle`, `ssSquare`, `ssArrow`, `ssBounce`, and
`ssBar`. Use `Update` repeatedly during work, then call `Stop` in `finally`.

## Errors and built-in requests

`ECLIException` is the framework exception type in `CLI.Errors`. Catch it or a
broader `Exception` only where your application can add useful recovery or
context.

Applications receive `-h`/`--help`, `--help-complete`, and `-v`/`--version`.
When it is the first argument, `--completion-file` prints a Bash script and
`--completion-file-pwsh` prints a PowerShell script. See
[shell completion](completion.md) for usage.

Definitions are validated when commands are registered or parameters are
added. Invalid names, flags, duplicate siblings/options, nil commands, and
command-tree cycles raise developer-facing exceptions. The parser rejects
unexpected positional arguments with exit code `1`; it does not implement the
`--` terminator in v1.x.
