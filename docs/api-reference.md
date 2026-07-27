# CLI Framework API Reference

## Table of Contents

- [Overview](#overview)
- [Units](#units)
  - [CLI.Interfaces](#cliinterfaces)
  - [CLI.Application](#cliapplication)
  - [CLI.Command](#clicommand)
  - [CLI.Parameter](#cliparameter)
  - [CLI.Progress](#cliprogress)
  - [CLI.Console](#cliconsole)
  - [CLI.Errors](#clierrors)
- [Examples](#examples)
  - [Basic Examples](#basic-examples)
  - [Advanced Examples](#advanced-examples)

## Overview

The Free Pascal CLI Framework provides a comprehensive set of units for building command-line applications. Each unit serves a specific purpose and can be used independently or in combination with others.

## Units

### CLI.Interfaces

Core interfaces that define the framework's contract.

#### Types

##### `TParameterType`
Enum defining parameter types:
```pascal
TParameterType = (
  ptString,   // String value (e.g., --name "John Doe")
  ptInteger,  // Integer value (e.g., --count 42)
  ptFloat,    // Float value (e.g., --rate 3.14)
  ptBoolean,  // Boolean value (e.g., --verbose true/false)
  ptPath,     // File/directory path (e.g., --input /path/to/file)
  ptEnum,     // Enumerated value (e.g., --log-level debug|info|warn|error)
  ptDateTime, // Date/time value (e.g., --start "2024-01-01 12:00")
  ptArray,    // Comma-separated list (e.g., --tags tag1,tag2,tag3)
  ptPassword, // Sensitive value stored as a string; no automatic redaction
  ptUrl       // URL value with format validation (e.g., --repo https://github.com/user/repo)
);
```

#### Interfaces

##### `ICommand`
Represents a CLI command or subcommand.
```pascal
ICommand = interface
  function GetName: string;
  function GetDescription: string;
  function GetParameters: specialize TArray<ICommandParameter>;
  function GetSubCommands: specialize TArray<ICommand>;
  function Execute: Integer;
  
  property Name: string read GetName;
  property Description: string read GetDescription;
  property Parameters: specialize TArray<ICommandParameter> read GetParameters;
  property SubCommands: specialize TArray<ICommand> read GetSubCommands;
end;
```

### Parameter Helper Methods

The `TBaseCommand` class provides helper methods for adding parameters. All helper methods are available on `TBaseCommand` and its descendants:

```pascal
// String parameter
procedure AddStringParameter(const ShortFlag, LongFlag, Description: string; 
  Required: Boolean = False; const DefaultValue: string = '');

// Integer parameter
procedure AddIntegerParameter(const ShortFlag, LongFlag, Description: string;
  Required: Boolean = False; const DefaultValue: string = '');

// Float parameter
procedure AddFloatParameter(const ShortFlag, LongFlag, Description: string;
  Required: Boolean = False; const DefaultValue: string = '');

// Boolean flag (defaults to false, becomes true when flag is present)
procedure AddFlag(const ShortFlag, LongFlag, Description: string;
  const DefaultValue: string = 'false');

// Boolean parameter (explicit true/false)
procedure AddBooleanParameter(const ShortFlag, LongFlag, Description: string;
  Required: Boolean; const DefaultValue: string);

// URL parameter
procedure AddUrlParameter(const ShortFlag, LongFlag, Description: string;
  Required: Boolean = False; const DefaultValue: string = '');

// Enum parameter
procedure AddEnumParameter(const ShortFlag, LongFlag, Description: string;
  const AllowedValues: string; Required: Boolean = False; const DefaultValue: string = '');

// DateTime parameter
procedure AddDateTimeParameter(const ShortFlag, LongFlag, Description: string;
  Required: Boolean = False; const DefaultValue: string = '');

// Array parameter (comma-separated values)
procedure AddArrayParameter(const ShortFlag, LongFlag, Description: string;
  Required: Boolean = False; const DefaultValue: string = '');

// Password parameter (no default-value argument)
procedure AddPasswordParameter(const ShortFlag, LongFlag, Description: string;
  Required: Boolean = False);

// Path parameter
procedure AddPathParameter(const ShortFlag, LongFlag, Description: string;
  Required: Boolean = False; const DefaultValue: string = '');
```

Each helper method:
- Creates a parameter of the appropriate type
- Handles default values appropriately
- Adds the parameter to the command's parameter list
- Validates values according to the parameter type

`AddDateTimeParameter` labels its help text with
`YYYY-MM-DD HH:MM:SS`, while validation currently delegates to
`TryStrToDateTime` with `yyyy-mm-dd` date settings. Consequently, date-only
values and values with or without seconds may also be accepted. Treat
`YYYY-MM-DD HH:MM` as the recommended portable input until validation is made
strict.

`AddPasswordParameter` marks a parameter as sensitive, but retrieved values are
ordinary strings. The framework does not automatically redact values written
by application code or external logging.

#### Getting Parameter Values

`TBaseCommand` currently exposes a single `GetParameterValue` overload:

```pascal
function GetParameterValue(const Flag: string; out Value: string): Boolean;
```
- For non-boolean parameters, it returns `True` when the parameter was provided or a default value exists.
- For boolean parameters, it writes `true`, `false`, or the configured default
  to `Value`. It returns `True` when the parameter was provided or has a
  non-empty default. Because `AddFlag` defaults to the string `'false'`, an
  absent standard flag normally still returns `True`; inspect `Value` rather
  than using the Boolean return to test whether the flag was explicitly typed.
- Convert string results yourself with helpers such as `TryStrToInt`, `TryStrToFloat`, and `SameText`.

Example usage:
```pascal
type
  TTestCommand = class(TBaseCommand)
  public
    function Execute: Integer; override;
  end;

function TTestCommand.Execute: Integer;
var
  PersonName, CountStr, RateStr, Level, VerboseStr, DateStr, Url, Tags, ApiKey: string;
  Count: Integer;
  Rate: Double;
begin
  // Get parameter values using the string-based helper
  if GetParameterValue('--name', PersonName) then
    WriteLn('Name: ', PersonName);

  if GetParameterValue('--count', CountStr) and TryStrToInt(CountStr, Count) then
    WriteLn('Count: ', Count);

  if GetParameterValue('--rate', RateStr) and TryStrToFloat(RateStr, Rate) then
    WriteLn('Rate: ', Rate:0:2);

  if GetParameterValue('--level', Level) then
    WriteLn('Level: ', Level);

  if GetParameterValue('--verbose', VerboseStr) and SameText(VerboseStr, 'true') then
    WriteLn('Verbose: true');

  if GetParameterValue('--date', DateStr) then
    WriteLn('Date: ', DateStr);

  if GetParameterValue('--url', Url) then
    WriteLn('URL: ', Url);

  if GetParameterValue('--tags', Tags) then
    WriteLn('Tags: ', Tags);

  if GetParameterValue('--api-key', ApiKey) then
    WriteLn('API key provided');

  Result := 0;
end;

var
  App: ICLIApplication;
  Cmd: TTestCommand;
begin
  App := CreateCLIApplication('TestApp', '1.0.0');
  
  // Create command and add parameters
  Cmd := TTestCommand.Create('test', 'Test parameters');
  
  // Add various parameters
  Cmd.AddStringParameter('-n', '--name', 'Your name');
  Cmd.AddIntegerParameter('-c', '--count', 'Number of items', True);  // Required
  Cmd.AddFloatParameter('-r', '--rate', 'Processing rate', False, '1.0');
  Cmd.AddFlag('-v', '--verbose', 'Enable verbose output');
  Cmd.AddDateTimeParameter('-d', '--date', 'Start date');  // Recommended: YYYY-MM-DD HH:MM
  Cmd.AddEnumParameter('-l', '--level', 'Log level', 'debug|info|warn|error');
  Cmd.AddUrlParameter('-u', '--url', 'Repository URL');
  Cmd.AddArrayParameter('-t', '--tags', 'Tag list', False, 'tag1,tag2');
  Cmd.AddPasswordParameter('-k', '--api-key', 'API Key', True);
  
  // Register command
  App.RegisterCommand(Cmd);
  
  // Execute application
  Halt(App.Execute);
end;
```

##### `ICommandParameter`
Represents a command parameter.
```pascal
ICommandParameter = interface
  function GetShortFlag: string;
  function GetLongFlag: string;
  function GetDescription: string;
  function GetRequired: Boolean;
  function GetParamType: TParameterType;
  function GetDefaultValue: string;
  function GetAllowedValues: string;
  
  property ShortFlag: string read GetShortFlag;
  property LongFlag: string read GetLongFlag;
  property Description: string read GetDescription;
  property Required: Boolean read GetRequired;
  property ParamType: TParameterType read GetParamType;
  property DefaultValue: string read GetDefaultValue;
  property AllowedValues: string read GetAllowedValues;
end;
```

##### `IProgressIndicator`
Interface for progress indicators.
```pascal
IProgressIndicator = interface
  procedure Start;
  procedure Stop;
  procedure Update(const Progress: Integer; const ACaption: string = '');
end;
```
- `ACaption` is optional and renders inline status text next to the spinner/progress bar for that update.

##### `ICLIApplication`
Main application interface.
```pascal
ICLIApplication = interface
  procedure RegisterCommand(const Command: ICommand);
  function Execute: Integer;
end;
```

### CLI.Application

Core application functionality implementation.

#### Types

##### `TCLIApplication`
Main application class implementing `ICLIApplication`.

```pascal
TCLIApplication = class(TInterfacedObject, ICLIApplication)
public
  property DebugMode: Boolean read FDebugMode write FDebugMode;
  property Version: string read FVersion;
  property RootCommand: ICommand read FRootCommand;
  property Commands: TCommandList read GetCommands;
end;
```

#### Functions

##### `CreateCLIApplication`
Creates a new CLI application instance.
```pascal
function CreateCLIApplication(const Name, Version: string): ICLIApplication;
function CreateCLIApplication(const Name, Version: string;
  const RootCommand: ICommand): ICLIApplication;
```

The two-argument overload preserves the traditional command-first behavior:
invoking the executable without arguments displays general help.

The three-argument overload configures an optional executable root command.
The root command runs when no arguments are supplied or when the first
argument is a non-global option:

```pascal
Root := TGreetCommand.Create('', 'Greet someone');
Root.AddStringParameter('-n', '--name', 'Name to greet', False, 'World');
App := CreateCLIApplication('MyApp', '1.0.0', Root);
```

This supports `MyApp` and `MyApp --name Gus` without requiring a command name.
Registered named commands still take precedence when the first argument is a
command token. Root parameters are local to root execution. Positional
arguments and inherited/persistent root flags are not currently modeled.

Application-level dispatch retains its existing ordering: sole `--help`/`-h`,
`--help-complete`, and `--version`/`-v` requests are handled before root
selection; completion-script options are handled when they are the first
argument; and command help is recognized after a named or root command has
been selected.

### CLI.Command

Base command implementation.

#### Types

##### `TBaseCommand`
Abstract base class for all CLI commands.
```pascal
TBaseCommand = class(TInterfacedObject, ICommand)
public
  constructor Create(const AName, ADescription: string);
  procedure AddParameter(const Parameter: ICommandParameter);
  procedure AddSubCommand(const Command: ICommand);
  procedure SetParsedParams(const Params: TStringList);
  function Execute: Integer; virtual; abstract;
  
  property Name: string read GetName;
  property Description: string read GetDescription;
  property Parameters: specialize TArray<ICommandParameter> read GetParameters;
  property SubCommands: specialize TArray<ICommand> read GetSubCommands;
end;
```

### CLI.Parameter

Parameter handling implementation.

#### Types

##### `TCommandParameter`
Implements command parameter functionality.
```pascal
TCommandParameter = class(TInterfacedObject, ICommandParameter)
public
  constructor Create(const AShortFlag, ALongFlag, ADescription: string;
    ARequired: Boolean; AParamType: TParameterType;
    const ADefaultValue: string = ''; const AAllowedValues: string = '');
    
  property ShortFlag: string read GetShortFlag;
  property LongFlag: string read GetLongFlag;
  property Description: string read GetDescription;
  property Required: Boolean read GetRequired;
  property ParamType: TParameterType read GetParamType;
  property DefaultValue: string read GetDefaultValue;
  property AllowedValues: string read GetAllowedValues;
end;
```

#### Functions

##### `CreateParameter`
Creates a new parameter instance.
```pascal
function CreateParameter(const ShortFlag, LongFlag, Description: string;
  Required: Boolean; ParamType: TParameterType;
  const DefaultValue: string = '';
  const AllowedValues: string = ''): ICommandParameter;
```

### CLI.Progress

Progress indicator implementations.

#### Types

##### `TSpinnerStyle`
Enum defining spinner animation styles:
```pascal
TSpinnerStyle = (
  ssDots,    // ⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏
  ssLine,    // -\|/
  ssCircle,  // ◐◓◑◒
  ssSquare,  // ◰◳◲◱
  ssArrow,   // ←↖↑↗→↘↓↙
  ssBounce,  // ⠁⠂⠄⠂
  ssBar      // ▏▎▍▌▋▊▉█▊▋▌▍▎▏
);
```

##### `TProgressIndicator`
Base class for progress indicators.
```pascal
TProgressIndicator = class(TInterfacedObject, IProgressIndicator)
public
  procedure Start; virtual;
  procedure Stop; virtual;
  procedure Update(const Progress: Integer; const ACaption: string = ''); virtual; abstract;
end;
```

##### `TSpinner`
Animated spinner progress indicator.
```pascal
TSpinner = class(TProgressIndicator)
public
  constructor Create(const AStyle: TSpinnerStyle);
  procedure Update(const Progress: Integer; const ACaption: string = ''); override;
end;
```

##### `TProgressBar`
Visual progress bar indicator.
```pascal
TProgressBar = class(TProgressIndicator)
public
  constructor Create(const ATotal: Integer; const AWidth: Integer = 10);
  procedure Update(const Progress: Integer; const ACaption: string = ''); override;
end;
```

#### Functions

##### `CreateSpinner`
Creates a new spinner progress indicator.
```pascal
function CreateSpinner(const Style: TSpinnerStyle = ssLine): IProgressIndicator;
```

##### `CreateProgressBar`
Creates a new progress bar indicator.
```pascal
function CreateProgressBar(const Total: Integer; const Width: Integer = 10): IProgressIndicator;
```
- `Total`: The total number of steps (required)
- `Width`: The width of the progress bar in characters (optional)

### CLI.Console

Console output functionality with color support.

#### Types

##### `TConsoleColor`
Enum defining console colors:
```pascal
TConsoleColor = (
  ccBlack, ccBlue, ccGreen, ccCyan, 
  ccRed, ccMagenta, ccYellow, ccWhite,
  ccBrightBlack, ccBrightBlue, ccBrightGreen, ccBrightCyan,
  ccBrightRed, ccBrightMagenta, ccBrightYellow, ccBrightWhite
);
```

##### `TConsole`
Static class for console operations.
```pascal
TConsole = class
public
  class procedure SetForegroundColor(const Color: TConsoleColor);
  class procedure SetBackgroundColor(const Color: TConsoleColor);
  class procedure ResetColors;
  class procedure ClearLine;
  class procedure MoveCursorUp(const Lines: Integer = 1);
  class procedure MoveCursorDown(const Lines: Integer = 1);
  class procedure MoveCursorLeft(const Columns: Integer = 1);
  class procedure MoveCursorRight(const Columns: Integer = 1);
  class procedure SaveCursorPosition;
  class procedure RestoreCursorPosition;
  class procedure Write(const Text: string); overload;
  class procedure Write(const Text: string; const FgColor: TConsoleColor); overload;
  class procedure WriteLn(const Text: string); overload;
  class procedure WriteLn(const Text: string; const FgColor: TConsoleColor); overload;
end;
```

### CLI.Errors

Exception hierarchy for error handling.

#### Types

##### `ECLIException`
Base exception class for all CLI-related errors.
```pascal
ECLIException = class(Exception)
public
  constructor Create(const Msg: string);
  constructor CreateFmt(const Msg: string; const Args: array of const);
end;
```

## Examples

### Basic Examples

#### 1. Simple Command with Parameter

```pascal
type
  TGreetCommand = class(TBaseCommand)
  public
    function Execute: Integer; override;
  end;

function TGreetCommand.Execute: Integer;
var
  PersonName: string;
begin
  GetParameterValue('--name', PersonName);
  TConsole.WriteLn('Hello, ' + PersonName + '!');

  Result := 0;
end;

// Usage
var
  App: ICLIApplication;
  Cmd: TGreetCommand;
begin
  App := CreateCLIApplication('MyApp', '1.0.0');
  
  Cmd := TGreetCommand.Create('greet', 'Greet a person');
  Cmd.AddStringParameter('-n', '--name', 'Name to greet', False, 'World');
  
  App.RegisterCommand(Cmd);
  Halt(App.Execute);
end;
```

#### 2. Command with Multiple Parameters

```pascal
type
  TCopyCommand = class(TBaseCommand)
  public
    function Execute: Integer; override;
  end;

function TCopyCommand.Execute: Integer;
var
  Source, Dest, ForceValue: string;
  Force: Boolean;
begin
  // Get required parameters
  if not GetParameterValue('--source', Source) then
  begin
    TConsole.WriteLn('Error: Source file is required', ccRed);
    Exit(1);
  end;
  
  if not GetParameterValue('--dest', Dest) then
  begin
    TConsole.WriteLn('Error: Destination is required', ccRed);
    Exit(1);
  end;
  
  // Get optional boolean flag as text, then interpret it
  GetParameterValue('--force', ForceValue);
  Force := SameText(ForceValue, 'true');
  
  // Show operation details
  TConsole.WriteLn('Copying file:', ccCyan);
  TConsole.WriteLn('  From: ' + Source);
  TConsole.WriteLn('  To: ' + Dest);
  if Force then
    TConsole.WriteLn('  Force: Yes', ccYellow);
    
  Result := 0;
end;

// Usage
var
  App: ICLIApplication;
  Cmd: TCopyCommand;
begin
  App := CreateCLIApplication('MyApp', '1.0.0');
  
  Cmd := TCopyCommand.Create('copy', 'Copy a file');
  
  // Add required parameters
  Cmd.AddStringParameter('-s', '--source', 'Source file path', True);
  
  Cmd.AddStringParameter('-d', '--dest', 'Destination path', True);
  
  // Add optional flag
  Cmd.AddFlag('-f', '--force', 'Overwrite if exists');
  
  App.RegisterCommand(Cmd);
  Halt(App.Execute);
end;
```

#### 3. Handling Boolean Parameters

```pascal
type
  TTestCommand = class(TBaseCommand)
  public
    function Execute: Integer; override;
  end;

function TTestCommand.Execute: Integer;
var
  ForceValue, VerboseValue: string;
  IsForced, IsVerbose: Boolean;
begin
  // Get boolean results as strings, then interpret them
  GetParameterValue('--force', ForceValue);
  IsForced := SameText(ForceValue, 'true');
  if IsForced then
    TConsole.WriteLn('Force flag is enabled', ccGreen)
  else
    TConsole.WriteLn('Force flag is disabled', ccYellow);

  GetParameterValue('--verbose', VerboseValue);
  IsVerbose := SameText(VerboseValue, 'true');
  if IsVerbose then
    TConsole.WriteLn('Verbose mode is ON', ccGreen)
  else
    TConsole.WriteLn('Verbose mode is OFF', ccYellow);
    
  Result := 0;
end;

// Usage
var
  App: ICLIApplication;
  Cmd: TTestCommand;
begin
  App := CreateCLIApplication('MyApp', '1.0.0');
  
  Cmd := TTestCommand.Create('test', 'Test parameters');
  
  // Flag (true when present, false by default)
  Cmd.AddFlag('-f', '--force', 'Force operation');
  
  // Boolean (requires explicit true/false value)
  Cmd.AddBooleanParameter('-v', '--verbose', 'Verbose mode', False, 'false');
  
  App.RegisterCommand(Cmd);
  Halt(App.Execute);
end;
```

Command-line usage:
```bash
# Flag usage (AddFlag)
myapp test --force  # Flag is present (true)
myapp test          # Flag is not present (false)

# Boolean usage (AddBooleanParameter)
myapp test --verbose=true   # Explicitly set to true
myapp test --verbose=false  # Explicitly set to false
myapp test                  # Uses default value (false)
```

### Advanced Examples

#### 1. Progress Indicator Example

```pascal
type
  TProcessCommand = class(TBaseCommand)
  private
    procedure ProcessFile(const FileName: string);
  public
    function Execute: Integer; override;
  end;

function TProcessCommand.Execute: Integer;
var
  CountStr, VerboseStr: string;
  Count: Integer;
  Verbose: Boolean;
  Progress: IProgressIndicator;
  i: Integer;
begin
  GetParameterValue('--count', CountStr);
  TryStrToInt(CountStr, Count);
  GetParameterValue('--verbose', VerboseStr);
  Verbose := SameText(VerboseStr, 'true');

  Progress := CreateProgressBar(Count);
  try
    Progress.Start;
    
    for i := 0 to Count - 1 do
    begin
      if Verbose then
        TConsole.WriteLn(Format('Processing file %d/%d...', [i + 1, Count]), ccCyan);
        
      ProcessFile('file' + IntToStr(i + 1));
      Progress.Update(i + 1, 'Processing files');
      Sleep(500); // Simulate work
    end;

    TConsole.WriteLn('All files processed successfully!', ccGreen);
    Result := 0;
  finally
    Progress.Stop;
  end;
end;

procedure TProcessCommand.ProcessFile(const FileName: string);
begin
  // Simulate file processing
  Sleep(100);
end;

{ Main program }
var
  App: ICLIApplication;
  Cmd: TProcessCommand;
begin
  App := CreateCLIApplication('MyApp', '1.0.0');
  
  // Create command and add parameters - simple and straightforward
  Cmd := TProcessCommand.Create('process', 'Process files');
  Cmd.AddIntegerParameter('-c', '--count', 'Number of files to process', False, '5');
  Cmd.AddFlag('-v', '--verbose', 'Show detailed progress');
  
  App.RegisterCommand(Cmd);
  Halt(App.Execute);
end;
```

#### 2. Error Handling Example

```pascal
type
  TValidateCommand = class(TBaseCommand)
  private
    procedure ValidateFile(const FilePath: string);
  public
    function Execute: Integer; override;
  end;

function TValidateCommand.Execute: Integer;
var
  Path, StopOnErrorValue: string;
  StopOnError: Boolean;
  ErrorCount: Integer;
  i: Integer;
begin
  GetParameterValue('--path', Path);
  GetParameterValue('--stop-on-error', StopOnErrorValue);
  StopOnError := SameText(StopOnErrorValue, 'true');
  ErrorCount := 0;

  for i := 1 to 10 do
  begin
    Write(Format('Validating %s\file%d.txt... ', [Path, i]));
    try
      ValidateFile(Path + '\file' + IntToStr(i) + '.txt');
      TConsole.WriteLn('OK', ccGreen);
    except
      on E: Exception do
      begin
        Inc(ErrorCount);
        TConsole.WriteLn('ERROR: ' + E.Message, ccRed);
        if StopOnError then
        begin
          TConsole.WriteLn('Stopping due to error (--stop-on-error)', ccYellow);
          Exit(1);
        end;
      end;
    end;
  end;

  if ErrorCount > 0 then
    TConsole.WriteLn(Format('Validation complete with %d errors', [ErrorCount]), ccYellow)
  else
    TConsole.WriteLn('All files validated successfully!', ccGreen);

  Result := ErrorCount;
end;

procedure TValidateCommand.ValidateFile(const FilePath: string);
begin
  // Demo validation - fail on file9.txt
  if Pos('file9.txt', FilePath) > 0 then
    raise Exception.Create('Demo validation failed for: ' + FilePath);
end;

{ Main program }
var
  App: ICLIApplication;
  Cmd: TValidateCommand;
begin
  App := CreateCLIApplication('MyApp', '1.0.0');
  
  // Create command and add parameters - simple and straightforward
  Cmd := TValidateCommand.Create('validate', 'Validate files');
  Cmd.AddPathParameter('-p', '--path', 'Path to validate', True);
  Cmd.AddFlag('-s', '--stop-on-error', 'Stop processing on first error');
  
  App.RegisterCommand(Cmd);
  Halt(App.Execute);
end;
```

### Completion Script Generation

The framework can generate robust, context-aware completion scripts for both Bash and PowerShell:

#### Bash Completion
- Generate with:
  ```bash
  ./yourcli --completion-file > myapp-completion.sh
  ```
- **Root level:** Root parameters and all application options are available
  after an option prefix, including `--help`, `-h`, `--help-complete`,
  `--version`, `-v`, `--completion-file`, and `--completion-file-pwsh`.
- **Named command levels:** An empty new token offers subcommands, command
  parameters, and help. Starting an option prefix also exposes `--version` and
  `-v`, matching the current completion engine.
- Command, subcommand, and parameter candidates are scoped to the resolved
  command path. Built-in application flags follow the behavior described
  above.
- **Automatic value completion:** Boolean parameters automatically complete with `true`/`false`, and enum parameters complete with their allowed values.
- **No file completion is ever offered.**

#### PowerShell Completion
- Generate with:
  ```powershell
  ./yourcli.exe --completion-file-pwsh > myapp-completion.ps1
  ```
- **Context-aware command metadata:** Commands, subcommands, and parameters are
  scoped to the resolved command path; built-in application flags follow the
  same behavior as Bash completion
- **No file fallback:** Only valid completions are shown (never files)
- **Automatic value completion:** Boolean parameters automatically complete with `true`/`false`, and enum parameters complete with their allowed values.
- **PowerShell compatibility:** Uses `Register-ArgumentCompleter`; PowerShell
  7+ additionally receives native executable registration.

See the user manual for setup, usage, and safe sourcing instructions.
