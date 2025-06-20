# CLI Framework API Reference

## Table of Contents

- [Overview](#overview)
- [Quick Start](#quick-start)
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
  ptPassword, // Sensitive value, masked in help/logs (e.g., --api-key ***)
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
  function GetParameters: TArray<ICommandParameter>;
  function GetSubCommands: TArray<ICommand>;
  function Execute: Integer;
  
  property Name: string read GetName;
  property Description: string read GetDescription;
  property Parameters: TArray<ICommandParameter> read GetParameters;
  property SubCommands: TArray<ICommand> read GetSubCommands;
end;
```

### Parameter Helper Methods

The `TBaseCommand` class provides helper methods for adding parameters:

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
procedure AddFlag(const ShortFlag, LongFlag, Description: string);

// Boolean parameter (explicit true/false)
procedure AddBooleanParameter(const ShortFlag, LongFlag, Description: string;
  Required: Boolean = False; const DefaultValue: string = 'false');

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

// Password parameter (masked in output)
procedure AddPasswordParameter(const ShortFlag, LongFlag, Description: string;
  Required: Boolean = False; const DefaultValue: string = '');

// Path parameter
procedure AddPathParameter(const ShortFlag, LongFlag, Description: string;
  Required: Boolean = False; const DefaultValue: string = '');
```

Each helper method:
- Creates a parameter of the appropriate type
- Handles default values appropriately
- Adds the parameter to the command's parameter list
- Validates values according to the parameter type

Example usage:
```pascal
type
  TTestCommand = class(TBaseCommand)
  public
    function Execute: Integer; override;
  end;

function TTestCommand.Execute: Integer;
var
  Name: string;
  Count: Integer;
  Rate: Double;
  Level: string;
begin
  // Get parameter values using helper methods
  if GetParameterValue('--name', Name) then
    WriteLn('Name: ', Name);
    
  if GetParameterValue('--count', Count) then
    WriteLn('Count: ', Count);
    
  if GetParameterValue('--rate', Rate) then
    WriteLn('Rate: ', Rate:0:2);
    
  if GetParameterValue('--level', Level) then
    WriteLn('Level: ', Level);

  if GetParameterValue('--verbose', Verbose) then
    WriteLn('Verbose: ', Verbose);
    
  if GetParameterValue('--date', DateStr) then
    WriteLn('Date: ', DateStr);
    
  if GetParameterValue('--url', Url) then
    WriteLn('URL: ', Url);
    
  if GetParameterValue('--tags', Tags) then
    WriteLn('Tags: ', Tags);
    
  if GetParameterValue('--api-key', ApiKey) then
    WriteLn('API Key: ', ApiKey);

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
  Cmd.AddDateTimeParameter('-d', '--date', 'Start date');  // Format: YYYY-MM-DD HH:MM
  Cmd.AddEnumParameter('-l', '--level', 'Log level', 'debug|info|warn|error');
  Cmd.AddUrlParameter('-u', '--url', 'Repository URL');
  Cmd.AddArrayParameter('-t', '--tags', 'Tag list', False, 'tag1,tag2');
  Cmd.AddPasswordParameter('-k', '--api-key', 'API Key', True);
  
  // Register command
  App.RegisterCommand(Cmd);
  
  // Execute application
  ExitCode := App.Execute;
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
  
  property ShortFlag: string read GetShortFlag;
  property LongFlag: string read GetLongFlag;
  property Description: string read GetDescription;
  property Required: Boolean read GetRequired;
  property ParamType: TParameterType read GetParamType;
  property DefaultValue: string read GetDefaultValue;
end;
```

##### `IProgressIndicator`
Interface for progress indicators.
```pascal
IProgressIndicator = interface
  procedure Start;
  procedure Stop;
  procedure Update(const Progress: Integer);
end;
```

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
  property Commands: TCommandList read GetCommands;
end;
```

#### Functions

##### `CreateCLIApplication`
Creates a new CLI application instance.
```pascal
function CreateCLIApplication(const Name, Version: string): ICLIApplication;
```

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
  property Parameters: TArray<ICommandParameter> read GetParameters;
  property SubCommands: TArray<ICommand> read GetSubCommands;
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
    ARequired: Boolean; AParamType: TParameterType; const ADefaultValue: string = '');
    
  property ShortFlag: string read GetShortFlag;
  property LongFlag: string read GetLongFlag;
  property Description: string read GetDescription;
  property Required: Boolean read GetRequired;
  property ParamType: TParameterType read GetParamType;
  property DefaultValue: string read GetDefaultValue;
end;
```

#### Functions

##### `CreateParameter`
Creates a new parameter instance.
```pascal
function CreateParameter(const ShortFlag, LongFlag, Description: string;
  Required: Boolean; ParamType: TParameterType; const DefaultValue: string = ''): ICommandParameter;
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
  ssArrow,   // ←↖↑→↘↓↙
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
  procedure Update(const Progress: Integer); virtual; abstract;
end;
```

##### `TSpinner`
Animated spinner progress indicator.
```pascal
TSpinner = class(TProgressIndicator)
public
  constructor Create(const AStyle: TSpinnerStyle);
  procedure Update(const Progress: Integer); override;
end;
```

##### `TProgressBar`
Visual progress bar indicator.
```pascal
TProgressBar = class(TProgressIndicator)
public
  constructor Create(const ATotal: Integer; const AWidth: Integer = 10);
  procedure Update(const Progress: Integer); override;
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
  constructor Create(const Msg: string); override;
  constructor CreateFmt(const Msg: string; const Args: array of const); override;
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
  Name: string;
begin
  GetParameterValue('--name', Name);
  TConsole.WriteLn('Hello, ' + Name + '!', ccDefault);

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
  ExitCode := App.Execute;
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
  Source, Dest: string;
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
  
  // Get optional parameter
  Force := GetParameterValue('--force', Force);
  
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
  ExitCode := App.Execute;
end;
```

#### 1. Handling Boolean Parameters

```pascal
type
  TTestCommand = class(TBaseCommand)
  public
    function Execute: Integer; override;
  end;

function TTestCommand.Execute: Integer;
var
  IsForced, IsVerbose: Boolean;
begin
  // Get flag value (true when present)
  GetParameterValue('--force', IsForced);
  if IsForced then
    TConsole.WriteLn('Force flag is enabled', ccGreen)
  else
    TConsole.WriteLn('Force flag is disabled', ccYellow);

  // Get boolean value (explicit true/false)
  GetParameterValue('--verbose', IsVerbose);
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
  
  // Flag (defaults to true when present)
  Cmd.AddFlag('-f', '--force', 'Force operation');
  
  // Boolean (requires explicit true/false value)
  Cmd.AddBooleanParameter('-v', '--verbose', 'Verbose mode', False, 'false');
  
  App.RegisterCommand(Cmd);
  ExitCode := App.Execute;
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
  Count: Integer;
  Verbose: Boolean;
  Progress: IProgressIndicator;
  i: Integer;
begin
  GetParameterValue('--count', Count);
  GetParameterValue('--verbose', Verbose);

  Progress := CreateProgressBar('Processing files', Count);
  try
    Progress.Start;
    
    for i := 0 to Count - 1 do
    begin
      if Verbose then
        TConsole.WriteLn(Format('Processing file %d/%d...', [i + 1, Count]), ccCyan);
        
      ProcessFile('file' + IntToStr(i + 1));
      Progress.Update(i + 1);
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
  ExitCode := App.Execute;
end;

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
  Path: string;
  StopOnError: Boolean;
  ErrorCount: Integer;
  i: Integer;
begin
  GetParameterValue('--path', Path);
  GetParameterValue('--stop-on-error', StopOnError);
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
  if FilePath.Contains('file9.txt') then
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
  ExitCode := App.Execute;
end;
``` 