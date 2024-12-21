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

## Quick Start

### 1. Installation

Add the framework to your project:
```pascal
uses
  CLI.Interfaces,
  CLI.Application,
  CLI.Command,
  CLI.Parameter,
  CLI.Progress,  // Optional: For progress indicators
  CLI.Console;   // Optional: For colored output
```

### 2. Create a Simple Command

```pascal
type
  THelloCommand = class(TBaseCommand)
  public
    function Execute: Integer; override;
  end;

function THelloCommand.Execute: Integer;
begin
  TConsole.WriteLn('Hello, World!', ccGreen);
  Result := 0;
end;
```

### 3. Create and Run Application

```pascal
var
  App: ICLIApplication;
  Cmd: THelloCommand;
begin
  App := CreateCLIApplication('MyApp', '1.0.0');
  Cmd := THelloCommand.Create('hello', 'Display greeting');
  App.RegisterCommand(Cmd);
  ExitCode := App.Execute;
end.
```

### 4. Run Your App

```bash
# Show help
myapp --help

# Run command
myapp hello
```

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
  ptFlag      // Flag parameter, no value needed (e.g., --force)
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
  if GetParameterValue('--name', Name) then
    TConsole.WriteLn('Hello, ' + Name + '!', ccGreen)
  else
    TConsole.WriteLn('Hello, World!', ccGreen);
  Result := 0;
end;

// Usage
var
  App: ICLIApplication;
  Cmd: TGreetCommand;
begin
  App := CreateCLIApplication('MyApp', '1.0.0');
  
  Cmd := TGreetCommand.Create('greet', 'Greet a person');
  Cmd.AddParameter(CreateParameter(
    '-n',
    '--name',
    'Name to greet',
    False,
    ptString
  ));
  
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
  Cmd.AddParameter(CreateParameter(
    '-s',
    '--source',
    'Source file path',
    True,     // Required
    ptString
  ));
  
  Cmd.AddParameter(CreateParameter(
    '-d',
    '--dest',
    'Destination path',
    True,     // Required
    ptString
  ));
  
  // Add optional flag
  Cmd.AddParameter(CreateParameter(
    '-f',
    '--force',
    'Overwrite if exists',
    False,    // Optional
    ptBoolean,
    'false'   // Default value
  ));
  
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
  VerboseStr: string;
  IsForced, IsVerbose: Boolean;
begin
  // Check if force flag is present (ptFlag)
  IsForced := GetParameterValue('--force', VerboseStr); // VerboseStr not used for flags
  if IsForced then
    TConsole.WriteLn('Force flag is present', ccGreen)
  else
    TConsole.WriteLn('Force flag is not present', ccYellow);

  // Get boolean value (ptBoolean)
  IsVerbose := False; // Default value
  if GetParameterValue('--verbose', VerboseStr) then
    IsVerbose := StrToBoolDef(VerboseStr, False);
  
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
  
  // Flag parameter (just presence)
  Cmd.AddParameter(CreateParameter(
    '-f',
    '--force',
    'Force operation',
    False,    // Optional
    ptFlag    // Just checks presence
  ));
  
  // Boolean parameter (true/false value)
  Cmd.AddParameter(CreateParameter(
    '-v',
    '--verbose',
    'Verbose mode (true/false)',
    False,     // Optional
    ptBoolean, // Requires true/false value
    'false'    // Default value
  ));
  
  App.RegisterCommand(Cmd);
  ExitCode := App.Execute;
end;
```

Command-line usage:
```bash
# Flag parameter (ptFlag)
myapp test --force         # Flag is present
myapp test                 # Flag is not present

# Boolean parameter (ptBoolean)
myapp test --verbose=true  # Boolean is true
myapp test --verbose=false # Boolean is false
myapp test                 # Uses default value
```

### Advanced Examples

#### 1. Long-Running Operation with Progress

```pascal
{
  Long Running Operation Demo

  This example demonstrates how to create a command-line application that shows progress
  indicators for long-running operations. It showcases several key features of the CLI framework:

  1. Progress indicators (spinner and progress bar)
  2. Command parameters (verbose flag)
  3. Colored console output
  4. Basic command structure

  How to run:
  Option 1 - Basic usage with progress bar only:
  
  $ LongRunningOpDemo.exe process
  Finding files...
  . 
  [====================] 100% All files processed successfully!
  

  Option 2 - Verbose mode shows detailed progress:
  
  $ LongRunningOpDemo.exe process -v true
  Finding files...
  .
  Processing: file1.txt
  [=======             ]  33% Processing: file2.txt
  [=============       ]  67% Processing: file3.txt
  [====================] 100% All files processed successfully!
  
}
program LongRunningOpDemo;

{ Compiler directives:
  - $mode objfpc: Use Object Pascal mode for modern OOP features
  - $H+: Use long strings (AnsiString) instead of short strings
  - $J-: Disable writeable typed constants for safety }
{$mode objfpc}{$H+}{$J-}

{ Import required units from the CLI framework }
uses
  SysUtils,
  Classes,
  CLI.Interfaces,    // Core interfaces
  CLI.Application,   // Application creation
  CLI.Command,       // Command base class
  CLI.Parameter,     // Parameter handling
  CLI.Progress,      // Progress indicators
  CLI.Console;       // Colored output

type
  { Define a command that processes files with progress indication }
  TProcessCommand = class(TBaseCommand)
  private
    { Simulates processing a single file }
    procedure ProcessFile(const FileName: string);
  public
    { Main execution method for the command }
    function Execute: integer; override;
  end;

  function TProcessCommand.Execute: integer;
  var
    Files: TStringList;
    Progress: IProgressIndicator;    // Progress bar interface
    Spinner: IProgressIndicator;     // Spinner interface for file search
    i, FileCount: integer;
    VerboseStr: string;             // Raw parameter value
    Verbose: boolean;               // Parsed verbose flag
  begin
    Files := TStringList.Create;
    try
      // Simulate finding files
      FileCount := 5;  // We know we'll add 5 files
      TConsole.WriteLn('Finding files...', ccCyan);
      Spinner := CreateSpinner(TSpinnerStyle.ssDots);
      Spinner.Start;
      try
        // Add files with spinner animation
        for i := 1 to FileCount do
        begin
          Spinner.Update(0);
          Sleep(300);  // Simulate searching
          Files.Add(Format('file%d.txt', [i]));
        end;
      finally
        Spinner.Stop;
      end;
      // Step 2: Check if verbose output is requested
      Verbose := False; // Default value
      if GetParameterValue('--verbose', VerboseStr) then
        Verbose := StrToBoolDef(VerboseStr, False);

      // Step 3: Process files with a progress bar
      Progress := CreateProgressBar(Files.Count, 20);  // 20 chars wide
      Progress.Start;
      try
        for i := 0 to Files.Count - 1 do
        begin
          if Verbose then
            TConsole.WriteLn(' Processing: ' + Files[i], ccCyan);

          ProcessFile(Files[i]);
          Progress.Update(i + 1);    // Update progress (1-based)
          Sleep(500); // Simulate work
        end;

        TConsole.WriteLn(' All files processed successfully!', ccGreen);
        Result := 0;
      finally
        Progress.Stop;
      end;
    finally
      Files.Free;
    end;
  end;

  procedure TProcessCommand.ProcessFile(const FileName: string);
  begin
    // Simulate file processing
    Sleep(100);
  end;

  { Main program setup }
var
  App: ICLIApplication;
  Cmd: TProcessCommand;
begin
  // Create the application with name and version
  App := CreateCLIApplication('MyApp', '1.0.0');

  // Create and configure the process command
  Cmd := TProcessCommand.Create('process', 'Process files');
  Cmd.AddParameter(CreateParameter('-v',           // Short flag
    '--verbose',    // Long flag
    'Show detailed progress',  // Description
    False,          // Not required
    ptBoolean,      // Boolean parameter type
    'false'         // Default value
    ));

  // Register command and run the application
  App.RegisterCommand(Cmd);
  ExitCode := App.Execute;
end.
```

#### 2. Error Handling Example

```pascal
{
  Error Handling Demo

  This example demonstrates how to create a command-line application that handles errors
  gracefully. It showcases several key features of the CLI framework:

  1. Error handling (stop-on-error flag)
  2. Command parameters (path and stop-on-error flag)
  3. Colored console output
  4. Basic command structure

  How to run (stop on error):

  $ ErrorHandlingDemo.exe validate -p z:\fake_path -s true
  Validating z:\fake_path\file1.txt... OK
  Validating z:\fake_path\file2.txt... OK
  Validating z:\fake_path\file3.txt... OK
  Validating z:\fake_path\file4.txt... OK
  Validating z:\fake_path\file5.txt... OK
  Validating z:\fake_path\file6.txt... OK
  Validating z:\fake_path\file7.txt... OK
  Validating z:\fake_path\file8.txt... OK
  Validating z:\fake_path\file9.txt... ERROR: Demo validation failed for: z:\fake_path\file9.txt
  Stopping due to error (--stop-on-error)

  How to run (don't stop on error):

  $ ErrorHandlingDemo.exe validate -p z:\fake_path -s false
  Validating z:\fake_path\file1.txt... OK
  Validating z:\fake_path\file2.txt... OK
  Validating z:\fake_path\file3.txt... OK
  Validating z:\fake_path\file4.txt... OK
  Validating z:\fake_path\file5.txt... OK
  Validating z:\fake_path\file6.txt... OK
  Validating z:\fake_path\file7.txt... OK
  Validating z:\fake_path\file8.txt... OK
  Validating z:\fake_path\file9.txt... ERROR: Demo validation failed for: z:\fake_path\file9.txt
  Validating z:\fake_path\file10.txt... OK
  Validation complete with 1 errors
}
program ErrorHandlingDemo;

{ Compiler directives:
  - $mode objfpc: Use Object Pascal mode for modern OOP features
  - $H+: Use long strings (AnsiString) instead of short strings
  - $J-: Disable writeable typed constants for safety }
{$mode objfpc}{$H+}{$J-}

{ Import required units from the CLI framework }
uses
  SysUtils,
  Classes,
  CLI.Interfaces,    // Core interfaces
  CLI.Application,   // Application creation
  CLI.Command,       // Command base class
  CLI.Parameter,     // Parameter handling
  CLI.Progress,      // Progress indicators
  CLI.Console;       // Colored output

type
  { TValidateCommand - Command class that validates files
    This demonstrates error handling patterns and parameter usage.
    The command takes a path and validates files in it, with options
    to stop on first error. }
  TValidateCommand = class(TBaseCommand)
  private
    { Simulates validating a single file
      @param Path The file path to validate
      @return True if validation passed, False if failed }
    function ValidateFile(const Path: string): Boolean;
  public
    { Main execution method that runs the validation process
      @return 0 for success, 1 for any errors }
    function Execute: Integer; override;
  end;

function TValidateCommand.Execute: Integer;
var
  Path: string;           // Path parameter from command line
  StopOnErrorStr: string; // Raw string value of stop-on-error flag
  StopOnError: Boolean;   // Parsed boolean value of stop-on-error flag
  Files: TStringList;     // List to hold files to validate
  ErrorCount: Integer;    // Tracks number of validation failures
  i: Integer;            // Loop counter
begin
  Result := 0;
  ErrorCount := 0;

  // Get the required path parameter
  // GetParameterValue is a helper method from TBaseCommand
  if not GetParameterValue('--path', Path) then
  begin
    TConsole.WriteLn('Error: Path is required', ccRed);
    Exit(1);
  end;

  // Get stop-on-error flag and convert to boolean
  // This demonstrates handling optional boolean parameters
  StopOnError := False; // Default value
  if GetParameterValue('--stop-on-error', StopOnErrorStr) then
    StopOnError := StrToBoolDef(StopOnErrorStr, False);

  // Create file list and use try-finally for cleanup
  Files := TStringList.Create;
  try
    try
      // In a real app, you would scan the directory here
      // This is just a simulation with hardcoded files
      Files.Add(Path + '\file1.txt');
      Files.Add(Path + '\file2.txt');
      Files.Add(Path + '\file3.txt');
      Files.Add(Path + '\file4.txt');
      Files.Add(Path + '\file5.txt');
      Files.Add(Path + '\file6.txt');
      Files.Add(Path + '\file7.txt');
      Files.Add(Path + '\file8.txt');
      Files.Add(Path + '\file9.txt');
      Files.Add(Path + '\file10.txt');

      // Process each file with error handling
      for i := 0 to Files.Count - 1 do
      begin
        // Show current file being processed
        TConsole.Write('Validating ' + Files[i] + '... ', ccCyan);

        try
          // Attempt to validate the file
          if ValidateFile(Files[i]) then
            TConsole.WriteLn('OK', ccGreen)
          else
          begin
            // Handle validation failure
            TConsole.WriteLn('FAILED', ccRed);
            Inc(ErrorCount);

            // Check if we should stop on first error
            if StopOnError then
            begin
              TConsole.WriteLn('Stopping due to error (--stop-on-error)', ccYellow);
              Exit(1);
            end;
          end;
        except
          // Handle any unexpected exceptions during validation
          on E: Exception do
          begin
            TConsole.WriteLn('ERROR: ' + E.Message, ccRed);
            Inc(ErrorCount);

            if StopOnError then
            begin
              TConsole.WriteLn('Stopping due to error (--stop-on-error)', ccYellow);
              Exit(1);
            end;
          end;
        end;
      end;

      // Show final summary with color-coded output
      if ErrorCount > 0 then
      begin
        TConsole.WriteLn(Format('Validation complete with %d errors', [ErrorCount]), ccYellow);
        Result := 1;
      end
      else
        TConsole.WriteLn('All files validated successfully', ccGreen);

    except
      // Handle any unexpected errors in the main process
      on E: Exception do
      begin
        TConsole.WriteLn('Fatal error: ' + E.Message, ccRed);
        Result := 1;
      end;
    end;
  finally
    // Always clean up resources
    Files.Free;
  end;
end;

function TValidateCommand.ValidateFile(const Path: string): Boolean;
begin
  // Demo: Simulate validation with random failures
  // In a real application, you would:
  // 1. Check if file exists
  // 2. Verify read permissions
  // 3. Validate file contents
  // 4. etc.
  
  Sleep(100);  // Simulate some work
  Result := Random(10) > 4;  // 50% chance of failure
  
  if not Result then
    raise Exception.CreateFmt('Demo validation failed for: %s', [Path]);
end;

// Main program setup
var
  App: ICLIApplication;
  Cmd: TValidateCommand;
begin
  // Create the main application with name and version
  App := CreateCLIApplication('MyApp', '1.0.0');

  // Create the validate command
  Cmd := TValidateCommand.Create('validate', 'Validate files');

  // Add required path parameter
  Cmd.AddParameter(CreateParameter(
    '-p',            // Short form
    '--path',        // Long form
    'Path to validate', // Description
    True,            // Required parameter
    ptString         // Parameter type: string
  ));

  // Add optional stop-on-error flag
  Cmd.AddParameter(CreateParameter(
    '-s',                          // Short form
    '--stop-on-error',            // Long form
    'Stop processing on first error', // Description
    False,                        // Optional parameter
    ptBoolean,                    // Parameter type: boolean
    'false'                       // Default value
  ));

  // Register command and run the application
  App.RegisterCommand(Cmd);
  ExitCode := App.Execute;
end.
``` 