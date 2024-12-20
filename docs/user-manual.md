# CLI Framework User Manual

## Overview

The Free Pascal CLI Framework is a modern, feature-rich library for building command-line applications. It provides an intuitive way to create CLIs with commands, subcommands, parameters, and interactive progress indicators.

## Features

- Command and subcommand support
- Parameter handling with validation
- Progress indicators (spinner and progress bar)
- Colored console output
- Built-in help system
- Automatic usage examples generation

## Installation

1. Clone the repository:
   ```bash
   git clone https://github.com/your-repo/cli-fp.git
   ```

2. Add to your project:
   ```pascal
   uses
     CLI.Interfaces,
     CLI.Application,
     CLI.Command,
     CLI.Parameter,
     CLI.Progress,  // For progress indicators
     CLI.Console;   // For colored output
   ```

## Quick Start

### 1. Creating a Simple CLI Application

```pascal
program MyApp;

{$mode objfpc}{$H+}{$J-}

uses
  SysUtils,
  CLI.Interfaces,
  CLI.Application,
  CLI.Command,
  CLI.Parameter,
  CLI.Console;

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
    TConsole.WriteLn('Hello, ' + Name + '!')
  else
    TConsole.WriteLn('Hello, World!');
  Result := 0;
end;

var
  App: ICLIApplication;
  Cmd: TGreetCommand;
  ExitCode: Integer;
begin
  try
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
  except
    on E: Exception do
    begin
      TConsole.WriteLn('Error: ' + E.Message, ccRed);
      ExitCode := 1;
    end;
  end;
  Halt(ExitCode);
end.
```

### 2. Creating a Git-like CLI

```pascal
type
  TCloneCommand = class(TBaseCommand)
  public
    function Execute: Integer; override;
  end;

  TInitCommand = class(TBaseCommand)
  public
    function Execute: Integer; override;
  end;

function TCloneCommand.Execute: Integer;
var
  Url: string;
  Progress: IProgressIndicator;
begin
  if not GetParameterValue('--url', Url) then
  begin
    TConsole.WriteLn('Error: URL is required', ccRed);
    Exit(1);
  end;

  Progress := CreateSpinner(ssLine);
  Progress.Start;
  try
    TConsole.WriteLn('Cloning from ' + Url + '...', ccCyan);
    Sleep(2000); // Simulate work
    TConsole.WriteLn('Clone complete!', ccGreen);
    Result := 0;
  finally
    Progress.Stop;
  end;
end;

function TInitCommand.Execute: Integer;
var
  Path: string;
  Progress: IProgressIndicator;
begin
  if not GetParameterValue('--path', Path) then
    Path := GetCurrentDir;

  Progress := CreateSpinner(ssLine);
  Progress.Start;
  try
    TConsole.WriteLn('Initializing repository at ' + Path + '...', ccCyan);
    Sleep(1000); // Simulate work
    TConsole.WriteLn('Repository initialized!', ccGreen);
    Result := 0;
  finally
    Progress.Stop;
  end;
end;

var
  App: ICLIApplication;
  RepoCmd: TBaseCommand;
  CloneCmd: TCloneCommand;
  InitCmd: TInitCommand;
  ExitCode: Integer;
begin
  try
    App := CreateCLIApplication('MyGit', '1.0.0');
    
    RepoCmd := TBaseCommand.Create('repo', 'Repository management');
    
    CloneCmd := TCloneCommand.Create('clone', 'Clone a repository');
    CloneCmd.AddParameter(CreateParameter(
      '-u',
      '--url',
      'Repository URL to clone',
      True,     // Mark as required
      ptString
    ));
    RepoCmd.AddSubCommand(CloneCmd);
    
    InitCmd := TInitCommand.Create('init', 'Initialize a repository');
    InitCmd.AddParameter(CreateParameter(
      '-p',
      '--path',
      'Path to initialize repository',
      False,    // Optional
      ptString,
      GetCurrentDir  // Default to current directory
    ));
    RepoCmd.AddSubCommand(InitCmd);
    
    App.RegisterCommand(RepoCmd);
    
    ExitCode := App.Execute;
  except
    on E: Exception do
    begin
      TConsole.WriteLn('Error: ' + E.Message, ccRed);
      ExitCode := 1;
    end;
  end;
  Halt(ExitCode);
end;
```

### 3. Progress Indicators

```pascal
// Spinner for indeterminate progress
var
  Spinner: IProgressIndicator;
begin
  Spinner := CreateSpinner(ssLine);
  Spinner.Start;
  try
    // Your long-running task here
    Sleep(1000);
  finally
    Spinner.Stop;
  end;
end;

// Progress bar for determinate progress
var
  Progress: IProgressIndicator;
  Total: Integer;
begin
  Total := 100;
  Progress := CreateProgressBar(Total, 20); // 20 chars wide
  Progress.Start;
  try
    TConsole.WriteLn('Processing...', ccCyan);
    for i := 1 to Total do
    begin
      Progress.Update(i);
      Sleep(50); // Simulate work
    end;
    TConsole.WriteLn('Complete!', ccGreen);
  finally
    Progress.Stop;
  end;
end;
```

## Parameter Types Reference

### 1. String Parameters

```pascal
// Required string parameter
Cmd.AddParameter(CreateParameter(
  '-n',
  '--name',
  'Your name',
  True,      // Required
  ptString
));

// Optional string with default
Cmd.AddParameter(CreateParameter(
  '-p',
  '--path',
  'File path',
  False,     // Optional
  ptString,
  './default.txt'  // Default value
));
```

### 2. Integer Parameters

```pascal
// Required integer
Cmd.AddParameter(CreateParameter(
  '-c',
  '--count',
  'Number of items',
  True,
  ptInteger
));

// Optional with default
Cmd.AddParameter(CreateParameter(
  '-l',
  '--limit',
  'Result limit',
  False,
  ptInteger,
  '10'
));
```

### 3. Boolean Flags

```pascal
// Simple flag
Cmd.AddParameter(CreateParameter(
  '-f',
  '--force',
  'Force operation',
  False,
  ptBoolean
));

// Flag with default
Cmd.AddParameter(CreateParameter(
  '-v',
  '--verbose',
  'Enable verbose output',
  False,
  ptBoolean,
  'false'
));
```

### 4. Float Parameters

```pascal
// Optional float with default
Cmd.AddParameter(CreateParameter(
  '-r',
  '--rate',
  'Processing rate',
  False,
  ptFloat,
  '1.0'
));
```

## Command-Line Usage

### Basic Command Structure

```bash
myapp <command> [options]
```

### Getting Help

- Show general help: `myapp -h` or `myapp --help`
- Show detailed reference: `myapp --help-complete`
- Show command help: `myapp <command> --help`

### Parameter Formats

The framework supports various parameter formats:
- Long format: `--param=value` or `--param value`
- Short format: `-p value`
- Boolean flags: `--flag` or `-f`

## Troubleshooting

### Common Issues

1. **Command Not Found**
   - Verify command name spelling
   - Check if command is properly registered with `App.RegisterCommand`
   - Enable debug mode:
     ```pascal
     (App as TCLIApplication).DebugMode := True;
     ```

2. **Parameter Errors**
   - Check parameter format:
     ```bash
     --param=value    # Equals syntax
     --param value    # Space syntax
     -p value        # Short format
     ```
   - Verify required parameters are provided
   - Check parameter type matches expected value
   - Use `GetParameterValue` correctly:
     ```pascal
     var
       Value: string;
     begin
       if GetParameterValue('--param', Value) then
         // Parameter was provided
       else
         // Parameter was not provided
     end;
     ```

3. **Console Colors Not Working**
   - Windows: Check if ANSI support is enabled
   - Unix/Linux: Verify terminal supports colors
   - Always reset colors:
     ```pascal
     TConsole.SetForegroundColor(ccRed);
     try
       // Your colored output
     finally
       TConsole.ResetColors;
     end;
     ```

## Best Practices

1. **Command Organization**
   - Group related functionality into commands
   - Use subcommands for complex features
   - Keep command names clear and consistent
   - Follow naming conventions

2. **User Experience**
   - Provide helpful descriptions
   - Include examples in help text
   - Use progress indicators for long operations
   - Provide feedback for all operations

3. **Error Handling**
   - Display clear error messages using appropriate colors
   - Use appropriate exit codes
   - Validate user input
   - Always handle exceptions

4. **Color Usage**
   - Use red for errors
   - Use yellow for warnings
   - Use green for success messages
   - Use cyan for information
   - Use white for normal output

5. **Progress Indication**
   - Use spinners for indeterminate progress
   - Use progress bars for determinate progress
   - Always stop indicators in a finally block
   - Provide status messages with progress

## Getting Help

- Use `--help-complete` for comprehensive documentation
- Check command-specific help with `<command> --help`
- Enable debug mode for troubleshooting
- Refer to the technical documentation for development details