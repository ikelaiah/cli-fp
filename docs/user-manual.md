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

## Quick Start

### 1. Creating a Simple CLI Application

```pascal
program MyApp;

{$mode objfpc}{$H+}

uses
  SysUtils, CLI.Interfaces, CLI.Application, CLI.Command, CLI.Parameter;

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
begin
  App := CreateCLIApplication('MyApp', '1.0.0');
  
  Cmd := TGreetCommand.Create('greet', 'Greet a person');
  Cmd.AddParameter(CreateParameter('-n', '--name', 'Name to greet', False));
  
  App.RegisterCommand(Cmd);
  ExitCode := App.Execute;
end.
```

### 2. Adding Progress Indicators

```pascal
// Show a spinner
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

// Show a progress bar
var
  Progress: IProgressIndicator;
begin
  Progress := CreateProgressBar(100);
  Progress.Start;
  try
    for i := 1 to 100 do
    begin
      Progress.Update(i);
      Sleep(50);
    end;
  finally
    Progress.Stop;
  end;
end;
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

## Built-in Features

### 1. Help System

Every command automatically includes:
- Usage information
- Command description
- Available options
- Examples

### 2. Parameter Validation

- Required parameters are validated automatically
- Default values are supported
- Type validation (string, integer, etc.)

### 3. Color Support

Use colored output for better user experience:

```pascal
TConsole.WriteLn('Success!', ccGreen);
TConsole.WriteLn('Warning!', ccYellow);
TConsole.WriteLn('Error!', ccRed);
```

### 4. Debug Mode

Enable debug output for troubleshooting:

```pascal
(App as TCLIApplication).DebugMode := True;
```

## Best Practices

1. **Command Organization**
   - Group related functionality into commands
   - Use subcommands for complex features
   - Keep command names clear and consistent

2. **User Experience**
   - Provide helpful descriptions
   - Include examples in help text
   - Use progress indicators for long operations

3. **Error Handling**
   - Display clear error messages
   - Use appropriate exit codes
   - Validate user input

## Examples

Check the `examples/` directory for complete working examples:
- `ProgressDemo`: Shows spinner and progress bar usage
- `SubCommandDemo`: Demonstrates command hierarchy
- `GreetDemo`: Simple parameter handling

## Getting Help

- Use `--help-complete` for comprehensive documentation
- Check command-specific help with `<command> --help`
- Refer to the technical documentation for development details 