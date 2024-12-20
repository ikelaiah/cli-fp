# CLI Framework User Manual

## Getting Started

### Installation

1. Add the framework to your project:
   ```pascal
   uses
     CLI.Interfaces,
     CLI.Application,
     CLI.Command,
     CLI.Parameter;
   ```

### Quick Start

1. Create a simple CLI application:
   ```pascal
   var
     App: ICLIApplication;
   begin
     App := CreateCLIApplication('MyApp', '1.0.0');
     App.Execute;
   end;
   ```

2. Add a command:
   ```pascal
   type
     TGreetCommand = class(TBaseCommand)
     public
       function Execute: Integer; override;
     end;

   function TGreetCommand.Execute: Integer;
   begin
     WriteLn('Hello, World!');
     Result := 0;
   end;

   // Register command
   App.RegisterCommand(TGreetCommand.Create('greet', 'Display a greeting'));
   ```

## Command Structure

### Creating Commands

```pascal
type
  TMyCommand = class(TBaseCommand)
  public
    function Execute: Integer; override;
  end;
```

### Adding Parameters

```pascal
var
  Cmd: TMyCommand;
begin
  Cmd := TMyCommand.Create('mycmd', 'My command description');
  Cmd.AddParameter(CreateParameter(
    '-n',                  // Short flag
    '--name',             // Long flag
    'Your name',          // Description
    True,                 // Required
    ptString             // Parameter type
  ));
end;
```

### Parameter Types

- `ptString`: String values
- `ptInteger`: Integer values
- `ptFloat`: Float values
- `ptBoolean`: Boolean flags
- `ptFlag`: Simple flags (no value)

## Console Output

### Basic Output

```pascal
TConsole.Write('Simple text');
TConsole.WriteLn('Text with newline');
```

### Colored Output

```pascal
// Foreground colors
TConsole.WriteLn('Error message', ccRed);
TConsole.WriteLn('Success message', ccGreen);
TConsole.WriteLn('Info message', ccCyan);

// Background colors
TConsole.SetBackgroundColor(ccBlue);
TConsole.WriteLn('Text with background');
TConsole.ResetColors;
```

### Available Colors

- Basic Colors:
  - `ccBlack`, `ccBlue`, `ccGreen`, `ccCyan`
  - `ccRed`, `ccMagenta`, `ccYellow`, `ccWhite`

- Bright Colors:
  - `ccBrightBlack`, `ccBrightBlue`, `ccBrightGreen`, `ccBrightCyan`
  - `ccBrightRed`, `ccBrightMagenta`, `ccBrightYellow`, `ccBrightWhite`

## Progress Indicators

### Spinner

```pascal
var
  Spinner: IProgressIndicator;
begin
  Spinner := CreateSpinner(ssLine);
  Spinner.Start;
  try
    // Do work
    Spinner.Update(0);
  finally
    Spinner.Stop;
  end;
end;
```

### Progress Bar

```pascal
var
  Progress: IProgressIndicator;
begin
  Progress := CreateProgressBar(100, 20); // total=100, width=20
  Progress.Start;
  try
    // Update progress
    Progress.Update(50); // 50%
  finally
    Progress.Stop;
  end;
end;
```

## Error Messages and Invalid Usage

When using the CLI framework, you might encounter various error messages that help you understand what went wrong and how to fix it. Here are the common scenarios:

### Invalid Command Usage

1. **Using an Unknown Command**
   ```bash
   $ myapp invalidcommand
   Error: Unknown command "invalidcommand"
   
   Usage: myapp <command> [options]

   Commands:
     repo              Repository management
     config            Configuration management

   Use --help for more information.
   ```

2. **Using an Unknown Subcommand**
   ```bash
   $ myapp repo invalid
   Error: Unknown subcommand "invalid" for repo

   Available subcommands:
     init              Initialize a new repository
     clone             Clone an existing repository

   Use "myapp repo --help" for more information.
   ```

3. **Using Invalid Flags**
   ```bash
   $ myapp repo clone -z
   Error: Unknown parameter "-z"
   Usage: myapp repo clone [options]

   Clone a repository

   Options:
     -u, --url               Repository URL (required)
     -p, --path              Target path
   ```

### Getting Help

- If you see an error message, the framework will automatically show relevant help information
- You can always use `-h` or `--help` with any command or subcommand to see detailed usage information
- Use `--help-complete` to see the complete reference for all commands and options

### Best Practices

1. **Check Command Syntax**
   - Always verify the correct command name and structure
   - Use tab completion if available in your shell

2. **Verify Parameters**
   - Make sure all required parameters are provided
   - Check that parameter values are in the correct format

3. **Use Help When Needed**
   - When in doubt, append `--help` to see command-specific help
   - Use `--help-complete` to explore all available commands

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