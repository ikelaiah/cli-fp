# Beginner's Guide to CLI Framework

## Introduction

This guide will walk you through creating your first command-line application using the Free Pascal CLI Framework. We'll build a simple Git-like application step by step, explaining each concept along the way.

## Prerequisites

- Basic knowledge of Free Pascal/Delphi syntax
- Free Pascal Compiler (FPC) installed
- A text editor or IDE

## Part 1: Understanding the Basics

### What is a CLI Application?

A Command-Line Interface (CLI) application is a program that users interact with through text commands in a terminal or console. Examples include:
- Git (`git commit`, `git push`)
- NPM (`npm install`, `npm start`)
- Docker (`docker run`, `docker build`)

### Core Concepts

1. **Commands**: Actions that your application can perform
   - Example: `git clone`, where `clone` is the command

2. **Parameters**: Additional information passed to commands
   - Example: `git clone --url https://github.com/user/repo`
   - Here, `--url` is a parameter

3. **Progress Indicators**: Visual feedback during long operations
   - Spinners for unknown duration tasks
   - Progress bars for tasks with known steps

## Part 2: Creating Your First CLI App

Let's create a simple Git-like application step by step.

### Step 1: Project Setup

1. Create a new project directory:
```bash
mkdir my-git-cli
cd my-git-cli
```

2. Create a new program file `mygit.pas`:
```pascal
program MyGit;

{$mode objfpc}{$H+}

uses
  SysUtils,
  CLI.Interfaces,
  CLI.Application,
  CLI.Command,
  CLI.Parameter,
  CLI.Progress,  // For progress indicators
  CLI.Console;

begin
  // We'll add code here
end.
```

### Step 2: Creating the Application

First, let's create the main application:

```pascal
var
  App: ICLIApplication;
begin
  try
    // Create application with name and version
    App := CreateCLIApplication('MyGit', '1.0.0');
    
    // Execute and get exit code
    ExitCode := App.Execute;
  except
    on E: Exception do
    begin
      // Handle any errors with red text
      TConsole.WriteLn('Error: ' + E.Message, ccRed);
      ExitCode := 1;
    end;
  end;
  Halt(ExitCode);
end.
```

**Why This Way?**
- We use `try-except` to catch and handle errors gracefully
- Error messages are shown in red for visibility
- We return proper exit codes (0 for success, 1 for error)

### Step 3: Adding Our First Command

Let's add an `init` command that initializes a repository:

```pascal
type
  { Command to initialize a repository }
  TInitCommand = class(TBaseCommand)
  public
    function Execute: Integer; override;
  end;

function TInitCommand.Execute: Integer;
var
  Path: string;
  Progress: IProgressIndicator;
begin
  // Get the path parameter or use current directory
  if not GetParameterValue('--path', Path) then
    Path := GetCurrentDir;

  // Show a spinner while "working"
  Progress := CreateSpinner(ssDots);
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
```

**Understanding the Code:**
1. We create a new class inheriting from `TBaseCommand`
2. The `Execute` function is where the command's work happens
3. We use a spinner to show activity
4. We use colored output for better user experience

### Step 4: Adding Parameters

Let's add the path parameter to our init command:

```pascal
var
  InitCmd: TInitCommand;
begin
  try
    App := CreateCLIApplication('MyGit', '1.0.0');
    
    // Create the init command
    InitCmd := TInitCommand.Create('init', 'Initialize a repository');
    
    // Add the path parameter
    InitCmd.AddParameter(CreateParameter(
      '-p',            // Short form
      '--path',        // Long form
      'Path to initialize repository',  // Description
      False,           // Not required
      ptString,        // Parameter type
      GetCurrentDir    // Default value
    ));
    
    // Register the command
    App.RegisterCommand(InitCmd);
    
    ExitCode := App.Execute;
  except
    // ... error handling ...
  end;
end;
```

**Parameter Breakdown:**
- Short form (`-p`): For quick typing
- Long form (`--path`): For clarity
- Description: Shown in help text
- Required: False means optional
- Type: String for file paths
- Default: Current directory if not specified

### Step 5: Adding a Progress Indicator

Let's understand the progress indicator in our init command:

```pascal
Progress := CreateSpinner(ssDots);  // Create a spinner
Progress.Start;                     // Start animation
try
  // Your work here
finally
  Progress.Stop;                    // Always stop the spinner
end;
```

**Why Use Progress Indicators?**
1. Shows the user that something is happening
2. Prevents the appearance of a "frozen" program
3. Provides a better user experience

**Available Spinner Styles:**
- `ssDots`: ⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏ (Good for modern terminals)
- `ssLine`: -\|/ (Works in all terminals)
- `ssCircle`: ◐◓◑◒ (Clean look)
- And more...

### Step 6: Adding More Commands

Now let's add a `clone` command:

```pascal
type
  TCloneCommand = class(TBaseCommand)
  public
    function Execute: Integer; override;
  end;

function TCloneCommand.Execute: Integer;
var
  Url: string;
  Progress: IProgressIndicator;
begin
  // URL is required
  if not GetParameterValue('--url', Url) then
  begin
    TConsole.WriteLn('Error: URL is required', ccRed);
    Exit(1);
  end;

  Progress := CreateSpinner(ssDots);
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
```

**Add it to the application:**
```pascal
var
  CloneCmd: TCloneCommand;
begin
  // ... previous code ...
  
  CloneCmd := TCloneCommand.Create('clone', 'Clone a repository');
  CloneCmd.AddParameter(CreateParameter(
    '-u',
    '--url',
    'Repository URL to clone',
    True,     // This parameter is required
    ptString
  ));
  
  App.RegisterCommand(CloneCmd);
  
  // ... rest of code ...
end;
```

### Step 7: Organizing Commands

For larger applications, you can group related commands:

```pascal
var
  RepoCmd: TBaseCommand;
begin
  // Create a parent command
  RepoCmd := TBaseCommand.Create('repo', 'Repository management');
  
  // Add our commands as subcommands
  RepoCmd.AddSubCommand(InitCmd);
  RepoCmd.AddSubCommand(CloneCmd);
  
  // Register the parent command
  App.RegisterCommand(RepoCmd);
end;
```

Now commands are organized like:
- `mygit repo init --path /path/to/repo`
- `mygit repo clone --url https://github.com/user/repo`

## Common Patterns and Best Practices

1. **Error Handling**
   ```pascal
   if not GetParameterValue('--param', Value) then
   begin
     TConsole.WriteLn('Error: Required parameter missing', ccRed);
     Exit(1);
   end;
   ```

2. **Progress Indication**
   ```pascal
   Progress := CreateSpinner(ssDots);
   Progress.Start;
   try
     // Work here
   finally
     Progress.Stop;  // Always stop in finally
   end;
   ```

3. **Color Usage**
   - Red (`ccRed`): Errors
   - Green (`ccGreen`): Success
   - Cyan (`ccCyan`): Information
   - Yellow (`ccYellow`): Warnings

4. **Parameter Validation**
   ```pascal
   if not DirectoryExists(Path) then
   begin
     TConsole.WriteLn('Error: Directory not found', ccRed);
     Exit(1);
   end;
   ```

## Next Steps

1. Try adding more commands to your application
2. Experiment with different spinner styles
3. Add parameter validation
4. Implement actual functionality instead of Sleep()
5. Add detailed help text for commands

## Common Issues and Solutions

1. **Command Not Found**
   - Check command registration
   - Verify command name spelling

2. **Parameter Errors**
   - Check parameter names
   - Verify required parameters
   - Check parameter types

3. **Progress Indicator Not Working**
   - Ensure proper Start/Stop usage
   - Use try-finally blocks
   - Call Update regularly

## Getting Help

- Use `--help` to see available commands
- Use `<command> --help` for command help
- Check the user manual for detailed information 

## Complete Application Code

Here's the complete code for our Git-like CLI application in a single file:

```pascal
program MyGit;

{$mode objfpc}{$H+}{$J-}

uses
  SysUtils,
  CLI.Interfaces,
  CLI.Application,
  CLI.Command,
  CLI.Parameter,
  CLI.Progress,  // For progress indicators
  CLI.Console;

type
  { Command to initialize a repository }
  TInitCommand = class(TBaseCommand)
  public
    function Execute: Integer; override;
  end;

  { Command to clone a repository }
  TCloneCommand = class(TBaseCommand)
  public
    function Execute: Integer; override;
  end;

{ TInitCommand implementation }
function TInitCommand.Execute: Integer;
var
  Path: string;
  Progress: IProgressIndicator;
begin
  // Get the path parameter or use current directory
  if not GetParameterValue('--path', Path) then
    Path := GetCurrentDir;

  // Validate the path
  if not DirectoryExists(Path) then
  begin
    TConsole.WriteLn('Error: Directory not found: ' + Path, ccRed);
    Exit(1);
  end;

  // Show a spinner while "working"
  Progress := CreateSpinner(ssDots);
  Progress.Start;
  try
    TConsole.WriteLn('Initializing repository at ' + Path + '...', ccCyan);
    Sleep(1000); // Simulate work
    
    // Here you would actually:
    // 1. Create .git directory
    // 2. Initialize repository structure
    // 3. Create initial config
    
    TConsole.WriteLn('Repository initialized!', ccGreen);
    Result := 0;
  finally
    Progress.Stop;
  end;
end;

{ TCloneCommand implementation }
function TCloneCommand.Execute: Integer;
var
  Url: string;
  Progress: IProgressIndicator;
begin
  // URL is required
  if not GetParameterValue('--url', Url) then
  begin
    TConsole.WriteLn('Error: URL is required', ccRed);
    Exit(1);
  end;

  // Validate URL (basic check)
  if (Pos('http://', Url) = 0) and (Pos('https://', Url) = 0) then
  begin
    TConsole.WriteLn('Error: Invalid URL. Must start with http:// or https://', ccRed);
    Exit(1);
  end;

  Progress := CreateSpinner(ssDots);
  Progress.Start;
  try
    TConsole.WriteLn('Cloning from ' + Url + '...', ccCyan);
    Sleep(2000); // Simulate work
    
    // Here you would actually:
    // 1. Create destination directory
    // 2. Connect to remote repository
    // 3. Download repository content
    // 4. Set up local configuration
    
    TConsole.WriteLn('Clone complete!', ccGreen);
    Result := 0;
  finally
    Progress.Stop;
  end;
end;

var
  App: ICLIApplication;
  RepoCmd: TBaseCommand;
  InitCmd: TInitCommand;
  CloneCmd: TCloneCommand;
  ExitCode: Integer;
begin
  try
    // Create the main application
    App := CreateCLIApplication('MyGit', '1.0.0');
    
    // Create the repository management command group
    RepoCmd := TBaseCommand.Create('repo', 'Repository management');
    
    // Create and configure the init command
    InitCmd := TInitCommand.Create('init', 'Initialize a repository');
    InitCmd.AddParameter(CreateParameter(
      '-p',
      '--path',
      'Path to initialize repository',
      False,           // Not required
      ptString,
      GetCurrentDir    // Default value
    ));
    
    // Create and configure the clone command
    CloneCmd := TCloneCommand.Create('clone', 'Clone a repository');
    CloneCmd.AddParameter(CreateParameter(
      '-u',
      '--url',
      'Repository URL to clone',
      True,     // Required
      ptString
    ));
    
    // Build the command hierarchy
    RepoCmd.AddSubCommand(InitCmd);
    RepoCmd.AddSubCommand(CloneCmd);
    
    // Register the main command
    App.RegisterCommand(RepoCmd);
    
    // Execute the application
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

### Building and Running

1. Save the code as `mygit.pas`

2. Compile the program:
```bash
fpc mygit.pas
```

3. Try the commands:
```bash
# Show help
./mygit --help

# Initialize repository
./mygit repo init
./mygit repo init --path /path/to/repo

# Clone repository
./mygit repo clone --url https://github.com/user/repo
```

### Next Steps

Now that you have a working CLI application, you can:
1. Add more commands (push, pull, commit, etc.)
2. Implement actual Git functionality
3. Add more parameter validations
4. Add detailed help text for each command
5. Add configuration options
6. Add logging and debugging features

Remember to check the user manual for more advanced features and options! 