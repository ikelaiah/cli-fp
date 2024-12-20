# Beginner's Guide to CLI Framework

## Introduction

This guide will walk you through creating your first command-line application using the Free Pascal CLI Framework. We'll build a simple Git-like application step by step, explaining each concept along the way.

## Prerequisites

- Basic knowledge of Free Pascal/Delphi syntax
- Free Pascal Compiler (FPC) installed
- Lazarus IDE installed


## Table of Contents
- [Beginner's Guide to CLI Framework](#beginners-guide-to-cli-framework)
  - [Introduction](#introduction)
  - [Prerequisites](#prerequisites)
  - [Table of Contents](#table-of-contents)
  - [Part 1: Understanding the Basics](#part-1-understanding-the-basics)
    - [What is a CLI Application?](#what-is-a-cli-application)
    - [Core Concepts](#core-concepts)
  - [Part 2: Creating Your First CLI App](#part-2-creating-your-first-cli-app)
    - [Step 1: Project Setup in Lazarus](#step-1-project-setup-in-lazarus)
    - [Step 2: Creating the Application](#step-2-creating-the-application)
    - [Step 3: Adding Our First Command](#step-3-adding-our-first-command)
    - [Step 4: Adding Parameters](#step-4-adding-parameters)
    - [Step 5: Adding a Progress Indicator](#step-5-adding-a-progress-indicator)
    - [Step 6: Adding the Clone Command](#step-6-adding-the-clone-command)
    - [Step 7: Organizing Commands](#step-7-organizing-commands)
  - [Common Patterns and Best Practices](#common-patterns-and-best-practices)
  - [Next Steps](#next-steps)
  - [Common Issues and Solutions](#common-issues-and-solutions)
  - [Getting Help](#getting-help)
  - [Complete Application Code](#complete-application-code)
    - [Building and Running](#building-and-running)
    - [Next Steps](#next-steps-1)
  - [Debugging Tips](#debugging-tips)


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

### Step 1: Project Setup in Lazarus

1. Launch Lazarus and create a new Project from **Project -> New Project -> Program**. 

2. Press Ctrl+S to save the lot:
   - save the project as `MyGit.lpi` in a new folder, 
   - and save the blank/empty main unit as `MyGit.lpr`.

3. Add the source directory to your project's search path; **Project -> Project Options ... -> Compiler Options -> Paths -> Other unit files**

4. Add the units to your uses clause:

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

**Note:** At this point, we've defined our command, but it won't work until we:
1. Create an instance of it
2. Add parameters (which we'll do in Step 4)
3. Register it with the application

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
1. `ssDots` - Braille dots animation
   ```
   ⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏
   ```
   Best for: Modern terminals with Unicode support

2. `ssLine` - Simple ASCII line animation
   ```
   -\|/
   ```
   Best for: Legacy terminals or when Unicode isn't supported

3. `ssCircle` - Unicode circle animation
   ```
   ◐◓◑◒
   ```
   Best for: Clean, minimalist look

4. `ssSquare` - Square rotation animation
   ```
   ◰◳◲◱
   ```
   Best for: Alternative to circle style

5. `ssArrow` - Arrow rotation animation
   ```
   ←↖↑↗→↘↓↙
   ```
   Best for: Directional indication

6. `ssBounce` - Bouncing dot animation
   ```
   ⠁⠂⠄⠂
   ```
   Best for: Subtle indication

7. `ssBar` - Block animation
   ```
   ▏▎▍▌▋▊▉█▊▋▌▍▎▏
   ```
   Best for: Smooth, wave-like animation

**Choosing a Spinner Style:**
- Use `ssLine` for maximum compatibility
- Use `ssDots` or `ssCircle` for modern terminals
- Consider your terminal's font support
- Test on target platforms

### Step 6: Adding the Clone Command

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

**Understanding URL Validation:**
```pascal
if (Pos('http://', Url) = 0) and (Pos('https://', Url) = 0) then
begin
  TConsole.WriteLn('Error: Invalid URL. Must start with http:// or https://', ccRed);
  Exit(1);
end;
```

Why validate URLs?
1. Prevents malformed input early
2. Provides clear error messages
3. Improves security by restricting protocols
4. Helps users identify mistakes quickly

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

1. Save the main unit file -- `MyGit.lpr`.

2. Compile the program:
   - Lazarus: Run -> Clean up and build

3. Open your terminal and try the commands:

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

## Debugging Tips

When developing your CLI application, these debugging techniques can help:

1. **Enable Debug Mode**
   ```pascal
   (App as TCLIApplication).DebugMode := True;
   ```
   This will show additional information about:
   - Command registration
   - Parameter parsing
   - Execution flow

2. **Add Debug Output**
   ```pascal
   if (App as TCLIApplication).DebugMode then
     TConsole.WriteLn('Debug: ' + Message, ccYellow);
   ```

3. **Test Parameter Values**
   ```pascal
   var
     Value: string;
   begin
     if GetParameterValue('--param', Value) then
       TConsole.WriteLn('Got value: ' + Value)
     else
       TConsole.WriteLn('Parameter not provided');
   end;
   ```

4. **Command Hierarchy**
   - Use `--help-complete` to view full command tree
   - Check parent-child relationships
   - Verify parameter registration

5. **Common Debug Scenarios**
   - Command not found: Check registration order
   - Parameter not recognized: Verify parameter creation
   - Wrong parameter type: Check conversion logic
   - Progress indicator issues: Check Start/Stop pairs

Remember to test your application thoroughly on different platforms and environments! 