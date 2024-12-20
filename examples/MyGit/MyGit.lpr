{ MyGit - A simple Git-like CLI application demonstrating the CLI Framework
  
  This example shows how to:
  1. Create a CLI application with multiple commands
  2. Handle command parameters
  3. Use progress indicators
  4. Display colored console output
  5. Implement a command hierarchy }

program MyGit;

{ Compiler directives:
  - $mode objfpc: Use Object Pascal mode for modern OOP features
  - $H+: Use long strings (AnsiString) instead of short strings
  - $J-: Disable writeable typed constants for safety }
{$mode objfpc}{$H+}{$J-}

{ Import required units:
  - SysUtils: Basic system utilities (e.g., GetCurrentDir)
  - CLI.* units: Our CLI framework components }
uses
  SysUtils,
  CLI.Interfaces,    // Core interfaces
  CLI.Application,   // Main application functionality
  CLI.Command,       // Base command implementation
  CLI.Parameter,     // Parameter handling
  CLI.Console,       // Colored console output
  CLI.Progress;      // Progress indicators (spinner/progress bar)

type
  { TInitCommand - Handles the 'init' command
    Usage: MyGit repo init [--path=<directory>]
    Inherits from TBaseCommand to get basic command functionality }
  TInitCommand = class(TBaseCommand)
  public
    function Execute: Integer; override;
  end;

  { TCloneCommand - Handles the 'clone' command
    Usage: MyGit repo clone --url=<repository-url>
    Also inherits from TBaseCommand }
  TCloneCommand = class(TBaseCommand)
  public
    function Execute: Integer; override;
  end;

{ TInitCommand implementation }
function TInitCommand.Execute: Integer;
var
  Path: string;                    // Directory to initialize repository
  Progress: IProgressIndicator;    // Spinner to show activity
begin
  { Get the --path parameter value. If not provided, use current directory.
    GetParameterValue is inherited from TBaseCommand }
  if not GetParameterValue('--path', Path) then
    Path := GetCurrentDir;

  { Basic validation - ensure directory exists
    In a real application, you'd do more validation }
  if not DirectoryExists(Path) then
  begin
    // Use colored output for errors (red is conventional for errors)
    TConsole.WriteLn('Error: Directory not found: ' + Path, ccRed);
    Exit(1);  // Return non-zero to indicate error
  end;

  { Create a "spinner" progress indicator
    ssDots style shows animated dots (...) while working }
  Progress := CreateSpinner(ssDots);
  Progress.Start;
  try
    // Show status in cyan (conventional for information messages)
    TConsole.WriteLn('Initializing repository at ' + Path + '...', ccCyan);
    Sleep(1000); // Simulate work (remove in real application)

    { Here you would actually:
      1. Create .git directory
      2. Initialize repository structure
      3. Create initial config
      4. Set up branches, etc. }

    // Show success message in green (conventional for success)
    TConsole.WriteLn('Repository initialized!', ccGreen);
    Result := 0;  // Return 0 to indicate success
  finally
    // Always stop the progress indicator when done
    Progress.Stop;
  end;
end;

{ TCloneCommand implementation - similar structure to TInitCommand }
function TCloneCommand.Execute: Integer;
var
  Url: string;
  Progress: IProgressIndicator;
begin
  { The --url parameter is required (set when creating parameter)
    If missing, GetParameterValue returns False }
  if not GetParameterValue('--url', Url) then
  begin
    TConsole.WriteLn('Error: URL is required', ccRed);
    Exit(1);
  end;

  { Basic URL validation
    In a real application, you'd want more thorough validation }
  if (Pos('http://', Url) = 0) and (Pos('https://', Url) = 0) then
  begin
    TConsole.WriteLn('Error: Invalid URL. Must start with http:// or https://', ccRed);
    Exit(1);
  end;

  // Similar progress indication pattern as in TInitCommand
  Progress := CreateSpinner(ssDots);
  Progress.Start;
  try
    TConsole.WriteLn('Cloning from ' + Url + '...', ccCyan);
    Sleep(2000); // Simulate work

    { Here you would actually:
      1. Create destination directory
      2. Connect to remote repository
      3. Download repository content
      4. Set up local configuration }

    TConsole.WriteLn('Clone complete!', ccGreen);
    Result := 0;
  finally
    Progress.Stop;
  end;
end;

{ Main program variables }
var
  App: ICLIApplication;           // Main application interface
  RepoCmd: TBaseCommand;         // Parent command for repository operations
  InitCmd: TInitCommand;         // 'init' command implementation
  CloneCmd: TCloneCommand;       // 'clone' command implementation
  ExitCode: Integer;             // Program exit code

{ Main program }
begin
  try
    // Create the main application with name and version
    App := CreateCLIApplication('MyGit', '1.0.0');

    // Create the main 'repo' command group
    RepoCmd := TBaseCommand.Create('repo', 'Repository management');

    // Create and configure the 'init' command
    InitCmd := TInitCommand.Create('init', 'Initialize a repository');
    InitCmd.AddParameter(CreateParameter(
      '-p',                        // Short form
      '--path',                    // Long form
      'Path to initialize repository', // Description
      False,                       // Not required
      ptString,                    // Parameter type
      GetCurrentDir               // Default value
    ));

    // Create and configure the 'clone' command
    CloneCmd := TCloneCommand.Create('clone', 'Clone a repository');
    CloneCmd.AddParameter(CreateParameter(
      '-u',
      '--url',
      'Repository URL to clone',
      True,     // Required parameter
      ptString
    ));

    // Build command hierarchy: repo -> (init, clone)
    RepoCmd.AddSubCommand(InitCmd);
    RepoCmd.AddSubCommand(CloneCmd);

    // Register the main command with the application
    App.RegisterCommand(RepoCmd);

    // Execute the application (handles command line parsing)
    ExitCode := App.Execute;
  except
    // Global error handler
    on E: Exception do
    begin
      TConsole.WriteLn('Error: ' + E.Message, ccRed);
      ExitCode := 1;
    end;
  end;

  // Exit with appropriate code (0 = success, non-zero = error)
  Halt(ExitCode);
end.
