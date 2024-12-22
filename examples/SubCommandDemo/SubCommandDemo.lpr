{ SubCommandDemo - An example showing how to create hierarchical commands
  
  This program demonstrates creating a Git-like repository manager with
  subcommands organized in a tree structure:

  repo                    (root command)
  ├── init                (subcommand)
  │   ├── --path          (optional parameter)
  │   └── --bare          (flag parameter)
  ├── clone               (subcommand)
  │   ├── --url           (required parameter)
  │   ├── --path          (optional parameter)
  │   ├── --branch        (optional parameter)
  │   └── --depth         (optional parameter)
  └── remote              (command group)
      ├── add             (subcommand)
      │   ├── --name      (required parameter)
      │   └── --url       (required parameter)
      └── remove          (subcommand)
          └── --name      (required parameter)
}

{ Compiler directives }
{$mode objfpc}  // Use Object Pascal mode for modern OOP features
{$H+}           // Use AnsiString instead of ShortString for better string handling
{$J-}           // Disable writeable typed constants for safety

{ Import required units }
uses
  SysUtils,         // Standard system utilities (e.g., GetCurrentDir)
  CLI.Interfaces,   // Core interfaces for CLI components
  CLI.Application,  // Main application framework
  CLI.Parameter,    // Parameter handling and validation
  CLI.Command,      // Base command implementation
  CLI.Console;      // Colored console output

type
  { Base command for repo operations
    This is the root command that serves as a container for subcommands }
  TRepoCommand = class(TBaseCommand)
  public
    function Execute: Integer; override;
  end;

  { Init command - Initializes a new repository
    Supports:
    - Optional path parameter
    - Bare repository flag }
  TRepoInitCommand = class(TBaseCommand)
  public
    function Execute: Integer; override;
  end;

  { Clone command - Clones a remote repository
    Supports:
    - Required URL parameter
    - Optional path, branch, and depth parameters }
  TRepoCloneCommand = class(TBaseCommand)
  public
    function Execute: Integer; override;
  end;

  { Remote command group - Container for remote-related commands
    This command doesn't do anything itself but groups remote subcommands }
  TRemoteCommand = class(TBaseCommand)
  public
    function Execute: Integer; override;
  end;

  { Remote add command - Adds a new remote repository reference
    Requires both name and URL parameters }
  TRemoteAddCommand = class(TBaseCommand)
  public
    function Execute: Integer; override;
  end;

  { Remote remove command - Removes a remote repository reference
    Requires the name parameter }
  TRemoteRemoveCommand = class(TBaseCommand)
  public
    function Execute: Integer; override;
  end;

{ TRepoCommand implementation
  This is a command group, so it just shows help when executed directly }
function TRepoCommand.Execute: Integer;
begin
  // Command groups should show help instead of doing any action
  ShowHelp;
  Result := 0;  // Return success
end;

{ TRepoInitCommand implementation
  Handles repository initialization with optional path and bare flag }
function TRepoInitCommand.Execute: Integer;
var
  Path: string;      // Repository path
  BareValue: string; // Raw value from bare flag
  Bare: Boolean;     // Converted bare flag value
begin
  Result := 0;  // Default to success

  // Get path parameter or use current directory if not specified
  if not GetParameterValue('--path', Path) then
    Path := GetCurrentDir;

  // Get bare flag value (true if flag is present)
  Bare := GetParameterValue('--bare', BareValue);

  // Display operation details in color
  TConsole.WriteLn('Initializing repository...', ccCyan);
  TConsole.WriteLn('  Path: ' + Path, ccWhite);
  if Bare then
    TConsole.WriteLn('  Type: Bare repository', ccWhite)
  else
    TConsole.WriteLn('  Type: Regular repository', ccWhite);
end;

{ TRepoCloneCommand implementation
  Handles repository cloning with various options }
function TRepoCloneCommand.Execute: Integer;
var
  URL, Path, Branch: string;  // Command parameters
  Depth: string;             // Clone depth option
begin
  Result := 0;  // Default to success

  // URL is required - exit with error if missing
  if not GetParameterValue('--url', URL) then
  begin
    TConsole.WriteLn('Error: URL is required', ccRed);
    Exit(1);  // Return error code
  end;

  // Get optional parameters with defaults
  if not GetParameterValue('--path', Path) then
    Path := ExtractFileName(URL);  // Use repo name from URL

  if not GetParameterValue('--branch', Branch) then
    Branch := 'main';  // Default to main branch

  if not GetParameterValue('--depth', Depth) then
    Depth := 'full';  // Default to full clone

  // Display operation details in color
  TConsole.WriteLn('Cloning repository...', ccCyan);
  TConsole.WriteLn('  From: ' + URL, ccWhite);
  TConsole.WriteLn('  To: ' + Path, ccWhite);
  TConsole.WriteLn('  Branch: ' + Branch, ccWhite);
  TConsole.WriteLn('  Depth: ' + Depth, ccWhite);
end;

{ TRemoteCommand implementation
  Another command group that just shows help }
function TRemoteCommand.Execute: Integer;
begin
  ShowHelp;
  Result := 0;
end;

{ TRemoteAddCommand implementation
  Adds a new remote with name and URL }
function TRemoteAddCommand.Execute: Integer;
var
  RemoteName, RemoteURL: string;
begin
  Result := 0;

  // Both name and URL are required
  if not GetParameterValue('--name', RemoteName) then
  begin
    TConsole.WriteLn('Error: Remote name is required', ccRed);
    Exit(1);
  end;

  if not GetParameterValue('--url', RemoteURL) then
  begin
    TConsole.WriteLn('Error: Remote URL is required', ccRed);
    Exit(1);
  end;

  // Display operation details
  TConsole.WriteLn('Adding remote...', ccCyan);
  TConsole.WriteLn('  Name: ' + RemoteName, ccWhite);
  TConsole.WriteLn('  URL: ' + RemoteURL, ccWhite);
end;

{ TRemoteRemoveCommand implementation
  Removes a remote by name }
function TRemoteRemoveCommand.Execute: Integer;
var
  RemoteName: string;
begin
  Result := 0;

  // Name is required
  if not GetParameterValue('--name', RemoteName) then
  begin
    TConsole.WriteLn('Error: Remote name is required', ccRed);
    Exit(1);
  end;

  // Display operation details
  TConsole.WriteLn('Removing remote...', ccCyan);
  TConsole.WriteLn('  Name: ' + RemoteName, ccWhite);
end;

{ Main program variables }
var
  App: ICLIApplication;          // Main application interface
  RepoCmd: TRepoCommand;         // Root command for repo operations
  InitCmd: TRepoInitCommand;     // Init subcommand
  CloneCmd: TRepoCloneCommand;   // Clone subcommand
  RemoteCmd: TRemoteCommand;     // Remote command group
  RemoteAddCmd: TRemoteAddCommand;     // Remote add subcommand
  RemoteRemoveCmd: TRemoteRemoveCommand; // Remote remove subcommand

{ Main program }
begin
  try
    // Create main application with name and version
    App := CreateCLIApplication('RepoManager', '1.0.0');

    // Create and register the main repo command group
    RepoCmd := TRepoCommand.Create('repo', 'Repository management commands');
    App.RegisterCommand(RepoCmd);

    // Create and configure init command
    InitCmd := TRepoInitCommand.Create('init', 'Initialize a new repository');
    InitCmd.AddPathParameter('-p', '--path', 'Repository path');
    InitCmd.AddFlag('-b', '--bare', 'Create a bare repository');
    RepoCmd.AddSubCommand(InitCmd);

    // Create and configure clone command
    CloneCmd := TRepoCloneCommand.Create('clone', 'Clone a repository');
    CloneCmd.AddStringParameter('-u', '--url', 'Repository URL', True);
    CloneCmd.AddPathParameter('-p', '--path', 'Target path');
    CloneCmd.AddStringParameter('-b', '--branch', 'Branch to clone', False, 'main');
    CloneCmd.AddStringParameter('-d', '--depth', 'Clone depth', False, 'full');
    RepoCmd.AddSubCommand(CloneCmd);

    // Create remote command group
    RemoteCmd := TRemoteCommand.Create('remote', 'Remote repository commands');
    RepoCmd.AddSubCommand(RemoteCmd);

    // Create and configure remote add command
    RemoteAddCmd := TRemoteAddCommand.Create('add', 'Add a new remote');
    RemoteAddCmd.AddStringParameter('-n', '--name', 'Remote name', True);
    RemoteAddCmd.AddUrlParameter('-u', '--url', 'Remote URL', True);
    RemoteCmd.AddSubCommand(RemoteAddCmd);

    // Create and configure remote remove command
    RemoteRemoveCmd := TRemoteRemoveCommand.Create('remove', 'Remove a remote');
    RemoteRemoveCmd.AddStringParameter('-n', '--name', 'Remote name', True);
    RemoteCmd.AddSubCommand(RemoteRemoveCmd);

    // Clean up command references (not strictly necessary but good practice)
    RepoCmd := nil;
    InitCmd := nil;
    CloneCmd := nil;
    RemoteCmd := nil;
    RemoteAddCmd := nil;
    RemoteRemoveCmd := nil;

    // Execute the application and get exit code
    ExitCode := App.Execute;
  except
    // Handle any unhandled exceptions
    on E: Exception do
    begin
      TConsole.WriteLn('Error: ' + E.Message, ccRed);
      ExitCode := 1;
    end;
  end;
end. 