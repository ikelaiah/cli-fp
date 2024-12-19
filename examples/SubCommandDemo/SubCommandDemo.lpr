program SubCommandDemo;

{
  This example demonstrates how to create a subcommand for a main command.
  It creates a repository manager with subcommands for initializing, cloning,
  and managing remotes.

  repo
  ├── init
  │   ├── --path (optional)
  │   └── --bare (flag)
  ├── clone
  │   ├── --url (required)
  │   ├── --path (optional)
  │   ├── --branch (optional, default: main)
  │   └── --depth (optional, default: full)
  └── remote
      ├── add
      │   ├── --name (required)
      │   └── --url (required)
      └── remove
          └── --name (required)

  # Initialize a repository
  SubCommandDemo repo init --path /my/repo --bare
  
  # Clone a repository
  SubCommandDemo repo clone --url https://github.com/user/repo --branch develop
  
  # Add a remote
  SubCommandDemo repo remote add --name upstream --url https://github.com/upstream/repo
  
  # Remove a remote
  SubCommandDemo repo remote remove --name upstream
}

{$mode objfpc}{$H+}

uses
  SysUtils,
  CLI.Interfaces,
  CLI.Application,
  CLI.Parameter,
  CLI.Command,
  CLI.Console;

type
  { Base command for repo operations }
  TRepoCommand = class(TBaseCommand)
  public
    function Execute: Integer; override;
  end;

  { Init command }
  TRepoInitCommand = class(TBaseCommand)
  public
    function Execute: Integer; override;
  end;

  { Clone command }
  TRepoCloneCommand = class(TBaseCommand)
  public
    function Execute: Integer; override;
  end;

  { Remote command group }
  TRemoteCommand = class(TBaseCommand)
  public
    function Execute: Integer; override;
  end;

  { Remote add command }
  TRemoteAddCommand = class(TBaseCommand)
  public
    function Execute: Integer; override;
  end;

  { Remote remove command }
  TRemoteRemoveCommand = class(TBaseCommand)
  public
    function Execute: Integer; override;
  end;

{ TRepoCommand }
function TRepoCommand.Execute: Integer;
begin
  // This is a command group, just show help
  ShowHelp;
  Result := 0;
end;

{ TRepoInitCommand }
function TRepoInitCommand.Execute: Integer;
var
  Path: string;
  BareValue: string;
  Bare: Boolean;
begin
  Result := 0;
  if not GetParameterValue('--path', Path) then
    Path := GetCurrentDir;

  Bare := GetParameterValue('--bare', BareValue);

  TConsole.WriteLn('Initializing repository...', ccCyan);
  TConsole.WriteLn('  Path: ' + Path, ccWhite);
  if Bare then
    TConsole.WriteLn('  Type: Bare repository', ccWhite)
  else
    TConsole.WriteLn('  Type: Regular repository', ccWhite);
end;

{ TRepoCloneCommand }
function TRepoCloneCommand.Execute: Integer;
var
  URL, Path, Branch: string;
  Depth: string;
begin
  Result := 0;
  if not GetParameterValue('--url', URL) then
  begin
    TConsole.WriteLn('Error: URL is required', ccRed);
    Exit(1);
  end;

  if not GetParameterValue('--path', Path) then
    Path := ExtractFileName(URL);

  if not GetParameterValue('--branch', Branch) then
    Branch := 'main';

  if not GetParameterValue('--depth', Depth) then
    Depth := 'full';

  TConsole.WriteLn('Cloning repository...', ccCyan);
  TConsole.WriteLn('  From: ' + URL, ccWhite);
  TConsole.WriteLn('  To: ' + Path, ccWhite);
  TConsole.WriteLn('  Branch: ' + Branch, ccWhite);
  TConsole.WriteLn('  Depth: ' + Depth, ccWhite);
end;

{ TRemoteCommand }
function TRemoteCommand.Execute: Integer;
begin
  // This is a command group, just show help
  ShowHelp;
  Result := 0;
end;

{ TRemoteAddCommand }
function TRemoteAddCommand.Execute: Integer;
var
  RemoteName, RemoteURL: string;
begin
  Result := 0;
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

  TConsole.WriteLn('Adding remote...', ccCyan);
  TConsole.WriteLn('  Name: ' + RemoteName, ccWhite);
  TConsole.WriteLn('  URL: ' + RemoteURL, ccWhite);
end;

{ TRemoteRemoveCommand }
function TRemoteRemoveCommand.Execute: Integer;
var
  RemoteName: string;
begin
  Result := 0;
  if not GetParameterValue('--name', RemoteName) then
  begin
    TConsole.WriteLn('Error: Remote name is required', ccRed);
    Exit(1);
  end;

  TConsole.WriteLn('Removing remote...', ccCyan);
  TConsole.WriteLn('  Name: ' + RemoteName, ccWhite);
end;

var
  App: ICLIApplication;
  RepoCmd: TRepoCommand;
  InitCmd: TRepoInitCommand;
  CloneCmd: TRepoCloneCommand;
  RemoteCmd: TRemoteCommand;
  RemoteAddCmd: TRemoteAddCommand;
  RemoteRemoveCmd: TRemoteRemoveCommand;
begin
  try
    // Create main application
    App := CreateCLIApplication('RepoManager', '1.0.0');

    // Create main repo command group
    RepoCmd := TRepoCommand.Create('repo', 'Repository management commands');
    App.RegisterCommand(RepoCmd);

    // Create and configure init command
    InitCmd := TRepoInitCommand.Create('init', 'Initialize a new repository');
    InitCmd.AddParameter(CreateParameter('-p', '--path', 'Repository path', False, ptString));
    InitCmd.AddParameter(CreateParameter('-b', '--bare', 'Create a bare repository', False, ptFlag));
    RepoCmd.AddSubCommand(InitCmd);

    // Create and configure clone command
    CloneCmd := TRepoCloneCommand.Create('clone', 'Clone a repository');
    CloneCmd.AddParameter(CreateParameter('-u', '--url', 'Repository URL', True, ptString));
    CloneCmd.AddParameter(CreateParameter('-p', '--path', 'Target path', False, ptString));
    CloneCmd.AddParameter(CreateParameter('-b', '--branch', 'Branch to clone', False, ptString, 'main'));
    CloneCmd.AddParameter(CreateParameter('-d', '--depth', 'Clone depth', False, ptString, 'full'));
    RepoCmd.AddSubCommand(CloneCmd);

    // Create remote command group
    RemoteCmd := TRemoteCommand.Create('remote', 'Remote repository commands');
    RepoCmd.AddSubCommand(RemoteCmd);

    // Create and configure remote add command
    RemoteAddCmd := TRemoteAddCommand.Create('add', 'Add a new remote');
    RemoteAddCmd.AddParameter(CreateParameter('-n', '--name', 'Remote name', True, ptString));
    RemoteAddCmd.AddParameter(CreateParameter('-u', '--url', 'Remote URL', True, ptString));
    RemoteCmd.AddSubCommand(RemoteAddCmd);

    // Create and configure remote remove command
    RemoteRemoveCmd := TRemoteRemoveCommand.Create('remove', 'Remove a remote');
    RemoteRemoveCmd.AddParameter(CreateParameter('-n', '--name', 'Remote name', True, ptString));
    RemoteCmd.AddSubCommand(RemoteRemoveCmd);

    // Clean up command references
    RepoCmd := nil;
    InitCmd := nil;
    CloneCmd := nil;
    RemoteCmd := nil;
    RemoteAddCmd := nil;
    RemoteRemoveCmd := nil;

    // Execute the application
    ExitCode := App.Execute;
  except
    on E: Exception do
    begin
      TConsole.WriteLn('Error: ' + E.Message, ccRed);
      ExitCode := 1;
    end;
  end;
end. 