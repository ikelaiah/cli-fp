program MyGit;

{$mode objfpc}{$H+}{$J-}

uses
  SysUtils,
  CLI.Interfaces,
  CLI.Application,
  CLI.Command,
  CLI.Parameter,
  CLI.Console,
  CLI.Progress;

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
