unit CliFpGen.App;

{$mode objfpc}{$H+}{$J-}

interface

procedure RunCliFpGen;

implementation

uses
  SysUtils, CliFpGen.Model, CliFpGen.Generate;

function ArgOrEmpty(const Index: Integer): string;
begin
  if (Index >= 1) and (Index <= ParamCount) then
    Result := ParamStr(Index)
  else
    Result := '';
end;

procedure ShowHelp;
begin
  WriteLn('cli-fp-gen - Free Pascal CLI project generator');
  WriteLn('');
  WriteLn('Usage:');
  WriteLn('  cli-fp-gen init <target-dir> [--name <app-name>] [--version <x.y.z>] [--dry-run] [--force]');
  WriteLn('  cli-fp-gen generate [--project <dir-or-spec-file>] [--dry-run] [--force]');
  WriteLn('  cli-fp-gen add command <name> [--parent <cmd/path>] [--description <text>] [--project <dir-or-spec-file>] [--dry-run] [--force]');
  WriteLn('  cli-fp-gen remove command <cmd/path> [--cascade] [--project <dir-or-spec-file>] [--dry-run] [--force]');
  WriteLn('');
  WriteLn('Notes:');
  WriteLn('  - Project spec is clifp.json');
  WriteLn('  - Generated files are written to src/generated');
  WriteLn('  - Command stubs are created in src/commands and not overwritten unless --force');
end;

function ResolveSpecFile(const ProjectArg: string): string;
var
  Candidate: string;
begin
  Candidate := Trim(ProjectArg);
  if Candidate = '' then
    Candidate := GetCurrentDir;

  Candidate := ExpandFileName(Candidate);
  if DirectoryExists(Candidate) then
    Result := IncludeTrailingPathDelimiter(Candidate) + 'clifp.json'
  else
    Result := Candidate;
end;

procedure ParseWriteOptions(var Index: Integer; var Opts: TWriteOptions; const Count: Integer);
var
  Arg: string;
begin
  while Index <= Count do
  begin
    Arg := ArgOrEmpty(Index);
    if Arg = '--dry-run' then
      Opts.DryRun := True
    else if Arg = '--force' then
      Opts.Force := True
    else
      Break;
    Inc(Index);
  end;
end;

procedure HandleInit;
var
  TargetDir, AppName, AppVersion: string;
  i: Integer;
  Opts: TWriteOptions;
  Arg: string;
begin
  if ParamCount < 2 then
    raise Exception.Create('init requires <target-dir>');

  FillChar(Opts, SizeOf(Opts), 0);
  TargetDir := ExpandFileName(ParamStr(2));
  AppName := '';
  AppVersion := '0.1.0';

  i := 3;
  while i <= ParamCount do
  begin
    Arg := ArgOrEmpty(i);
    if Arg = '--name' then
    begin
      Inc(i);
      if i > ParamCount then raise Exception.Create('--name requires a value');
      AppName := ArgOrEmpty(i);
    end
    else if Arg = '--version' then
    begin
      Inc(i);
      if i > ParamCount then raise Exception.Create('--version requires a value');
      AppVersion := ArgOrEmpty(i);
    end
    else if Arg = '--dry-run' then
      Opts.DryRun := True
    else if Arg = '--force' then
      Opts.Force := True
    else
      raise Exception.CreateFmt('Unknown option for init: %s', [Arg]);
    Inc(i);
  end;

  InitNewProject(TargetDir, AppName, Opts, AppVersion);
end;

procedure HandleGenerate;
var
  i: Integer;
  SpecFile, ProjectArg, Arg: string;
  Opts: TWriteOptions;
begin
  FillChar(Opts, SizeOf(Opts), 0);
  ProjectArg := '';
  i := 2;
  while i <= ParamCount do
  begin
    Arg := ArgOrEmpty(i);
    if Arg = '--project' then
    begin
      Inc(i);
      if i > ParamCount then raise Exception.Create('--project requires a value');
      ProjectArg := ArgOrEmpty(i);
    end
    else if Arg = '--dry-run' then
      Opts.DryRun := True
    else if Arg = '--force' then
      Opts.Force := True
    else
      raise Exception.CreateFmt('Unknown option for generate: %s', [Arg]);
    Inc(i);
  end;

  SpecFile := ResolveSpecFile(ProjectArg);
  GenerateFromSpecFile(SpecFile, Opts);
end;

procedure HandleAddCommand;
var
  i: Integer;
  CommandName, ParentPath, Description, ProjectArg, Arg: string;
  Opts: TWriteOptions;
begin
  if ParamCount < 3 then
    raise Exception.Create('Usage: cli-fp-gen add command <name> [options]');

  FillChar(Opts, SizeOf(Opts), 0);
  CommandName := ParamStr(3);
  ParentPath := '';
  Description := '';
  ProjectArg := '';

  i := 4;
  while i <= ParamCount do
  begin
    Arg := ArgOrEmpty(i);
    if Arg = '--parent' then
    begin
      Inc(i);
      if i > ParamCount then raise Exception.Create('--parent requires a value');
      ParentPath := ArgOrEmpty(i);
    end
    else if Arg = '--description' then
    begin
      Inc(i);
      if i > ParamCount then raise Exception.Create('--description requires a value');
      Description := ArgOrEmpty(i);
      while (i < ParamCount) and (Copy(ArgOrEmpty(i + 1), 1, 2) <> '--') do
      begin
        Inc(i);
        Description := Description + ' ' + ArgOrEmpty(i);
      end;
    end
    else if Arg = '--project' then
    begin
      Inc(i);
      if i > ParamCount then raise Exception.Create('--project requires a value');
      ProjectArg := ArgOrEmpty(i);
    end
    else if Arg = '--dry-run' then
      Opts.DryRun := True
    else if Arg = '--force' then
      Opts.Force := True
    else
      raise Exception.CreateFmt('Unknown option for add command: %s', [Arg]);
    Inc(i);
  end;

  AddCommandToProject(ResolveSpecFile(ProjectArg), CommandName, ParentPath, Description, Opts);
end;

procedure HandleRemoveCommand;
var
  i: Integer;
  CommandPath, ProjectArg, Arg: string;
  Cascade: Boolean;
  Opts: TWriteOptions;
begin
  if ParamCount < 3 then
    raise Exception.Create('Usage: cli-fp-gen remove command <cmd/path> [options]');

  FillChar(Opts, SizeOf(Opts), 0);
  CommandPath := ParamStr(3);
  ProjectArg := '';
  Cascade := False;

  i := 4;
  while i <= ParamCount do
  begin
    Arg := ArgOrEmpty(i);
    if Arg = '--project' then
    begin
      Inc(i);
      if i > ParamCount then raise Exception.Create('--project requires a value');
      ProjectArg := ArgOrEmpty(i);
    end
    else if Arg = '--cascade' then
      Cascade := True
    else if Arg = '--dry-run' then
      Opts.DryRun := True
    else if Arg = '--force' then
      Opts.Force := True
    else
      raise Exception.CreateFmt('Unknown option for remove command: %s', [Arg]);
    Inc(i);
  end;

  RemoveCommandFromProject(ResolveSpecFile(ProjectArg), CommandPath, Cascade, Opts);
end;

procedure RunCliFpGen;
var
  Cmd1, Cmd2: string;
begin
  if (ParamCount = 0) or (ParamStr(1) = '--help') or (ParamStr(1) = '-h') then
  begin
    ShowHelp;
    Exit;
  end;

  Cmd1 := LowerCase(ParamStr(1));
  if Cmd1 = 'init' then
    HandleInit
  else if Cmd1 = 'generate' then
    HandleGenerate
  else if Cmd1 = 'add' then
  begin
    Cmd2 := LowerCase(ArgOrEmpty(2));
    if Cmd2 = 'command' then
      HandleAddCommand
    else
      raise Exception.Create('Only "add command" is implemented in Phase 1');
  end
  else if Cmd1 = 'remove' then
  begin
    Cmd2 := LowerCase(ArgOrEmpty(2));
    if Cmd2 = 'command' then
      HandleRemoveCommand
    else
      raise Exception.Create('Only "remove command" is implemented');
  end
  else
    raise Exception.CreateFmt('Unknown command: %s', [ParamStr(1)]);
end;

end.
