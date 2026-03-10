unit CliFpGen.Generate;

{$mode objfpc}{$H+}{$J-}

interface

uses
  Classes, SysUtils, CliFpGen.Model;

procedure GenerateFromSpecFile(const SpecFile: string; const Options: TWriteOptions);
procedure InitNewProject(const TargetDir, AppName: string; const Options: TWriteOptions; const AppVersion: string = '0.1.0');
procedure AddCommandToProject(const SpecFile, CommandName, ParentPath, Description: string; const Options: TWriteOptions);
procedure RemoveCommandFromProject(const SpecFile, CommandPath: string; Cascade: Boolean; const Options: TWriteOptions);

implementation

uses
  CliFpGen.Naming,
  CliFpGen.SpecIO,
  CliFpGen.Validate,
  CliFpGen.Renderer,
  CliFpGen.Filesystem,
  CliFpGen.Manifest;

function PathCombine(const BaseDir, RelPath: string): string;
begin
  Result := ExpandFileName(IncludeTrailingPathDelimiter(BaseDir) + RelPath);
end;

function FindCommandByFullPath(const Spec: TProjectSpec; const FullPath: string): TCommandSpec;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Spec.Commands.Count - 1 do
    if SameText(CommandFullPath(Spec.Commands[i]), FullPath) then
      Exit(Spec.Commands[i]);
end;

function NormalizeRelPath(const S: string): string;
begin
  Result := StringReplace(S, '\', '/', [rfReplaceAll]);
end;

function IsDescendantPath(const BasePath, CandidatePath: string): Boolean;
begin
  Result := SameText(CandidatePath, BasePath) or
    (Copy(AnsiLowerCase(CandidatePath), 1, Length(BasePath) + 1) = AnsiLowerCase(BasePath) + '/');
end;

procedure GenerateProjectFiles(const ProjectDir: string; const Spec: TProjectSpec; const Options: TWriteOptions);
var
  i: Integer;
  Cmd: TCommandSpec;
  ProgramPath, RegistryPath, CommandUnitPath, CommandsDir, GeneratedDir: string;
  PreviousManifest, CurrentGeneratedRel: TStringList;
  RegistryRelPath: string;
begin
  ValidateProjectSpec(Spec);

  EnsureDirectoryExistsSafe(ProjectDir, Options);
  EnsureDirectoryExistsSafe(PathCombine(ProjectDir, 'src'), Options);
  CommandsDir := PathCombine(ProjectDir, 'src' + PathDelim + 'commands');
  GeneratedDir := PathCombine(ProjectDir, 'src' + PathDelim + 'generated');
  EnsureDirectoryExistsSafe(CommandsDir, Options);
  EnsureDirectoryExistsSafe(GeneratedDir, Options);

  PreviousManifest := LoadGeneratedManifest(ProjectDir);
  CurrentGeneratedRel := TStringList.Create;
  try
    CurrentGeneratedRel.CaseSensitive := False;
    CurrentGeneratedRel.Duplicates := dupIgnore;
    CurrentGeneratedRel.Add(NormalizeRelPath(Spec.ProgramFile));
    RegistryRelPath := 'src/generated/' + MakeRegistryUnitName(Spec.AppName) + '.pas';
    CurrentGeneratedRel.Add(NormalizeRelPath(RegistryRelPath));
    CurrentGeneratedRel.Add(NormalizeRelPath(ManifestRelPath));

    ProgramPath := PathCombine(ProjectDir, Spec.ProgramFile);
    WriteManagedTextFile(ProgramPath, RenderProgramFile(Spec), wkGenerated, Options);

    RegistryPath := PathCombine(ProjectDir, StringReplace(RegistryRelPath, '/', PathDelim, [rfReplaceAll]));
    WriteManagedTextFile(RegistryPath, RenderRegistryUnit(Spec), wkGenerated, Options);

    for i := 0 to Spec.Commands.Count - 1 do
    begin
      Cmd := Spec.Commands[i];
      CommandUnitPath := PathCombine(ProjectDir, 'src' + PathDelim + 'commands' + PathDelim +
        MakeCommandUnitName(Spec.AppName, CommandFullPath(Cmd)) + '.pas');
      WriteManagedTextFile(
        CommandUnitPath,
        RenderCommandUnit(Spec, Cmd),
        wkUserStub,
        Options
      );
    end;

    CleanupStaleGeneratedFiles(ProjectDir, PreviousManifest, CurrentGeneratedRel, Options);
    SaveGeneratedManifest(ProjectDir, CurrentGeneratedRel, Options);
  finally
    CurrentGeneratedRel.Free;
    PreviousManifest.Free;
  end;
end;

procedure GenerateFromSpecFile(const SpecFile: string; const Options: TWriteOptions);
var
  Spec: TProjectSpec;
  ProjectDir: string;
begin
  Spec := LoadProjectSpec(SpecFile);
  try
    ProjectDir := ExtractFileDir(ExpandFileName(SpecFile));
    GenerateProjectFiles(ProjectDir, Spec, Options);
  finally
    Spec.Free;
  end;
end;

procedure InitNewProject(const TargetDir, AppName: string; const Options: TWriteOptions; const AppVersion: string = '0.1.0');
var
  Spec: TProjectSpec;
  Cmd: TCommandSpec;
  Param: TParameterSpec;
  ResolvedAppName: string;
  SpecFile: string;
begin
  ResolvedAppName := Trim(AppName);
  if ResolvedAppName = '' then
    ResolvedAppName := ExtractFileName(ExcludeTrailingPathDelimiter(TargetDir));
  if ResolvedAppName = '' then
    raise Exception.Create('Unable to infer app name from target directory. Use --name.');

  Spec := TProjectSpec.Create;
  try
    Spec.AppName := ResolvedAppName;
    Spec.AppVersion := AppVersion;
    Spec.ProgramFile := MakeProgramFileRelPath(ResolvedAppName);

    Cmd := TCommandSpec.Create;
    Cmd.Name := 'greet';
    Cmd.Description := 'Say hello';
    Cmd.ParentPath := '';
    Param := TParameterSpec.Create;
    Param.Kind := pkString;
    Param.ShortFlag := '-n';
    Param.LongFlag := '--name';
    Param.Description := 'Name to greet';
    Param.Required := False;
    Param.DefaultValue := 'World';
    Cmd.Parameters.Add(Param);
    Spec.Commands.Add(Cmd);

    ValidateProjectSpec(Spec);

    EnsureDirectoryExistsSafe(TargetDir, Options);
    SpecFile := PathCombine(TargetDir, 'clifp.json');
    SaveProjectSpec(Spec, SpecFile, Options);
    GenerateProjectFiles(TargetDir, Spec, Options);
  finally
    Spec.Free;
  end;
end;

procedure AddCommandToProject(const SpecFile, CommandName, ParentPath, Description: string; const Options: TWriteOptions);
var
  Spec: TProjectSpec;
  Cmd: TCommandSpec;
  FullPath, ParentNorm, NameNorm: string;
begin
  Spec := LoadProjectSpec(SpecFile);
  try
    NameNorm := NormalizeCommandName(CommandName);
    ParentNorm := NormalizeCommandPath(ParentPath);
    if NameNorm = '' then
      raise Exception.Create('Command name is required');
    if not IsValidCommandToken(NameNorm) then
      raise Exception.CreateFmt('Invalid command name "%s"', [CommandName]);

    FullPath := JoinCommandPath(ParentNorm, NameNorm);
    if FindCommandByFullPath(Spec, FullPath) <> nil then
      raise Exception.CreateFmt('Command "%s" already exists', [FullPath]);
    if (ParentNorm <> '') and (FindCommandByFullPath(Spec, ParentNorm) = nil) then
      raise Exception.CreateFmt('Parent command "%s" does not exist', [ParentNorm]);

    Cmd := TCommandSpec.Create;
    Cmd.Name := NameNorm;
    if Trim(Description) <> '' then
      Cmd.Description := Description
    else
      Cmd.Description := 'TODO: Describe "' + NameNorm + '" command';
    Cmd.ParentPath := ParentNorm;
    Spec.Commands.Add(Cmd);

    ValidateProjectSpec(Spec);
    SaveProjectSpec(Spec, SpecFile, Options);
    GenerateFromSpecFile(SpecFile, Options);
  finally
    Spec.Free;
  end;
end;

procedure RemoveCommandFromProject(const SpecFile, CommandPath: string; Cascade: Boolean; const Options: TWriteOptions);
var
  Spec: TProjectSpec;
  PathNorm: string;
  i: Integer;
  RemovedAny, HasChildren: Boolean;
  CandidatePath: string;
begin
  Spec := LoadProjectSpec(SpecFile);
  try
    PathNorm := NormalizeCommandPath(CommandPath);
    if PathNorm = '' then
      raise Exception.Create('Command path is required');
    if FindCommandByFullPath(Spec, PathNorm) = nil then
      raise Exception.CreateFmt('Command "%s" does not exist', [PathNorm]);

    HasChildren := False;
    for i := 0 to Spec.Commands.Count - 1 do
      if SameText(Spec.Commands[i].ParentPath, PathNorm) then
      begin
        HasChildren := True;
        Break;
      end;
    if HasChildren and (not Cascade) then
      raise Exception.CreateFmt('Command "%s" has subcommands; use --cascade to remove subtree', [PathNorm]);

    RemovedAny := False;
    for i := Spec.Commands.Count - 1 downto 0 do
    begin
      CandidatePath := CommandFullPath(Spec.Commands[i]);
      if Cascade then
      begin
        if IsDescendantPath(PathNorm, CandidatePath) then
        begin
          Spec.Commands.Delete(i);
          RemovedAny := True;
        end;
      end
      else if SameText(CandidatePath, PathNorm) then
      begin
        Spec.Commands.Delete(i);
        RemovedAny := True;
      end;
    end;

    if not RemovedAny then
      raise Exception.CreateFmt('Command "%s" was not removed', [PathNorm]);

    ValidateProjectSpec(Spec);
    SaveProjectSpec(Spec, SpecFile, Options);
    GenerateFromSpecFile(SpecFile, Options);
  finally
    Spec.Free;
  end;
end;

end.
