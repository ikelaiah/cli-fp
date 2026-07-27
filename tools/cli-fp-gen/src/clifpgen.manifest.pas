unit CliFpGen.Manifest;

{$mode objfpc}{$H+}{$J-}

interface

uses
  Classes, SysUtils, CliFpGen.Model;

function ManifestRelPath: string;
function LoadGeneratedManifest(const ProjectDir: string): TStringList;
procedure SaveGeneratedManifest(const ProjectDir: string; const GeneratedRelFiles: TStrings; const Options: TWriteOptions);
procedure CleanupStaleGeneratedFiles(const ProjectDir: string; const PreviousRelFiles, CurrentRelFiles: TStrings; const Options: TWriteOptions);

implementation

uses
  fpjson, jsonparser, CliFpGen.Filesystem
  {$IFDEF MSWINDOWS}, Windows{$ENDIF};

function ManifestRelPath: string;
begin
  Result := 'src/generated/.clifp-manifest.json';
end;

function NormalizeRelPath(const S: string): string;
begin
  Result := StringReplace(Trim(S), '\', '/', [rfReplaceAll]);
end;

procedure ConfigurePathList(const Paths: TStringList);
begin
  {$IFDEF MSWINDOWS}
  Paths.CaseSensitive := False;
  {$ELSE}
  Paths.CaseSensitive := True;
  {$ENDIF}
end;

function ProjectPath(const ProjectDir, RelPath: string): string;
begin
  Result := ExpandFileName(IncludeTrailingPathDelimiter(ProjectDir) +
    StringReplace(RelPath, '/', PathDelim, [rfReplaceAll]));
end;

function IsWithinProjectDir(const ProjectDir, FileName: string): Boolean;
var
  RootNorm, FileNorm: string;
begin
  RootNorm := IncludeTrailingPathDelimiter(ExpandFileName(ProjectDir));
  FileNorm := ExpandFileName(FileName);
  {$IFDEF MSWINDOWS}
  Result := SameText(Copy(FileNorm, 1, Length(RootNorm)), RootNorm);
  {$ELSE}
  Result := Copy(FileNorm, 1, Length(RootNorm)) = RootNorm;
  {$ENDIF}
end;

function PathsEqual(const LeftPath, RightPath: string): Boolean;
begin
  {$IFDEF MSWINDOWS}
  Result := SameText(LeftPath, RightPath);
  {$ELSE}
  Result := LeftPath = RightPath;
  {$ENDIF}
end;

function IsLinkOrReparsePoint(const FileName: string): Boolean;
var
  Attributes: LongInt;
begin
  Attributes := FileGetAttr(FileName);
  {$IFDEF MSWINDOWS}
  Result := (Attributes <> -1) and
    ((Attributes and FILE_ATTRIBUTE_REPARSE_POINT) <> 0);
  {$ELSE}
  Result := (Attributes <> -1) and ((Attributes and faSymLink) <> 0);
  {$ENDIF}
end;

function FindLinkOrReparsePoint(const ProjectDir, FileName: string;
  out LinkPath: string): Boolean;
var
  RootNorm: string;
  CurrentPath: string;
  ParentPath: string;
begin
  Result := False;
  LinkPath := '';
  RootNorm := ExcludeTrailingPathDelimiter(ExpandFileName(ProjectDir));
  CurrentPath := ExcludeTrailingPathDelimiter(ExpandFileName(FileName));

  while not PathsEqual(CurrentPath, RootNorm) do
  begin
    if IsLinkOrReparsePoint(CurrentPath) then
    begin
      LinkPath := CurrentPath;
      Exit(True);
    end;

    ParentPath := ExcludeTrailingPathDelimiter(ExtractFileDir(CurrentPath));
    if PathsEqual(ParentPath, CurrentPath) then
      Exit;
    CurrentPath := ParentPath;
  end;
end;

function LoadGeneratedManifest(const ProjectDir: string): TStringList;
var
  FileName: string;
  Root: TJSONData;
  Obj: TJSONObject;
  Arr: TJSONArray;
  i: Integer;
begin
  Result := TStringList.Create;
  ConfigurePathList(Result);
  Result.Sorted := False;
  Result.Duplicates := dupIgnore;

  FileName := ProjectPath(ProjectDir, ManifestRelPath);
  if not FileExists(FileName) then
    Exit;

  Root := GetJSON(ReadTextFileStrict(FileName));
  try
    if not (Root is TJSONObject) then
      Exit;
    Obj := TJSONObject(Root);
    if (Obj.Find('generatedFiles') = nil) or not (Obj.Arrays['generatedFiles'] is TJSONArray) then
      Exit;
    Arr := Obj.Arrays['generatedFiles'];
    for i := 0 to Arr.Count - 1 do
      Result.Add(NormalizeRelPath(Arr.Strings[i]));
  finally
    Root.Free;
  end;
end;

procedure SaveGeneratedManifest(const ProjectDir: string; const GeneratedRelFiles: TStrings; const Options: TWriteOptions);
var
  Root: TJSONObject;
  Arr: TJSONArray;
  i: Integer;
  ManifestFile: string;
begin
  Root := TJSONObject.Create;
  try
    Root.Add('schemaVersion', 1);
    Arr := TJSONArray.Create;
    for i := 0 to GeneratedRelFiles.Count - 1 do
      Arr.Add(NormalizeRelPath(GeneratedRelFiles[i]));
    Root.Add('generatedFiles', Arr);

    ManifestFile := ProjectPath(ProjectDir, ManifestRelPath);
    WriteManagedTextFile(ManifestFile, Root.FormatJSON([], 2) + LineEnding, wkGenerated, Options);
  finally
    Root.Free;
  end;
end;

procedure CleanupStaleGeneratedFiles(const ProjectDir: string; const PreviousRelFiles, CurrentRelFiles: TStrings; const Options: TWriteOptions);
var
  CurrentSet: TStringList;
  i: Integer;
  RelPath: string;
  AbsPath: string;
  LinkPath: string;
begin
  CurrentSet := TStringList.Create;
  try
    ConfigurePathList(CurrentSet);
    for i := 0 to CurrentRelFiles.Count - 1 do
      CurrentSet.Add(NormalizeRelPath(CurrentRelFiles[i]));

    for i := 0 to PreviousRelFiles.Count - 1 do
    begin
      RelPath := NormalizeRelPath(PreviousRelFiles[i]);
      if SameText(RelPath, NormalizeRelPath(ManifestRelPath)) then
        Continue;
      if CurrentSet.IndexOf(RelPath) >= 0 then
        Continue;
      AbsPath := ProjectPath(ProjectDir, RelPath);
      if not IsWithinProjectDir(ProjectDir, AbsPath) then
        raise Exception.CreateFmt('Refusing to delete outside project dir: %s', [AbsPath]);
      if FindLinkOrReparsePoint(ProjectDir, AbsPath, LinkPath) then
        raise Exception.CreateFmt(
          'Refusing to delete through symbolic link or reparse point: %s',
          [LinkPath]);
      DeleteManagedFile(AbsPath, Options);
    end;
  finally
    CurrentSet.Free;
  end;
end;

end.
