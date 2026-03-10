unit CliFpGen.Filesystem;

{$mode objfpc}{$H+}{$J-}

interface

uses
  Classes, SysUtils, CliFpGen.Model;

type
  TWriteKind = (wkGenerated, wkUserStub);

procedure EnsureDirectoryExistsSafe(const DirPath: string; const Options: TWriteOptions);
procedure WriteManagedTextFile(const FileName, Content: string; Kind: TWriteKind; const Options: TWriteOptions);
function ReadTextFileStrict(const FileName: string): string;
procedure DeleteManagedFile(const FileName: string; const Options: TWriteOptions);

implementation

procedure EnsureDirectoryExistsSafe(const DirPath: string; const Options: TWriteOptions);
begin
  if DirPath = '' then
    Exit;

  if DirectoryExists(DirPath) then
    Exit;

  if Options.DryRun then
  begin
    WriteLn('[dry-run] mkdir ', DirPath);
    Exit;
  end;

  if not ForceDirectories(DirPath) then
    raise Exception.CreateFmt('Failed to create directory: %s', [DirPath]);

  WriteLn('[write] mkdir ', DirPath);
end;

function ReadTextFileStrict(const FileName: string): string;
var
  Stream: TFileStream;
  Bytes: TBytes;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    SetLength(Bytes, Stream.Size);
    if Length(Bytes) > 0 then
    begin
      Stream.ReadBuffer(Bytes[0], Length(Bytes));
      SetString(Result, PChar(@Bytes[0]), Length(Bytes));
    end
    else
      Result := '';
  finally
    Stream.Free;
  end;
end;

procedure WriteStringToFile(const FileName, Content: string);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    if Content <> '' then
      Stream.WriteBuffer(Content[1], Length(Content));
  finally
    Stream.Free;
  end;
end;

procedure WriteManagedTextFile(const FileName, Content: string; Kind: TWriteKind; const Options: TWriteOptions);
var
  Existing: string;
  FileExistsAlready: Boolean;
  AllowOverwrite: Boolean;
begin
  EnsureDirectoryExistsSafe(ExtractFileDir(FileName), Options);

  FileExistsAlready := FileExists(FileName);

  if FileExistsAlready then
  begin
    Existing := ReadTextFileStrict(FileName);
    if Existing = Content then
    begin
      WriteLn('[skip]  unchanged ', FileName);
      Exit;
    end;
  end;

  AllowOverwrite := (Kind = wkGenerated) or Options.Force;
  if FileExistsAlready and not AllowOverwrite then
  begin
    WriteLn('[skip]  exists (user-owned) ', FileName);
    Exit;
  end;

  if Options.DryRun then
  begin
    if FileExistsAlready then
      WriteLn('[dry-run] overwrite ', FileName)
    else
      WriteLn('[dry-run] create ', FileName);
    Exit;
  end;

  WriteStringToFile(FileName, Content);
  if FileExistsAlready then
    WriteLn('[write] overwrite ', FileName)
  else
    WriteLn('[write] create ', FileName);
end;

procedure DeleteManagedFile(const FileName: string; const Options: TWriteOptions);
begin
  if not FileExists(FileName) then
    Exit;

  if Options.DryRun then
  begin
    WriteLn('[dry-run] delete ', FileName);
    Exit;
  end;

  if not SysUtils.DeleteFile(FileName) then
    raise Exception.CreateFmt('Failed to delete file: %s', [FileName]);
  WriteLn('[write] delete ', FileName);
end;

end.
