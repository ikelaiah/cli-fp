unit CliFpGen.Naming;

{$mode objfpc}{$H+}{$J-}

interface

uses
  Classes, SysUtils;

function NormalizePathSlashes(const S: string): string;
function NormalizeCommandName(const S: string): string;
function NormalizeCommandPath(const S: string): string;
function JoinCommandPath(const ParentPath, Name: string): string;
function PathDepth(const CommandPath: string): Integer;
function SegmentCount(const CommandPath: string): Integer;

function MakeProgramIdentifier(const AppName: string): string;
function MakeProgramFileRelPath(const AppName: string): string;
function MakeRegistryUnitName(const AppName: string): string;
function MakeCommandUnitName(const AppName, CommandPath: string): string;
function MakeCommandClassName(const CommandPath: string): string;
function MakeCommandVarName(const CommandPath: string): string;

function IsValidCommandToken(const S: string): Boolean;

implementation

function NormalizePathSlashes(const S: string): string;
begin
  Result := StringReplace(S, '\', '/', [rfReplaceAll]);
  Result := StringReplace(Result, '//', '/', [rfReplaceAll]);
  while Pos('//', Result) > 0 do
    Result := StringReplace(Result, '//', '/', [rfReplaceAll]);
end;

// NormalizeCommandName strips all path separators (both '/' and '\') and
// surrounding whitespace, yielding a plain single-token name.
// On Windows, backslashes in user input (e.g. 'repo\clone') are converted
// to forward slashes by NormalizePathSlashes before being stripped here,
// so only the final segment survives. Callers that expect a path should
// use NormalizeCommandPath instead.
function NormalizeCommandName(const S: string): string;
begin
  Result := Trim(NormalizePathSlashes(S));
  Result := StringReplace(Result, '/', '', [rfReplaceAll]);
end;

function NormalizeCommandPath(const S: string): string;
var
  Parts, OutParts: TStringList;
  i: Integer;
  Item: string;
begin
  Result := Trim(NormalizePathSlashes(S));
  if Result = '' then
    Exit('');

  Parts := TStringList.Create;
  OutParts := TStringList.Create;
  try
    Parts.Delimiter := '/';
    Parts.StrictDelimiter := True;
    Parts.DelimitedText := Result;
    for i := 0 to Parts.Count - 1 do
    begin
      Item := Trim(Parts[i]);
      if Item <> '' then
        OutParts.Add(Item);
    end;
    Result := StringReplace(Trim(OutParts.Text), LineEnding, '/', [rfReplaceAll]);
    if (Result <> '') and (Result[Length(Result)] = '/') then
      Delete(Result, Length(Result), 1);
  finally
    OutParts.Free;
    Parts.Free;
  end;
end;

function JoinCommandPath(const ParentPath, Name: string): string;
var
  P, N: string;
begin
  P := NormalizeCommandPath(ParentPath);
  N := NormalizeCommandName(Name);
  if P = '' then
    Result := N
  else
    Result := P + '/' + N;
end;

function PathDepth(const CommandPath: string): Integer;
begin
  if Trim(CommandPath) = '' then
    Exit(0);
  Result := SegmentCount(CommandPath);
end;

function SegmentCount(const CommandPath: string): Integer;
var
  i: Integer;
begin
  if Trim(CommandPath) = '' then
    Exit(0);
  Result := 1;
  for i := 1 to Length(CommandPath) do
    if CommandPath[i] = '/' then
      Inc(Result);
end;

function TokenToPascalPart(const S: string): string;
var
  i: Integer;
  NextUpper: Boolean;
  Ch: Char;
begin
  Result := '';
  NextUpper := True;
  for i := 1 to Length(S) do
  begin
    Ch := S[i];
    if Ch in ['A'..'Z', 'a'..'z', '0'..'9'] then
    begin
      if NextUpper then
        Result := Result + UpCase(Ch)
      else
        Result := Result + LowerCase(Ch);
      NextUpper := False;
    end
    else
      NextUpper := True;
  end;
  if Result = '' then
    Result := 'X';
  if Result[1] in ['0'..'9'] then
    Result := 'N' + Result;
end;

function PathToPascal(const S: string): string;
var
  P: string;
  Parts: TStringList;
  i: Integer;
begin
  P := NormalizeCommandPath(S);
  if P = '' then
    Exit('Root');
  Parts := TStringList.Create;
  try
    Parts.Delimiter := '/';
    Parts.StrictDelimiter := True;
    Parts.DelimitedText := P;
    Result := '';
    for i := 0 to Parts.Count - 1 do
      Result := Result + TokenToPascalPart(Parts[i]);
  finally
    Parts.Free;
  end;
end;

function NormalizeUnitId(const S: string): string;
var
  i: Integer;
  Ch: Char;
begin
  Result := '';
  for i := 1 to Length(S) do
  begin
    Ch := S[i];
    if Ch in ['A'..'Z', 'a'..'z', '0'..'9', '_'] then
      Result := Result + Ch
    else
      Result := Result + '_';
  end;
  if Result = '' then
    Result := 'GeneratedUnit';
  if Result[1] in ['0'..'9'] then
    Result := 'U_' + Result;
end;

function MakeProgramIdentifier(const AppName: string): string;
begin
  Result := TokenToPascalPart(AppName);
end;

// Returns a relative path such as 'src/Myapp.lpr'. The filename is
// PascalCase (first letter capitalised) per Free Pascal conventions.
// On case-sensitive filesystems (Linux/macOS) the generated .lpr file
// must be referenced by the exact same casing in build scripts.
function MakeProgramFileRelPath(const AppName: string): string;
begin
  Result := 'src' + PathDelim + MakeProgramIdentifier(AppName) + '.lpr';
end;

function MakeRegistryUnitName(const AppName: string): string;
begin
  Result := NormalizeUnitId(TokenToPascalPart(AppName) + '_CommandRegistry_Generated');
end;

function MakeCommandUnitName(const AppName, CommandPath: string): string;
begin
  Result := NormalizeUnitId(TokenToPascalPart(AppName) + '_Command_' + PathToPascal(CommandPath));
end;

function MakeCommandClassName(const CommandPath: string): string;
begin
  Result := 'T' + PathToPascal(CommandPath) + 'Command';
end;

function MakeCommandVarName(const CommandPath: string): string;
begin
  Result := 'Cmd' + PathToPascal(CommandPath);
end;

function IsValidCommandToken(const S: string): Boolean;
var
  i: Integer;
  Ch: Char;
begin
  Result := False;
  if S = '' then
    Exit;
  for i := 1 to Length(S) do
  begin
    Ch := S[i];
    if not (Ch in ['A'..'Z', 'a'..'z', '0'..'9', '-', '_']) then
      Exit(False);
  end;
  Result := True;
end;

end.
