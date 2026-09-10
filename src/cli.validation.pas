unit CLI.Validation;

{$mode objfpc}{$H+}{$J-}

interface

uses
  Classes, SysUtils, CLI.Interfaces;

procedure ValidateCommandName(const Name, Context: string;
  const AllowEmpty: Boolean = False);
procedure ValidateParameterDefinition(const Parameter: ICommandParameter;
  const Context: string);
procedure ValidateCommandTree(const Command: ICommand;
  const Context: string; const AllowEmptyRoot: Boolean = False);
function CommandTreeContainsName(const Root: ICommand;
  const TargetName: string): Boolean;

implementation

function IsAsciiAlphaNumeric(const Character: Char): Boolean;
begin
  Result := Character in ['A'..'Z', 'a'..'z', '0'..'9'];
end;

function IsValidLongFlag(const Flag: string): Boolean;
var
  i: Integer;
begin
  Result := (Length(Flag) >= 3) and (Copy(Flag, 1, 2) = '--') and
    IsAsciiAlphaNumeric(Flag[3]);
  if not Result then
    Exit;
  for i := 3 to Length(Flag) do
    if not (IsAsciiAlphaNumeric(Flag[i]) or (Flag[i] in ['-', '_'])) then
      Exit(False);
end;

function IsValidShortFlag(const Flag: string): Boolean;
begin
  Result := (Length(Flag) = 2) and (Flag[1] = '-') and
    (Flag[2] <> '-') and (Ord(Flag[2]) >= 33) and (Ord(Flag[2]) < 127) and
    not (Flag[2] in ['=']);
end;

procedure ValidateCommandName(const Name, Context: string;
  const AllowEmpty: Boolean);
var
  i: Integer;
begin
  if (Name = '') and AllowEmpty then
    Exit;
  if Name = '' then
    raise EArgumentException.CreateFmt('%s name must not be empty', [Context]);
  for i := 1 to Length(Name) do
    if not (IsAsciiAlphaNumeric(Name[i]) or (Name[i] in ['-', '_'])) then
      raise EArgumentException.CreateFmt(
        '%s name "%s" contains invalid command-name token characters',
        [Context, Name]);
  if Name[1] = '-' then
    raise EArgumentException.CreateFmt(
      '%s name "%s" must not begin with a dash', [Context, Name]);
end;

procedure ValidateParameterDefinition(const Parameter: ICommandParameter;
  const Context: string);
begin
  if not Assigned(Parameter) then
    raise EArgumentNilException.CreateFmt('%s parameter cannot be nil', [Context]);
  if (Parameter.ShortFlag = '') and (Parameter.LongFlag = '') then
    raise EArgumentException.CreateFmt(
      '%s parameter must define a short or long flag', [Context]);
  if (Parameter.LongFlag <> '') and not IsValidLongFlag(Parameter.LongFlag) then
    raise EArgumentException.CreateFmt(
      '%s parameter has invalid long flag "%s"', [Context, Parameter.LongFlag]);
  if (Parameter.ShortFlag <> '') and not IsValidShortFlag(Parameter.ShortFlag) then
    raise EArgumentException.CreateFmt(
      '%s parameter has invalid short flag "%s"', [Context, Parameter.ShortFlag]);
end;

function TreeContainsName(const Root: ICommand; const TargetName: string;
  const Active: TStringList): Boolean;
var
  Children: specialize TArray<ICommand>;
  i: Integer;
begin
  Result := False;
  if not Assigned(Root) then
    Exit;
  if SameText(Root.Name, TargetName) then
    Exit(True);
  if Active.IndexOf(Root.Name) >= 0 then
    Exit;
  Active.Add(Root.Name);
  try
    Children := Root.SubCommands;
    for i := 0 to Length(Children) - 1 do
      if TreeContainsName(Children[i], TargetName, Active) then
        Exit(True);
  finally
    Active.Delete(Active.Count - 1);
  end;
end;

function CommandTreeContainsName(const Root: ICommand;
  const TargetName: string): Boolean;
var
  Active: TStringList;
begin
  Active := TStringList.Create;
  try
    Result := TreeContainsName(Root, TargetName, Active);
  finally
    Active.Free;
  end;
end;

procedure WalkCommandTree(const Current: ICommand; const Context: string;
  const IsRoot, AllowEmptyRoot: Boolean; const Active: TStringList);
var
  Children: specialize TArray<ICommand>;
  Parameters: specialize TArray<ICommandParameter>;
  i: Integer;
begin
  if not Assigned(Current) then
    raise EArgumentNilException.CreateFmt('%s contains a nil command', [Context]);
  ValidateCommandName(Current.Name, Context + ' command', IsRoot and AllowEmptyRoot);
  if Active.IndexOf(Current.Name) >= 0 then
    raise EArgumentException.CreateFmt(
      '%s contains a command cycle at "%s"', [Context, Current.Name]);
  Active.Add(Current.Name);
  try
    Parameters := Current.Parameters;
    for i := 0 to Length(Parameters) - 1 do
      ValidateParameterDefinition(Parameters[i],
        Context + ' command "' + Current.Name + '"');
    Children := Current.SubCommands;
    for i := 0 to Length(Children) - 1 do
      WalkCommandTree(Children[i], Context, False, AllowEmptyRoot,
        Active);
  finally
    Active.Delete(Active.Count - 1);
  end;
end;

procedure ValidateCommandTree(const Command: ICommand; const Context: string;
  const AllowEmptyRoot: Boolean);
var
  Active: TStringList;
begin
  Active := TStringList.Create;
  try
    WalkCommandTree(Command, Context, True, AllowEmptyRoot, Active);
  finally
    Active.Free;
  end;
end;

end.
