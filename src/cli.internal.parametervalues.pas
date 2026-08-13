unit CLI.Internal.ParameterValues;

{$mode objfpc}{$H+}{$J-}

interface

uses
  Classes, SysUtils, CLI.Interfaces;

function FindParameterByFlag(const Command: ICommand;
  const Flag: string): ICommandParameter;
function TryGetParameterValue(const Param: ICommandParameter;
  const ParsedParams: TStrings; out Value: string): Boolean;
function RedactParameterValue(const Command: ICommand; const Flag,
  Value: string): string;
function RedactArgument(const Command: ICommand;
  const Argument: string): string;

implementation

function FindParameterByFlag(const Command: ICommand;
  const Flag: string): ICommandParameter;
var
  Param: ICommandParameter;
begin
  Result := nil;
  if not Assigned(Command) then
    Exit;

  for Param in Command.Parameters do
    if SameText(Param.LongFlag, Flag) or SameText(Param.ShortFlag, Flag) then
      Exit(Param);
end;

function TryGetParameterValue(const Param: ICommandParameter;
  const ParsedParams: TStrings; out Value: string): Boolean;
var
  Index: Integer;
begin
  Result := False;
  Value := '';
  if not Assigned(Param) or not Assigned(ParsedParams) then
    Exit;

  Index := ParsedParams.IndexOfName(Param.LongFlag);
  if Index = -1 then
    Index := ParsedParams.IndexOfName(Param.ShortFlag);

  if Index <> -1 then
  begin
    Value := ParsedParams.ValueFromIndex[Index];
    if Param.ParamType = ptBoolean then
    begin
      if Value = '' then
        Value := 'true';
      Exit(True);
    end;
    if Value <> '' then
      Exit(True);
  end;

  if Param.DefaultValue <> '' then
  begin
    Value := Param.DefaultValue;
    Exit(True);
  end;

  if Param.ParamType = ptBoolean then
    Value := 'false';
end;

function RedactParameterValue(const Command: ICommand; const Flag,
  Value: string): string;
var
  Param: ICommandParameter;
begin
  Param := FindParameterByFlag(Command, Flag);
  if Assigned(Param) and (Param.ParamType = ptPassword) then
    Result := '[REDACTED]'
  else
    Result := Value;
end;

function RedactArgument(const Command: ICommand;
  const Argument: string): string;
var
  SeparatorPos: Integer;
  Flag: string;
  Param: ICommandParameter;
begin
  Result := Argument;
  SeparatorPos := Pos('=', Argument);
  if SeparatorPos = 0 then
    Exit;

  Flag := Copy(Argument, 1, SeparatorPos - 1);
  Param := FindParameterByFlag(Command, Flag);
  if Assigned(Param) and (Param.ParamType = ptPassword) then
    Result := Flag + '=[REDACTED]';
end;

end.
