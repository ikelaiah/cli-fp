unit CLI.Internal.Completion;

{$mode objfpc}{$H+}{$J-}

interface

uses
  Classes, CLI.Interfaces;

function CompleteCLI(const Tokens: array of string;
  const RootCommand: ICommand;
  const Commands: array of ICommand): TStringList;

implementation

uses
  SysUtils, StrUtils, CLI.Internal.ParameterValues;

const
  CD_NOFILE = 4;

type
  TCommandArray = specialize TArray<ICommand>;

  TCLICompletionEngine = class
  private
    FRootCommand: ICommand;
    FCommands: TCommandArray;
    function FindCommand(const Name: string): ICommand;
    procedure AddGlobalFlags(const Suggestions: TStrings;
      const Prefix: string; const IncludeExtended, Standalone: Boolean);
    procedure AddCommandFlags(const Suggestions: TStrings;
      const Command: ICommand; const Prefix: string);
    procedure AddParameterValues(const Suggestions: TStrings;
      const Param: ICommandParameter; var Directive: Integer);
    function ResolveCommand(const Tokens: array of string;
      out Command: ICommand; out ParameterIndex: Integer): Boolean;
    procedure CompleteFlag(const Tokens: array of string;
      const Command: ICommand; const Suggestions: TStrings;
      var Directive: Integer);
    procedure CompleteValueOrPosition(const Tokens: array of string;
      const Command: ICommand; const ParameterIndex: Integer;
      const Suggestions: TStrings; var Directive: Integer);
  public
    constructor Create(const RootCommand: ICommand;
      const Commands: array of ICommand);
    function Complete(const Tokens: array of string): TStringList;
  end;

constructor TCLICompletionEngine.Create(const RootCommand: ICommand;
  const Commands: array of ICommand);
var
  i: Integer;
begin
  inherited Create;
  FRootCommand := RootCommand;
  SetLength(FCommands, Length(Commands));
  for i := 0 to Length(Commands) - 1 do
    FCommands[i] := Commands[i];
end;

function TCLICompletionEngine.FindCommand(const Name: string): ICommand;
var
  Command: ICommand;
begin
  Result := nil;
  for Command in FCommands do
    if SameText(Command.Name, Name) then
      Exit(Command);
end;

procedure TCLICompletionEngine.AddGlobalFlags(const Suggestions: TStrings;
  const Prefix: string; const IncludeExtended, Standalone: Boolean);
begin
  if Standalone then
  begin
    if StartsStr(LowerCase(Prefix), '--help') then Suggestions.Add('--help');
    if StartsStr(LowerCase(Prefix), '--help-complete') then
      Suggestions.Add('--help-complete');
    if StartsStr(LowerCase(Prefix), '--version') then
      Suggestions.Add('--version');
    if StartsStr(LowerCase(Prefix), '--completion-file') then
      Suggestions.Add('--completion-file');
    if StartsStr(LowerCase(Prefix), '--completion-file-pwsh') then
      Suggestions.Add('--completion-file-pwsh');
    if StartsStr(LowerCase(Prefix), '-h') then Suggestions.Add('-h');
    if StartsStr(LowerCase(Prefix), '-v') then Suggestions.Add('-v');
    Exit;
  end;

  if StartsStr(LowerCase(Prefix), '--help') then Suggestions.Add('--help');
  if StartsStr(LowerCase(Prefix), '-h') then Suggestions.Add('-h');
  if StartsStr(LowerCase(Prefix), '--version') then Suggestions.Add('--version');
  if StartsStr(LowerCase(Prefix), '-v') then Suggestions.Add('-v');
  if IncludeExtended then
  begin
    if StartsStr(LowerCase(Prefix), '--help-complete') then
      Suggestions.Add('--help-complete');
    if StartsStr(LowerCase(Prefix), '--completion-file') then
      Suggestions.Add('--completion-file');
    if StartsStr(LowerCase(Prefix), '--completion-file-pwsh') then
      Suggestions.Add('--completion-file-pwsh');
  end;
end;

procedure TCLICompletionEngine.AddCommandFlags(const Suggestions: TStrings;
  const Command: ICommand; const Prefix: string);
var
  Param: ICommandParameter;
begin
  for Param in Command.Parameters do
  begin
    if StartsStr(LowerCase(Prefix), LowerCase(Param.LongFlag)) then
      Suggestions.Add(Param.LongFlag);
    if (Param.ShortFlag <> '') and
      StartsStr(LowerCase(Prefix), LowerCase(Param.ShortFlag)) then
      Suggestions.Add(Param.ShortFlag);
  end;
  AddGlobalFlags(Suggestions, Prefix, Command = FRootCommand, False);
end;

procedure TCLICompletionEngine.AddParameterValues(const Suggestions: TStrings;
  const Param: ICommandParameter; var Directive: Integer);
var
  Values: TStringList;
  i: Integer;
begin
  if not Assigned(Param) then
    Exit;
  if Param.ParamType = ptBoolean then
  begin
    Suggestions.Add('true');
    Suggestions.Add('false');
  end
  else if Param.ParamType = ptEnum then
  begin
    Values := TStringList.Create;
    try
      Values.Delimiter := '|';
      Values.DelimitedText := Param.AllowedValues;
      for i := 0 to Values.Count - 1 do
        Suggestions.Add(Values[i]);
    finally
      Values.Free;
    end;
  end
  else
    Exit;
  Directive := Directive or CD_NOFILE;
end;

function TCLICompletionEngine.ResolveCommand(const Tokens: array of string;
  out Command: ICommand; out ParameterIndex: Integer): Boolean;
var
  Candidate, SubCommand: ICommand;
begin
  if StartsStr('-', Tokens[0]) then
  begin
    Command := FRootCommand;
    ParameterIndex := 0;
  end
  else
  begin
    Command := FindCommand(Tokens[0]);
    ParameterIndex := 1;
  end;

  Result := Assigned(Command);
  if not Result then
    Exit;

  while (ParameterIndex < Length(Tokens)) and
    not StartsStr('-', Tokens[ParameterIndex]) do
  begin
    SubCommand := nil;
    for Candidate in Command.SubCommands do
      if SameText(Candidate.Name, Tokens[ParameterIndex]) then
      begin
        SubCommand := Candidate;
        Break;
      end;
    if not Assigned(SubCommand) then
      Break;
    Command := SubCommand;
    Inc(ParameterIndex);
  end;
end;

procedure TCLICompletionEngine.CompleteFlag(const Tokens: array of string;
  const Command: ICommand; const Suggestions: TStrings;
  var Directive: Integer);
var
  Current: string;
  Param: ICommandParameter;
begin
  Current := Tokens[High(Tokens)];
  Param := FindParameterByFlag(Command, Current);
  if Assigned(Param) and
    (Param.ParamType in [ptBoolean, ptEnum]) then
    AddParameterValues(Suggestions, Param, Directive)
  else
    AddCommandFlags(Suggestions, Command, Current);
end;

procedure TCLICompletionEngine.CompleteValueOrPosition(
  const Tokens: array of string; const Command: ICommand;
  const ParameterIndex: Integer; const Suggestions: TStrings;
  var Directive: Integer);
var
  i, PositionalCount, LastIndex: Integer;
  Param: ICommandParameter;
  SubCommand: ICommand;
begin
  LastIndex := High(Tokens);
  if (Length(Tokens) >= 2) and StartsStr('-', Tokens[LastIndex - 1]) and
    not StartsStr('-', Tokens[LastIndex]) then
  begin
    Param := FindParameterByFlag(Command, Tokens[LastIndex - 1]);
    AddParameterValues(Suggestions, Param, Directive);
    Exit;
  end;

  PositionalCount := 0;
  i := ParameterIndex;
  while i <= LastIndex - Ord(Tokens[LastIndex] <> '') do
  begin
    if not StartsStr('-', Tokens[i]) and (Tokens[i] <> '') then
      Inc(PositionalCount);
    if StartsStr('-', Tokens[i]) and (i + 1 <= LastIndex) and
      not StartsStr('-', Tokens[i + 1]) then
      Inc(i);
    Inc(i);
  end;

  if PositionalCount <> 0 then
    Exit;
  for SubCommand in Command.SubCommands do
    Suggestions.Add(SubCommand.Name);
  for Param in Command.Parameters do
  begin
    Suggestions.Add(Param.LongFlag);
    if Param.ShortFlag <> '' then
      Suggestions.Add(Param.ShortFlag);
  end;
  Suggestions.Add('--help');
  Suggestions.Add('-h');
end;

function TCLICompletionEngine.Complete(
  const Tokens: array of string): TStringList;
var
  Command: ICommand;
  ParameterIndex, Directive: Integer;
  Current: string;
begin
  Result := TStringList.Create;
  Result.Duplicates := dupIgnore;
  Result.Sorted := False;
  Directive := 0;

  if Length(Tokens) = 0 then
  begin
    for Command in FCommands do
      Result.Add(Command.Name);
    Exit;
  end;

  if StartsStr('-', Tokens[0]) and not Assigned(FRootCommand) then
  begin
    AddGlobalFlags(Result, Tokens[0], True, True);
    Result.Add(':' + IntToStr(Directive));
    Exit;
  end;

  if not ResolveCommand(Tokens, Command, ParameterIndex) then
  begin
    Current := Tokens[0];
    for Command in FCommands do
      if StartsStr(LowerCase(Current), LowerCase(Command.Name)) then
        Result.Add(Command.Name);
    Exit;
  end;

  Current := Tokens[High(Tokens)];
  if (Current <> '') and StartsStr('-', Current) then
    CompleteFlag(Tokens, Command, Result, Directive)
  else
    CompleteValueOrPosition(Tokens, Command, ParameterIndex, Result,
      Directive);
  Result.Add(':' + IntToStr(Directive));
end;

function CompleteCLI(const Tokens: array of string;
  const RootCommand: ICommand;
  const Commands: array of ICommand): TStringList;
var
  Engine: TCLICompletionEngine;
begin
  Engine := TCLICompletionEngine.Create(RootCommand, Commands);
  try
    Result := Engine.Complete(Tokens);
  finally
    Engine.Free;
  end;
end;

end.
