unit Test_Release_161;

{$mode objfpc}{$H+}{$J-}

interface

uses Classes, SysUtils, fpcunit, testregistry, CLI.Interfaces,
  CLI.Application, CLI.Command;

type
  TReleaseCommand = class(TBaseCommand)
  public
    Executions: Integer;
    function Execute: Integer; override;
    function ReadValue(const Flag: string; out Value: string): Boolean;
  end;

  TRelease161Tests = class(TTestCase)
  private
    procedure CheckVersion(const Scope: Integer);
    procedure CheckAlias(const Args: array of string);
  published
    procedure VersionRoot;
    procedure VersionNamed;
    procedure VersionNested;
    procedure ReservedRegistration;
    procedure VersionLookingValues;
    procedure AliasLongThenShort;
    procedure AliasShortThenLong;
    procedure AliasSameSpelling;
    procedure AliasCaseEquals;
    procedure AliasValidationAgrees;
    procedure AliasPasswordRedaction;
    procedure AliasEmptyWinner;
    procedure ValuePresenceCharacterization;
  end;

implementation

function ArgsOf(const Values: array of string): TStringArray;
var I: Integer;
begin
  Result := nil;
  SetLength(Result, Length(Values));
  for I := 0 to High(Values) do Result[I] := Values[I];
end;

function TReleaseCommand.Execute: Integer;
begin
  Inc(Executions);
  Result := 0;
end;

function TReleaseCommand.ReadValue(const Flag: string; out Value: string): Boolean;
begin
  Result := GetParameterValue(Flag, Value);
end;

procedure TRelease161Tests.CheckVersion(const Scope: Integer);
const Flags: array[0..3] of string = ('-v', '--version', '-V', '--VeRsIoN');
var App: TCLIApplication; Root, Named, Nested: TReleaseCommand;
  Captured, Candidates: TStringList; Flag: string; Args: TStringArray;
begin
  Root := TReleaseCommand.Create('', 'Root');
  Named := TReleaseCommand.Create('named', 'Named');
  Nested := TReleaseCommand.Create('nested', 'Nested');
  Root.AddStringParameter('-r', '--required', 'Required', True);
  Named.AddStringParameter('-r', '--required', 'Required', True);
  Nested.AddStringParameter('-r', '--required', 'Required', True);
  Named.AddSubCommand(Nested);
  App := TCLIApplication.Create('ReleaseFixture', '1.6.1', Root);
  Captured := TStringList.Create;
  try
    App.RegisterCommand(Named);
    for Flag in Flags do
    begin
      case Scope of
        0: Args := ArgsOf([Flag]);
        1: Args := ArgsOf(['named', Flag]);
        else Args := ArgsOf(['named', 'nested', Flag]);
      end;
      Captured.Clear;
      AssertEquals(Flag + ' succeeds without required parameters', 0,
        App.TestExecuteAndCapture(Args, Captured));
      AssertTrue('Application version printed',
        Pos('ReleaseFixture version 1.6.1', Captured.Text) > 0);
      case Scope of
        0: Args := ArgsOf(['--required=value', Flag]);
        1: Args := ArgsOf(['named', '--required=value', Flag]);
        else Args := ArgsOf(['named', 'nested', '--required=value', Flag]);
      end;
      Captured.Clear;
      AssertEquals('Version after an earlier option succeeds', 0,
        App.TestExecuteAndCapture(Args, Captured));
      AssertTrue('Version after earlier option prints application version',
        Pos('ReleaseFixture version 1.6.1', Captured.Text) > 0);
      AssertEquals('Root skipped', 0, Root.Executions);
      AssertEquals('Named skipped', 0, Named.Executions);
      AssertEquals('Nested skipped', 0, Nested.Executions);
    end;
    case Scope of
      0: Args := ArgsOf(['--help']);
      1: Args := ArgsOf(['named', '--help']);
      else Args := ArgsOf(['named', 'nested', '--help']);
    end;
    Captured.Clear;
    AssertEquals('Help succeeds', 0, App.TestExecuteAndCapture(Args, Captured));
    AssertTrue('Help advertises reserved short and long version aliases',
      Pos('-v, --version', Captured.Text) > 0);
    Args[High(Args)] := '-';
    Candidates := App.TestComplete(Args);
    try
      AssertTrue('Completion advertises short version', Candidates.IndexOf('-v') >= 0);
      AssertTrue('Completion advertises long version', Candidates.IndexOf('--version') >= 0);
    finally Candidates.Free; end;
  finally Captured.Free; App.Free; end;
end;

procedure TRelease161Tests.VersionRoot;
begin CheckVersion(0); end;
procedure TRelease161Tests.VersionNamed;
begin CheckVersion(1); end;
procedure TRelease161Tests.VersionNested;
begin CheckVersion(2); end;

procedure TRelease161Tests.ReservedRegistration;
const Flags: array[0..3] of string = ('-v', '-V', '--version', '--VeRsIoN');
var Cmd: TReleaseCommand; Flag, MessageText: string; Rejected: Boolean;
begin
  for Flag in Flags do
  begin
    Cmd := TReleaseCommand.Create('fixture', 'Fixture');
    try
      Rejected := False;
      MessageText := '';
      try
        if Copy(Flag, 1, 2) = '--' then Cmd.AddFlag('', Flag, 'Conflict')
        else Cmd.AddFlag(Flag, '--verbose', 'Conflict');
      except on E: Exception do begin Rejected := True; MessageText := E.Message; end; end;
      AssertTrue(Flag + ' must be reserved', Rejected);
      AssertTrue('Error explains version reservation',
        Pos('version', LowerCase(MessageText)) > 0);
    finally Cmd.Free; end;
  end;
end;

procedure TRelease161Tests.VersionLookingValues;
const Values: array[0..3] of string = ('-v', '--version', '-V', '--VERSION');
var App: TCLIApplication; Root, Named, Parent, Nested, Cmd: TReleaseCommand;
  Captured: TStringList; Expected, Actual: string; Scope: Integer;
  Args: TStringArray;
begin
  Root := TReleaseCommand.Create('', 'Root');
  Named := TReleaseCommand.Create('named', 'Named');
  Parent := TReleaseCommand.Create('parent', 'Parent');
  Nested := TReleaseCommand.Create('nested', 'Nested');
  Root.AddStringParameter('-n', '--name', 'Name', True);
  Named.AddStringParameter('-n', '--name', 'Name', True);
  Nested.AddStringParameter('-n', '--name', 'Name', True);
  Parent.AddSubCommand(Nested);
  App := TCLIApplication.Create('ReleaseFixture', '1.6.1', Root);
  Captured := TStringList.Create;
  try
    App.RegisterCommand(Named);
    App.RegisterCommand(Parent);
    for Scope := 0 to 2 do
    begin
      for Expected in Values do
      begin
        case Scope of
          0: begin Cmd := Root; Args := ArgsOf(['--name=' + Expected]); end;
          1: begin Cmd := Named; Args := ArgsOf(['named', '--name=' + Expected]); end;
          else begin Cmd := Nested; Args := ArgsOf(['parent', 'nested', '--name=' + Expected]); end;
        end;
        Captured.Clear;
        AssertEquals('Value execution scope ' + IntToStr(Scope), 0,
          App.TestExecuteAndCapture(Args, Captured));
        AssertTrue('Value available scope ' + IntToStr(Scope) + ' ' + Expected,
          Cmd.ReadValue('--name', Actual));
        AssertEquals('Value preserved scope ' + IntToStr(Scope), Expected, Actual);
        AssertEquals('Not a version request', 0, Pos('ReleaseFixture version', Captured.Text));
      end;
    end;
    AssertEquals('Root values execute', Length(Values), Root.Executions);
    AssertEquals('Named values execute', Length(Values), Named.Executions);
    AssertEquals('Nested values execute', Length(Values), Nested.Executions);
  finally Captured.Free; App.Free; end;
end;

procedure TRelease161Tests.CheckAlias(const Args: array of string);
var App: TCLIApplication; Cmd: TReleaseCommand; Value: string;
begin
  Cmd := TReleaseCommand.Create('greet', 'Greet');
  Cmd.AddStringParameter('-n', '--name', 'Name', True);
  App := TCLIApplication.Create('ReleaseFixture', '1.6.1');
  try
    App.RegisterCommand(Cmd);
    AssertEquals('Execute succeeds', 0, App.TestExecute(ArgsOf(Args)));
    AssertTrue(Cmd.ReadValue('--name', Value));
    AssertEquals('Long lookup returns last occurrence', 'Bob', Value);
    AssertTrue(Cmd.ReadValue('-N', Value));
    AssertEquals('Case-insensitive short lookup agrees', 'Bob', Value);
  finally App.Free; end;
end;

procedure TRelease161Tests.AliasLongThenShort;
begin CheckAlias(['greet', '--name', 'Alice', '-n', 'Bob']); end;
procedure TRelease161Tests.AliasShortThenLong;
begin CheckAlias(['greet', '-n', 'Alice', '--name', 'Bob']); end;
procedure TRelease161Tests.AliasSameSpelling;
begin
  CheckAlias(['greet', '--name', 'Alice', '--name', 'Bob']);
  CheckAlias(['greet', '-n', 'Alice', '-n', 'Bob']);
end;
procedure TRelease161Tests.AliasCaseEquals;
begin
  CheckAlias(['greet', '--NAME=Alice', '-N', 'Bob']);
  CheckAlias(['greet', '-N', 'Alice', '--NaMe=Bob']);
  CheckAlias(['greet', '--NAME=First', '-n', 'Alice', '--name=Bob']);
end;

procedure TRelease161Tests.AliasValidationAgrees;
var App: TCLIApplication; Cmd: TReleaseCommand; Value: string;
begin
  Cmd := TReleaseCommand.Create('measure', 'Measure');
  Cmd.AddIntegerParameter('-c', '--count', 'Count', True);
  App := TCLIApplication.Create('ReleaseFixture', '1.6.1');
  try
    App.RegisterCommand(Cmd);
    AssertEquals('Winning valid value overrides earlier invalid', 0,
      App.TestExecute(ArgsOf(['measure', '--count=invalid', '-C', '-3'])));
    AssertTrue(Cmd.ReadValue('--count', Value));
    AssertEquals('-3', Value);
    AssertEquals('Winning invalid value fails validation', 1,
      App.TestExecute(ArgsOf(['measure', '--count=3', '-c', 'invalid'])));
    AssertEquals('Failed validation skips execution', 1, Cmd.Executions);
    AssertEquals('Separated negative value remains supported', 0,
      App.TestExecute(ArgsOf(['measure', '-c', '1', '--count', '-4'])));
    AssertTrue(Cmd.ReadValue('-c', Value));
    AssertEquals('-4', Value);
  finally App.Free; end;
end;

procedure TRelease161Tests.AliasPasswordRedaction;
var App: TCLIApplication; Cmd: TReleaseCommand; Captured: TStringList; Value: string;
begin
  Cmd := TReleaseCommand.Create('login', 'Login');
  Cmd.AddPasswordParameter('-p', '--password', 'Password', True);
  App := TCLIApplication.Create('ReleaseFixture', '1.6.1');
  Captured := TStringList.Create;
  try
    App.RegisterCommand(Cmd);
    App.DebugMode := True;
    AssertEquals(0, App.TestExecuteAndCapture(ArgsOf(['login',
      '--PASSWORD=first-secret', '-P', 'last-secret']), Captured));
    AssertEquals('Earlier secret redacted', 0, Pos('first-secret', Captured.Text));
    AssertEquals('Winning secret redacted', 0, Pos('last-secret', Captured.Text));
    AssertTrue('Redaction visible', Pos('[REDACTED]', Captured.Text) > 0);
    AssertTrue(Cmd.ReadValue('--password', Value));
    AssertEquals('Last password retrieved', 'last-secret', Value);
  finally Captured.Free; App.Free; end;
end;

procedure TRelease161Tests.AliasEmptyWinner;
var App: TCLIApplication; Cmd: TReleaseCommand; Value: string;
begin
  Cmd := TReleaseCommand.Create('values', 'Values');
  Cmd.AddStringParameter('-n', '--name', 'Name');
  Cmd.AddStringParameter('-d', '--default', 'Default', False, 'fallback');
  App := TCLIApplication.Create('ReleaseFixture', '1.6.1');
  try
    App.RegisterCommand(Cmd);
    AssertEquals(0, App.TestExecute(ArgsOf(['values', '-n', 'earlier',
      '--NAME=', '-d', 'earlier', '--DEFAULT='])));
    AssertFalse('Winning empty value suppresses earlier alias', Cmd.ReadValue('-n', Value));
    AssertEquals('', Value);
    AssertTrue('Winning empty value uses registered default', Cmd.ReadValue('-d', Value));
    AssertEquals('fallback', Value);
    AssertEquals(0, App.TestExecute(ArgsOf(['values', '--name=earlier',
      '-N', '', '--default=earlier', '-D', ''])));
    AssertFalse('Winning empty short value suppresses earlier alias', Cmd.ReadValue('--name', Value));
    AssertEquals('', Value);
    AssertTrue('Winning empty short value uses default', Cmd.ReadValue('--default', Value));
    AssertEquals('fallback', Value);
  finally App.Free; end;
end;

procedure TRelease161Tests.ValuePresenceCharacterization;
var App: TCLIApplication; Cmd: TReleaseCommand; Value: string;
begin
  Cmd := TReleaseCommand.Create('values', 'Values');
  Cmd.AddBooleanParameter('-b', '--bool', 'Boolean', False, '');
  Cmd.AddFlag('-f', '--flag', 'Flag');
  Cmd.AddStringParameter('-n', '--name', 'Name');
  Cmd.AddStringParameter('-d', '--default', 'Default', False, 'fallback');
  App := TCLIApplication.Create('ReleaseFixture', '1.6.1');
  try
    App.RegisterCommand(Cmd);
    AssertEquals(0, App.TestExecute(ArgsOf(['values'])));
    Value := 'sentinel';
    AssertFalse('Omitted Boolean without default reports false', Cmd.ReadValue('--bool', Value));
    AssertEquals('False result can carry Boolean false', 'false', Value);
    AssertTrue('Omitted flag has default', Cmd.ReadValue('--flag', Value));
    AssertEquals('false', Value);
    AssertFalse('Omitted string without default', Cmd.ReadValue('--name', Value));
    AssertEquals('', Value);
    AssertTrue('Default exists without explicit option', Cmd.ReadValue('--default', Value));
    AssertEquals('fallback', Value);
    AssertEquals(0, App.TestExecute(ArgsOf(['values', '--name=', '--default=', '--bool', '--flag=false'])));
    AssertFalse('Empty string without default', Cmd.ReadValue('--name', Value));
    AssertEquals('', Value);
    AssertTrue('Empty string falls back to default', Cmd.ReadValue('--default', Value));
    AssertEquals('fallback', Value);
    AssertTrue('Bare Boolean accepted', Cmd.ReadValue('--bool', Value));
    AssertEquals('true', Value);
    AssertTrue('Flag accepts explicit false', Cmd.ReadValue('--flag', Value));
    AssertEquals('false', Value);
  finally App.Free; end;
end;

initialization
  RegisterTest(TRelease161Tests);
end.
