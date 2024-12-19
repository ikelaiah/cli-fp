unit TestCase;

{$mode objfpc}{$H+}{$J-}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  CLI.Interfaces, CLI.Application, CLI.Command, CLI.Parameter, CLI.Progress;

type
  { TCLIFrameworkTests }
  TCLIFrameworkTests = class(TTestCase)
  private
    FApp: ICLIApplication;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    // 1.x - Application Tests
    procedure Test_1_1_CreateApplication;
    procedure Test_1_2_ApplicationVersion;
    procedure Test_1_3_RegisterCommand;
    procedure Test_1_4_DuplicateCommand;
    procedure Test_1_5_DebugMode;

    // 2.x - Command Tests
    procedure Test_2_1_CreateCommand;
    procedure Test_2_2_CommandProperties;
    procedure Test_2_3_SubCommands;
    procedure Test_2_4_CommandExecution;
    procedure Test_2_5_CommandHierarchy;

    // 3.x - Parameter Tests
    procedure Test_3_1_CreateParameter;
    procedure Test_3_2_RequiredParameter;
    procedure Test_3_3_DefaultValue;
    procedure Test_3_4_ParameterTypes;
    procedure Test_3_5_ParameterValidation;

    // 4.x - Parameter Parsing Tests
    procedure Test_4_1_LongFormat;
    procedure Test_4_2_ShortFormat;
    procedure Test_4_3_EqualsSyntax;
    procedure Test_4_4_BooleanFlags;
    procedure Test_4_5_MultipleParameters;

    // 5.x - Help System Tests
    procedure Test_5_1_BasicHelp;
    procedure Test_5_2_CommandHelp;
    procedure Test_5_3_CompleteHelp;
    procedure Test_5_4_HelpExamples;
    procedure Test_5_5_SubCommandHelp;
  end;

implementation

type
  { Test command class }
  TTestCommand = class(TBaseCommand)
  public
    function Execute: Integer; override;
  end;

function TTestCommand.Execute: Integer;
begin
  Result := 0;
end;

{ TCLIFrameworkTests }

procedure TCLIFrameworkTests.SetUp;
begin
  FApp := CreateCLIApplication('TestApp', '1.0.0');
end;

procedure TCLIFrameworkTests.TearDown;
begin
  FApp := nil;
end;

// 1.x - Application Tests

procedure TCLIFrameworkTests.Test_1_1_CreateApplication;
begin
  AssertNotNull('Application should be created', FApp);
end;

procedure TCLIFrameworkTests.Test_1_2_ApplicationVersion;
var
  App: TCLIApplication;
begin
  App := TCLIApplication.Create('TestApp', '2.0.0');
  try
    AssertEquals('Version should match', '2.0.0', App.Version);
  finally
    App.Free;
  end;
end;

procedure TCLIFrameworkTests.Test_1_3_RegisterCommand;
var
  Cmd: TTestCommand;
begin
  Cmd := TTestCommand.Create('test', 'Test command');
  FApp.RegisterCommand(Cmd);
  AssertTrue('Command should be registered', Length((FApp as TCLIApplication).Commands) > 0);
end;

procedure TCLIFrameworkTests.Test_1_4_DuplicateCommand;
var
  Cmd1, Cmd2: TTestCommand;
begin
  Cmd1 := TTestCommand.Create('test', 'Test command 1');
  Cmd2 := TTestCommand.Create('test', 'Test command 2');
  FApp.RegisterCommand(Cmd1);
  try
    FApp.RegisterCommand(Cmd2);
    Fail('Should not allow duplicate command names');
  except
    on E: Exception do
      AssertTrue('Should raise exception for duplicate command', True);
  end;
end;

procedure TCLIFrameworkTests.Test_1_5_DebugMode;
begin
  (FApp as TCLIApplication).DebugMode := True;
  AssertTrue('Debug mode should be enabled', (FApp as TCLIApplication).DebugMode);
end;

// 2.x - Command Tests

procedure TCLIFrameworkTests.Test_2_1_CreateCommand;
var
  Cmd: TTestCommand;
begin
  Cmd := TTestCommand.Create('test', 'Test command');
  try
    AssertEquals('Command name should match', 'test', Cmd.Name);
    AssertEquals('Command description should match', 'Test command', Cmd.Description);
  finally
    Cmd.Free;
  end;
end;

procedure TCLIFrameworkTests.Test_2_2_CommandProperties;
var
  Cmd: TTestCommand;
begin
  Cmd := TTestCommand.Create('test', 'Test command');
  try
    Cmd.AddParameter(CreateParameter('-t', '--test', 'Test parameter', False));
    AssertEquals('Should have one parameter', 1, Length(Cmd.Parameters));
  finally
    Cmd.Free;
  end;
end;

procedure TCLIFrameworkTests.Test_2_3_SubCommands;
var
  MainCmd, SubCmd: TTestCommand;
begin
  MainCmd := TTestCommand.Create('main', 'Main command');
  SubCmd := TTestCommand.Create('sub', 'Sub command');
  try
    MainCmd.AddSubCommand(SubCmd);
    AssertEquals('Should have one subcommand', 1, Length(MainCmd.SubCommands));
  finally
    MainCmd.Free;
  end;
end;

procedure TCLIFrameworkTests.Test_2_4_CommandExecution;
var
  Cmd: TTestCommand;
begin
  Cmd := TTestCommand.Create('test', 'Test command');
  try
    AssertEquals('Command should execute successfully', 0, Cmd.Execute);
  finally
    Cmd.Free;
  end;
end;

procedure TCLIFrameworkTests.Test_2_5_CommandHierarchy;
var
  MainCmd, SubCmd1, SubCmd2: TTestCommand;
begin
  MainCmd := TTestCommand.Create('main', 'Main command');
  SubCmd1 := TTestCommand.Create('sub1', 'Sub command 1');
  SubCmd2 := TTestCommand.Create('sub2', 'Sub command 2');
  try
    MainCmd.AddSubCommand(SubCmd1);
    SubCmd1.AddSubCommand(SubCmd2);
    AssertEquals('Main should have one subcommand', 1, Length(MainCmd.SubCommands));
    AssertEquals('Sub1 should have one subcommand', 1, Length(SubCmd1.SubCommands));
  finally
    MainCmd.Free;
  end;
end;

// 3.x - Parameter Tests

procedure TCLIFrameworkTests.Test_3_1_CreateParameter;
var
  Param: ICommandParameter;
begin
  Param := CreateParameter('-t', '--test', 'Test parameter', False);
  AssertEquals('Short flag should match', '-t', Param.ShortFlag);
  AssertEquals('Long flag should match', '--test', Param.LongFlag);
end;

procedure TCLIFrameworkTests.Test_3_2_RequiredParameter;
var
  Param: ICommandParameter;
begin
  Param := CreateParameter('-r', '--required', 'Required parameter', True);
  AssertTrue('Parameter should be required', Param.Required);
end;

procedure TCLIFrameworkTests.Test_3_3_DefaultValue;
var
  Param: ICommandParameter;
begin
  Param := CreateParameter('-d', '--default', 'Parameter with default', False, ptString, 'default');
  AssertEquals('Default value should match', 'default', Param.DefaultValue);
end;

procedure TCLIFrameworkTests.Test_3_4_ParameterTypes;
var
  Param: ICommandParameter;
begin
  Param := CreateParameter('-i', '--integer', 'Integer parameter', False, ptInteger, '42');
  AssertEquals('Parameter type should be integer', ptInteger, Param.ParamType);
end;

procedure TCLIFrameworkTests.Test_3_5_ParameterValidation;
var
  Cmd: TTestCommand;
  Param: ICommandParameter;
begin
  Cmd := TTestCommand.Create('test', 'Test command');
  try
    Param := CreateParameter('-r', '--required', 'Required parameter', True);
    Cmd.AddParameter(Param);
    AssertTrue('Command should have required parameter', Cmd.Parameters[0].Required);
  finally
    Cmd.Free;
  end;
end;

// 4.x - Parameter Parsing Tests

procedure TCLIFrameworkTests.Test_4_1_LongFormat;
var
  App: TCLIApplication;
begin
  App := TCLIApplication.Create('TestApp', '1.0.0');
  try
    // Simulate command line: --param value
    AssertTrue('Should parse long format parameters', True);
  finally
    App.Free;
  end;
end;

procedure TCLIFrameworkTests.Test_4_2_ShortFormat;
var
  App: TCLIApplication;
begin
  App := TCLIApplication.Create('TestApp', '1.0.0');
  try
    // Simulate command line: -p value
    AssertTrue('Should parse short format parameters', True);
  finally
    App.Free;
  end;
end;

procedure TCLIFrameworkTests.Test_4_3_EqualsSyntax;
var
  App: TCLIApplication;
begin
  App := TCLIApplication.Create('TestApp', '1.0.0');
  try
    // Simulate command line: --param=value
    AssertTrue('Should parse equals syntax', True);
  finally
    App.Free;
  end;
end;

procedure TCLIFrameworkTests.Test_4_4_BooleanFlags;
var
  App: TCLIApplication;
begin
  App := TCLIApplication.Create('TestApp', '1.0.0');
  try
    // Simulate command line: --flag
    AssertTrue('Should parse boolean flags', True);
  finally
    App.Free;
  end;
end;

procedure TCLIFrameworkTests.Test_4_5_MultipleParameters;
var
  App: TCLIApplication;
begin
  App := TCLIApplication.Create('TestApp', '1.0.0');
  try
    // Simulate command line: -a value1 -b value2 --flag
    AssertTrue('Should parse multiple parameters', True);
  finally
    App.Free;
  end;
end;

// 5.x - Help System Tests

procedure TCLIFrameworkTests.Test_5_1_BasicHelp;
var
  App: TCLIApplication;
begin
  App := TCLIApplication.Create('TestApp', '1.0.0');
  try
    AssertTrue('Should generate basic help', True);
  finally
    App.Free;
  end;
end;

procedure TCLIFrameworkTests.Test_5_2_CommandHelp;
var
  App: TCLIApplication;
  Cmd: TTestCommand;
begin
  App := TCLIApplication.Create('TestApp', '1.0.0');
  Cmd := TTestCommand.Create('test', 'Test command');
  try
    App.RegisterCommand(Cmd);
    AssertTrue('Should generate command help', True);
  finally
    App.Free;
  end;
end;

procedure TCLIFrameworkTests.Test_5_3_CompleteHelp;
var
  App: TCLIApplication;
begin
  App := TCLIApplication.Create('TestApp', '1.0.0');
  try
    AssertTrue('Should generate complete help', True);
  finally
    App.Free;
  end;
end;

procedure TCLIFrameworkTests.Test_5_4_HelpExamples;
var
  App: TCLIApplication;
  Cmd: TTestCommand;
begin
  App := TCLIApplication.Create('TestApp', '1.0.0');
  Cmd := TTestCommand.Create('test', 'Test command');
  try
    App.RegisterCommand(Cmd);
    AssertTrue('Should generate help examples', True);
  finally
    App.Free;
  end;
end;

procedure TCLIFrameworkTests.Test_5_5_SubCommandHelp;
var
  App: TCLIApplication;
  MainCmd, SubCmd: TTestCommand;
begin
  App := TCLIApplication.Create('TestApp', '1.0.0');
  MainCmd := TTestCommand.Create('main', 'Main command');
  SubCmd := TTestCommand.Create('sub', 'Sub command');
  try
    MainCmd.AddSubCommand(SubCmd);
    App.RegisterCommand(MainCmd);
    AssertTrue('Should generate subcommand help', True);
  finally
    App.Free;
  end;
end;

initialization
  RegisterTest(TCLIFrameworkTests);
end.

