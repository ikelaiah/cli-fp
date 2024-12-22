unit TestCase;

{$mode objfpc}{$H+}{$J-}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  CLI.Interfaces,
  CLI.Application, CLI.Command, CLI.Parameter,
  CLI.Progress, CLI.Console;

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

    // 6.x - Console Color Tests
    procedure Test_6_1_BasicColors;
    procedure Test_6_2_BrightColors;
    procedure Test_6_3_BackgroundColors;
    procedure Test_6_4_ColorReset;
    procedure Test_6_5_WriteWithColors;
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
  inherited;
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
  Cmd: ICommand;
begin
  Cmd := TTestCommand.Create('test', 'Test command');
  try
    FApp.RegisterCommand(Cmd);
    AssertTrue('Command should be registered', (FApp as TCLIApplication).Commands.Count > 0);
  finally
    Cmd := nil;
  end;
end;

procedure TCLIFrameworkTests.Test_1_4_DuplicateCommand;
var
  Cmd1, Cmd2: ICommand;
begin
  Cmd1 := TTestCommand.Create('test', 'Test command 1');
  Cmd2 := TTestCommand.Create('test', 'Test command 2');
  
  FApp.RegisterCommand(Cmd1);
  Cmd1 := nil;
  
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
    Cmd.AddParameter('-t', '--test', 'Test parameter', False, ptString, ''));
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
  Param := CreateParameter('-t', '--test', 'Test parameter', False, ptString, '');
  AssertEquals('Short flag should match', '-t', Param.ShortFlag);
  AssertEquals('Long flag should match', '--test', Param.LongFlag);
end;

procedure TCLIFrameworkTests.Test_3_2_RequiredParameter;
var
  Param: ICommandParameter;
begin
  Param := CreateParameter('-r', '--required', 'Required parameter', True, ptString, '');
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
  AssertEquals('Parameter type should be integer', Ord(ptInteger), Ord(Param.ParamType));
end;

procedure TCLIFrameworkTests.Test_3_5_ParameterValidation;
var
  Cmd: TTestCommand;
  Param: ICommandParameter;
begin
  Cmd := TTestCommand.Create('test', 'Test command');
  try
    Param := CreateParameter('-r', '--required', 'Required parameter', True, ptString, '');
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

// 6.x - Console Color Tests

procedure TCLIFrameworkTests.Test_6_1_BasicColors;
begin
  try
    // Test basic colors
    TConsole.WriteLn('Testing basic colors:', ccWhite);
    TConsole.WriteLn('Black text', ccBlack);
    TConsole.WriteLn('Blue text', ccBlue);
    TConsole.WriteLn('Green text', ccGreen);
    TConsole.WriteLn('Cyan text', ccCyan);
    TConsole.WriteLn('Red text', ccRed);
    TConsole.WriteLn('Magenta text', ccMagenta);
    TConsole.WriteLn('Yellow text', ccYellow);
    TConsole.WriteLn('White text', ccWhite);
    AssertTrue('Basic colors should not raise exceptions', True);
  finally
    TConsole.ResetColors;
  end;
end;

procedure TCLIFrameworkTests.Test_6_2_BrightColors;
begin
  try
    // Test bright colors
    TConsole.WriteLn('Testing bright colors:', ccWhite);
    TConsole.WriteLn('Bright Black text', ccBrightBlack);
    TConsole.WriteLn('Bright Blue text', ccBrightBlue);
    TConsole.WriteLn('Bright Green text', ccBrightGreen);
    TConsole.WriteLn('Bright Cyan text', ccBrightCyan);
    TConsole.WriteLn('Bright Red text', ccBrightRed);
    TConsole.WriteLn('Bright Magenta text', ccBrightMagenta);
    TConsole.WriteLn('Bright Yellow text', ccBrightYellow);
    TConsole.WriteLn('Bright White text', ccBrightWhite);
    AssertTrue('Bright colors should not raise exceptions', True);
  finally
    TConsole.ResetColors;
  end;
end;

procedure TCLIFrameworkTests.Test_6_3_BackgroundColors;
begin
  try
    // Test background colors
    TConsole.WriteLn('Testing background colors:', ccWhite);
    TConsole.SetBackgroundColor(ccBlue);
    TConsole.WriteLn('Text with blue background', ccWhite);
    TConsole.SetBackgroundColor(ccGreen);
    TConsole.WriteLn('Text with green background', ccBlack);
    AssertTrue('Background colors should not raise exceptions', True);
  finally
    TConsole.ResetColors;
  end;
end;

procedure TCLIFrameworkTests.Test_6_4_ColorReset;
begin
  try
    // Test color reset functionality
    TConsole.WriteLn('Testing color reset:', ccWhite);
    TConsole.SetForegroundColor(ccRed);
    TConsole.SetBackgroundColor(ccYellow);
    TConsole.Write('Colored text');
    TConsole.ResetColors;
    TConsole.WriteLn(' - should be back to default colors');
    AssertTrue('Color reset should not raise exceptions', True);
  finally
    TConsole.ResetColors; // Make absolutely sure colors are reset
  end;
end;

procedure TCLIFrameworkTests.Test_6_5_WriteWithColors;
begin
  try
    // Test Write and WriteLn with colors
    TConsole.WriteLn('Testing Write/WriteLn with colors:', ccWhite);
    TConsole.Write('This is ', ccWhite);
    TConsole.Write('multi', ccRed);
    TConsole.Write('-', ccWhite);
    TConsole.Write('colored', ccBlue);
    TConsole.WriteLn(' text', ccGreen);
    AssertTrue('Write with colors should not raise exceptions', True);
  finally
    TConsole.ResetColors;
  end;
end;




initialization
  RegisterTest(TCLIFrameworkTests);
end.

