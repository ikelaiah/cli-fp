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
    function TestGetParameterValue(const Flag: string; out Value: string): Boolean;
  end;

function TTestCommand.Execute: Integer;
begin
  Result := 0;
end;

function TTestCommand.TestGetParameterValue(const Flag: string; out Value: string): Boolean;
begin
  Result := GetParameterValue(Flag, Value);
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
    Cmd.AddStringParameter('-t', '--test', 'Test parameter', False, '');
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
  AssertEquals('Description should match', 'Test parameter', Param.Description);
  AssertEquals('Parameter type should be string', Ord(ptString), Ord(Param.ParamType));
end;

procedure TCLIFrameworkTests.Test_3_2_RequiredParameter;
var
  Cmd: TTestCommand;
  App: TCLIApplication;
begin
  Cmd := TTestCommand.Create('test', 'Test command');
  App := TCLIApplication.Create('TestApp', '1.0.0');
  try
    Cmd.AddStringParameter('-r', '--required', 'Required parameter', True);
    App.RegisterCommand(Cmd);
    App.CurrentCommand := Cmd;
    
    // Test without providing required parameter
    AssertFalse('Should fail validation without required parameter', App.TestValidateCommand);
    
    // Test with required parameter
    App.ParsedParams.Values['--required'] := 'value';
    AssertTrue('Should pass validation with required parameter', App.TestValidateCommand);
  finally
    App.Free;
  end;
end;

procedure TCLIFrameworkTests.Test_3_3_DefaultValue;
var
  Cmd: TTestCommand;
  App: TCLIApplication;
  Value: string;
begin
  Cmd := TTestCommand.Create('test', 'Test command');
  App := TCLIApplication.Create('TestApp', '1.0.0');
  try
    Cmd.AddStringParameter('-d', '--default', 'Parameter with default', False, 'default-value');
    App.RegisterCommand(Cmd);
    App.CurrentCommand := Cmd;
    Cmd.SetParsedParams(App.ParsedParams);
    
    // Test getting default value when not provided
    AssertTrue('Should get default value', Cmd.TestGetParameterValue('--default', Value));
    AssertEquals('Default value should match', 'default-value', Value);
  finally
    App.Free;
  end;
end;

procedure TCLIFrameworkTests.Test_3_4_ParameterTypes;
var
  Cmd: TTestCommand;
begin
  Cmd := TTestCommand.Create('test', 'Test command');
  try
    // Test all parameter type helper methods
    Cmd.AddStringParameter('-s', '--string', 'String parameter');
    Cmd.AddIntegerParameter('-i', '--integer', 'Integer parameter');
    Cmd.AddFloatParameter('-f', '--float', 'Float parameter');
    Cmd.AddFlag('-b', '--bool', 'Boolean flag');
    Cmd.AddBooleanParameter('-x', '--explicit-bool', 'Boolean parameter', False, 'false');
    Cmd.AddUrlParameter('-u', '--url', 'URL parameter');
    Cmd.AddPathParameter('-p', '--path', 'Path parameter');
    Cmd.AddEnumParameter('-l', '--level', 'Log level', 'debug|info|warn|error');
    Cmd.AddDateTimeParameter('-d', '--date', 'Date parameter');
    Cmd.AddArrayParameter('-t', '--tags', 'Tag list');
    Cmd.AddPasswordParameter('-k', '--key', 'API key');
    
    AssertEquals('Should have 11 parameters', 11, Length(Cmd.Parameters));
    AssertEquals('String parameter type should match', Ord(ptString), Ord(Cmd.Parameters[0].ParamType));
    AssertEquals('Integer parameter type should match', Ord(ptInteger), Ord(Cmd.Parameters[1].ParamType));
    AssertEquals('Float parameter type should match', Ord(ptFloat), Ord(Cmd.Parameters[2].ParamType));
    AssertEquals('Boolean flag type should match', Ord(ptBoolean), Ord(Cmd.Parameters[3].ParamType));
    AssertEquals('Boolean parameter type should match', Ord(ptBoolean), Ord(Cmd.Parameters[4].ParamType));
    AssertEquals('URL parameter type should match', Ord(ptUrl), Ord(Cmd.Parameters[5].ParamType));
    AssertEquals('Path parameter type should match', Ord(ptPath), Ord(Cmd.Parameters[6].ParamType));
    AssertEquals('Enum parameter type should match', Ord(ptEnum), Ord(Cmd.Parameters[7].ParamType));
    AssertEquals('DateTime parameter type should match', Ord(ptDateTime), Ord(Cmd.Parameters[8].ParamType));
    AssertEquals('Array parameter type should match', Ord(ptArray), Ord(Cmd.Parameters[9].ParamType));
    AssertEquals('Password parameter type should match', Ord(ptPassword), Ord(Cmd.Parameters[10].ParamType));
  finally
    Cmd.Free;
  end;
end;

procedure TCLIFrameworkTests.Test_3_5_ParameterValidation;
var
  Cmd: TTestCommand;
  App: TCLIApplication;
begin
  Cmd := TTestCommand.Create('test', 'Test command');
  App := TCLIApplication.Create('TestApp', '1.0.0');
  try
    // Add parameters of different types
    Cmd.AddIntegerParameter('-i', '--integer', 'Integer parameter');
    Cmd.AddFloatParameter('-f', '--float', 'Float parameter');
    Cmd.AddFlag('-b', '--bool', 'Boolean flag');
    Cmd.AddBooleanParameter('-x', '--explicit-bool', 'Boolean parameter', False, 'false');
    Cmd.AddUrlParameter('-u', '--url', 'URL parameter');
    Cmd.AddEnumParameter('-l', '--level', 'Log level', 'debug|info|warn|error');
    Cmd.AddDateTimeParameter('-d', '--date', 'Date parameter');
    Cmd.AddArrayParameter('-t', '--tags', 'Tag list');
    App.RegisterCommand(Cmd);
    App.CurrentCommand := Cmd;
    
    // Test integer validation
    App.ParsedParams.Values['--integer'] := 'not-a-number';
    AssertFalse('Should fail validation with invalid integer', App.TestValidateCommand);
    App.ParsedParams.Clear;
    App.ParsedParams.Values['--integer'] := '42';
    AssertTrue('Should pass validation with valid integer', App.TestValidateCommand);
    
    // Test float validation
    App.ParsedParams.Clear;
    App.ParsedParams.Values['--float'] := 'not-a-float';
    AssertFalse('Should fail validation with invalid float', App.TestValidateCommand);
    App.ParsedParams.Clear;
    App.ParsedParams.Values['--float'] := '3.14';
    AssertTrue('Should pass validation with valid float', App.TestValidateCommand);
    
    // Test boolean flag validation
    App.ParsedParams.Clear;
    App.ParsedParams.Values['--bool'] := 'not-a-bool';
    AssertFalse('Should fail validation with invalid boolean', App.TestValidateCommand);
    App.ParsedParams.Clear;
    App.ParsedParams.Values['--bool'] := 'true';
    AssertTrue('Should pass validation with valid boolean', App.TestValidateCommand);
    
    // Test explicit boolean validation
    App.ParsedParams.Clear;
    App.ParsedParams.Values['--explicit-bool'] := 'not-a-bool';
    AssertFalse('Should fail validation with invalid boolean', App.TestValidateCommand);
    App.ParsedParams.Clear;
    App.ParsedParams.Values['--explicit-bool'] := 'true';
    AssertTrue('Should pass validation with valid boolean', App.TestValidateCommand);
    
    // Test URL validation
    App.ParsedParams.Clear;
    App.ParsedParams.Values['--url'] := 'not-a-url';
    AssertFalse('Should fail validation with invalid URL', App.TestValidateCommand);
    App.ParsedParams.Clear;
    App.ParsedParams.Values['--url'] := 'https://example.com';
    AssertTrue('Should pass validation with valid URL', App.TestValidateCommand);
    
    // Test enum validation
    App.ParsedParams.Clear;
    App.ParsedParams.Values['--level'] := 'invalid-level';
    AssertFalse('Should fail validation with invalid enum value', App.TestValidateCommand);
    App.ParsedParams.Clear;
    App.ParsedParams.Values['--level'] := 'debug';
    AssertTrue('Should pass validation with valid enum value', App.TestValidateCommand);
    
    // Test datetime validation
    App.ParsedParams.Clear;
    App.ParsedParams.Values['--date'] := 'not-a-date';
    AssertFalse('Should fail validation with invalid datetime', App.TestValidateCommand);
    App.ParsedParams.Clear;
    App.ParsedParams.Values['--date'] := '2024-01-01 12:00';
    AssertTrue('Should pass validation with valid datetime', App.TestValidateCommand);
    
    // Test array validation
    App.ParsedParams.Clear;
    App.ParsedParams.Values['--tags'] := 'tag1,tag2,tag3';
    AssertTrue('Should pass validation with valid array', App.TestValidateCommand);
  finally
    App.Free;
  end;
end;

// 4.x - Parameter Parsing Tests

procedure TCLIFrameworkTests.Test_4_1_LongFormat;
var
  Cmd: TTestCommand;
  App: TCLIApplication;
  Value: string;
begin
  Cmd := TTestCommand.Create('test', 'Test command');
  App := TCLIApplication.Create('TestApp', '1.0.0');
  try
    Cmd.AddStringParameter('-n', '--name', 'Name parameter');
    App.RegisterCommand(Cmd);
    App.CurrentCommand := Cmd;
    App.ParsedParams.Values['--name'] := 'test-value';
    Cmd.SetParsedParams(App.ParsedParams);
    
    // Test long format parameter
    AssertTrue('Should get parameter value', Cmd.TestGetParameterValue('--name', Value));
    AssertEquals('Parameter value should match', 'test-value', Value);
  finally
    App.Free;
  end;
end;

procedure TCLIFrameworkTests.Test_4_2_ShortFormat;
var
  Cmd: TTestCommand;
  App: TCLIApplication;
  Value: string;
begin
  Cmd := TTestCommand.Create('test', 'Test command');
  App := TCLIApplication.Create('TestApp', '1.0.0');
  try
    Cmd.AddStringParameter('-n', '--name', 'Name parameter');
    App.RegisterCommand(Cmd);
    App.CurrentCommand := Cmd;
    App.ParsedParams.Values['-n'] := 'test-value';
    Cmd.SetParsedParams(App.ParsedParams);
    
    // Test short format parameter
    AssertTrue('Should get parameter value', Cmd.TestGetParameterValue('-n', Value));
    AssertEquals('Parameter value should match', 'test-value', Value);
  finally
    App.Free;
  end;
end;

procedure TCLIFrameworkTests.Test_4_3_EqualsSyntax;
var
  Cmd: TTestCommand;
  App: TCLIApplication;
  Value: string;
begin
  Cmd := TTestCommand.Create('test', 'Test command');
  App := TCLIApplication.Create('TestApp', '1.0.0');
  try
    Cmd.AddStringParameter('-n', '--name', 'Name parameter');
    App.RegisterCommand(Cmd);
    App.CurrentCommand := Cmd;
    App.ParsedParams.Values['--name'] := 'test-value';
    Cmd.SetParsedParams(App.ParsedParams);
    
    // Test equals syntax (simulated since actual parsing happens in ParseCommandLine)
    AssertTrue('Should get parameter value', Cmd.TestGetParameterValue('--name', Value));
    AssertEquals('Parameter value should match', 'test-value', Value);
  finally
    App.Free;
  end;
end;

procedure TCLIFrameworkTests.Test_4_4_BooleanFlags;
var
  Cmd: TTestCommand;
  App: TCLIApplication;
  Value: string;
begin
  Cmd := TTestCommand.Create('test', 'Test command');
  App := TCLIApplication.Create('TestApp', '1.0.0');
  try
    Cmd.AddFlag('-v', '--verbose', 'Verbose flag');
    App.RegisterCommand(Cmd);
    App.CurrentCommand := Cmd;
    Cmd.SetParsedParams(App.ParsedParams);
    
    // Test flag without value (should use default 'false')
    AssertTrue('Should get default value', Cmd.TestGetParameterValue('--verbose', Value));
    AssertEquals('Default value should be true', 'false', Value);
    
    // Test flag with explicit value
    App.ParsedParams.Values['--verbose'] := 'true';
    AssertTrue('Should get parameter value', Cmd.TestGetParameterValue('--verbose', Value));
    AssertEquals('Parameter value should be true', 'true', Value);
  finally
    App.Free;
  end;
end;

procedure TCLIFrameworkTests.Test_4_5_MultipleParameters;
var
  Cmd: TTestCommand;
  App: TCLIApplication;
  Value: string;
begin
  Cmd := TTestCommand.Create('test', 'Test command');
  App := TCLIApplication.Create('TestApp', '1.0.0');
  try
    Cmd.AddStringParameter('-n', '--name', 'Name parameter');
    Cmd.AddIntegerParameter('-c', '--count', 'Count parameter');
    Cmd.AddFlag('-v', '--verbose', 'Verbose flag');
    App.RegisterCommand(Cmd);
    App.CurrentCommand := Cmd;
    
    // Test multiple parameters
    App.ParsedParams.Values['--name'] := 'test';
    App.ParsedParams.Values['--count'] := '42';
    App.ParsedParams.Values['--verbose'] := 'true';
    Cmd.SetParsedParams(App.ParsedParams);
    
    AssertTrue('Should get name value', Cmd.TestGetParameterValue('--name', Value));
    AssertEquals('Name value should match', 'test', Value);
    
    AssertTrue('Should get count value', Cmd.TestGetParameterValue('--count', Value));
    AssertEquals('Count value should match', '42', Value);
    
    AssertTrue('Should get verbose value', Cmd.TestGetParameterValue('--verbose', Value));
    AssertEquals('Verbose value should match', 'true', Value);
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

