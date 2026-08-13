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
    procedure Test_2_6_InterfaceOnlyCommandExecution;

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
    procedure Test_4_6_NegativeNumericValues;
    procedure Test_4_7_UnknownOptionStillFails;

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

    // 7.x - Root Command Tests
    procedure Test_7_1_CreateApplicationWithRootCommand;
    procedure Test_7_2_ExecuteRootCommandWithoutArguments;
    procedure Test_7_3_ExecuteRootCommandWithParameters;
    procedure Test_7_4_NamedCommandTakesPrecedence;
    procedure Test_7_5_GlobalHelpDoesNotExecuteRoot;
    procedure Test_7_6_InvalidRootParameterDoesNotExecute;
    procedure Test_7_7_CompleteRootParameters;
    procedure Test_7_8_NoRootPreservesEmptyArgumentBehavior;
  end;

implementation

type
  { Test command class }
  TTestCommand = class(TBaseCommand)
  public
    function Execute: Integer; override;
    function TestGetParameterValue(const Flag: string; out Value: string): Boolean;
  end;

  TRecordingCommand = class(TBaseCommand)
  private
    FExecuteCount: Integer;
    FLastName: string;
  public
    function Execute: Integer; override;
    property ExecuteCount: Integer read FExecuteCount;
    property LastName: string read FLastName;
  end;

  { ICommand implementation that deliberately does not inherit TBaseCommand. }
  TInterfaceOnlyCommand = class(TInterfacedObject, ICommand)
  private
    FExecuteCount: Integer;
  public
    function GetName: string;
    function GetDescription: string;
    function GetParameters: specialize TArray<ICommandParameter>;
    function GetSubCommands: specialize TArray<ICommand>;
    function Execute: Integer;
    property ExecuteCount: Integer read FExecuteCount;
  end;

function TTestCommand.Execute: Integer;
begin
  Result := 0;
end;

function TTestCommand.TestGetParameterValue(const Flag: string; out Value: string): Boolean;
begin
  Result := GetParameterValue(Flag, Value);
end;

function TRecordingCommand.Execute: Integer;
begin
  Inc(FExecuteCount);
  if not GetParameterValue('--name', FLastName) then
    FLastName := '';
  Result := 0;
end;

function TInterfaceOnlyCommand.GetName: string;
begin
  Result := 'standalone';
end;

function TInterfaceOnlyCommand.GetDescription: string;
begin
  Result := 'Interface-only command';
end;

function TInterfaceOnlyCommand.GetParameters: specialize TArray<ICommandParameter>;
begin
  Result := nil;
end;

function TInterfaceOnlyCommand.GetSubCommands: specialize TArray<ICommand>;
begin
  Result := nil;
end;

function TInterfaceOnlyCommand.Execute: Integer;
begin
  Inc(FExecuteCount);
  Result := 23;
end;

function MakeArgs(const Values: array of string): TStringArray;
var
  i: Integer;
begin
  Result := nil;
  SetLength(Result, Length(Values));
  for i := 0 to Length(Values) - 1 do
    Result[i] := Values[i];
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

procedure TCLIFrameworkTests.Test_2_6_InterfaceOnlyCommandExecution;
var
  App: TCLIApplication;
  Cmd: TInterfaceOnlyCommand;
  Command: ICommand;
begin
  App := TCLIApplication.Create('TestApp', '1.3.2');
  Cmd := TInterfaceOnlyCommand.Create;
  Command := Cmd;
  try
    App.RegisterCommand(Command);
    AssertEquals('Interface-only command exit code should be returned', 23,
      App.TestExecute(MakeArgs(['standalone'])));
    AssertEquals('Interface-only command should execute once', 1,
      Cmd.ExecuteCount);
  finally
    App.Free;
    Command := nil;
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

procedure TCLIFrameworkTests.Test_4_6_NegativeNumericValues;
var
  Cmd: TRecordingCommand;
  App: TCLIApplication;
begin
  Cmd := TRecordingCommand.Create('measure', 'Measure a signed value');
  App := TCLIApplication.Create('TestApp', '1.3.3');
  try
    Cmd.AddIntegerParameter('-c', '--count', 'Signed count', True);
    Cmd.AddFloatParameter('-r', '--rate', 'Signed rate', True);
    App.RegisterCommand(Cmd);

    AssertEquals('Separated negative integer and float values should succeed', 0,
      App.TestExecute(MakeArgs(['measure', '--count', '-1', '--rate', '-2.5'])));
    AssertEquals('Separated negative integer should be retained', '-1',
      App.ParsedParams.Values['--count']);
    AssertEquals('Separated negative float should be retained', '-2.5',
      App.ParsedParams.Values['--rate']);

    AssertEquals('Equals-form negative integer and float values should succeed', 0,
      App.TestExecute(MakeArgs(['measure', '--count=-3', '--rate=-4.75'])));
    AssertEquals('Equals-form negative integer should be retained', '-3',
      App.ParsedParams.Values['--count']);
    AssertEquals('Equals-form negative float should be retained', '-4.75',
      App.ParsedParams.Values['--rate']);
  finally
    App.Free;
  end;
end;

procedure TCLIFrameworkTests.Test_4_7_UnknownOptionStillFails;
var
  Cmd: TRecordingCommand;
  App: TCLIApplication;
begin
  Cmd := TRecordingCommand.Create('measure', 'Measure a signed value');
  App := TCLIApplication.Create('TestApp', '1.3.3');
  try
    Cmd.AddIntegerParameter('-c', '--count', 'Signed count', True);
    App.RegisterCommand(Cmd);
    AssertEquals('An unknown option must remain an error', 1,
      App.TestExecute(MakeArgs(['measure', '--count', '-1', '--unknown'])));
    AssertEquals('Unknown options must prevent command execution', 0,
      Cmd.ExecuteCount);
  finally
    App.Free;
  end;
end;

// 5.x - Help System Tests

procedure TCLIFrameworkTests.Test_5_1_BasicHelp;
var
  App: TCLIApplication;
  Root, Deploy: TTestCommand;
  Output: TStringList;
begin
  Root := TTestCommand.Create('', 'Run the default action');
  Root.AddStringParameter('-c', '--config', 'Configuration file', True);
  Root.AddStringParameter('-f', '--format', 'Output format', False, 'json');
  Deploy := TTestCommand.Create('deploy', 'Deploy the current release');
  App := TCLIApplication.Create('TestApp', '1.0.0', Root);
  Output := TStringList.Create;
  try
    App.RegisterCommand(Deploy);
    AssertEquals('General help should succeed', 0,
      App.TestExecuteAndCapture(MakeArgs(['--help']), Output));
    AssertTrue('General help should include the application version',
      Pos('TestApp version 1.0.0', Output.Text) > 0);
    AssertTrue('General help should include usage',
      Pos('Usage:', Output.Text) > 0);
    AssertTrue('General help should include the root description',
      Pos('Run the default action', Output.Text) > 0);
    AssertTrue('General help should include the required option',
      Pos('--config', Output.Text) > 0);
    AssertTrue('General help should label required options',
      Pos('(required)', Output.Text) > 0);
    AssertTrue('General help should include option defaults',
      Pos('Default: json', Output.Text) > 0);
    AssertTrue('General help should list command descriptions',
      Pos('Deploy the current release', Output.Text) > 0);
  finally
    Output.Free;
    App.Free;
  end;
end;

procedure TCLIFrameworkTests.Test_5_2_CommandHelp;
var
  App: TCLIApplication;
  Cmd: TTestCommand;
  Output: TStringList;
begin
  App := TCLIApplication.Create('TestApp', '1.0.0');
  Cmd := TTestCommand.Create('test', 'Test command');
  Output := TStringList.Create;
  try
    Cmd.AddIntegerParameter('-r', '--retries', 'Retry count', False, '3');
    App.RegisterCommand(Cmd);
    AssertEquals('Command help should succeed', 0,
      App.TestExecuteAndCapture(MakeArgs(['test', '--help']), Output));
    AssertTrue('Command help should include command usage',
      Pos('test [options]', Output.Text) > 0);
    AssertTrue('Command help should include the command description',
      Pos('Test command', Output.Text) > 0);
    AssertTrue('Command help should include option descriptions',
      Pos('Retry count', Output.Text) > 0);
    AssertTrue('Command help should include option defaults',
      Pos('Default: 3', Output.Text) > 0);
  finally
    Output.Free;
    App.Free;
  end;
end;

procedure TCLIFrameworkTests.Test_5_3_CompleteHelp;
var
  App: TCLIApplication;
  Cmd: TTestCommand;
  Output: TStringList;
begin
  App := TCLIApplication.Create('TestApp', '1.0.0');
  Cmd := TTestCommand.Create('report', 'Generate a report');
  Output := TStringList.Create;
  try
    Cmd.AddStringParameter('-o', '--output', 'Output file', True);
    App.RegisterCommand(Cmd);
    AssertEquals('Complete help should succeed', 0,
      App.TestExecuteAndCapture(MakeArgs(['--help-complete']), Output));
    AssertTrue('Complete help should include its heading',
      Pos('DESCRIPTION', Output.Text) > 0);
    AssertTrue('Complete help should include global options',
      Pos('GLOBAL OPTIONS', Output.Text) > 0);
    AssertTrue('Complete help should include registered commands',
      Pos('report - Generate a report', Output.Text) > 0);
    AssertTrue('Complete help should include required options',
      Pos('--output', Output.Text) > 0);
  finally
    Output.Free;
    App.Free;
  end;
end;

procedure TCLIFrameworkTests.Test_5_4_HelpExamples;
var
  App: TCLIApplication;
  Cmd: TTestCommand;
  Output: TStringList;
begin
  App := TCLIApplication.Create('TestApp', '1.0.0');
  Cmd := TTestCommand.Create('test', 'Test command');
  Output := TStringList.Create;
  try
    Cmd.AddStringParameter('-n', '--name', 'Name to greet', False, 'World');
    App.RegisterCommand(Cmd);
    AssertEquals('Command help should succeed', 0,
      App.TestExecuteAndCapture(MakeArgs(['test', '--help']), Output));
    AssertTrue('Command help should include option flags',
      Pos('--name', Output.Text) > 0);
    AssertTrue('Command help should include option descriptions',
      Pos('Name to greet', Output.Text) > 0);
    AssertTrue('Command help should include the documented default',
      Pos('Default: World', Output.Text) > 0);
  finally
    Output.Free;
    App.Free;
  end;
end;

procedure TCLIFrameworkTests.Test_5_5_SubCommandHelp;
var
  App: TCLIApplication;
  MainCmd, SubCmd: TTestCommand;
  Output: TStringList;
begin
  App := TCLIApplication.Create('TestApp', '1.0.0');
  MainCmd := TTestCommand.Create('main', 'Main command');
  SubCmd := TTestCommand.Create('sub', 'Sub command');
  Output := TStringList.Create;
  try
    MainCmd.AddSubCommand(SubCmd);
    App.RegisterCommand(MainCmd);
    AssertEquals('Parent command help should succeed', 0,
      App.TestExecuteAndCapture(MakeArgs(['main', '--help']), Output));
    AssertTrue('Parent help should include a subcommand section',
      Pos('Commands:', Output.Text) > 0);
    AssertTrue('Parent help should include subcommand names',
      Pos('sub', Output.Text) > 0);
    AssertTrue('Parent help should include subcommand descriptions',
      Pos('Sub command', Output.Text) > 0);
  finally
    Output.Free;
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

// 7.x - Root Command Tests

procedure TCLIFrameworkTests.Test_7_1_CreateApplicationWithRootCommand;
var
  Root: TRecordingCommand;
  App: ICLIApplication;
begin
  Root := TRecordingCommand.Create('', 'Default application action');
  App := CreateCLIApplication('TestApp', '1.3.0', Root);
  AssertNotNull('Application with root command should be created', App);
  AssertTrue('Application should retain the configured root command',
    Assigned((App as TCLIApplication).RootCommand));
end;

procedure TCLIFrameworkTests.Test_7_2_ExecuteRootCommandWithoutArguments;
var
  Root: TRecordingCommand;
  App: TCLIApplication;
begin
  Root := TRecordingCommand.Create('', 'Default application action');
  App := TCLIApplication.Create('TestApp', '1.3.0', Root);
  try
    AssertEquals('Root execution should succeed', 0,
      App.TestExecute(MakeArgs([])));
    AssertEquals('Root command should execute once', 1, Root.ExecuteCount);
  finally
    App.Free;
  end;
end;

procedure TCLIFrameworkTests.Test_7_3_ExecuteRootCommandWithParameters;
var
  Root: TRecordingCommand;
  App: TCLIApplication;
begin
  Root := TRecordingCommand.Create('', 'Default application action');
  Root.AddStringParameter('-n', '--name', 'Name to greet');
  App := TCLIApplication.Create('TestApp', '1.3.0', Root);
  try
    AssertEquals('Root execution with parameters should succeed', 0,
      App.TestExecute(MakeArgs(['--name', 'Gus'])));
    AssertEquals('Root command should execute once', 1, Root.ExecuteCount);
    AssertEquals('Root command should receive its parameter', 'Gus',
      Root.LastName);
  finally
    App.Free;
  end;
end;

procedure TCLIFrameworkTests.Test_7_4_NamedCommandTakesPrecedence;
var
  Root, Named: TRecordingCommand;
  App: TCLIApplication;
begin
  Root := TRecordingCommand.Create('', 'Default application action');
  Named := TRecordingCommand.Create('named', 'Named action');
  App := TCLIApplication.Create('TestApp', '1.3.0', Root);
  try
    App.RegisterCommand(Named);
    AssertEquals('Named command execution should succeed', 0,
      App.TestExecute(MakeArgs(['named'])));
    AssertEquals('Root command should not execute', 0, Root.ExecuteCount);
    AssertEquals('Named command should execute once', 1, Named.ExecuteCount);
  finally
    App.Free;
  end;
end;

procedure TCLIFrameworkTests.Test_7_5_GlobalHelpDoesNotExecuteRoot;
var
  Root: TRecordingCommand;
  App: TCLIApplication;
begin
  Root := TRecordingCommand.Create('', 'Default application action');
  App := TCLIApplication.Create('TestApp', '1.3.0', Root);
  try
    AssertEquals('Global help should succeed', 0,
      App.TestExecute(MakeArgs(['--help'])));
    AssertEquals('Global help should not execute the root command', 0,
      Root.ExecuteCount);
  finally
    App.Free;
  end;
end;

procedure TCLIFrameworkTests.Test_7_6_InvalidRootParameterDoesNotExecute;
var
  Root: TRecordingCommand;
  App: TCLIApplication;
begin
  Root := TRecordingCommand.Create('', 'Default application action');
  Root.AddIntegerParameter('-c', '--count', 'Number of runs');
  App := TCLIApplication.Create('TestApp', '1.3.0', Root);
  try
    AssertEquals('Invalid root parameter should fail', 1,
      App.TestExecute(MakeArgs(['--count', 'not-a-number'])));
    AssertEquals('Invalid parameters should prevent root execution', 0,
      Root.ExecuteCount);
  finally
    App.Free;
  end;
end;

procedure TCLIFrameworkTests.Test_7_7_CompleteRootParameters;
var
  Root: TRecordingCommand;
  App: TCLIApplication;
  Candidates: TStringList;
begin
  Root := TRecordingCommand.Create('', 'Default application action');
  Root.AddStringParameter('-n', '--name', 'Name to greet');
  Root.AddEnumParameter('-m', '--mode', 'Greeting mode',
    'normal|friendly|formal');
  App := TCLIApplication.Create('TestApp', '1.3.0', Root);
  try
    Candidates := App.TestComplete(MakeArgs(['--n']));
    try
      AssertTrue('Root parameter name should be completed',
        Candidates.IndexOf('--name') >= 0);
    finally
      Candidates.Free;
    end;

    Candidates := App.TestComplete(MakeArgs(['--']));
    try
      AssertTrue('Root completion should retain all global options',
        Candidates.IndexOf('--completion-file') >= 0);
    finally
      Candidates.Free;
    end;

    Candidates := App.TestComplete(MakeArgs(['--mode', '']));
    try
      AssertTrue('Root enum value should be completed',
        Candidates.IndexOf('friendly') >= 0);
    finally
      Candidates.Free;
    end;
  finally
    App.Free;
  end;
end;

procedure TCLIFrameworkTests.Test_7_8_NoRootPreservesEmptyArgumentBehavior;
var
  App: TCLIApplication;
begin
  App := TCLIApplication.Create('TestApp', '1.3.0');
  try
    AssertEquals('Empty execution without a root command should show help', 0,
      App.TestExecute(MakeArgs([])));
    AssertFalse('No command should be selected without a root command',
      Assigned(App.CurrentCommand));
  finally
    App.Free;
  end;
end;

initialization
  RegisterTest(TCLIFrameworkTests);
end.

