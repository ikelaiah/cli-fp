unit CodegenTestCase;

{$mode objfpc}{$H+}{$J-}

interface

uses
  Classes,
  SysUtils,
  FpcUnit,
  TestRegistry,
  CliFpGen.Model;

type
  TCodegenTests = class(TTestCase)
  private
    function NewValidSpec: TProjectSpec;
    function AddCommand(const Spec: TProjectSpec; const Name: string;
      const ParentPath: string = ''): TCommandSpec;
    procedure AssertValidationFails(const Spec: TProjectSpec;
      const ExpectedMessagePart: string);
    procedure AssertSpecLoadFails(const JsonText, ExpectedMessagePart: string);
  published
    procedure TestCommandNameSeparatorIsRejected;
    procedure TestGeneratedIdentifierCollisionIsRejected;
    procedure TestNestedGeneratedIdentifierCollisionIsRejected;
    procedure TestReservedWordAppNameProducesValidProgramIdentifier;
    procedure TestProgramFileCannotEscapeProject;
    procedure TestInvalidParameterKindReportsItsLocation;
    procedure TestInvalidRootParameterKindReportsItsLocation;
    procedure TestNonObjectParameterReportsItsLocation;
    procedure TestMalformedParameterDoesNotLeakOwnedSpecs;
    procedure TestRootCommandSpecRoundTrips;
  end;

implementation

uses
  CliFpGen.Naming,
  CliFpGen.SpecIO,
  CliFpGen.Validate;

procedure TCodegenTests.AssertSpecLoadFails(const JsonText,
  ExpectedMessagePart: string);
var
  SpecFile: string;
  Lines: TStringList;
  Spec: TProjectSpec;
  RaisedExpectedError: Boolean;
begin
  SpecFile := GetTempFileName(GetTempDir(False), 'cfg');
  Lines := TStringList.Create;
  try
    Lines.Text := JsonText;
    Lines.SaveToFile(SpecFile);
  finally
    Lines.Free;
  end;

  try
    RaisedExpectedError := False;
    Spec := nil;
    try
      Spec := LoadProjectSpec(SpecFile);
    except
      on E: Exception do
      begin
        RaisedExpectedError := True;
        AssertTrue(
          Format('Expected error containing "%s", got "%s"',
            [ExpectedMessagePart, E.Message]),
          Pos(LowerCase(ExpectedMessagePart), LowerCase(E.Message)) > 0
        );
      end;
    end;
    Spec.Free;
    AssertTrue('Expected spec loading to fail', RaisedExpectedError);
  finally
    DeleteFile(SpecFile);
  end;
end;

function TCodegenTests.NewValidSpec: TProjectSpec;
begin
  Result := TProjectSpec.Create;
  Result.AppName := 'demo';
  Result.AppVersion := '1.0.0';
  Result.ProgramFile := 'src/Demo.lpr';
end;

function TCodegenTests.AddCommand(const Spec: TProjectSpec;
  const Name: string; const ParentPath: string): TCommandSpec;
begin
  Result := TCommandSpec.Create;
  Result.Name := Name;
  Result.Description := 'Test command';
  Result.ParentPath := ParentPath;
  Spec.Commands.Add(Result);
end;

procedure TCodegenTests.AssertValidationFails(const Spec: TProjectSpec;
  const ExpectedMessagePart: string);
var
  RaisedExpectedError: Boolean;
begin
  RaisedExpectedError := False;
  try
    ValidateProjectSpec(Spec);
  except
    on E: Exception do
    begin
      RaisedExpectedError := True;
      AssertTrue(
        Format('Expected error containing "%s", got "%s"',
          [ExpectedMessagePart, E.Message]),
        Pos(LowerCase(ExpectedMessagePart), LowerCase(E.Message)) > 0
      );
    end;
  end;
  AssertTrue('Expected validation to fail', RaisedExpectedError);
end;

procedure TCodegenTests.TestCommandNameSeparatorIsRejected;
var
  Spec: TProjectSpec;
begin
  Spec := NewValidSpec;
  try
    AddCommand(Spec, 'repo/clone');
    AssertValidationFails(Spec, 'Invalid command name');
  finally
    Spec.Free;
  end;
end;

procedure TCodegenTests.TestGeneratedIdentifierCollisionIsRejected;
var
  Spec: TProjectSpec;
begin
  Spec := NewValidSpec;
  try
    AddCommand(Spec, 'foo-bar');
    AddCommand(Spec, 'foo_bar');
    AssertValidationFails(Spec, 'same Pascal identifier');
  finally
    Spec.Free;
  end;
end;

procedure TCodegenTests.TestNestedGeneratedIdentifierCollisionIsRejected;
var
  Spec: TProjectSpec;
begin
  Spec := NewValidSpec;
  try
    AddCommand(Spec, 'a');
    AddCommand(Spec, 'ab');
    AddCommand(Spec, 'bc', 'a');
    AddCommand(Spec, 'c', 'ab');
    AssertValidationFails(Spec, 'same Pascal identifier');
  finally
    Spec.Free;
  end;
end;

procedure TCodegenTests.TestReservedWordAppNameProducesValidProgramIdentifier;
begin
  AssertEquals('AppProgram', MakeProgramIdentifier('program'));
  AssertEquals('GoldenDemo', MakeProgramIdentifier('golden-demo'));
end;

procedure TCodegenTests.TestProgramFileCannotEscapeProject;
var
  Spec: TProjectSpec;
begin
  Spec := NewValidSpec;
  try
    Spec.ProgramFile := 'src/../outside/Demo.lpr';
    AssertValidationFails(Spec, 'must not escape');
  finally
    Spec.Free;
  end;
end;

procedure TCodegenTests.TestInvalidParameterKindReportsItsLocation;
begin
  AssertSpecLoadFails(
    '{"schemaVersion":1,"app":{"name":"demo","version":"1.0.0",' +
    '"programFile":"src/Demo.lpr"},"commands":[{"name":"run",' +
    '"description":"Run","parent":"","parameters":[{"kind":"string",' +
    '"short":"-n","long":"--name","description":"Name","required":false},' +
    '{"kind":"not-a-kind","short":"","long":"--bad",' +
    '"description":"Bad","required":false}]}]}',
    'commands[0].parameters[1]'
  );
end;

procedure TCodegenTests.TestInvalidRootParameterKindReportsItsLocation;
begin
  AssertSpecLoadFails(
    '{"schemaVersion":1,"app":{"name":"demo","version":"1.0.0",' +
    '"programFile":"src/Demo.lpr"},"rootCommand":{"description":"Run",' +
    '"parameters":[{"kind":"not-a-kind","short":"-x","long":"--bad",' +
    '"description":"Bad","required":false}]},"commands":[]}',
    'rootCommand.parameters[0]'
  );
end;

procedure TCodegenTests.TestNonObjectParameterReportsItsLocation;
begin
  AssertSpecLoadFails(
    '{"schemaVersion":1,"app":{"name":"demo","version":"1.0.0",' +
    '"programFile":"src/Demo.lpr"},"commands":[{"name":"run",' +
    '"description":"Run","parent":"","parameters":[42]}]}',
    'commands[0].parameters[0] must be an object'
  );
end;

procedure TCodegenTests.TestMalformedParameterDoesNotLeakOwnedSpecs;
const
  MalformedJson =
    '{"schemaVersion":1,"app":{"name":"demo","version":"1.0.0",' +
    '"programFile":"src/Demo.lpr"},"commands":[{"name":"run",' +
    '"description":"Run","parent":"","parameters":[{"kind":"string",' +
    '"short":"-n","long":"--name","description":"Name","required":false},' +
    '{"kind":"not-a-kind","short":"","long":"--bad",' +
    '"description":"Bad","required":false}]}]}';
var
  SpecFile: string;
  Lines: TStringList;
  Spec: TProjectSpec;
  BeforeStatus, AfterStatus: TFPCHeapStatus;
  i: Integer;
begin
  SpecFile := GetTempFileName(GetTempDir(False), 'cfg');
  Lines := TStringList.Create;
  try
    Lines.Text := MalformedJson;
    Lines.SaveToFile(SpecFile);
  finally
    Lines.Free;
  end;

  try
    // Warm up the JSON parser and exception path before measuring live blocks.
    for i := 1 to 10 do
    begin
      Spec := nil;
      try
        Spec := LoadProjectSpec(SpecFile);
      except
        on E: Exception do
          ;
      end;
      Spec.Free;
    end;

    BeforeStatus := GetFPCHeapStatus;
    for i := 1 to 100 do
    begin
      Spec := nil;
      try
        Spec := LoadProjectSpec(SpecFile);
      except
        on E: Exception do
          ;
      end;
      Spec.Free;
    end;
    AfterStatus := GetFPCHeapStatus;

    AssertTrue(
      Format('Malformed spec loads leaked memory: before=%d, after=%d',
        [BeforeStatus.CurrHeapUsed, AfterStatus.CurrHeapUsed]),
      AfterStatus.CurrHeapUsed <= BeforeStatus.CurrHeapUsed
    );
  finally
    DeleteFile(SpecFile);
  end;
end;

procedure TCodegenTests.TestRootCommandSpecRoundTrips;
var
  Spec, Loaded: TProjectSpec;
  Param: TParameterSpec;
  SpecFile: string;
  Options: TWriteOptions;
begin
  Spec := NewValidSpec;
  SpecFile := GetTempFileName(GetTempDir(False), 'cfg');
  try
    Spec.HasRootCommand := True;
    Spec.RootCommand.Description := 'Default action';
    Param := TParameterSpec.Create;
    Param.Kind := pkString;
    Param.ShortFlag := '-n';
    Param.LongFlag := '--name';
    Param.Description := 'Name';
    Param.DefaultValue := 'World';
    Spec.RootCommand.Parameters.Add(Param);

    Options.DryRun := False;
    Options.Force := True;
    SaveProjectSpec(Spec, SpecFile, Options);

    Loaded := LoadProjectSpec(SpecFile);
    try
      AssertTrue('Root command should remain enabled',
        Loaded.HasRootCommand);
      AssertEquals('Root description should round-trip',
        'Default action', Loaded.RootCommand.Description);
      AssertEquals('Root parameters should round-trip', 1,
        Loaded.RootCommand.Parameters.Count);
      AssertEquals('Root parameter flag should round-trip',
        '--name', Loaded.RootCommand.Parameters[0].LongFlag);
    finally
      Loaded.Free;
    end;
  finally
    Spec.Free;
    DeleteFile(SpecFile);
  end;
end;

initialization
  RegisterTest(TCodegenTests);

end.
