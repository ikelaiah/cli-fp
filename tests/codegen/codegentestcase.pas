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
  published
    procedure TestCommandNameSeparatorIsRejected;
    procedure TestGeneratedIdentifierCollisionIsRejected;
    procedure TestNestedGeneratedIdentifierCollisionIsRejected;
    procedure TestReservedWordAppNameProducesValidProgramIdentifier;
    procedure TestProgramFileCannotEscapeProject;
  end;

implementation

uses
  CliFpGen.Naming,
  CliFpGen.Validate;

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

initialization
  RegisterTest(TCodegenTests);

end.
