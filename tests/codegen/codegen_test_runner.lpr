program CodegenTestRunner;

{$mode objfpc}{$H+}{$J-}

uses
  Classes,
  ConsoleTestRunner,
  CodegenTestCase;

type
  TCodegenTestRunner = class(TTestRunner);

var
  Application: TCodegenTestRunner;

begin
  Application := TCodegenTestRunner.Create(nil);
  try
    Application.Initialize;
    Application.Title := 'cli-fp-gen unit tests';
    Application.Run;
  finally
    Application.Free;
  end;
end.
