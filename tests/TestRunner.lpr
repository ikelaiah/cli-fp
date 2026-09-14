program TestRunner;

{$mode objfpc}{$H+}{$J-}

uses
  Classes, consoletestrunner, TestCase, Test_Completion_Scripts, Test_Release_161;

type

  { TMyTestRunner }

  TMyTestRunner = class(TTestRunner)
  protected
  // override the protected methods of TTestRunner to customize its behavior
  end;

var
  Application: TMyTestRunner;

begin
  Application := TMyTestRunner.Create(nil);
  Application.Initialize;
  Application.Title := 'FPCUnit Console test runner';
  Application.Run;
  Application.Free;
end.
