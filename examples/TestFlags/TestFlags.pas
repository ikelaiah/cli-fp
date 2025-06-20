program TestBooleanFlag;

{$mode objfpc}{$H+}{$J-}

uses
  Classes,
  SysUtils,
  CLI.Interfaces,    // Core interfaces
  CLI.Application,   // Main application framework
  CLI.Command,       // Base command implementation
  CLI.Parameter,     // Parameter handling
  CLI.Progress,      // Optional: Progress indicators
  CLI.Console;       // Optional: Colored console output

type
  TTestCommand = class(TBaseCommand)
  public
    function Execute: Integer; override;
  end;

function TTestCommand.Execute: Integer;
var
  FlagValue: string;
  IsFlagSet: Boolean;
begin
  // Default value should be false when flag is not provided
  IsFlagSet := GetParameterValue('--test-flag', FlagValue);
  
  if IsFlagSet then
  begin
    if SameText(FlagValue, 'true') then
      TConsole.WriteLn('Test passed: Flag is true when provided', ccGreen)
    else if SameText(FlagValue, 'false') then
      TConsole.WriteLn('Test failed: Flag should be true when provided', ccRed)
    else
      TConsole.WriteLn('Test failed: Invalid flag value: ' + FlagValue, ccRed);
  end
  else
  begin
    TConsole.WriteLn('Test passed: Flag is false by default', ccGreen);
  end;
  
  Result := 0;
end;

var
  App: ICLIApplication;
  Cmd: TTestCommand;
begin
  App := CreateCLIApplication('TestBooleanFlag', '1.0.0');
  
  Cmd := TTestCommand.Create('test', 'Test boolean flag default value');
  Cmd.AddFlag('-t', '--test-flag', 'Test boolean flag');
  
  App.RegisterCommand(Cmd);
  
  ExitCode := App.Execute;
end.
