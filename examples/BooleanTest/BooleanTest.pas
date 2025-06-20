program BooleanTest;

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
  FlagValue, BoolValue, TrueFlagValue: string;
  HasFlag, HasBool, HasTrueFlag: Boolean;
begin
  // Get the values
  HasFlag := GetParameterValue('--flag', FlagValue);
  HasBool := GetParameterValue('--bool', BoolValue);
  HasTrueFlag := GetParameterValue('--true-flag', TrueFlagValue);
  
  // Output the results
  TConsole.WriteLn('Test Results:', ccCyan);
  TConsole.WriteLn('----------------------------------------', ccCyan);
  
  // Simple flag (--flag)
  if HasFlag then
  begin
    if SameText(FlagValue, 'true') then
      TConsole.WriteLn('1. Simple flag (--flag): true', ccGreen)
    else
      TConsole.WriteLn('1. Simple flag (--flag): ' + FlagValue , ccYellow);
  end
  else
    TConsole.WriteLn('1. Simple flag (--flag): false (default)', ccGreen);
  
  // Boolean parameter (--bool)
  if HasBool then
    TConsole.WriteLn('2. Boolean parameter (--bool): ' + BoolValue, ccGreen)
  else
    TConsole.WriteLn('2. Boolean parameter (--bool): false (default)', ccYellow);

  // True flag (--true-flag)
  if HasTrueFlag then
  begin
    if SameText(TrueFlagValue, 'true') then
      TConsole.WriteLn('3. True flag (--true-flag): true', ccGreen)
    else
      TConsole.WriteLn('3. True flag (--true-flag): ' + TrueFlagValue + ' (explicitly set)', ccYellow);
  end
  else
    TConsole.WriteLn('3. True flag (--true-flag): true (default)', ccGreen);
  
  Result := 0;
end;

var
  App: ICLIApplication;
  Cmd: TTestCommand;
begin
  App := CreateCLIApplication('Boolean Test', '1.1.1');
  
  Cmd := TTestCommand.Create('test', 'Test boolean flag behavior');
  
  // Add parameters directly to the command
  Cmd.AddFlag('-f', '--flag', 'A simple boolean flag');
  Cmd.AddBooleanParameter('-b', '--bool', 'A boolean parameter that requires true/false', False, 'false');
  Cmd.AddFlag('-t', '--true-flag', 'A flag that defaults to true', 'true');
  
  App.RegisterCommand(Cmd);
  
  ExitCode := App.Execute;
end.
