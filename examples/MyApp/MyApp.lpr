program MyApp;

{$mode objfpc}{$H+}{$J-}

uses
  SysUtils, CLI.Interfaces, CLI.Application, CLI.Command;

type
  TGreetCommand = class(TBaseCommand)
  public
    function Execute: Integer; override;
  end;

function TGreetCommand.Execute: Integer;
var
  UserName: string;
  PrintCount: string;
  i: Integer;
begin
  // Get parameter values using helper methods
  GetParameterValue('--name', UserName);
  GetParameterValue('--count', PrintCount);

  for i := 1 to StrToIntDef(PrintCount, 1) do
    WriteLn('Hello, ', UserName, '!');

  Result := 0;
end;

var
  App: ICLIApplication;
  Cmd: TGreetCommand;
begin
  App := CreateCLIApplication('MyApp', '1.0.0');
  Cmd := TGreetCommand.Create('greet', 'Say hello');

  // Add parameters using new helper methods
  Cmd.AddStringParameter('-n', '--name', 'Name to greet', False, 'World');
  Cmd.AddIntegerParameter('-c', '--count', 'Number of times to greet', False, '1');

  App.RegisterCommand(Cmd);
  ExitCode := App.Execute;
end.
