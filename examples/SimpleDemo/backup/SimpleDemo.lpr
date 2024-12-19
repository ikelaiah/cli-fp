program SimpleDemo;

{$mode objfpc}{$H+}

uses
  SysUtils,
  CLI.Interfaces,
  CLI.Application,
  CLI.Parameter,
  CLI.Progress,
  CLI.Console,
  CLI.Errors;

type
  TGreetCommand = class(TBaseCommand)
  public
    function Execute: Integer; override;
  end;

function TGreetCommand.Execute: Integer;
var
  Name: string;
  Count: Integer;
  i: Integer;
  Spinner: IProgressIndicator;
begin
  Result := 0;

  if not GetParameterValue('--name', Name) then
    Name := 'World';

  if not GetParameterValue('--count', Count) then
    Count := 1;

  Spinner := CreateSpinner(ssLine);
  Spinner.Start;

  try
    for i := 1 to Count do
    begin
      Spinner.Update(0);
      Sleep(500);
      TConsole.WriteLn('Hello, ' + Name + '!', ccGreen);
    end;
  finally
    Spinner.Stop;
  end;
end;

var
  App: ICLIApplication;
  Cmd: ICommand;
begin
  try
    App := CreateCLIApplication('MyApp', '1.0.0');

    Cmd := CreateCommand('greet', 'Greet someone nicely')
      .AddParameter(CreateParameter('-n', '--name', 'Name to greet', False, ptString, 'World'))
      .AddParameter(CreateParameter('-c', '--count', 'Number of times to greet', False, ptInteger, '1'));

    App.RegisterCommand(Cmd);
    ExitCode := App.Execute;
  except
    on E: Exception do
    begin
      TConsole.WriteLn('Error: ' + E.Message, ccRed);
      ExitCode := 1;
    end;
  end;
end.
