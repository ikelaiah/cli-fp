program SimpleDemo;

{$mode objfpc}{$H+}

uses
  SysUtils,
  CLI.Interfaces,
  CLI.Application,
  CLI.Parameter,
  CLI.Progress,
  CLI.Command,
  CLI.Console,
  CLI.Errors;

type
  TGreetCommand = class(TBaseCommand)
  public
    function Execute: Integer; override;
  end;

function TGreetCommand.Execute: Integer;
var
  NameValue: string;
  CountValue: string;
  Count: Integer;
  i: Integer;
  Spinner: IProgressIndicator;
begin
  Result := 0;

  if not GetParameterValue('--name', NameValue) then
    NameValue := 'World';

  if not GetParameterValue('--count', CountValue) then
    Count := 1
  else
    Count := StrToIntDef(CountValue, 1);

  Spinner := CreateSpinner(ssLine);
  Spinner.Start;

  try
    for i := 1 to Count do
    begin
      Spinner.Update(0);
      Sleep(500);
      TConsole.WriteLn('Hello, ' + NameValue + '!', ccGreen);
    end;
  finally
    Spinner.Stop;
  end;
end;

var
  App: ICLIApplication;
  Cmd: TGreetCommand;
begin
  try
    App := CreateCLIApplication('MyApp', '1.0.0');

    Cmd := TGreetCommand.Create('greet', 'Greet someone nicely');
    Cmd.AddParameter(CreateParameter('-n', '--name', 'Name to greet', False, ptString, 'World'));
    Cmd.AddParameter(CreateParameter('-c', '--count', 'Number of times to greet', False, ptInteger, '1'));

    App.RegisterCommand(Cmd);
    Cmd := nil;
    ExitCode := App.Execute;
  except
    on E: Exception do
    begin
      TConsole.WriteLn('Error: ' + E.Message, ccRed);
      ExitCode := 1;
    end;
  end;
end.