program MyApp;

{$mode objfpc}{$H+}

uses
  SysUtils, CLI.Interfaces, CLI.Application, CLI.Command;

type
  TGreetCommand = class(TBaseCommand)
  public
    function Execute: Integer; override;
  end;

function TGreetCommand.Execute: Integer;
begin
  WriteLn('Hello, CLI World!');
  Result := 0;
end;

var
  App: ICLIApplication;
  Cmd: TGreetCommand;
begin
  App := CreateCLIApplication('MyApp', '1.0.0');
  Cmd := TGreetCommand.Create('greet', 'Say hello');
  App.RegisterCommand(Cmd);
  ExitCode := App.Execute;
end.
