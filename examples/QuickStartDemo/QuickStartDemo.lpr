program QuickStartDemo;

{$mode objfpc}{$H+}{$J-}

uses
  CLI.Interfaces,
  CLI.Application,
  CLI.Command;

type
  THelloCommand = class(TBaseCommand)
  public
    function Execute: Integer; override;
  end;

function THelloCommand.Execute: Integer;
var
  PersonName: string;
begin
  if not GetParameterValue('--name', PersonName) then
    PersonName := 'World';
  WriteLn('Hello, ', PersonName, '!');
  Result := 0;
end;

var
  App: ICLIApplication;
  Main: THelloCommand;
begin
  Main := THelloCommand.Create('', 'Print a greeting');
  Main.AddStringParameter('-n', '--name', 'Name to greet', False, 'World');
  App := CreateCLIApplication('hello', '1.0.0', Main);
  Halt(App.Execute);
end.
