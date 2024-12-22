program MyApp;

{$mode objfpc}{$H+}{$J-}

uses
  SysUtils,
  CLI.Interfaces,
  CLI.Application,
  CLI.Command;

type
  // Define a new command (class)
  TGreetCommand = class(TBaseCommand)
  public
    function Execute: integer; override;
  end;

  // Define the command's Execute behaviour
  function TGreetCommand.Execute: integer;
  var
    UserName: string;
    PrintCount: string;
    i: integer;
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

  // Main block
begin
  App := CreateCLIApplication('MyApp', '1.0.0');
  Cmd := TGreetCommand.Create('greet', 'Say hello');

  // Add parameters using new helper methods
  Cmd.AddStringParameter('-n', '--name', 'Name to greet', False, 'World');
  Cmd.AddIntegerParameter('-c', '--count', 'Number of times to greet', False, '1');

  // Register the command to the application
  App.RegisterCommand(Cmd);

  // Execute the application
  ExitCode := App.Execute;
end.
