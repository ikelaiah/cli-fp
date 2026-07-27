program RootCommandDemo;

{$mode objfpc}{$H+}{$J-}

{ Demonstrates an optional root command. The root command runs directly:

    RootCommandDemo
    RootCommandDemo --name Gus
    RootCommandDemo --name Gus --shout

  Named commands can still coexist:

    RootCommandDemo about
}

uses
  SysUtils,
  CLI.Interfaces,
  CLI.Application,
  CLI.Command,
  CLI.Console;

type
  TGreetRootCommand = class(TBaseCommand)
  public
    function Execute: Integer; override;
  end;

  TAboutCommand = class(TBaseCommand)
  public
    function Execute: Integer; override;
  end;

function TGreetRootCommand.Execute: Integer;
var
  NameValue, ShoutValue, Greeting: string;
begin
  if not GetParameterValue('--name', NameValue) then
    NameValue := 'World';

  GetParameterValue('--shout', ShoutValue);
  Greeting := 'Hello, ' + NameValue + '!';
  if SameText(ShoutValue, 'true') then
    Greeting := UpperCase(Greeting);

  TConsole.WriteLn(Greeting, ccGreen);
  Result := 0;
end;

function TAboutCommand.Execute: Integer;
begin
  TConsole.WriteLn('RootCommandDemo uses cli-fp v1.3.0.', ccCyan);
  Result := 0;
end;

var
  App: ICLIApplication;
  RootCommand: TGreetRootCommand;
  AboutCommand: TAboutCommand;
begin
  try
    // An empty command name is conventional for a root command. Its name is
    // never required on the command line.
    RootCommand := TGreetRootCommand.Create('',
      'Greets someone without requiring a named command.');
    RootCommand.AddStringParameter('-n', '--name', 'Name to greet',
      False, 'World');
    RootCommand.AddFlag('-s', '--shout', 'Print the greeting in uppercase');

    App := CreateCLIApplication('RootCommandDemo', '1.3.0', RootCommand);

    AboutCommand := TAboutCommand.Create('about',
      'Show information about this example');
    App.RegisterCommand(AboutCommand);

    ExitCode := App.Execute;
  except
    on E: Exception do
    begin
      TConsole.WriteLn('Error: ' + E.Message, ccRed);
      ExitCode := 1;
    end;
  end;
end.
