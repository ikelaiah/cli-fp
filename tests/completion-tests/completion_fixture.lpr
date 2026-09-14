program CompletionFixture;

{$mode objfpc}{$H+}{$J-}

{ Small executable used by the shell-specific completion CI contracts. }

uses
  CLI.Application, CLI.Command;

type
  TCompletionFixtureCommand = class(TBaseCommand)
  public
    function Execute: Integer; override;
  end;

function TCompletionFixtureCommand.Execute: Integer;
begin
  Result := 0;
end;

var
  App: TCLIApplication;
  Deploy, Target: TCompletionFixtureCommand;
  ExitCode: Integer;
begin
  App := TCLIApplication.Create('completion-fixture', '1.6.0');
  try
    Deploy := TCompletionFixtureCommand.Create('deploy', 'Deploy an application');
    Deploy.AddStringParameter('-o', '--output', 'Output file');
    Target := TCompletionFixtureCommand.Create('target', 'Manage targets');
    Target.AddFlag('-f', '--force', 'Force the operation');
    Deploy.AddSubCommand(Target);
    App.RegisterCommand(Deploy);
    ExitCode := App.Execute;
  finally
    App.Free;
  end;
  Halt(ExitCode);
end.
