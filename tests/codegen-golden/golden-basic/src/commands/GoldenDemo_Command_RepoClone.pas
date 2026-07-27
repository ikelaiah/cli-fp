unit GoldenDemo_Command_RepoClone;

{$mode objfpc}{$H+}{$J-}

interface

uses
  CLI.Command;

{ User-owned stub created by cli-fp-gen. Safe to edit. }
type
  TRepoCloneCommand = class(TBaseCommand)
  public
    constructor Create; reintroduce;
    function Execute: Integer; override;
  end;

implementation

constructor TRepoCloneCommand.Create;
begin
  inherited Create('clone', 'Clone repo');
end;

function TRepoCloneCommand.Execute: Integer;
begin
  if Length(SubCommands) > 0 then
  begin
    ShowHelp;
    Exit(0);
  end;

  WriteLn('TODO: Implement command "repo/clone"');
  Result := 0;
end;

end.
