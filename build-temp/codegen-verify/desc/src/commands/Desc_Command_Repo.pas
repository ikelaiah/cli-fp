unit Desc_Command_Repo;

{$mode objfpc}{$H+}{$J-}

interface

uses
  CLI.Command;

{ User-owned stub created by cli-fp-gen. Safe to edit. }
type
  TRepoCommand = class(TBaseCommand)
  public
    constructor Create; reintroduce;
    function Execute: Integer; override;
  end;

implementation

constructor TRepoCommand.Create;
begin
  inherited Create('repo', 'Owner''s tools');
end;

function TRepoCommand.Execute: Integer;
begin
  if Length(SubCommands) > 0 then
  begin
    ShowHelp;
    Exit(0);
  end;

  WriteLn('TODO: Implement command "repo"');
  Result := 0;
end;

end.
