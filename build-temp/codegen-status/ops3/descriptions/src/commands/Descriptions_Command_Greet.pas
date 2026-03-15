unit Descriptions_Command_Greet;

{$mode objfpc}{$H+}{$J-}

interface

uses
  CLI.Command;

{ User-owned stub created by cli-fp-gen. Safe to edit. }
type
  TGreetCommand = class(TBaseCommand)
  public
    constructor Create; reintroduce;
    function Execute: Integer; override;
  end;

implementation

constructor TGreetCommand.Create;
begin
  inherited Create('greet', 'Say hello');
end;

function TGreetCommand.Execute: Integer;
begin
  if Length(SubCommands) > 0 then
  begin
    ShowHelp;
    Exit(0);
  end;

  WriteLn('TODO: Implement command "greet"');
  Result := 0;
end;

end.
