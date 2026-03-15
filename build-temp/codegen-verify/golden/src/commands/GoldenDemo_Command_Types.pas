unit GoldenDemo_Command_Types;

{$mode objfpc}{$H+}{$J-}

interface

uses
  CLI.Command;

{ User-owned stub created by cli-fp-gen. Safe to edit. }
type
  TTypesCommand = class(TBaseCommand)
  public
    constructor Create; reintroduce;
    function Execute: Integer; override;
  end;

implementation

constructor TTypesCommand.Create;
begin
  inherited Create('types', 'Parameter type showcase');
end;

function TTypesCommand.Execute: Integer;
begin
  if Length(SubCommands) > 0 then
  begin
    ShowHelp;
    Exit(0);
  end;

  WriteLn('TODO: Implement command "types"');
  Result := 0;
end;

end.
