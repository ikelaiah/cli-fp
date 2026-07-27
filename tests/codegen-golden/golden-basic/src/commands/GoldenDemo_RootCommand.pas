unit GoldenDemo_RootCommand;

{$mode objfpc}{$H+}{$J-}

interface

uses
  CLI.Command;

{ User-owned root command stub created by cli-fp-gen. Safe to edit. }
type
  TRootCommand = class(TBaseCommand)
  public
    constructor Create; reintroduce;
    function Execute: Integer; override;
  end;

implementation

constructor TRootCommand.Create;
begin
  inherited Create('', 'Run the default greeting');
end;

function TRootCommand.Execute: Integer;
begin
  WriteLn('TODO: Implement the root command');
  Result := 0;
end;

end.
