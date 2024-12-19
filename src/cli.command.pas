unit CLI.Command;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CLI.Interfaces;

type
  { Base command class }
  TBaseCommand = class(TInterfacedObject, ICommand)
  private
    FName: string;
    FDescription: string;
    FParameters: TCommandParameterArray;
    FParsedParams: TStringList;
    function GetName: string;
    function GetDescription: string;
    function GetParameters: TCommandParameterArray;
  protected
    function ValidateParameters: Boolean; virtual;
    function GetParameterValue(const Flag: string; out Value: string): Boolean;
  public
    constructor Create(const AName, ADescription: string);
    destructor Destroy; override;
    function AddParameter(const Parameter: ICommandParameter): TBaseCommand;
    function Execute: Integer; virtual; abstract;
    procedure SetParsedParams(const Params: TStringList);
    
    property Name: string read GetName;
    property Description: string read GetDescription;
    property Parameters: TCommandParameterArray read GetParameters;
  end;

{ Helper function to create commands }
function CreateCommand(const Name, Description: string): TBaseCommand;

implementation

constructor TBaseCommand.Create(const AName, ADescription: string);
begin
  inherited Create;
  FName := AName;
  FDescription := ADescription;
  SetLength(FParameters, 0);
  FParsedParams := TStringList.Create;
  FParsedParams.CaseSensitive := True;
end;

destructor TBaseCommand.Destroy;
begin
  FParsedParams.Free;
  inherited;
end;

function TBaseCommand.GetName: string;
begin
  Result := FName;
end;

function TBaseCommand.GetDescription: string;
begin
  Result := FDescription;
end;

function TBaseCommand.GetParameters: TCommandParameterArray;
begin
  Result := FParameters;
end;

function TBaseCommand.AddParameter(const Parameter: ICommandParameter): TBaseCommand;
begin
  SetLength(FParameters, Length(FParameters) + 1);
  FParameters[High(FParameters)] := Parameter;
  Result := Self;
end;

function TBaseCommand.ValidateParameters: Boolean;
var
  Param: ICommandParameter;
  Value: string;
begin
  Result := True;
  for Param in FParameters do
  begin
    if Param.Required then
    begin
      if not GetParameterValue(Param.LongFlag, Value) and
         not GetParameterValue(Param.ShortFlag, Value) then
      begin
        Result := False;
        Break;
      end;
    end;
  end;
end;

function TBaseCommand.GetParameterValue(const Flag: string; out Value: string): Boolean;
var
  Param: ICommandParameter;
begin
  // First try to get the value from parsed parameters
  Value := FParsedParams.Values[Flag];
  Result := Value <> '';
  
  // If not found, look for the parameter and use its default value
  if not Result then
  begin
    for Param in FParameters do
    begin
      if (Param.LongFlag = Flag) or (Param.ShortFlag = Flag) then
      begin
        Value := Param.DefaultValue;
        Result := Value <> '';
        Break;
      end;
    end;
  end;
end;

procedure TBaseCommand.SetParsedParams(const Params: TStringList);
var
  i: Integer;
begin
  FParsedParams.Clear;
  for i := 0 to Params.Count - 1 do
  begin
    FParsedParams.Add(Params[i]);
  end;
end;

function CreateCommand(const Name, Description: string): TBaseCommand;
begin
  Result := TBaseCommand.Create(Name, Description);
end;

end.
