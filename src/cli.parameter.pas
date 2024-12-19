unit CLI.Parameter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CLI.Interfaces;

type
  { Parameter class }
  TCommandParameter = class(TInterfacedObject, ICommandParameter)
  private
    FShortFlag: string;
    FLongFlag: string;
    FDescription: string;
    FRequired: Boolean;
    FParamType: TParameterType;
    FDefaultValue: string;
    function GetShortFlag: string;
    function GetLongFlag: string;
    function GetDescription: string;
    function GetRequired: Boolean;
    function GetParamType: TParameterType;
    function GetDefaultValue: string;
  public
    constructor Create(const AShortFlag, ALongFlag, ADescription: string;
      ARequired: Boolean; AParamType: TParameterType; const ADefaultValue: string = '');
    property ShortFlag: string read GetShortFlag;
    property LongFlag: string read GetLongFlag;
    property Description: string read GetDescription;
    property Required: Boolean read GetRequired;
    property ParamType: TParameterType read GetParamType;
    property DefaultValue: string read GetDefaultValue;
  end;

{ Helper function to create parameters }
function CreateParameter(const ShortFlag, LongFlag, Description: string;
  Required: Boolean; ParamType: TParameterType; const DefaultValue: string = ''): ICommandParameter;

implementation

constructor TCommandParameter.Create(const AShortFlag, ALongFlag, ADescription: string;
  ARequired: Boolean; AParamType: TParameterType; const ADefaultValue: string);
begin
  inherited Create;
  FShortFlag := AShortFlag;
  FLongFlag := ALongFlag;
  FDescription := ADescription;
  FRequired := ARequired;
  FParamType := AParamType;
  FDefaultValue := ADefaultValue;
end;

function TCommandParameter.GetShortFlag: string;
begin
  Result := FShortFlag;
end;

function TCommandParameter.GetLongFlag: string;
begin
  Result := FLongFlag;
end;

function TCommandParameter.GetDescription: string;
begin
  Result := FDescription;
end;

function TCommandParameter.GetRequired: Boolean;
begin
  Result := FRequired;
end;

function TCommandParameter.GetParamType: TParameterType;
begin
  Result := FParamType;
end;

function TCommandParameter.GetDefaultValue: string;
begin
  Result := FDefaultValue;
end;

function CreateParameter(const ShortFlag, LongFlag, Description: string;
  Required: Boolean; ParamType: TParameterType; const DefaultValue: string): ICommandParameter;
begin
  Result := TCommandParameter.Create(ShortFlag, LongFlag, Description,
    Required, ParamType, DefaultValue);
end;

end. 