unit CLI.Parameter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CLI.Interfaces;

type
  TCommandParameter = class(TInterfacedObject, ICommandParameter)
  private
    FShortFlag: string;
    FLongFlag: string;
    FDescription: string;
    FRequired: Boolean;
    FParamType: TParamType;
    FDefaultValue: string;
    
    function GetShortFlag: string;
    function GetLongFlag: string;
    function GetDescription: string;
    function GetRequired: Boolean;
    function GetParamType: TParamType;
    function GetDefaultValue: string;
  public
    constructor Create(const AShortFlag, ALongFlag, ADescription: string;
      const ARequired: Boolean; const AParamType: TParamType;
      const ADefaultValue: string = '');
  end;

  { Helper function to create parameters easily }
  function CreateParameter(const AShortFlag, ALongFlag, ADescription: string;
    const ARequired: Boolean; const AParamType: TParamType;
    const ADefaultValue: string = ''): ICommandParameter;

implementation

constructor TCommandParameter.Create(const AShortFlag, ALongFlag, ADescription: string;
  const ARequired: Boolean; const AParamType: TParamType; const ADefaultValue: string);
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

function TCommandParameter.GetParamType: TParamType;
begin
  Result := FParamType;
end;

function TCommandParameter.GetDefaultValue: string;
begin
  Result := FDefaultValue;
end;

function CreateParameter(const AShortFlag, ALongFlag, ADescription: string;
  const ARequired: Boolean; const AParamType: TParamType;
  const ADefaultValue: string): ICommandParameter;
begin
  Result := TCommandParameter.Create(AShortFlag, ALongFlag, ADescription,
    ARequired, AParamType, ADefaultValue);
end;

end. 