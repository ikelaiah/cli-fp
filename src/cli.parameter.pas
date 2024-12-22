unit CLI.Parameter;

{$mode objfpc}{$H+}{$J-}

{ This unit implements command parameter functionality.
  It provides the foundation for handling command-line parameters
  with support for different types, validation, and default values. }

interface

uses
  Classes, SysUtils, CLI.Interfaces;

type
  { TCommandParameter - Implements command parameter functionality
    Provides:
    - Short and long form flags
    - Parameter descriptions
    - Required/optional status
    - Type information
    - Default values }
  TCommandParameter = class(TInterfacedObject, ICommandParameter)
  private
    FShortFlag: string;      // Short form flag (e.g., '-n')
    FLongFlag: string;       // Long form flag (e.g., '--name')
    FDescription: string;    // Parameter description for help
    FRequired: Boolean;      // Whether parameter is required
    FParamType: TParameterType;  // Parameter data type
    FDefaultValue: string;   // Default value if not provided
    
    { Gets short form flag
      @returns String containing short form flag }
    function GetShortFlag: string;
    
    { Gets long form flag
      @returns String containing long form flag }
    function GetLongFlag: string;
    
    { Gets parameter description
      @returns String containing parameter description }
    function GetDescription: string;
    
    { Gets required status
      @returns Boolean indicating if parameter is required }
    function GetRequired: Boolean;
    
    { Gets parameter type
      @returns TParameterType indicating parameter's data type }
    function GetParamType: TParameterType;
    
    { Gets default value
      @returns String containing default value (empty if none) }
    function GetDefaultValue: string;
  public
    { Creates new parameter instance
      @param AShortFlag Short form flag (e.g., '-n')
      @param ALongFlag Long form flag (e.g., '--name')
      @param ADescription Parameter description for help
      @param ARequired Whether parameter is required
      @param AParamType Parameter data type
      @param ADefaultValue Default value if not provided }
    constructor Create(const AShortFlag, ALongFlag, ADescription: string;
      ARequired: Boolean; AParamType: TParameterType; const ADefaultValue: string = '');
      
    { Short form flag (e.g., '-n') }
    property ShortFlag: string read GetShortFlag;
    
    { Long form flag (e.g., '--name') }
    property LongFlag: string read GetLongFlag;
    
    { Parameter description for help text }
    property Description: string read GetDescription;
    
    { Whether parameter is required }
    property Required: Boolean read GetRequired;
    
    { Parameter data type }
    property ParamType: TParameterType read GetParamType;
    
    { Default value if not provided }
    property DefaultValue: string read GetDefaultValue;
  end;

{ Helper function to create parameter instances
  @param ShortFlag Short form flag (e.g., '-n')
  @param LongFlag Long form flag (e.g., '--name')
  @param Description Parameter description for help
  @param Required Whether parameter is required
  @param ParamType Parameter data type
  @param DefaultValue Default value if not provided
  @returns ICommandParameter interface to new parameter instance }
function CreateParameter(const ShortFlag, LongFlag, Description: string;
  Required: Boolean; ParamType: TParameterType; const DefaultValue: string = ''): ICommandParameter;

implementation

{ Constructor: Creates new parameter instance
  @param AShortFlag Short form flag (e.g., '-n')
  @param ALongFlag Long form flag (e.g., '--name')
  @param ADescription Parameter description for help
  @param ARequired Whether parameter is required
  @param AParamType Parameter data type
  @param ADefaultValue Default value if not provided }
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

{ GetShortFlag: Returns short form flag
  @returns String containing short form flag }
function TCommandParameter.GetShortFlag: string;
begin
  Result := FShortFlag;
end;

{ GetLongFlag: Returns long form flag
  @returns String containing long form flag }
function TCommandParameter.GetLongFlag: string;
begin
  Result := FLongFlag;
end;

{ GetDescription: Returns parameter description
  @returns String containing parameter description }
function TCommandParameter.GetDescription: string;
begin
  Result := FDescription;
end;

{ GetRequired: Returns required status
  @returns Boolean indicating if parameter is required }
function TCommandParameter.GetRequired: Boolean;
begin
  Result := FRequired;
end;

{ GetParamType: Returns parameter type
  @returns TParameterType indicating parameter's data type }
function TCommandParameter.GetParamType: TParameterType;
begin
  Result := FParamType;
end;

{ GetDefaultValue: Returns default value
  @returns String containing default value (empty if none) }
function TCommandParameter.GetDefaultValue: string;
begin
  Result := FDefaultValue;
end;

{ CreateParameter: Helper function to create parameter instances
  @param ShortFlag Short form flag (e.g., '-n')
  @param LongFlag Long form flag (e.g., '--name')
  @param Description Parameter description for help
  @param Required Whether parameter is required
  @param ParamType Parameter data type
  @param DefaultValue Default value if not provided
  @returns ICommandParameter interface to new parameter instance }
function CreateParameter(const ShortFlag, LongFlag, Description: string;
  Required: Boolean; ParamType: TParameterType; const DefaultValue: string): ICommandParameter;
begin
  Result := TCommandParameter.Create(ShortFlag, LongFlag, Description,
    Required, ParamType, DefaultValue);
end;

end. 