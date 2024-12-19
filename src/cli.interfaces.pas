unit CLI.Interfaces;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  { Command parameter types }
  TParamType = (ptString, ptInteger, ptBoolean, ptFloat);

  { Forward declarations }
  ICommandParameter = interface;
  
  { Type definitions }
  TCommandParameterArray = array of ICommandParameter;

  { Interface for parameter/flag definition }
  ICommandParameter = interface
    ['{A1B2C3D4-E5F6-4788-89A0-B1C2D3E4F5A6}']
    function GetShortFlag: string;
    function GetLongFlag: string;
    function GetDescription: string;
    function GetRequired: Boolean;
    function GetParamType: TParamType;
    function GetDefaultValue: string;
    
    property ShortFlag: string read GetShortFlag;
    property LongFlag: string read GetLongFlag;
    property Description: string read GetDescription;
    property Required: Boolean read GetRequired;
    property ParamType: TParamType read GetParamType;
    property DefaultValue: string read GetDefaultValue;
  end;

  { Interface for command definition }
  ICommand = interface
    ['{B2C3D4E5-F6A7-4889-90B1-C2D3E4F5A6B7}']
    function GetName: string;
    function GetDescription: string;
    function GetParameters: TCommandParameterArray;
    function Execute: Integer;
    procedure SetParsedParams(const Params: TStringList);
    
    property Name: string read GetName;
    property Description: string read GetDescription;
    property Parameters: TCommandParameterArray read GetParameters;
  end;

  { Interface for progress indicators }
  IProgressIndicator = interface
    ['{C3D4E5F6-A7B8-49C0-D1E2-F3A4B5C6D7E8}']
    procedure Start;
    procedure Stop;
    procedure Update(const Progress: Integer); // 0-100 for percentage
  end;

  { Interface for the CLI application }
  ICLIApplication = interface
    ['{D4E5F6A7-B8C9-4AD0-91E2-F3A4B5C6D7E8}']
    procedure RegisterCommand(const Command: ICommand);
    function Execute: Integer;
  end;

implementation

end. 
