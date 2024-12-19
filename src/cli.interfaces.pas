unit CLI.Interfaces;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  { Parameter types }
  TParameterType = (
    ptString,   // String value
    ptInteger,  // Integer value
    ptFloat,    // Float value
    ptBoolean,  // Boolean value (true/false)
    ptFlag      // Flag parameter (no value)
  );

  { Forward declarations }
  ICommand = interface;
  ICommandParameter = interface;
  ICLIApplication = interface;

  { Command interface }
  ICommand = interface
    ['{D6F6D6D0-5C5C-4B5B-9B5B-5B5B5B5B5B5B}']
    function GetName: string;
    function GetDescription: string;
    function GetParameters: specialize TArray<ICommandParameter>;
    function GetSubCommands: specialize TArray<ICommand>;
    function Execute: Integer;
    property Name: string read GetName;
    property Description: string read GetDescription;
    property Parameters: specialize TArray<ICommandParameter> read GetParameters;
    property SubCommands: specialize TArray<ICommand> read GetSubCommands;
  end;

  { Command parameter interface }
  ICommandParameter = interface
    ['{D6F6D6D1-5C5C-4B5B-9B5B-5B5B5B5B5B5B}']
    function GetShortFlag: string;
    function GetLongFlag: string;
    function GetDescription: string;
    function GetRequired: Boolean;
    function GetParamType: TParameterType;
    function GetDefaultValue: string;
    property ShortFlag: string read GetShortFlag;
    property LongFlag: string read GetLongFlag;
    property Description: string read GetDescription;
    property Required: Boolean read GetRequired;
    property ParamType: TParameterType read GetParamType;
    property DefaultValue: string read GetDefaultValue;
  end;

  { CLI application interface }
  ICLIApplication = interface
    ['{D6F6D6D2-5C5C-4B5B-9B5B-5B5B5B5B5B5B}']
    procedure RegisterCommand(const Command: ICommand);
    function Execute: Integer;
  end;

implementation

end. 
