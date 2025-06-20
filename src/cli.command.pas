unit CLI.Command;

{$mode objfpc}{$H+}{$J-}

{ This unit implements the base command functionality.
  It provides the foundation for creating CLI commands with parameters
  and subcommands. All concrete commands should inherit from TBaseCommand. }

interface

uses
  Classes, SysUtils, StrUtils, CLI.Interfaces, CLI.Parameter, CLI.Console;

type
  { TBaseCommand - Abstract base class for all CLI commands
    Provides:
    - Parameter management
    - Subcommand support
    - Help text generation
    - Parameter value access }
  TBaseCommand = class(TInterfacedObject, ICommand)
  private
    FName: string;              // Command name used in CLI
    FDescription: string;       // Command description for help
    FParameters: array of ICommandParameter;    // Command parameters
    FSubCommands: array of ICommand;           // Nested subcommands
    FParsedParams: TStringList; // Parameter values (owned by application)
  protected
    { Gets the command name
      @returns String containing command name as used in CLI }
    function GetName: string;
    
    { Gets the command description
      @returns String containing command description for help text }
    function GetDescription: string;
    
    { Gets array of command parameters
      @returns Array of ICommandParameter containing all parameters }
    function GetParameters: specialize TArray<ICommandParameter>;
    
    { Gets array of subcommands
      @returns Array of ICommand containing all subcommands }
    function GetSubCommands: specialize TArray<ICommand>;
    
    { Gets value of a parameter by flag
      @param Flag The parameter flag to look up (can be short or long form)
      @param Value Output parameter that receives the value if found
      @returns True if parameter has value (provided or default), False otherwise }
    function GetParameterValue(const Flag: string; out Value: string): Boolean;
    
    { Shows help text for this command
      Displays usage, description, parameters, and examples }
    procedure ShowHelp;
  public
    { Creates new command instance
      @param AName Command name as used in CLI
      @param ADescription Command description for help text }
    constructor Create(const AName, ADescription: string);
    
    { Cleans up command resources }
    destructor Destroy; override;
    
    { Adds a parameter to the command
      @param Parameter The parameter to add }
    procedure AddParameter(const Parameter: ICommandParameter);
    
    { Adds a parameter to the command with direct values
      @param ShortFlag Short form flag (e.g., '-n')
      @param LongFlag Long form flag (e.g., '--name')
      @param Description Parameter description for help
      @param Required Whether parameter is required
      @param ParamType Parameter data type
      @param DefaultValue Default value if not provided
      @param AllowedValues Pipe-separated list of allowed values (e.g., 'debug|info|warn|error') }
    procedure AddParameter(const ShortFlag, LongFlag, Description: string;
      Required: Boolean; ParamType: TParameterType; const DefaultValue: string = '';
      const AllowedValues: string = '');
    
    { Helper: Adds a string parameter
      @param ShortFlag Short form flag (e.g., '-n')
      @param LongFlag Long form flag (e.g., '--name')
      @param Description Parameter description
      @param Required Whether parameter is required
      @param DefaultValue Default value if not provided }
    procedure AddStringParameter(const ShortFlag, LongFlag, Description: string;
      Required: Boolean = False; const DefaultValue: string = '');
    
    { Helper: Adds an integer parameter
      @param ShortFlag Short form flag (e.g., '-n')
      @param LongFlag Long form flag (e.g., '--count')
      @param Description Parameter description
      @param Required Whether parameter is required
      @param DefaultValue Default value if not provided }
    procedure AddIntegerParameter(const ShortFlag, LongFlag, Description: string;
      Required: Boolean = False; const DefaultValue: string = '');
    
    { Helper: Adds a boolean flag parameter
      @param ShortFlag Short form flag (e.g., '-v')
      @param LongFlag Long form flag (e.g., '--verbose')
      @param Description Parameter description
      @param DefaultValue Default value if not provided }
    procedure AddFlag(const ShortFlag, LongFlag, Description: string;
      const DefaultValue: string = 'false');
    
    { Helper: Adds a boolean parameter that requires explicit true/false value  
      @param ShortFlag Short form flag (e.g., '-c')
      @param LongFlag Long form flag (e.g., '--colorful')
      @param Description Parameter description
      @param Required Whether parameter is required
      @param DefaultValue Default value if not provided }
    procedure AddBooleanParameter(const ShortFlag, LongFlag, Description: string;
      Required: Boolean; const DefaultValue: string);
    
    { Helper: Adds a float parameter
      @param ShortFlag Short form flag (e.g., '-r')
      @param LongFlag Long form flag (e.g., '--rate')
      @param Description Parameter description
      @param Required Whether parameter is required
      @param DefaultValue Default value if not provided }
    procedure AddFloatParameter(const ShortFlag, LongFlag, Description: string;
      Required: Boolean = False; const DefaultValue: string = '');
    
    { Helper: Adds a file or directory path parameter
      @param ShortFlag Short form flag (e.g., '-i')
      @param LongFlag Long form flag (e.g., '--input')
      @param Description Parameter description
      @param Required Whether parameter is required
      @param DefaultValue Default value if not provided }
    procedure AddPathParameter(const ShortFlag, LongFlag, Description: string;
      Required: Boolean = False; const DefaultValue: string = '');
    
    { Helper: Adds an enumerated value parameter
      @param ShortFlag Short form flag (e.g., '-l')
      @param LongFlag Long form flag (e.g., '--log-level')
      @param Description Parameter description
      @param AllowedValues Pipe-separated list of allowed values (e.g., 'debug|info|warn|error')
      @param Required Whether parameter is required
      @param DefaultValue Default value if not provided }
    procedure AddEnumParameter(const ShortFlag, LongFlag, Description, AllowedValues: string;
      Required: Boolean = False; const DefaultValue: string = '');
    
    { Helper: Adds a date/time parameter
      @param ShortFlag Short form flag (e.g., '-d')
      @param LongFlag Long form flag (e.g., '--date')
      @param Description Parameter description
      @param Required Whether parameter is required
      @param DefaultValue Default value if not provided (format: YYYY-MM-DD HH:MM:SS) }
    procedure AddDateTimeParameter(const ShortFlag, LongFlag, Description: string;
      Required: Boolean = False; const DefaultValue: string = '');
    
    { Helper: Adds an array parameter for comma-separated values
      @param ShortFlag Short form flag (e.g., '-t')
      @param LongFlag Long form flag (e.g., '--tags')
      @param Description Parameter description
      @param Required Whether parameter is required
      @param DefaultValue Default value if not provided (comma-separated) }
    procedure AddArrayParameter(const ShortFlag, LongFlag, Description: string;
      Required: Boolean = False; const DefaultValue: string = '');
    
    { Helper: Adds a password parameter (value will be masked in help/logs)
      @param ShortFlag Short form flag (e.g., '-k')
      @param LongFlag Long form flag (e.g., '--api-key')
      @param Description Parameter description
      @param Required Whether parameter is required }
    procedure AddPasswordParameter(const ShortFlag, LongFlag, Description: string;
      Required: Boolean = False);
    
    { Helper: Adds a URL parameter with format validation
      @param ShortFlag Short form flag (e.g., '-u')
      @param LongFlag Long form flag (e.g., '--url')
      @param Description Parameter description
      @param Required Whether parameter is required
      @param DefaultValue Default value if not provided }
    procedure AddUrlParameter(const ShortFlag, LongFlag, Description: string;
      Required: Boolean = False; const DefaultValue: string = '');
    
    { Adds a subcommand to this command
      @param Command The subcommand to add }
    procedure AddSubCommand(const Command: ICommand);
    
    { Sets parsed parameter values from application
      @param Params StringList containing parsed parameter values }
    procedure SetParsedParams(const Params: TStringList);
    
    { Executes the command - must be implemented by concrete classes
      @returns Integer exit code (0 for success, non-zero for error) }
    function Execute: Integer; virtual; abstract;
    
    { Command name as used in CLI }
    property Name: string read GetName;
    
    { Command description for help text }
    property Description: string read GetDescription;
    
    { Array of command parameters }
    property Parameters: specialize TArray<ICommandParameter> read GetParameters;
    
    { Array of subcommands }
    property SubCommands: specialize TArray<ICommand> read GetSubCommands;
       

  end;

implementation

{ Constructor: Creates new command instance
  @param AName Command name as used in CLI
  @param ADescription Command description for help text }
constructor TBaseCommand.Create(const AName, ADescription: string);
begin
  inherited Create;
  FName := AName;
  FDescription := ADescription;
  SetLength(FParameters, 0);
  SetLength(FSubCommands, 0);
  FParsedParams := nil;  // Will be set by application
end;

{ Destructor: Cleans up command resources }
destructor TBaseCommand.Destroy;
begin
  SetLength(FParameters, 0);
  SetLength(FSubCommands, 0);
  // Don't free FParsedParams as it's owned by the application
  inherited;
end;

{ GetName: Returns command name
  @returns String containing command name }
function TBaseCommand.GetName: string;
begin
  Result := FName;
end;

{ GetDescription: Returns command description
  @returns String containing command description }
function TBaseCommand.GetDescription: string;
begin
  Result := FDescription;
end;

{ GetParameters: Returns array of command parameters
  @returns Array of ICommandParameter }
function TBaseCommand.GetParameters: specialize TArray<ICommandParameter>;
begin
  Result := FParameters;
end;

{ GetSubCommands: Returns array of subcommands
  @returns Array of ICommand }
function TBaseCommand.GetSubCommands: specialize TArray<ICommand>;
begin
  Result := FSubCommands;
end;

{ AddParameter: Adds a parameter to the command
  @param Parameter The parameter to add }
procedure TBaseCommand.AddParameter(const Parameter: ICommandParameter);
begin
  SetLength(FParameters, Length(FParameters) + 1);
  FParameters[High(FParameters)] := Parameter;
end;

{ AddParameter: Adds a parameter to the command with direct values
  @param ShortFlag Short form flag (e.g., '-n')
  @param LongFlag Long form flag (e.g., '--name')
  @param Description Parameter description for help
  @param Required Whether parameter is required
  @param ParamType Parameter data type
  @param DefaultValue Default value if not provided
  @param AllowedValues Pipe-separated list of allowed values (e.g., 'debug|info|warn|error') }
procedure TBaseCommand.AddParameter(const ShortFlag, LongFlag, Description: string;
  Required: Boolean; ParamType: TParameterType; const DefaultValue: string = '';
  const AllowedValues: string = '');
var
  Param: ICommandParameter;
begin
  Param := TCommandParameter.Create(ShortFlag, LongFlag, Description, Required,
    ParamType, DefaultValue, AllowedValues);
  AddParameter(Param);
end;

{ AddStringParameter: Helper to add a string parameter }
procedure TBaseCommand.AddStringParameter(const ShortFlag, LongFlag, Description: string;
  Required: Boolean = False; const DefaultValue: string = '');
begin
  AddParameter(ShortFlag, LongFlag, Description, Required, ptString, DefaultValue);
end;

{ AddIntegerParameter: Helper to add an integer parameter }
procedure TBaseCommand.AddIntegerParameter(const ShortFlag, LongFlag, Description: string;
  Required: Boolean = False; const DefaultValue: string = '');
begin
  AddParameter(ShortFlag, LongFlag, Description, Required, ptInteger, DefaultValue);
end;

{ AddFlag: Helper to add a boolean flag parameter }
procedure TBaseCommand.AddFlag(const ShortFlag, LongFlag, Description: string;
  const DefaultValue: string = 'false');
begin
  AddParameter(ShortFlag, LongFlag, Description, False, ptBoolean, DefaultValue);
end;

{ Adds a boolean parameter that requires explicit true/false value }
procedure TBaseCommand.AddBooleanParameter(const ShortFlag, LongFlag, Description: string;
  Required: Boolean; const DefaultValue: string);
begin
  AddParameter(ShortFlag, LongFlag, Description, Required, ptBoolean, DefaultValue);
end;

{ AddFloatParameter: Helper to add a float parameter }
procedure TBaseCommand.AddFloatParameter(const ShortFlag, LongFlag, Description: string;
  Required: Boolean = False; const DefaultValue: string = '');
begin
  AddParameter(ShortFlag, LongFlag, Description, Required, ptFloat, DefaultValue);
end;

{ AddPathParameter: Helper to add a file/directory path parameter }
procedure TBaseCommand.AddPathParameter(const ShortFlag, LongFlag, Description: string;
  Required: Boolean = False; const DefaultValue: string = '');
begin
  AddParameter(ShortFlag, LongFlag, Description, Required, ptPath, DefaultValue);
end;

{ AddEnumParameter: Helper to add an enumerated value parameter }
procedure TBaseCommand.AddEnumParameter(const ShortFlag, LongFlag, Description, AllowedValues: string;
  Required: Boolean = False; const DefaultValue: string = '');
var
  FullDescription: string;
begin
  FullDescription := Format('%s (allowed: %s)', [Description, AllowedValues]);
  AddParameter(ShortFlag, LongFlag, FullDescription, Required, ptEnum, DefaultValue, AllowedValues);
end;

{ AddDateTimeParameter: Helper to add a date/time parameter }
procedure TBaseCommand.AddDateTimeParameter(const ShortFlag, LongFlag, Description: string;
  Required: Boolean = False; const DefaultValue: string = '');
begin
  AddParameter(ShortFlag, LongFlag, Description + ' (format: YYYY-MM-DD HH:MM:SS)', Required, ptDateTime, DefaultValue);
end;

{ AddArrayParameter: Helper to add an array parameter }
procedure TBaseCommand.AddArrayParameter(const ShortFlag, LongFlag, Description: string;
  Required: Boolean = False; const DefaultValue: string = '');
begin
  AddParameter(ShortFlag, LongFlag, Description + ' (comma-separated)', Required, ptArray, DefaultValue);
end;

{ AddPasswordParameter: Helper to add a password parameter }
procedure TBaseCommand.AddPasswordParameter(const ShortFlag, LongFlag, Description: string;
  Required: Boolean = False);
begin
  AddParameter(ShortFlag, LongFlag, Description + ' (value will be masked)', Required, ptPassword, '');
end;

{ AddUrlParameter: Helper to add a URL parameter }
procedure TBaseCommand.AddUrlParameter(const ShortFlag, LongFlag, Description: string;
  Required: Boolean = False; const DefaultValue: string = '');
begin
  AddParameter(ShortFlag, LongFlag, Description + ' (must be a valid URL)', Required, ptUrl, DefaultValue);
end;

{ AddSubCommand: Adds a subcommand to this command
  @param Command The subcommand to add }
procedure TBaseCommand.AddSubCommand(const Command: ICommand);
begin
  SetLength(FSubCommands, Length(FSubCommands) + 1);
  FSubCommands[High(FSubCommands)] := Command;
end;

{ SetParsedParams: Sets parsed parameter values from application
  @param Params StringList containing parsed parameter values }
procedure TBaseCommand.SetParsedParams(const Params: TStringList);
begin
  FParsedParams := Params;
end;

{ GetParameterValue: Gets value of a parameter by flag
  @param Flag The parameter flag to look up (can be short or long form)
  @param Value Output parameter that receives the value if found
  @returns True if parameter has value (provided or default), False otherwise }
function TBaseCommand.GetParameterValue(const Flag: string; out Value: string): Boolean;
var
  Param: ICommandParameter;
  idx: Integer;
  paramVal: string;
begin
  Result := False;
  if not Assigned(FParsedParams) then
    Exit;

  // Find the parameter object for type info
  for Param in FParameters do
  begin
    if (Param.LongFlag = Flag) or (Param.ShortFlag = Flag) then
    begin
      // Special handling for boolean flags
      if Param.ParamType = ptBoolean then
      begin
        idx := FParsedParams.IndexOfName(Param.LongFlag);
        if idx = -1 then
          idx := FParsedParams.IndexOfName(Param.ShortFlag);
        if idx <> -1 then
        begin
          paramVal := FParsedParams.ValueFromIndex[idx];
          if (paramVal = '') then
          begin
            Value := 'true'; // flag present, no value
            Result := True;
            Exit;
          end
          else if SameText(paramVal, 'true') or SameText(paramVal, 'false') then
          begin
            Value := paramVal;
            Result := True;
            Exit;
          end
          else
          begin
            Value := paramVal;
            Result := True;
            Exit;
          end;
        end
        else if Param.DefaultValue <> '' then
        begin
          Value := Param.DefaultValue;
          Result := False;
          Exit;
        end
        else
        begin
          Value := 'false';
          Result := False;
          Exit;
        end;
      end;
      // Check both long and short flags in parsed parameters
      Value := FParsedParams.Values[Param.LongFlag];
      if Value = '' then
        Value := FParsedParams.Values[Param.ShortFlag];
      if Value <> '' then
        Exit(True)
      else if Param.DefaultValue <> '' then
      begin
        Value := Param.DefaultValue;
        Exit(True);
      end;
      Break;
    end;
  end;
end;

{ ShowHelp: Shows help text for this command
  Displays:
  - Command usage
  - Description
  - Available subcommands
  - Parameters with descriptions
  - Examples }
procedure TBaseCommand.ShowHelp;
var
  Param: ICommandParameter;
  SubCmd: ICommand;
  RequiredText: string;
  ExeName: string;
  CommandPath: string;
  i: Integer;
begin
  ExeName := ExtractFileName(ParamStr(0));
  
  // Build full command path
  CommandPath := '';
  for i := 1 to ParamCount do
  begin
    if StartsStr('-', ParamStr(i)) then
      Break;
    if CommandPath <> '' then
      CommandPath := CommandPath + ' ';
    CommandPath := CommandPath + ParamStr(i);
  end;
  if CommandPath = '' then
    CommandPath := Name;
  
  // Show usage and description
  TConsole.WriteLn('Usage: ' + ExeName + ' ' + CommandPath + ' [options]');
  TConsole.WriteLn('');
  TConsole.WriteLn(Description);
  
  // Show subcommands if any
  if Length(SubCommands) > 0 then
  begin
    TConsole.WriteLn('');
    TConsole.WriteLn('Commands:', ccCyan);
    for SubCmd in SubCommands do
      TConsole.WriteLn('  ' + PadRight(SubCmd.Name, 15) + SubCmd.Description);
      
    TConsole.WriteLn('');
    TConsole.WriteLn('Examples:', ccCyan);
    TConsole.WriteLn('  ' + ExeName + ' ' + CommandPath + ' <command> --help');
    TConsole.WriteLn('    Show help for a specific command');
    for SubCmd in SubCommands do
      TConsole.WriteLn('  ' + ExeName + ' ' + CommandPath + ' ' + SubCmd.Name + ' --help');
  end;
  
  // Show parameters if any
  if Length(Parameters) > 0 then
  begin
    TConsole.WriteLn('');
    TConsole.WriteLn('Options:', ccCyan);
    for Param in Parameters do
    begin
      if Param.Required then
        RequiredText := ' (required)'
      else
        RequiredText := '';
        
      TConsole.WriteLn('  ' + Param.ShortFlag + ', ' + PadRight(Param.LongFlag, 20) +
        Param.Description + RequiredText);
      
      if Param.DefaultValue <> '' then
        TConsole.WriteLn('      Default: ' + Param.DefaultValue);
    end;
  end;
end;



end.
