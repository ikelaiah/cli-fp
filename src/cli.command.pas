unit CLI.Command;

{$mode objfpc}{$H+}

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
begin
  Result := False;
  if not Assigned(FParsedParams) then
    Exit;

  // First try to get the value directly from parsed parameters
  Value := FParsedParams.Values[Flag];
  if Value <> '' then
    Exit(True);
    
  // If not found, try to find the parameter and check both its flags
  for Param in FParameters do
  begin
    if (Param.LongFlag = Flag) or (Param.ShortFlag = Flag) then
    begin
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
