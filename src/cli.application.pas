unit CLI.Application;

{$mode objfpc}{$H+}{$J-}

{ This unit implements the core CLI application functionality.
  It handles command registration, parameter parsing, help system,
  and command execution flow. }

interface

uses
  Classes, SysUtils, Generics.Collections, Generics.Defaults, CLI.Interfaces;

type
  { List type for storing registered commands }
  TCommandList = specialize TList<ICommand>;

  { TCLIApplication - Main application class that implements ICLIApplication
    Handles:
    - Command registration and management
    - Command-line parsing
    - Parameter validation
    - Help system
    - Command execution }
  TCLIApplication = class(TInterfacedObject, ICLIApplication)
  private
    FName: string;              // Application name
    FVersion: string;           // Application version
    FCommands: TCommandList;    // List of registered commands
    FCurrentCommand: ICommand;  // Currently executing command
    FParsedParams: TStringList; // Parsed command-line parameters
    FParamStartIndex: Integer;  // Index where command parameters start
    FDebugMode: Boolean;        // Debug output flag
    
    { Parses command-line arguments into FParsedParams
      Handles both --param=value and -p value formats }
    procedure ParseCommandLine;
    
    { Shows general help with command list and global options }
    procedure ShowHelp;
    
    { Shows application version }
    procedure ShowVersion;
    
    { Shows detailed help for a specific command
      @param Command The command to show help for }
    procedure ShowCommandHelp(const Command: ICommand);
    
    { Finds a command by name
      @param Name The command name to find
      @returns ICommand if found, nil if not found }
    function FindCommand(const Name: string): ICommand;
    
    { Validates current command parameters
      Checks required parameters and unknown flags
      @returns True if validation passes, False otherwise }
    function ValidateCommand: Boolean;
    
    { Gets parameter value for a command parameter
      @param Param The parameter to get value for
      @param Value Output parameter that receives the value
      @returns True if parameter has value, False otherwise }
    function GetParameterValue(const Param: ICommandParameter; out Value: string): Boolean;
    
    { Shows complete help for all commands
      @param Indent Current indentation level for formatting
      @param Command Current command being documented, nil for root level }
    procedure ShowCompleteHelp(const Indent: string = ''; const Command: ICommand = nil);
    
    { Gets the list of registered commands
      @returns TCommandList containing all registered commands }
    function GetCommands: TCommandList;
    
    { Shows brief help when errors occur }
    procedure ShowBriefHelp;
    
    { Gets list of valid parameter flags for current command
      @returns TStringList containing all valid flags }
    function GetValidParameterFlags: TStringList;
    
    { Validates a parameter value based on its type
      @param Param The parameter to validate
      @param Value The value to validate
      @returns True if validation passes, False if any check fails }
    function ValidateParameterValue(const Param: ICommandParameter; const Value: string): Boolean;
    
    { Outputs a Bash completion script for the application }
    procedure OutputBashCompletionScript;
  public
    { Creates a new CLI application instance
      @param AName Application name
      @param AVersion Application version }
    constructor Create(const AName, AVersion: string);
    
    { Cleans up application resources }
    destructor Destroy; override;
    
    { Registers a new command with the application
      @param Command The command to register
      @raises Exception if command with same name exists }
    procedure RegisterCommand(const Command: ICommand);
    
    { Executes the application
      Parses command line, validates parameters, and runs command
      @returns Integer exit code (0 for success, non-zero for error) }
    function Execute: Integer;
    
    { Debug mode flag - enables detailed output when true }
    property DebugMode: Boolean read FDebugMode write FDebugMode;
    
    { Application version string }
    property Version: string read FVersion;
    
    { List of registered commands }
    property Commands: TCommandList read GetCommands;
    
    { For testing purposes }
    property ParsedParams: TStringList read FParsedParams;
    property CurrentCommand: ICommand read FCurrentCommand write FCurrentCommand;
    
    { For testing validation }
    function TestValidateCommand: Boolean;
  end;

{ Helper function to create a new CLI application instance
  @param Name Application name
  @param Version Application version
  @returns ICLIApplication interface to the new instance }
function CreateCLIApplication(const Name, Version: string): ICLIApplication;

implementation

uses
  StrUtils, CLI.Command, CLI.Console;

{ Constructor: Initializes a new CLI application instance
  @param AName The name of the application
  @param AVersion The version string
  Note: Creates empty command list and parameter storage }
constructor TCLIApplication.Create(const AName, AVersion: string);
begin
  inherited Create;
  FName := AName;
  FVersion := AVersion;
  FCommands := TCommandList.Create;
  FParsedParams := TStringList.Create;
  FParsedParams.CaseSensitive := True;  // Parameters are case-sensitive
  FParamStartIndex := 2;                // Skip program name and command name
  FDebugMode := False;                  // Debug output disabled by default
end;

{ Destructor: Cleans up application resources
  Note: Ensures proper cleanup of command list and parameter storage }
destructor TCLIApplication.Destroy;
begin
  FCurrentCommand := nil;  // Release current command reference
  FCommands.Free;         // Free command list
  FParsedParams.Free;     // Free parameter storage
  inherited;
end;

{ RegisterCommand: Adds a new command to the application
  @param Command The command to register
  @raises Exception if a command with the same name already exists
  Note: Command names are case-insensitive for comparison }
procedure TCLIApplication.RegisterCommand(const Command: ICommand);
var
  i: Integer;
begin
  // Check for duplicate command names
  for i := 0 to FCommands.Count - 1 do
    if SameText(FCommands[i].Name, Command.Name) then
      raise Exception.CreateFmt('Command "%s" is already registered', [Command.Name]);

  FCommands.Add(Command);
end;

{ Execute: Main entry point for running the application
  Handles:
  - Parameter parsing
  - Command identification
  - Subcommand resolution
  - Help display
  - Command execution
  @returns Integer exit code (0 for success, non-zero for error) }
function TCLIApplication.Execute: Integer;
var
  CmdName: string;
  Command: TBaseCommand;
  SubCmd: ICommand;
  SubCmdName: string;
  i: Integer;
  CurrentCmd: ICommand;
  Cmd: ICommand;
  ShowHelpForCommand: Boolean;
begin
  Result := 0;
  ShowHelpForCommand := False;
  
  // Check for empty command line - show general help
  if ParamCount = 0 then
  begin
    ShowHelp;
    Exit;
  end;

  // Show general help if -h or --help is the only argument
  if (ParamCount = 1) and ((ParamStr(1) = '-h') or (ParamStr(1) = '--help')) then
  begin
    ShowHelp;
    Exit;
  end;

  // Show complete help if --help-complete is the only argument
  if (ParamCount = 1) and (ParamStr(1) = '--help-complete') then
  begin
    ShowCompleteHelp;
    Exit;
  end;

  // Handle global completion file flag
  if (ParamStr(1) = '--completion-file') then
  begin
    // Check if user is writing directly to .bashrc or .bash_profile (as argument)
    if (ParamCount > 1) and (
      (Pos('.bashrc', LowerCase(ParamStr(2))) > 0) or
      (Pos('.bash_profile', LowerCase(ParamStr(2))) > 0)
    ) then
    begin
      TConsole.WriteLn('⚠️  Warning: Do NOT write completion scripts directly to .bashrc or .bash_profile! Source them instead to avoid polluting your shell config.', ccYellow);
      TConsole.WriteLn('Example:');
      TConsole.WriteLn('  ./'+ExtractFileName(ParamStr(0))+' --completion-file > myapp-completion.sh');
      TConsole.WriteLn('  echo "source $(pwd)/myapp-completion.sh" >> ~/.bashrc');
      TConsole.WriteLn('');
    end;
    OutputBashCompletionScript;
    Exit;
  end;
  
  // Get and validate command name
  CmdName := ParamStr(1);
  if StartsStr('-', CmdName) then
  begin
    TConsole.WriteLn('Error: No command specified', ccRed);
    ShowBriefHelp;
    Exit(1);
  end;
  
  // Find main command
  CurrentCmd := FindCommand(CmdName);
  
  if not Assigned(CurrentCmd) then
  begin
    TConsole.WriteLn('Error: Unknown command "' + CmdName + '"', ccRed);
    ShowBriefHelp;
    Exit(1);
  end;
  
  FCurrentCommand := CurrentCmd;
  
  // Process subcommands if present
  i := 2;
  while (i <= ParamCount) and not StartsStr('-', ParamStr(i)) do
  begin
    SubCmdName := ParamStr(i);
    SubCmd := nil;
    
    // Search for matching subcommand
    for Cmd in CurrentCmd.SubCommands do
    begin
      if SameText(Cmd.Name, SubCmdName) then
      begin
        SubCmd := Cmd;
        Break;
      end;
    end;
    
    if Assigned(SubCmd) then
    begin
      CurrentCmd := SubCmd;
      FCurrentCommand := SubCmd;
      Inc(FParamStartIndex);
      Inc(i);
    end
    else
    begin
      // Show available subcommands on error
      TConsole.WriteLn('Error: Unknown subcommand "' + SubCmdName + '" for ' + CurrentCmd.Name, ccRed);
      TConsole.WriteLn('');
      TConsole.WriteLn('Available subcommands:', ccCyan);
      for Cmd in CurrentCmd.SubCommands do
        TConsole.WriteLn('  ' + PadRight(Cmd.Name, 15) + Cmd.Description);
      TConsole.WriteLn('');
      TConsole.WriteLn('Use "' + ExtractFileName(ParamStr(0)) + ' ' + 
        CurrentCmd.Name + ' --help" for more information.');
      Exit(1);
    end;
  end;

  // Check for help request for current command
  for i := FParamStartIndex to ParamCount do
  begin
    if (ParamStr(i) = '-h') or (ParamStr(i) = '--help') then
    begin
      ShowCommandHelp(FCurrentCommand);
      Exit;
    end;
  end;

  // Show help for commands with subcommands when no subcommand specified
  if (Length(FCurrentCommand.SubCommands) > 0) and (FParamStartIndex = 2) then
  begin
    ShowCommandHelp(FCurrentCommand);
    Exit;
  end;
  
  // Parse command line arguments
  ParseCommandLine;
  
  // Set up command for execution
  Command := FCurrentCommand as TBaseCommand;
  Command.SetParsedParams(FParsedParams);
  
  // Validate command parameters
  if not ValidateCommand then
    Exit(1);
    
  // Execute the command with error handling
  try
    Result := FCurrentCommand.Execute;
  except
    on E: Exception do
    begin
      TConsole.WriteLn('Error executing command: ' + E.Message, ccRed);
      Result := 1;
    end;
  end;
end;

{ ParseCommandLine: Processes command line arguments into parameter dictionary
  Handles:
  - Long format (--param=value)
  - Long format with space (--param value)
  - Short format (-p value)
  - Boolean flags (--flag)
  Note: Updates FParsedParams with parsed values }
procedure TCLIApplication.ParseCommandLine;
var
  i: Integer;
  Param, Value: string;
begin
  FParsedParams.Clear;
  i := FParamStartIndex; // Start after program name and command name(s)

  if FDebugMode then
    TConsole.WriteLn('Parsing command line...', ccCyan);

  while i <= ParamCount do
  begin
    Param := ParamStr(i);
    if FDebugMode then
      TConsole.WriteLn('Processing argument ' + IntToStr(i) + ': ' + Param, ccCyan);

    // Handle --param=value format
    if StartsStr('--', Param) then
    begin
      Value := '';
      if Pos('=', Param) > 0 then
      begin
        Value := Copy(Param, Pos('=', Param) + 1, Length(Param));
        Param := Copy(Param, 1, Pos('=', Param) - 1);
      end
      else if (i < ParamCount) and not StartsStr('-', ParamStr(i + 1)) then
      begin
        Value := ParamStr(i + 1);
        Inc(i);
      end;
      // Store flag with empty string if no value is provided
      FParsedParams.Values[Param] := Value;
      if FDebugMode then
        TConsole.WriteLn('  Added: ' + Param + ' = ' + Value, ccCyan);
    end
    // Handle -p value format
    else if StartsStr('-', Param) then
    begin
      if (i < ParamCount) and not StartsStr('-', ParamStr(i + 1)) then
      begin
        Value := ParamStr(i + 1);
        Inc(i);
      end
      else
        Value := '';
      // Store flag with empty string if no value is provided
      FParsedParams.Values[Param] := Value;
      if FDebugMode then
        TConsole.WriteLn('  Added: ' + Param + ' = ' + Value, ccCyan);
    end;

    Inc(i);
  end;

  if FDebugMode then
  begin
    TConsole.WriteLn('Parsed parameters:', ccCyan);
    for i := 0 to FParsedParams.Count - 1 do
    begin
      TConsole.WriteLn('  ' + FParsedParams.Names[i] + ' = ' + FParsedParams.ValueFromIndex[i], ccCyan);
    end;
  end;
end;

{ FindCommand: Searches for a command by name
  @param Name The command name to find
  @returns ICommand if found, nil if not found
  Note: Command names are case-insensitive }
function TCLIApplication.FindCommand(const Name: string): ICommand;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to FCommands.Count - 1 do
    if SameText(FCommands[i].Name, Name) then
      Exit(FCommands[i]);
end;

{ GetValidParameterFlags: Creates list of valid parameter flags
  @returns TStringList containing all valid parameter flags
  Note: Includes both command-specific and global flags }
function TCLIApplication.GetValidParameterFlags: TStringList;
var
  Param: ICommandParameter;
begin
  Result := TStringList.Create;
  Result.CaseSensitive := True;
  
  // Add command-specific parameter flags
  for Param in FCurrentCommand.Parameters do
  begin
    Result.Add(Param.LongFlag);
    Result.Add(Param.ShortFlag);
  end;
  
  // Add global flags
  Result.Add('--help');
  Result.Add('-h');
  Result.Add('--version');
  Result.Add('-v');
end;

{ ValidateCommand: Checks if all parameters are valid
  Verifies:
  - All parameters are recognized
  - Required parameters are provided
  - Parameter values are present when needed
  @returns True if validation passes, False if any check fails }
function TCLIApplication.ValidateCommand: Boolean;
var
  Param: ICommandParameter;
  Value: string;
  HasValue: Boolean;
  ValidFlags: TStringList;
  i: Integer;
  Flag: string;
begin
  Result := True;
  ValidFlags := GetValidParameterFlags;
  try
    // Check for unknown parameters
    for i := 0 to FParsedParams.Count - 1 do
    begin
      Flag := FParsedParams.Names[i];
      if (Flag <> '') and (ValidFlags.IndexOf(Flag) = -1) then
      begin
        TConsole.WriteLn('Error: Unknown parameter "' + Flag + '"', ccRed);
        ShowCommandHelp(FCurrentCommand);
        Exit(False);
      end;
    end;

    // Validate required parameters and their values
    for Param in FCurrentCommand.Parameters do
    begin
      // Check both long and short flags
      HasValue := GetParameterValue(Param, Value);
      
      if Param.Required and not HasValue then
      begin
        TConsole.WriteLn('Error: Required parameter "' + Param.LongFlag + '" not provided', ccRed);
        ShowCommandHelp(FCurrentCommand);
        Exit(False);
      end;
      
      if HasValue and not ValidateParameterValue(Param, Value) then
      begin
        ShowCommandHelp(FCurrentCommand);
        Exit(False);
      end;
    end;
  finally
    ValidFlags.Free;
  end;
end;

{ GetParameterValue: Retrieves value for a parameter
  @param Param The parameter to get value for
  @param Value Output parameter that receives the value
  @returns True if parameter has value (provided or default), False otherwise
  Note: Checks both long and short forms of the parameter }
function TCLIApplication.GetParameterValue(const Param: ICommandParameter; 
  out Value: string): Boolean;
var
  idx: Integer;
  paramVal: string;
begin
  // Special handling for boolean flags (ptBoolean)
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
      Result := False; // Not present on command line
      Exit;
    end
    else
    begin
      Value := 'false';
      Result := False;
      Exit;
    end;
  end;

  Result := FParsedParams.Values[Param.LongFlag] <> '';
  if Result then
    Value := FParsedParams.Values[Param.LongFlag]
  else
  begin
    Result := FParsedParams.Values[Param.ShortFlag] <> '';
    if Result then
      Value := FParsedParams.Values[Param.ShortFlag]
    else if Param.DefaultValue <> '' then
    begin
      Value := Param.DefaultValue;
      Result := True;
    end;
  end;
end;

{ ShowHelp: Displays general application help
  Shows:
  - Application name and version
  - Basic usage
  - Available commands
  - Global options
  - Usage examples }
procedure TCLIApplication.ShowHelp;
var
  Cmd: ICommand;
begin
  // Program header
  TConsole.WriteLn(FName + ' version ' + FVersion);
  TConsole.WriteLn('');

  // Basic usage
  TConsole.WriteLn('Usage:', ccCyan);
  TConsole.WriteLn('  ' + ExtractFileName(ParamStr(0)) + ' <command> [options]');
  TConsole.WriteLn('');

  // Available commands
  TConsole.WriteLn('Commands:', ccCyan);
  for Cmd in FCommands do
    TConsole.WriteLn('  ' + PadRight(Cmd.Name, 15) + Cmd.Description);
  TConsole.WriteLn('');

  // Global options
  TConsole.WriteLn('Global Options:', ccCyan);
  TConsole.WriteLn('  -h, --help           Show this help message');
  TConsole.WriteLn('  --help-complete      Show complete reference for all commands');
  TConsole.WriteLn('  -v, --version        Show version information');
  TConsole.WriteLn('');

  // Examples section
  TConsole.WriteLn('Examples:', ccCyan);
  TConsole.WriteLn('  Get help for commands:');
  TConsole.WriteLn('    ' + ExtractFileName(ParamStr(0)) + ' <command> --help');
  TConsole.WriteLn('');
  TConsole.WriteLn('  Available command help:');
  for Cmd in FCommands do
    TConsole.WriteLn('    ' + ExtractFileName(ParamStr(0)) + ' ' + Cmd.Name + ' --help');
  TConsole.WriteLn('');
end;

{ ShowCommandHelp: Displays detailed help for a specific command
  @param Command The command to show help for
  Shows:
  - Command usage
  - Description
  - Available subcommands
  - Command parameters
  - Parameter defaults
  - Usage examples }
procedure TCLIApplication.ShowCommandHelp(const Command: ICommand);
var
  Param: ICommandParameter;
  RequiredText: string;
  CommandPath: string;
  i: Integer;
  SubCmd: ICommand;
begin
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
    CommandPath := Command.Name;

  // Show usage and description
  TConsole.WriteLn('Usage: ' + ExtractFileName(ParamStr(0)) + ' ' + CommandPath + ' [options]');
  TConsole.WriteLn('');
  TConsole.WriteLn(Command.Description);
  
  // List subcommands if any
  if Length(Command.SubCommands) > 0 then
  begin
    TConsole.WriteLn('');
    TConsole.WriteLn('Commands:', ccCyan);
    for SubCmd in Command.SubCommands do
      TConsole.WriteLn('  ' + PadRight(SubCmd.Name, 15) + SubCmd.Description);
  end;
  
  // Show parameters if any
  if Length(Command.Parameters) > 0 then
  begin
    TConsole.WriteLn('');
    TConsole.WriteLn('Options:', ccCyan);
    for Param in Command.Parameters do
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

  // Show examples for commands with subcommands
  if Length(Command.SubCommands) > 0 then
  begin
    TConsole.WriteLn('');
    TConsole.WriteLn('Examples:', ccCyan);
    TConsole.WriteLn('  Get help for commands:');
    TConsole.WriteLn('    ' + ExtractFileName(ParamStr(0)) + ' ' + CommandPath + ' <command> --help');
    TConsole.WriteLn('');
    TConsole.WriteLn('  Available command help:');
    for SubCmd in Command.SubCommands do
      TConsole.WriteLn('    ' + ExtractFileName(ParamStr(0)) + ' ' + CommandPath + ' ' + SubCmd.Name + ' --help');
    TConsole.WriteLn('');
  end;
end;

{ ShowVersion: Displays application version }
procedure TCLIApplication.ShowVersion;
begin
  TConsole.WriteLn(FName + ' version ' + FVersion);
end;

{ ShowCompleteHelp: Displays complete help for all commands
  @param Indent Current indentation level for formatting
  @param Command Current command being documented, nil for root level
  Shows:
  - Full application description
  - Global options
  - All commands with full details
  - All subcommands recursively
  - All parameters with defaults }
procedure TCLIApplication.ShowCompleteHelp(const Indent: string = ''; const Command: ICommand = nil);
var
  Cmd: ICommand;
  Param: ICommandParameter;
  RequiredText: string;
  i: Integer;
begin
  if Command = nil then
  begin
    // Show program header and global information
    TConsole.WriteLn(FName + ' version ' + FVersion);
    TConsole.WriteLn('');
    TConsole.WriteLn('DESCRIPTION', ccCyan);
    TConsole.WriteLn('  Complete reference for all commands and options');
    TConsole.WriteLn('');
    TConsole.WriteLn('GLOBAL OPTIONS', ccCyan);
    TConsole.WriteLn('  -h, --help           Show command help');
    TConsole.WriteLn('  --help-complete      Show this complete reference');
    TConsole.WriteLn('  -v, --version        Show version information');
    TConsole.WriteLn('');
    TConsole.WriteLn('COMMANDS', ccCyan);
    
    // Show all commands recursively
    for i := 0 to FCommands.Count - 1 do
    begin
      if i > 0 then
        TConsole.WriteLn('');
      ShowCompleteHelp(Indent + '  ', FCommands[i]);
    end;
  end
  else
  begin
    // Show command details
    TConsole.WriteLn(Indent + Command.Name + ' - ' + Command.Description);
    
    // Show command parameters
    if Length(Command.Parameters) > 0 then
    begin
      TConsole.WriteLn('');
      TConsole.WriteLn(Indent + 'OPTIONS:', ccCyan);
      for Param in Command.Parameters do
      begin
        if Param.Required then
          RequiredText := ' (required)'
        else
          RequiredText := '';
          
        TConsole.WriteLn(Indent + '  ' + Param.ShortFlag + ', ' + 
          PadRight(Param.LongFlag, 20) + Param.Description + RequiredText);
        
        if Param.DefaultValue <> '' then
          TConsole.WriteLn(Indent + '      Default: ' + Param.DefaultValue);
      end;
    end;
    
    // Show subcommands recursively
    if Length(Command.SubCommands) > 0 then
    begin
      TConsole.WriteLn('');
      TConsole.WriteLn(Indent + 'SUBCOMMANDS:', ccCyan);
      for Cmd in Command.SubCommands do
      begin
        ShowCompleteHelp(Indent + '  ', Cmd);
        TConsole.WriteLn(''); // Add a blank line after each subcommand for clarity
      end;
    end;
  end;
  
  // Show help usage hint at root level
  if (Command = nil) and (FCommands.Count > 0) then
  begin
    TConsole.WriteLn('');
    TConsole.WriteLn('For more details on a specific command, use:');
    TConsole.WriteLn('  ' + ExtractFileName(ParamStr(0)) + ' <command> --help');
  end;
end;

{ GetCommands: Returns the list of registered commands
  @returns TCommandList containing all registered commands
  Note: Returns direct reference to command list }
function TCLIApplication.GetCommands: TCommandList;
begin
  Result := FCommands;
end;

{ ShowBriefHelp: Displays brief help for error cases
  Shows:
  - Basic usage
  - Available commands
  - Help command reminder }
procedure TCLIApplication.ShowBriefHelp;
var
  Cmd: ICommand;
begin
  // Show minimal help for error cases
  TConsole.WriteLn('Usage: ' + ExtractFileName(ParamStr(0)) + ' <command> [options]');
  TConsole.WriteLn('');
  TConsole.WriteLn('Commands:', ccCyan);
  for Cmd in FCommands do
    TConsole.WriteLn('  ' + PadRight(Cmd.Name, 15) + Cmd.Description);
  TConsole.WriteLn('');
  TConsole.WriteLn('Use --help for more information.');
end;

{ CreateCLIApplication: Factory function to create new CLI application
  @param Name Application name
  @param Version Application version string
  @returns ICLIApplication interface to new application instance }
function CreateCLIApplication(const Name, Version: string): ICLIApplication;
begin
  Result := TCLIApplication.Create(Name, Version);
end;

{ Validates a parameter value based on its type
  @param Param The parameter to validate
  @param Value The value to validate
  @returns True if validation passes, False if any check fails }
function TCLIApplication.ValidateParameterValue(const Param: ICommandParameter; const Value: string): Boolean;
var
  IntValue: Integer;
  FloatValue: Double;
  AllowedValues: TStringList;
  i: Integer;
  DateTimeValue: TDateTime;
begin
  Result := True;
  
  case Param.ParamType of
    ptInteger:
      if not TryStrToInt(Value, IntValue) then
      begin
        TConsole.WriteLn(Format('Error: Parameter "%s" must be an integer', [Param.LongFlag]), ccRed);
        Result := False;
      end;
      
    ptFloat:
      if not TryStrToFloat(Value, FloatValue) then
      begin
        TConsole.WriteLn(Format('Error: Parameter "%s" must be a float', [Param.LongFlag]), ccRed);
        Result := False;
      end;
      
    ptBoolean:
      if not (SameText(Value, 'true') or SameText(Value, 'false')) then
      begin
        TConsole.WriteLn(Format('Error: Parameter "%s" must be "true" or "false"', [Param.LongFlag]), ccRed);
        Result := False;
      end;
      
    ptUrl:
      if not (StartsStr('http://', Value) or StartsStr('https://', Value) or
             StartsStr('git://', Value) or StartsStr('ssh://', Value)) then
      begin
        TConsole.WriteLn(Format('Error: Parameter "%s" must be a valid URL starting with http://, https://, git://, or ssh://',
          [Param.LongFlag]), ccRed);
        Result := False;
      end;

    ptEnum:
      begin
        if Param.AllowedValues = '' then
          Exit;
          
        AllowedValues := TStringList.Create;
        try
          AllowedValues.Delimiter := '|';
          AllowedValues.DelimitedText := Param.AllowedValues;
          
          Result := False;
          for i := 0 to AllowedValues.Count - 1 do
            if SameText(Value, AllowedValues[i]) then
            begin
              Result := True;
              Break;
            end;
            
          if not Result then
            TConsole.WriteLn(Format('Error: Parameter "%s" must be one of: %s',
              [Param.LongFlag, Param.AllowedValues]), ccRed);
        finally
          AllowedValues.Free;
        end;
      end;
    
    ptDateTime:
      begin
        FormatSettings.DateSeparator := '-';
        FormatSettings.ShortDateFormat := 'yyyy-mm-dd';
        FormatSettings.LongTimeFormat := 'HH:nn';
        
        if not TryStrToDateTime(Value, DateTimeValue) then
        begin
          TConsole.WriteLn(Format('Error: Parameter "%s" must be in format YYYY-MM-DD HH:MM',
            [Param.LongFlag]), ccRed);
          Result := False;
        end;
      end;
  end;
end;

{ TestValidateCommand: Public wrapper for ValidateCommand for testing }
function TCLIApplication.TestValidateCommand: Boolean;
begin
  Result := ValidateCommand;
end;

{ OutputBashCompletionScript: Outputs a Bash completion script for the application }
procedure TCLIApplication.OutputBashCompletionScript;
  procedure OutputBashTree(const Cmd: ICommand; const Path: string);
  var
    Sub: ICommand;
    Param: ICommandParameter;
    SubNames, ParamFlags: string;
  begin
    // Output subcommands for this path
    SubNames := '';
    for Sub in Cmd.SubCommands do
    begin
      if SubNames <> '' then SubNames := SubNames + ' ';
      SubNames := SubNames + Sub.Name;
    end;
    // Output parameters for this path
    ParamFlags := '';
    for Param in Cmd.Parameters do
    begin
      if ParamFlags <> '' then ParamFlags := ParamFlags + ' ';
      ParamFlags := ParamFlags + Param.LongFlag;
      if Param.ShortFlag <> '' then
        ParamFlags := ParamFlags + ' ' + Param.ShortFlag;
    end;
    // Only add -h and --help as global flags for non-root nodes
    if ParamFlags <> '' then
      ParamFlags := ParamFlags + ' ';
    ParamFlags := ParamFlags + '--help -h';
    // Output Bash associative arrays for this path (no leading spaces)
    TConsole.WriteLn('tree["' + Path + '|subcommands"]="' + SubNames + '"');
    TConsole.WriteLn('tree["' + Path + '|params"]="' + ParamFlags + '"');
    // Recurse for subcommands
    for Sub in Cmd.SubCommands do
      OutputBashTree(Sub, Path + ' ' + Sub.Name);
  end;
var
  Cmd: ICommand;
  BashFunc, AppName, RootSubNames, RootParamFlags: string;
begin
  AppName := ExtractFileName(ParamStr(0));
  BashFunc := '_' + LowerCase(FName) + '_completions';

  TConsole.WriteLn('#!/bin/bash');
  TConsole.WriteLn('declare -A tree');

  // Output the root (empty path) entry for top-level completions
  RootSubNames := '';
  RootParamFlags := '';
  for Cmd in FCommands do
  begin
    if RootSubNames <> '' then RootSubNames := RootSubNames + ' ';
    RootSubNames := RootSubNames + Cmd.Name;
  end;
  // Add global flags for root only
  RootParamFlags := '--help --help-complete --version --completion-file -h';
  // Use a special root key for Bash associative array (no leading spaces)
  TConsole.WriteLn('tree["__root__|subcommands"]="' + RootSubNames + '"');
  TConsole.WriteLn('tree["__root__|params"]="' + RootParamFlags + '"');

  // Output the command tree
  for Cmd in FCommands do
    OutputBashTree(Cmd, Cmd.Name);

  TConsole.WriteLn('');
  TConsole.WriteLn(BashFunc+'()');
  TConsole.WriteLn('{');
  TConsole.WriteLn('  local cur words cword path subcmds params i');
  TConsole.WriteLn('  # DEBUG: Print function call and COMP_WORDS');
  TConsole.WriteLn('  # Uncomment for debugging:');
  TConsole.WriteLn('  # echo "[DEBUG] Called: $FUNCNAME, COMP_WORDS=(\"${COMP_WORDS[@]}\") COMP_CWORD=$COMP_CWORD" >&2');
  TConsole.WriteLn('  cur="${COMP_WORDS[COMP_CWORD]}"');
  TConsole.WriteLn('  words=("${COMP_WORDS[@]}")');
  TConsole.WriteLn('  cword=$COMP_CWORD');
  TConsole.WriteLn('  # Determine path and index');
  TConsole.WriteLn('  if [[ $cword -eq 1 ]]; then');
  TConsole.WriteLn('    path="__root__"');
  TConsole.WriteLn('    i=1');
  TConsole.WriteLn('  else');
  TConsole.WriteLn('    path="${words[1]}"');
  TConsole.WriteLn('    i=2');
  TConsole.WriteLn('    while [[ $i -le $cword ]]; do');
  TConsole.WriteLn('      subcmds="${tree[$path|subcommands]}"');
  TConsole.WriteLn('      found=0');
  TConsole.WriteLn('      for sub in $subcmds; do');
  TConsole.WriteLn('        if [[ "${words[$i]}" == "$sub" ]]; then');
  TConsole.WriteLn('          path="$path $sub"');
  TConsole.WriteLn('          found=1');
  TConsole.WriteLn('          break');
  TConsole.WriteLn('        fi');
  TConsole.WriteLn('      done');
  TConsole.WriteLn('      if [[ $found -eq 0 ]]; then break; fi');
  TConsole.WriteLn('      ((i++))');
  TConsole.WriteLn('    done');
  TConsole.WriteLn('  fi');
  TConsole.WriteLn('  subcmds="${tree[$path|subcommands]}"');
  TConsole.WriteLn('  params="${tree[$path|params]}"');
  TConsole.WriteLn('  # DEBUG: Print path, subcmds, params, i, cword');
  TConsole.WriteLn('  # Uncomment for debugging:');
  TConsole.WriteLn('  # echo "[DEBUG] path=[$path] subcmds=[$subcmds] params=[$params] i=$i cword=$cword cur=[$cur]" >&2');
  TConsole.WriteLn('  if [[ -n "$subcmds" && $i -eq $cword ]]; then');
  TConsole.WriteLn('    COMPREPLY=( $(compgen -W "$subcmds $params" -- "$cur") )');
  TConsole.WriteLn('  else');
  TConsole.WriteLn('    COMPREPLY=( $(compgen -W "$params" -- "$cur") )');
  TConsole.WriteLn('  fi');
  TConsole.WriteLn('  return 0');
  TConsole.WriteLn('}');
  TConsole.WriteLn('complete -F '+BashFunc+' '+AppName);
  TConsole.WriteLn('complete -F '+BashFunc+' ./'+AppName);
end;

end.
