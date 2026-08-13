unit CLI.Application;

{$mode objfpc}{$H+}{$J-}

{ This unit implements the core CLI application functionality.
  It handles command registration, parameter parsing, help system,
  and command execution flow. }

interface

uses
  Classes, SysUtils, Generics.Collections, Generics.Defaults, CLI.Interfaces,
  CLI.Console;

type
  { List type for storing registered commands }
  TCommandList = specialize TList<ICommand>;

  { Completion types and helpers }
  TStringArray = array of string;

  { Function types for completion callbacks (compatible with FPC without anonymous functions) }
  TFlagValueCompletionFunc = function (Args: TStringArray; ToComplete: string): TStringArray;
  TPositionalCompletionFunc = function (Args: TStringArray; ToComplete: string): TStringArray;

  { Simple storage record for completion callbacks - separate types to avoid nil pointer issues }
  TFlagCompletionEntry = record
    Key: string;
    Callback: TFlagValueCompletionFunc;
  end;
  TFlagCompletionList = array of TFlagCompletionEntry;

  TPosCompletionEntry = record
    Key: string;
    Callback: TPositionalCompletionFunc;
  end;
  TPosCompletionList = array of TPosCompletionEntry;

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
    FRootCommand: ICommand;     // Optional command executed without a name
    FCommands: TCommandList;    // List of registered commands
    FCurrentCommand: ICommand;  // Currently executing command
    FParsedParams: TStringList; // Parsed command-line parameters
    FParamStartIndex: Integer;  // Index where command parameters start
    FDebugMode: Boolean;        // Debug output flag
    FArguments: TStringArray;   // Current arguments, excluding the executable
    {$IFDEF CLI_FP_TESTING}
    FOutputCapture: TStrings;   // Exists only in framework test builds
    {$ENDIF}

    { Writes help output to the console, or to the active test capture. }
    procedure WriteOutput(const Text: string); overload;
    procedure WriteOutput(const Text: string; const Color: TConsoleColor); overload;
    procedure WriteHelpLine(const Text: string; const Color: TConsoleColor;
      const UseColor: Boolean);
    function CommandSnapshot: specialize TArray<ICommand>;

    { Parses command-line arguments into FParsedParams
      Handles both --param=value and -p value formats }
    procedure ParseCommandLine;

    { Loads the process command line into FArguments. }
    procedure LoadProcessArguments;

    { Returns the current number of arguments. }
    function ArgumentCount: Integer;

    { Returns a one-based argument, or an empty string when out of range. }
    function ArgumentAt(const Index: Integer): string;

    { Executes using the arguments already stored in FArguments. }
    function ExecuteArguments: Integer;

    { Handles requests that do not select or execute a command. }
    function HandleGlobalRequest: Boolean;

    { Selects the root or named command and resolves its subcommand path. }
    function SelectCurrentCommand: Boolean;

    { Shows requested or implicit command help. }
    function HandleCurrentCommandHelp: Boolean;

    { Parses, validates, and executes FCurrentCommand. }
    function ExecuteCurrentCommand: Integer;
    
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

    { Returns True when Candidate is a signed numeric value accepted by Param. }
    function IsNegativeNumericValue(const Param: string;
      const Candidate: string): Boolean;

    { Shows complete help for all commands. }
    procedure ShowCompleteHelp;
    
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
    
    { Outputs a PowerShell completion script for the application }
    procedure OutputPowerShellCompletionScript;

    { Hidden completion entrypoint handler (invoked when first arg is '__complete') }
    procedure HandleCompletion;

    { Internal completion implementation }
    function DoComplete(const Tokens: TStringArray): TStringList;
  public
    { Creates a new CLI application instance
      @param AName Application name
      @param AVersion Application version }
    constructor Create(const AName, AVersion: string;
      const ARootCommand: ICommand = nil);
    
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

    { Optional command executed when no named command is supplied }
    property RootCommand: ICommand read FRootCommand;
    
    { List of registered commands }
    property Commands: TCommandList read GetCommands;
    
    { For testing purposes }
    property ParsedParams: TStringList read FParsedParams;
    property CurrentCommand: ICommand read FCurrentCommand write FCurrentCommand;

    { Deprecated no-op callback registration retained for 1.x source
      compatibility. Built-in metadata completion is unaffected. }
    procedure RegisterFlagValueCompletion(const CommandPath, FlagName: string;
      Func: TFlagValueCompletionFunc);
      deprecated 'Custom completion callbacks are non-functional and will be removed in v2.0.0';
    procedure RegisterPositionalCompletion(const CommandPath: string;
      ArgIndex: Integer; Func: TPositionalCompletionFunc);
      deprecated 'Custom completion callbacks are non-functional and will be removed in v2.0.0';
    
    { For testing validation }
    function TestValidateCommand: Boolean;

    { For testing completions: returns list of lines (candidates + final ":<directive>") }
    function TestComplete(const Tokens: TStringArray): TStringList;

    { For testing execution without changing the process command line. }
    function TestExecute(const Args: TStringArray): Integer;

    {$IFDEF CLI_FP_TESTING}
    { Captures output from the normal execution path for framework tests only. }
    function TestExecuteAndCapture(const Args: TStringArray;
      const Output: TStrings): Integer;
    {$ENDIF}
  end;

const
  { Completion directive bits (Cobra-like) }
  CD_ERROR = 1;
  CD_NOSPACE = 2;
  CD_NOFILE = 4;
  CD_KEEPORDER = 8;

{ Helper function to create a new CLI application instance
  @param Name Application name
  @param Version Application version
  @returns ICLIApplication interface to the new instance }
function CreateCLIApplication(const Name, Version: string): ICLIApplication; overload;

{ Creates an application with an optional executable root command.
  The root command's name is not part of the command line. }
function CreateCLIApplication(const Name, Version: string;
  const RootCommand: ICommand): ICLIApplication; overload;

implementation

uses
  StrUtils, CLI.Internal.ParameterValues, CLI.Internal.Help,
  CLI.Internal.Completion;

{ Constructor: Initializes a new CLI application instance
  @param AName The name of the application
  @param AVersion The version string
  Note: Creates empty command list and parameter storage }
constructor TCLIApplication.Create(const AName, AVersion: string;
  const ARootCommand: ICommand);
begin
  inherited Create;
  FName := AName;
  FVersion := AVersion;
  FRootCommand := ARootCommand;
  FCommands := TCommandList.Create;
  FParsedParams := TStringList.Create;
  FParsedParams.CaseSensitive := True;  // Parameters are case-sensitive
  FParamStartIndex := 2;                // Skip program name and command name
  FDebugMode := False;                  // Debug output disabled by default
  {$IFDEF CLI_FP_TESTING}
  FOutputCapture := nil;
  {$ENDIF}
  SetLength(FArguments, 0);

  // Completion registries are auto-initialized as empty dynamic arrays
end;

procedure TCLIApplication.WriteOutput(const Text: string);
begin
  {$IFDEF CLI_FP_TESTING}
  if Assigned(FOutputCapture) then
  begin
    FOutputCapture.Add(Text);
    Exit;
  end;
  {$ENDIF}
  TConsole.WriteLn(Text);
end;

procedure TCLIApplication.WriteOutput(const Text: string;
  const Color: TConsoleColor);
begin
  {$IFDEF CLI_FP_TESTING}
  if Assigned(FOutputCapture) then
  begin
    FOutputCapture.Add(Text);
    Exit;
  end;
  {$ENDIF}
  TConsole.WriteLn(Text, Color);
end;

procedure TCLIApplication.WriteHelpLine(const Text: string;
  const Color: TConsoleColor; const UseColor: Boolean);
begin
  if UseColor then
    WriteOutput(Text, Color)
  else
    WriteOutput(Text);
end;

function TCLIApplication.CommandSnapshot: specialize TArray<ICommand>;
var
  i: Integer;
begin
  Result := nil;
  SetLength(Result, FCommands.Count);
  for i := 0 to FCommands.Count - 1 do
    Result[i] := FCommands[i];
end;

{ Destructor: Cleans up application resources
  Note: Ensures proper cleanup of command list and parameter storage }
destructor TCLIApplication.Destroy;
begin
  FCurrentCommand := nil;  // Release current command reference
  FRootCommand := nil;     // Release optional root command reference
  FCommands.Free;         // Free command list
  FParsedParams.Free;     // Free parameter storage
  // NOTE: Dynamic arrays with function pointers cause issues in FPC during cleanup
  // The arrays will be auto-freed when the object is destroyed
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

{ Loads the process arguments without the executable name. }
procedure TCLIApplication.LoadProcessArguments;
var
  i: Integer;
begin
  SetLength(FArguments, ParamCount);
  for i := 1 to ParamCount do
    FArguments[i - 1] := ParamStr(i);
end;

function TCLIApplication.ArgumentCount: Integer;
begin
  Result := Length(FArguments);
end;

function TCLIApplication.ArgumentAt(const Index: Integer): string;
begin
  if (Index < 1) or (Index > ArgumentCount) then
    Exit('');
  Result := FArguments[Index - 1];
end;

function TCLIApplication.Execute: Integer;
begin
  LoadProcessArguments;
  Result := ExecuteArguments;
end;

{ Handles completion, help, version, and shell-script requests. }
function TCLIApplication.HandleGlobalRequest: Boolean;
begin
  Result := True;

  if ArgumentAt(1) = '__complete' then
  begin
    HandleCompletion;
    Exit;
  end;

  if (ArgumentCount = 1) and
    ((ArgumentAt(1) = '-h') or (ArgumentAt(1) = '--help')) then
  begin
    ShowHelp;
    Exit;
  end;

  if (ArgumentCount = 1) and (ArgumentAt(1) = '--help-complete') then
  begin
    ShowCompleteHelp;
    Exit;
  end;

  if (ArgumentCount = 1) and
    ((ArgumentAt(1) = '-v') or (ArgumentAt(1) = '--version')) then
  begin
    ShowVersion;
    Exit;
  end;

  if ArgumentAt(1) = '--completion-file' then
  begin
    if (ArgumentCount > 1) and
      ((Pos('.bashrc', LowerCase(ArgumentAt(2))) > 0) or
       (Pos('.bash_profile', LowerCase(ArgumentAt(2))) > 0)) then
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

  if ArgumentAt(1) = '--completion-file-pwsh' then
  begin
    TConsole.WriteLn('# Usage: ./' + ExtractFileName(ParamStr(0)) + ' --completion-file-pwsh > myapp-completion.ps1');
    TConsole.WriteLn('# Then in PowerShell:');
    TConsole.WriteLn('#   . ./myapp-completion.ps1');
    TConsole.WriteLn('# To make it permanent, add the above line to your $PROFILE');
    OutputPowerShellCompletionScript;
    Exit;
  end;

  Result := False;
end;

function TCLIApplication.SelectCurrentCommand: Boolean;
var
  CmdName, SubCmdName: string;
  CurrentCmd, SubCmd, Cmd: ICommand;
  i: Integer;
begin
  Result := False;
  CmdName := ArgumentAt(1);

  if StartsStr('-', CmdName) then
  begin
    if not Assigned(FRootCommand) then
    begin
      TConsole.WriteLn('Error: No command specified', ccRed);
      ShowBriefHelp;
      Exit;
    end;
    FCurrentCommand := FRootCommand;
    FParamStartIndex := 1;
    Exit(True);
  end;

  CurrentCmd := FindCommand(CmdName);
  if not Assigned(CurrentCmd) then
  begin
    TConsole.WriteLn('Error: Unknown command "' + CmdName + '"', ccRed);
    ShowBriefHelp;
    Exit;
  end;

  FCurrentCommand := CurrentCmd;
  i := 2;
  while (i <= ArgumentCount) and not StartsStr('-', ArgumentAt(i)) do
  begin
    SubCmdName := ArgumentAt(i);
    SubCmd := nil;
    for Cmd in CurrentCmd.SubCommands do
      if SameText(Cmd.Name, SubCmdName) then
      begin
        SubCmd := Cmd;
        Break;
      end;

    if not Assigned(SubCmd) then
    begin
      TConsole.WriteLn('Error: Unknown subcommand "' + SubCmdName +
        '" for ' + CurrentCmd.Name, ccRed);
      TConsole.WriteLn('');
      TConsole.WriteLn('Available subcommands:', ccCyan);
      for Cmd in CurrentCmd.SubCommands do
        TConsole.WriteLn('  ' + PadRight(Cmd.Name, 15) + Cmd.Description);
      TConsole.WriteLn('');
      TConsole.WriteLn('Use "' + ExtractFileName(ParamStr(0)) + ' ' +
        CurrentCmd.Name + ' --help" for more information.');
      Exit;
    end;

    CurrentCmd := SubCmd;
    FCurrentCommand := SubCmd;
    Inc(FParamStartIndex);
    Inc(i);
  end;
  Result := True;
end;

function TCLIApplication.HandleCurrentCommandHelp: Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := FParamStartIndex to ArgumentCount do
    if (ArgumentAt(i) = '-h') or (ArgumentAt(i) = '--help') then
    begin
      if FCurrentCommand = FRootCommand then
        ShowHelp
      else
        ShowCommandHelp(FCurrentCommand);
      Exit;
    end;

  if (FCurrentCommand <> FRootCommand) and
    (Length(FCurrentCommand.SubCommands) > 0) and (FParamStartIndex = 2) then
  begin
    ShowCommandHelp(FCurrentCommand);
    Exit;
  end;
  Result := False;
end;

function TCLIApplication.ExecuteArguments: Integer;
begin
  Result := 0;
  FCurrentCommand := nil;
  FParsedParams.Clear;
  FParamStartIndex := 2;

  if ArgumentCount = 0 then
  begin
    if not Assigned(FRootCommand) then
    begin
      ShowHelp;
      Exit;
    end;
    FCurrentCommand := FRootCommand;
    FParamStartIndex := 1;
    Exit(ExecuteCurrentCommand);
  end;

  if HandleGlobalRequest then
    Exit;
  if not SelectCurrentCommand then
    Exit(1);
  if HandleCurrentCommandHelp then
    Exit;
  Result := ExecuteCurrentCommand;
end;

{ Parses, validates, and executes the selected command. }
function TCLIApplication.ExecuteCurrentCommand: Integer;
var
  ParameterReceiver: ICommandParameterReceiver;
begin
  ParseCommandLine;

  if Supports(FCurrentCommand, ICommandParameterReceiver, ParameterReceiver) then
    ParameterReceiver.SetParsedParams(FParsedParams);

  if not ValidateCommand then
    Exit(1);

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
    WriteOutput('Parsing command line...', ccCyan);

  while i <= ArgumentCount do
  begin
    Param := ArgumentAt(i);
    if FDebugMode then
      WriteOutput('Processing argument ' + IntToStr(i) + ': ' +
        RedactArgument(FCurrentCommand, Param), ccCyan);

    // Handle --param=value format
    if StartsStr('--', Param) then
    begin
      Value := '';
      if Pos('=', Param) > 0 then
      begin
        Value := Copy(Param, Pos('=', Param) + 1, Length(Param));
        Param := Copy(Param, 1, Pos('=', Param) - 1);
      end
      else if (i < ArgumentCount) and
        (not StartsStr('-', ArgumentAt(i + 1)) or
         IsNegativeNumericValue(Param, ArgumentAt(i + 1))) then
      begin
        Value := ArgumentAt(i + 1);
        Inc(i);
      end;
      // Store flag with empty string if no value is provided
      FParsedParams.Values[Param] := Value;
      if FDebugMode then
        WriteOutput('  Added: ' + Param + ' = ' +
          RedactParameterValue(FCurrentCommand, Param, Value), ccCyan);
    end
    // Handle -p value format
    else if StartsStr('-', Param) then
    begin
      if (i < ArgumentCount) and
        (not StartsStr('-', ArgumentAt(i + 1)) or
         IsNegativeNumericValue(Param, ArgumentAt(i + 1))) then
      begin
        Value := ArgumentAt(i + 1);
        Inc(i);
      end
      else
        Value := '';
      // Store flag with empty string if no value is provided
      FParsedParams.Values[Param] := Value;
      if FDebugMode then
        WriteOutput('  Added: ' + Param + ' = ' +
          RedactParameterValue(FCurrentCommand, Param, Value), ccCyan);
    end;

    Inc(i);
  end;

  if FDebugMode then
  begin
    WriteOutput('Parsed parameters:', ccCyan);
    for i := 0 to FParsedParams.Count - 1 do
    begin
      WriteOutput('  ' + FParsedParams.Names[i] + ' = ' +
        RedactParameterValue(FCurrentCommand, FParsedParams.Names[i],
          FParsedParams.ValueFromIndex[i]),
        ccCyan);
    end;
  end;
end;

function TCLIApplication.IsNegativeNumericValue(const Param: string;
  const Candidate: string): Boolean;
var
  CommandParam: ICommandParameter;
  IntValue: Integer;
  FloatValue: Double;
begin
  Result := False;
  if not StartsStr('-', Candidate) then
    Exit;

  CommandParam := FindParameterByFlag(FCurrentCommand, Param);
  if not Assigned(CommandParam) then
    Exit;

  case CommandParam.ParamType of
    ptInteger:
      Result := TryStrToInt(Candidate, IntValue);
    ptFloat:
      Result := TryStrToFloat(Candidate, FloatValue);
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
begin
  Result := TryGetParameterValue(Param, FParsedParams, Value);
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
  Renderer: TCLIHelpRenderer;
begin
  Renderer := TCLIHelpRenderer.Create(FName, FVersion,
    ExtractFileName(ParamStr(0)), FRootCommand, CommandSnapshot,
    @WriteHelpLine);
  try
    Renderer.ShowGeneral;
  finally
    Renderer.Free;
  end;
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
  CommandPath: string;
  i: Integer;
  Renderer: TCLIHelpRenderer;
begin
  if Assigned(FRootCommand) and (Command = FRootCommand) then
  begin
    ShowHelp;
    Exit;
  end;

  // Build full command path
  CommandPath := '';
  for i := 1 to ArgumentCount do
  begin
    if StartsStr('-', ArgumentAt(i)) then
      Break;
    if CommandPath <> '' then
      CommandPath := CommandPath + ' ';
    CommandPath := CommandPath + ArgumentAt(i);
  end;
  if CommandPath = '' then
    CommandPath := Command.Name;

  Renderer := TCLIHelpRenderer.Create(FName, FVersion,
    ExtractFileName(ParamStr(0)), FRootCommand, CommandSnapshot,
    @WriteHelpLine);
  try
    Renderer.ShowCommand(Command, CommandPath);
  finally
    Renderer.Free;
  end;
end;

{ ShowVersion: Displays application version }
procedure TCLIApplication.ShowVersion;
begin
  WriteOutput(FName + ' version ' + FVersion);
end;

{ ShowCompleteHelp: Displays complete help for all commands
  Shows:
  - Full application description
  - Global options
  - All commands with full details
  - All subcommands recursively
  - All parameters with defaults }
procedure TCLIApplication.ShowCompleteHelp;
var
  Renderer: TCLIHelpRenderer;
begin
  Renderer := TCLIHelpRenderer.Create(FName, FVersion,
    ExtractFileName(ParamStr(0)), FRootCommand, CommandSnapshot,
    @WriteHelpLine);
  try
    Renderer.ShowComplete;
  finally
    Renderer.Free;
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
  Renderer: TCLIHelpRenderer;
begin
  Renderer := TCLIHelpRenderer.Create(FName, FVersion,
    ExtractFileName(ParamStr(0)), FRootCommand, CommandSnapshot,
    @WriteHelpLine);
  try
    Renderer.ShowBrief;
  finally
    Renderer.Free;
  end;
end;

{ CreateCLIApplication: Factory function to create new CLI application
  @param Name Application name
  @param Version Application version string
  @returns ICLIApplication interface to new application instance }
function CreateCLIApplication(const Name, Version: string): ICLIApplication;
begin
  Result := TCLIApplication.Create(Name, Version);
end;

function CreateCLIApplication(const Name, Version: string;
  const RootCommand: ICommand): ICLIApplication;
begin
  Result := TCLIApplication.Create(Name, Version, RootCommand);
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
    ptString,   
    ptPath,     
    ptArray,    
    ptPassword: ; // no validation 
  end;
end;

{ TestValidateCommand: Public wrapper for ValidateCommand for testing }
function TCLIApplication.TestValidateCommand: Boolean;
begin
  Result := ValidateCommand;
end;

{ RegisterFlagValueCompletion: Register a callback for flag value completion }
procedure TCLIApplication.RegisterFlagValueCompletion(const CommandPath, FlagName: string; Func: TFlagValueCompletionFunc);
begin
  // Deprecated no-op retained for 1.x source compatibility.
end;

{ RegisterPositionalCompletion: Register a callback for positional argument completion }
procedure TCLIApplication.RegisterPositionalCompletion(const CommandPath: string; ArgIndex: Integer; Func: TPositionalCompletionFunc);
begin
  // Deprecated no-op retained for 1.x source compatibility.
end;

function TCLIApplication.DoComplete(const Tokens: TStringArray): TStringList;
begin
  Result := CompleteCLI(Tokens, FRootCommand, CommandSnapshot);
end;

procedure TCLIApplication.HandleCompletion;
var
  i: Integer;
  Tokens: TStringArray;
  outList: TStringList;
begin
  // Build tokens after the hidden __complete argument.
  Tokens := nil;
  SetLength(Tokens, ArgumentCount - 1);
  for i := 0 to ArgumentCount - 2 do
    Tokens[i] := ArgumentAt(i + 2);

  outList := DoComplete(Tokens);
  try
    for i := 0 to outList.Count - 1 do
      TConsole.WriteLn(outList[i]);
  finally
    outList.Free;
  end;
end;

{ TestComplete: helper for unit tests. Returns a list of candidates with final directive line }
function TCLIApplication.TestComplete(const Tokens: TStringArray): TStringList;
begin
  Result := DoComplete(Tokens);
end;

function TCLIApplication.TestExecute(const Args: TStringArray): Integer;
var
  i: Integer;
begin
  SetLength(FArguments, Length(Args));
  for i := 0 to Length(Args) - 1 do
    FArguments[i] := Args[i];
  Result := ExecuteArguments;
end;

{$IFDEF CLI_FP_TESTING}
function TCLIApplication.TestExecuteAndCapture(const Args: TStringArray;
  const Output: TStrings): Integer;
begin
  if not Assigned(Output) then
    raise EArgumentNilException.Create('Output capture cannot be nil');

  FOutputCapture := Output;
  try
    Result := TestExecute(Args);
  finally
    FOutputCapture := nil;
  end;
end;
{$ENDIF}

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
  Param: ICommandParameter;
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
  if Assigned(FRootCommand) then
  begin
    for Param in FRootCommand.Parameters do
    begin
      if RootParamFlags <> '' then
        RootParamFlags := RootParamFlags + ' ';
      RootParamFlags := RootParamFlags + Param.LongFlag;
      if Param.ShortFlag <> '' then
        RootParamFlags := RootParamFlags + ' ' + Param.ShortFlag;
    end;
  end;
  if RootParamFlags <> '' then
    RootParamFlags := RootParamFlags + ' ';
  // Add global flags for root only
  RootParamFlags := RootParamFlags +
    '--help --help-complete --version --completion-file --completion-file-pwsh -h -v';
  // Use a special root key for Bash associative array (no leading spaces)
  TConsole.WriteLn('tree["__root__|subcommands"]="' + RootSubNames + '"');
  TConsole.WriteLn('tree["__root__|params"]="' + RootParamFlags + '"');

  // Output the command tree
  for Cmd in FCommands do
    OutputBashTree(Cmd, Cmd.Name);

  TConsole.WriteLn('');
  TConsole.WriteLn(BashFunc+'()');
  TConsole.WriteLn('{');
  TConsole.WriteLn('  local cur words cword args out dir candidates');
  if FDebugMode then
  begin
    TConsole.WriteLn('  # DEBUG: Print function call and COMP_WORDS');
    TConsole.WriteLn('  echo "[DEBUG] Called: $FUNCNAME, COMP_WORDS=(\"${COMP_WORDS[@]}\") COMP_CWORD=$COMP_CWORD" >&2');
  end;
  TConsole.WriteLn('  cur="${COMP_WORDS[COMP_CWORD]}"');
  TConsole.WriteLn('  words=("${COMP_WORDS[@]}")');
  TConsole.WriteLn('  cword=$COMP_CWORD');
  TConsole.WriteLn('  # Build args for __complete and call the application');
  TConsole.WriteLn('  args=()');
  TConsole.WriteLn('  for ((i=1;i<cword;i++)); do args+=("${words[i]}"); done');
  TConsole.WriteLn('  # If cursor is after a space, append empty token to indicate new word');
  TConsole.WriteLn('  if [[ "${COMP_LINE: -1}" == " " ]]; then');
  TConsole.WriteLn('    args+=("")');
  TConsole.WriteLn('  else');
  TConsole.WriteLn('    args+=("${words[cword]}")');
  TConsole.WriteLn('  fi');
  TConsole.WriteLn('  out=$("./'+AppName+'" __complete "${args[@]}")');
  TConsole.WriteLn('  # Last line is directive in form :<number>');
  TConsole.WriteLn('  dir="$(printf "%s\n" "$out" | tail -n1)"');
  TConsole.WriteLn('  if [[ $dir =~ ^:([0-9]+)$ ]]; then');
  TConsole.WriteLn('    candidates="$(printf "%s\n" "$out" | sed ''$d'')"');
  TConsole.WriteLn('    directive=${BASH_REMATCH[1]}');
  TConsole.WriteLn('  else');
  TConsole.WriteLn('    candidates="$out"');
  TConsole.WriteLn('    directive=0');
  TConsole.WriteLn('  fi');
  if FDebugMode then
  begin
    TConsole.WriteLn('  # DEBUG: Print completion call information');
    TConsole.WriteLn('  echo "[DEBUG] args=(${args[@]}) out=\"$out\" directive=$directive cur=[$cur] candidates=[$candidates]" >&2');
  end;
  TConsole.WriteLn('  # Populate COMPREPLY with matching candidates');
  TConsole.WriteLn('  while IFS='''' read -r comp; do');
  TConsole.WriteLn('    [[ -z "$comp" ]] && continue');
  TConsole.WriteLn('    COMPREPLY+=("$comp")');
  TConsole.WriteLn('  done < <(compgen -W "$candidates" -- "$cur")');
  TConsole.WriteLn('  return 0');
  TConsole.WriteLn('}');
  TConsole.WriteLn('complete -F '+BashFunc+' '+AppName);
  TConsole.WriteLn('complete -F '+BashFunc+' ./'+AppName);
end;

{ OutputPowerShellCompletionScript: Outputs a PowerShell completion script for the application }
procedure TCLIApplication.OutputPowerShellCompletionScript;
var
  AppName: string;
begin
  AppName := ExtractFileName(ParamStr(0));
  TConsole.WriteLn('# Usage: ./' + AppName + ' --completion-file-pwsh > myapp-completion.ps1');
  TConsole.WriteLn('# Then in PowerShell:');
  TConsole.WriteLn('#   . ./myapp-completion.ps1');
  TConsole.WriteLn('# To make it permanent, add the above line to your $PROFILE');
  TConsole.WriteLn('# PowerShell argument completer for ' + AppName);
  TConsole.WriteLn('');
  TConsole.WriteLn('$scriptBlock = {');
  TConsole.WriteLn('  param($wordToComplete, $commandAst, $cursorPosition)');
  TConsole.WriteLn('  $line = $commandAst.ToString()');
  TConsole.WriteLn('  $words = $line -split " +" | Where-Object { $_ -ne '''' }');
  TConsole.WriteLn('  $argsList = @($words | Select-Object -Skip 1)');
  TConsole.WriteLn('  if ($line.EndsWith(" ")) { $argsList += "" }');
  TConsole.WriteLn('  $out = & "./' + AppName + '" __complete @argsList 2>$null');
  TConsole.WriteLn('  if (-not $out) { return @() }');
  TConsole.WriteLn('  # Extract directive and candidates');
  TConsole.WriteLn('  $directive = 0');
  TConsole.WriteLn('  $candidates = @()');
  TConsole.WriteLn('  foreach ($line in $out) {');
  TConsole.WriteLn('    if ($line -match "^:([0-9]+)$") {');
  TConsole.WriteLn('      $directive = [int]$Matches[1]');
  TConsole.WriteLn('    } else {');
  TConsole.WriteLn('      $candidates += $line');
  TConsole.WriteLn('    }');
  TConsole.WriteLn('  }');
  TConsole.WriteLn('  $results = @()');
  TConsole.WriteLn('  if ($candidates.Count -eq 0) { return @() }');
  TConsole.WriteLn('  foreach ($c in $candidates) {');
  TConsole.WriteLn('    # Skip empty candidates');
  TConsole.WriteLn('    if ([string]::IsNullOrWhiteSpace($c)) { continue }');
  TConsole.WriteLn('    # Filter by prefix');
  TConsole.WriteLn('    if ([string]::IsNullOrEmpty($wordToComplete) -or $c.StartsWith($wordToComplete, [StringComparison]::CurrentCultureIgnoreCase)) {');
  TConsole.WriteLn('      if (($directive -band 2) -ne 0) {');
  TConsole.WriteLn('        $results += [System.Management.Automation.CompletionResult]::new($c, $c, "ParameterName", $c)');
  TConsole.WriteLn('      } else {');
  TConsole.WriteLn('        $results += [System.Management.Automation.CompletionResult]::new($c, $c, "ParameterValue", $c)');
  TConsole.WriteLn('      }');
  TConsole.WriteLn('    }');
  TConsole.WriteLn('  }');
  TConsole.WriteLn('  return $results');
  TConsole.WriteLn('}');
  TConsole.WriteLn('');
  TConsole.WriteLn('# Register for all common invocation patterns');
  TConsole.WriteLn('Register-ArgumentCompleter -CommandName "' + AppName + '" -ScriptBlock $scriptBlock');
  TConsole.WriteLn('Register-ArgumentCompleter -CommandName "' + ChangeFileExt(AppName, '') + '" -ScriptBlock $scriptBlock');
  TConsole.WriteLn('Register-ArgumentCompleter -CommandName "./' + AppName + '" -ScriptBlock $scriptBlock');
  TConsole.WriteLn('Register-ArgumentCompleter -CommandName ".\' + AppName + '" -ScriptBlock $scriptBlock');
  TConsole.WriteLn('Register-ArgumentCompleter -CommandName ".\\' + AppName + '" -ScriptBlock $scriptBlock');
  TConsole.WriteLn('');
  TConsole.WriteLn('# Try -Native flag if PowerShell 7+');
  TConsole.WriteLn('if ($PSVersionTable.PSVersion.Major -ge 7) {');
  TConsole.WriteLn('  Register-ArgumentCompleter -Native -CommandName "' + ChangeFileExt(AppName, '') + '" -ScriptBlock {');
  TConsole.WriteLn('    param($wordToComplete, $commandAst, $cursorPosition)');
  TConsole.WriteLn('    & $scriptBlock $wordToComplete $commandAst $cursorPosition');
  TConsole.WriteLn('  }');
  TConsole.WriteLn('}');
end;

// To enable: add a CLI flag (e.g. --completion-file-pwsh) to call OutputPowerShellCompletionScript.
end.
