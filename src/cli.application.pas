unit CLI.Application;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections, Generics.Defaults, CLI.Interfaces;

type
  TCommandList = specialize TList<ICommand>;

  { Main CLI application class }
  TCLIApplication = class(TInterfacedObject, ICLIApplication)
  private
    FName: string;
    FVersion: string;
    FCommands: TCommandList;
    FCurrentCommand: ICommand;
    FParsedParams: TStringList;
    FParamStartIndex: Integer;
    FDebugMode: Boolean;
    
    procedure ParseCommandLine;
    procedure ShowHelp;
    procedure ShowVersion;
    procedure ShowCommandHelp(const Command: ICommand);
    function FindCommand(const Name: string): ICommand;
    function ValidateCommand: Boolean;
    function GetParameterValue(const Param: ICommandParameter; out Value: string): Boolean;
    procedure ShowCompleteHelp(const Indent: string = ''; const Command: ICommand = nil);
    function GetCommands: TCommandList;
    procedure ShowBriefHelp;
    function GetValidParameterFlags: TStringList;
  public
    constructor Create(const AName, AVersion: string);
    destructor Destroy; override;
    
    procedure RegisterCommand(const Command: ICommand);
    function Execute: Integer;
    property DebugMode: Boolean read FDebugMode write FDebugMode;
    property Version: string read FVersion;
    property Commands: TCommandList read GetCommands;
  end;

{ Helper function to create CLI application }
function CreateCLIApplication(const Name, Version: string): ICLIApplication;

implementation

uses
  StrUtils, CLI.Command, CLI.Console;

constructor TCLIApplication.Create(const AName, AVersion: string);
begin
  inherited Create;
  FName := AName;
  FVersion := AVersion;
  FCommands := TCommandList.Create;
  FParsedParams := TStringList.Create;
  FParsedParams.CaseSensitive := True;
  FParamStartIndex := 2; // Skip program name and command name
  FDebugMode := False; // Debug output disabled by default
end;

destructor TCLIApplication.Destroy;
begin
  FCurrentCommand := nil;
  FCommands.Free;
  FParsedParams.Free;
  inherited;
end;

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
  
  // Check for empty command line
  if ParamCount = 0 then
  begin
    ShowHelp;
    Exit;
  end;
  
  // Check for global flags first
  if (ParamStr(1) = '--help-complete') then
  begin
    ShowCompleteHelp;
    Exit;
  end;
  
  // Get command name (first argument)
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
  
  // Check for subcommand
  i := 2;
  while (i <= ParamCount) and not StartsStr('-', ParamStr(i)) do
  begin
    SubCmdName := ParamStr(i);
    SubCmd := nil;
    
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
      // Unknown subcommand
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

  // Check for help flag
  for i := FParamStartIndex to ParamCount do
  begin
    if (ParamStr(i) = '-h') or (ParamStr(i) = '--help') then
    begin
      ShowCommandHelp(FCurrentCommand);
      Exit;
    end;
  end;

  // Show help if command has subcommands and no subcommand specified
  if (Length(FCurrentCommand.SubCommands) > 0) and (FParamStartIndex = 2) then
  begin
    ShowCommandHelp(FCurrentCommand);
    Exit;
  end;
  
  // Parse command line arguments
  ParseCommandLine;
  
  // Pass parsed parameters to the command
  Command := FCurrentCommand as TBaseCommand;
  Command.SetParsedParams(FParsedParams);
  
  // Validate command parameters
  if not ValidateCommand then
    Exit(1);
    
  // Execute the command
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

function TCLIApplication.FindCommand(const Name: string): ICommand;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to FCommands.Count - 1 do
    if SameText(FCommands[i].Name, Name) then
      Exit(FCommands[i]);
end;

function TCLIApplication.GetValidParameterFlags: TStringList;
var
  Param: ICommandParameter;
begin
  Result := TStringList.Create;
  Result.CaseSensitive := True;
  
  // Add all valid parameter flags for the current command
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

    // Check required parameters
    for Param in FCurrentCommand.Parameters do
    begin
      if Param.Required then
      begin
        // Check both long and short flags
        HasValue := (FParsedParams.Values[Param.LongFlag] <> '') or
                   (FParsedParams.Values[Param.ShortFlag] <> '');
                   
        if not HasValue and (Param.DefaultValue = '') then
        begin
          TConsole.WriteLn('Error: Required parameter "' + Param.LongFlag + '" not provided', ccRed);
          ShowCommandHelp(FCurrentCommand);
          Exit(False);
        end;
      end;
    end;
  finally
    ValidFlags.Free;
  end;
end;

function TCLIApplication.GetParameterValue(const Param: ICommandParameter; 
  out Value: string): Boolean;
begin
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

  // Examples section with better formatting
  TConsole.WriteLn('Examples:', ccCyan);
  TConsole.WriteLn('  Get help for commands:');
  TConsole.WriteLn('    ' + ExtractFileName(ParamStr(0)) + ' <command> --help');
  TConsole.WriteLn('');
  TConsole.WriteLn('  Available command help:');
  for Cmd in FCommands do
    TConsole.WriteLn('    ' + ExtractFileName(ParamStr(0)) + ' ' + Cmd.Name + ' --help');
  TConsole.WriteLn('');
end;

procedure TCLIApplication.ShowCommandHelp(const Command: ICommand);
var
  Param: ICommandParameter;
  RequiredText: string;
  CommandPath: string;
  i: Integer;
  SubCmd: ICommand;
begin
  // Build command path from parameters
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

  // Basic usage and description
  TConsole.WriteLn('Usage: ' + ExtractFileName(ParamStr(0)) + ' ' + CommandPath + ' [options]');
  TConsole.WriteLn('');
  TConsole.WriteLn(Command.Description);
  
  // Show subcommands if any
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

  // Always show examples for commands with subcommands
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

procedure TCLIApplication.ShowVersion;
begin
  TConsole.WriteLn(FName + ' version ' + FVersion);
end;

procedure TCLIApplication.ShowCompleteHelp(const Indent: string = ''; const Command: ICommand = nil);
var
  Cmd: ICommand;
  Param: ICommandParameter;
  RequiredText: string;
  i: Integer;
begin
  if Command = nil then
  begin
    // Show program header
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
    
    // Show all commands
    for i := 0 to FCommands.Count - 1 do
    begin
      if i > 0 then
        TConsole.WriteLn('');
      ShowCompleteHelp(Indent + '  ', FCommands[i]);
    end;
  end
  else
  begin
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
    
    if Length(Command.SubCommands) > 0 then
    begin
      TConsole.WriteLn('');
      TConsole.WriteLn(Indent + 'SUBCOMMANDS:', ccCyan);
      for Cmd in Command.SubCommands do
        ShowCompleteHelp(Indent + '  ', Cmd);
    end;
  end;
  
  if (Command = nil) and (FCommands.Count > 0) then
  begin
    TConsole.WriteLn('');
    TConsole.WriteLn('For more details on a specific command, use:');
    TConsole.WriteLn('  ' + ExtractFileName(ParamStr(0)) + ' <command> --help');
  end;
end;

function CreateCLIApplication(const Name, Version: string): ICLIApplication;
begin
  Result := TCLIApplication.Create(Name, Version);
end;

function TCLIApplication.GetCommands: TCommandList;
begin
  // Return the actual commands list instead of creating a copy
  Result := FCommands;
end;

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

end.
