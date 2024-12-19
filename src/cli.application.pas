unit CLI.Application;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CLI.Interfaces;

type
  { Main CLI application class }
  TCLIApplication = class(TInterfacedObject, ICLIApplication)
  private
    FName: string;
    FVersion: string;
    FCommands: array of ICommand;
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
  public
    constructor Create(const AName, AVersion: string);
    destructor Destroy; override;
    
    procedure RegisterCommand(const Command: ICommand);
    function Execute: Integer;
    property DebugMode: Boolean read FDebugMode write FDebugMode;
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
  SetLength(FCommands, 0);
  FParsedParams := TStringList.Create;
  FParsedParams.CaseSensitive := True;
  FParamStartIndex := 2; // Skip program name and command name
  FDebugMode := False; // Debug output disabled by default
end;

destructor TCLIApplication.Destroy;
begin
  FParsedParams.Free;
  inherited;
end;

procedure TCLIApplication.RegisterCommand(const Command: ICommand);
begin
  SetLength(FCommands, Length(FCommands) + 1);
  FCommands[High(FCommands)] := Command;
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
  if ParamCount = 0 then
  begin
    ShowHelp;
    Exit;
  end;
  
  // Check for global help and version flags
  for i := 1 to ParamCount do
  begin
    if (ParamStr(i) = '-h') or (ParamStr(i) = '--help') then
    begin
      if i = 1 then
      begin
        ShowHelp;
        Exit;
      end;
      ShowHelpForCommand := True;
      Break;
    end;
    if (ParamStr(i) = '-v') or (ParamStr(i) = '--version') then
    begin
      if i = 1 then
      begin
        ShowVersion;
        Exit;
      end;
      Break;
    end;
  end;
  
  CmdName := ParamStr(1);
  if StartsStr('-', CmdName) then
  begin
    TConsole.WriteLn('Error: No command specified', ccRed);
    ShowHelp;
    Exit(1);
  end;
  
  // Find main command
  CurrentCmd := FindCommand(CmdName);
  
  if not Assigned(CurrentCmd) then
  begin
    TConsole.WriteLn('Error: Unknown command "' + CmdName + '"', ccRed);
    ShowHelp;
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
      Break;
  end;

  // Show help if requested or if command has subcommands and no subcommand specified
  if ShowHelpForCommand or 
     ((Length(FCurrentCommand.SubCommands) > 0) and (FParamStartIndex = 2)) then
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
  Cmd: ICommand;
begin
  Result := nil;
  for Cmd in FCommands do
    if SameText(Cmd.Name, Name) then
      Exit(Cmd);
end;

function TCLIApplication.ValidateCommand: Boolean;
var
  Param: ICommandParameter;
  Value: string;
  HasValue: Boolean;
begin
  Result := True;
  
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
  CmdList: array of ICommand;
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
    CmdList := FCommands;
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
    end;
    CmdList := Command.SubCommands;
  end;

  // Recursively show subcommands
  for Cmd in CmdList do
  begin
    if Command = nil then
      TConsole.WriteLn('');
    ShowCompleteHelp(Indent + '  ', Cmd);
  end;
  
  if (Command = nil) and (Length(FCommands) > 0) then
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

end.
