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
    
    procedure ParseCommandLine;
    procedure ShowHelp;
    procedure ShowVersion;
    procedure ShowCommandHelp(const Command: ICommand);
    function FindCommand(const Name: string): ICommand;
    function ValidateCommand: Boolean;
    function GetParameterValue(const Param: ICommandParameter; out Value: string): Boolean;
  public
    constructor Create(const AName, AVersion: string);
    destructor Destroy; override;
    
    procedure RegisterCommand(const Command: ICommand);
    function Execute: Integer;
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
begin
  Result := 0;
  
  // Parse command line arguments
  ParseCommandLine;
  
  // Check for help and version flags
  if (FParsedParams.IndexOf('-h') >= 0) or 
     (FParsedParams.IndexOf('--help') >= 0) then
  begin
    ShowHelp;
    Exit;
  end;
  
  if (FParsedParams.IndexOf('-v') >= 0) or 
     (FParsedParams.IndexOf('--version') >= 0) then
  begin
    ShowVersion;
    Exit;
  end;
  
  // Get command name (first argument)
  if ParamCount = 0 then
  begin
    TConsole.WriteLn('Error: No command specified', ccRed);
    ShowHelp;
    Exit(1);
  end;
  
  CmdName := ParamStr(1);
  FCurrentCommand := FindCommand(CmdName);
  
  if not Assigned(FCurrentCommand) then
  begin
    TConsole.WriteLn('Error: Unknown command "' + CmdName + '"', ccRed);
    ShowHelp;
    Exit(1);
  end;
  
  // Show command help if requested
  if (FParsedParams.IndexOf('-h') >= 0) or 
     (FParsedParams.IndexOf('--help') >= 0) then
  begin
    ShowCommandHelp(FCurrentCommand);
    Exit;
  end;
  
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
  i := 2; // Skip program name and command name
  
  TConsole.WriteLn('Parsing command line...', ccCyan);
  while i <= ParamCount do
  begin
    Param := ParamStr(i);
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
      TConsole.WriteLn('  Added: ' + Param + ' = ' + Value, ccCyan);
    end;
    
    Inc(i);
  end;
  
  TConsole.WriteLn('Parsed parameters:', ccCyan);
  for i := 0 to FParsedParams.Count - 1 do
  begin
    TConsole.WriteLn('  ' + FParsedParams.Names[i] + ' = ' + FParsedParams.ValueFromIndex[i], ccCyan);
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
begin
  Result := True;
  
  for Param in FCurrentCommand.Parameters do
  begin
    if Param.Required then
    begin
      if not GetParameterValue(Param, Value) then
      begin
        WriteLn('Error: Required parameter "', Param.LongFlag, '" not provided');
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
  TConsole.WriteLn(FName + ' version ' + FVersion);
  TConsole.WriteLn('');
  TConsole.WriteLn('Usage: ' + ExtractFileName(ParamStr(0)) + ' <command> [options]');
  TConsole.WriteLn('');
  TConsole.WriteLn('Commands:', ccCyan);
  for Cmd in FCommands do
    TConsole.WriteLn('  ' + PadRight(Cmd.Name, 15) + Cmd.Description);
  TConsole.WriteLn('');
  TConsole.WriteLn('Global Options:', ccCyan);
  TConsole.WriteLn('  -h, --help     Show this help message');
  TConsole.WriteLn('  -v, --version  Show version information');
  TConsole.WriteLn('');
  TConsole.WriteLn('Run ''' + ExtractFileName(ParamStr(0)) + ' <command> --help'' for more information about a command.');
end;

procedure TCLIApplication.ShowCommandHelp(const Command: ICommand);
var
  Param: ICommandParameter;
  RequiredText: string;
begin
  TConsole.WriteLn('Usage: ' + ExtractFileName(ParamStr(0)) + ' ' + Command.Name + ' [options]');
  TConsole.WriteLn('');
  TConsole.WriteLn(Command.Description);
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

procedure TCLIApplication.ShowVersion;
begin
  TConsole.WriteLn(FName + ' version ' + FVersion);
end;

function CreateCLIApplication(const Name, Version: string): ICLIApplication;
begin
  Result := TCLIApplication.Create(Name, Version);
end;

end.
