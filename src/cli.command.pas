unit CLI.Command;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, CLI.Interfaces, CLI.Parameter, CLI.Console;

type
  { Base command class }
  TBaseCommand = class(TInterfacedObject, ICommand)
  private
    FName: string;
    FDescription: string;
    FParameters: array of ICommandParameter;
    FSubCommands: array of ICommand;
    FParsedParams: TStringList;
  protected
    function GetName: string;
    function GetDescription: string;
    function GetParameters: specialize TArray<ICommandParameter>;
    function GetSubCommands: specialize TArray<ICommand>;
    function GetParameterValue(const Flag: string; out Value: string): Boolean;
    procedure ShowHelp;
  public
    constructor Create(const AName, ADescription: string);
    destructor Destroy; override;
    
    procedure AddParameter(const Parameter: ICommandParameter);
    procedure AddSubCommand(const Command: ICommand);
    procedure SetParsedParams(const Params: TStringList);
    function Execute: Integer; virtual; abstract;
    
    property Name: string read GetName;
    property Description: string read GetDescription;
    property Parameters: specialize TArray<ICommandParameter> read GetParameters;
    property SubCommands: specialize TArray<ICommand> read GetSubCommands;
  end;

implementation

constructor TBaseCommand.Create(const AName, ADescription: string);
begin
  inherited Create;
  FName := AName;
  FDescription := ADescription;
  SetLength(FParameters, 0);
  SetLength(FSubCommands, 0);
  FParsedParams := nil;
end;

destructor TBaseCommand.Destroy;
begin
  SetLength(FParameters, 0);
  SetLength(FSubCommands, 0);
  // Don't free FParsedParams as it's owned by the application
  inherited;
end;

function TBaseCommand.GetName: string;
begin
  Result := FName;
end;

function TBaseCommand.GetDescription: string;
begin
  Result := FDescription;
end;

function TBaseCommand.GetParameters: specialize TArray<ICommandParameter>;
begin
  Result := FParameters;
end;

function TBaseCommand.GetSubCommands: specialize TArray<ICommand>;
begin
  Result := FSubCommands;
end;

procedure TBaseCommand.AddParameter(const Parameter: ICommandParameter);
begin
  SetLength(FParameters, Length(FParameters) + 1);
  FParameters[High(FParameters)] := Parameter;
end;

procedure TBaseCommand.AddSubCommand(const Command: ICommand);
begin
  SetLength(FSubCommands, Length(FSubCommands) + 1);
  FSubCommands[High(FSubCommands)] := Command;
end;

procedure TBaseCommand.SetParsedParams(const Params: TStringList);
begin
  FParsedParams := Params;
end;

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
    CommandPath := Name;
  
  TConsole.WriteLn('Usage: ' + ExeName + ' ' + CommandPath + ' [options]');
  TConsole.WriteLn('');
  TConsole.WriteLn(Description);
  
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
