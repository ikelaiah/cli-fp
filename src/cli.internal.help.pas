unit CLI.Internal.Help;

{$mode objfpc}{$H+}{$J-}

interface

uses
  SysUtils, StrUtils, CLI.Interfaces, CLI.Console;

type
  TCommandArray = specialize TArray<ICommand>;
  TCLIHelpWriter = procedure(const Text: string; const Color: TConsoleColor;
    const UseColor: Boolean) of object;
  TCommandHelpStyle = (chsApplication, chsCommand);

  TCLIHelpRenderer = class
  private
    FName: string;
    FVersion: string;
    FExecutableName: string;
    FRootCommand: ICommand;
    FCommands: TCommandArray;
    FWriter: TCLIHelpWriter;
    procedure WriteLine(const Text: string); overload;
    procedure WriteLine(const Text: string;
      const Color: TConsoleColor); overload;
    procedure WriteParameters(const Parameters: array of ICommandParameter;
      const Indent: string);
    procedure ShowCommandExamples(const SubCommands: array of ICommand;
      const CommandPath: string; const Style: TCommandHelpStyle);
    procedure ShowCompleteCommand(const Command: ICommand;
      const Indent: string);
  public
    constructor Create(const AName, AVersion, AExecutableName: string;
      const ARootCommand: ICommand; const ACommands: TCommandArray;
      const AWriter: TCLIHelpWriter);
    procedure ShowGeneral;
    procedure ShowCommand(const Command: ICommand; const CommandPath: string;
      const Style: TCommandHelpStyle = chsApplication);
    procedure ShowCommandDetails(const Description: string;
      const Parameters: array of ICommandParameter;
      const SubCommands: array of ICommand; const CommandPath: string;
      const Style: TCommandHelpStyle = chsApplication);
    procedure ShowComplete;
    procedure ShowBrief;
  end;

implementation

constructor TCLIHelpRenderer.Create(const AName, AVersion,
  AExecutableName: string; const ARootCommand: ICommand;
  const ACommands: TCommandArray; const AWriter: TCLIHelpWriter);
begin
  inherited Create;
  FName := AName;
  FVersion := AVersion;
  FExecutableName := AExecutableName;
  FRootCommand := ARootCommand;
  FCommands := Copy(ACommands);
  FWriter := AWriter;
end;

procedure TCLIHelpRenderer.WriteLine(const Text: string);
begin
  FWriter(Text, ccWhite, False);
end;

procedure TCLIHelpRenderer.WriteLine(const Text: string;
  const Color: TConsoleColor);
begin
  FWriter(Text, Color, True);
end;

procedure TCLIHelpRenderer.WriteParameters(
  const Parameters: array of ICommandParameter; const Indent: string);
var
  Param: ICommandParameter;
  RequiredText: string;
begin
  for Param in Parameters do
  begin
    if Param.Required then
      RequiredText := ' (required)'
    else
      RequiredText := '';
    WriteLine(Indent + Param.ShortFlag + ', ' +
      PadRight(Param.LongFlag, 20) + Param.Description + RequiredText);
    if Param.DefaultValue <> '' then
      WriteLine(Indent + '    Default: ' + Param.DefaultValue);
  end;
end;

procedure TCLIHelpRenderer.ShowGeneral;
var
  Command: ICommand;
begin
  WriteLine(FName + ' version ' + FVersion);
  WriteLine('');
  WriteLine('Usage:', ccCyan);
  if Assigned(FRootCommand) then
  begin
    WriteLine('  ' + FExecutableName + ' [options]');
    if Length(FCommands) > 0 then
      WriteLine('  ' + FExecutableName + ' <command> [options]');
  end
  else
    WriteLine('  ' + FExecutableName + ' <command> [options]');
  WriteLine('');

  if Assigned(FRootCommand) and (FRootCommand.Description <> '') then
  begin
    WriteLine(FRootCommand.Description);
    WriteLine('');
  end;

  if Assigned(FRootCommand) and (Length(FRootCommand.Parameters) > 0) then
  begin
    WriteLine('Options:', ccCyan);
    WriteParameters(FRootCommand.Parameters, '  ');
    WriteLine('');
  end;

  if Length(FCommands) > 0 then
  begin
    WriteLine('Commands:', ccCyan);
    for Command in FCommands do
      WriteLine('  ' + PadRight(Command.Name, 15) + Command.Description);
    WriteLine('');
  end;

  WriteLine('Global Options:', ccCyan);
  WriteLine('  -h, --help           Show this help message');
  WriteLine('  --help-complete      Show complete reference for all commands');
  WriteLine('  --completion-file    Output Bash completion script (redirect to a file)');
  WriteLine('  --completion-file-pwsh  Output PowerShell completion script (redirect to a .ps1 file)');
  WriteLine('  -v, --version        Show version information');
  WriteLine('');

  if Length(FCommands) > 0 then
  begin
    WriteLine('Examples:', ccCyan);
    WriteLine('  Get help for commands:');
    WriteLine('    ' + FExecutableName + ' <command> --help');
    WriteLine('');
    WriteLine('  Available command help:');
    for Command in FCommands do
      WriteLine('    ' + FExecutableName + ' ' + Command.Name + ' --help');
    WriteLine('');
  end;
end;

procedure TCLIHelpRenderer.ShowCommand(const Command: ICommand;
  const CommandPath: string; const Style: TCommandHelpStyle);
begin
  ShowCommandDetails(Command.Description, Command.Parameters,
    Command.SubCommands, CommandPath, Style);
end;

procedure TCLIHelpRenderer.ShowCommandDetails(const Description: string;
  const Parameters: array of ICommandParameter;
  const SubCommands: array of ICommand; const CommandPath: string;
  const Style: TCommandHelpStyle);
var
  SubCommand: ICommand;
begin
  WriteLine('Usage: ' + FExecutableName + ' ' + CommandPath + ' [options]');
  WriteLine('');
  WriteLine(Description);

  if Length(SubCommands) > 0 then
  begin
    WriteLine('');
    WriteLine('Commands:', ccCyan);
    for SubCommand in SubCommands do
      WriteLine('  ' + PadRight(SubCommand.Name, 15) + SubCommand.Description);
  end;

  if (Style = chsCommand) and (Length(SubCommands) > 0) then
    ShowCommandExamples(SubCommands, CommandPath, Style);

  if Length(Parameters) > 0 then
  begin
    WriteLine('');
    WriteLine('Options:', ccCyan);
    WriteParameters(Parameters, '  ');
  end;

  if (Style = chsApplication) and (Length(SubCommands) > 0) then
    ShowCommandExamples(SubCommands, CommandPath, Style);
end;

procedure TCLIHelpRenderer.ShowCommandExamples(
  const SubCommands: array of ICommand;
  const CommandPath: string; const Style: TCommandHelpStyle);
var
  SubCommand: ICommand;
begin
  WriteLine('');
  WriteLine('Examples:', ccCyan);
  if Style = chsCommand then
  begin
    WriteLine('  ' + FExecutableName + ' ' + CommandPath +
      ' <command> --help');
    WriteLine('    Show help for a specific command');
    for SubCommand in SubCommands do
      WriteLine('  ' + FExecutableName + ' ' + CommandPath + ' ' +
        SubCommand.Name + ' --help');
  end
  else
  begin
    WriteLine('  Get help for commands:');
    WriteLine('    ' + FExecutableName + ' ' + CommandPath +
      ' <command> --help');
    WriteLine('');
    WriteLine('  Available command help:');
    for SubCommand in SubCommands do
      WriteLine('    ' + FExecutableName + ' ' + CommandPath + ' ' +
        SubCommand.Name + ' --help');
    WriteLine('');
  end;
end;

procedure TCLIHelpRenderer.ShowCompleteCommand(const Command: ICommand;
  const Indent: string);
var
  SubCommand: ICommand;
begin
  WriteLine(Indent + Command.Name + ' - ' + Command.Description);
  if Length(Command.Parameters) > 0 then
  begin
    WriteLine('');
    WriteLine(Indent + 'OPTIONS:', ccCyan);
    WriteParameters(Command.Parameters, Indent + '  ');
  end;

  if Length(Command.SubCommands) > 0 then
  begin
    WriteLine('');
    WriteLine(Indent + 'SUBCOMMANDS:', ccCyan);
    for SubCommand in Command.SubCommands do
    begin
      ShowCompleteCommand(SubCommand, Indent + '  ');
      WriteLine('');
    end;
  end;
end;

procedure TCLIHelpRenderer.ShowComplete;
var
  i: Integer;
begin
  WriteLine(FName + ' version ' + FVersion);
  WriteLine('');
  WriteLine('DESCRIPTION', ccCyan);
  if Assigned(FRootCommand) and (FRootCommand.Description <> '') then
    WriteLine('  ' + FRootCommand.Description)
  else
    WriteLine('  Complete reference for all commands and options');
  WriteLine('');

  if Assigned(FRootCommand) and (Length(FRootCommand.Parameters) > 0) then
  begin
    WriteLine('ROOT OPTIONS', ccCyan);
    WriteParameters(FRootCommand.Parameters, '  ');
    WriteLine('');
  end;

  WriteLine('GLOBAL OPTIONS', ccCyan);
  WriteLine('  -h, --help           Show command help');
  WriteLine('  --help-complete      Show this complete reference');
  WriteLine('  --completion-file    Output Bash completion script (use --completion-file > myapp-completion.sh)');
  WriteLine('  --completion-file-pwsh  Output PowerShell completion script (use --completion-file-pwsh > myapp-completion.ps1)');
  WriteLine('  -v, --version        Show version information');

  if Length(FCommands) > 0 then
  begin
    WriteLine('');
    WriteLine('COMMANDS', ccCyan);
    for i := 0 to Length(FCommands) - 1 do
    begin
      if i > 0 then
        WriteLine('');
      ShowCompleteCommand(FCommands[i], '  ');
    end;
    WriteLine('');
    WriteLine('For more details on a specific command, use:');
    WriteLine('  ' + FExecutableName + ' <command> --help');
  end;
end;

procedure TCLIHelpRenderer.ShowBrief;
var
  Command: ICommand;
begin
  if Assigned(FRootCommand) then
  begin
    WriteLine('Usage: ' + FExecutableName + ' [options]');
    if Length(FCommands) > 0 then
      WriteLine('       ' + FExecutableName + ' <command> [options]');
  end
  else
    WriteLine('Usage: ' + FExecutableName + ' <command> [options]');
  WriteLine('');
  WriteLine('Commands:', ccCyan);
  for Command in FCommands do
    WriteLine('  ' + PadRight(Command.Name, 15) + Command.Description);
  WriteLine('');
  WriteLine('Use --help for more information.');
end;

end.
