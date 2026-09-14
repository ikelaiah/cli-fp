unit CLI.Internal.CompletionScripts;

{$mode objfpc}{$H+}{$J-}

{ Internal rendering for the Bash and PowerShell completion scripts.
  The application facade owns process-specific output, while this unit owns
  only deterministic script construction and shell quoting. }

interface

uses
  CLI.Interfaces;

type
  TCompletionScriptLine = record
    Text: string;
    { Retains the historical TCLIApplication.WriteOutput routing used by
      framework-test capture; unmarked lines go directly to TConsole. }
    CaptureThroughApplication: Boolean;
  end;

  TCompletionScript = array of TCompletionScriptLine;

function RenderBashCompletionScript(const ApplicationName: string;
  const Commands: array of ICommand; const RootCommand: ICommand;
  const ExecutableName, ExecutablePath: string;
  const DebugMode: Boolean): TCompletionScript;

function RenderPowerShellCompletionScript(const ExecutableName,
  ExecutablePath: string): TCompletionScript;

implementation

uses
  SysUtils;

procedure AddLine(var Script: TCompletionScript; const Text: string;
  const CaptureThroughApplication: Boolean = False);
var
  Count: Integer;
begin
  Count := Length(Script);
  SetLength(Script, Count + 1);
  Script[Count].Text := Text;
  Script[Count].CaptureThroughApplication := CaptureThroughApplication;
end;

function QuoteForBash(const Value: string): string;
begin
  Result := #39 + StringReplace(Value, #39,
    #39 + '"' + #39 + '"' + #39, [rfReplaceAll]) + #39;
end;

function QuoteForPowerShell(const Value: string): string;
begin
  Result := #39 + StringReplace(Value, #39, #39 + #39,
    [rfReplaceAll]) + #39;
end;

function ShellIdentifier(const Value: string): string;
var
  i: Integer;
  Character: Char;
begin
  Result := '';
  for i := 1 to Length(Value) do
  begin
    Character := Value[i];
    if ((Character >= 'a') and (Character <= 'z')) or
      ((Character >= 'A') and (Character <= 'Z')) or
      ((Character >= '0') and (Character <= '9')) or
      (Character = '_') then
      Result := Result + Character
    else
      Result := Result + '_';
  end;
  if Result = '' then
    Result := 'cli';
end;

procedure AddBashTree(var Script: TCompletionScript; const Cmd: ICommand;
  const Path: string);
var
  Sub: ICommand;
  Param: ICommandParameter;
  SubNames, ParamFlags: string;
begin
  SubNames := '';
  for Sub in Cmd.SubCommands do
  begin
    if SubNames <> '' then
      SubNames := SubNames + ' ';
    SubNames := SubNames + Sub.Name;
  end;

  ParamFlags := '';
  for Param in Cmd.Parameters do
  begin
    if ParamFlags <> '' then
      ParamFlags := ParamFlags + ' ';
    ParamFlags := ParamFlags + Param.LongFlag;
    if Param.ShortFlag <> '' then
      ParamFlags := ParamFlags + ' ' + Param.ShortFlag;
  end;
  if ParamFlags <> '' then
    ParamFlags := ParamFlags + ' ';
  ParamFlags := ParamFlags + '--help -h --version -v';

  AddLine(Script, 'tree[' + QuoteForBash(Path + '|subcommands') + ']=' +
    QuoteForBash(SubNames));
  AddLine(Script, 'tree[' + QuoteForBash(Path + '|params') + ']=' +
    QuoteForBash(ParamFlags));

  for Sub in Cmd.SubCommands do
    AddBashTree(Script, Sub, Path + ' ' + Sub.Name);
end;

function RenderBashCompletionScript(const ApplicationName: string;
  const Commands: array of ICommand; const RootCommand: ICommand;
  const ExecutableName, ExecutablePath: string;
  const DebugMode: Boolean): TCompletionScript;
var
  Cmd: ICommand;
  Param: ICommandParameter;
  BashFunc, RootSubNames, RootParamFlags: string;
  i: Integer;
begin
  Result := nil;
  BashFunc := '_' + LowerCase(ShellIdentifier(ApplicationName)) +
    '_completions';

  AddLine(Result, '#!/bin/bash');
  AddLine(Result, 'declare -A tree');

  RootSubNames := '';
  RootParamFlags := '';
  for i := 0 to High(Commands) do
  begin
    Cmd := Commands[i];
    if RootSubNames <> '' then
      RootSubNames := RootSubNames + ' ';
    RootSubNames := RootSubNames + Cmd.Name;
  end;
  if Assigned(RootCommand) then
  begin
    for Param in RootCommand.Parameters do
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
  RootParamFlags := RootParamFlags +
    '--help --help-complete --version --completion-file --completion-file-pwsh -h -v';
  AddLine(Result, 'tree[' + QuoteForBash('__root__|subcommands') + ']=' +
    QuoteForBash(RootSubNames));
  AddLine(Result, 'tree[' + QuoteForBash('__root__|params') + ']=' +
    QuoteForBash(RootParamFlags));

  for i := 0 to High(Commands) do
    AddBashTree(Result, Commands[i], Commands[i].Name);

  AddLine(Result, '');
  AddLine(Result, BashFunc + '()', True);
  AddLine(Result, '{');
  AddLine(Result, '  local cur words cword args out dir candidates');
  AddLine(Result, '  executable=' + QuoteForBash(ExecutablePath), True);
  if DebugMode then
  begin
    AddLine(Result, '  # DEBUG: Print function call and COMP_WORDS');
    AddLine(Result,
      '  echo "[DEBUG] Called: $FUNCNAME, COMP_WORDS=(\"${COMP_WORDS[@]}\") COMP_CWORD=$COMP_CWORD" >&2');
  end;
  AddLine(Result, '  cur="${COMP_WORDS[COMP_CWORD]}"');
  AddLine(Result, '  words=("${COMP_WORDS[@]}")');
  AddLine(Result, '  cword=$COMP_CWORD');
  AddLine(Result, '  # Build args for __complete and call the application');
  AddLine(Result, '  args=()');
  AddLine(Result, '  for ((i=1;i<cword;i++)); do args+=("${words[i]}"); done');
  AddLine(Result, '  # If cursor is after a space, append empty token to indicate new word');
  AddLine(Result, '  if [[ "${COMP_LINE: -1}" == " " ]]; then');
  AddLine(Result, '    args+=("")');
  AddLine(Result, '  else');
  AddLine(Result, '    args+=("${words[cword]}")');
  AddLine(Result, '  fi');
  AddLine(Result, '  out=$("$executable" __complete "${args[@]}")', True);
  AddLine(Result, '  # Last line is directive in form :<number>');
  AddLine(Result, '  dir="$(printf "%s\n" "$out" | tail -n1)"');
  AddLine(Result, '  if [[ $dir =~ ^:([0-9]+)$ ]]; then');
  AddLine(Result, '    candidates="$(printf "%s\n" "$out" | sed ''$d'')"');
  AddLine(Result, '    directive=${BASH_REMATCH[1]}');
  AddLine(Result, '  else');
  AddLine(Result, '    candidates="$out"');
  AddLine(Result, '    directive=0');
  AddLine(Result, '  fi');
  if DebugMode then
  begin
    AddLine(Result, '  # DEBUG: Print completion call information');
    AddLine(Result,
      '  echo "[DEBUG] args=(${args[@]}) out=\"$out\" directive=$directive cur=[$cur] candidates=[$candidates]" >&2');
  end;
  AddLine(Result, '  # Populate COMPREPLY with matching candidates');
  AddLine(Result, '  while IFS='''' read -r comp; do');
  AddLine(Result, '    [[ -z "$comp" ]] && continue');
  AddLine(Result, '    COMPREPLY+=("$comp")');
  AddLine(Result, '  done < <(compgen -W "$candidates" -- "$cur")');
  AddLine(Result, '  return 0');
  AddLine(Result, '}');
  AddLine(Result, 'complete -F ' + BashFunc + ' -- ' +
    QuoteForBash(ExecutableName));
  AddLine(Result, 'complete -F ' + BashFunc + ' -- ' +
    QuoteForBash('./' + ExecutableName));
end;

function RenderPowerShellCompletionScript(const ExecutableName,
  ExecutablePath: string): TCompletionScript;
begin
  Result := nil;
  AddLine(Result, '# Usage: ./' + ExecutableName +
    ' --completion-file-pwsh > myapp-completion.ps1', True);
  AddLine(Result, '# Then in PowerShell:', True);
  AddLine(Result, '#   . ./myapp-completion.ps1', True);
  AddLine(Result, '# To make it permanent, add the above line to your $PROFILE', True);
  AddLine(Result, '# PowerShell argument completer for ' + ExecutableName);
  AddLine(Result, '');
  AddLine(Result, '$scriptBlock = {');
  AddLine(Result, '  param($wordToComplete, $commandAst, $cursorPosition)');
  AddLine(Result, '  $line = $commandAst.ToString()');
  AddLine(Result, '  $words = $line -split " +" | Where-Object { $_ -ne '''' }');
  AddLine(Result, '  $argsList = @($words | Select-Object -Skip 1)');
  AddLine(Result, '  if ($line.EndsWith(" ")) { $argsList += "" }');
  AddLine(Result, '$cliFpExecutable = ' + QuoteForPowerShell(ExecutablePath), True);
  AddLine(Result, '  $out = & $cliFpExecutable __complete @argsList 2>$null', True);
  AddLine(Result, '  if (-not $out) { return @() }');
  AddLine(Result, '  # Extract directive and candidates');
  AddLine(Result, '  $directive = 0');
  AddLine(Result, '  $candidates = @()');
  AddLine(Result, '  foreach ($line in $out) {');
  AddLine(Result, '    if ($line -match "^:([0-9]+)$") {');
  AddLine(Result, '      $directive = [int]$Matches[1]');
  AddLine(Result, '    } else {');
  AddLine(Result, '      $candidates += $line');
  AddLine(Result, '    }');
  AddLine(Result, '  }');
  AddLine(Result, '  $results = @()');
  AddLine(Result, '  if ($candidates.Count -eq 0) { return @() }');
  AddLine(Result, '  foreach ($c in $candidates) {');
  AddLine(Result, '    # Skip empty candidates');
  AddLine(Result, '    if ([string]::IsNullOrWhiteSpace($c)) { continue }');
  AddLine(Result, '    # Filter by prefix');
  AddLine(Result,
    '    if ([string]::IsNullOrEmpty($wordToComplete) -or $c.StartsWith($wordToComplete, [StringComparison]::CurrentCultureIgnoreCase)) {');
  AddLine(Result, '      if (($directive -band 2) -ne 0) {');
  AddLine(Result,
    '        $results += [System.Management.Automation.CompletionResult]::new($c, $c, "ParameterName", $c)');
  AddLine(Result, '      } else {');
  AddLine(Result,
    '        $results += [System.Management.Automation.CompletionResult]::new($c, $c, "ParameterValue", $c)');
  AddLine(Result, '      }');
  AddLine(Result, '    }');
  AddLine(Result, '  }');
  AddLine(Result, '  return $results');
  AddLine(Result, '}');
  AddLine(Result, '');
  AddLine(Result, '# Register for all common invocation patterns');
  AddLine(Result, 'Register-ArgumentCompleter -CommandName ' +
    QuoteForPowerShell(ExecutableName) + ' -ScriptBlock $scriptBlock');
  AddLine(Result, 'Register-ArgumentCompleter -CommandName ' +
    QuoteForPowerShell(ChangeFileExt(ExecutableName, '')) +
    ' -ScriptBlock $scriptBlock');
  AddLine(Result, 'Register-ArgumentCompleter -CommandName ' +
    QuoteForPowerShell('./' + ExecutableName) + ' -ScriptBlock $scriptBlock');
  AddLine(Result, 'Register-ArgumentCompleter -CommandName ' +
    QuoteForPowerShell('.\' + ExecutableName) + ' -ScriptBlock $scriptBlock');
  AddLine(Result, 'Register-ArgumentCompleter -CommandName ' +
    QuoteForPowerShell('.\\' + ExecutableName) + ' -ScriptBlock $scriptBlock');
  AddLine(Result, '');
  AddLine(Result, '# Try -Native flag if PowerShell 7+');
  AddLine(Result, 'if ($PSVersionTable.PSVersion.Major -ge 7) {');
  AddLine(Result, '  Register-ArgumentCompleter -Native -CommandName ' +
    QuoteForPowerShell(ChangeFileExt(ExecutableName, '')) + ' -ScriptBlock {');
  AddLine(Result, '    param($wordToComplete, $commandAst, $cursorPosition)');
  AddLine(Result, '    & $scriptBlock $wordToComplete $commandAst $cursorPosition');
  AddLine(Result, '  }');
  AddLine(Result, '}');
end;

end.
