unit Test_Completion_Scripts;

{$mode objfpc}{$H+}{$J-}

{ Characterization tests for the internal completion-script renderer.
  The expected lines preserve the v1.5.4 shell output exactly while allowing
  the renderer to be exercised without redirecting the process console. }

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  CLI.Interfaces, CLI.Application, CLI.Command,
  CLI.Internal.CompletionScripts;

type
  TCompletionFixtureCommand = class(TBaseCommand)
  public
    function Execute: Integer; override;
  end;

  TCompletionScriptTests = class(TTestCase)
  private
    procedure AssertScriptEquals(const Name: string;
      const Expected: array of string; const Actual: TCompletionScript);
  published
    procedure Test_RootParameterCompletion;
    procedure Test_CandidateCompletionBehaviour;
    procedure Test_DirectiveCompletionBehaviour;
    procedure Test_ApplicationScriptOutputContract;
    procedure Test_SingleFlagParameterCompletion;
    procedure Test_EnumCompletionMatchesValidation;
    procedure Test_BashRenderingPreservesV154Contract;
    procedure Test_PowerShellRenderingPreservesV154Contract;
  end;

implementation

function TCompletionFixtureCommand.Execute: Integer;
begin
  Result := 0;
end;

function MakeArgs(const Values: array of string): TStringArray;
var
  i: Integer;
begin
  Result := nil;
  SetLength(Result, Length(Values));
  for i := 0 to Length(Values) - 1 do
    Result[i] := Values[i];
end;

procedure TCompletionScriptTests.AssertScriptEquals(const Name: string;
  const Expected: array of string; const Actual: TCompletionScript);
var
  i: Integer;
begin
  AssertEquals(Name + ' line count', Length(Expected), Length(Actual));
  for i := 0 to High(Expected) do
    AssertEquals(Format('%s line %d', [Name, i + 1]), Expected[i],
      Actual[i].Text);
end;

procedure TCompletionScriptTests.Test_RootParameterCompletion;
var
  Root: TCompletionFixtureCommand;
  App: TCLIApplication;
  Candidates: TStringList;
begin
  Root := TCompletionFixtureCommand.Create('', 'Default application action');
  Root.AddStringParameter('-n', '--name', 'Name to greet');
  Root.AddEnumParameter('-m', '--mode', 'Greeting mode',
    'normal|friendly|formal');
  App := TCLIApplication.Create('TestApp', '1.3.0', Root);
  try
    Candidates := App.TestComplete(MakeArgs(['--n']));
    try
      AssertTrue('Root parameter name should be completed',
        Candidates.IndexOf('--name') >= 0);
    finally
      Candidates.Free;
    end;

    Candidates := App.TestComplete(MakeArgs(['--']));
    try
      AssertTrue('Root completion should retain all global options',
        Candidates.IndexOf('--completion-file') >= 0);
    finally
      Candidates.Free;
    end;

    Candidates := App.TestComplete(MakeArgs(['--mode', '']));
    try
      AssertTrue('Root enum value should be completed',
        Candidates.IndexOf('friendly') >= 0);
    finally
      Candidates.Free;
    end;
  finally
    App.Free;
  end;
end;

procedure TCompletionScriptTests.Test_CandidateCompletionBehaviour;
var
  App: TCLIApplication;
  Deploy, Target: TCompletionFixtureCommand;
  Candidates: TStringList;
begin
  App := TCLIApplication.Create('TestApp', '1.3.3');
  Deploy := TCompletionFixtureCommand.Create('deploy', 'Deploy an application');
  Target := TCompletionFixtureCommand.Create('target', 'Manage deployment targets');
  try
    Deploy.AddFlag('-v', '--verbose', 'Verbose output');
    Deploy.AddEnumParameter('-m', '--mode', 'Deployment mode',
      'safe|fast');
    Deploy.AddSubCommand(Target);
    App.RegisterCommand(Deploy);

    Candidates := App.TestComplete(MakeArgs([]));
    try
      AssertTrue('Empty completion should list top-level commands',
        Candidates.IndexOf('deploy') >= 0);
    finally
      Candidates.Free;
    end;

    Candidates := App.TestComplete(MakeArgs(['de']));
    try
      AssertTrue('Command prefixes should be completed',
        Candidates.IndexOf('deploy') >= 0);
    finally
      Candidates.Free;
    end;

    Candidates := App.TestComplete(MakeArgs(['deploy', '--v']));
    try
      AssertTrue('Command flags should be completed',
        Candidates.IndexOf('--verbose') >= 0);
    finally
      Candidates.Free;
    end;

    Candidates := App.TestComplete(MakeArgs(['deploy', '--mode', '']));
    try
      AssertTrue('Enum values should be completed',
        Candidates.IndexOf('safe') >= 0);
      AssertTrue('Completion should include a directive',
        Candidates.IndexOf(':' + IntToStr(CD_NOFILE)) >= 0);
    finally
      Candidates.Free;
    end;

    Candidates := App.TestComplete(MakeArgs(['deploy', '']));
    try
      AssertTrue('Subcommands should be completed',
        Candidates.IndexOf('target') >= 0);
      AssertTrue('Available flags should accompany subcommands',
        Candidates.IndexOf('--mode') >= 0);
    finally
      Candidates.Free;
    end;
  finally
    App.Free;
  end;
end;

procedure TCompletionScriptTests.Test_DirectiveCompletionBehaviour;
var
  App: TCLIApplication;
  Cmd: TCompletionFixtureCommand;
  Candidates: TStringList;
begin
  App := TCLIApplication.Create('TestApp', '1.4.2');
  Cmd := TCompletionFixtureCommand.Create('deploy', 'Deploy an application');
  try
    App.RegisterCommand(Cmd);

    Candidates := App.TestComplete(MakeArgs([]));
    try
      AssertEquals('Empty completion should finish with a directive', ':0',
        Candidates[Candidates.Count - 1]);
    finally
      Candidates.Free;
    end;

    Candidates := App.TestComplete(MakeArgs(['de']));
    try
      AssertEquals('Command-prefix completion should finish with a directive',
        ':0', Candidates[Candidates.Count - 1]);
    finally
      Candidates.Free;
    end;
  finally
    App.Free;
  end;
end;

procedure TCompletionScriptTests.Test_ApplicationScriptOutputContract;
var
  App: TCLIApplication;
  Output: TStringList;
  i, HeaderCount: Integer;
begin
  App := TCLIApplication.Create('my app$unsafe', '1.4.2');
  Output := TStringList.Create;
  try
    AssertEquals('Bash script generation should succeed', 0,
      App.TestExecuteAndCapture(MakeArgs(['--completion-file']), Output));
    AssertTrue('Bash function names should be safe shell identifiers',
      Pos('_my_app_unsafe_completions()', Output.Text) > 0);
    AssertTrue('Bash should invoke the executable through a quoted variable',
      Pos('"$executable" __complete', Output.Text) > 0);

    Output.Clear;
    AssertEquals('PowerShell script generation should succeed', 0,
      App.TestExecuteAndCapture(MakeArgs(['--completion-file-pwsh']), Output));
    HeaderCount := 0;
    for i := 0 to Output.Count - 1 do
      if Pos('# Usage:', Output[i]) = 1 then
        Inc(HeaderCount);
    AssertEquals('PowerShell output should contain exactly one preamble', 1,
      HeaderCount);
    AssertTrue('PowerShell should invoke a quoted executable variable',
      Pos('& $cliFpExecutable __complete @argsList', Output.Text) > 0);
  finally
    Output.Free;
    App.Free;
  end;
end;

procedure TCompletionScriptTests.Test_SingleFlagParameterCompletion;
var
  App: TCLIApplication;
  Cmd: TCompletionFixtureCommand;
  Candidates: TStringList;
begin
  App := TCLIApplication.Create('TestApp', '1.5.1');
  Cmd := TCompletionFixtureCommand.Create('test', 'Test command');
  try
    Cmd.AddStringParameter('-n', '', 'Short-only parameter');
    Cmd.AddStringParameter('', '--name', 'Long-only parameter');
    Cmd.AddStringParameter('-b', '--both', 'Parameter with both flags');
    App.RegisterCommand(Cmd);

    Candidates := App.TestComplete(MakeArgs(['test', '']));
    try
      AssertEquals('Completion should not emit an empty flag candidate', -1,
        Candidates.IndexOf(''));
      AssertTrue('Short-only parameters should be completed',
        Candidates.IndexOf('-n') >= 0);
      AssertTrue('Long-only parameters should be completed',
        Candidates.IndexOf('--name') >= 0);
      AssertTrue('Both parameter flags should be completed',
        Candidates.IndexOf('-b') >= 0);
      AssertTrue('Both parameter long flags should be completed',
        Candidates.IndexOf('--both') >= 0);
    finally
      Candidates.Free;
    end;

    Candidates := App.TestComplete(MakeArgs(['test', '--']));
    try
      AssertEquals('Flag-prefix completion should not emit an empty candidate',
        -1, Candidates.IndexOf(''));
      AssertTrue('Long-only parameters should complete after a long prefix',
        Candidates.IndexOf('--name') >= 0);
      AssertTrue('Both parameters should complete after a long prefix',
        Candidates.IndexOf('--both') >= 0);
    finally
      Candidates.Free;
    end;
  finally
    App.Free;
  end;
end;

procedure TCompletionScriptTests.Test_EnumCompletionMatchesValidation;
var
  App: TCLIApplication;
  Cmd: TCompletionFixtureCommand;
  Candidates: TStringList;
begin
  App := TCLIApplication.Create('TestApp', '1.5.3');
  Cmd := TCompletionFixtureCommand.Create('test', 'Test command');
  try
    Cmd.AddEnumParameter('-m', '--mode', 'Mode', '"normal mode"|fast mode');
    App.RegisterCommand(Cmd);
    AssertEquals('Quoted enum values containing spaces should validate', 0,
      App.TestExecute(MakeArgs(['test', '--mode', 'normal mode'])));

    Candidates := App.TestComplete(MakeArgs(['test', '--mode', '']));
    try
      AssertTrue('Completion must offer the same quoted enum value as validation',
        Candidates.IndexOf('normal mode') >= 0);
      AssertTrue('Completion must retain unquoted values containing spaces',
        Candidates.IndexOf('fast mode') >= 0);
    finally
      Candidates.Free;
    end;
  finally
    App.Free;
  end;
end;

procedure TCompletionScriptTests.Test_BashRenderingPreservesV154Contract;
var
  Deploy, Target: TCompletionFixtureCommand;
  Commands: array of ICommand;
  Script: TCompletionScript;
begin
  Deploy := TCompletionFixtureCommand.Create('deploy', 'Deploy an application');
  try
    Deploy.AddStringParameter('-o', '--output', 'Output file');
    Target := TCompletionFixtureCommand.Create('target', 'Manage targets');
    Deploy.AddSubCommand(Target);
    SetLength(Commands, 1);
    Commands[0] := Deploy;

    Script := RenderBashCompletionScript('my app$unsafe', Commands, nil,
      'tool''s app.exe', 'C:\Program Files\tool''s app.exe', True);

    AssertScriptEquals('Bash completion script', [
      '#!/bin/bash',
      'declare -A tree',
      'tree[''__root__|subcommands'']=''deploy''',
      'tree[''__root__|params'']=''--help --help-complete --version --completion-file --completion-file-pwsh -h -v''',
      'tree[''deploy|subcommands'']=''target''',
      'tree[''deploy|params'']=''--output -o --help -h''',
      'tree[''deploy target|subcommands'']=''''',
      'tree[''deploy target|params'']=''--help -h''',
      '',
      '_my_app_unsafe_completions()',
      '{',
      '  local cur words cword args out dir candidates',
      '  executable=''C:\Program Files\tool''"''"''s app.exe''',
      '  # DEBUG: Print function call and COMP_WORDS',
      '  echo "[DEBUG] Called: $FUNCNAME, COMP_WORDS=(\"${COMP_WORDS[@]}\") COMP_CWORD=$COMP_CWORD" >&2',
      '  cur="${COMP_WORDS[COMP_CWORD]}"',
      '  words=("${COMP_WORDS[@]}")',
      '  cword=$COMP_CWORD',
      '  # Build args for __complete and call the application',
      '  args=()',
      '  for ((i=1;i<cword;i++)); do args+=("${words[i]}"); done',
      '  # If cursor is after a space, append empty token to indicate new word',
      '  if [[ "${COMP_LINE: -1}" == " " ]]; then',
      '    args+=("")',
      '  else',
      '    args+=("${words[cword]}")',
      '  fi',
      '  out=$("$executable" __complete "${args[@]}")',
      '  # Last line is directive in form :<number>',
      '  dir="$(printf "%s\n" "$out" | tail -n1)"',
      '  if [[ $dir =~ ^:([0-9]+)$ ]]; then',
      '    candidates="$(printf "%s\n" "$out" | sed ''$d'')"',
      '    directive=${BASH_REMATCH[1]}',
      '  else',
      '    candidates="$out"',
      '    directive=0',
      '  fi',
      '  # DEBUG: Print completion call information',
      '  echo "[DEBUG] args=(${args[@]}) out=\"$out\" directive=$directive cur=[$cur] candidates=[$candidates]" >&2',
      '  # Populate COMPREPLY with matching candidates',
      '  while IFS='''' read -r comp; do',
      '    [[ -z "$comp" ]] && continue',
      '    COMPREPLY+=("$comp")',
      '  done < <(compgen -W "$candidates" -- "$cur")',
      '  return 0',
      '}',
      'complete -F _my_app_unsafe_completions -- ''tool''"''"''s app.exe''',
      'complete -F _my_app_unsafe_completions -- ''./tool''"''"''s app.exe'''
    ], Script);

    AssertFalse('Bash shebang keeps direct console routing',
      Script[0].CaptureThroughApplication);
    AssertTrue('Bash function declaration keeps application output routing',
      Script[9].CaptureThroughApplication);
    AssertTrue('Bash executable assignment keeps application output routing',
      Script[12].CaptureThroughApplication);
    AssertTrue('Bash completion call keeps application output routing',
      Script[27].CaptureThroughApplication);
  finally
    SetLength(Commands, 0);
  end;
end;

procedure TCompletionScriptTests.Test_PowerShellRenderingPreservesV154Contract;
var
  Script: TCompletionScript;
begin
  Script := RenderPowerShellCompletionScript('tool''s app.exe',
    'C:\Program Files\tool''s app.exe');

  AssertScriptEquals('PowerShell completion script', [
    '# Usage: ./tool''s app.exe --completion-file-pwsh > myapp-completion.ps1',
    '# Then in PowerShell:',
    '#   . ./myapp-completion.ps1',
    '# To make it permanent, add the above line to your $PROFILE',
    '# PowerShell argument completer for tool''s app.exe',
    '',
    '$scriptBlock = {',
    '  param($wordToComplete, $commandAst, $cursorPosition)',
    '  $line = $commandAst.ToString()',
    '  $words = $line -split " +" | Where-Object { $_ -ne '''' }',
    '  $argsList = @($words | Select-Object -Skip 1)',
    '  if ($line.EndsWith(" ")) { $argsList += "" }',
    '$cliFpExecutable = ''C:\Program Files\tool''''s app.exe''',
    '  $out = & $cliFpExecutable __complete @argsList 2>$null',
    '  if (-not $out) { return @() }',
    '  # Extract directive and candidates',
    '  $directive = 0',
    '  $candidates = @()',
    '  foreach ($line in $out) {',
    '    if ($line -match "^:([0-9]+)$") {',
    '      $directive = [int]$Matches[1]',
    '    } else {',
    '      $candidates += $line',
    '    }',
    '  }',
    '  $results = @()',
    '  if ($candidates.Count -eq 0) { return @() }',
    '  foreach ($c in $candidates) {',
    '    # Skip empty candidates',
    '    if ([string]::IsNullOrWhiteSpace($c)) { continue }',
    '    # Filter by prefix',
    '    if ([string]::IsNullOrEmpty($wordToComplete) -or $c.StartsWith($wordToComplete, [StringComparison]::CurrentCultureIgnoreCase)) {',
    '      if (($directive -band 2) -ne 0) {',
    '        $results += [System.Management.Automation.CompletionResult]::new($c, $c, "ParameterName", $c)',
    '      } else {',
    '        $results += [System.Management.Automation.CompletionResult]::new($c, $c, "ParameterValue", $c)',
    '      }',
    '    }',
    '  }',
    '  return $results',
    '}',
    '',
    '# Register for all common invocation patterns',
    'Register-ArgumentCompleter -CommandName ''tool''''s app.exe'' -ScriptBlock $scriptBlock',
    'Register-ArgumentCompleter -CommandName ''tool''''s app'' -ScriptBlock $scriptBlock',
    'Register-ArgumentCompleter -CommandName ''./tool''''s app.exe'' -ScriptBlock $scriptBlock',
    'Register-ArgumentCompleter -CommandName ''.\tool''''s app.exe'' -ScriptBlock $scriptBlock',
    'Register-ArgumentCompleter -CommandName ''.\\tool''''s app.exe'' -ScriptBlock $scriptBlock',
    '',
    '# Try -Native flag if PowerShell 7+',
    'if ($PSVersionTable.PSVersion.Major -ge 7) {',
    '  Register-ArgumentCompleter -Native -CommandName ''tool''''s app'' -ScriptBlock {',
    '    param($wordToComplete, $commandAst, $cursorPosition)',
    '    & $scriptBlock $wordToComplete $commandAst $cursorPosition',
    '  }',
    '}'
  ], Script);

  AssertTrue('PowerShell preamble keeps application output routing',
    Script[0].CaptureThroughApplication);
  AssertFalse('PowerShell script block keeps direct console routing',
    Script[6].CaptureThroughApplication);
  AssertTrue('PowerShell executable assignment keeps application output routing',
    Script[12].CaptureThroughApplication);
  AssertTrue('PowerShell completion call keeps application output routing',
    Script[13].CaptureThroughApplication);
end;

initialization
  RegisterTest(TCompletionScriptTests);
end.
