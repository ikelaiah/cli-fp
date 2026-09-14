$ErrorActionPreference = 'Stop'

function Assert-LastExitCode([string]$Message) {
  if ($LASTEXITCODE -ne 0) {
    throw $Message
  }
}

$RootDir = (Resolve-Path (Join-Path $PSScriptRoot "..\..")).Path
$TmpDir = Join-Path ([System.IO.Path]::GetTempPath()) (
  "cli-fp-completion-" + [System.Guid]::NewGuid().ToString("N")
)

try {
  $UnitDir = Join-Path $TmpDir "units"
  $FixtureSource = Join-Path $RootDir "tests\completion-tests\completion_fixture.lpr"
  $Fixture = Join-Path $TmpDir "completion_fixture.exe"
  $GeneratedScript = Join-Path $TmpDir "completion.ps1"
  New-Item -ItemType Directory -Force -Path $UnitDir | Out-Null

  fpc `
    -B `
    "-Fu$RootDir\src" `
    "-FE$TmpDir" `
    "-FU$UnitDir" `
    $FixtureSource
  Assert-LastExitCode "Failed to compile the completion fixture"

  & $Fixture --completion-file-pwsh | Set-Content -LiteralPath $GeneratedScript -Encoding utf8
  Assert-LastExitCode "Failed to generate the PowerShell completion script"

  $Tokens = $null
  $ParseErrors = $null
  [void][System.Management.Automation.Language.Parser]::ParseFile(
    $GeneratedScript, [ref]$Tokens, [ref]$ParseErrors
  )
  if ($ParseErrors.Count -gt 0) {
    throw "Generated PowerShell completion script did not parse: $($ParseErrors | Out-String)"
  }

  $Content = Get-Content -LiteralPath $GeneratedScript -Raw
  if ($Content -notmatch 'Register-ArgumentCompleter -CommandName') {
    throw "Generated PowerShell completion script lacks completer registration"
  }
}
finally {
  if (Test-Path -LiteralPath $TmpDir) {
    Remove-Item -Recurse -Force $TmpDir
  }
}
