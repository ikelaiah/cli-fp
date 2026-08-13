$ErrorActionPreference = 'Stop'

function Assert-LastExitCode([string]$Message) {
  if ($LASTEXITCODE -ne 0) {
    throw $Message
  }
}

$RootDir = (Resolve-Path (Join-Path $PSScriptRoot "..")).Path
$TmpDir = Join-Path (
  [System.IO.Path]::GetTempPath()
) ("cli-fp-tests-" + [System.Guid]::NewGuid().ToString("N"))

try {
  $UnitDir = Join-Path $TmpDir "units"
  New-Item -ItemType Directory -Force -Path $UnitDir | Out-Null

  fpc `
    -dCLI_FP_TESTING `
    "-Fu$RootDir\src" `
    "-Fu$RootDir\tests" `
    "-FE$TmpDir" `
    "-FU$UnitDir" `
    (Join-Path $RootDir "tests\TestRunner.lpr")
  Assert-LastExitCode "Failed to compile framework tests"

  & (Join-Path $TmpDir "TestRunner.exe") --all --format=plain
  Assert-LastExitCode "Framework tests failed"
}
finally {
  if (Test-Path $TmpDir) {
    Remove-Item -Recurse -Force $TmpDir
  }
}
