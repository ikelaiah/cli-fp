$ErrorActionPreference = 'Stop'

$SourceRoot = (Resolve-Path (Join-Path $PSScriptRoot "..")).Path
$TmpDir = Join-Path ([System.IO.Path]::GetTempPath()) (
  "cli-fp-cleanup-smoke-" + [System.Guid]::NewGuid().ToString("N")
)
$WorkRoot = Join-Path $TmpDir "cli-fp"

function Assert-LastExitCode([string]$Message) {
  if ($LASTEXITCODE -ne 0) {
    throw $Message
  }
}

try {
  git clone --quiet --no-hardlinks $SourceRoot $WorkRoot
  Assert-LastExitCode "Failed to create isolated cleanup-smoke repository"

  $Examples = @(
    'ColorDemo',
    'ErrorHandlingDemo',
    'LongRunningOpDemo',
    'ProgressDemo',
    'RootCommandDemo',
    'SimpleDemo',
    'SubCommandDemo'
  )
  $Sentinels = @(
    'example-bin/.gitkeep',
    'example-bin/README.md',
    'example-bin/simpledemo_completion.bash',
    'example-bin/simpledemo_completion.ps1',
    'example-bin/subcommanddemo_completion.bash',
    'example-bin/subcommanddemo_completion.ps1'
  )

  foreach ($Sentinel in $Sentinels) {
    if (-not (Test-Path -LiteralPath (Join-Path $WorkRoot $Sentinel))) {
      throw "Tracked cleanup sentinel is missing before the test: $Sentinel"
    }
  }

  $UnitRoot = Join-Path $TmpDir "units"
  New-Item -ItemType Directory -Force -Path $UnitRoot | Out-Null
  $BuildDir = Join-Path $WorkRoot "example-bin"

  foreach ($Example in $Examples) {
    $UnitDir = Join-Path $UnitRoot $Example
    New-Item -ItemType Directory -Force -Path $UnitDir | Out-Null
    fpc `
      "-Fu$WorkRoot\src" `
      "-FE$BuildDir" `
      "-FU$UnitDir" `
      (Join-Path $WorkRoot "examples\$Example\$Example.lpr")
    Assert-LastExitCode "Failed to compile example: $Example"

    $Binary = Join-Path $BuildDir "$Example.exe"
    $UnixStyleBinary = Join-Path $BuildDir $Example
    if (-not (Test-Path -LiteralPath $Binary) -and
        -not (Test-Path -LiteralPath $UnixStyleBinary)) {
      throw "Expected compiled example binary was not found: $Example"
    }
  }

  Push-Location $WorkRoot
  try {
    & (Join-Path $WorkRoot "clean-all-examples.ps1") | Out-Null
  }
  finally {
    Pop-Location
  }

  foreach ($Example in $Examples) {
    if ((Test-Path -LiteralPath (Join-Path $BuildDir "$Example.exe")) -or
        (Test-Path -LiteralPath (Join-Path $BuildDir $Example))) {
      throw "Generated example binary was not removed: $Example"
    }
  }

  foreach ($Sentinel in $Sentinels) {
    $SentinelPath = Join-Path $WorkRoot $Sentinel
    if (-not (Test-Path -LiteralPath $SentinelPath)) {
      throw "Tracked cleanup sentinel was deleted: $Sentinel"
    }
    git -C $WorkRoot diff --quiet -- $Sentinel
    Assert-LastExitCode "Tracked cleanup sentinel was changed: $Sentinel"
  }

  git -C $WorkRoot diff --quiet
  Assert-LastExitCode "Cleanup changed tracked repository files"
  Write-Host "Example cleanup smoke check passed."
}
finally {
  if (Test-Path -LiteralPath $TmpDir) {
    Remove-Item -LiteralPath $TmpDir -Recurse -Force
  }
}
