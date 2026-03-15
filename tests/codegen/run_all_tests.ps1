$ErrorActionPreference = 'Stop'

function New-TempDir {
  $path = Join-Path ([System.IO.Path]::GetTempPath()) ("cli-fp-codegen-" + [System.Guid]::NewGuid().ToString("N"))
  New-Item -ItemType Directory -Path $path | Out-Null
  return $path
}

function Normalize-Content([string]$Path) {
  return (Get-Content -Raw $Path) -replace "`r`n", "`n"
}

function Assert-True([bool]$Condition, [string]$Message) {
  if (-not $Condition) {
    throw $Message
  }
}

function Write-JsonFile([string]$Path, $Object) {
  $Object | ConvertTo-Json -Depth 20 | Set-Content -Path $Path
}

$RootDir = (Resolve-Path (Join-Path $PSScriptRoot "..\..")).Path
$GenSrc = Join-Path $RootDir "tools\cli-fp-gen\cli_fp_gen.lpr"
$GenExe = Join-Path $RootDir "tools\cli-fp-gen\cli_fp_gen.exe"
$FixtureDir = Join-Path $RootDir "tests\codegen-fixtures\golden-basic"
$GoldenDir = Join-Path $RootDir "tests\codegen-golden\golden-basic"
$TmpDir = New-TempDir

try {
  fpc "-Fu$RootDir\tools\cli-fp-gen\src" $GenSrc | Out-Null

  # Golden output check
  $GoldenProject = Join-Path $TmpDir "golden"
  New-Item -ItemType Directory -Force -Path $GoldenProject | Out-Null
  Copy-Item -Force (Join-Path $FixtureDir "clifp.json") (Join-Path $GoldenProject "clifp.json")
  & $GenExe generate --project $GoldenProject | Out-Null

  $GoldenFiles = @(
    "src\GoldenDemo.lpr",
    "src\generated\GoldenDemo_CommandRegistry_Generated.pas",
    "src\generated\.clifp-manifest.json",
    "src\commands\GoldenDemo_Command_Greet.pas",
    "src\commands\GoldenDemo_Command_Repo.pas",
    "src\commands\GoldenDemo_Command_RepoClone.pas",
    "src\commands\GoldenDemo_Command_Types.pas"
  )

  foreach ($RelativePath in $GoldenFiles) {
    $Expected = Normalize-Content (Join-Path $GoldenDir $RelativePath)
    $Actual = Normalize-Content (Join-Path $GoldenProject $RelativePath)
    Assert-True ($Expected -eq $Actual) "Golden mismatch: $RelativePath"
  }

  Write-Host "Golden test passed"

  # Compile smoke check
  fpc `
    "-Fu$RootDir\src" `
    "-Fu$GoldenProject\src" `
    "-Fu$GoldenProject\src\generated" `
    "-Fu$GoldenProject\src\commands" `
    (Join-Path $GoldenProject "src\GoldenDemo.lpr") | Out-Null

  & (Join-Path $GoldenProject "src\GoldenDemo.exe") --help | Out-Null
  & (Join-Path $GoldenProject "src\GoldenDemo.exe") repo | Out-Null

  Write-Host "Compile smoke test passed"

  # Operations and path guard check
  $DemoProject = Join-Path $TmpDir "demo"
  & $GenExe init $DemoProject --force | Out-Null

  $DryRunOutput = (& $GenExe add command repo --project $DemoProject --description "Repo tools" --dry-run) | Out-String
  Assert-True ($DryRunOutput -match "Demo_Command_Repo\.pas") "Dry-run add did not preview the new command stub"
  Assert-True (-not ((Get-Content -Raw (Join-Path $DemoProject "clifp.json")) -match '"name"\s*:\s*"repo"')) "Dry-run add modified clifp.json"
  Assert-True (-not (Test-Path (Join-Path $DemoProject "src\commands\Demo_Command_Repo.pas"))) "Dry-run add created a command stub"

  & $GenExe add command repo --project $DemoProject --description "Repo tools" | Out-Null
  & $GenExe add command clone --parent repo --project $DemoProject --description "Clone repo" | Out-Null

  $null = (& $GenExe remove command repo --project $DemoProject 2>&1 | Out-String)
  Assert-True ($LASTEXITCODE -ne 0) "Expected remove command without --cascade to fail"

  & $GenExe remove command repo --cascade --project $DemoProject | Out-Null
  Assert-True (-not ((Get-Content -Raw (Join-Path $DemoProject "clifp.json")) -match '"name"\s*:\s*"repo"')) "repo command still present after cascade remove"

  $DemoSpec = Get-Content -Raw (Join-Path $DemoProject "clifp.json") | ConvertFrom-Json
  $OldProgramPath = Join-Path $DemoProject $DemoSpec.app.programFile
  Assert-True (Test-Path $OldProgramPath) "Expected original program file to exist after init"

  $DemoSpec.app.programFile = "src/DemoRenamed.lpr"
  Write-JsonFile (Join-Path $DemoProject "clifp.json") $DemoSpec
  & $GenExe generate --project $DemoProject | Out-Null

  Assert-True (Test-Path (Join-Path $DemoProject "src\DemoRenamed.lpr")) "Renamed program file was not generated"
  Assert-True (-not (Test-Path $OldProgramPath)) "Old generated program file was not removed by manifest cleanup"
  Assert-True ((Get-Content -Raw (Join-Path $DemoProject "src\generated\.clifp-manifest.json")) -match "src/DemoRenamed\.lpr") "Manifest did not track renamed program file"

  $DescriptionsProject = Join-Path $TmpDir "descriptions"
  & $GenExe init $DescriptionsProject --force | Out-Null
  & $GenExe add command repo --project $DescriptionsProject --description "Owner's tools" | Out-Null

  $DescriptionsSpec = Get-Content -Raw (Join-Path $DescriptionsProject "clifp.json") | ConvertFrom-Json
  foreach ($Command in $DescriptionsSpec.commands) {
    if ($Command.name -eq "repo") {
      $Command.description = "Repo team's tools"
    }
  }
  Write-JsonFile (Join-Path $DescriptionsProject "clifp.json") $DescriptionsSpec
  & $GenExe generate --project $DescriptionsProject | Out-Null

  $ProgramPath = Join-Path $DescriptionsProject $DescriptionsSpec.app.programFile
  fpc `
    "-Fu$RootDir\src" `
    "-Fu$DescriptionsProject\src" `
    "-Fu$DescriptionsProject\src\generated" `
    "-Fu$DescriptionsProject\src\commands" `
    $ProgramPath | Out-Null

  $ExePath = [System.IO.Path]::ChangeExtension($ProgramPath, ".exe")
  $HelpOutput = (& $ExePath repo --help) | Out-String
  Assert-True ($HelpOutput -match "Repo team's tools") "Regenerated command description did not update runtime help"

  $PathGuardProject = Join-Path $TmpDir "path-guard"
  & $GenExe init $PathGuardProject --force | Out-Null

  $PathGuardSpec = Get-Content -Raw (Join-Path $PathGuardProject "clifp.json") | ConvertFrom-Json
  $PathGuardSpec.app.programFile = "../outside/Escape.lpr"
  Write-JsonFile (Join-Path $PathGuardProject "clifp.json") $PathGuardSpec

  $null = (& $GenExe generate --project $PathGuardProject 2>&1 | Out-String)
  Assert-True ($LASTEXITCODE -ne 0) "Expected invalid programFile path to fail validation"
  Assert-True (-not (Test-Path (Join-Path $TmpDir "outside\Escape.lpr"))) "Generator wrote a program file outside the project directory"

  Write-Host "Ops test passed"
}
finally {
  if (Test-Path $TmpDir) {
    Remove-Item -Recurse -Force $TmpDir
  }
}
