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
$FixtureDir = Join-Path $RootDir "tests\codegen-fixtures\golden-basic"
$GoldenDir = Join-Path $RootDir "tests\codegen-golden\golden-basic"
$TmpDir = New-TempDir
$GenExe = Join-Path $TmpDir "cli_fp_gen.exe"
$LinkGuardJunction = $null

try {
  $GenUnits = Join-Path $TmpDir "gen-units"
  New-Item -ItemType Directory -Path $GenUnits | Out-Null
  fpc `
    "-Fu$RootDir\tools\cli-fp-gen\src" `
    "-FE$TmpDir" `
    "-FU$GenUnits" `
    $GenSrc
  Assert-True ($LASTEXITCODE -eq 0) "Failed to compile cli-fp-gen"

  # Focused unit tests for naming and validation
  $UnitTestOutput = Join-Path $TmpDir "unit-tests"
  $UnitTestUnits = Join-Path $UnitTestOutput "units"
  New-Item -ItemType Directory -Force -Path $UnitTestUnits | Out-Null
  fpc `
    "-Fu$RootDir\tools\cli-fp-gen\src" `
    "-Fu$PSScriptRoot" `
    "-FE$UnitTestOutput" `
    "-FU$UnitTestUnits" `
    (Join-Path $PSScriptRoot "codegen_test_runner.lpr")
  Assert-True ($LASTEXITCODE -eq 0) "Failed to compile codegen unit tests"

  & (Join-Path $UnitTestOutput "codegen_test_runner.exe") --all --format=plain | Out-Null
  Assert-True ($LASTEXITCODE -eq 0) "Codegen unit tests failed"
  Write-Host "Unit tests passed"

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
    (Join-Path $GoldenProject "src\GoldenDemo.lpr")
  Assert-True ($LASTEXITCODE -eq 0) "Failed to compile generated golden project"

  & (Join-Path $GoldenProject "src\GoldenDemo.exe") --help | Out-Null
  & (Join-Path $GoldenProject "src\GoldenDemo.exe") repo | Out-Null

  Write-Host "Compile smoke test passed"

  # Operations and path guard check
  $DemoProject = Join-Path $TmpDir "demo"
  & $GenExe init $DemoProject | Out-Null

  $SpecBeforeReinit = Normalize-Content (Join-Path $DemoProject "clifp.json")
  $null = (& $GenExe init $DemoProject 2>&1 | Out-String)
  Assert-True ($LASTEXITCODE -ne 0) "Expected init without --force to protect the existing project spec"
  Assert-True (
    (Normalize-Content (Join-Path $DemoProject "clifp.json")) -eq $SpecBeforeReinit
  ) "Init without --force modified the existing project spec"

  $null = (& $GenExe add command "repo/clone" --project $DemoProject 2>&1 | Out-String)
  Assert-True ($LASTEXITCODE -ne 0) "Expected a command name containing a path separator to fail"
  Assert-True (
    (Normalize-Content (Join-Path $DemoProject "clifp.json")) -eq $SpecBeforeReinit
  ) "Invalid add command modified the project spec"

  $DryRunOutput = (& $GenExe add command repo --project $DemoProject --description "Repo tools" --dry-run) | Out-String
  Assert-True ($DryRunOutput -match "Demo_Command_Repo\.pas") "Dry-run add did not preview the new command stub"
  Assert-True (-not ((Get-Content -Raw (Join-Path $DemoProject "clifp.json")) -match '"name"\s*:\s*"repo"')) "Dry-run add modified clifp.json"
  Assert-True (-not (Test-Path (Join-Path $DemoProject "src\commands\Demo_Command_Repo.pas"))) "Dry-run add created a command stub"

  & $GenExe add command repo --project $DemoProject --description "Repo tools" | Out-Null
  & $GenExe add command clone --parent repo --project $DemoProject --description "Clone repo" | Out-Null

  $RepoStub = Join-Path $DemoProject "src\commands\Demo_Command_Repo.pas"
  Add-Content -Path $RepoStub -Value "`n{ user customization }"
  & $GenExe generate --project $DemoProject | Out-Null
  Assert-True (
    (Get-Content -Raw $RepoStub) -match [regex]::Escape("{ user customization }")
  ) "Generate overwrote a user-owned command stub"

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
    $ProgramPath
  Assert-True ($LASTEXITCODE -eq 0) "Failed to compile generated descriptions project"

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

  $ManifestGuardProject = Join-Path $TmpDir "manifest-guard"
  & $GenExe init $ManifestGuardProject | Out-Null
  $ManifestOutsideDir = Join-Path $TmpDir "manifest-outside"
  New-Item -ItemType Directory -Path $ManifestOutsideDir | Out-Null
  $ManifestVictim = Join-Path $ManifestOutsideDir "victim.txt"
  Set-Content -Path $ManifestVictim -Value "protected"

  $ManifestPath = Join-Path $ManifestGuardProject "src\generated\.clifp-manifest.json"
  $Manifest = Get-Content -Raw $ManifestPath | ConvertFrom-Json
  $Manifest.generatedFiles = @("../manifest-outside/victim.txt")
  Write-JsonFile $ManifestPath $Manifest

  $null = (& $GenExe generate --project $ManifestGuardProject 2>&1 | Out-String)
  Assert-True ($LASTEXITCODE -ne 0) "Expected an out-of-project manifest entry to fail cleanup"
  Assert-True (Test-Path $ManifestVictim) "Manifest cleanup deleted a file outside the project directory"

  $LinkGuardProject = Join-Path $TmpDir "link-guard"
  $LinkGuardOutside = Join-Path $TmpDir "link-guard-outside"
  & $GenExe init $LinkGuardProject | Out-Null
  New-Item -ItemType Directory -Path $LinkGuardOutside | Out-Null
  $LinkGuardVictim = Join-Path $LinkGuardOutside "victim.txt"
  Set-Content -Path $LinkGuardVictim -Value "protected"
  $LinkGuardJunction = Join-Path $LinkGuardProject "linked"
  New-Item -ItemType Junction -Path $LinkGuardJunction -Target $LinkGuardOutside | Out-Null

  $LinkGuardManifestPath = Join-Path $LinkGuardProject "src\generated\.clifp-manifest.json"
  $LinkGuardManifest = Get-Content -Raw $LinkGuardManifestPath | ConvertFrom-Json
  $LinkGuardManifest.generatedFiles = @("linked/victim.txt")
  Write-JsonFile $LinkGuardManifestPath $LinkGuardManifest

  $LinkGuardOutput = (& $GenExe generate --project $LinkGuardProject 2>&1 | Out-String)
  Assert-True ($LASTEXITCODE -ne 0) "Expected a manifest entry through a junction to fail cleanup"
  Assert-True ($LinkGuardOutput -match "symbolic link or reparse point") "Generator did not report the linked manifest path"
  Assert-True (Test-Path $LinkGuardVictim) "Manifest cleanup followed a junction and deleted an external file"

  Write-Host "Ops test passed"
}
finally {
  if (($null -ne $LinkGuardJunction) -and (Test-Path -LiteralPath $LinkGuardJunction)) {
    [System.IO.Directory]::Delete($LinkGuardJunction)
  }
  if (Test-Path $TmpDir) {
    Remove-Item -Recurse -Force $TmpDir
  }
}
