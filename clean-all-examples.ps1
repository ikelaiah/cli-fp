# clean-all-examples.ps1
# Remove generated example build artifacts while preserving tracked files.

$RootDir = Split-Path -Parent $PSCommandPath

$Examples = @(
    'ColorDemo',
    'ErrorHandlingDemo',
    'LongRunningOpDemo',
    'ProgressDemo',
    'RootCommandDemo',
    'SimpleDemo',
    'SubCommandDemo'
)
$GeneratedExtensions = @(
    '.o', '.ppu', '.compiled', '.or', '.a', '.rst', '.res', '.dbg', '.tds', '.lps'
)
$GeneratedFileNames = @('link.res')
foreach ($Example in $Examples) {
    $GeneratedFileNames += $Example
    $GeneratedFileNames += "$Example.exe"
}

function Remove-GeneratedArtifacts([string]$Path) {
    if (-not (Test-Path -LiteralPath $Path)) {
        return
    }

    Get-ChildItem -LiteralPath $Path -File -Recurse -Force |
        Where-Object {
            ($GeneratedExtensions -contains $_.Extension.ToLowerInvariant()) -or
            ($_.Name -like '*.lps.bak') -or
            ($GeneratedFileNames -contains $_.Name)
        } |
        ForEach-Object {
            Write-Host "🧹 Removing generated artifact: $($_.FullName)" -ForegroundColor Yellow
            Remove-Item -LiteralPath $_.FullName -Force
        }
}

Remove-GeneratedArtifacts (Join-Path $RootDir 'example-bin')
foreach ($Example in $Examples) {
    Remove-GeneratedArtifacts (Join-Path $RootDir "examples\$Example")
}

Write-Host "✅ Generated example build artifacts removed." -ForegroundColor Green
