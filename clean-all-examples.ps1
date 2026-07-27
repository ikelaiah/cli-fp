# clean-all-examples.ps1
# Remove all built example binaries and unit output

$exampleBin = "example-bin"
if (Test-Path $exampleBin) {
    Write-Host "🧹 Removing example-bin/ directory..." -ForegroundColor Yellow
    Remove-Item $exampleBin -Recurse -Force
    Write-Host "✅ example-bin/ cleaned." -ForegroundColor Green
} else {
    Write-Host "ℹ️  example-bin/ does not exist. Nothing to clean." -ForegroundColor Gray
}

$examples = @(
    'ColorDemo',
    'ErrorHandlingDemo',
    'LongRunningOpDemo',
    'ProgressDemo',
    'RootCommandDemo',
    'SimpleDemo',
    'SubCommandDemo'
)

foreach ($ex in $examples) {
    $libPath = "examples/$ex/lib"
    if (Test-Path $libPath) {
        Write-Host "🧹 Removing old lib/ from examples/$ex..." -ForegroundColor Yellow
        Remove-Item $libPath -Recurse -Force
    }
    $win64Path = "examples/$ex/x86_64-win64"
    if (Test-Path $win64Path) {
        Write-Host "🧹 Removing old x86_64-win64/ from examples/$ex..." -ForegroundColor Yellow
        Remove-Item $win64Path -Recurse -Force
    }
    $linuxPath = "examples/$ex/x86_64-linux"
    if (Test-Path $linuxPath) {
        Write-Host "🧹 Removing old x86_64-linux/ from examples/$ex..." -ForegroundColor Yellow
        Remove-Item $linuxPath -Recurse -Force
    }
    $backupPath = "examples/$ex/backup"
    if (Test-Path $backupPath) {
        Write-Host "🧹 Cleaning backup/ in examples/$ex..." -ForegroundColor Yellow
        Get-ChildItem $backupPath -Include *.exe,*.dbg,*.o,*.ppu -Recurse | Remove-Item -Force
    }
}

Write-Host "`n✅ Cleanup complete." -ForegroundColor Green
