# compile-all-examples.ps1
# Build all example projects in the examples/ folder using lazbuild
# Usage: ./compile-all-examples.ps1 [lazbuild flags]

param(
    [Parameter(ValueFromRemainingArguments=$true)]
    [string[]]$LazbuildArgs
)

function Check-Lazbuild {
    if (-not (Get-Command lazbuild -ErrorAction SilentlyContinue)) {
        Write-Host "❌ Error: lazbuild not found in PATH. Please install Lazarus and ensure lazbuild is available." -ForegroundColor Red
        exit 1
    }
}

$examples = @(
    'BooleanTest',
    'ColorDemo',
    'ErrorHandlingDemo',
    'LongRunningOpDemo',
    'MyApp',
    'MyGit',
    'ProgressDemo',
    'SimpleDemo',
    'SubCommandDemo',
    'TestFlags'
)

Check-Lazbuild

foreach ($ex in $examples) {
    Write-Host "`n🔨 Building $ex ..." -ForegroundColor Cyan
    lazbuild "examples/$ex/$ex.lpi" @LazbuildArgs
    if ($LASTEXITCODE -ne 0) {
        Write-Host "❌ Build failed for $ex" -ForegroundColor Red
        exit 1
    } else {
        Write-Host "✅ $ex built successfully." -ForegroundColor Green
    }
}

Write-Host "`n🎉 All examples built successfully!" -ForegroundColor Green
