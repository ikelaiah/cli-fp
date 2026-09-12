# Example Binaries

This directory tracks generated completion-script examples. Precompiled example
executables are not committed; local example builds place their output here.

## Available Examples

### Canonical Examples
- **QuickStartDemo** - Smallest root-command CLI
- **SimpleDemo** - Basic CLI with parameters, spinner, and colored output
- **ColorDemo** - Professional colored output with decorative formatting
- **ProgressDemo** - Spinner and progress bar demonstrations
- **RootCommandDemo** - Command-less root execution with optional named commands
- **SubCommandDemo** - Hierarchical commands (git-like structure)
- **ErrorHandlingDemo** - Error handling patterns
- **LongRunningOpDemo** - Advanced parameter types

## Shell Completion Scripts

Pre-generated completion scripts for the examples:

### Bash Completion
- `simpledemo_completion.bash` - SimpleDemo Bash completion
- `subcommanddemo_completion.bash` - SubCommandDemo Bash completion

**Usage:**
```bash
source simpledemo_completion.bash
./SimpleDemo.exe [TAB][TAB]
```

### PowerShell Completion
- `simpledemo_completion.ps1` - SimpleDemo PowerShell completion

**Usage:**
```powershell
. .\simpledemo_completion.ps1
.\SimpleDemo.exe [TAB]
```

## Generating Completion Scripts

Any cli-fp application can generate its own completion scripts:

```bash
# Bash
./YourApp.exe --completion-file > yourapp_completion.bash
source yourapp_completion.bash

# PowerShell
./YourApp.exe --completion-file-pwsh > yourapp_completion.ps1
. .\yourapp_completion.ps1
```

## Running Examples

Each example includes `--help` to show usage:

```bash
./SimpleDemo.exe --help
./QuickStartDemo.exe --name Gus
./RootCommandDemo.exe --name Gus
./SubCommandDemo.exe repo --help
./ProgressDemo.exe process --help
```

## Source Code

Example source code is located in the `examples/` directory:
- `examples/SimpleDemo/`
- `examples/QuickStartDemo/`
- `examples/ColorDemo/`
- `examples/ProgressDemo/`
- `examples/RootCommandDemo/`
- `examples/SubCommandDemo/`
- `examples/ErrorHandlingDemo/`
- `examples/LongRunningOpDemo/`

## Rebuilding Examples

To rebuild any example:

```bash
cd examples/SimpleDemo
lazbuild -B SimpleDemo.lpi
```

The executable will be placed in `example-bin/`.

## Directory Contents

- `*_completion.bash` files - Bash completion scripts
- `*_completion.ps1` files - PowerShell completion scripts
- locally-built executables and `lib/` units (ignored by Git)

Build examples from source with `compile-all-examples.ps1` or
`compile-all-examples.sh`. Generated executables remain local and can be
removed with the matching cleanup script.

## Documentation

For detailed completion documentation, see:
- [docs/completion.md](../docs/completion.md) - Completion usage
- [docs/completion-testing/](../docs/completion-testing/) - Historical testing documentation

## Notes

- Source code in `examples/` is the authoritative version
- Rebuild locally after framework changes
- Completion scripts should be regenerated after rebuilding
