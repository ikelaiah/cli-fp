# Example Binaries

This directory contains pre-compiled example executables demonstrating the cli-fp framework.

## Available Examples

### Core Examples
- **SimpleDemo.exe** - Basic CLI with parameters, spinner, and colored output
- **ColorDemo.exe** - Professional colored output with decorative formatting
- **ProgressDemo.exe** - Spinner and progress bar demonstrations
- **RootCommandDemo.exe** - Command-less root execution with optional named commands
- **SubCommandDemo.exe** - Hierarchical commands (git-like structure)
- **ErrorHandlingDemo.exe** - Error handling patterns
- **LongRunningOpDemo.exe** - Advanced parameter types

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
./RootCommandDemo.exe --name Gus
./SubCommandDemo.exe repo --help
./ProgressDemo.exe process --help
```

## Source Code

Example source code is located in the `examples/` directory:
- `examples/SimpleDemo/`
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
- `lib/` - Shared compiled units (can be ignored)

**Note:** Pre-compiled binaries are no longer included in this directory. Users should compile examples from source using the build scripts in the repository root (`compile-all-examples.ps1` or `compile-all-examples.sh`).

## Documentation

For detailed completion documentation, see:
- [docs/completion-testing/](../docs/completion-testing/) - Testing documentation
- [docs/completion-testing/BASH_COMPLETION_GUIDE.md](../docs/completion-testing/BASH_COMPLETION_GUIDE.md) - User guide

## Notes

- These are pre-built binaries for convenience
- Source code in `examples/` is the authoritative version
- Rebuild after framework changes
- Completion scripts should be regenerated after rebuilding
