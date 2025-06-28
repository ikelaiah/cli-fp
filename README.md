# Command-Line Interface Framework for Free Pascal 🚀

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Version](https://img.shields.io/badge/version-1.1.4-blue.svg)](https://github.com/ikelaiah/cli-fp/releases)
[![Free Pascal](https://img.shields.io/badge/Free%20Pascal-3.2.2-blue.svg)](https://www.freepascal.org/)
[![Lazarus](https://img.shields.io/badge/Lazarus-4.0-orange.svg)](https://www.lazarus-ide.org/)
[![GitHub stars](https://img.shields.io/github/stars/ikelaiah/cli-fp?style=social)](https://github.com/ikelaiah/cli-fp/stargazers)
[![GitHub issues](https://img.shields.io/github/issues/ikelaiah/cli-fp)](https://github.com/ikelaiah/cli-fp/issues)

A robust toolkit for building command-line (terminal) applications in Free Pascal. Leverage Pascal's strong typing and compile-time checks while creating sophisticated terminal tools with features like `git`-style commands, progress bars, and coloured output for better readability.

Combines Free Pascal's speed and reliability with professional-grade features. The object-oriented design handles the complex parts, letting you focus on your application's logic.

## 📑 Table of Contents

- [Command-Line Interface Framework for Free Pascal 🚀](#command-line-interface-framework-for-free-pascal-)
  - [📑 Table of Contents](#-table-of-contents)
  - [✨ Features](#-features)
  - [🚀 Quick Start](#-quick-start)
  - [🎯 Parameter Types and Validation](#-parameter-types-and-validation)
    - [Basic Types](#basic-types)
    - [Boolean and Flags](#boolean-and-flags)
    - [Complex Types](#complex-types)
    - [Validation Rules](#validation-rules)
  - [📖 Screenshots](#-screenshots)
  - [📖 System Requirements](#-system-requirements)
    - [Tested Environments](#tested-environments)
    - [Theoretical Compatibility](#theoretical-compatibility)
    - [Dependencies](#dependencies)
    - [Build Requirements](#build-requirements)
  - [📖 Documentation](#-documentation)
  - [🎯 Use Cases](#-use-cases)
  - [🤝 Contributing](#-contributing)
  - [📝 License](#-license)
  - [🙏 Acknowledgments](#-acknowledgments)
  - [🧩 Bash Completion Script (`--completion-file`)](#-bash-completion-script---completion-file)

## ✨ Features

- 🎯 **Command & Subcommand Support**: Organize complex CLIs with hierarchical commands
- 🔍 **Smart Parameter Handling**: Automatic validation and type checking
- 📊 **Progress Indicators**: Built-in spinners and progress bars
- 🎨 **Colored Output**: Rich console output with basic color support
- 📚 **Comprehensive Help System**: Auto-generated help with examples
- 🛡️ **Type-Safe**: Interface-based design with strong typing
- 🔌 **Extensible**: Easy to extend with custom commands and parameters
- **Modern Command-Line Interface**
  - Subcommand support (e.g., `app repo init`, `app repo clone`)
  - Short and long flags (`-h`, `--help`)
  - Automatic help generation
  - Colored output support
  - **Advanced Bash Completion**: Generate a Bash completion script for your CLI with `--completion-file` (see below for safe usage)
- **Robust Error Handling**
  - Clear error messages for unknown commands and subcommands
  - Validation of command-line flags and parameters
  - Helpful suggestions when errors occur
  - Context-aware help display
- **Developer-Friendly**
  - Interface-based design
  - Easy command registration
  - Extensible parameter system
  - Built-in progress indicators
- **User-Friendly**
  - Consistent help formatting
  - Command suggestions
  - Default values support
  - Required parameter validation

## 🚀 Quick Start

1. **Installation**

No complex build system needed! Just:

> **Note:** All example builds output their executables and units to the `example-bin/` folder in the repository root for easy access and cleanup.
>
> **Tip:** To build or clean all example projects at once, use the provided scripts:
>
> - On **Linux/macOS**: `./compile-all-examples.sh` and `./clean-all-examples.sh`
> - On **Windows**: `./compile-all-examples.ps1` and `./clean-all-examples.ps1`


```bash
# Clone the repository
git clone https://github.com/ikelaiah/cli-fp.git

# Or copy the source files to your project's directory
```

2. **Use in Your Project**

- Add the source directory to your project's search path (Project -> Project Options ... -> Compiler Options -> Paths -> Other unit files)
- Add the units to your uses clause:

```pascal
uses
  CLI.Interfaces,    // Core interfaces
  CLI.Application,   // Main application framework
  CLI.Command,       // Base command implementation
  CLI.Parameter,     // Parameter handling
  CLI.Progress,      // Optional: Progress indicators
  CLI.Console;       // Optional: Colored console output
```

3. **Create Your First CLI App**

```pascal
program MyApp;

{$mode objfpc}{$H+}{$J-}

uses
  SysUtils,
  CLI.Interfaces,
  CLI.Application,
  CLI.Command;

type
  // Define a new command
  TGreetCommand = class(TBaseCommand)
  public
    function Execute: integer; override;
  end;

  function TGreetCommand.Execute: integer;
  var
    Name: string;
  begin
    // Get parameter value using helper method
    if GetParameterValue('--name', Name) then
      WriteLn('Hello, ', Name, '!')
    else
      WriteLn('Hello, World!');
    Result := 0;
  end;

{ Main program }
var
  App: ICLIApplication;
  Cmd: TGreetCommand;

begin
  App := CreateCLIApplication('MyApp', '1.0.0');
  
  // Create and configure command
  Cmd := TGreetCommand.Create('greet', 'Say hello');
  Cmd.AddStringParameter('-n', '--name', 'Name to greet', False, 'World');
  
  // Register command
  App.RegisterCommand(Cmd);
  
  // Execute application
  ExitCode := App.Execute;
end.
```

**Output:**
```
$ ./myapp greet --name "John"
Hello, John!

$ ./myapp greet
Hello, World!

$ ./myapp greet --help
Usage: myapp greet [options]

Say hello

Options:
  -n, --name           Name to greet
      Default: World
  -h, --help          Show this help message
```

**Lazarus users:**
A runtime-only Lazarus package is provided in `packages/lazarus/cli_fp.lpk`.
To use it, open the `.lpk` file in Lazarus, click “Compile,” then click “Add” to add it to your project’s required packages.

## 🎯 Parameter Types and Validation

The framework provides comprehensive type-safe parameter handling with built-in validation:

### Basic Types

```pascal
// String parameter
Cmd.AddStringParameter('-n', '--name', 'Name to greet');

// Integer parameter (required)
Cmd.AddIntegerParameter('-c', '--count', 'Number of items', True);

// Float parameter with default
Cmd.AddFloatParameter('-r', '--rate', 'Processing rate', False, '1.0');
```

### Boolean and Flags

```pascal
// Flag (true when present, false by default)
Cmd.AddFlag('-v', '--verbose', 'Enable verbose output'); // Standard CLI behavior

// Boolean parameter (explicit true/false)
Cmd.AddBooleanParameter('-d', '--debug', 'Enable debug mode', False, 'false');
```

> **Note:** By default, flags created with `AddFlag` are `false` unless present on the command line. If you specify a default value of `'true'`, the flag will be `true` even if not present, which is nonstandard for CLI flags and not recommended unless you have a specific use case.

### Complex Types

```pascal
// DateTime (YYYY-MM-DD HH:MM)
Cmd.AddDateTimeParameter('-d', '--date', 'Start date');

// Enum with allowed values
Cmd.AddEnumParameter('-l', '--level', 'Log level', 'debug|info|warn|error');

// URL with protocol validation
Cmd.AddUrlParameter('-u', '--url', 'Repository URL');

// Array (comma-separated)
Cmd.AddArrayParameter('-t', '--tags', 'Tag list');

// Password (masked in output)
Cmd.AddPasswordParameter('-k', '--api-key', 'API Key');
```

### Validation Rules

Each parameter type has built-in validation:

- `String`: No validation
- `Integer`: Must be a valid integer number
- `Float`: Must be a valid floating-point number
- `Boolean`: Must be 'true' or 'false' (case-insensitive)
- `DateTime`: Must be in format "YYYY-MM-DD HH:MM" (24-hour)
- `Enum`: Must match one of the pipe-separated allowed values
- `URL`: Must start with http://, https://, git://, or ssh://
- `Array`: No validation on individual items
- `Password`: No validation, but value is masked in output

## 📖 Screenshots

![ColorDemo Help](docs/images/colordemo-help.png)
![ColorDemo Greeting](docs/images/colordemo-greeting.png)
*Above: The ColorDemo example showing professional CLI styling with colors, Unicode characters, and progress indicators.*

## 📖 System Requirements

### Tested Environments

- **Operating System**: Windows 11, Ubuntu 24.04
- **Compiler**: Free Pascal (FPC) 3.2.2
- **IDE**: Lazarus 3.6, Lazarus 4.0

### Theoretical Compatibility

- **Operating Systems**:
  - Windows (7, 8, 10, 11)
  - Linux (Any distribution with FPC support)
  - macOS (with FPC support)
  - FreeBSD
- **Compiler**: Free Pascal 3.2.2 or higher
- **IDE & Editor**: Any IDE that supports Free Pascal
  - Lazarus 3.6 or higher
  - VS Code with Pascal extensions
  - Other text editors

### Dependencies

- No external dependencies required
- Uses only standard Free Pascal RTL units

### Build Requirements

- Free Pascal Compiler (FPC) 3.2.2+
- Lazarus 3.6+
- Basic development tools (git, terminal, etc)

## 📖 Documentation

- [User Manual](docs/user-manual.md): Complete guide for using the framework, *including a cheat sheet*
- [API Reference](docs/api-reference.md): Detailed API reference for the framework
- [Technical Documentation](docs/technical-docs.md): Architecture and implementation details
- [Examples](examples/): Working example applications
- [Changelog](CHANGELOG.md): Version history and updates

## 🎯 Use Cases

Perfect for building:

- Version Control Systems
- Build Tools
- Package Managers
- Development Tools
- System Utilities
- DevOps Tools

## 🤝 Contributing

Contributions are welcome! Please feel free to submit a Pull Request. For major changes, please open an issue first to discuss what you would like to change.

1. Fork the Project
2. Create your Feature Branch (`git checkout -b feature/AmazingFeature`)
3. Commit your Changes (`git commit -m 'Add some AmazingFeature'`)
4. Push to the Branch (`git push origin feature/AmazingFeature`)
5. Open a Pull Request

## 📝 License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## 🙏 Acknowledgments

- Inspired by modern CLI frameworks
- Built with Free Pascal and Lazarus IDE
- Thanks to the Free Pascal community for their support and contributions

## 🧩 Bash Completion Script (`--completion-file`)

Generate a Bash completion script for your CLI with:

```bash
./yourcli --completion-file > myapp-completion.sh
```

- **Root level:** All global flags (`--help`, `-h`, `--help-complete`, `--version`, `--completion-file`) are offered.
- **Subcommands:** Only `-h` and `--help` are offered as global flags.
- **Completions are always context-aware**—only valid subcommands and parameters for the current path are suggested.

> This matches the CLI's actual argument parsing and ensures completions are always valid. See the user manual for full details and safe usage instructions.