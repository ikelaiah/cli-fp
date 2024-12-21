# CLI Framework for Free Pascal 🚀

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Free Pascal](https://img.shields.io/badge/Free%20Pascal-3.2.2-blue.svg)](https://www.freepascal.org/)
[![Lazarus](https://img.shields.io/badge/Lazarus-3.6-orange.svg)](https://www.lazarus-ide.org/)


A robust Free Pascal framework for building professional CLI applications. Create powerful command-line tools with hierarchical commands, rich interactions, and comprehensive help systems - all with type-safe, object-oriented design.

## 📑 Table of Contents

- [CLI Framework for Free Pascal 🚀](#cli-framework-for-free-pascal-)
  - [📑 Table of Contents](#-table-of-contents)
  - [✨ Features](#-features)
  - [🚀 Quick Start](#-quick-start)
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
  - [📞 Contact](#-contact)


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

```bash
# Clone the repository
git clone https://github.com/yourusername/cli-fp.git

# Copy the source files to your project's directory
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

{$mode objfpc}{$H+}

uses
  SysUtils, CLI.Interfaces, CLI.Application, CLI.Command;

type
  TGreetCommand = class(TBaseCommand)
  public
    function Execute: Integer; override;
  end;

function TGreetCommand.Execute: Integer;
begin
  WriteLn('Hello, CLI World!');
  Result := 0;
end;

var
  App: ICLIApplication;
  Cmd: TGreetCommand;
begin
  App := CreateCLIApplication('MyApp', '1.0.0');
  Cmd := TGreetCommand.Create('greet', 'Say hello');
  App.RegisterCommand(Cmd);
  ExitCode := App.Execute;
end.
```

**Output:**

```
$ ./MyApp.exe
MyApp version 1.0.0

Usage:
  MyApp.exe <command> [options]

Commands:
  greet          Say hello

Global Options:
  -h, --help           Show this help message
  --help-complete      Show complete reference for all commands
  -v, --version        Show version information

Examples:
  Get help for commands:
    MyApp.exe <command> --help

  Available command help:
    MyApp.exe greet --help
```

```
$ ./MyApp.exe greet
Hello, CLI World!
``` 

That's it! No makefiles, no complex configuration, no external dependencies. 

Just pure Object Pascal code.

## 📖 Screenshots

![ColorDemo Help](docs/images/colordemo-help.png)
![ColorDemo Greeting](docs/images/colordemo-greeting.png)
*Above: The ColorDemo example showing professional CLI styling with colors, Unicode characters, and progress indicators.*

## 📖 System Requirements

### Tested Environments
- **Operating System**: Windows 11
- **Compiler**: Free Pascal (FPC) 3.2.2
- **IDE**: Lazarus 3.6

### Theoretical Compatibility
- **Operating Systems**:
  - Windows (7, 8, 10, 11)
  - Linux (Any distribution with FPC support)
  - macOS (with FPC support)
  - FreeBSD
- **Compiler**: Free Pascal 3.2.2 or higher
- **IDE**: Any IDE that supports Free Pascal
  - Lazarus 3.6 or higher
  - VS Code with OmniPascal
  - Other text editors

### Dependencies
- No external dependencies required
- Uses only standard Free Pascal RTL units

### Build Requirements
- Free Pascal Compiler (FPC) 3.2.2+
- Lazarus 3.6+
- Basic development tools (git, terminal, etc)

## 📖 Documentation

- [Beginners Guide](docs/beginners-guide.md): A guide for beginners to get started with the framework
- [User Manual](docs/user-manual.md): Complete guide for using the framework
- [API Reference](docs/api-reference.md): Detailed API reference for the framework
- [Technical Documentation](docs/technical-docs.md): Architecture and implementation details
- [Examples](examples/): Working example applications

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

## 📞 Contact

Your Name - [ikelaiah](https://github.com/ikelaiah)

Project Link: [https://github.com/ikelaiah/cli-fp](https://github.com/ikelaiah/cli-fp) 


