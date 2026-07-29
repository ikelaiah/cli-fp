# Command-Line Interface Framework for Free Pascal 🚀

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Version](https://img.shields.io/badge/version-1.3.2-blue.svg)](https://github.com/ikelaiah/cli-fp/releases)
[![Free Pascal](https://img.shields.io/badge/Free%20Pascal-3.2.2-blue.svg)](https://www.freepascal.org/)
[![Lazarus](https://img.shields.io/badge/Lazarus-4.0-orange.svg)](https://www.lazarus-ide.org/)
[![GitHub stars](https://img.shields.io/github/stars/ikelaiah/cli-fp?style=social)](https://github.com/ikelaiah/cli-fp/stargazers)
[![GitHub issues](https://img.shields.io/github/issues/ikelaiah/cli-fp)](https://github.com/ikelaiah/cli-fp/issues)

Build native command-line tools in Object Pascal.

`cli-fp` adds command trees, typed option metadata, generated help, Bash and
PowerShell completion, progress indicators, and coloured output to an ordinary
Free Pascal program. The core framework uses standard FPC units and produces
a normal native executable—there is no separate application runtime to deploy.

The repository is also a practical Free Pascal showcase: it contains an
interface-based framework, a JSON-driven source generator, platform-aware
console code, shell-completion protocols, and an FPCUnit test suite. CI builds
and tests the project on Windows and Linux.

## Start Here

| If you are... | Start with... |
| --- | --- |
| New to Free Pascal | [Install Free Pascal](#install-free-pascal), then read [Free Pascal in two minutes](#free-pascal-in-two-minutes) |
| New to `cli-fp` | [Build your first generated CLI](#build-your-first-generated-cli) |
| Adding CLI support to an existing Pascal program | [Manual Quick Start](#manual-quick-start) |
| Using Lazarus | [Lazarus setup](#lazarus-setup) |
| Looking for a specific API or design detail | [Documentation map](#documentation-map) |

## Install Free Pascal

`cli-fp` targets Free Pascal 3.2.2. Install FPC from the
[official download page](https://docs.freepascal.org/download.html), or use an
installation supplied with Lazarus. Confirm the compiler is available:

```text
fpc -iV
```

The command should print `3.2.2`. The
[official Free Pascal manuals](https://www.freepascal.org/docs.html) include a
language reference, compiler user guide, RTL reference, and FCL reference.

Some Linux distributions split the compiler and Free Component Library into
separate packages. On Debian and Ubuntu, the same packages used by this
project's CI can be installed with:

```bash
sudo apt-get install fp-compiler fp-units-fcl
```

The FCL package supplies the JSON units used by `cli-fp-gen`.

## Build Your First Generated CLI

This path creates, compiles, and runs a real application before you need to
understand the framework internals.

### Linux/macOS with Bash

```bash
git clone https://github.com/ikelaiah/cli-fp.git
cd cli-fp
fpc -Futools/cli-fp-gen/src tools/cli-fp-gen/cli_fp_gen.lpr
./tools/cli-fp-gen/cli_fp_gen init ./build-temp/myapp --name myapp
cd build-temp/myapp
fpc -Fu../../src -Fu./src -Fu./src/generated -Fu./src/commands ./src/Myapp.lpr
./src/Myapp greet --help
```

### Windows with PowerShell

```powershell
git clone https://github.com/ikelaiah/cli-fp.git
Set-Location .\cli-fp
fpc "-Futools\cli-fp-gen\src" .\tools\cli-fp-gen\cli_fp_gen.lpr
.\tools\cli-fp-gen\cli_fp_gen.exe init .\build-temp\myapp --name myapp
Set-Location .\build-temp\myapp
fpc "-Fu..\..\src" "-Fu.\src" "-Fu.\src\generated" "-Fu.\src\commands" .\src\Myapp.lpr
.\src\Myapp.exe greet --help
```

You now have a native executable with a registered `greet` command, generated
usage text, a typed `--name` option, and no application runtime dependency.

### Make the Generated Command Yours

Open `src/commands/Myapp_Command_Greet.pas`. Command files are user-owned, so
the generator will not overwrite your implementation during normal
regeneration. Replace the generated `Execute` method with:

```pascal
function TGreetCommand.Execute: Integer;
var
  PersonName: string;
begin
  if not GetParameterValue('--name', PersonName) then
    PersonName := 'World';

  WriteLn('Hello, ', PersonName, '!');
  Result := 0;
end;
```

Compile again with the same `fpc` command. On Linux or macOS, run:

```bash
./src/Myapp greet --name Ada
```

On Windows, run:

```powershell
.\src\Myapp.exe greet --name Ada
```

```text
Hello, Ada!
```

That small command already uses class inheritance, a virtual method, generated
parameter registration, framework parsing and defaults, and native
compilation.

## Free Pascal in Two Minutes

You only need a few conventions to follow this project:

| Pascal concept | Meaning in this repository |
| --- | --- |
| `.lpr` file | The program entry point—the file passed to `fpc` |
| `.pas` file | Usually a reusable unit containing classes or other declarations |
| `uses` | Imports units, similar to importing modules in other languages |
| `-Fu<path>` | Adds a directory to FPC's unit search path |
| `{$mode objfpc}` | Enables the Object Pascal language mode used by `cli-fp` |
| `TBaseCommand` descendant | A command class whose `Execute` method performs the work |
| `Halt(App.Execute)` | Returns the framework's exit code to the operating system |

Free Pascal gives this library strong typing, classes, interfaces, generics,
exceptions, deterministic `try..finally` cleanup, conditional compilation, and
native binaries. `cli-fp` packages those capabilities into a focused CLI
developer experience.

## Choose Your CLI Shape

Use a root command for a focused utility, named commands for a `git`-style
application, or combine both:

| Invocation | What runs | Option ownership |
| --- | --- | --- |
| `MyApp` | Root action with its defaults | Root command |
| `MyApp --name Gus` | Root action | Root command |
| `MyApp about` | Named `about` command | `about` command |
| `MyApp about --verbose` | Named `about` command | `about` command |
| `MyApp --help` | General application help, including root options | Application-level |

Root options belong to the default action; they are not inherited by named
commands. Built-in requests have distinct scopes:

- Used alone, `--help`/`-h` shows general application help,
  `--help-complete` shows the complete command reference, and
  `--version`/`-v` prints version information.
- After a named command or subcommand, `--help`/`-h` shows help for that
  selected level.
- Completion-script generation is handled at application level when
  `--completion-file` or `--completion-file-pwsh` is the first argument.

## Feature Tour

- **Commands and subcommands:** Build command trees such as
  `app repo remote add`.
- **Optional root commands:** Build focused applications that run as
  `app [options]`, while retaining named commands when useful.
- **Typed parameter metadata:** Define strings, numbers, paths, URLs, enums,
  passwords, arrays, booleans, and date/time values, with type-specific
  validation where implemented.
- **Generated help:** Show usage, defaults, required options, subcommands, and
  a complete command reference.
- **Shell completion:** Generate Bash and PowerShell scripts with contextual
  command, option, Boolean, and enum suggestions.
- **Terminal UX:** Use ANSI/Windows colour support, seven spinner styles, and
  progress bars with inline captions.
- **Project generation:** Initialize projects and add or remove nested commands
  from a versioned `clifp.json` specification.
- **Testable design:** Exercise argument handling without mutating the process
  command line; the repository includes framework and generator suites.

## Contents

- [Build Your First Generated CLI](#build-your-first-generated-cli)
- [Free Pascal in Two Minutes](#free-pascal-in-two-minutes)
- [Choose Your CLI Shape](#choose-your-cli-shape)
- [Feature Tour](#feature-tour)
- [Manual Quick Start](#manual-quick-start)
- [Root Commands](#root-commands)
- [Parameter Types and Validation](#parameter-types-and-validation)
- [Shell Completion](#shell-completion)
- [System Requirements](#system-requirements)
- [Documentation Map](#documentation-map)

## Manual Quick Start

1. **Get the source**

```bash
git clone https://github.com/ikelaiah/cli-fp.git
```

You can also download a release archive. Keep the repository's `src/`
directory available to your project.

2. **Add the framework to your project**

Add `cli-fp/src` to FPC's unit search path with `-Fu`, or in Lazarus open
**Project → Project Options → Compiler Options → Paths → Other unit files**.
Then add the units you need:

```pascal
uses
  CLI.Interfaces,    // Core interfaces
  CLI.Application,   // Main application framework
  CLI.Command,       // Base command implementation
  CLI.Parameter,     // Parameter handling
  CLI.Progress,      // Optional: Progress indicators
  CLI.Console;       // Optional: Colored console output
```

3. **Create your first CLI app**

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
    PersonName: string;
  begin
    // Get parameter value using helper method
    if GetParameterValue('--name', PersonName) then
      WriteLn('Hello, ', PersonName, '!')
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
  Halt(App.Execute);
end.
```

Save the file as `MyApp.lpr`. If the `cli-fp` repository is next to your
project directory, compile and run it with:

```bash
fpc -Fu../cli-fp/src MyApp.lpr
./MyApp greet --name "John"
```

On Windows, run the generated executable as `.\MyApp.exe`.

**Output:**
```
$ ./MyApp greet --name "John"
Hello, John!

$ ./MyApp greet
Hello, World!

$ ./MyApp greet --help
Usage: MyApp greet [options]

Say hello

Options:
  -n, --name           Name to greet
      Default: World
  -h, --help          Show this help message
```

### Lazarus Setup

A runtime-only Lazarus package is provided in `packages/lazarus/cli_fp.lpk`.
To use it, open the `.lpk` file in Lazarus, click “Compile,” then click “Add” to add it to your project’s required packages.

## Root Commands

A root command is the application's unnamed default action. It lets a focused
utility run with defaults or accept options directly after the executable,
without inventing a command name solely to hold those options:

```text
MyApp
MyApp --name Gus
```

It is still an ordinary command, so its options use the same parameter,
validation, help, and completion APIs as named commands. Create it with an
empty name and pass it to the three-argument application factory:

```pascal
var
  App: ICLIApplication;
  RootCommand: TGreetCommand;
begin
  RootCommand := TGreetCommand.Create('', 'Greet someone');
  RootCommand.AddStringParameter('-n', '--name', 'Name to greet',
    False, 'World');

  App := CreateCLIApplication('MyApp', '1.0.0', RootCommand);
  Halt(App.Execute);
end.
```

The root command is explicit and optional. Applications using the existing
two-argument `CreateCLIApplication` continue to show general help when invoked
without a named command.

With a root command configured, no arguments or a leading root option selects
the root action. A registered command name still selects that named command.
Root parameters are not application-global and are not automatically available
to named commands. The framework does not currently model positional arguments
or Cobra-style persistent flags.

Named commands can coexist with a root command:

```text
MyApp --name Gus
MyApp about
```

See [`RootCommandDemo`](examples/RootCommandDemo/RootCommandDemo.lpr) for a
complete example.

## Parameter Types and Validation

The framework records parameter types for validation and help generation.
Command implementations retrieve the resulting values as strings and convert
them as needed:

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

// File or directory path
Cmd.AddPathParameter('-p', '--path', 'Target path');

// Array (comma-separated)
Cmd.AddArrayParameter('-t', '--tags', 'Tag list');

// Password (stored as a string; handle it as sensitive data)
Cmd.AddPasswordParameter('-k', '--api-key', 'API Key');
```

### Validation Rules

Each parameter type has built-in validation:

- `String`: No validation
- `Integer`: Must be a valid integer number
- `Float`: Must be a valid floating-point number
- `Flag`: Presence sets the value; absent flags use their default
- `Boolean`: Must be 'true' or 'false' (case-insensitive)
- `DateTime`: Parsed with `TryStrToDateTime`; `YYYY-MM-DD HH:MM` is the recommended portable input
- `Enum`: Must match one of the pipe-separated allowed values
- `URL`: Must start with http://, https://, git://, or ssh://
- `Path`: No path-existence validation
- `Array`: No validation on individual items
- `Password`: No validation or automatic output redaction

## Shell Completion

Generate a completion script from your application:

```bash
./yourcli --completion-file > myapp-completion.sh
```

```powershell
.\yourcli.exe --completion-file-pwsh > myapp-completion.ps1
```

Completion is context-aware for root options, commands, subcommands, and their
parameters. Boolean values complete as `true` or `false`, and enum values use
their configured choices. At an empty root prompt, named commands are suggested
first; type `-` or `--` to request root and global options.

See the [user manual](docs/user-manual.md#bash-completion) for safe installation
instructions and [`docs/completion-testing/`](docs/completion-testing/) for the
detailed Bash and PowerShell verification guides.

## Screenshots

![ColorDemo Help](docs/images/colordemo-help.png)
![ColorDemo Greeting](docs/images/colordemo-greeting.png)
*Above: The ColorDemo example showing professional CLI styling with colors, Unicode characters, and progress indicators.*

## System Requirements

### Tested Environments

- **Linux CI:** `ubuntu-latest` with the distribution's `fp-compiler` and
  `fp-units-fcl` packages
- **Windows CI:** `windows-latest` with Free Pascal 3.2.2 from Lazarus 4.0

The CI workflow compiles and runs both the framework tests and generator
tests. Lazarus itself is used to supply the Windows toolchain; the package is
runtime-only and does not require Lazarus at application runtime.

### Other Platforms

The console implementation has Windows and Unix code paths, so macOS,
FreeBSD, and other FPC-supported Unix systems are expected to be viable, but
they are not currently exercised by this repository's CI. Reports and fixes
from those platforms are welcome.

### Dependencies

- No third-party libraries are required
- The framework uses units supplied with Free Pascal
- The generator additionally uses the standard FCL JSON units

### Build Requirements

- Free Pascal Compiler (the project is tested with FPC 3.2.2)
- Lazarus only when using the Lazarus IDE or supplied package
- Git only when cloning the repository rather than using a release archive

## Documentation Map

- [Documentation home](docs/README.md): Choose a guide by goal or experience
- [User manual](docs/user-manual.md): Build applications and use framework features
- [Code generator](docs/codegen.md): Generate a project and evolve its command tree
- [API reference](docs/api-reference.md): Look up public types and methods
- [Technical documentation](docs/technical-docs.md): Understand architecture and internals
- [Examples](examples/): Explore focused, compilable applications
- [Changelog](CHANGELOG.md): Review version history and updates

## Use Cases

Perfect for building:

- Version Control Systems
- Build Tools
- Package Managers
- Development Tools
- System Utilities
- DevOps Tools

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request. For major changes, please open an issue first to discuss what you would like to change.

1. Fork the Project
2. Create your Feature Branch (`git checkout -b feature/AmazingFeature`)
3. Commit your Changes (`git commit -m 'Add some AmazingFeature'`)
4. Push to the Branch (`git push origin feature/AmazingFeature`)
5. Open a Pull Request

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Acknowledgments

- Inspired by modern CLI frameworks
- Built with Free Pascal and Lazarus IDE
- Thanks to the Free Pascal community for their support and contributions
