# Runnable examples

The repository examples are ordinary Free Pascal programs. They are the
quickest way to compare the three command shapes before choosing an API:

- a root command for one focused action;
- named commands for a small toolbox;
- nested commands for a Git-like command tree.

## Build an example

Run these commands from a clone of the repository. The tested compiler is FPC
3.2.2; Lazarus is optional.

```bash
fpc -Fu./src -FE./example-bin ./examples/RootCommandDemo/RootCommandDemo.lpr
./example-bin/RootCommandDemo --help
```

On Windows, quote the unit path so PowerShell passes it to FPC as one option:

```powershell
fpc "-Fu.\src" "-FE.\example-bin" .\examples\RootCommandDemo\RootCommandDemo.lpr
.\example-bin\RootCommandDemo.exe --help
```

The repository's [example build scripts](https://github.com/ikelaiah/cli-fp/blob/main/compile-all-examples.ps1)
compile all eight canonical examples. Individual source files remain in the
repository so you can copy a small working program and change it directly.

## Choose by goal

| Example | Shows |
| --- | --- |
| [QuickStartDemo](https://github.com/ikelaiah/cli-fp/tree/main/examples/QuickStartDemo) | The homepage's smallest compiling root CLI |
| [RootCommandDemo](https://github.com/ikelaiah/cli-fp/tree/main/examples/RootCommandDemo) | A focused default action and root options |
| [SimpleDemo](https://github.com/ikelaiah/cli-fp/tree/main/examples/SimpleDemo) | Named commands, options, output and a spinner |
| [SubCommandDemo](https://github.com/ikelaiah/cli-fp/tree/main/examples/SubCommandDemo) | Nested command trees and completion |
| [ColorDemo](https://github.com/ikelaiah/cli-fp/tree/main/examples/ColorDemo) | Colours and terminal presentation |
| [ProgressDemo](https://github.com/ikelaiah/cli-fp/tree/main/examples/ProgressDemo) | Spinners and progress bars |
| [LongRunningOpDemo](https://github.com/ikelaiah/cli-fp/tree/main/examples/LongRunningOpDemo) | Longer operations and cleanup |
| [ErrorHandlingDemo](https://github.com/ikelaiah/cli-fp/tree/main/examples/ErrorHandlingDemo) | Errors and exit behaviour |

The cleanup smoke checks compile all eight examples on Windows and Linux,
including QuickStartDemo. For the API decision behind root, named, and nested
commands, continue with [commands](commands.md).
