# Shell completion

[How do I...?](how-to.md) · [Limitations](limitations.md) ·
[API reference](api-reference.md)

`cli-fp` can generate completion scripts from your registered command tree.
Built-in completion supplies commands, subcommands, options, Boolean values,
and enum values; it does not currently support dynamic application callbacks.
The generated shell function calls the application's internal `__complete`
entrypoint, so candidates always reflect the command tree in the executable
that generated the script.

## Bash

Start in the directory containing your compiled `myapp` executable (replace
this placeholder with the actual name, for example `QuickStartDemo`).

```bash
./myapp --completion-file > myapp-completion.bash
source ./myapp-completion.bash
```

Generate the file after the application is compiled. Source it for the current
shell; add a source command to your shell configuration only after confirming
the script path is stable.

## PowerShell

```powershell
.\myapp.exe --completion-file-pwsh > .\myapp-completion.ps1
. .\myapp-completion.ps1
```

The scripts register completion for the executable name and common relative
invocation forms. Generated executable names, paths, and metadata are quoted
for their target shell, including values containing spaces or shell-sensitive
characters. PowerShell cycles candidates with Tab; Bash normally lists matches
according to its configured completion behavior. Build and try
[SimpleDemo](examples.md#choose-by-goal) for a runnable command/subcommand
completion example.

## Boundaries

Completion-script requests are handled only when they are the first argument.
The first non-option token selects a named command; later non-option tokens
can select its subcommands. `--help` works at application and selected-command
scope. Reserved `-v`/`--version` requests display the application version at
root, named and nested scopes, and are offered in command-option completions.
Use `-d` or long-only `--verbose` for verbosity; `-V` is also a version request.
Historical completion qualification material is retained in the
[archive](https://github.com/ikelaiah/cli-fp/blob/main/docs/archive/completion-testing/README.md),
not in the user guide.
