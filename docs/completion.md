# Shell completion

[How do I...?](how-to.md) · [Limitations](limitations.md) ·
[API reference](api-reference.md)

`cli-fp` can generate completion scripts from your registered command tree.
Built-in completion supplies commands, subcommands, options, Boolean values,
and enum values; it does not currently support dynamic application callbacks.

## Bash

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
invocation forms. Generated executable values are quoted so names and paths
containing spaces or shell-sensitive characters remain usable. PowerShell
cycles candidates with Tab; Bash normally lists matches according to its
configured completion behavior.

## Boundaries

Completion-script requests are handled when they are the first argument.
`--help` works at application and selected-command scope; `--version` is an
application-level request when used alone. For detailed shell-specific
troubleshooting and historical verification records, see the archived
[Bash guide](https://github.com/ikelaiah/cli-fp/blob/main/docs/completion-testing/BASH_COMPLETION_GUIDE.md)
and [PowerShell guide](https://github.com/ikelaiah/cli-fp/blob/main/docs/completion-testing/PS_COMPLETION_GUIDE.md).
