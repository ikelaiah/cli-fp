# PowerShell Completion User Guide

**Document Version:** 1.1
**Last Updated:** 2026-07-27
**Framework:** cli-fp
**Applies To:** Windows PowerShell 5.1 on Windows; PowerShell 7+ cross-platform

## Overview

This guide explains how to use PowerShell completion effectively with CLI applications built using the cli-fp framework.

## Installation

1. Generate the completion script:
   ```powershell
   .\SubCommandDemo.exe --completion-file-pwsh > subcommanddemo_completion.ps1
   ```

2. Source the script in your current session:
   ```powershell
   . .\subcommanddemo_completion.ps1
   ```

3. (Optional) Make it permanent by adding to your PowerShell profile:
   ```powershell
   # Find your profile location
   $PROFILE

   # Add the source command to your profile
   Add-Content $PROFILE ". C:\path\to\subcommanddemo_completion.ps1"
   ```

---

## How PowerShell Completion Works

### Basic Usage

Press `TAB` once to cycle through available options:
```powershell
PS> .\SubCommandDemo.exe repo [TAB]
PS> .\SubCommandDemo.exe repo init    # First option

PS> .\SubCommandDemo.exe repo [TAB]   # Press again
PS> .\SubCommandDemo.exe repo clone   # Second option

PS> .\SubCommandDemo.exe repo [TAB]   # Press again
PS> .\SubCommandDemo.exe repo -h      # Third option
```

Press `SHIFT+TAB` to cycle backwards through options.

### Understanding Completion Behavior

#### 0. Commands First - The Design Principle

**Important:** The cli-fp completion engine follows a "commands first" design principle:

When you press `[TAB]` at the root level **without any prefix**, only commands are shown, not flags:

```powershell
PS> .\SubCommandDemo.exe [TAB]
PS> .\SubCommandDemo.exe repo    # Only the command, cycles to it first
```

**To see flags, type a dash prefix:**

```powershell
PS> .\SubCommandDemo.exe -[TAB]
PS> .\SubCommandDemo.exe --help    # Cycles through all flags

PS> .\SubCommandDemo.exe --[TAB]
PS> .\SubCommandDemo.exe --help    # Cycles through long flags only
```

**Why this design?**
- Reduces cognitive overload by showing the most commonly needed items first (commands)
- Follows the natural workflow: choose a command, then choose flags
- Keeps the initial suggestion list short and focused
- Flags are still easily accessible by typing `-` or `--`

This behavior is **intentional** and consistent across both Bash and PowerShell completion.

If the application defines a root command, its parameters are also offered when
the current token starts with `-`. An empty root-level token still suggests named
commands first.

#### 1. TAB Cycling vs. Showing All Options

**PowerShell behavior differs from Bash:**

PowerShell cycles through options one at a time:
```powershell
PS> .\SubCommandDemo.exe repo [TAB]      # Shows: init
PS> .\SubCommandDemo.exe repo [TAB]      # Shows: clone
PS> .\SubCommandDemo.exe repo [TAB]      # Shows: -h
PS> .\SubCommandDemo.exe repo [TAB]      # Shows: --help
# ... continues cycling
```

Bash shows all options at once with double-TAB:
```bash
$ ./SubCommandDemo.exe repo [TAB][TAB]
--help  -h  clone  init  remote    # All shown at once
```

**Tip:** Press `TAB` multiple times to see all available completions in PowerShell.

#### 2. Completing Flags After Values

After entering a parameter value, you need to type `-` or `--` to see remaining options:

**Won't work:**
```powershell
PS> .\SubCommandDemo.exe repo clone --url https://example.com [TAB]
# No completions shown - PowerShell needs a prefix to match against
```

**Will work:**
```powershell
PS> .\SubCommandDemo.exe repo clone --url https://example.com --[TAB]
PS> .\SubCommandDemo.exe repo clone --url https://example.com --path
# Cycles through remaining flags
```

This is standard PowerShell completion behavior - the shell needs something to complete.

#### 3. Application Flags in Command Contexts

The completion engine offers `--help`, `--version`, `-h`, and `-v` while
completing flags at a named command:

```powershell
PS> .\SubCommandDemo.exe repo init --[TAB]
# Cycles through: --path, --bare, --help, --version
```

`--help` and `-h` work at any command level. Version output is currently an
application-level operation, so `--version` and `-v` must be the executable's
only argument even though completion also offers them in command contexts.

#### 4. Command Groups Have No Flags

Some commands are "command groups" that contain subcommands but have no flags of their own:

```powershell
PS> .\SubCommandDemo.exe repo --[TAB]
# Shows only: --help, --version (global flags)
# No repo-specific flags because repo is a command group
```

To see subcommands:
```powershell
PS> .\SubCommandDemo.exe repo [TAB]
# Cycles through: init, clone, remote, -h, --help
```

#### 5. Case Insensitivity

PowerShell completion is case-insensitive by default:

```powershell
PS> .\SubCommandDemo.exe repo INIT --BARE [TAB]
# Works! Completes with: true, false
```

---

## Common Patterns

### Complete a Command
```powershell
PS> .\SubCommandDemo.exe r[TAB]
PS> .\SubCommandDemo.exe repo
```

### Complete a Subcommand
```powershell
PS> .\SubCommandDemo.exe repo i[TAB]
PS> .\SubCommandDemo.exe repo init
```

### See All Flags
```powershell
PS> .\SubCommandDemo.exe repo init --[TAB]
# Keeps pressing TAB to cycle through all flags
```

### Complete Boolean Values
```powershell
PS> .\SubCommandDemo.exe repo init --bare [TAB]
PS> .\SubCommandDemo.exe repo init --bare false

PS> .\SubCommandDemo.exe repo init --bare t[TAB]
PS> .\SubCommandDemo.exe repo init --bare true
```

### Multi-Level Commands
```powershell
PS> .\SubCommandDemo.exe repo remote [TAB]
PS> .\SubCommandDemo.exe repo remote add

PS> .\SubCommandDemo.exe repo remote add --[TAB]
PS> .\SubCommandDemo.exe repo remote add --name
```

---

## Tips and Tricks

### 1. Use SHIFT+TAB to Go Backwards
If you miss the option you want, press `SHIFT+TAB` to cycle backwards:
```powershell
PS> .\SubCommandDemo.exe repo [TAB]      # init
PS> .\SubCommandDemo.exe repo [TAB]      # clone
PS> .\SubCommandDemo.exe repo [SHIFT+TAB]  # Goes back to init
```

### 2. Start with a Letter to Filter
Type the first letter to narrow down options:
```powershell
PS> .\SubCommandDemo.exe repo i[TAB]     # Completes to: init
PS> .\SubCommandDemo.exe repo c[TAB]     # Completes to: clone
```

### 3. Use `--` for Long Flags Only
Typing `--` shows only long-form flags:
```powershell
PS> .\SubCommandDemo.exe repo init --[TAB]
# Shows: --path, --bare, --help, --version (no short flags)
```

### 4. Use `-` for All Flags
Typing `-` shows both short and long flags:
```powershell
PS> .\SubCommandDemo.exe repo init -[TAB]
# Shows: --path, -p, --bare, -b, --help, -h, --version, -v
```

---

## Troubleshooting

### No Completions Appearing

**Problem:** Pressing TAB shows no completions

**Solutions:**
1. Make sure the completion script is sourced:
   ```powershell
   . .\subcommanddemo_completion.ps1
   ```

2. Verify the script loaded successfully:
   ```powershell
   Get-Command Register-ArgumentCompleter
   ```

3. Check the PowerShell version:
   ```powershell
   $PSVersionTable.PSVersion
   ```

### Completions Show File Names

**Problem:** TAB shows file names instead of command completions

**Solution:** The completion script is not loaded. Source it:
```powershell
. .\subcommanddemo_completion.ps1
```

### Want to See All Options at Once

**Problem:** Cycling through options one-by-one is slow

**Solution:** PowerShell behavior is to cycle. To see all options, use `--help`:
```powershell
.\SubCommandDemo.exe repo init --help
```

Or use `PSReadLine` menu completion (PowerShell 7+):
```powershell
Set-PSReadLineKeyHandler -Key Tab -Function MenuComplete
```

---

## Comparison: PowerShell vs Bash

| Feature | PowerShell | Bash |
|---------|-----------|------|
| **TAB Behavior** | Cycles through options | Shows all options (double-TAB) |
| **Root Completion** | Commands only (without prefix) | Commands only (without prefix) |
| **Case Sensitivity** | Case-insensitive | Case-insensitive |
| **Reverse Cycling** | SHIFT+TAB | N/A (shows all) |
| **Application Flags** | Help and version flags are offered in command contexts | Same |
| **Boolean Values** | `true`/`false` | `true`/`false` |

---

## Advanced Usage

### Menu Completion (PowerShell 7+)

Enable menu-style completion for easier navigation:

```powershell
# Add to your $PROFILE
Set-PSReadLineKeyHandler -Key Tab -Function MenuComplete
```

Now TAB will show a menu:
```
PS> .\SubCommandDemo.exe repo [TAB]
init
clone
remote
--help
-h
```

Use arrow keys to select, ENTER to complete.

### List Completion (Alternative)

Show all completions in a list:
```powershell
# Add to your $PROFILE
Set-PSReadLineKeyHandler -Key Ctrl+Space -Function Complete
```

Press `CTRL+SPACE` to see all options.

---

## Summary

Key points to remember:

1. **Commands first**: Type `-` or `--` to see flags at root level
2. **TAB cycles**: Press TAB multiple times to see all options
3. **SHIFT+TAB**: Goes backwards through options
4. **Prefix required**: Type `-` or `--` after values to see flags
5. **Case-insensitive**: Uppercase and lowercase both work
6. **Application flags**: completion offers help and version flags in command contexts; version execution is standalone

---

## See Also

- [PowerShell Completion Test Results](PS_COMPLETION_TESTS.md) - Full test suite
- [PowerShell Completion Analysis](PS_COMPLETION_SUMMARY.md) - Technical analysis
- [Bash Completion Guide](BASH_COMPLETION_GUIDE.md) - Bash version

---

**Need Help?** Run any command with `--help` to see detailed usage information.
