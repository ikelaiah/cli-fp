# Bash Completion User Guide

[Documentation home](../README.md) · [Completion index](README.md) ·
[PowerShell guide](PS_COMPLETION_GUIDE.md)

**Document Version:** 1.1
**Last Updated:** 2026-07-28
**Framework:** cli-fp
**Applies To:** Bash 4.0+ (Linux, macOS with a newer Bash, Git Bash on Windows)

## Overview

This guide explains how to use Bash completion effectively with CLI applications built using the cli-fp framework.

## Installation

1. Generate the completion script:
   ```bash
   ./SubCommandDemo.exe --completion-file > subcommanddemo_completion.bash
   ```

2. Source the script in your current shell:
   ```bash
   source subcommanddemo_completion.bash
   ```

3. (Optional) Make it permanent by adding to your `~/.bashrc`:
   ```bash
   echo "source ~/path/to/subcommanddemo_completion.bash" >> ~/.bashrc
   ```

---

## How Bash Completion Works

### Basic Usage

Press `TAB` once to auto-complete when there's only one match:
```bash
$ ./SubCommandDemo.exe re[TAB]
$ ./SubCommandDemo.exe repo     # auto-completed
```

Press `TAB` twice to show all available options:
```bash
$ ./SubCommandDemo.exe repo [TAB][TAB]
--help  -h      clone   init    remote
```

### Understanding Completion Behavior

#### 0. Commands First - The Design Principle

**Important:** The cli-fp completion engine follows a "commands first" design principle:

When you press `[TAB]` at the root level **without any prefix**, only commands are shown, not flags:

```bash
$ ./SubCommandDemo.exe [TAB]
$ ./SubCommandDemo.exe repo     # Only the command, auto-completes since there's one match
```

**To see flags, type a dash prefix:**

```bash
$ ./SubCommandDemo.exe -[TAB][TAB]
--completion-file  --completion-file-pwsh  --help  --help-complete  --version  -h  -v

$ ./SubCommandDemo.exe --[TAB][TAB]
--completion-file  --completion-file-pwsh  --help  --help-complete  --version
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

#### 1. Auto-Completion vs. Showing Options

**Auto-completion** happens when there's only one match:
```bash
$ ./SubCommandDemo.exe [TAB]
$ ./SubCommandDemo.exe repo     # Only one command exists, so it auto-completes
```

**Showing options** happens when there are multiple matches:
```bash
$ ./SubCommandDemo.exe repo [TAB][TAB]
--help  -h      clone   init    remote    # Multiple options shown
```

#### 2. Completing Flags After Values

After entering a parameter value, you need to type `-` or `--` to see remaining options:

**Won't work:**
```bash
$ ./SubCommandDemo.exe repo clone --url https://example.com [TAB][TAB]
# Nothing shown - Bash needs a prefix to match against
```

**Will work:**
```bash
$ ./SubCommandDemo.exe repo clone --url https://example.com -[TAB][TAB]
--branch   --depth    --help     --path     --url      --version  -b  -d  -h  -p  -u  -v
```

This is standard Bash completion behavior - the shell needs something to complete.

#### 3. Application Flags in Command Contexts

The completion engine offers `--help`, `--version`, `-h`, and `-v` while
completing flags at a named command:

```bash
$ ./SubCommandDemo.exe repo init -[TAB][TAB]
--bare     --help     --path     --version  -b         -h         -p         -v
```

`--help` and `-h` work at any command level. Version output is currently an
application-level operation, so `--version` and `-v` must be the executable's
only argument even though completion also offers them in command contexts.

#### 4. Short Flags Don't Auto-Complete Further

Short flags are complete by themselves, so they may show value options instead:

```bash
$ ./SubCommandDemo.exe repo init -b[TAB]
# May show: true  false (boolean values)
```

This is because `-b` is a complete flag (short form of `--bare`), and the completion system is offering possible values.

---

## Common Completion Patterns

### Completing Commands

```bash
# Show all root-level commands
$ ./SubCommandDemo.exe [TAB][TAB]

# Complete command by prefix
$ ./SubCommandDemo.exe re[TAB]
→ ./SubCommandDemo.exe repo

# Show subcommands
$ ./SubCommandDemo.exe repo [TAB][TAB]
→ init  clone  remote  --help  -h
```

### Completing Flags

```bash
# Show all long flags (starting with --)
$ ./SubCommandDemo.exe repo init --[TAB][TAB]
→ --bare  --help  --path  --version

# Show all flags (both long and short)
$ ./SubCommandDemo.exe repo init -[TAB][TAB]
→ --bare  --help  --path  --version  -b  -h  -p  -v

# Complete flag by prefix
$ ./SubCommandDemo.exe repo init --p[TAB]
→ ./SubCommandDemo.exe repo init --path
```

### Completing Flag Values

For boolean flags:
```bash
$ ./SubCommandDemo.exe repo init --bare [TAB][TAB]
→ true  false
```

For enum flags:
```bash
$ ./SubCommandDemo.exe process --log-level [TAB][TAB]
→ debug  info  warn  error
```

### Multi-Level Commands

Deep nesting works seamlessly:
```bash
$ ./SubCommandDemo.exe repo remote [TAB][TAB]
→ add  remove  --help  -h

$ ./SubCommandDemo.exe repo remote add -[TAB][TAB]
→ --help  --name  --url  --version  -h  -n  -u  -v
```

---

## Tips and Tricks

### 1. See Remaining Options After Providing Values

Always type `-` to see what flags are still available:
```bash
$ ./SubCommandDemo.exe repo clone --url https://test.com --branch main -[TAB][TAB]
→ Shows all remaining flags
```

### 2. Use Long Form for Clarity

Type `--` to see only long-form flags (more readable):
```bash
$ ./SubCommandDemo.exe repo init --[TAB][TAB]
→ --bare  --help  --path  --version  (easier to read than short forms)
```

### 3. Prefix Matching is Case-Insensitive

```bash
$ ./SubCommandDemo.exe repo IN[TAB]
→ ./SubCommandDemo.exe repo init    # Works despite different case
```

### 4. Check Help Anytime

`--help` is always available at every level:
```bash
$ ./SubCommandDemo.exe --help
$ ./SubCommandDemo.exe repo --help
$ ./SubCommandDemo.exe repo init --help
```

---

## Troubleshooting

### Completion Not Working

1. **Did you source the script?**
   ```bash
   source subcommanddemo_completion.bash
   ```

2. **Check if completion is registered:**
   ```bash
   complete -p SubCommandDemo.exe
   # Should show: complete -F _repomanager_completions SubCommandDemo.exe
   ```

3. **Regenerate the completion script if you updated the application:**
   ```bash
   ./SubCommandDemo.exe --completion-file > subcommanddemo_completion.bash
   source subcommanddemo_completion.bash
   ```

### No Suggestions After Entering a Value

This is normal! Type `-` or `--` to trigger completion:
```bash
$ ./SubCommandDemo.exe repo clone --url value -[TAB]
```

### Getting Duplicate Suggestions

The framework automatically prevents duplicates, but bash may show both short and long forms:
```bash
-p  --path    # Both are valid, choose either one
```

---

## Expected Behavior Reference

| Scenario | What Happens | Why |
|----------|-------------|-----|
| `./app [TAB]` with one command | Auto-completes to that command | Only one match exists |
| `./app [TAB]` with multiple commands | Shows all commands | Multiple matches available |
| `./app cmd --value [TAB]` | Nothing shown | Need prefix `-` or `--` |
| `./app cmd --value -[TAB]` | Shows available flags | Prefix provided for matching |
| Short flag like `-b[TAB]` | May show values (true/false) | Flag is complete, offering values |
| `--vers[TAB]` | Completes to `--version` | Prefix matching |
| Flags at a named command | Completion offers help and version flags | Help works at command level; version is standalone only |

---

## Summary

✅ **DO:**
- Press `TAB` twice to see all options
- Type `-` after a value to see remaining flags
- Use `--help` at any level to understand available options
- Regenerate completion script after updating your app

❌ **DON'T:**
- Expect completions after a value without typing `-` or `--`
- Treat a suggested command-level `--version` as valid; version is standalone
- Forget to source the completion script in new shells

For more help, run `./SubCommandDemo.exe --help` or consult the framework documentation.
