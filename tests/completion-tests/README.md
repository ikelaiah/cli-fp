# Completion tests

This directory contains the required shell-specific completion contracts and
low-level scripts retained for manual development investigation.

## Required CI contracts

- `run_ci_bash.sh` compiles `completion_fixture.lpr`, generates a Bash script,
  checks its syntax with `bash -n`, and verifies its registration metadata on
  Linux.
- `run_ci_pwsh.ps1` compiles the same fixture, generates a PowerShell script,
  and parses it with the PowerShell language parser on Windows. It parses only;
  it does not register a completer in the CI host session.

The renderer's full deterministic line-by-line contract is covered by the
FPCUnit `Test_Completion_Scripts` suite. The shell checks deliberately exercise
the public executable generation path as a separate, required CI step.

## Manual development scripts

## Files

### Bash Test Scripts
- `test_args.sh` - Test argument parsing
- `test_completion.sh` - Basic completion testing
- `test_completion_debug.sh` - Debug version with verbose output
- `test_compline.sh` - Test COMP_LINE handling
- `test_real_bash.sh` - Real bash completion test
- `test_word.sh` - Word splitting test

### PowerShell Test Scripts
- `test_pwsh_completion.ps1` - PowerShell completion testing
- `test-args.ps1` - PowerShell argument testing
- `test-paramcount.lpr` - Pascal test program for param counting
- `test-paramcount.exe` - Compiled param count tester
- `test-pwsh-args.ps1` - PowerShell argument parsing
- `test-pwsh-completion.ps1` - PowerShell completion testing
- `test-pwsh-params.ps1` - PowerShell parameter testing
- `test-simple.ps1` - Simple PowerShell test

## Purpose

These scripts were used during development to:
1. Test low-level completion behavior
2. Debug argument passing from shells to the application
3. Verify `__complete` command output
4. Test different shell environments

## Historical qualification material

Historical manual qualification material is kept in the
[documentation archive](../../docs/archive/completion-testing/README.md).

## Usage

The manual development scripts typically do not need to be run unless:
- Debugging completion issues
- Testing low-level shell behavior
- Verifying argument passing

Most users should use the current [completion guide](../../docs/completion.md)
and the repository's automated tests instead.

## Note

These scripts are retained for debugging and historical reference. The required
FPCUnit and CI contracts above are the maintained formal test path.
