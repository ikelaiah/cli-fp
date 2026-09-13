# Completion Test Scripts

This directory contains low-level test scripts used during development of the Bash and PowerShell completion functionality.

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

## For Formal Testing

Historical manual qualification material is kept in the
[documentation archive](../../docs/archive/completion-testing/README.md).

## Usage

These are development scripts and typically don't need to be run unless:
- Debugging completion issues
- Testing low-level shell behavior
- Verifying argument passing

Most users should use the current [completion guide](../../docs/completion.md)
and the repository's automated tests instead.

## Note

These scripts were kept for historical reference and debugging purposes. They may not be actively maintained as the formal test suite (in docs/) is the primary testing method.
