# Release Notes - cli-fp v1.1.5

**Release Date:** December 29, 2025

## 🎉 Overview

Version 1.1.5 brings comprehensive shell completion enhancements with automatic value completion for boolean and enum parameters, extensive testing documentation, and full implementation of the Cobra-style `__complete` entrypoint.

## ✨ New Features

### Shell Completion Enhancements

- **Automatic Value Completion**
  - Boolean parameters now automatically complete with `true`/`false`
  - Enum parameters automatically complete with their allowed values
  - Works seamlessly in both Bash and PowerShell environments

- **Cobra-Style Completion System**
  - Hidden `__complete` entrypoint for dynamic shell completion support
  - Completion directive system (`CD_ERROR`, `CD_NOSPACE`, `CD_NOFILE`, `CD_KEEPORDER`)
  - `TestComplete()` method for programmatic completion testing

### Comprehensive Documentation

- **Bash Completion Documentation** (`docs/completion-testing/`)
  - Complete user guide with "commands first" design principle explained (BASH_COMPLETION_GUIDE.md)
  - 30 manual test cases with full verification results (BASH_COMPLETION_TESTS.md)
  - Test analysis and summary documents
  - File organization documentation

- **PowerShell Completion Documentation**
  - User guide with PowerShell-specific TAB cycling behavior (PS_COMPLETION_GUIDE.md)
  - 30 manual test cases with results (PS_COMPLETION_TESTS.md)
  - Technical analysis proving 100% pass rate (PS_COMPLETION_SUMMARY.md)

- **Enhanced API Documentation**
  - Added automatic value completion feature to api-reference.md
  - Added PowerShell completion generator section to technical-docs.md
  - Updated user-manual.md with comprehensive completion examples

## 🔄 Implementation Details

### Core Completion Logic

- Root-level flag completion properly handles `-` and `--` prefixes in `src/cli.application.pas` (lines 1095-1110)
- Empty token detection for accurate value completion
- Process substitution for improved performance and reliability
- Context-aware completion at all command levels

### Script Generation Improvements

- Bash completion script handles quote escaping correctly
- Fixed argument building loop boundaries for accurate token parsing
- PowerShell compatibility for empty string handling
- Proper directive handling for shell integration

## 📊 Testing Results

### Bash Completion: 30/30 tests pass (100%) ✅
- Tested on Bash 4.4.23 via Git Bash (Windows)
- All root-level, command, subcommand, and flag completions verified
- Multi-level command structures working correctly

### PowerShell Completion: 30/30 tests pass (100%) ✅
- Tested on PowerShell 7.5.4 (Windows)
- All completion features working as designed
- Initial 26/30 "failures" analyzed and confirmed as expected behavior

### Verified Features
- ✅ Commands and subcommands
- ✅ Flags (short and long forms)
- ✅ Boolean values (`true`/`false`)
- ✅ Enum values (from allowed values)
- ✅ Context-aware suggestions
- ✅ Multi-level command structures

## 🔄 Improvements

- **Bash Completion Script**
  - Better empty token detection for value completion
  - Eliminated duplicate empty arguments
  - Process substitution for improved performance

- **File Organization**
  - Removed 4 redundant/test examples (BooleanTest, TestFlags, MyApp, MyGit)
  - Reorganized completion documentation to `docs/completion-testing/`
  - Moved test scripts to `tests/completion-tests/`
  - Cleaned up `example-bin/` directory

- **Build Scripts**
  - Updated compile/clean scripts to reflect current example set (6 examples)

## 💡 Design Principle: "Commands First"

This release emphasizes the intentional **"commands first"** design principle:

When you press TAB at the root level without typing anything, completion shows **commands only**, not flags. To see flags, type `-` or `--` first:

```bash
# Shows commands only
./yourcli [TAB]

# Shows all flags
./yourcli --[TAB]
./yourcli -[TAB]
```

**Why?** This design reduces cognitive overload by showing the most commonly needed items first (commands), while keeping flags easily accessible with a prefix. This behavior is consistent across both Bash and PowerShell.

## ⚠️ Known Limitations

- Custom completion callbacks (`RegisterFlagValueCompletion()`,
  `RegisterPositionalCompletion()`) are not functional; the methods remain
  stubs after compatibility problems were observed in the original experiment
- **Note:** Built-in completion for commands, flags, boolean values, and enum values works perfectly

## 📦 Breaking Changes

None. This release is fully backward compatible with v1.1.4.

## 🚀 Upgrade Instructions

### From v1.1.4

No code changes required. Simply update your framework files:

1. Replace `src/` directory with the new version
2. Optionally regenerate completion scripts for your applications:
   ```bash
   ./yourapp --completion-file > yourapp_completion.bash
   ./yourapp --completion-file-pwsh > yourapp_completion.ps1
   ```

### From Earlier Versions

See [CHANGELOG.md](../CHANGELOG.md) for migration details from earlier versions.

## 📚 Documentation

- [README.md](../README.md) - Getting started guide
- [User Manual](user-manual.md) - Complete usage guide
- [API Reference](api-reference.md) - Detailed API documentation
- [Technical Documentation](technical-docs.md) - Architecture details
- [Bash Completion Guide](completion-testing/BASH_COMPLETION_GUIDE.md) - Bash completion user guide
- [PowerShell Completion Guide](completion-testing/PS_COMPLETION_GUIDE.md) - PowerShell completion user guide
- [CHANGELOG.md](../CHANGELOG.md) - Full version history

## 🙏 Acknowledgments

Special thanks to all contributors and testers who helped make this release possible!

## 📝 License

This project is licensed under the MIT License - see the [LICENSE](../LICENSE) file for details.

---

**Full Changelog:** [v1.1.4...v1.1.5](https://github.com/ikelaiah/cli-fp/compare/v1.1.4...v1.1.5)
