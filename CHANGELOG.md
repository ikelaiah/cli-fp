# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [1.1.6] - 2026-02-21

### Added

- Progress indicator captions:
  - New `Update(const Progress: Integer; const ACaption: string = '')` API for inline status text (single supported caption style)

### Improved

- Progress line rendering now tracks previous output length and clears trailing characters correctly when status text changes length.
- Spinner and progress bar now render optional caption text in a single managed line, so callers do not need to manually blank stale text.

### Updated

- `examples/ProgressDemo/ProgressDemo.lpr` now demonstrates inline caption updates via `Update(..., ACaption)`.
- README updated for version `1.1.6` and caption feature usage.

## [1.1.5] - 2025-12-29

### Added

- **Shell Completion Enhancements**
  - Hidden `__complete` entrypoint for dynamic shell completion support (Cobra-style)
  - Automatic value completion for boolean parameters (`true`/`false`)
  - Automatic value completion for enum parameters (from allowed values list)
  - `TestComplete()` method for programmatic completion testing
  - Completion directive system (`CD_ERROR`, `CD_NOSPACE`, `CD_NOFILE`, `CD_KEEPORDER`) for shell integration

- **Comprehensive Documentation**
  - Complete Bash completion documentation in `docs/completion-testing/`:
    - User guide with "commands first" design principle explained (BASH_COMPLETION_GUIDE.md)
    - 30 manual test cases with full verification results (BASH_COMPLETION_TESTS.md)
    - Test analysis and summary documents
    - File organization documentation
  - Complete PowerShell completion documentation:
    - User guide with PowerShell-specific TAB cycling behavior (PS_COMPLETION_GUIDE.md)
    - 30 manual test cases with results (PS_COMPLETION_TESTS.md)
    - Technical analysis proving 100% pass rate (PS_COMPLETION_SUMMARY.md)
  - Professional metadata headers (dates, versions, status) added to all test documentation

### Improved

- Bash completion script now properly handles empty token detection for value completion
- Bash completion script fixed to avoid duplicate empty arguments when completing new words
- Completion scripts now use process substitution for better performance and reliability

### Fixed

- **Root-level flag completion bug**: Flags starting with `-` at root level (e.g., `./app --h[TAB]`, `./app -[TAB]`) now complete correctly
  - Modified `src/cli.application.pas` lines 1095-1110 in `DoComplete()` function
  - Added check for `-` prefix before attempting command matching
  - All global flags (`--help`, `--version`, `--completion-file`, etc.) now complete at root level
- Bash completion script generation: fixed quote escaping in `sed` command (was `\"$d\"`, now `'$d'`)
- Bash completion argument building: changed loop from `i<=cword` to `i<cword` to prevent duplicate tokens
- Bash completion now correctly distinguishes between completing a new word vs. completing a partial word
- PowerShell completion: Fixed empty string argument passing limitation (PowerShell doesn't pass empty strings as separate args)
- Completion logic: Added special handling for when last token is a complete flag (for PowerShell compatibility)
- Completion logic: Empty tokens are now properly excluded from positional argument counting
- Completion logic: Flags are now suggested alongside subcommands when completing a new word after a command

### Changed

- Removed 4 redundant/test examples for improved project clarity:
  - Removed `examples/BooleanTest/` (test file, not a demo)
  - Removed `examples/TestFlags/` (test file, not a demo)
  - Removed `examples/MyApp/` (redundant with SimpleDemo)
  - Removed `examples/MyGit/` (redundant with SubCommandDemo)
- Reorganized completion-related files for better maintainability:
  - Moved all documentation to `docs/completion-testing/` (8 files)
  - Moved all test scripts to `tests/completion-tests/` (15 files)
  - `example-bin/` now contains only executables and completion scripts
  - Added README.md files to each directory for navigation

### Technical

- Added type definitions for completion callbacks: `TFlagValueCompletionFunc`, `TPositionalCompletionFunc`
- Added completion registry structures: `TFlagCompletionEntry`, `TPosCompletionEntry`, `TFlagCompletionList`, `TPosCompletionList`
- Completion callback registration methods exist but are currently stubbed due to FPC function pointer storage limitations
- `DoComplete()` function implements full Cobra-style completion logic for commands, subcommands, flags, and values

### Known Limitations

- Custom completion callbacks (`RegisterFlagValueCompletion()`, `RegisterPositionalCompletion()`) are not yet functional due to FPC 3.2.2 limitations with function pointer storage in dynamic arrays
- These methods are stubbed with TODO comments pending FPC improvements

### Testing

- **Bash Completion: 30/30 tests pass (100%)** ✅
  - Tested on Bash 4.4.23 via Git Bash (Windows)
  - All root-level, command, subcommand, and flag completions verified
  - Multi-level command structures working correctly
  - Full test documentation: `docs/completion-testing/BASH_COMPLETION_TESTS.md`

- **PowerShell Completion: 30/30 tests pass (100%)** ✅
  - Tested on PowerShell 7.5.4 (Windows)
  - All completion features working as designed
  - Initial 26/30 "failures" analyzed and confirmed as expected behavior
  - Full test documentation: `docs/completion-testing/PS_COMPLETION_TESTS.md`
  - Technical analysis: `docs/completion-testing/PS_COMPLETION_SUMMARY.md`

- **All built-in completion features thoroughly verified:**
  - Commands and subcommands
  - Flags (short and long forms)
  - Boolean values (`true`/`false`)
  - Enum values (from allowed values)
  - Context-aware suggestions
  - Multi-level command structures

## [1.1.4] - 2025-06-28

### Added
- Global `--completion-file` option to output an advanced Bash completion script reflecting the full command/subcommand/flag tree.
- Global `--completion-file-pwsh` option to output a robust, context-aware PowerShell completion script for your CLI.
- Safety warning if user attempts to write completion script directly to `.bashrc` (with usage guidance for safe sourcing).
- Documentation and usage examples for both Bash and PowerShell completion features in the README and user manual.

### Improved
- Help system: `-h`/`--help` now show general help if no command is specified, matching modern CLI conventions.
- Bash and PowerShell completion script generators now output scripts that fully reflect the CLI structure, including all commands, subcommands, and flags.

### Changed
- Bash completion script (`--completion-file`) is now stricter and more accurate: only the root level offers all global flags (`--help`, `-h`, `--help-complete`, `--version`, `--completion-file`), while subcommands only offer `-h` and `--help` as global flags. Completions are always context-aware and match the CLI's actual argument parsing.
- PowerShell completion script (`--completion-file-pwsh`) now provides completions for subcommands and flags at every level, never falls back to file completion, and matches the behavior of modern CLI frameworks.

### Testing & Documentation
- Bash completion tested on Ubuntu 24.04; PowerShell completion tested on PowerShell 7.5.2.
- Guidance on how to test and safely use the new completion features, including manual and optional automated test strategies.
- README and user manual updated to document safe usage of completion scripts and to discourage polluting shell config files.

## [1.1.3] - 2025-06-26

### Added
- User-friendly scripts for building and cleaning all example projects at once:
  - `compile-all-examples.sh` and `clean-all-examples.sh` (Linux/macOS)
  - `compile-all-examples.ps1` and `clean-all-examples.ps1` (Windows)
- Emoji/status output in all scripts for a more pleasant user experience.

### Improved
- All example project outputs are now standardized to the `example-bin/` folder for easier access and cleanup.
- Updated `README.md` to document the new scripts and output location for examples.

## [1.1.2] - 2025-06-20

### Improved
- Boolean flag handling now strictly follows standard CLI conventions: flags are always `false` by default and only become `true` if present on the command line.
- Documentation and examples updated to clarify boolean flag default behavior and best practices.
- Internal logic for `GetParameterValue` and parameter parsing unified for robust and predictable flag/parameter handling.

### Fixed
- Edge case where boolean flags with default `'true'` would always be true, even if not present, is now clearly documented as nonstandard.
- Test and example output now matches standard CLI expectations for boolean flags and parameters.

### Updated
- README, API Reference, Technical Docs, and User Manual to clarify boolean flag behavior and best practices.

## [1.1.1] - 2025-06-20

### Fixed
- Changed default value of boolean flags from 'true' to 'false' to follow standard CLI conventions
- Fixed boolean flag handling in `GetParameterValue` to properly detect flag presence

### Tests
- Added test cases to verify correct boolean flag behavior
- Improved test output messages for invalid flag values

### Updated
- Documentation to reflect new boolean flag default behavior
- API reference with examples of boolean flag usage
- TestFlags example to demonstrate proper boolean flag handling

## [1.1.0] - 2024-12-24

### Added
- Parameter validation for URL type (must start with http://, https://, git://, or ssh://)
- Parameter validation for DateTime type (must be in format "YYYY-MM-DD HH:MM")
- Parameter validation for Enum type (must match allowed values)
- Parameter validation for Integer and Float types
- Password parameter type with masked output
- Comprehensive test suite for parameter validation

### Updated
- Expanded parameter validation documentation in README
- Added parameter type reference to user manual
- Improved validation error messages with examples
- Added test coverage for edge cases and error scenarios
- Updated technical documentation with validation details
- Updated API reference with new parameter types

### Fixed
- Parameter validation edge cases and error handling
- Type conversion issues for numeric parameters
- Test failures in parameter validation suite


## [1.0.2] - 2024-12-21

### Updated

- Added api reference
- Added more examples with comments
- Added screenshots to README.md
- Added useful unicode characters for cli interfaces to user manual

## [1.0.1] - 2024-12-21

### Updated

- Updated README.md
- Updated user manual
- Updated technical docs
- Updated beginners guide 
- Updated test output
- More comments
- New style for spinner


## [1.0.0] - 2024-12-20

### Added
- Initial release of the CLI Framework
- Command and subcommand support with hierarchical structure
- Parameter handling with validation and type checking
- Progress indicators (spinner and progress bar)
- Colored console output support
- Comprehensive help system with auto-generated documentation
- Interface-based design for type safety and extensibility
- Error handling with context-aware help display
- Example applications:
  - SimpleDemo: Basic command implementation
  - ProgressDemo: Progress indicators showcase
  - SubCommandDemo: Hierarchical commands example

### Documentation
- Complete user manual with examples
- Technical documentation with architecture details
- Inline code documentation
- README with quick start guide
- System requirements and compatibility information

[1.0.0]: https://github.com/ikelaiah/cli-fp/releases/tag/v1.0.0

