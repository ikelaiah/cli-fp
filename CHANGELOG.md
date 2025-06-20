# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

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