# Release Notes - cli-fp v1.4.2

`v1.4.2` is a backward-compatible correctness and maintenance release.

## Fixed

- Progress bars safely handle zero totals and values beyond their total.
- Completion responses consistently include their trailing directive.
- Generated Bash and PowerShell completion scripts quote executable values,
  and PowerShell output now has one preamble.
- Date/time validation keeps caller-owned `FormatSettings` unchanged.

## Documentation and maintenance

- The DebugMode recipe now runs the application with `Halt(App.Execute)`.
- Obsolete tracked Pascal backup files were removed.

No public API migration is required. See the [changelog](../CHANGELOG.md) for
the complete release history.
