# Pull Request: Release v1.2.0 - CLI Project Generator

## Summary

This release adds `cli-fp-gen`, a standalone scaffold generator that creates
compilable `cli-fp` applications from a versioned JSON project specification.
It also adds cross-platform CI for both the framework and generator, strengthens
generated-file safety, and fixes boolean default-value lookup behavior.

## Type of change

- [x] New feature (backward-compatible)
- [x] Bug fixes
- [x] Documentation
- [x] Test and CI improvements
- [ ] Breaking change

## Generator functionality

- [x] Initialize a new project with `init`
- [x] Regenerate project infrastructure from `clifp.json`
- [x] Add root and nested commands
- [x] Remove commands, with explicit cascade removal for subtrees
- [x] Generate all supported parameter registrations
- [x] Preview changes with `--dry-run`
- [x] Preserve user-owned command stubs unless `--force` is supplied
- [x] Clean stale generator-owned files through a manifest

## Review fixes

- Existing `clifp.json` files are protected during `init`
- Invalid command separators are rejected instead of silently rewritten
- Conflicting generated Pascal identifiers are detected before writing files
- Reserved Pascal application names produce valid program identifiers
- Program and manifest paths are guarded against project-directory escape
- Manifest path comparisons follow platform casing rules
- Manifest cleanup refuses paths that traverse Unix symbolic links or Windows
  reparse points, including directory junctions
- Malformed project specifications release partially constructed commands and
  parameters safely
- Boolean defaults satisfy the documented `GetParameterValue` contract
- Test compilers write to temporary directories instead of dirtying the source
  tree

## Testing

### Framework

- [x] 30 FPCUnit framework tests pass
- [x] Lazarus package compiles as version `1.2.0`
- [x] All six shipped example applications compile

### Generator

- [x] Focused naming, validation, and malformed-spec ownership tests pass
- [x] Golden output matches expected generated source
- [x] Generated application compiles and runs
- [x] `init`, `generate`, `add command`, and `remove command` operations pass
- [x] Dry-run behavior is non-mutating
- [x] User-owned command stubs are preserved
- [x] Program and manifest path guards are covered, including Unix symlink and
  Windows junction escape attempts

### CI

- [x] Linux job configured
- [x] Windows job configured
- [x] Read-only repository permissions
- [x] Manual workflow dispatch
- [x] Job timeouts

## Release metadata

- [x] README version badge updated to `1.2.0`
- [x] Lazarus package version updated to `1.2.0`
- [x] `CHANGELOG.md` promoted from Unreleased to `1.2.0`
- [x] Release notes added
- [x] Generator and testing documentation updated

## Compatibility

This is a backward-compatible minor release. Existing framework APIs remain
supported, and the optional generator does not change how existing applications
are built.

## Documentation

- [Release notes](RELEASE_NOTES_v1.2.0.md)
- [Changelog](../CHANGELOG.md)
- [Generator guide](codegen.md)
- [README](../README.md)
- [Contributing](../CONTRIBUTING.md)
