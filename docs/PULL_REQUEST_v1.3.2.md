# Pull Request: Release v1.3.2 - Contract Hardening

**Target Release:** v1.3.2

**Release Date:** 2026-07-30

## Summary

This PR delivers the `v1.3.2` roadmap milestone by correcting the mismatch
between the public `ICommand` contract and the runtime implementation, and by
clearly deprecating the non-functional custom-completion callback methods.

It also adds the maintenance roadmap, regression coverage, current
documentation, release notes, and version metadata for the patch release.

## Type of Change

- [x] Bug fix
- [x] Backward-compatible interface addition
- [x] API deprecation
- [x] Regression test
- [x] Documentation
- [ ] Breaking change

## Contract Correction

- [x] Add the optional `ICommandParameterReceiver` capability.
- [x] Make `TBaseCommand` implement the receiver.
- [x] Replace the unconditional `ICommand`-to-`TBaseCommand` downcast with a
  `Supports()` capability check.
- [x] Remove the `CLI.Command` implementation dependency from
  `CLI.Application`.
- [x] Verify that an independent `ICommand` implementation executes and
  returns its exit code.

## Completion Callback Deprecation

- [x] Mark `RegisterFlagValueCompletion()` as deprecated.
- [x] Mark `RegisterPositionalCompletion()` as deprecated.
- [x] Retain both methods as no-ops for 1.x source compatibility.
- [x] State that removal is planned for v2.0.0.
- [x] Confirm that built-in metadata completion is unaffected.

## Documentation and Versioning

- [x] Add `ROADMAP.md` with maintenance outcomes through v2.0.0.
- [x] Update the API reference for `ICommandParameterReceiver`.
- [x] Update the technical completion notes and architecture description.
- [x] Add the dated `1.3.2` changelog entry and comparison links.
- [x] Update the README badge to `1.3.2`.
- [x] Update Lazarus package metadata to `1.3.2`.
- [x] Add v1.3.2 release notes.
- [x] Add this pull request note.

## Compatibility

Existing `TBaseCommand` descendants require no changes. Existing application
factories, command registration calls, generated projects, and schema-version-1
manifests retain their behaviour.

Direct `ICommand` implementations no longer fail merely because they do not
inherit from `TBaseCommand`. The new receiver interface is optional.

The deprecated custom-completion methods remain callable in 1.x but can emit
compiler warnings. Removing calls to them is behaviour-preserving because the
methods were already no-ops.

## Verification

- [x] Framework suite: 39 tests, 0 errors, 0 failures.
- [x] Interface-only command regression test.
- [x] Deprecation compiler-warning smoke test.
- [x] Windows generator unit tests.
- [x] Generator golden-output test.
- [x] Generated-project compile smoke test.
- [x] Generator operations test.
- [x] All seven example applications compiled.
- [x] Lazarus runtime package build with metadata at `1.3.2`.
- [x] Local Markdown targets and fenced code blocks across 39 Markdown files.
- [x] FPC 3.2.2.
- [ ] GitHub Actions after the PR is opened.

## Release Readiness

- [x] Release date finalized as 2026-07-30.
- [x] Version metadata updated to `1.3.2`.
- [x] Changelog section dated and comparison links updated.
- [x] Release notes prepared.
- [x] Pull request notes prepared.
- [ ] Confirm GitHub Actions on Windows and Linux.

After merge, create the `v1.3.2` tag and publish the prepared release notes.
