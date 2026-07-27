# Pull Request: Release v1.3.1 - Documentation Onboarding and Accuracy

**Target Release:** v1.3.1

**Release Date:** July 28, 2026

## Summary

This PR makes `cli-fp` substantially easier to approach for developers who
are new to the library or to Free Pascal. It introduces a verified
first-success path, explains the Object Pascal conventions used by the
project, improves navigation across the documentation set, and corrects
platform and API details discovered during the documentation audit.

This is a documentation-focused patch release. It does not change framework
runtime behaviour, public APIs, generator schemas, or generated output.

## Type of Change

- [x] Documentation improvement
- [x] Documentation correction
- [x] Documentation and example verification
- [ ] New framework feature
- [ ] Breaking change

## README Onboarding

- [x] Add goal-based “Start Here” routes for new FPC developers, new `cli-fp`
  users, existing Pascal applications, Lazarus users, and API readers.
- [x] Add Free Pascal installation and split-package guidance.
- [x] Add a five-minute generated CLI that reaches a working native
  executable before introducing framework internals.
- [x] Show how to replace a generated command implementation and run it.
- [x] Add a concise Free Pascal and Object Pascal orientation.
- [x] Explain root, named, and combined CLI application shapes.
- [x] Present implemented capabilities through a focused feature tour.

## Documentation Navigation

- [x] Add `docs/README.md` as a goal-based documentation landing page.
- [x] Map examples to the concepts they demonstrate.
- [x] Add repository-layout orientation for new contributors.
- [x] Distinguish current guides from release notes and dated test records.
- [x] Add navigation between the user manual, generator guide, API reference,
  technical documentation, and completion guides.
- [x] Mark advanced references with their intended audience.

## Manual and Generator Guides

- [x] Replace the user manual’s duplicated opening cheat sheet with
  prerequisites and learning paths.
- [x] Retain one API cheat sheet for quick lookup.
- [x] Clarify root-command, parameter retrieval, help, debug, and console
  terminology.
- [x] Expand the generator guide with verified Bash and PowerShell first-build
  paths.
- [x] Explain the roles of `.lpr`, generated units, user-owned command units,
  `clifp.json`, and FPC unit-search paths.

## Accuracy Corrections

- [x] Distinguish typed parameter metadata and validation from string-based
  value retrieval.
- [x] Clarify the FCL JSON dependency used by `cli-fp-gen`.
- [x] Quote PowerShell `-Fu` arguments where required.
- [x] Separate CI-tested platforms from expected but currently untested Unix
  compatibility.
- [x] Describe Windows console colour handling and ANSI-dependent cursor
  operations accurately.
- [x] Expand public API excerpts and document omitted testing and disabled
  callback surfaces.
- [x] Preserve the distinct application, command-level, complete-help,
  version, and completion-script scopes.

## Compatibility

No application or generator migration is required:

- framework source is unchanged;
- public API signatures are unchanged;
- `clifp.json` schema version 1 is unchanged;
- generated project ownership rules are unchanged; and
- existing applications do not need to be rebuilt for this documentation
  release.

## Verification

- [x] Framework suite: 38 tests, 0 failures.
- [x] Full Windows code-generator suite.
- [x] Documented generator build and generated-application compile.
- [x] Documented `TGreetCommand.Execute` implementation and
  `Hello, Ada!` runtime result.
- [x] General-help and command-help runtime smoke tests.
- [x] `RootCommandDemo` compile and runtime smoke tests.
- [x] Lazarus runtime package build with metadata at `1.3.1`.
- [x] All local targets and heading anchors across 30 Markdown files.
- [x] Balanced fenced code blocks across 30 Markdown files.
- [x] `git diff --check`.
- [ ] GitHub Actions after the PR is opened.

## Release Readiness

- [x] Changelog entry prepared under `Unreleased`.
- [x] v1.3.1 release notes prepared.
- [x] Finalize the release date.
- [x] Update version metadata to `1.3.1` where required.
- [ ] Confirm GitHub Actions on Linux and Windows.
- [x] Move the changelog entry from `Unreleased` to a dated `1.3.1` section.

After merge, create the `v1.3.1` tag and publish the prepared release notes.
