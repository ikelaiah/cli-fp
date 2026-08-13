# Release Notes - cli-fp v1.3.3

**Release Date:** 2026-08-13

## Overview

Version `1.3.3` is a stabilization release. It improves cleanup safety,
behavioural test coverage, parser correctness, diagnostic safety, and internal
maintenance boundaries without adding a public API or changing existing
command contracts.

## Safe example cleanup

`clean-all-examples.sh` and `clean-all-examples.ps1` now remove only generated
compiler output. They preserve the tracked completion scripts and documentation
in `example-bin/`, along with other repository files.

New cross-platform smoke checks compile all seven canonical examples in an
isolated repository copy, run the cleanup script, and verify that generated
artifacts are removed while tracked files remain unchanged. GitHub Actions runs
these checks on Linux and Windows.

## Trustworthy help coverage

The framework tests now capture output from the normal help execution path
through an internal test-only seam. They assert application and command usage,
descriptions, required options, defaults, complete help, and subcommands.

The capture seam is not included in normal builds and does not add a method to
the public `ICLIApplication` API.

The framework test runners also force an isolated unit rebuild. Existing
non-test `.ppu` files can no longer bypass the test define or affect whether
the suite compiles.

## Safer diagnostics

`DebugMode` continues to show parsing details, but values associated with
registered password parameters are now written as `[REDACTED]`. This covers
both `--password value` and `--password=value` forms. Applications remain
responsible for redacting sensitive values in their own output and logging.

## Smaller internal responsibilities

Help formatting is now shared by the application and base-command paths.
Completion calculation and parameter-value handling live in focused internal
units, and application dispatch is divided into named stages. Unreachable
private completion callback branches and unused temporary allocations were
removed.

The `TCLIApplication` facade, `ICLIApplication` contract, deprecated 1.x
completion compatibility methods, and all existing command APIs remain
unchanged.

## Negative numeric options

Registered integer and float options now accept separated signed values:

```text
myapp measure --count -1 --rate -2.5
```

The existing equals forms continue to work:

```text
myapp measure --count=-1 --rate=-2.5
```

Only candidates that parse as an integer or float for the registered option are
accepted this way; unknown options remain validation errors.

## Compatibility

No migration is required. This release adds no public command API, parameter
kinds, generator capabilities, completion features, or breaking changes.

**Full Changelog:** [v1.3.2...v1.3.3](https://github.com/ikelaiah/cli-fp/compare/v1.3.2...v1.3.3)
