# Release Notes - cli-fp v1.3.3

**Release Date:** 2026-08-13

## Overview

Version `1.3.3` is a stabilization release. It improves cleanup safety,
behavioural test coverage, and parser correctness without adding a public API
or changing existing command contracts.

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
