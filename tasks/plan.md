# Implementation Plan: v1.3.3 Stabilization

## Overview

Deliver the ROADMAP.md v1.3.3 stabilization work only: safe example cleanup,
behavioural coverage, hermetic builds, safe diagnostics, and behaviour-
preserving internal decomposition before the next public API is added.

## Architecture Decisions

- Keep cleanup targets allowlisted to generated compiler artifacts; never remove
  source-controlled directories or files.
- Capture help output through an internal `TCLIApplication` test seam, without
  adding a method to the public `ICLIApplication` API.
- Extend the existing parser and validation path for separated signed numbers;
  do not create a parallel parsing path.
- Preserve `TCLIApplication` and all public compatibility symbols while moving
  help rendering, completion calculation, and parameter-value semantics into
  focused internal units.
- Keep deprecated completion callback registration as public no-ops for 1.x,
  but remove private branches that can never execute.

## Task List

### Phase 1: Safe cleanup

- [x] Task 1: Restrict both cleanup scripts to generated artifacts and add
  cross-platform smoke checks that prove tracked files survive.

### Phase 2: Runtime behaviour

- [x] Task 2: Replace placeholder help tests with output assertions using an
  internal capture seam.
- [x] Task 3: Support separated negative integer and float option values, with
  regression coverage for equals, separated, and unknown-option forms.

### Phase 3: Release integration

- [x] Task 4: Compile all seven canonical examples in Linux and Windows CI,
  document the behavioural changes, and run the release verification suite.

### Phase 4: Review hardening

- [x] Task 5: Make framework test compilation hermetic and exclude capture
  state and entry points from normal runtime builds.
- [x] Task 6: Characterize debug output and redact registered password values.
- [x] Task 7: Single-source parameter lookup and help rendering behind the
  unchanged public facade.
- [x] Task 8: Characterize and extract completion calculation, deleting
  unreachable private callback paths and unused allocations.
- [ ] Task 9: Decompose application dispatch into focused internal helpers,
  then run the complete cross-platform release verification.

### Checkpoint: Complete

- [x] Windows cleanup smoke check passes; CI runs the platform-native checks.
- [x] Framework and generator tests pass on Windows.
- [x] All seven examples compile locally on Windows; both CI jobs run the check.
- [x] No public API was added or changed.
- [x] Normal builds contain no capture-specific state or entry point.
- [x] Debug output cannot reveal registered password values.
- [x] Internal decomposition preserves all characterized behaviour and public
  compatibility symbols.

## Risks and Mitigations

| Risk | Impact | Mitigation |
| --- | --- | --- |
| Cleanup removes user content | High | Allowlist only generated compiler extensions and dedicated generated directories; assert tracked paths after cleanup. |
| Output capture changes runtime output | High | Keep it internal, disabled by default, and test the normal help execution path. |
| Signed numbers weaken option detection | High | Accept a leading `-` only when the registered parameter is integer or float and the candidate validates as numeric. |
| Internal refactoring changes observable output | High | Add characterization tests first and verify each extraction independently. |
| Stale compiler units bypass test defines | High | Force an isolated rebuild in both platform test runners. |
| Debug diagnostics expose secrets | High | Resolve parameter metadata before logging and redact password values in every debug form. |

## Scope Guard

No new command API, parameter kinds, generator features, completion features,
or public API removal is included. Public compatibility no-ops and test-oriented
members remain until the planned v2.0.0 cleanup.
