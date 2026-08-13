# Implementation Plan: v1.3.3 Stabilization

## Overview

Deliver the ROADMAP.md v1.3.3 stabilization work only: safe example cleanup,
behavioural test coverage for help and numeric parsing, CI compilation of the
seven canonical examples, and documentation of the changed behaviour.

## Architecture Decisions

- Keep cleanup targets allowlisted to generated compiler artifacts; never remove
  source-controlled directories or files.
- Capture help output through an internal `TCLIApplication` test seam, without
  adding a method to the public `ICLIApplication` API.
- Extend the existing parser and validation path for separated signed numbers;
  do not create a parallel parsing path.

## Task List

### Phase 1: Safe cleanup

- [ ] Task 1: Restrict both cleanup scripts to generated artifacts and add
  cross-platform smoke checks that prove tracked files survive.

### Phase 2: Runtime behaviour

- [ ] Task 2: Replace placeholder help tests with output assertions using an
  internal capture seam.
- [ ] Task 3: Support separated negative integer and float option values, with
  regression coverage for equals, separated, and unknown-option forms.

### Phase 3: Release integration

- [ ] Task 4: Compile all seven canonical examples in Linux and Windows CI,
  document the behavioural changes, and run the release verification suite.

### Checkpoint: Complete

- [ ] Cleanup smoke checks pass on their supported platforms.
- [ ] Framework and generator tests pass.
- [ ] All seven examples compile on the local platform and in both CI jobs.
- [ ] No public API was added or changed.

## Risks and Mitigations

| Risk | Impact | Mitigation |
| --- | --- | --- |
| Cleanup removes user content | High | Allowlist only generated compiler extensions and dedicated generated directories; assert tracked paths after cleanup. |
| Output capture changes runtime output | High | Keep it internal, disabled by default, and test the normal help execution path. |
| Signed numbers weaken option detection | High | Accept a leading `-` only when the registered parameter is integer or float and the candidate validates as numeric. |

## Scope Guard

No new command API, parameter kinds, generator features, completion features,
or broad application refactor is included.
