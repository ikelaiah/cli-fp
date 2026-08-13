# v1.3.3 Task Checklist

## Task 1: Safe example cleanup

**Acceptance criteria:**

- [x] Shell and PowerShell cleanup remove generated artifacts only.
- [x] Smoke checks build examples, run cleanup, and confirm tracked files remain.

**Verification:** cleanup smoke tests on Linux and Windows.

**Dependencies:** None.

## Task 2: Help-output coverage

**Acceptance criteria:**

- [x] Tests assert real usage, descriptions, required options, defaults, and subcommands.
- [x] Capture support is internal to `TCLIApplication`; `ICLIApplication` remains unchanged.

**Verification:** focused framework test suite.

**Dependencies:** None.

## Task 3: Negative numeric parsing

**Acceptance criteria:**

- [x] Integer and float options accept equals and separated negative values.
- [x] Unknown options remain errors.

**Verification:** focused framework test suite.

**Dependencies:** None.

## Task 4: Release integration and documentation

**Acceptance criteria:**

- [x] Both CI jobs are configured to compile the seven canonical examples.
- [x] Release behaviour is documented in user-facing documentation and changelog.

**Verification:** CI-script inspection and local compilation where available.

**Dependencies:** Tasks 1–3.

## Task 5: Hermetic tests and production-safe capture

**Acceptance criteria:**

- [x] Windows and Linux runners force all units to rebuild in isolated output directories.
- [x] Capture-specific fields and methods do not exist in normal runtime builds.

**Verification:** normal build followed by both framework runners; normal package build.

**Dependencies:** Task 2.

## Task 6: Password-safe debug diagnostics

**Acceptance criteria:**

- [x] Characterization covers ordinary debug output.
- [x] Separated and equals-form password values are replaced with `[REDACTED]` everywhere.

**Verification:** focused framework tests.

**Dependencies:** Task 5.

## Task 7: Parameter and help consolidation

**Acceptance criteria:**

- [x] Application validation and command execution share one parameter-value implementation.
- [x] Application and base-command help share one renderer with unchanged observable output.

**Verification:** framework characterization tests and generator compile suite.

**Dependencies:** Tasks 5–6.

## Task 8: Completion extraction

**Acceptance criteria:**

- [x] Existing completion behavior is characterized before extraction.
- [x] Completion calculation lives outside `TCLIApplication`; unreachable private callback branches and unused allocations are deleted.

**Verification:** focused completion tests and framework suite.

**Dependencies:** Task 7.

## Task 9: Dispatch decomposition and release verification

**Acceptance criteria:**

- [x] Application dispatch is composed from focused helpers without changing its public facade.
- [ ] Framework, generator, cleanup, package, and seven-example checks pass.

**Verification:** complete Windows suite and Linux/Windows CI.

**Dependencies:** Tasks 5–8.
