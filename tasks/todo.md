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
