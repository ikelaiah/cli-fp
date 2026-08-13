# v1.3.3 Task Checklist

## Task 1: Safe example cleanup

**Acceptance criteria:**

- [ ] Shell and PowerShell cleanup remove generated artifacts only.
- [ ] Smoke checks build examples, run cleanup, and confirm tracked files remain.

**Verification:** cleanup smoke tests on Linux and Windows.

**Dependencies:** None.

## Task 2: Help-output coverage

**Acceptance criteria:**

- [ ] Tests assert real usage, descriptions, required options, defaults, and subcommands.
- [ ] Capture support is internal to `TCLIApplication`; `ICLIApplication` remains unchanged.

**Verification:** focused framework test suite.

**Dependencies:** None.

## Task 3: Negative numeric parsing

**Acceptance criteria:**

- [ ] Integer and float options accept equals and separated negative values.
- [ ] Unknown options remain errors.

**Verification:** focused framework test suite.

**Dependencies:** None.

## Task 4: Release integration and documentation

**Acceptance criteria:**

- [ ] Both CI jobs compile the seven canonical examples.
- [ ] Release behaviour is documented in user-facing documentation and changelog.

**Verification:** CI-script inspection and local compilation where available.

**Dependencies:** Tasks 1–3.
