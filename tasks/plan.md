# Implementation Plan: cli-fp v1.5.0 Defensive CLI Core

## Overview

Harden the existing v1.4.3 command/parameter model, make parser failures
predictable, secure generated output, fix confirmed value and terminal bugs,
and publish a beginner-oriented v1.5.0 release without redesigning the
public facade or adding positional-argument semantics.

## Architecture decisions

- Keep validation in small shared helpers in the runtime and generator rather
  than changing the public interfaces or adding dependencies.
- Treat an empty command name as valid only for the configured root command;
  named application commands and subcommands must use valid single tokens.
- Preserve last-write-wins option parsing and reject unexpected positional
  arguments explicitly.
- Keep shell emitters in the current application unit for v1.5.0; defer the
  larger renderer extraction to v1.6.x.
- Encode Pascal strings as valid literals with `#` character fragments for
  line breaks/control characters rather than silently corrupting generated
  source.

## Task list

### Phase 1: Runtime contracts and regression coverage

- [x] Add focused tests for malformed definitions, parser edge cases, value
  initialization, enum/date-time correctness, and help behavior.
- [x] Add reusable command/flag validation and cycle detection while preserving
  root-command semantics.
- [x] Reject unexpected positional arguments and make help requests with extra
  arguments deterministic.
- [x] Fix case-consistent parameter lookup, literal enum splitting, documented
  date-time format, and empty flag filtering.

### Checkpoint: Runtime

- [x] Framework tests pass and normal root/named/nested examples still compile.

### Phase 2: Output, generator, and terminal hardening

- [x] Quote/escape Bash and PowerShell completion metadata and sanitize unsafe
  terminal controls in rendered text.
- [x] Harden generator validation, argument parsing, corrupt-manifest/spec
  failures, and Pascal literal rendering.
- [x] Make color reset exception-safe, bound progress width, remove spinner
  update sleeping, and improve line clearing.
- [x] Extend generator/completion/terminal regression tests.

### Checkpoint: Qualification

- [x] Framework, generator, golden, compile-smoke, cleanup, completion, and
  documentation checks pass where supported by the environment.

### Phase 3: Documentation and release

- [x] Update beginner docs, limitations, API/reference pages, navigation,
  changelog, roadmap, and DocKit version metadata for v1.5.0.
- [x] Run review and final diff checks; commit coherent changes.
- [ ] Push, create/check/merge PR, tag, publish release, and verify Pages if
  GitHub authentication and required checks permit remote operations.

## Deferred by design

- Positional-argument APIs and `--` terminator semantics remain later design
  work because the v1.x parser has no positional destination.
- Per-command versioning remains unsupported; version stays application-level.
- The major `TCLIApplication` structural split remains v1.6.x work.
- Deprecated/no-op public completion callbacks remain source-compatible until
  v2.0.0.

## Risks and mitigations

| Risk | Mitigation |
| --- | --- |
| Existing valid command declarations are rejected | Validate only malformed token shapes and preserve root `''`. |
| Shell quoting fix creates invalid scripts | Add generated-script inspection and shell syntax tests. |
| FPC 3.2.2 differences | Use repository runners and CI-equivalent commands on Windows. |
| Release permissions unavailable | Finish local qualification and report exact remote blocker/actions. |

---

# Implementation Plan: cli-fp v1.5.1 Correctness and Completion Patch

## Overview

Deliver a surgical patch release from the v1.5.0 `main` baseline. Confirm and
fix only the reported `GetParameterValue`, completion-suggestion, and
application-version semantics; clarify current documentation; update current
version metadata and immutable DocKit version history; then qualify and
release v1.5.1 without changing the public facade.

## Architecture decisions

- Reuse `SameText` and the existing parameter-value lookup conventions rather
  than introducing a second lookup abstraction.
- Keep application `--version`/`-v` handling at the application boundary and
  remove those flags only from selected named-command validation/completion.
- Keep the existing completion engine and command model intact; add only
  independent empty-flag guards.
- Preserve v1.5.0 and older documentation sources as immutable historical
  releases; make only current documentation and version metadata current for
  v1.5.1.

## Task list

### Phase 1: Confirm and reproduce

- [x] Inspect the v1.5.0 `main` implementation, tests, docs, version metadata,
  DocKit configuration, and CI workflows.
- [x] Add focused failing regressions for case-insensitive descendant lookup,
  empty completion suggestions, and named-command version rejection.

### Phase 2: Surgical runtime fixes

- [x] Make `TBaseCommand.GetParameterValue` use case-insensitive flag matching
  while preserving miss initialization.
- [x] Guard both flag forms independently in every relevant completion path.
- [x] Keep version global at application level; reject and avoid suggesting it
  after named-command selection.

### Checkpoint: Runtime patch

- [x] Focused regressions fail before the fixes and pass afterward.
- [x] Existing v1.5.0 behavior remains covered and passing.

### Phase 3: Documentation and release metadata

- [x] Correct stale current 1.x wording and label `GetParameterValue` as a
  protected descendant member in the API reference.
- [x] Update authoritative current version locations to 1.5.1, add concise
  changelog/release notes, update roadmap context, and add DocKit v1.5.1.
- [x] Preserve historical release notes and tags unchanged.

### Checkpoint: Qualification

- [x] Framework, generator, completion, example, cleanup, golden,
  compile-smoke, docs, DocKit, and diff checks pass where supported.
- [x] Code review finds no required correctness, security, architecture, or
  compatibility issues.

### Phase 4: Remote release

- [ ] Push `release/v1.5.1` and open the PR.
- [ ] Observe green Linux and Windows CI, then stop for explicit merge
  authorization if repository safety requires it.
- [ ] After authorization, squash merge, tag, publish the release, verify
  post-merge/tag CI, deploy/live-check Pages, and report final state.

## Explicitly deferred

Positional arguments, `--` terminator semantics, per-command versioning,
typed getters, execution-context redesign, the major `TCLIApplication` split,
renderer extraction, help architecture redesign, exception hierarchy changes,
deprecated API removal, presentation/data separation, Unicode display-width
redesign, historical archive reorganization, sanitizer policy expansion, and
new dependencies remain out of scope.

## Risks and mitigations

| Risk | Mitigation |
| --- | --- |
| Removing command-level version suggestions changes completion output | Keep application-level handling unchanged and add explicit root/named tests. |
| A one-flag parameter regresses completion | Test short-only, long-only, and two-flag definitions through the real engine. |
| Current-version edits rewrite history | Limit edits to active metadata/current docs and preserve v1.5.0 release sources. |

---

# Implementation Plan: cli-fp v1.5.3 Generator Safety & Contract Corrections

## Overview

Deliver a conservative patch release from the immutable v1.5.2 baseline. The
release prevents a manifest from claiming arbitrary user files, fixes proven
generator contracts, preserves normal 1.x runtime behavior, and records only
the directly related documentation and version metadata.

## Architecture decisions

- A stale manifest is input, not proof of file ownership. Cleanup accepts only
  the generated program file under `src/` and generated artifacts under
  `src/generated/`; command stubs remain user-owned.
- Generator specs and manifests use `/` on disk while path conversion happens
  at the filesystem boundary, retaining `\\` input compatibility.
- Collision resolution will be deterministic and narrowly applied only where
  an emitted class conflicts with a framework/generated identifier.
- Enum parsing will have one shared internal implementation for validation and
  completion; Boolean parser behavior will remain unchanged.

## Task list

### Phase 1: Safety boundary

- [ ] Add failing manifest ownership, malformed JSON/type, stale-file, and
  portable-path regressions using isolated temporary projects.
- [ ] Implement ownership enforcement and resource-safe manifest loading.
- [ ] Verify path-escape and link/reparse protections remain effective.

### Checkpoint: generator safety

- [ ] Focused generator tests pass and malicious manifests leave user files
  untouched while legitimate stale generated output is removed.

### Phase 2: Generator/package correctness

- [ ] Reproduce generated `root`/`base` collisions with actual compile smoke
  tests and apply the smallest collision-safe naming correction.
- [ ] Canonicalize emitted program paths, retain legacy input compatibility,
  and update Lazarus package unit metadata after clean-package verification.

### Checkpoint: generator correctness

- [ ] Generator unit, golden, operations/lifecycle, and compile-smoke tests
  pass; generated collision projects compile.

### Phase 3: Runtime-contract characterization and documentation

- [ ] Add completion/validation enum consistency and Boolean behavior tests
  without changing Boolean runtime semantics.
- [ ] Correct current documentation for Boolean usage, actual exception
  behavior, generator ownership, and portable paths.
- [ ] Update only current v1.5.3 metadata, changelog, roadmap, and DocKit
  registry; preserve historical release sources.

### Checkpoint: qualification and review

- [ ] Run framework, generator, examples, clean package, clean checkout,
  documentation, and diff checks.
- [ ] Perform code review, commit the qualified candidate, push the release
  branch, create the PR, and observe Linux/Windows CI.

### Phase 4: authorization-gated release

- [ ] Stop after green PR CI for explicit merge authorization.
- [ ] After authorization only: squash merge, annotate/push v1.5.3, publish,
  verify tag CI/Pages/current and immutable docs, and clean state.

## Risks and mitigations

| Risk | Mitigation |
| --- | --- |
| Invalid stale manifest causes data loss | Refuse unowned entries before delete and test representative protected files. |
| Naming correction churns normal output | Limit the change to reserved collisions and compile generated projects. |
| Portable-path fix breaks existing specs | Normalize on load, serialize canonically, and test both separators. |
| Package build masks missing units through source search paths | Inspect package graph and build from a clean isolated checkout. |
