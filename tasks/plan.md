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
- [ ] Add focused failing regressions for case-insensitive descendant lookup,
  empty completion suggestions, and named-command version rejection.

### Phase 2: Surgical runtime fixes

- [ ] Make `TBaseCommand.GetParameterValue` use case-insensitive flag matching
  while preserving miss initialization.
- [ ] Guard both flag forms independently in every relevant completion path.
- [ ] Keep version global at application level; reject and avoid suggesting it
  after named-command selection.

### Checkpoint: Runtime patch

- [ ] Focused regressions fail before the fixes and pass afterward.
- [ ] Existing v1.5.0 behavior remains covered and passing.

### Phase 3: Documentation and release metadata

- [ ] Correct stale current 1.x wording and label `GetParameterValue` as a
  protected descendant member in the API reference.
- [ ] Update authoritative current version locations to 1.5.1, add concise
  changelog/release notes, update roadmap context, and add DocKit v1.5.1.
- [ ] Preserve historical release notes and tags unchanged.

### Checkpoint: Qualification

- [ ] Framework, generator, completion, example, cleanup, golden,
  compile-smoke, docs, DocKit, and diff checks pass where supported.
- [ ] Code review finds no required correctness, security, architecture, or
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
