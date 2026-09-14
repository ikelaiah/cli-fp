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

---

# Implementation Plan: cli-fp v1.5.4 Validation, Examples & Documentation Accuracy

## Overview

Deliver a narrow patch from the released `v1.5.3` baseline. Verify the two
reported generator contracts against the runtime, repair only proven example
and documentation defects, archive historical material without losing links,
and qualify the Lazarus package from clean inputs before release.

## Architecture decisions

- Generator enum/default validation will use the runtime's existing
  case-comparison semantics; no second enum policy will be introduced.
- A parameter may have either a valid short or valid long flag, matching the
  public runtime API.
- The automatically generated Lazarus package unit remains unchanged unless a
  clean package build proves its metadata is defective.
- Historical material will move under `docs/archive/`, with current guides
  retaining canonical navigation and corrected relative links.

## Task list

### Phase 0: baseline

- [x] Record the v1.5.3 candidate SHA and run the Windows-equivalent framework,
  generator, example-cleanup, and Lazarus package qualifications.

### Phase 1: generator validation contracts

- [x] Add failing regressions for short-only, long-only, dual, missing,
  malformed, and duplicate flags.
- [x] Add failing enum default/allowed-value regressions using the actual
  runtime casing behaviour.
- [x] Apply the minimal validator corrections and run focused generator tests.

### Phase 2: executable examples

- [x] Repair the confirmed ErrorHandlingDemo and LongRunningOpDemo teaching
  defects and audit all eight canonical examples for concrete issues.
- [x] Compile all examples on the local Windows toolchain.

### Phase 3: accurate current documentation and archive

- [x] Correct verified current-doc/API/codegen/completion inaccuracies.
- [x] Archive release notes, PR records, completion work logs, and historical
  output with an archive index; update navigation and relative links.
- [x] Inspect tracked artifacts and simplify ignore rules only where justified.

### Checkpoint: v1.5.4 qualification

- [x] Run framework, generator unit/golden/operations/compile-smoke, all
  examples, cleanup smoke, clean Lazarus package build, docs/link checks, and
  diff checks.
- [x] Update version metadata, changelog, roadmap, and current release
  documentation.
- [x] Review, commit, push, open PR, observe Windows/Linux CI, merge, tag,
  publish, and verify Pages before starting v1.6.0.

### Qualification record

- v1.5.3 baseline: `216aefb` (`v1.5.3`); release candidate branch:
  `release/v1.5.4`.
- Windows/FPC 3.2.2: 63 framework tests; 28 generator unit tests plus golden,
  lifecycle/operations, and compile-smoke; all eight examples; cleanup smoke;
  normal and isolated `lazbuild --build-all packages/lazarus/cli_fp.lpk`.
- Documentation local-link and diff checks pass. Generated compiler byproducts
  are absent from the candidate and no build artifacts are tracked.
- Merge commit: `1f755e8ec8367daf35fcfc201b5e0cae7ecc74c4`; annotated `v1.5.4`
  and its GitHub Release target that exact commit. The successful Linux and
  Windows post-merge jobs each covered the complete suite. An earlier Windows
  attempt was cancelled during Chocolatey/Lazarus installation, before tests,
  and is recorded as toolchain infrastructure rather than a test failure.
- The `v1.5.4` Pages deployment completed successfully and
  `https://ikelaiah.github.io/cli-fp/` returned HTTP 200 with the released
  version before `release/v1.6.0` was created from `main`.

## Deferred by release boundary

The completion-renderer extraction, test-suite restructuring, required
completion CI, and architecture ADRs are v1.6.0 work and will not be mixed
into this patch.

## Risks and mitigations

| Risk | Mitigation |
| --- | --- |
| Validator changes drift from runtime | Characterize runtime enum matching first, then test generator inputs. |
| Historical moves break user links | Search inbound links and validate all relative links after each move. |
| Local Lazarus cache masks package issues | Build from an isolated clean copy and output directory. |

---

# Implementation Plan: cli-fp v1.6.0 Internal Architecture, Test Structure & Maintainability

## Overview

Deliver a maintenance-focused minor release from the released `v1.5.4` merge
commit `1f755e8ec8367daf35fcfc201b5e0cae7ecc74c4`. Make completion-script
rendering independently testable, make completion checks visible and required
in both CI environments, improve test ownership only where it preserves
behavioural coverage, and record the resulting internal boundaries. The public
1.x API and command model remain unchanged.

## Architecture decisions

- `TCLIApplication` remains the public application facade. Its public
  completion entry points, command selection, parsing coordination, dispatch,
  help orchestration, validation interaction, and completion orchestration stay
  owned by the facade unless a small cohesive move proves lower-risk.
- A single internal `CLI.Internal.CompletionScripts` unit will own Bash and
  PowerShell rendering, shell quoting, and command-tree formatting. It will
  return ordered rendered lines with their existing output-routing marker, so
  the facade can preserve the current distinction between direct console output
  and framework-test capture without changing generated script text.
- The extraction is a characterization refactor: generated output is not
  intentionally changed. Full deterministic Bash and PowerShell renderings,
  including shell-sensitive names, will have regression coverage before the
  implementation is moved.
- Test organisation will be changed only after the completion contracts are
  protected. Completion behaviour may move to a focused FPCUnit test unit with
  shared fixture helpers; unrelated tests remain in place if a split creates
  coupling or runner risk.
- The existing `CLI.Errors` hierarchy remains public 1.x compatibility API.
  Documentation will state its actual runtime scope; no exception-routing
  redesign, deletion, or deprecation is planned.

## Non-goals

- Do not replace `TCLIApplication`, introduce an execution context or v2
  contract, redesign parameters, remove compatibility APIs, add dependencies,
  or split files merely to reduce line counts.
- Do not make `CLI.Errors` a breaking runtime contract, invent speculative
  abstractions, or change generated completion output without a separately
  demonstrated defect.

## Task list

### Phase 0: baseline and release record

- [x] Confirm `v1.5.4`, GitHub Release, Pages, clean tree, and the exact
  `main` commit before branching `release/v1.6.0`.
- [x] Run and record the isolated framework baseline, including current
  completion behaviour, before structural changes.

### Phase 1: characterize and extract completion scripts

- [x] Add failing deterministic full-rendering characterization tests for
  Bash and PowerShell scripts, including quoting, tree metadata, debug output,
  and the existing capture-routing contract.
- [x] Create the internal completion-script renderer and retain thin
  `TCLIApplication` wrappers that delegate to it without public API changes.
- [x] Add the internal unit to the Lazarus package with
  `AddToUsesPkgSection=False` and prove normal plus isolated clean-package
  builds resolve it.

### Checkpoint: completion renderer

- [x] Framework tests demonstrate byte-for-byte expected script lines and
  pre-existing completion candidates remain unchanged.
- [x] The resulting `CLI.Application` has a documented, coherent facade role;
  no further application split is made without a similarly cohesive boundary.

### Phase 2: test ownership and required completion CI

- [x] Move completion-focused FPCUnit cases into a focused unit only if the
  runner keeps all existing behaviour and test count intact; add only the new
  characterization coverage required by Phase 1.
- [x] Add deterministic Bash completion qualification to Linux CI and
  PowerShell completion qualification to Windows CI, with direct failures and
  no best-effort or fake-green steps.
- [x] Keep ad-hoc historical shell probes separate from the required test
  path and update their documentation accordingly.

### Phase 3: maintenance documentation

- [x] Document the retained application-facade responsibilities and internal
  completion-renderer boundary in the technical documentation; add a concise
  ADR if it provides durable decision context.
- [x] Document `CLI.Errors` as retained public compatibility types with the
  current execution-path behaviour, without API churn.
- [x] Correct stale comments, naming/casing guidance, completion-registration
  labels, and documentation-fragment conventions only where verified.

### Phase 4: qualification and release

- [x] Update current 1.6.0 metadata, changelog, roadmap, and DocKit metadata;
  preserve historical release sources.
- [ ] From clean inputs run framework and completion suites, all generator
  checks, eight example builds, cleanup smoke, normal and isolated Lazarus
  package builds, documentation/link checks, public-API comparison with
  `v1.5.4`, and repository-hygiene checks.
- [ ] Perform code review, push the release branch, open a PR, and verify the
  required Linux and Windows suites on the merged commit.
- [ ] Annotate the exact merged commit `v1.6.0`, create the GitHub Release,
  verify tag/Pages deployment and the live documentation, then verify a clean
  working tree.

## Risks and mitigations

| Risk | Mitigation |
| --- | --- |
| Refactor alters generated shell text or test capture | Characterize full ordered lines and retain a line-routing marker in the internal model. |
| Test splitting loses registration or coverage | Keep FPCUnit registration explicit and compare counts/behaviour before and after. |
| Shell checks vary by host | Use deterministic rendered-script assertions plus Bash on Linux and PowerShell on Windows. |
| Internal cleanup leaks into a public redesign | Keep the application facade and exported units unchanged; compare the v1.5.4 public API before release. |
