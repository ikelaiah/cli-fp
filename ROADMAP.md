# cli-fp Roadmap

The runtime library is the primary product. This roadmap prioritizes a smaller
public API, clearer internal boundaries, trustworthy tests, and lower
maintenance cost.

New convenience APIs must delegate to one underlying implementation rather
than creating parallel parsing, validation, help, completion, or execution
paths. Examples are executable documentation and should be verified in CI.

## v1.3.2 — Correct the Current Contracts (completed 2026-07-30)

- Remove or clearly deprecate the non-functional completion callback APIs.
- Fix the mismatch between the public `ICommand` contract and the runtime
  requirement for `TBaseCommand`.

**Maintenance outcome:** public contracts accurately describe what the
framework supports, and command execution no longer relies on a hidden unsafe
downcast.

## v1.3.3 — Stabilize Before Expanding (implementation complete; release 2026-08-14)

This focused stabilization release makes the current framework safer to
maintain before the documentation-first v1.4.0 release improves how developers
learn and navigate the current public API.

### Safe repository maintenance

- Fix `clean-all-examples.sh` and `clean-all-examples.ps1` so they remove only
  generated build artifacts and never delete tracked completion scripts,
  documentation, or other user-owned files.
- Add cross-platform cleanup smoke checks that build the examples, run the
  cleanup scripts, and confirm tracked files remain intact.

### Trustworthy behavioural coverage

- Replace placeholder help tests with assertions that exercise real help
  output, including usage, descriptions, required options, defaults, and
  subcommands.
- Add an internal output-capture seam for tests without expanding the public
  runtime API.
- Compile all seven canonical examples in Windows and Linux CI so learning
  material cannot silently drift away from the framework.

### Parser correctness

- Accept separated negative numeric values such as `--count -1` for registered
  integer and float options, matching the existing `--count=-1` behaviour.
- Preserve unknown-option detection and add regression coverage for both
  numeric forms.

### Hermetic tests and safe diagnostics

- Make the Windows and Linux framework test runners rebuild the unit graph
  with the test define into an isolated output directory. A previous normal
  build must not leave a stale `.ppu` that changes whether the tests compile.
- Keep output-capture state and entry points out of normal runtime builds while
  retaining one production execution and help-rendering path.
- Redact values for registered password parameters from debug output and add a
  regression test proving credentials are never printed.

### Internal maintenance boundaries

- Move help formatting into one internal renderer shared by the application
  and base-command paths.
- Move completion calculation into a focused internal engine and delete
  unreachable private callback branches and unused temporary allocations,
  while retaining the deprecated public 1.x no-op methods.
- Single-source parameter lookup and password redaction for validation,
  execution, and diagnostics.
- Decompose application dispatch into focused stages without changing the
  `TCLIApplication` facade or `ICLIApplication` contract.

### Release acceptance criteria

- Cleanup scripts leave all tracked files intact.
- Framework, generator, and example compilation checks pass on Windows and
  Linux.
- Help tests fail when required help content is removed or changed incorrectly.
- Negative integer and float values work in equals and separated forms.
- Framework tests pass after a normal non-test build has produced reusable
  units in the source tree or another configured unit-search directory.
- Normal runtime builds contain no test-output capture state or entry points.
- Debug output never prints values supplied to password parameters.
- Help rendering, completion calculation, and parameter-value semantics each
  have one internal implementation covered by characterization tests.
- Every behaviour changed in v1.3.3 is documented and has automated coverage.

### Non-goals

- No new public command API or breaking API changes.
- No new parameter kinds, generator capabilities, or completion features.
- No replacement of the `TCLIApplication` facade or execution-state contract.
- No removal of public compatibility APIs or broad completion/help cleanup;
  those changes remain planned for v1.6.0 and v2.0.0.
- No large historical-documentation cleanup mixed into the behavioural fixes.

**Maintenance outcome:** the repository can be cleaned safely, examples remain
buildable, test results do not depend on stale compiler units, diagnostics do
not expose password values, and the test suite provides a dependable safety
net for the v1.4.0 documentation work.

## v1.4.3 — Historical Documentation Publishing (completed 2026-09-11)

- Publish DocKit historical/versioned documentation from immutable cli-fp
  release sources rather than one moving `latest` site.
- Preserve existing release tags and add the migration as a new documentation
  maintenance release.

**Maintenance outcome:** readers can select documentation that matches a
released cli-fp version without changing prior release history.

## v1.4.2 — Correctness and Maintenance Patch (completed 2026-09-10)

- Make progress bars safe at zero and over-total boundaries.
- Preserve caller-owned date/time formatting state during validation.
- Make all completion protocol branches directive-complete and generated Bash
  and PowerShell scripts safe for shell-sensitive executable names and paths.
- Remove obsolete source backup files and correct the DebugMode recipe.

**Maintenance outcome:** v1.4.1 behavior is more robust without expanding the
public API or command model.

## v1.4.1 — Documentation Accuracy Patch (completed 2026-09-09)

- Correct published snippets so the class-based command relationship is
  explicit: a `TBaseCommand` descendant, its option-owning instance, and the
  `ICLIApplication` registration that invokes `Execute`.
- Keep the v1.4.0 learning path and executable QuickStart intact while making
  recipes and reference fragments safe to read in context.

**Maintenance outcome:** readers can follow the current public API without
inventing command, application, or terminal variables that the documentation
did not define.

## v1.4.0 — Documentation and Developer Learning Experience (completed)

- Publish a task-oriented documentation path that starts with a compiling
  example, distinguishes root, named, and nested command shapes, and makes
  supported limitations easy to find.
- Add concise How-To recipes for the current class-based API, parameter types,
  terminal UX, completion, debugging, and `cli-fp-gen` workflows.
- Make canonical beginner examples executable documentation in the existing CI
  example build and cleanup smoke checks.
- Keep tutorials, API reference, and maintainer material separate so each
  concept has one primary home.

**Maintenance outcome:** developers can find the shortest correct path for a
common task without guessing which manual or reference page contains it.

## v1.5.0 — Defensive CLI Core (completed 2026-09-11)

- Validate command names, parameter flags, duplicate definitions, nil commands,
  and command-tree cycles before dispatch or rendering.
- Reject unsupported positional arguments clearly; preserve last-occurrence-wins
  option semantics and deterministic global help handling.
- Harden completion/Pascal output, terminal text, colour handling, progress
  widths, and spinner update cadence.
- Improve the beginner path with prerequisite, ownership, root-command, and
  troubleshooting guidance while keeping the class-based public facade.

**Maintenance outcome:** valid v1.x CLIs remain familiar while malformed
definitions and unsafe edge cases fail predictably.

## v1.5.1 — Correctness and Completion Patch (completed 2026-09-12)

- Align direct option lookup with the parser's case-insensitive flag semantics.
- Keep completion free of empty flag candidates for one-form parameters.
- Keep version explicitly application-level in validation and completion.
- Clarify current API and learning documentation without changing the public
  facade.

**Maintenance outcome:** existing v1.x command implementations keep their
runtime shape while lookup, completion, and built-in version boundaries are
consistent and predictable.

## v1.5.2 — Safety and Documentation Corrections (completed 2026-09-12)

- Sanitize caller-controlled progress captions using the established terminal
  text policy without changing renderer redraw semantics.
- Correct runnable example guidance, portable demo path construction, the
  ProgressDemo compiler directive, and directly related current documentation.
- Keep current DocKit metadata ready for the immutable `v1.5.2` release
  snapshot.

**Maintenance outcome:** progress output and example documentation remain safe,
portable, and truthful without changing the v1.x command model or public API.

## v1.6.0 — Finish the Application Core Boundaries

- Continue internal architecture cleanup without changing the public facade.
- Separate command selection and execution orchestration from parsing and
  validation.
- Extract Bash and PowerShell script rendering from `TCLIApplication`, building
  on the completion engine introduced in v1.3.3.
- Deduplicate command lookup, path-building, and output helpers.
- Improve console/progress internals and test seams.
- Strengthen the internal help and completion boundaries introduced in v1.3.3
  without exposing them as new public APIs.
- Preserve existing observable behaviour with the v1.3.3 characterization
  tests and focused tests around each extracted component.
- Keep these internal changes behind the stable public facade.

**Maintenance outcome:** changes to help, completion, parsing, and execution
can be made and tested independently.

## v2.0.0 — Make Execution State Explicit

- Evaluate actual positional-argument support and `--` terminator semantics
  together as one parser design.
- Adopt an explicit execution-context contract for commands.
- Remove legacy shared-state plumbing between the application and commands.
- Remove test-only methods and mutable implementation details from the public
  concrete application surface.
- Remove APIs deprecated during the 1.x releases.
- Reconsider the exception hierarchy and other breaking parser/API
  simplifications.

**Maintenance outcome:** command inputs and ownership are explicit, legacy
compatibility paths are retired, and the core has one coherent execution
model.
