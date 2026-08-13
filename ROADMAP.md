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

## v1.3.3 — Stabilize Before Expanding (next)

This is a focused stabilization release. It should make the current framework
safer to maintain before v1.4.0 adds another public entry point.

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
  those changes remain planned for v1.5.0 and v2.0.0.
- No large historical-documentation cleanup mixed into the behavioural fixes.

**Maintenance outcome:** the repository can be cleaned safely, examples remain
buildable, test results do not depend on stale compiler units, diagnostics do
not expose password values, and the test suite provides a dependable safety
net for the v1.4.0 ergonomics work.

## v1.4.0 — Make Simple CLIs Simple

- Introduce a single beginner-facing facade with a callback-based command API.
- Add typed argument access for strings, integers, booleans, floats, and other
  supported parameter types so command code does not parse validated strings a
  second time.
- Implement the simple API as a facade over the existing command machinery so
  parsing and validation remain single-sourced.
- Keep the class-based `TBaseCommand` API available for advanced applications.
- Make the primary example a single source file that does not require the
  project generator.

**Maintenance outcome:** beginner-oriented ergonomics improve without creating
a second framework to maintain.

## v1.5.0 — Finish the Application Core Boundaries

- Separate command selection and execution orchestration from parsing and
  validation.
- Extract Bash and PowerShell script rendering from `TCLIApplication`, building
  on the completion engine introduced in v1.3.3.
- Strengthen the internal help and completion boundaries introduced in v1.3.3
  without exposing them as new public APIs.
- Preserve existing observable behaviour with the v1.3.3 characterization
  tests and focused tests around each extracted component.
- Keep these internal changes behind the stable public facade.

**Maintenance outcome:** changes to help, completion, parsing, and execution
can be made and tested independently.

## v2.0.0 — Make Execution State Explicit

- Adopt an explicit execution-context contract for commands.
- Remove legacy shared-state plumbing between the application and commands.
- Remove test-only methods and mutable implementation details from the public
  concrete application surface.
- Remove APIs deprecated during the 1.x releases.

**Maintenance outcome:** command inputs and ownership are explicit, legacy
compatibility paths are retired, and the core has one coherent execution
model.
