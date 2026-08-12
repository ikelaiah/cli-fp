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

### Release acceptance criteria

- Cleanup scripts leave all tracked files intact.
- Framework, generator, and example compilation checks pass on Windows and
  Linux.
- Help tests fail when required help content is removed or changed incorrectly.
- Negative integer and float values work in equals and separated forms.
- Every behaviour changed in v1.3.3 is documented and has automated coverage.

### Non-goals

- No new public command API or breaking API changes.
- No new parameter kinds, generator capabilities, or completion features.
- No broad `TCLIApplication` split; that remains planned for v1.5.0.
- No large historical-documentation cleanup mixed into the behavioural fixes.

**Maintenance outcome:** the repository can be cleaned safely, examples remain
buildable, and the test suite provides a dependable safety net for the v1.4.0
ergonomics work.

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

## v1.5.0 — Split the Application Core

- Separate command selection and execution orchestration from parsing and
  validation.
- Extract help rendering from `TCLIApplication` behind the output seam proven
  in v1.3.3.
- Separate completion calculation from Bash and PowerShell script rendering.
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
