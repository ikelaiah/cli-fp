# cli-fp Roadmap

This roadmap prioritizes a smaller public API, clearer internal boundaries, and
lower maintenance cost. New convenience APIs should delegate to one underlying
implementation rather than creating parallel parsing, validation, or execution
paths.

## v1.3.2 — Correct the Current Contracts

- Remove or clearly deprecate the non-functional completion callback APIs.
- Fix the mismatch between the public `ICommand` contract and the runtime
  requirement for `TBaseCommand`.

**Maintenance outcome:** public contracts accurately describe what the
framework supports, and command execution no longer relies on a hidden unsafe
downcast.

## v1.4.0 — Make Simple CLIs Simple

- Introduce a simple callback-based command API.
- Add typed argument access for strings, integers, booleans, floats, and other
  supported parameter types.
- Implement the simple API as a facade over the existing command machinery so
  parsing and validation remain single-sourced.

**Maintenance outcome:** beginner-oriented ergonomics improve without creating
a second framework to maintain.

## v1.5.0 — Split the Application Core

- Extract help rendering from `TCLIApplication`.
- Extract completion calculation and shell-completion script generation from
  `TCLIApplication`.
- Preserve existing observable behaviour with focused tests around each
  extracted component.

**Maintenance outcome:** changes to help, completion, parsing, and execution
can be made and tested independently.

## v2.0.0 — Make Execution State Explicit

- Adopt an explicit execution-context contract for commands.
- Remove legacy shared-state plumbing between the application and commands.
- Remove APIs deprecated during the 1.x releases.

**Maintenance outcome:** command inputs and ownership are explicit, legacy
compatibility paths are retired, and the core has one coherent execution
model.

