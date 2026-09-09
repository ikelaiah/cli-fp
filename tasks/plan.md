# Implementation Plan: cli-fp v1.4.1 documentation accuracy patch

## Overview

Correct the published v1.4.0 documentation so readers always see the normal
class-based cli-fp model: define a `TBaseCommand` descendant, create its
instance, register options on that instance, and run its `Execute` method via
an `ICLIApplication`. This is a documentation-only release; no runtime API,
parser, or generator behaviour changes.

## Architecture Decisions

- Treat the Getting Started QuickStart as the canonical full program; use
  clearly labelled command patterns and in-context fragments elsewhere.
- Give every reader-facing Pascal block either local declarations or an
  explicit immediate context statement. Do not rely on implied `App`, `Root`,
  `Command`, spinner, or progress variables.
- Keep `docs/how-to.md` as one task-oriented page, but introduce the command
  hierarchy there and link outward instead of duplicating full programs.
- Keep technical documentation as source-context excerpts and label that scope
  plainly rather than presenting implementation fragments as application code.

## Task List

### Phase 1: Context audit and reader path

- [ ] Task 1: Audit every published Pascal block and record its category
  (complete program, command pattern, or explicitly scoped fragment).
- [ ] Task 2: Add the command/object/application mental model to Getting
  Started, Commands, and How-To; repair root, named, and nested command setup.

### Checkpoint: Reader context

- [ ] A reader of How-To alone can identify the developer-defined
  `TBaseCommand` descendant, the option-owning instance, and `Execute`.
- [ ] No reader-facing Pascal block contains an unexplained identifier.

### Phase 2: Recipe and reference accuracy

- [ ] Task 3: Repair option, value-retrieval, exit-code, terminal, progress,
  and debug recipes with exact units, receiver types, and scopes.
- [ ] Task 4: Audit reference, technical, release, and navigation pages;
  update v1.4.1 version records without changing runtime claims.

### Checkpoint: Documentation qualification

- [ ] DocKit check, strict audit, and build succeed.
- [ ] Framework, generator, and canonical-example checks succeed as applicable.

### Phase 3: Review and release

- [ ] Task 5: Review the diff for documentation/API accuracy, commit the
  candidate, and publish the authorised PR.
- [ ] Task 6: Qualify the exact candidate in CI, merge, tag v1.4.1, create the
  GitHub release, and verify rendered Pages content.

## Risks and Mitigations

| Risk | Impact | Mitigation |
| --- | --- | --- |
| A concise snippet still looks standalone | High | State its category and scope immediately before it; favour the canonical QuickStart link. |
| Documentation drifts from source | High | Compare every touched API call to `src/` and compile all existing canonical examples. |
| Docs-only changes do not trigger tests CI | Medium | Run local qualification and manually dispatch the existing Tests workflow for the PR branch. |
| Pages deployment masks stale content | Medium | Verify route-specific rendered text after the main-branch deploy finishes. |
