# Release Notes - cli-fp v1.4.1

`v1.4.1` is a documentation-accuracy patch. It does not add or change a
runtime API, parser behavior, generator behavior, or the supported
class-based command model.

## What changed

- Reader-facing Pascal snippets now explicitly distinguish a developer-defined
  `TBaseCommand` descendant, its concrete command instance, and its
  registration with `ICLIApplication`.
- How-To, Commands, Options, and Terminal guidance now state whether a block
  is a complete command pattern, a program-setup fragment, or an
  `Execute`-method fragment, including required units and variable scope.
- The API reference labels signature declarations and setup context; technical
  documentation labels its implementation excerpts as non-standalone source
  context.

## Upgrade notes

No source migration is required. Existing applications, generator projects,
and the public runtime API remain unchanged.

See the [changelog](../CHANGELOG.md) for the complete release history.
