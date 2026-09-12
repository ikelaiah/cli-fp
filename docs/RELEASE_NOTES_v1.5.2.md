# cli-fp v1.5.2 — Safety and Documentation Corrections

cli-fp v1.5.2 is a focused maintenance release. It keeps the existing 1.x
command model and public API unchanged.

## Fixed

- `TProgressIndicator.RenderText` now applies the established terminal-text
  sanitization policy to caller-controlled render text. NUL and ESC characters
  are removed, while the renderer's own carriage-return redraw remains intact.
- LongRunningOpDemo documents a working invocation with its required
  `--input` option and uses portable path construction.
- ErrorHandlingDemo explains that its simulated validation results vary per
  run and uses portable path construction.
- ProgressDemo now uses the project's standard `{$J-}` compiler-safety
  directive.
- The completion-testing index links to the current completion documentation,
  and `example-bin/README.md` now accurately describes the source-only binary
  policy and all canonical examples.

## Compatibility

Normal v1.5.1 applications remain source- and behavior-compatible. No parser
grammar, command ownership, completion architecture, public API, or exception
hierarchy changes are included.

## Deferred work

The planned v1.6.0 internal extraction and application-core work remains
deferred. Parser grammar changes and public API redesign remain out of scope
for this patch release.
