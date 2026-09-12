# cli-fp v1.5.1 — Correctness and Completion Patch

cli-fp v1.5.1 is a focused maintenance release for the existing 1.x command
model.

## Fixed

- Direct `TBaseCommand.GetParameterValue` lookups are case-insensitive for
  both short and long flags, and missing lookups clear their output value.
- Completion omits empty candidates when a parameter has only one flag form.
- `--version` and `-v` remain application-level requests. Named commands now
  reject them during validation and do not advertise them in completion.

## Documentation and metadata

- Updated active documentation, package metadata, changelog, roadmap, and the
  DocKit version registry for 1.5.1.
- Preserved the v1.5.0 and older versioned documentation entries and release
  history.

## Compatibility and scope

This patch does not change the public facade or normal class-based command
model. Positional arguments, the `--` terminator, per-command versions, typed
getters, execution redesign, custom completion callbacks, and other v1.6+/v2
work remain deferred.
