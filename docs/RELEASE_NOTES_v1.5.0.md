# cli-fp v1.5.0 — Defensive CLI Core

Release date: 2026-09-11

v1.5.0 keeps the familiar class-based `TBaseCommand` / `ICLIApplication`
facade and makes invalid definitions and edge-case output safer.

## Highlights

- Invalid command names, flags, duplicates, nil commands, and cyclic command
  trees fail early with actionable exceptions.
- Unexpected positional arguments now produce an error and exit code `1`.
  Duplicate options continue to use the last value supplied.
- Parameter lookup is consistently case-insensitive, missing values clear the
  output variable, and enum values may contain spaces.
- Completion scripts, terminal text, and generated Pascal literals are safer
  around shell-sensitive and control characters.
- Colour disables itself for redirected output and `NO_COLOR`; progress widths
  are capped at 200 characters; spinner updates do not block with a sleep.

## Compatibility

Root, named, and nested command programs remain supported. The empty command
name still means the root/default action. Date/time help documents
`YYYY-MM-DD HH:MM`; existing values with seconds remain accepted for v1.x
compatibility.

Positional-argument APIs, `--` terminator semantics, per-command versions, a
major `TCLIApplication` split, and removal of deprecated/no-op APIs are
deliberately deferred to later design work.

See the [full changelog](../CHANGELOG.md) and the [current limitations](limitations.md)
for details.
