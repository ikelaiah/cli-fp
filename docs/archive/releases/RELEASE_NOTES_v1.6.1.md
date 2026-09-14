# cli-fp v1.6.1 — Version requests, alias ordering, and verified guides

`-v` and `--version` now always request the application version when used as
options at root, named, or nested command scope. Matching is case-insensitive,
so `-V` is also reserved. Requests return 0 and skip validation and dispatch.
Option values such as `--name=--version` remain values.

This intentionally corrects previously accepted conflicting registrations and
named-command rejection. Runtime definitions and generator specifications that
reuse these flags now fail with a developer-facing error. Migrate verbosity to
`-d`/`--verbose` or long-only `--verbose`; help, completion and current examples
have been updated. The generator tool's own `init --version <value>` continues
to set generated application metadata.

Repeated options now use the last occurrence across short/long aliases and
case variants: `--name Alice -n Bob` and `-n Alice --NAME=Bob` both yield `Bob`.
Validation and command retrieval share that ordering. Long equals syntax,
negative numeric values, empty/default semantics and password redaction are
covered by regression tests.

The current reader guides now explain executable paths, factory versus
execution, safe recipe insertion, Boolean defaults/requiredness and lookup
results, default colour reset, non-empty `NO_COLOR`, locale-sensitive floats,
and Pascal identifiers versus filesystem casing. Required Linux/Windows
Markdown recipe checks compile snippets and run the documented commands.
Existing class-based signatures and historical documentation are preserved.
