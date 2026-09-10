# Implementation Plan: cli-fp v1.5.0 Defensive CLI Core

## Overview

Harden the existing v1.4.3 command/parameter model, make parser failures
predictable, secure generated output, fix confirmed value and terminal bugs,
and publish a beginner-oriented v1.5.0 release without redesigning the
public facade or adding positional-argument semantics.

## Architecture decisions

- Keep validation in small shared helpers in the runtime and generator rather
  than changing the public interfaces or adding dependencies.
- Treat an empty command name as valid only for the configured root command;
  named application commands and subcommands must use valid single tokens.
- Preserve last-write-wins option parsing and reject unexpected positional
  arguments explicitly.
- Keep shell emitters in the current application unit for v1.5.0; defer the
  larger renderer extraction to v1.6.x.
- Encode Pascal strings as valid literals with `#` character fragments for
  line breaks/control characters rather than silently corrupting generated
  source.

## Task list

### Phase 1: Runtime contracts and regression coverage

- [x] Add focused tests for malformed definitions, parser edge cases, value
  initialization, enum/date-time correctness, and help behavior.
- [x] Add reusable command/flag validation and cycle detection while preserving
  root-command semantics.
- [x] Reject unexpected positional arguments and make help requests with extra
  arguments deterministic.
- [x] Fix case-consistent parameter lookup, literal enum splitting, documented
  date-time format, and empty flag filtering.

### Checkpoint: Runtime

- [x] Framework tests pass and normal root/named/nested examples still compile.

### Phase 2: Output, generator, and terminal hardening

- [x] Quote/escape Bash and PowerShell completion metadata and sanitize unsafe
  terminal controls in rendered text.
- [x] Harden generator validation, argument parsing, corrupt-manifest/spec
  failures, and Pascal literal rendering.
- [x] Make color reset exception-safe, bound progress width, remove spinner
  update sleeping, and improve line clearing.
- [x] Extend generator/completion/terminal regression tests.

### Checkpoint: Qualification

- [x] Framework, generator, golden, compile-smoke, cleanup, completion, and
  documentation checks pass where supported by the environment.

### Phase 3: Documentation and release

- [x] Update beginner docs, limitations, API/reference pages, navigation,
  changelog, roadmap, and DocKit version metadata for v1.5.0.
- [x] Run review and final diff checks; commit coherent changes.
- [ ] Push, create/check/merge PR, tag, publish release, and verify Pages if
  GitHub authentication and required checks permit remote operations.

## Deferred by design

- Positional-argument APIs and `--` terminator semantics remain later design
  work because the v1.x parser has no positional destination.
- Per-command versioning remains unsupported; version stays application-level.
- The major `TCLIApplication` structural split remains v1.6.x work.
- Deprecated/no-op public completion callbacks remain source-compatible until
  v2.0.0.

## Risks and mitigations

| Risk | Mitigation |
| --- | --- |
| Existing valid command declarations are rejected | Validate only malformed token shapes and preserve root `''`. |
| Shell quoting fix creates invalid scripts | Add generated-script inspection and shell syntax tests. |
| FPC 3.2.2 differences | Use repository runners and CI-equivalent commands on Windows. |
| Release permissions unavailable | Finish local qualification and report exact remote blocker/actions. |
