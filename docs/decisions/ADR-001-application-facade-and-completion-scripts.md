# ADR-001: Retain the application facade and extract completion-script rendering

## Status

Accepted

## Date

2026-09-14

## Context

`TCLIApplication` is the stable 1.x application façade. It owns process
arguments, registered command state, global requests, test-output capture, and
the public execution flow. Its former Bash and PowerShell rendering block mixed
those concerns with deterministic shell formatting and quoting, making a
behaviour-preserving refactor hard to test without redirecting process output.

## Decision

Keep `TCLIApplication` as the façade for application setup, root and named
command selection, parsing coordination, dispatch, help orchestration, and
completion orchestration. Move only Bash and PowerShell script construction to
the internal `CLI.Internal.CompletionScripts` unit.

The renderer returns ordered lines with the pre-existing output-routing marker.
The façade remains responsible for choosing direct console output or the
test-capture path. The new unit is an internal Lazarus package member and is
not added to the public generated package `uses` surface.

## Alternatives considered

- Split parsing, dispatch, help, and completion into several new application
  units. Rejected: the remaining façade responsibilities are cohesive and a
  larger split would expand risk without a confirmed maintenance problem.
- Let the renderer write directly to the console. Rejected: it would hide
  deterministic output from tests and change the existing capture seam.
- Expose a public renderer API. Rejected: applications only need the existing
  completion flags; a new API would expand 1.x surface area without a user
  need.

## Consequences

- Full Bash and PowerShell renderings can be characterized without a process
  redirect, while the existing public flags and output routing remain intact.
- Linux CI syntax-checks a generated Bash script and Windows CI parses a
  generated PowerShell script as required steps.
- Future application-core changes must establish an equally cohesive internal
  boundary before splitting the façade further.
