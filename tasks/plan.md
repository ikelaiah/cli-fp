# Implementation Plan: cli-fp v1.4.0 documentation release

## Overview

Turn the published documentation into a task-oriented learning path while
keeping the current v1.3.3 runtime API intact. The release uses the existing
canonical-example cleanup smoke test as the executable-documentation guard.

## Architecture Decisions

- Keep `docs/user-manual.md` as a compact compatibility landing page rather
  than breaking existing links; move reader-facing material into focused pages.
- Add one concise task guide (`docs/how-to.md`) and link to deeper learning and
  reference pages instead of duplicating explanations.
- Keep generator safety and implementation details in technical documentation;
  keep application-author guidance in `docs/codegen.md`.
- Publish only current reader-facing pages through DocKit navigation; historical
  release and completion test records remain unlisted.
- Do not change the runtime API. Existing cleanup-smoke CI compiles every
  canonical example and protects the beginner examples from documentation drift.

## Task List

### Phase 1: Release foundation

- [ ] Task 1: Record the documentation audit and update release planning.
- [ ] Task 2: Establish focused learning pages and compatibility links.

### Checkpoint: Learning path

- [ ] Documentation links resolve locally.
- [ ] Root, named, and nested command shapes have distinct entry points.

### Phase 2: Task-oriented documentation

- [ ] Task 3: Add concise How-To recipes grounded in tested APIs.
- [ ] Task 4: Add limitations, terminal, and completion guidance.
- [ ] Task 5: Refresh README, examples, generator, and API cross-links.

### Checkpoint: Published documentation

- [ ] DocKit navigation contains each page once and excludes historical records.
- [ ] Release notes, roadmap, and project material agree on v1.4.0.

### Phase 3: Qualification

- [ ] Task 6: Run documentation checks, framework/generator checks, and example smoke checks.
- [ ] Task 7: Review the final diff, commit, push, and create the requested PR/release if credentials permit.

### Checkpoint: Complete

- [ ] All release acceptance criteria that can be verified locally pass.
- [ ] Working tree is clean after qualification.

## Risks and Mitigations

| Risk | Impact | Mitigation |
| --- | --- | --- |
| Documentation claims drift from runtime behavior | High | Verify all recipe APIs against `src/` and existing FPCUnit tests. |
| Breaking inbound manual links | Medium | Retain `user-manual.md` as a short migration/learning index. |
| DocKit tooling unavailable locally | Medium | Run its configured commands if installed; otherwise validate JSON, Markdown links, and CI configuration locally and report the limitation. |
| Remote permissions unavailable | Medium | Complete and qualify the candidate locally, then report exact remote steps blocked. |
