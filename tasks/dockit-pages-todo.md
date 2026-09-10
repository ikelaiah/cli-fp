# cli-fp DocKit Pages checklist

- [x] Add `docs/dockit.json` with cli-fp identity and built-in theme settings.
- [x] Add explicit `docs/layout.json` with root README home and curated pages.
- [x] Repair current documentation links and anchors found by DocKit audit.
- [x] Add the Docs badge/link to the root README after deployment config exists.
- [x] Add the DocKit-managed Pages workflow pinned to `v1.0.0`.
- [x] Run `dockit-fp check`.
- [x] Run `dockit-fp audit --strict`.
- [x] Build and preview the site locally; inspect generated assets and routes.
- [x] Run the existing cli-fp Windows test/build checks.
- [ ] Commit, push, open PR, pass CI, merge, and verify Pages/live site.

## Deliberately unlisted unless a later task promotes them

- `docs/PULL_REQUEST_v*.md`
- `docs/RELEASE_NOTES_v*.md`
- `docs/completion-testing/*` test-result and work-summary artefacts
- `docs/test-output.md`
- root `CHANGELOG.md`, `ROADMAP.md`, `CONTRIBUTING.md`, and `LICENSE`
- source, example, and test directories that are linked as repository code but
  are not Markdown documentation pages

## Historical/versioned migration (v1.4.3)

- [ ] Add `docs/versions.json` from verified immutable release sources.
- [ ] Replace the single-version Pages caller with DocKit v1.0.0's
  tag-triggered historical caller.
- [ ] Record why the pre-existing v1.4.2 tag cannot be retrofitted as current.
- [ ] Run `dockit-fp check`, `check-release`, and `build-all` against a
  local-only candidate tag.
- [ ] Merge the qualified documentation-maintenance release and create an
  annotated v1.4.3 tag without changing earlier tags.
- [ ] Verify the deployed selector and release-specific historical pages.
