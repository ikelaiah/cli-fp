# cli-fp v1.6.2 — Documentation publishing on DocSprout

`v1.6.2` is a documentation-infrastructure maintenance release.

## DocSprout migration

- GitHub Pages publishing moved from DocKit-FP v1.0.0 to DocSprout v1.1.1,
  the rebranded continuation of the same documentation-site builder.
- The site configuration is now `docs/docsprout.json` (same schema and
  routes), and the historical release-tag Pages workflow is
  `.github/workflows/docsprout-pages.yml`, pinned to
  `ikelaiah/docsprout/.github/workflows/publish-docs.yml@v1.1.1`.
- Published pages that referenced intentionally unlisted archive and ADR
  documents now link to their GitHub sources, so `docsprout audit --strict`
  reports no findings.

Readers keep the versioned documentation selector and every previously
published release. No cli-fp runtime, parser, completion, generator, or public
API behaviour changed in this release.
