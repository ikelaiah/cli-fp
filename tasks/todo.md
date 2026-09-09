# v1.4.0 documentation release tasks

## Task 1: Audit and release plan

**Description:** Capture the documentation problems found and move the planned
release sequence to documentation-first v1.4.0.

**Acceptance criteria:**
- [ ] Roadmap assigns documentation/developer learning to v1.4.0.
- [ ] Runtime simplification work moves to v1.5.0 and application boundaries to v1.6.0.
- [ ] Release records state that no runtime API is added.

**Verification:** Read all changed release records and validate Markdown links.

**Dependencies:** None

**Files likely touched:** `ROADMAP.md`, `CHANGELOG.md`, `docs/project.md`

**Estimated scope:** Small: 1-2 files
## Task 2: Focus the learning path

**Description:** Replace the monolithic manual experience with focused pages,
preserving the manual as a compatibility landing page.

**Acceptance criteria:**
- [ ] Newcomers have a first-CLI path.
- [ ] Commands, options, terminal UX, completion, and limitations have clear homes.
- [ ] Existing manual URLs continue to land on useful documentation.

**Verification:** Validate internal links and compile the homepage example through the existing smoke suite.

**Dependencies:** Task 1

**Files likely touched:** `docs/*.md`, `docs/layout.json`

**Estimated scope:** Medium: 3-5 files

## Task 3: Add practical recipes

**Description:** Create a compact How-To guide answering common application-author questions with minimal, supported examples.

**Acceptance criteria:**
- [ ] Recipes cover command shape, parameters, values, output, completion, generator, debugging, and unsupported work.
- [ ] Every API call matches the public source interface.
- [ ] Deeper links avoid restating full tutorials.

**Verification:** Source/API comparison and Markdown link validation.

**Dependencies:** Task 2

**Files likely touched:** `docs/how-to.md`, `docs/commands.md`, `docs/options.md`

**Estimated scope:** Medium: 3-5 files

## Task 4: Publish-oriented refresh

**Description:** Update the homepage, navigation, examples, generator guide, and reference links for the new information architecture.

**Acceptance criteria:**
- [ ] README stays concise and points to task-oriented documentation.
- [ ] Navigation is intent-oriented with no duplicate entries.
- [ ] Generator internals are separated from normal application guidance.

**Verification:** DocKit check/build if available; otherwise JSON, link, and Markdown validation.

**Dependencies:** Tasks 2-3

**Files likely touched:** `README.md`, `docs/layout.json`, `docs/examples.md`, `docs/codegen.md`, `docs/api-reference.md`

**Estimated scope:** Medium: 3-5 files

## Task 5: Qualify and release

**Description:** Run existing project checks, review the candidate, and perform authorized remote release operations if possible.

**Acceptance criteria:**
- [ ] Framework, generator, cleanup, documentation, and diff checks are recorded.
- [ ] The candidate is committed and reviewed.
- [ ] PR, CI, merge, tag, release, and Pages verification are completed or precisely reported as blocked.

**Verification:** Repository scripts, DocKit tooling, GitHub CLI/API where authenticated.

**Dependencies:** Tasks 1-4

**Files likely touched:** release metadata and documentation only

**Estimated scope:** Small: 1-2 files
