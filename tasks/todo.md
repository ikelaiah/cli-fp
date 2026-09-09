# v1.4.1 documentation accuracy tasks

## Task 1: Audit and classify published Pascal snippets

**Acceptance criteria:** Every Pascal block in a published page is classified
as a complete program, complete command pattern, or explicitly scoped fragment;
all identifiers have declarations or immediate context.

**Verification:** Search all published Markdown files and manually inspect each
Pascal block against the source API.

**Dependencies:** None

## Task 2: Establish the class-based command model

**Acceptance criteria:** Getting Started, Commands, and How-To explicitly show
`TBaseCommand` descendants, command instances, option registration, application
registration, and `Execute`.

**Verification:** Apply the supplied reader test to How-To and compile the
canonical QuickStart example.

**Dependencies:** Task 1

## Task 3: Correct recipe and reference fragments

**Acceptance criteria:** Options, value retrieval, exit codes, terminal output,
spinner/progress, debug casting, API reference, and technical excerpts state
their receiver, units, and execution scope accurately.

**Verification:** Compare the touched APIs with `src/`; run DocKit checks.

**Dependencies:** Tasks 1-2

## Task 4: Release records and qualification

**Acceptance criteria:** Version metadata, changelog, roadmap, project page,
and v1.4.1 release notes truthfully describe a documentation-only patch; local
documentation/framework/generator/example checks pass.

**Verification:** Repository test scripts, DocKit strict audit/build, and diff
whitespace check.

**Dependencies:** Task 3

## Task 5: Review, publish, and verify release

**Acceptance criteria:** The candidate is reviewed, approved by checks, merged,
tagged, released, and its rendered docs demonstrate the repaired context.

**Verification:** GitHub PR/checks/release state and route-specific Pages text.

**Dependencies:** Task 4
