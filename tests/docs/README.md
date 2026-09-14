# Current documentation recipes

Run `node tests/docs/verify-recipes.cjs` from the repository with Node.js 18+
and FPC on PATH. The same command is required on Linux and Windows CI.

The runner extracts current Markdown Pascal blocks, supplies the units/classes
explicitly required for fragments, compiles them in temporary directories, and
checks stdout and exit codes. It also executes documented platform build/run
commands in isolated source copies. Clone/cd preambles are fulfilled by those
local copies; the checks do not clone the network repository for each example.

Coverage includes the complete beginner program, combined How-To option
recipes, conversion/default/error cases, command shapes, option tables,
terminal methods, and example/generator build instructions. Local relative
links are checked. Historical documentation is not rewritten or executed.

Shell completion syntax/parser contracts remain in `tests/completion-tests`;
the framework suite covers parser/lookup behaviour directly. DocKit build and
rendered-site inspection are separate release checks, not implied by these
recipe tests.
