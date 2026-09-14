# cli-fp project generator

[How do I...?](how-to.md) · [Commands](commands.md) ·
[Generator internals](technical-docs.md#generator-maintenance)

`cli-fp-gen` creates a Free Pascal project with a command registry and
user-owned command units. Use it when a command tree is large enough that a
scaffold and a JSON command specification are useful. For a single program,
start with [your first cli-fp program](getting-started.md) instead.

## Start a project

From the repository root, build the generator and initialize a project:

```bash
fpc -Futools/cli-fp-gen/src tools/cli-fp-gen/cli_fp_gen.lpr
./tools/cli-fp-gen/cli_fp_gen init ./build-temp/myapp --name myapp
cd build-temp/myapp
fpc -Fu../../src -Fu./src -Fu./src/generated -Fu./src/commands ./src/Myapp.lpr
./src/Myapp greet --help
```

On PowerShell:

```powershell
fpc "-Futools\cli-fp-gen\src" .\tools\cli-fp-gen\cli_fp_gen.lpr
.\tools\cli-fp-gen\cli_fp_gen.exe init .\build-temp\myapp --name myapp
Set-Location .\build-temp\myapp
fpc "-Fu..\..\src" "-Fu.\src" "-Fu.\src\generated" "-Fu.\src\commands" .\src\Myapp.lpr
.\src\Myapp.exe greet --help
```

Adjust the first `-Fu` path when the generated project is not two directories
below the `cli-fp` repository.
Start in a fresh target directory. The build creates `src/Myapp` (Windows:
`src/Myapp.exe`); the final command shows generated `greet` help and exits 0.

## A minimal root-command project

`clifp.json` is the source of truth. This is a complete small project with a
default action and one option. Replace the initialized project's `clifp.json`
with this object, then, from `build-temp/myapp`, run
`../../tools/cli-fp-gen/cli_fp_gen generate --project .` (PowerShell:
`..\..\tools\cli-fp-gen\cli_fp_gen.exe generate --project .`).
Recompile with the same unit paths as above but source `src/Hello.lpr`.
The stub prints a TODO message until you implement its root command; the
specification supplies metadata and registration, not greeting logic.

```json
{
  "schemaVersion": 1,
  "app": {
    "name": "hello",
    "version": "1.0.0",
    "programFile": "src/Hello.lpr"
  },
  "rootCommand": {
    "description": "Print a greeting",
    "parameters": [
      {
        "kind": "string",
        "short": "-n",
        "long": "--name",
        "description": "Name to greet",
        "required": false,
        "default": "World",
        "allowedValues": ""
      }
    ]
  },
  "commands": []
}
```

## Everyday commands

```text
cli-fp-gen init <target-dir> [--name <app-name>] [--version <x.y.z>] [--dry-run] [--force]
cli-fp-gen generate [--project <dir-or-spec-file>] [--dry-run] [--force]
cli-fp-gen add command <name> [--parent <cmd/path>] [--description <text>] [--project <dir-or-spec-file>] [--dry-run] [--force]
cli-fp-gen remove command <cmd/path> [--cascade] [--project <dir-or-spec-file>] [--dry-run] [--force]
```

Use `init` once. Use `add command` or edit `clifp.json`, then run `generate`.
The generator tool's own `init --version <value>` sets application metadata;
it is distinct from a generated application's reserved version request.
In generated specifications, `-v` and `--version` (including case variants)
cannot be registered as user options at any scope. Use `-d`/`--verbose`.
Use `remove command` for a command entry; add `--cascade` when its nested
commands should be removed too. Try `--dry-run` before a substantial change.

## Know which files you own

| Path | Ownership | What to do |
| --- | --- | --- |
| `clifp.json` | You | Change command metadata here. |
| `src/commands/*.pas` | You | Implement `Execute` here. |
| `src/generated/*.pas` | Generator | Do not hand-edit; regenerate it. |
| `src/generated/.clifp-manifest.json` | Generator | Do not hand-edit. |
| `src/<App>.lpr` | Generator | Regenerate instead of editing generated wiring. |

The generator creates command stubs once and preserves them on normal
regeneration. Generated registry/program files are rewritten from
`clifp.json`.

Generator validation rejects malformed command and option tokens, including
bare `-`/`--`, whitespace, dash-prefixed command names, and duplicate flags.
`--description` consumes one command-line argument, so quote descriptions that
contain spaces. Corrupt project JSON or generated manifests fail with an
explicit error; they are never silently treated as empty configuration.

Command descriptions are escaped as Pascal string expressions during
generation, so apostrophes and control characters cannot produce invalid
source. The manifest is not proof that a path is generator-owned. Stale cleanup
only removes artifacts under src/generated/ or a marker-bearing generated .lpr
program under src/; it refuses clifp.json, command stubs, README files, .git
content, arbitrary source, malformed manifest entries, path escapes, and
symbolic-link/reparse traversal. New specs and manifests serialize
project-relative paths with /; existing specs using backslashes remain readable.

## Project specification

`clifp.json` is the source of truth. Commands are a flat list; use a slash
path such as `repo/remote` in `parent` to make a nested command. A
`rootCommand` object is optional and creates the default action for
`myapp [options]`.

### Fields

| Location | Field | Meaning |
| --- | --- | --- |
| root | `schemaVersion` | Required schema version; current generator supports `1`. |
| `app` | `name`, `version` | Application name and display version. `version` defaults to `0.1.0` when omitted. |
| `app` | `programFile` | Required project-relative `.lpr` path under `src/`; use `/` in new specs. |
| `rootCommand` | `description`, `parameters` | Optional default action for `myapp [options]`. |
| `commands[]` | `name`, `parent`, `description`, `parameters` | Named commands. `parent` is empty for a top-level command or a slash path such as `repo`. |
| `parameters[]` | `kind`, `short`, `long`, `description`, `required`, `default`, `allowedValues` | An option definition. At least one of `short` or `long` is required; flags are case-insensitive and unique within a command. |

Use `short` such as `-d`, `long` such as `--verbose`, or both. Long flags must
start with `--`; short flags are exactly one printable character after `-`.
Supported `kind` values are `string`, `integer`, `float`, `flag`, `boolean`,
`path`, `enum`, `datetime`, `array`, `password`, and `url`.

`default` is optional. For `enum`, `allowedValues` is a required `|`-separated
list and a non-empty default must match one listed value case-insensitively,
using the same quoting and comparison behavior as the runtime. For example,
`"normal mode"|fast mode` accepts `NORMAL MODE` as a default.

Supported parameter kinds are `string`, `integer`, `float`, `flag`, `boolean`,
`path`, `enum`, `datetime`, `array`, `password`, and `url`.

## Safe regeneration

`init` refuses to replace an existing spec unless given `--force`.
`generate` protects user-owned stubs unless forced. Its generated-file manifest
is constrained to the project directory and refuses cleanup through symbolic
links or Windows reparse points; `--force` does not bypass that path-safety
check.

`--force` changes only overwrite policy: it permits replacing an existing
`clifp.json` during `init` and user-owned command stubs during generation. It
does not relax validation or filesystem-safety checks. `--dry-run` prints every
directory, create, overwrite, skip, and deletion it would perform without
writing the spec or any generated file.

`remove command repo` removes that command from `clifp.json`; it refuses to
remove a parent with children unless `--cascade` is also supplied. Regenerate
after manual specification edits. Removing a command does not delete its
user-owned stub automatically: it may contain work you want to keep, and a
later regeneration creates a new stub only when the command is present again.

For the internal unit map, extension checklist, and generator test details,
read [Generator maintenance](technical-docs.md#generator-maintenance).
