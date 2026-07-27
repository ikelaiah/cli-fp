# CLI Code Generator (Phase 1)

`cli-fp-gen` is a standalone scaffold generator for `cli-fp` applications.

Phase 1 focuses on CLI project generation only (no Lazarus wizard yet).

## Location

- Tool source: `tools/cli-fp-gen/`

## Commands

```text
cli-fp-gen init <target-dir> [--name <app-name>] [--version <x.y.z>] [--dry-run] [--force]
cli-fp-gen generate [--project <dir-or-spec-file>] [--dry-run] [--force]
cli-fp-gen add command <name> [--parent <cmd/path>] [--description <text>] [--project <dir-or-spec-file>] [--dry-run] [--force]
cli-fp-gen remove command <cmd/path> [--cascade] [--project <dir-or-spec-file>] [--dry-run] [--force]
```

## Project Spec

Generated projects use `clifp.json` as the source of truth.

Example:

```json
{
  "schemaVersion": 1,
  "app": {
    "name": "myapp",
    "version": "0.1.0",
    "programFile": "src/Myapp.lpr"
  },
  "rootCommand": {
    "description": "Run the default greeting",
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
  "commands": [
    {
      "name": "greet",
      "description": "Say hello",
      "parent": "",
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
    }
  ]
}
```

### Optional Root Command

`rootCommand` is optional in schema version 1. When present, the generated
application can execute without a named command:

```text
Myapp
Myapp --name Gus
```

The object accepts `description` and `parameters`; it deliberately has no
`name` or `parent`. Generation creates
`src/commands/<App>_RootCommand.pas` as a user-owned implementation stub and
wires it into the three-argument `CreateCLIApplication` overload. Named
commands in `commands` remain available alongside it.

Removing `rootCommand` restores the traditional command-first generated
application. The former root stub is retained because user-owned files are
never removed as stale generated output.

### Parameter Kinds

Supported `kind` values:

- `string`
- `integer`
- `float`
- `flag`
- `boolean`
- `path`
- `enum` (requires `allowedValues`)
- `datetime`
- `array`
- `password`
- `url`

## File Ownership

- `clifp.json`: project source of truth; `init` refuses to replace an existing
  spec unless `--force` is supplied
- `src/generated/*.pas`: generator-owned, overwritten on `generate`
- `src/generated/.clifp-manifest.json`: generator-owned manifest for cleanup
- `src/commands/*.pas`: user-owned command and optional root-command stubs,
  created once and not overwritten unless `--force`
- `src/*.lpr`: generator-owned in Phase 1

### Cleanup Safety

The manifest is used only to remove stale generator-owned files. Before
deleting a manifest entry, `cli-fp-gen` verifies that its normalized path is
inside the project directory and that no child path component is a symbolic
link or Windows reparse point (including directory junctions). If either check
fails, generation stops and reports the unsafe path.

This protection is deliberately conservative: a stale generated file reached
through a link is not deleted, even when that link points to another location
inside the project. Edit or remove the unexpected manifest entry or link, then
run `generate` again.

`--force` allows overwrite operations that normally protect existing files,
including replacement of an existing spec during `init` and regeneration of
user-owned command stubs. It does not bypass manifest path safety checks.

## Generated Layout

```text
<project>/
  clifp.json
  src/
    <App>.lpr
    commands/
      <App>_RootCommand.pas       # only when rootCommand is configured
      <App>_Command_*.pas
    generated/
      <App>_CommandRegistry_Generated.pas
      .clifp-manifest.json
```

## Build Generated App (example)

From the generated project directory, compile with the framework source path plus local generated/unit paths.

### Linux/macOS (Bash)

```bash
fpc -Fu../../src -Fu./src -Fu./src/generated -Fu./src/commands ./src/Myapp.lpr
```

### Windows (PowerShell)

```powershell
fpc "-Fu..\..\src" "-Fu.\src" "-Fu.\src\generated" "-Fu.\src\commands" .\src\Myapp.lpr
```

Adjust the first `-Fu` path (`../../src` or `..\..\src`) to point at the
`cli-fp` framework `src/` directory.

## Verification

### Linux/macOS (Bash)

The repository includes focused codegen checks under `tests/codegen/`:

- `run_unit_tests.sh`
- `run_golden_test.sh`
- `run_compile_smoke.sh`
- `run_ops_test.sh`

### Windows (PowerShell)

Use the Windows-native verification script from the repository root:

```powershell
powershell -ExecutionPolicy Bypass -File .\tests\codegen\run_all_tests.ps1
```

This script compiles `cli-fp-gen`, runs the focused unit tests,
verifies golden output, compiles a generated app, and checks `init` / `generate`
/ `add command` / `remove command` behavior plus overwrite and path validation
guards.

GitHub Actions runs the focused suite on Linux and Windows for pushes and pull
requests that change the generator, its fixtures, the framework source, or the
workflow. The workflow can also be started manually.

The operations tests include manifest cleanup escape attempts through a Unix
symbolic link and a Windows directory junction. They assert that the generator
fails safely and leaves the external file untouched.

## Maintainer Guide

The generator is split into small units with one main responsibility:

- `CliFpGen.App`: command-line parsing and command dispatch
- `CliFpGen.Generate`: project operations and generation workflow
- `CliFpGen.Model`: in-memory project and parameter types
- `CliFpGen.SpecIO`: `clifp.json` loading and saving
- `CliFpGen.Validate`: semantic and path validation
- `CliFpGen.Naming`: Pascal identifiers, unit names, and command paths
- `CliFpGen.Renderer`: Pascal source rendering
- `CliFpGen.Filesystem`: managed writes, deletions, and dry-run behavior
- `CliFpGen.Manifest`: generated-file tracking and safe stale-file cleanup

`TProjectSpec` owns its root-command specification and named commands. The
root-command specification and each `TCommandSpec` own their parameters.
When parsing JSON, construct an object completely before transferring it to
its owning list. If parsing raises an exception before that transfer, free the
partially constructed object in the same routine.

### Adding a Parameter Kind

Use this checklist when the framework gains a new parameter type:

1. Add the enum value and both text mappings in `CliFpGen.Model`.
2. Add any kind-specific defaults or semantic rules in `CliFpGen.Validate`.
3. Render the matching framework registration call in
   `CliFpGen.Renderer.RenderParameterCall`.
4. If the kind needs new JSON fields, add them symmetrically to load and save
   in `CliFpGen.SpecIO`; update the project-spec example above.
5. Add the kind to `tests/codegen-fixtures/golden-basic/clifp.json` and update
   the expected registry in `tests/codegen-golden/golden-basic/`.
6. Add focused validation or parsing tests when the kind has unique rules.
7. Update the supported-kind lists here and in the root README.
8. Run all Linux scripts under `tests/codegen/` and the Windows
   `run_all_tests.ps1` script. The compile smoke test confirms that the
   generated call still matches the current framework units in `src/`.

## Notes

- Commands are defined in a flat list with `parent` paths (slash-delimited, e.g. `repo/remote`).
- `app.programFile` must stay project-relative under `src/` and point to an `.lpr` file.
- `remove command` deletes command entries from `clifp.json`; use `--cascade` to remove a command subtree.
- Default command stubs automatically show help when they have subcommands at runtime.
- This avoids stale stub behavior when a command later becomes a command group.
- Parameter registrations and command descriptions are generated in the registry unit (not user stubs), so editing `clifp.json` and re-running `generate` updates metadata without overwriting user code.
