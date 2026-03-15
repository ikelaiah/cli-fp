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
    "programFile": "src\\Myapp.lpr"
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

- `src/generated/*.pas`: generator-owned, overwritten on `generate`
- `src/generated/.clifp-manifest.json`: generator-owned manifest for cleanup
- `src/commands/*.pas`: user-owned command stubs, created once and not overwritten unless `--force`
- `src/*.lpr`: generator-owned in Phase 1

## Generated Layout

```text
<project>/
  clifp.json
  src/
    <App>.lpr
    commands/
      <App>_Command_*.pas
    generated/
      <App>_CommandRegistry_Generated.pas
      .clifp-manifest.json
```

## Build Generated App (example)

From the generated project directory, compile with the framework source path plus local generated/unit paths.

### Windows (PowerShell)

```powershell
fpc "-Fu..\..\src" "-Fu.\src" "-Fu.\src\generated" "-Fu.\src\commands" .\src\MyApp.lpr
```

Adjust the `..\..\src` path to point at the `cli-fp` framework `src/` directory.

## Notes

- Commands are defined in a flat list with `parent` paths (slash-delimited, e.g. `repo/remote`).
- `app.programFile` must stay project-relative under `src/` and point to an `.lpr` file.
- `remove command` deletes command entries from `clifp.json`; use `--cascade` to remove a command subtree.
- Default command stubs automatically show help when they have subcommands at runtime.
- This avoids stale stub behavior when a command later becomes a command group.
- Parameter registrations and command descriptions are generated in the registry unit (not user stubs), so editing `clifp.json` and re-running `generate` updates metadata without overwriting user code.
