# cli-fp-gen

A standalone scaffold generator for [cli-fp](../../README.md) applications.

> **You must compile this tool before use.** It is a Free Pascal project and no pre-built binary is distributed.

## Requirements

- [Free Pascal Compiler (FPC) 3.2.2+](https://www.freepascal.org/)

## Build

From the repository root:

```bash
fpc -Ftools/cli-fp-gen/src tools/cli-fp-gen/cli_fp_gen.lpr
```

Or from this directory:

```bash
fpc -Fsrc cli_fp_gen.lpr
```

The output binary (`cli_fp_gen` / `cli_fp_gen.exe`) will be placed in this directory.

## Usage

```text
cli-fp-gen init <target-dir> [--name <app-name>] [--version <x.y.z>] [--dry-run] [--force]
cli-fp-gen generate [--project <dir-or-spec-file>] [--dry-run] [--force]
cli-fp-gen add command <name> [--parent <cmd/path>] [--description <text>] [--project <dir-or-spec-file>] [--dry-run] [--force]
cli-fp-gen remove command <cmd/path> [--cascade] [--project <dir-or-spec-file>] [--dry-run] [--force]
```

## Full Documentation

See [docs/codegen.md](../../docs/codegen.md) for the full reference, including:

- Project spec format (`clifp.json`)
- Supported parameter kinds
- File ownership rules
- Generated project layout
- How to build a generated app
- Verification / test scripts
