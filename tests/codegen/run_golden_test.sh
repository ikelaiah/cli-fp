#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
TMP_DIR="$(mktemp -d)"
trap 'rm -rf "$TMP_DIR"' EXIT

GEN_SRC="$ROOT_DIR/tools/cli-fp-gen/cli_fp_gen.lpr"
GEN_BIN="$ROOT_DIR/tools/cli-fp-gen/cli_fp_gen"
FIXTURE_DIR="$ROOT_DIR/tests/codegen-fixtures/golden-basic"
GOLDEN_DIR="$ROOT_DIR/tests/codegen-golden/golden-basic"

fpc -Fu"$ROOT_DIR/tools/cli-fp-gen/src" "$GEN_SRC" >/dev/null

mkdir -p "$TMP_DIR/project"
cp "$FIXTURE_DIR/clifp.json" "$TMP_DIR/project/clifp.json"
"$GEN_BIN" generate --project "$TMP_DIR/project" >/dev/null

compare_file() {
  local rel="$1"
  diff -u --strip-trailing-cr \
    "$GOLDEN_DIR/$rel" \
    "$TMP_DIR/project/$rel"
}

compare_file "src/GoldenDemo.lpr"
compare_file "src/generated/GoldenDemo_CommandRegistry_Generated.pas"
compare_file "src/generated/.clifp-manifest.json"
compare_file "src/commands/GoldenDemo_Command_Greet.pas"
compare_file "src/commands/GoldenDemo_Command_Repo.pas"
compare_file "src/commands/GoldenDemo_Command_RepoClone.pas"
compare_file "src/commands/GoldenDemo_Command_Types.pas"

echo "Golden test passed"
