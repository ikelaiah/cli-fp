#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
TMP_DIR="$(mktemp -d)"
trap 'rm -rf "$TMP_DIR"' EXIT

GEN_SRC="$ROOT_DIR/tools/cli-fp-gen/cli_fp_gen.lpr"
GEN_BIN="$ROOT_DIR/tools/cli-fp-gen/cli_fp_gen"
FIXTURE_DIR="$ROOT_DIR/tests/codegen-fixtures/golden-basic"

fpc -Fu"$ROOT_DIR/tools/cli-fp-gen/src" "$GEN_SRC" >/dev/null

mkdir -p "$TMP_DIR/project"
cp "$FIXTURE_DIR/clifp.json" "$TMP_DIR/project/clifp.json"
"$GEN_BIN" generate --project "$TMP_DIR/project" >/dev/null

fpc \
  -Fu"$ROOT_DIR/src" \
  -Fu"$TMP_DIR/project/src" \
  -Fu"$TMP_DIR/project/src/generated" \
  -Fu"$TMP_DIR/project/src/commands" \
  "$TMP_DIR/project/src/GoldenDemo.lpr" >/dev/null

"$TMP_DIR/project/src/GoldenDemo" --help >/dev/null
"$TMP_DIR/project/src/GoldenDemo" repo >/dev/null

echo "Compile smoke test passed"
