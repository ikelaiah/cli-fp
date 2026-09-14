#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
TMP_DIR="$(mktemp -d)"
trap 'rm -rf "$TMP_DIR"' EXIT

GEN_SRC="$ROOT_DIR/tools/cli-fp-gen/cli_fp_gen.lpr"
GEN_BIN="$TMP_DIR/cli_fp_gen"
FIXTURE_DIR="$ROOT_DIR/tests/codegen-fixtures/golden-basic"

mkdir -p "$TMP_DIR/gen-units"
fpc \
  -Fu"$ROOT_DIR/tools/cli-fp-gen/src" \
  -FE"$TMP_DIR" \
  -FU"$TMP_DIR/gen-units" \
  "$GEN_SRC" >/dev/null

mkdir -p "$TMP_DIR/project"
cp "$FIXTURE_DIR/clifp.json" "$TMP_DIR/project/clifp.json"
"$GEN_BIN" generate --project "$TMP_DIR/project" >/dev/null

mkdir -p "$TMP_DIR/project/build/units"
fpc \
  -Fu"$ROOT_DIR/src" \
  -Fu"$TMP_DIR/project/src" \
  -Fu"$TMP_DIR/project/src/generated" \
  -Fu"$TMP_DIR/project/src/commands" \
  -FE"$TMP_DIR/project/build" \
  -FU"$TMP_DIR/project/build/units" \
  "$TMP_DIR/project/src/GoldenDemo.lpr" >/dev/null

"$TMP_DIR/project/build/GoldenDemo" --help >/dev/null
"$TMP_DIR/project/build/GoldenDemo" --root-name Gus | grep -q \
  'TODO: Implement the root command'
"$TMP_DIR/project/build/GoldenDemo" repo >/dev/null

if find "$ROOT_DIR/src" -maxdepth 1 -type f \
  \( -name '*.o' -o -name '*.ppu' -o -name '*.or' -o -name '*.a' \) \
  -print -quit | grep -q .; then
  echo "Compiler artifacts escaped the test temporary directories"
  exit 1
fi

echo "Compile smoke test passed"
