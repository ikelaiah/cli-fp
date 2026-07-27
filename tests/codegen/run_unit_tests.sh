#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
TMP_DIR="$(mktemp -d)"
trap 'rm -rf "$TMP_DIR"' EXIT

mkdir -p "$TMP_DIR/units"

fpc \
  -Fu"$ROOT_DIR/tools/cli-fp-gen/src" \
  -Fu"$ROOT_DIR/tests/codegen" \
  -FE"$TMP_DIR" \
  -FU"$TMP_DIR/units" \
  "$ROOT_DIR/tests/codegen/codegen_test_runner.lpr" >/dev/null

"$TMP_DIR/codegen_test_runner" --all --format=plain
