#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
TMP_DIR="$(mktemp -d)"
trap 'rm -rf "$TMP_DIR"' EXIT

mkdir -p "$TMP_DIR/units"

fpc \
  -dCLI_FP_TESTING \
  -Fu"$ROOT_DIR/src" \
  -Fu"$ROOT_DIR/tests" \
  -FE"$TMP_DIR" \
  -FU"$TMP_DIR/units" \
  "$ROOT_DIR/tests/TestRunner.lpr" >/dev/null

"$TMP_DIR/TestRunner" --all --format=plain
