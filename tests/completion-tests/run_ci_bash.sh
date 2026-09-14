#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
TMP_DIR="$(mktemp -d)"
trap 'rm -rf "$TMP_DIR"' EXIT

mkdir -p "$TMP_DIR/units"

fpc \
  -B \
  -Fu"$ROOT_DIR/src" \
  -FE"$TMP_DIR" \
  -FU"$TMP_DIR/units" \
  "$ROOT_DIR/tests/completion-tests/completion_fixture.lpr" >/dev/null

"$TMP_DIR/completion_fixture" --completion-file > "$TMP_DIR/completion.bash"
bash -n "$TMP_DIR/completion.bash"

grep -Fqx '#!/bin/bash' "$TMP_DIR/completion.bash"
grep -Fqx 'declare -A tree' "$TMP_DIR/completion.bash"
grep -Fq 'complete -F _completion_fixture_completions' "$TMP_DIR/completion.bash"
