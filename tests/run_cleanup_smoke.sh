#!/usr/bin/env bash
set -euo pipefail

SOURCE_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
TMP_DIR="$(mktemp -d)"
WORK_ROOT="$TMP_DIR/cli-fp"

cleanup() {
  rm -rf "$TMP_DIR"
}
trap cleanup EXIT

git clone --quiet --no-hardlinks "$SOURCE_ROOT" "$WORK_ROOT"

EXAMPLES=(
  ColorDemo
  ErrorHandlingDemo
  LongRunningOpDemo
  ProgressDemo
  RootCommandDemo
  SimpleDemo
  SubCommandDemo
)
SENTINELS=(
  example-bin/.gitkeep
  example-bin/README.md
  example-bin/simpledemo_completion.bash
  example-bin/simpledemo_completion.ps1
  example-bin/subcommanddemo_completion.bash
  example-bin/subcommanddemo_completion.ps1
)

for sentinel in "${SENTINELS[@]}"; do
  test -f "$WORK_ROOT/$sentinel"
done

mkdir -p "$TMP_DIR/units"
for example in "${EXAMPLES[@]}"; do
  fpc \
    -Fu"$WORK_ROOT/src" \
    -FE"$WORK_ROOT/example-bin" \
    -FU"$TMP_DIR/units/$example" \
    "$WORK_ROOT/examples/$example/$example.lpr" >/dev/null

  test -f "$WORK_ROOT/example-bin/$example" ||
    test -f "$WORK_ROOT/example-bin/$example.exe"
done

(cd "$WORK_ROOT" && ./clean-all-examples.sh >/dev/null)

for example in "${EXAMPLES[@]}"; do
  test ! -e "$WORK_ROOT/example-bin/$example"
  test ! -e "$WORK_ROOT/example-bin/$example.exe"
done

for sentinel in "${SENTINELS[@]}"; do
  test -f "$WORK_ROOT/$sentinel"
  git -C "$WORK_ROOT" diff --quiet -- "$sentinel"
done

git -C "$WORK_ROOT" diff --quiet

echo "Example cleanup smoke check passed."
