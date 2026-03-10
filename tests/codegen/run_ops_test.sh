#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
TMP_DIR="$(mktemp -d)"
trap 'rm -rf "$TMP_DIR"' EXIT

GEN_SRC="$ROOT_DIR/tools/cli-fp-gen/cli_fp_gen.lpr"
GEN_BIN="$ROOT_DIR/tools/cli-fp-gen/cli_fp_gen"

fpc -Fu"$ROOT_DIR/tools/cli-fp-gen/src" "$GEN_SRC" >/dev/null

"$GEN_BIN" init "$TMP_DIR/demo" --force >/dev/null
"$GEN_BIN" add command repo --project "$TMP_DIR/demo" --description "Repo tools" >/dev/null
"$GEN_BIN" add command clone --parent repo --project "$TMP_DIR/demo" --description "Clone repo" >/dev/null

if "$GEN_BIN" remove command repo --project "$TMP_DIR/demo" >/dev/null 2>&1; then
  echo "Expected remove command without --cascade to fail"
  exit 1
fi

"$GEN_BIN" remove command repo --cascade --project "$TMP_DIR/demo" >/dev/null

grep -q '"name" : "repo"' "$TMP_DIR/demo/clifp.json" && {
  echo "repo command still present after cascade remove"
  exit 1
}

old_program="$TMP_DIR/demo/src/Demo.lpr"
test -f "$old_program"

python3 - <<'PY' "$TMP_DIR/demo/clifp.json"
import json, sys
p = sys.argv[1]
with open(p, "r", encoding="utf-8") as f:
    data = json.load(f)
data["app"]["programFile"] = "src/DemoRenamed.lpr"
with open(p, "w", encoding="utf-8") as f:
    json.dump(data, f, indent=2)
    f.write("\n")
PY

"$GEN_BIN" generate --project "$TMP_DIR/demo" >/dev/null

test -f "$TMP_DIR/demo/src/DemoRenamed.lpr"
if test -f "$old_program"; then
  echo "Old generated program file was not removed by manifest cleanup"
  exit 1
fi

grep -q 'src/DemoRenamed.lpr' "$TMP_DIR/demo/src/generated/.clifp-manifest.json"

echo "Ops test passed"
