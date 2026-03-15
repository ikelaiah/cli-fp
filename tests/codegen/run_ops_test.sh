#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
TMP_DIR="$(mktemp -d)"
trap 'rm -rf "$TMP_DIR"' EXIT

GEN_SRC="$ROOT_DIR/tools/cli-fp-gen/cli_fp_gen.lpr"
GEN_BIN="$ROOT_DIR/tools/cli-fp-gen/cli_fp_gen"

fpc -Fu"$ROOT_DIR/tools/cli-fp-gen/src" "$GEN_SRC" >/dev/null

"$GEN_BIN" init "$TMP_DIR/demo" --force >/dev/null

dry_run_output="$("$GEN_BIN" add command repo --project "$TMP_DIR/demo" --description "Repo tools" --dry-run)"
printf '%s' "$dry_run_output" | grep -q 'Demo_Command_Repo.pas' || {
  echo "Dry-run add did not preview the new command stub"
  exit 1
}
if grep -q '"name" : "repo"' "$TMP_DIR/demo/clifp.json"; then
  echo "Dry-run add modified clifp.json"
  exit 1
fi
test ! -f "$TMP_DIR/demo/src/commands/Demo_Command_Repo.pas"

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

"$GEN_BIN" init "$TMP_DIR/descriptions" --force >/dev/null
"$GEN_BIN" add command repo --project "$TMP_DIR/descriptions" --description "Owner's tools" >/dev/null

python3 - <<'PY' "$TMP_DIR/descriptions/clifp.json"
import json, sys
p = sys.argv[1]
with open(p, "r", encoding="utf-8") as f:
    data = json.load(f)
for cmd in data["commands"]:
    if cmd["name"] == "repo":
        cmd["description"] = "Repo team's tools"
with open(p, "w", encoding="utf-8") as f:
    json.dump(data, f, indent=2)
    f.write("\n")
PY

"$GEN_BIN" generate --project "$TMP_DIR/descriptions" >/dev/null

fpc \
  -Fu"$ROOT_DIR/src" \
  -Fu"$TMP_DIR/descriptions/src" \
  -Fu"$TMP_DIR/descriptions/src/generated" \
  -Fu"$TMP_DIR/descriptions/src/commands" \
  "$TMP_DIR/descriptions/src/Demo.lpr" >/dev/null

"$TMP_DIR/descriptions/src/Demo" repo --help | grep -q "Repo team's tools" || {
  echo "Regenerated command description did not update runtime help"
  exit 1
}

"$GEN_BIN" init "$TMP_DIR/path-guard" --force >/dev/null

python3 - <<'PY' "$TMP_DIR/path-guard/clifp.json"
import json, sys
p = sys.argv[1]
with open(p, "r", encoding="utf-8") as f:
    data = json.load(f)
data["app"]["programFile"] = "../outside/Escape.lpr"
with open(p, "w", encoding="utf-8") as f:
    json.dump(data, f, indent=2)
    f.write("\n")
PY

if "$GEN_BIN" generate --project "$TMP_DIR/path-guard" >/dev/null 2>&1; then
  echo "Expected invalid programFile path to fail validation"
  exit 1
fi

if test -f "$TMP_DIR/outside/Escape.lpr" || test -f "$TMP_DIR/path-guard/../outside/Escape.lpr"; then
  echo "Generator wrote a program file outside the project directory"
  exit 1
fi

echo "Ops test passed"
