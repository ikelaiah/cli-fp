#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
TMP_DIR="$(mktemp -d)"
trap 'rm -rf "$TMP_DIR"' EXIT

GEN_SRC="$ROOT_DIR/tools/cli-fp-gen/cli_fp_gen.lpr"
GEN_BIN="$TMP_DIR/cli_fp_gen"

mkdir -p "$TMP_DIR/gen-units"
fpc \
  -Fu"$ROOT_DIR/tools/cli-fp-gen/src" \
  -FE"$TMP_DIR" \
  -FU"$TMP_DIR/gen-units" \
  "$GEN_SRC" >/dev/null

"$GEN_BIN" init "$TMP_DIR/demo" >/dev/null

spec_before_reinit="$(cat "$TMP_DIR/demo/clifp.json")"
if "$GEN_BIN" init "$TMP_DIR/demo" >/dev/null 2>&1; then
  echo "Expected init without --force to protect the existing project spec"
  exit 1
fi
test "$(cat "$TMP_DIR/demo/clifp.json")" = "$spec_before_reinit" || {
  echo "Init without --force modified the existing project spec"
  exit 1
}

if "$GEN_BIN" add command 'repo/clone' --project "$TMP_DIR/demo" >/dev/null 2>&1; then
  echo "Expected a command name containing a path separator to fail"
  exit 1
fi
test "$(cat "$TMP_DIR/demo/clifp.json")" = "$spec_before_reinit" || {
  echo "Invalid add command modified the project spec"
  exit 1
}

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

repo_stub="$TMP_DIR/demo/src/commands/Demo_Command_Repo.pas"
printf '\n{ user customization }\n' >>"$repo_stub"
"$GEN_BIN" generate --project "$TMP_DIR/demo" >/dev/null
grep -q '{ user customization }' "$repo_stub" || {
  echo "Generate overwrote a user-owned command stub"
  exit 1
}

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

manifest_guard_project="$TMP_DIR/manifest-guard"
"$GEN_BIN" init "$manifest_guard_project" >/dev/null
mkdir -p "$TMP_DIR/manifest-outside"
printf 'protected\n' >"$TMP_DIR/manifest-outside/victim.txt"

python3 - <<'PY' "$manifest_guard_project/src/generated/.clifp-manifest.json"
import json, sys
p = sys.argv[1]
with open(p, "r", encoding="utf-8") as f:
    data = json.load(f)
data["generatedFiles"] = ["../manifest-outside/victim.txt"]
with open(p, "w", encoding="utf-8") as f:
    json.dump(data, f, indent=2)
    f.write("\n")
PY

if "$GEN_BIN" generate --project "$manifest_guard_project" >/dev/null 2>&1; then
  echo "Expected an out-of-project manifest entry to fail cleanup"
  exit 1
fi
test -f "$TMP_DIR/manifest-outside/victim.txt" || {
  echo "Manifest cleanup deleted a file outside the project directory"
  exit 1
}

# On case-sensitive filesystems, a sibling that differs only by case is still
# outside the project and must not pass the manifest cleanup prefix check.
case_project="$TMP_DIR/CaseProject"
case_sibling="$TMP_DIR/caseproject"
"$GEN_BIN" init "$case_project" >/dev/null
mkdir -p "$case_sibling"
if [[ ! "$case_project" -ef "$case_sibling" ]]; then
  printf 'protected\n' >"$case_sibling/victim.txt"
  python3 - <<'PY' "$case_project/src/generated/.clifp-manifest.json"
import json, sys
p = sys.argv[1]
with open(p, "r", encoding="utf-8") as f:
    data = json.load(f)
data["generatedFiles"] = ["../caseproject/victim.txt"]
with open(p, "w", encoding="utf-8") as f:
    json.dump(data, f, indent=2)
    f.write("\n")
PY

  if "$GEN_BIN" generate --project "$case_project" >/dev/null 2>&1; then
    echo "Expected a differently-cased sibling manifest entry to fail cleanup"
    exit 1
  fi
  test -f "$case_sibling/victim.txt" || {
    echo "Manifest cleanup deleted a file from a differently-cased sibling directory"
    exit 1
  }
fi

echo "Ops test passed"
