#!/bin/bash
# clean-all-examples.sh
# Remove generated example build artifacts while preserving tracked files.

set -eu

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

EXAMPLES="ColorDemo ErrorHandlingDemo LongRunningOpDemo ProgressDemo RootCommandDemo SimpleDemo SubCommandDemo"

remove_generated_artifacts() {
  directory="$1"

  [ -d "$directory" ] || return

  find "$directory" -type f \( \
    -name '*.o' -o \
    -name '*.ppu' -o \
    -name '*.compiled' -o \
    -name '*.or' -o \
    -name '*.a' -o \
    -name '*.rst' -o \
    -name '*.res' -o \
    -name '*.dbg' -o \
    -name '*.tds' -o \
    -name '*.lps' -o \
    -name '*.lps.bak' -o \
    -name 'ColorDemo' -o -name 'ColorDemo.exe' -o \
    -name 'ErrorHandlingDemo' -o -name 'ErrorHandlingDemo.exe' -o \
    -name 'LongRunningOpDemo' -o -name 'LongRunningOpDemo.exe' -o \
    -name 'ProgressDemo' -o -name 'ProgressDemo.exe' -o \
    -name 'RootCommandDemo' -o -name 'RootCommandDemo.exe' -o \
    -name 'SimpleDemo' -o -name 'SimpleDemo.exe' -o \
    -name 'SubCommandDemo' -o -name 'SubCommandDemo.exe' \
  \) -print -delete
}

remove_generated_artifacts "$ROOT_DIR/example-bin"

for ex in $EXAMPLES; do
  remove_generated_artifacts "$ROOT_DIR/examples/$ex"
done

echo "✅ Generated example build artifacts removed."
