#!/bin/bash
# clean-all-examples.sh
# Remove all built example binaries and unit output

set -e

if [ -d "example-bin" ]; then
  echo "🧹 Removing example-bin/ directory..."
  rm -rf example-bin
  echo "✅ example-bin/ cleaned."
else
  echo "ℹ️  example-bin/ does not exist. Nothing to clean."
fi

for ex in BooleanTest ColorDemo ErrorHandlingDemo LongRunningOpDemo MyApp MyGit ProgressDemo SimpleDemo SubCommandDemo TestFlags; do
  if [ -d "examples/$ex/lib" ]; then
    echo "🧹 Removing old lib/ from examples/$ex..."
    rm -rf "examples/$ex/lib"
  fi
  if [ -d "examples/$ex/x86_64-win64" ]; then
    echo "🧹 Removing old x86_64-win64/ from examples/$ex..."
    rm -rf "examples/$ex/x86_64-win64"
  fi
  if [ -d "examples/$ex/x86_64-linux" ]; then
    echo "🧹 Removing old x86_64-linux/ from examples/$ex..."
    rm -rf "examples/$ex/x86_64-linux"
  fi
  if [ -d "examples/$ex/backup" ]; then
    echo "🧹 Cleaning backup/ in examples/$ex..."
    find "examples/$ex/backup" -type f \( -name '*.exe' -o -name '*.dbg' -o -name '*.o' -o -name '*.ppu' \) -delete
  fi
done

echo "\n✅ Cleanup complete."
