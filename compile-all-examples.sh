#!/bin/bash
# compile-all-examples.sh
# Build all example projects in the examples/ folder using lazbuild
# Usage: ./compile-all-examples.sh [lazbuild flags]

set -e

if ! command -v lazbuild &> /dev/null; then
  echo "❌ Error: lazbuild not found in PATH. Please install Lazarus and ensure lazbuild is available." >&2
  exit 1
fi

EXAMPLES=(
  BooleanTest
  ColorDemo
  ErrorHandlingDemo
  LongRunningOpDemo
  MyApp
  MyGit
  ProgressDemo
  SimpleDemo
  SubCommandDemo
  TestFlags
)

for ex in "${EXAMPLES[@]}"; do
  echo -e "\n🔨 Building $ex ..."
  lazbuild "examples/$ex/$ex.lpi" "$@"
  if [ $? -ne 0 ]; then
    echo "❌ Build failed for $ex" >&2
    exit 1
  else
    echo "✅ $ex built successfully."
  fi
done

echo -e "\n🎉 All examples built successfully!"
