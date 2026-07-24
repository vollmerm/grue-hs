#!/bin/bash
# Compile the precise-test stories from their Inform 6 source.
#
# Each story in test/stories/precise/ has an .inf source that declares
# its target Z-machine version with a "! Target: vN" comment (default
# v3); the compiled story is written beside it as <name>.zN.  The
# compiled files are checked in, so this only needs re-running when a
# source changes.  Set INFORM to override the compiler path.
set -eu
INFORM="${INFORM:-$HOME/Repos/Inform6/inform6}"
dir="$(cd "$(dirname "$0")/.." && pwd)/test/stories/precise"

for src in "$dir"/*.inf; do
  base="${src%.inf}"
  ver=3
  grep -q 'Target: v4' "$src" && ver=4
  out="$base.z$ver"
  if "$INFORM" "-v$ver" "$src" "$out" >/dev/null 2>&1; then
    echo "built $(basename "$out")"
  else
    echo "FAILED $(basename "$src"):" >&2
    "$INFORM" "-v$ver" "$src" "$out" >&2
    exit 1
  fi
done
