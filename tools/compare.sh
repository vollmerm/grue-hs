#!/bin/bash
# Compare grue-hs transcripts against a reference interpreter.
#
# Usage: tools/compare.sh STORY-FILE WALKTHROUGH
#
# Runs both interpreters over the walkthrough's input lines and diffs
# the transcripts.  The reference defaults to fizmo-console (set FIZMO
# to override).  fizmo-console prints one extra blank line before the
# game banner, which is dropped before comparing.
#
# Walkthroughs must stick to deterministic commands: responses chosen
# at random (Zork's snarky refusals, combat) vary between runs of the
# reference interpreter itself.
set -u
FIZMO="${FIZMO:-fizmo-console}"
story="$1"
script="$2"

grue=$(cd "$(dirname "$0")/.." && cabal list-bin exe:grue-hs 2>/dev/null)
if [ ! -x "$grue" ]; then
  echo "error: build grue-hs first (cabal build exe:grue-hs)" >&2
  exit 2
fi

fz=$(mktemp) gr=$(mktemp)
trap 'rm -f "$fz" "$gr"' EXIT

timeout 60 "$FIZMO" "$story" < "$script" 2>&1 \
  | awk 'NR==1 && $0=="" {next} {print}' > "$fz"
timeout 60 "$grue" "$story" < "$script" > "$gr" 2>&1

if diff -q "$fz" "$gr" >/dev/null; then
  echo "IDENTICAL: $(basename "$story") ($(wc -l < "$gr" | tr -d ' ') lines)"
else
  echo "DIFFERS: $(basename "$story")"
  diff "$fz" "$gr"
  exit 1
fi
