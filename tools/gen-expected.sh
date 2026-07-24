#!/bin/bash
# Regenerate the frozen expected output for the precise-test stories.
#
# For each compiled story in test/stories/precise/, run it over its
# <name>.in input (empty if absent) and write the exact game output to
# <name>.out.  The oracle is fizmo-console (set FIZMO to override),
# whose one leading blank line is dropped; a story whose source
# declares "! Oracle: grue" is instead frozen from grue-hs itself,
# because its output depends on grue's own deterministic PRNG and no
# other interpreter will reproduce it.  Regenerate whenever a story or
# its input changes, then eyeball the diff before committing.
set -eu
FIZMO="${FIZMO:-fizmo-console}"
dir="$(cd "$(dirname "$0")/.." && pwd)/test/stories/precise"
grue="$(cd "$(dirname "$0")/.." && cabal list-bin exe:grue-hs 2>/dev/null)"
if [ ! -x "$grue" ]; then
  echo "error: build grue-hs first (cabal build exe:grue-hs)" >&2
  exit 2
fi

shopt -s nullglob
for story in "$dir"/*.z3 "$dir"/*.z4; do
  base="${story%.*}"
  name="$(basename "$base")"
  in="$base.in"
  [ -f "$in" ] || in=/dev/null
  if grep -q 'Oracle: grue' "$base.inf"; then
    "$grue" --console "$story" < "$in" > "$base.out"
    echo "froze $name.out (grue)"
  else
    "$FIZMO" "$story" < "$in" 2>&1 \
      | awk 'NR==1 && $0=="" {next} {print}' > "$base.out"
    echo "froze $name.out (fizmo)"
  fi
done
