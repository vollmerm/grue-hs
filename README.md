# grue-hs

A Z-machine interpreter written in Haskell. The [Z-machine](https://ifarchive.org/indexes/if-archive/infocom/interpreters/specification/) is the virtual machine that ran Infocom's text adventures, such as [Zork](https://ifdb.org/viewgame?id=0dbnusxunq7fw5ro), [A Mind Forever Voyaging](https://ifdb.org/viewgame?id=4h62dvooeg9ajtfa), and [Wishbringer](https://ifdb.org/viewgame?id=z02joykzh66wfhcl). The interpreter fully supports versions 3 and 4 and now runs much of version 5 as well.

The opcode set, object table, dictionary, and save/restore (via the [Quetzal format](https://www.inform-fiction.org/zmachine/standards/quetzal/)) all work. Version 5 support includes the core call/save/restore semantics, text and dictionary tables, custom alphabet and Unicode translation tables, and enough control-flow support to run the version-5 build of the CZECH conformance suite. Some presentation-oriented version 5 features are still being filled in.

## Building

Requires GHC 9.10.x and cabal 3.12 or later (a ghcup install works fine).

```
cabal build
```

## Running

```
cabal run grue-hs -- STORY.z3
```

The `grue-hs` executable opens a full-screen curses interface when run from a terminal. When its input or output is piped, or when run with `--console`, it falls back to plain stdio.

## Testing

Correctness is checked two ways.

The `cabal test` suite runs a set of unit tests along with the CZECH conformance suite (bundled for v3 and v4, plus v5 when `GRUE_STORY_DIR` or the sibling `../zifmia` checkout provides `testers/czech/czech.z5`) and a full save/restore roundtrip against the bundled `cloak.z3`:

```
cabal test
```

To run a single test or group, pass a [tasty](https://hackage.haskell.org/package/tasty) pattern:

```
cabal run grue-test -- -p Quetzal
```

The second check is byte-for-byte transcript comparison against a reference interpreter. `tools/compare.sh` runs both grue-hs and `fizmo-console` over the same walkthrough script and diffs the output. Each walkthrough is written for one story and takes it as the first argument; the bundled `cloak.z3` has one you can run out of the box:

```
tools/compare.sh test/stories/cloak.z3 tools/walkthroughs/cloak.txt
```

The other walkthroughs in `tools/walkthroughs/` (`zork1`, `minizork`, `advent`) target commercial Infocom stories that aren't distributed here; run them by supplying your own copy of the matching story file.

Walkthroughs stick to deterministic commands, since responses chosen at random (combat, snarky refusals) vary between runs of the reference interpreter itself.

## Acknowledgements

grue-hs has leaned heavily on [fizmo](https://github.com/chrender/fizmo) throughout its development. Its `fizmo-console` frontend serves as the reference interpreter for transcript comparison, and fizmo has been a valuable baseline and source of inspiration for getting the Z-machine's behaviour right.

## License

BSD-3-Clause. See `LICENSE`.
