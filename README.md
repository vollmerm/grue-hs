# grue-hs

A Z-machine interpreter written in Haskell. The [Z-machine](https://ifarchive.org/indexes/if-archive/infocom/interpreters/specification/) is the virtual machine that ran Infocom's text adventures, such as [Zork](https://ifdb.org/viewgame?id=0dbnusxunq7fw5ro), [A Mind Forever Voyaging](https://ifdb.org/viewgame?id=4h62dvooeg9ajtfa), and [Wishbringer](https://ifdb.org/viewgame?id=z02joykzh66wfhcl). This interpreter targets version 4 of the format, which covers much of the Infocom catalog.

The core opcode set, object table, dictionary, and save/restore (via the [Quetzal format](https://www.inform-fiction.org/zmachine/standards/quetzal/)) all work, but the implementation hasn't been fully tested yet. Support for Z-machine versions beyond 4 is planned but not implemented.

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

```
cabal test
```

## License

BSD-3-Clause. See `LICENSE`.
