# Bundled test stories

Story files kept in the repository so the integration tests always have
something real to run, regardless of what is installed on the machine.

- `cloak.z3` — "Cloak of Darkness" by Roger Firth, the standard freely
  distributable interactive-fiction demonstration game, in the ZIL
  conversion by Jesse McGrew, Jayson Smith, and Josh Lawrence, compiled
  to Z-machine version 3 with ZILF. Being ZILF output rather than an
  Infocom or Inform compile, it also exercises a different code
  generator than the commercial story files.
- `czech.z3`, `czech.z4` — CZECH, the "Comprehensive Z-machine Emulation
  CHecker" by Amir Karger, a conformance suite that runs several hundred
  opcode and table tests and reports how many passed. Compiled from
  `czech.inf` with Inform 6 at versions 3 and 4; the test suite asserts
  that both run to completion with no failures. Distributed under the
  permissive licence reproduced in `czech-LICENSE.txt`.

Commercial story files (the Infocom games) are not distributable and
are therefore looked up outside the repository: in the directory named
by the `GRUE_STORY_DIR` environment variable, or failing that in a
`zifmia` story collection checked out beside this repository. Tests
for stories that cannot be found are skipped.
