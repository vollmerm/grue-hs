# Bundled test stories

Story files kept in the repository so the integration tests always have
something real to run, regardless of what is installed on the machine.

- `cloak.z3` — "Cloak of Darkness" by Roger Firth, the standard freely
  distributable interactive-fiction demonstration game, in the ZIL
  conversion by Jesse McGrew, Jayson Smith, and Josh Lawrence, compiled
  to Z-machine version 3 with ZILF. Being ZILF output rather than an
  Infocom or Inform compile, it also exercises a different code
  generator than the commercial story files.

Commercial story files (the Infocom games) are not distributable and
are therefore looked up outside the repository: in the directory named
by the `GRUE_STORY_DIR` environment variable, or failing that in a
`zifmia` story collection checked out beside this repository. Tests
for stories that cannot be found are skipped.
