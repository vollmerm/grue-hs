# Revision history for grue-hs

## 0.2.0.0 -- 2026-07-22

* Support for Z-machine version 4 stories: the wider object table, nine
  Z-character dictionary entries, the version 4 instruction set (the
  call variants, `scan_table`, `read_char`, and the cursor, text-style,
  buffer-mode and window-erasing controls), and games that draw their
  own status line in the upper window.
* Passes the CZECH conformance suite at versions 3 and 4, bundled and
  checked by the test suite.

## 0.1.0.0 -- 2026-07-21

* First version. Released on an unsuspecting world.
* Tentative support for Z-machine version 3 stories.
* Curses-based interface, with a plain stdio fallback for piped or scripted play.
