import std/[sugar, strutils, sequtils, strformat, options]
import
  hmisc/core/all,
  hmisc/other/[blockfmt, hunittest]

suite "Block formatting minimal":
  initBlockFmtDsl()
  test "Vertical layouts":
    check V[T["a"], T["b"]].toString() == "a\nb"
