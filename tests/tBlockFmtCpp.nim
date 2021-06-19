import std/[sugar, strutils, sequtils, strformat, options]
import hmisc/helpers
import hmisc/hdebug_misc

import unittest
import hmisc/other/blockfmt

suite "Block formatting minimal":
  initBlockFmtDsl()
  test "Vertical layouts":
    assertEq V[T["a"], T["b"]].toString(), "a\nb"
