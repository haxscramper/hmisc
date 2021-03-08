import std/[sugar, strutils, sequtils, strformat, options]
import hmisc/helpers
import hmisc/hdebug_misc

import unittest
import hmisc/other/blockfmt

suite "Block formatting minimal":
  let
    txb = makeTextBlock
    vsb = makeStackBlock
    hsb = makeLineBlock
    ind = makeIndentBlock
    verb = makeVerbBlock
    wrap = makeWrapBlock
    choice = makeChoiceBlock
    nl = makeForceLinebreak


  proc lyt(bl: LytBlock, m1: int = 40): string =
    var bl = bl
    let ops = defaultFormatOpts.withIt do:
      it.rightMargin = m1

    let sln = none(LytSolution).withResIt do:
      bl.doOptLayout(it, ops).get()

    sln.layouts[0].printOn(result)

  let str = lyt

  test "Vertical layouts":
    assertEq @["a".txb, "b".txb].vsb().lyt(), "a\nb"
