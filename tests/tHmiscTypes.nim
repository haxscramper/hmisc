import sugar, strutils, sequtils, strformat

import hmisc/types/[seq2d, hprimitives]
import hmisc/hcommon_converters

import unittest

suite "{Seq2D}":
  test "{mapIt2d}":
    let grid = toSeq2D(@[@["HEllo"]])
    let grid2 = grid.mapIt2d(it.len)
    assert grid2[0, 0] == 5

  test "{mapIt2d}":
    let grid = toSeq2D(@[@["Hello\nworld"]])

    # I'm losing my sanity
    assert grid is Seq2D[string]
    assert grid[0, 0] is string
    assert "a\nb".split("\n") == @["a", "b"]
    assert @["a", "b"] is seq[string]
    assert toSeq(strutils.split("Hello", "\n")) is seq[string]
    assert toSeq(grid[0, 0].split("\n")) is seq[string]

    let mapped: Seq2D[StrBlock] = grid.mapIt2d(
      toSeq(($it).split("\n"))
    )

    assert mapped[0, 0] == @["Hello", "world"]
