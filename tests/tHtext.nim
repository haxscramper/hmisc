import
  hmisc/preludes/unittest,
  hmisc/algo/htext_algo

import
  std/[sugar, strutils, sequtils, strformat]

suite "Htext algo":
  test "Hypenation":
    assert hyphenate("reformation") == @[
      "ref", "or", "ma", "tion"
    ]
