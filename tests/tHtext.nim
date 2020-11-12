import sugar, strutils, sequtils, strformat
import hmisc/algo/htext_algo

#===========================  implementation  ============================#

#================================  tests  ================================#

import unittest

suite "Htext algo":
  test "Hypenation":
    assert hyphenate("reformation") == @[
      "ref", "or", "ma", "tion"
    ]
