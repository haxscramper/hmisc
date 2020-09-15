import sugar, strutils, sequtils, strformat
import hmisc/helpers
import hmisc/macros/matching
import json

{.experimental: "caseStmtMacros".}

#===========================  implementation  ============================#

#================================  tests  ================================#

import unittest

suite "Matching":
  test "test":
    when false:
      echo case (true, false):
             of (true, _): "hehe"
             else: "2222"

      echo case (a: 12, b: 12):
             of (a: 12, b: 22): "nice"
             of (a: it mod 2 == 0, b: _): "hello world"
             else: "default value"

      echo case (a: 22, b: 90):
             of (_, b: it * 2 < 90): "900999"
             elif "some other" == "check": "rly?"
             elif true: "default fallback"
             else: raiseAssert("#[ not possible ! ]#")

      assertEq 12, case (12, 24):
                     of (_, it == 24): expr[1] div 2
                     else: raiseAssert("#[ not possible ]#")

    echo case %{"hello" : %"world"}:
           of {"999": _}: "nice"
           of {"hello": _}: "000"
           else: "discard"



