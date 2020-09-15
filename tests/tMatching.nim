import sugar, strutils, sequtils, strformat
import hmisc/helpers
import hmisc/macros/matching
import json

{.experimental: "caseStmtMacros".}

#===========================  implementation  ============================#

#================================  tests  ================================#

import unittest

suite "Matching":
  test "Main test":
    assertEq 12, case (12, 24):
                   of (_, it == 24): expr[1] div 2
                   else: raiseAssert("#[ not possible ]#")


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

    echo case %{"hello" : %"world"}:
           of {"999": _}: "nice"
           of {"hello": _}: "000"
           else: "discard"

    echo case @[12, 32]:
           of @[_, it mod 2 == 1]: expr[0]
           else: 999

  test "Regular objects":
    type
      A = object
        f1: int

    case A(f1: 12):
      of (f1: it > 10):
        echo "> 10"

    assertEq 10, case A(f1: 90):
                   of (f1: 0 <= it and it <= 80): 80
                   else: 10

  test "Private fields":
    type
      A = object
        hidden: float

    func public(a: A): string = $a.hidden

    startLog()

    case A():
      of (public: it.startsWith("0")):
        echo "matched: ", expr.public
      else:
        echo expr.public

    assertEq "10", case A(hidden: 8.0):
                     of (public: "8.0"): "10"
                     else: raiseAssert("#[ IMPLEMENT ]#") 

  test "Case objects":
    discard
    # type
    #   En = object


  test "Variable binding":
    discard

  test "Alternative":
    discard

  test "Trailing one-or-more":
    discard
