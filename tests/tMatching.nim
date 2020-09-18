import sugar, strutils, sequtils, strformat
import hmisc/helpers
import hmisc/macros/matching
import json

{.experimental: "caseStmtMacros".}

#===========================  implementation  ============================#

#================================  tests  ================================#

import unittest

suite "Matching":
  test "Has kind for anything":
    type
      En = enum
        eN11
        eN12

      Obj = object
        case kind: En
         of eN11:
           f1: int
         of eN12:
           f2: float

    let val = Obj()
    echo val.hasKind(N11)
    echo val.hasKind(eN11)

  test "Simple uses":
    assertEq 12, case (12, 24):
                   of (_, 24): expr[1] div 2
                   else: raiseAssert("#[ not possible ]#")


    assertEq "hehe", case (true, false):
           of (true, _): "hehe"
           else: "2222"

    assertEq "hello world", case (a: 12, b: 12):
           of (a: 12, b: 22): "nice"
           of (a: 12, b: _): "hello world"
           else: "default value"

    assertEq "default fallback", case (a: 22, b: 90):
           of (_, b: 91): "900999"
           elif "some other" == "check": "rly?"
           elif true: "default fallback"
           else: raiseAssert("#[ not possible ! ]#")

    assertEq "000", case %{"hello" : %"world"}:
           of {"999": _}: "nice"
           of {"hello": _}: "000"
           else: "discard"

    assertEq 12, case @[12, 32]:
           of [_, 32]: expr[0]
           else: 999

    assertEq 1, case [(1, 3), (3, 4)]:
                  of [(1, _), _]: 1
                  else: 999


  test "Regular objects":
    type
      A = object
        f1: int

    case A(f1: 12):
      of (f1: 12):
        discard "> 10"

    assertEq 10, case A(f1: 90):
                   of (f1: 20): 80
                   else: 10

  test "Private fields":
    type
      A = object
        hidden: float

    func public(a: A): string = $a.hidden


    case A():
      of (public: _):
        echo "matched: ", expr.public
      else:
        echo expr.public

    assertEq "10", case A(hidden: 8.0):
                     of (public: "8.0"): "10"
                     else: raiseAssert("#[ IMPLEMENT ]#")

  test "Case objects":
    type
      En = enum
        enEE
        enZZ

      Obj = object
        case kind: En
          of enEE:
            eee: seq[Obj]
          of enZZ:
            fl: int
    echo case Obj():
           of EE(): "00"
           of ZZ(): "hello worlkd"
           else: raiseAssert("#[ IMPLEMENT ]#")

    case Obj(kind: enEE, eee: @[Obj(kind: enZZ, fl: 12)]):
      of enEE(eee: [(kind: enZZ, f1: 12)]):
        discard
      else:
        fail()

    case Obj():
      of enEE():
        discard
      of enZZ():
        fail()
      else:
        fail()

    iterator items(o: Obj): Obj =
      for it in o.eee:
        yield it

    case Obj(kind: enEE, eee: @[Obj(), Obj()]):
      of [_, _]:
        discard
      else:
        fail()


  test "Variable binding":

    echo case (a: 12, b: 2):
           of (a: $a, b: $b): $a & $b
           else: "✠ ♰ ♱ ☩ ☦ ☨ ☧ ⁜ ☥"

    assertEq 12, case (a: 2, b: 10):
                   of (a: $a, b: $b): a + b
                   else: 89

    startHax()
    echo case (1, (3, 4, ("e", (9, 2)))):
           of ($a, _): a
           of (_, ($a, $b, _)): a + b
           of (_, (_, _, (_, ($c, $d)))): c * d
           else: 12

    # stopHax()
    echo "hello"


  test "Alternative":
    echo case (a: 12, c: 90):
           of (a: 12 | 90, c: _): "matched"
           else: "not matched"

    assertEq 12, case (a: 9):
                  of (a: 9 | 12): 12
                  else: 666

  test "Trailing one-or-more":
    discard
