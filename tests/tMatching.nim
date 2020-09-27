import sugar, strutils, sequtils, strformat, macros, options
import hmisc/[helpers, hexceptions]
import hmisc/macros/matching
import json

{.experimental: "caseStmtMacros".}

#===========================  implementation  ============================#

#================================  tests  ================================#

import unittest

suite "Matching":
  # FIXME
  # test "Nim Node":
  #   macro e(body: untyped): untyped =
  #     case body[0]:
  #       of ForStmt([@ident, _, @expr]):
  #         quote do:
  #           9
  #       of ForStmt([@ident, Infix([== ident(".."), @rbegin, @rend]),
  #                   @body]):
  #         quote do:
  #           `rbegin` + `rend`
  #       else:
  #         quote do:
  #           90


  #   let a = e:
  #     for i in 10 .. 12:
  #       echo i

  #   assertEq a, 22


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

  test "Simple uses":
    assertEq 12, case (12, 24):
                   of (_, 24): expr[1] div 2
                   else: raiseAssert("#[ not possible ]#")


    case [1]:
      of [_]: discard

    workHaxComp false:
      case [1,2,3,4]:
        of [_]: fail()
        of [_, 2, 3, _]:
          discard


    assertEq "hehe", case (true, false):
           of (true, _): "hehe"
           else: "2222"

    assertEq "hello world", case (a: 12, b: 12):
           of (a: 12, b: 22): "nice"
           of (a: 12, b: _): "hello world"
           else: "default value"

    assertEq "default fallback", case (a: 22, b: 90):
           of (b: 91): "900999"
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

    assertEq 2, case (true, false):
                  of (true, true) | (false, false): 3
                  else: 2

  test "Len test":
    macro e(body: untyped): untyped =
      case body:
        of Bracket([Bracket(len: in {1 .. 3})]):
          newLit("Nested bracket !")
        of Bracket(len: in {3 .. 6}):
          newLit(expr.toStrLit().strVal() & " matched")
        else:
          newLit("not matched")

    discard e([2,3,4])
    discard e([[1, 3, 4]])
    discard e([3, 4])


  test "Regular objects":
    type
      A = object
        f1: int

    case A(f1: 12):
      of (f1: 12):
        discard "> 10"
      else:
        fail()

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
        discard
      else:
        fail()

    case A(hidden: 8.0):
      of (public: "8.0"): discard
      else: fail()

  type
    En = enum
      enEE
      enEE1
      enZZ

    Obj = object
      case kind: En
        of enEE, enEE1:
          eee: seq[Obj]
        of enZZ:
          fl: int


  test "Case objects":
    case Obj():
      of EE(): discard
      of ZZ(): fail()

    case (c: (a: 12)):
      of (c: (a: _)): discard
      else: fail("")

    case [(a: 12, b: 3)]:
      of [(a: 12, b: 22)]: fail("")
      of [(a: _, b: _)]: discard

    case (c: [3, 3, 4]):
      of (c: [_, _, _]): discard
      of (c: _): fail("")

    case (c: [(a: [1, 3])]):
      of (c: [(a: [_])]): fail("")
      else: discard

    case (c: [(a: [1, 3]), (a: [1, 4])]):
      of (c: [(a: [_]), _]): fail("")
      else:
        discard

    case Obj(kind: enEE, eee: @[Obj(kind: enZZ, fl: 12)]):
      of enEE(eee: [(kind: enZZ, fl: 12)]):
        discard
      else:
        fail("")

    case Obj():
      of enEE():
        discard
      of enZZ():
        fail()
      else:
        fail()

    case Obj():
      of (kind: in {enEE, enEE1}):
        discard
      else:
        fail()

  test "Object items":
    func `[]`(o: Obj, idx: int): Obj = o.eee[idx]
    func len(o: Obj): int = o.eee.len

    case Obj(kind: enEE, eee: @[Obj(), Obj()]):
      of [_, _]:
        discard
      else:
        fail()

    # startHax()
    case Obj(kind: enEE, eee: @[Obj(), Obj()]):
      of EE(eee: [_, _, _]): fail()
      of EE(eee: [_, _]): discard
      else: fail()
    # stopHax()

    # case Obj(kind: enEE1, eee: @[Obj(), Obj()]):
    #   of EE([_, _]):
    #     fail()
    #   of EE1([_, _, _]):
    #     fail()
    #   of EE1([_, _]):
    #     discard
    #   else:
    #     fail()



  test "Variable binding":
    when false: # NOTE compilation error test
      case (1, 2):
        of ($a, $a, $a, $a):
          discard
        else:
          fail()

    assertEq "122", case (a: 12, b: 2):
                      of (a: @a, b: @b): $a & $b
                      else: "✠ ♰ ♱ ☩ ☦ ☨ ☧ ⁜ ☥"

    assertEq 12, case (a: 2, b: 10):
                   of (a: @a, b: @b): a + b
                   else: 89

    assertEq 1, case (1, (3, 4, ("e", (9, 2)))):
      of (@a, _): a
      of (_, (@a, @b, _)): a + b
      of (_, (_, _, (_, (@c, @d)))): c * d
      else: 12

    proc tupleOpen(a: (bool, int)): int =
      case a:
        of (true, @capture): capture
        else: -90

    assertEq 12, tupleOpen((true, 12))

    # proc f((a, b): (bool, int)) = if a: b else: b+10

  test "Infix":
    macro a(): untyped  =
      workHaxComp false:
        case newPar(ident "1", ident "2"):
          of Par([@ident1, @ident2]):
            assert ident1.strVal == "1"
            assert ident2.strVal == "2"
          else:
            assert false

    a()

  test "Iflet 2":
    macro ifLet2(head: untyped,  body: untyped): untyped =
      # startHaxComp()
      case head[0]:
        of Asgn([@lhs is Ident(), @rhs]):
          quote do:
            let expr = `rhs`
            if expr.isSome():
              let `lhs` = expr.get()
              `body`
        else:
          head[0].assertNodeKind({nnkAsgn})
          head[0][0].assertNodeKind({nnkIdent})
          head[0].raiseCodeError("Expected assgn expression")

    ifLet2 (nice = some(69)):
      assert nice == 69



  test "Alternative":
    assertEq "matched", case (a: 12, c: 90):
      of (a: 12 | 90, c: _): "matched"
      else: "not matched"

    assertEq 12, case (a: 9):
                  of (a: 9 | 12): 12
                  else: 666


  # test "Set":
  #   case {0 .. 3}:
  #     of {2, 3}: discard
  #     else: fail()

  #   case {4 .. 10}:
  #     of {@a, 9}:
  #       assert a is set
  #       assert 7 in a
  #     else:
  #       fail()

  test "Match assertions":
    [1,2,3].assertMatch([all @res]); assertEq res, @[1,2,3]
    [1,2,3].assertMatch([all @res2]); assertEq res2, @[1,2,3]
    [1,2,3].assertMatch([@first, all @other])
    assertEq first, 1
    assertEq other, @[2, 3]


    block: [@first, all @other] := [1,2,3]
    block: [_, _, _] := @[1,2,3]
    block: (@a, @b) := ("1", "2")
    block: (_, (@a, @b)) := (1, (2, 3))
    block:
      let tmp = [1,2,3,4,5,6,5,6]
      [until @a == 6, .._] := tmp; assertEq a, @[1,2,3,4,5]

    block: # REVIEW special case ?
      # startHaxComp()
      block: [0..3 is @head] := @[1,2,3,4]

    case [%*"hello", %*"12"]:
      of [any @elem is JString()]:
        discard
      else:
        fail()

    case ("foo", 78)
      of ("foo", 78):
        discard
      of ("bar", 88):
        fail()

    expandMacros:
      block: Some(@x) := some("hello")

  test "One-or-more":
    template testCase(main, patt, body: untyped): untyped =
      case main:
        of patt:
          body
        else:
          fail()
          raiseAssert("#[ IMPLEMENT ]#")

    case [1]:
      of [@a]: assertEq a, 1
      else: fail()

    case [1]:
      of [all @a]: assertEq a, @[1]
      else: fail()

    static:
      debugTraceMatch = true


    # startHaxComp()
    case [1,2,3,4]:
      of [_, until @a is 4, 4]:
        assertEq a, @[2,3]
      else:
        echo trace
        fail()

    # assertEq 1, testCase([1], [@a], a)

    # assertEq @[1], testCase([1], [all @a], a)
    # assertEq @[2, 2, 2], testCase([1, 2, 2, 2, 4],
    #                               [_, until @a is 4, 4], a)

    # assertEq (@[1], @[3, 3, 3]), testCase(
    #   [1, 2, 3, 3, 3], [until @a is 2, _, all @b], (a, b))



  #   case [1,2,3,4]:
  #     of [@a, .._]:
  #       assert a is int
  #       assert a == 1
  #     else:
  #       fail()


  #   workHax true:
  #     case [1,2,3,4]:
  #       of [*@a]:
  #         assert a is seq[int]
  #         assert a == @[1,2,3,4]
  #       else:
  #         fail()

  #   startHaxComp()
  #   stopHaxComp()

  # test "Optional matches":
  #   case [1,2,3,4]:
  #     of [@a is *(1 | 2), _, _, 5 ?@ a]:
  #       echo a

  #   case [1,2,2,1,1,1]:
  #     of [*(1 | @a)]:
  #       assert a is seq[int]
  #       assertEq a, @[2, 2]

  #   case (1, some(12)):
  #     of (_, 13 ?@ hello):
  #       assert hello is int
  #       assertEq hello, 13

  #   case (1, none(int)):
  #     of (_, 15 ?@ hello):
  #       assert hello is int
  #       assertEq hello, 15

  #   case (3, none(string)):
  #     of (_, ?@ hello):
  #       assert hello is Option[string]
  #       assert hello.isNone()


  # dumpTree:
  #   IfStmt([*ElseIf([_, @bodies]), newEmptyNode() ?@ bodies])
  #   [@a, .._ @b] [.._ @b, @c] [@b, @c .._]

  #   ForStmt([-> ident,
  #            Infix([== ident(".."),
  #                   -> rbegin,
  #                   -> rend]),
  #            -> body])

  #   [_.isString() isnot doError()]
  #   #  capture all leading elements until found "d"; do not include it
  #   [add @a is Patt(), until "d", .._]
  #   # capture all leading icludiinng
  #   [add @a is Patt(), incl "d", .._]
  #   # optional second value
  #   [add Patt(), opt @b]
  #   [add @s, until 4]
  #   [add @s, with 4]
  #   # optional with default value
  #   [add @head, opt @tail ? "default"]

  #   [add @leading, until @middle is "d", add @trailing]

  #   [*@leading, @middle is "d", *@trailing]

  #   case node:
  #     of IfStmt([add ElseIf([@cond, @body]), opt @elseClause]):
  #       discard
