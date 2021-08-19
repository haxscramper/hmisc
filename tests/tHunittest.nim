import
  hmisc/other/[hunittest, hpprint, blockfmt],
  hmisc/types/[colorstring],
  hmisc/macros/wrapfields,
  hmisc/core/all

import
  std/macros

startHax()

type
  AstKind = enum
    akFirst
    akSecond
    akThird

  Ast = object
    subnodes: seq[Ast]
    kind: AstKind

wrapSeqContainer(Ast.subnodes, Ast, exported = false)


suite "main suite":
  test "test suite":
    check(strdiff("a", "a"))

  test "parametrize":
    parametrizeOnConst N, [1, 3, 4]:
      check N == N

suite "Matchdiff":
  test "tuples":
    check matchdiff((a: 12), (a: 12))

  test "operators":
    check matchdiff((a: 12), (a: > 10))

  test "Sequence matches":
    check matchdiff([1, 2], [1, 2])
    check matchdiff([1, 2], [1, > 0])

  test "AST":

    let ast = Ast(kind: akFirst, subnodes: @[
      Ast(
        kind: akSecond,
        subnodes: @[
          Ast(kind: akThird),
          Ast(kind: akThird)])])

    check astdiff(ast, akFirst[
      akSecond[
        akThird(),
        akThird()]])

  test "Multimatch":
    proc call(arg: int): seq[tuple[a: int, b: string]] =
      @[(arg, $arg), (arg + 1, $(arg + 1))]

    check:
      matchdiff @(a, b), [
        call(0): [(0, "0"), (1, "1")],
        call(1): [(1, "1"), (2, "2")],
        (a: 1, b: 1, c: 1): (1, 1, c: 1),
        (a: 1): (_),
        (a: 1, q: 2): (1, q: 2)
      ]


suite "Simple checks":
  test "equality":
    check 12 == 12

# suite "structdiff":
#   test "different fields":
#     type
#       TestStruct = object
#         f: string

#     let
#       s1 = TestStruct(f: "struct-1")
#       s2 = TestStruct(f: "struct-2")

#     check structdiff(s1, s2)

#   test "Mismatched fields":
#     check structdiff(
#       newPPrintObject("head", @{"field1": newPPrintConst("12")}),
#       newPPrintObject("head", @{"field2": newPPrintConst("12")})
#     )
