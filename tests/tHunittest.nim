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
    check(stringdiff("a", "b"))

  test "parametrize":
    parametrizeOnConst N, [1, 3, 4]:
      check N == N

suite "Matchdiff":
  test "tuples":
    check matchdiff((a: 12), (a: 13))

  test "operators":
    check matchdiff((a: 12), (a: > 13))

  test "Sequence matches":
    check matchdiff([1, 2], [2, 3])
    check matchdiff([1, 2], [2, > 10])
    check matchdiff([1, 2], [2])

  test "AST":

    let ast = Ast(kind: akFirst, subnodes: @[
      Ast(
        kind: akSecond,
        subnodes: @[
          Ast(kind: akThird),
          Ast(kind: akThird)])])

    check matchdiff(ast, akFirst[
      akSecond[
        akThird(),
        akFirst()]])

    check astdiff(ast, akFirst[
      akSecond[
        akThird(),
        akFirst()]])


suite "Simple checks":
  test "equality":
    check 123 == 12

suite "structdiff":
  test "different fields":
    type
      TestStruct = object
        f: string

    let
      s1 = TestStruct(f: "struct-1")
      s2 = TestStruct(f: "struct-2")

    # echo pptree(s1).pstring()
    # echo pptree(s2).pstring()

    check structdiff(s1, s2)

  test "Mismatched fields":
    # let blc = ppblock newPPrintAnnotation(
    #   "head" + fgBlue,
    #   @[
    #     newPPrintAnnotation(
    #       "deleted field" + fgRed, @[newPPrintConst("12")]),

    #     newPPrintAnnotation(
    #       "added field" + fgGreen, @[newPPrintConst("12")]),
    #   ]
    # )

    # echo blc.toString()

    check structdiff(
      newPPrintObject("head", @{"field1": newPPrintConst("12")}),
      newPPrintObject("head", @{"field2": newPPrintConst("12")})
    )
