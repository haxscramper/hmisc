import hmisc/preludes/unittest

testFileStarted()


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
        (a: 1, q: 2): (1, q: 2),
        [1, 2]: [_, _]
      ]


suite "Simple checks":
  test "equality":
    check 12 == 12

  test "timeIt":
    timeIt "add numbers":
      let a = 2 + 3
      let b = a + 2

import hmisc/hasts/xml_ast
import std/algorithm

type
  XUnitContext* = ref object of TestContext
    w: XmlWriter

method report*(ctx: XUNitContext, report: TestReport) =
  case report.kind:
    of trkSuiteStart:
      ctx.w.xmlStart("assembly", {
        "name": report.name
      })
      ctx.w.indent()

    of trkSuiteEnd:
      ctx.w.dedent()
      ctx.w.xmlEnd("assembly")

    of trkTestEnd:
      ctx.w.dedent()
      ctx.w.xmlEnd("collection")

    of trkTestFail:
      case report.failKind:
        of tfkException:
          let e = report.exception
          ctx.w.ewrapl("failure", {"exception-type": $e.name}):
            ctx.w.writeIndent()
            ctx.w.ewrap("message"):
              ctx.w.writeRaw(e.msg)

            ctx.w.line()

            ctx.w.ewrapl("stacktrace"):
              for entry in e.getStackTraceEntries().reversed():
                ctx.w.xmlSingle("trace", {
                  "line": $entry.line,
                  "proc": $entry.procname,
                  "filename": $entry.filename
                })



        else:
          ctx.w.xmlRawSingle($report.failKind)

      ctx.w.dedent()
      ctx.w.xmlEnd("collection")

    of trkTestStart:
      ctx.w.xmlStart("collection", {
        "name": report.name
      })

      ctx.w.indent()


    of trkCheckOk:
      ctx.w.xmlSingle("test", {
        "result": "Pass",
        "name": report.expr & " at " & report.name
      })

    of trkCheckFail:
      ctx.w.ewrapl("test", {
        "result": "Fail",
        "name": report.expr & " at " & report.name
      }):
        ctx.w.ewrapl("failure", []):
          ctx.w.xmlSingle($report.failKind, [])

    of trkFileEnded:
      ctx.w.close()

    else:
      ctx.w.xmlRawSingle($report.kind)

proc newXUnitContext*(): XUnitContext =
  XUnitContext(w: newXmlWriter(stdout))

setTestContext(newXUnitContext())

suite "Test suite with XML reporting":
  test "Starting test":
    check:
      12 == 12
      "test" == "test"

    check:
      12 == 3

  test "Raise exception":
    raise newException(OsError, "Message")


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

testFileEnded()
