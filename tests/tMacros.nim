import hmisc/preludes/unittest

testFileStarted()

import
  hmisc/macros/[introspection, nim_ast_aux, ast_spec, wrapfields],
  hmisc/types/colorstring

import
  std/macros

suite "Enum introspection":
  test "Named subnodes":
    type
      MacAstKind = enum
        askFirst
        askSecond
        askThird

    check:
      namedSubnode(askFirst, 1, @{
        askFirst : @["head", "tail"]
      }) == "tail"

type
  MacAstKind = enum
    akIfStmt
    akExpr
    akTryStmt
    akIdent

  MacAst = object
    kind: MacAstKind
    subnodes: seq[MacAst]

wrapSeqContainer(MacAst.subnodes, MacAst)


suite "MacAst spec":
  test "On nim node":
    macro spec() =
      let infix = newInfix(ident"+", ident"a", ident"b", ident"00")
      # let tr = newTryStmt(newEmptyNode())

      doAssert infix["op"].eqIdent("+")
      doAssert infix.getSub("op")[0].eqIdent("+")

      echo nimAstSpec.validateAst(
        nnkInfix.newTree(newLit(12), ident"1", ident"2"))

      echo nimAstSpec.validateAst(nnkInfix.newTree())

    spec()

  test "On custom type":

    const spec = astSpec(MacAst, MacAstKind):
      akIfStmt:
        0 as "condition":
          ## If statement conditiond
          akExpr

        1 as "then branch":
          ## then branch, can be anything

        2 as "else branch":
          ## else branch

        ?3 as "optional":
          ## Optional trailing element


    check:
      astdiff MacAst(kind: akIfStmt), spec

testFileEnded()