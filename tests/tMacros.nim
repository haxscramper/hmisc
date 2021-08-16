import
  hmisc/preludes/unittest

import
  hmisc/macros/[introspection, nim_ast_aux, ast_spec, wrapfields],
  hmisc/types/colorstring

import
  std/macros

suite "Enum introspection":
  test "Named subnodes":
    type
      AstKind = enum
        askFirst
        askSecond
        askThird

    check:
      namedSubnode(askFirst, 1, @{
        askFirst : @["head", "tail"]
      }) == "tail"

type
  AstKind = enum
    akIfStmt
    akExpr
    akTryStmt

  Ast = object
    kind: AstKind
    subnodes: seq[Ast]

wrapSeqContainer(Ast.subnodes, Ast)


suite "Ast spec":
  test "On nim node":
    macro spec() =
      echo nimAstSpec.validateAst(
        nnkInfix.newTree(newLit(12), ident"1", ident"2"))

      echo nimAstSpec.validateAst(nnkInfix.newTree())

    spec()

  test "On custom type":

    const spec = astSpec(Ast, AstKind):
      akIfStmt:
        0 .. 2: _

    check:
      astdiff Ast(kind: akIfStmt), spec
