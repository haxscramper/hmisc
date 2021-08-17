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
    akIdent

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
        0 .. 1:
          ## Documentation for first element in range

          akIdent

        2:
          ## Documentation for second node in range

          akExpr

          akIdent
          ## Comment 2

          akExpr:
            0: akIdent
            1: akIdent

        ?3:
          ## Documentation for optional field at index 3


    check:
      astdiff Ast(kind: akIfStmt), spec
