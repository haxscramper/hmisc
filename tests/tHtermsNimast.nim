import hmisc/nimast_trs

import unittest

suite "Hterms nim ast":
  test "DSL to declare rewriting system":
    discard
    #[ IMPLEMENT ]#
    # var
    #   matchedPattern {.global, compiletime.}: bool = false
    #   calledReduction: bool = false

    # macro rewriteTest(body: untyped): untyped =
    #   let rewrite = makeNodeRewriteSystem:
    #     rule:
    #       patt: Call(Ident("hello"), [[other]])
    #       outp:
    #         matchedPattern = true
    #         let exprStr = ($other.toStrLit()).newLit()
    #         echo "Matched pattern, `other`: ", other.toStrLit()
    #         quote do:
    #           echo "calling proc hello with one argument"
    #           echo "expr: ", `exprStr`
    #           echo "argument value: ", `other`
    #           calledReduction = true
    #           hello(`other`)

    #   let term = body.toTerm()
    #   let nodeTree = proc(n: NimNode): string = n.treeRepr()
    #   let reduced = reduce(term, rewrite)
    #   if reduced.ok:
    #     result = reduced.term.fromTerm()

    #   assert matchedPattern


    # proc hello(param: int) =
    #   echo  param

    # rewriteTest:
    #   hello(12 + 999)

    # assert calledReduction
    #[ IMPLEMENT ]#
