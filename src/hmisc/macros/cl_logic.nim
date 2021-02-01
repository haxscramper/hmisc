import macros
import ../hexceptions

macro typeCondIt*(head, body: untyped): untyped =
  assert body.kind == nnkStmtList
  result = nnkWhenStmt.newTree()
  for line in body:
    case line.kind:
      of nnkBracket:
        assertNodeIt(
          line, it.len == 2, "Expected bracket with two elements")
        let
          ntype = line[0]

        result.add nnkElifBranch.newTree(
          nnkInfix.newTree(ident "is", ident "it", line[0]), line[1])
      else:
        assertNodeIt(line, false, "Expected bracket with two elements")

  result.add nnkElse.newTree(head)

  result = quote do:
    block:
      let it {.inject.} = `head`
      `result`

macro cond*(v: untyped, conds: varargs[untyped]): untyped =
  ## Lisp-style condition checking. Simplifies use of if-expressions in
  ## other expressions. Syntax - takes `<input-value>` and multiple
  ## `(<value>, <expr>)` pairs and compares result of `<input-value>` with
  ## each `<value>` in branches. Yields first matched result.
  runnableExamples:
    let vald = cond(12,
                    (13, "hello"),
                    (14, "world"))

  var exprId = genSym(nskLet, "expr")

  result = nnkIfExpr.newTree()

  for cond in conds:
    cond.assertNodeKind({nnkTupleConstr, nnkPar})

    if cond.len == 1 or
       cond[0].eqIdent("_"):
      result.add nnkElseExpr.newTree(cond[^1])

    else:
      result.add nnkElifExpr.newTree(
        nnkInfix.newTree(ident "==", exprId, cond[0]),
        cond[1]
      )

  if result[^1].kind != nnkElseExpr:
    raise conds[^1].toCodeError(
      "No default branch for condition",
      "Use (_, <expr>) or (<expr>) to create default"
    )

  result = quote do:
    let `exprId` = `v`
    `result`

macro cond2*(conds: varargs[untyped]): untyped =
  result = nnkIfExpr.newTree()

  for cond in conds:
    cond.assertNodeKind({nnkTupleConstr, nnkPar})

    if cond.len == 1 or cond[0].eqIdent("_"):
      result.add nnkElseExpr.newTree(cond[^1])

    else:
      result.add nnkElifExpr.newTree(cond[0], cond[1])

  if result[^1].kind != nnkElseExpr:
    raise conds[^1].toCodeError(
      "No default branch for condition",
      "Use (_, <expr>) or (<expr>) to create default"
    )
