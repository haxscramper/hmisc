macro quoteDoInterpolStmt*(body: untyped): untyped =
  ## Allows to interpolate function call into `quote do` body. Literal
  ## strings surrounded by backticks will be replaced by function calls.
  runnableExamples:
    import macros
    macro test(): untyped = quote do: 123

    # With interpol stmt
    macro macrotest(): untyped =
      result = quoteDoInterpolStmt:
        `"test()"`

    macro macrotest2(): untyped =
      let sub = test()
      result = quote do:
        `sub`

    assert macrotest() == 123
    assert macrotest2() == macrotest()

  # defer:
  #   echo "quote do interpol"
  #   echo result.toStrLit()

  # TODO add documentaion
  # TODO refactor to two simpler functions
  var stmts: seq[tuple[i: int, node: NimNode]]
  var newBody: seq[NimNode]

  for idx, stmt in body:
    case stmt.kind
    of nnkAccQuoted:
      let parsed: NimNode = stmt[0].toStrLit.strVal.parseExpr
      if parsed.kind == nnkCall:
        stmts.add((i: idx, node: parsed))
        let newStmt = ident("tmp" & $idx)
        newBody.add(
          nnkAccQuoted.newTree(newStmt))
      else:
        newBody.add(stmt)

    of nnkLetSection, nnkVarSection:
      if stmt[0][2].kind == nnkAccQuoted:
        let parsed = stmt[0][2][0].toStrLit.strVal.parseExpr
        stmts.add((idx, parsed))

        let lhs = stmt[0][0]
        let typ = stmt[0][1]
        let rhs = nnkAccQuoted.newTree(ident("tmp" & $idx))
        let newAssgn = newTree(
          stmt.kind,
          nnkIdentDefs.newTree(lhs, typ, rhs))
        newBody.add(newAssgn)
      else:
        newBody.add(stmt)

    else:
      newBody.add(stmt)


  let lets: NimNode = nnkStmtList.newTree(
    stmts.mapIt(
      nnkLetSection.newTree nnkIdentDefs.newTree(
        newIdentNode("tmp" & $it.i),
        newIdentNode("NimNode"),
        it.node)))

  let newBodyStmts = newBody.newStmtList()

  let resBlock = quote do:
    block:
      `lets`
      quote do:
        `newBodyStmts`

  # echo toStrLit resBlock
  result = resBlock
