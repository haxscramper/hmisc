import macros

# TODO implement features described in
# https://github.com/rust-lang/rfcs/blob/master/text/2497-if-let-chains.md
# and https://github.com/rust-lang/rust/issues/53667

# TODO support not only optional types but also pointers and references.

# TODO `iflet (val1 = opt1 and val2 = opt2)`

macro ifLet*(head: untyped, bodies: varargs[untyped]): untyped =
  ##[
Unwrap optional value into variable if it contains something.
head must be an assignment in for `<value> = <opt-expression>`.
Expression is evaluated only once.
  ]##

  # assert head.kind == nnkPar
  # assert head[0].kind == nnkAsgn

  let varSymbol = ident head[0][0].strVal()
  let varValue = head[0][1]

  let ifBody = bodies[0]

  let optId = genSym(ident = "optValue")

  # echo varSymbol.treeRepr()
  var condBranch = newIfStmt(
    ((quote do: `optId`.isSome()),
     quote do:
       when declared(`varSymbol`):
         `varSymbol` = `optId`.get()
       else:
         let `varSymbol` = `optId`.get()
       `ifBody`))

  for body in bodies[1..^1]:
    condBranch.add body

  result = quote do:
    let `optId` = `varValue`
    `condBranch`

  result = newStmtList(result)
  # echo result.toStrLit()
