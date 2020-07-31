import macros

macro ifLet*(head: untyped, bodies: varargs[untyped]): untyped =
  # ##[ Unwrap optional value into variable if it contains something. ]##

  # assert head.kind == nnkPar
  # assert head[0].kind == nnkAsgn

  let varSymbol = head[0][0]
  let varValue = head[0][1]

  let ifBody = bodies[0]

  var condBranch = newIfStmt(
    ((quote do: optValue.isSome()),
     quote do:
       let `varSymbol` = optValue.get()
       `ifBody`))

  for body in bodies[1..^1]:
    condBranch.add body

  result = quote do:
    block:
      let optValue {.inject.} = `varValue`
      `condBranch`

  result = newStmtList(result)
