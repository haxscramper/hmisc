import macros

# TODO implement features described in
# https://github.com/rust-lang/rfcs/blob/master/text/2497-if-let-chains.md
# and https://github.com/rust-lang/rust/issues/53667

macro ifLet*(head: untyped, bodies: varargs[untyped]): untyped =
  ##[
Unwrap optional value into variable if it contains something.

head must be an assignment in for `<value> = <opt-expression>`.
Expression is evaluated only once.

  ]##

  # assert head.kind == nnkPar
  # assert head[0].kind == nnkAsgn

  let varSymbol = head[0][0]
  let varValue = head[0][1]

  let ifBody = bodies[0]

  let optId = genSym(ident = "optValue")

  var condBranch = newIfStmt(
    ((quote do: `optId`.isSome()),
     quote do:
       when declared(`varSymbol`): # Prevent shadowing of variables
         # TODO check if `varSymbol` is mutable or not
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
