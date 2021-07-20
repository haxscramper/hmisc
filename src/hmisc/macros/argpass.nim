import std/macros

macro argpass*(other: varargs[untyped]): untyped =
  let head = other[0]
  if head.kind == nnkCall:
    result = head

  else:
    result = newCall(head)

  for arg in other[1..^1]:
    if arg.kind == nnkIdent:
      result.add nnkExprEqExpr.newTree(arg, arg)

    else:
      result.add arg

func newEqE*(lhs, rhs: NimNode): NimNode = nnkExprEqExpr.newTree(lhs, rhs)
func newEqE*(lhs: string, rhs: NimNode): NimNode = newEqE(ident(lhs), rhs)

func addArg*(call: NimNode, name: string, value: NimNode) =
  call.add nnkExprEqExpr.newTree(ident(name), value)

func isArgpass*(call: NimNode): bool =
  call.kind in {nnkCall} and call[0].eqIdent("argpass")

func addArgpass*(call, pass: NimNode) =
  if isArgpass(pass) and pass.len > 1:
    for arg in pass[1 ..^ 1]:
      call.add arg

func mergeArgs*(call: NimNode, args: seq[NimNode]) =
  for arg in args:
    if isArgpass(arg):
      addArgpass(call, arg)

    else:
      call.add arg
