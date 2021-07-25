import std/[macros]

proc withFieldAssignsTo*(
    target, body: NimNode,
    withTmp: bool = false,
    asExpr: bool = false
  ): NimNode =

  expectKind(body, {nnkStmtList, nnkArgList})

  result = newStmtList()
  let tmp = if withTmp: ident("tmp") else: target
  proc addAsgn(re, part: NimNode) =
    expectKind(part, {nnkAsgn, nnkExprEqExpr})
    re.add nnkAsgn.newTree(
      nnkDotExpr.newTree(tmp, part[0]), part[1])

  proc convertAsgn(re: NimNode, entry: NimNode) =
    if entry.kind in {nnkStmtList, nnkArgList}:
      for part in entry:
        convertAsgn(re, part)

    else:
      addAsgn(re, entry)

  convertAsgn(result, body)

  result = quote do:
    block:
      var `tmp` = `target`
      `result`
      `tmp`


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
func newEcE*(lhs, rhs: NimNode): NimNode = nnkExprColonExpr.newTree(lhs, rhs)
func newEqE*(lhs: string, rhs: NimNode): NimNode = newEqE(ident(lhs), rhs)
func newEcE*(lhs: string, rhs: NimNode): NimNode = newEcE(ident(lhs), rhs)

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
