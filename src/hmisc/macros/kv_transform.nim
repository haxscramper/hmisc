import macros
import ../hexceptions
import ../algo/halgorithm

macro kvCall*(head, nodes: untyped): untyped =
  result = newCall(head)
  for node in nodes:
    node.assertNodeKind({nnkCall})
    node[1].assertNodeKind({nnkStmtList})
    result.add nnkExprEqExpr.newTree(node[0], node[1][0])

macro varargsCall*(head, nodes: untyped): untyped =
  result = newCall(head)
  for node in nodes:
    result.add node

macro stringKvTable*(nodes: untyped): untyped =
  result = nnkTableConstr.newTree()
  for node in nodes:
    node.assertNodeKind({nnkCall, nnkStrLit})
    var val = node[1][0]
    if val.kind == nnkTripleStrLit:
      val.strVal = val.strVal.dedent()

    result.add nnkExprColonExpr.newTree(
      newLit node[0].strVal(), val)


macro passKVargs*(head: untyped, args: varargs[untyped]): untyped =
  result = newCall(head)
  head.assertNodeKind({nnkIdent})
  for arg in args:
    arg.assertNodeKind({nnkIdent})
    result.add nnkExprEqExpr.newTree(arg, arg)
