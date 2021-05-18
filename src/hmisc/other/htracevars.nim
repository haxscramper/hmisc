# !!! WIP

import std/[macros, strformat, sequtils]

func makeVarDump(node: NimNode): seq[string] =
  case node.kind:
    of nnkCall:
      for arg in node[1..^1]:
        result.add arg.makeVarDump()
    of nnkIdent, nnkSym:
      result.add node.strVal()
    of nnkCharLit..nnkUInt64Lit,
       nnkFloatLit..nnkFloat64Lit,
       nnkStrLit..nnkTripleStrLit, nnkCommentStmt:
      discard
    else:
      raiseAssert(&"#[ IMPLEMENT for kind {node.kind} {instantiationInfo()} ]#")

type
  LoggedCallError = ref object of CatchableError
    originalError*: ref Exception
    variables*: seq[tuple[name: string, value: string]]


var cnt {.compiletime.}: int = 0
macro optCall{call(args)}(call: typed, args: varargs[typed]): untyped =
  inc cnt
  result = nnkCall.newTree(call)
  for arg in args:
    result.add arg

  var vardump: NimNode = nnkCurly.newTree()

  for varn in args.mapIt(it.makeVarDump()).concat().deduplicate():
    vardump.add nnkExprColonExpr.newTree(
      newLit varn,
      nnkPrefix.newTree(ident "$", ident varn)
    )

  if vardump.len == 0:
    result = quote do:
      {.noRewrite.}:
        `result`

  else:
    result = quote do:
      {.noRewrite.}:
        try:
            `result`
        except:
          raise LoggedCallError(
            originalError: getCurrentException(),
            variables: `vardump`
          )
