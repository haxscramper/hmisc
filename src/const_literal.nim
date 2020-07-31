import macros
import strformat
import hmisc/helpers
import sequtils
import strutils
import options
import deques
import typeinfo
import typetraits
import tables

## Partially successful experiment on passing **ANY** value from
## compile time to runtime as long as type can *exist* at runtime.
## Also might work if you need to pass /value/ of nim node to toplevel
## macro. Currently it works only on types that expos all necessary
## fields.

type
  OptDescBase* = ref object of RootObj
    name*: string
    help*: string
    docstr*: string
    pfix*: seq[string]

  OptDesc* = ref object of OptDescBase
    maxValues*: int
    # parseto*: Option[NimNode] # Disabling this field will allow to use
    #                           # this type at both compile-time and
    #                           # runtime. Otheriwse it is only
    #                           # available at compile-time.

  ArgDesc* = object
    name*: string
    variadic*: bool
    help*: string
    docstr*: string

  Test* = object
    name*: string
    help*: string
    dosctr*: string
    args*: seq[ArgDesc]
    subs*: seq[Test]
    opts*: seq[OptDesc]


proc initCodegen[T](arr: seq[T]): NimNode {.discardable.}

proc initCodegen(val: char | int | bool | string): NimNode {.discardable.} =
  when val is char or val is int or val is string:
    when val is string: newStrLitNode(val)
    elif val is int: newIntLitNode(val)
    else: newIdentNode($val)
  else:
    initCodegenObject(val)

proc initCodegenObject[T: object | tuple](obj: T): NimNode {.discardable.} =
  var fieldInit: seq[NimNode]
  if obj is object:
    fieldInit.add newIdentNode($typeof(T))

  for name, value in obj.fieldPairs:
    when isNamedTuple(typeof obj):
      fieldInit.add initCodegen(value)
    else:
      fieldInit.add nnkExprColonExpr.newTree(
        ident(name), initCodegen(value)
      )

  if obj is object:
    nnkObjConstr.newTree(fieldInit)
  else:
    nnkPar.newTree(fieldInit)

proc initCodegen(obj: object | tuple): NimNode {.discardable.} =
  initCodegenObject(obj)

proc initCodegen(nd: NimNode): NimNode {.discardable.} =
  discard

proc initCodegen[T](arr: seq[T]): NimNode {.discardable.} =
  nnkPrefix.newTree(
    newIdentNode("@"),
    nnkBracket.newTree(arr.mapIt(it.initCodegen())))

proc initCodegen[T](opt: Option[T]): NimNode {.discardable.} =
  if opt.isSome():
    initCodegen(opt.get())
  else:
    quote do:
      none(`T`)

proc initCodegen[K, V](tbl: Table[K, V]): NimNode {.discardable.} =
  var fieldInit: seq[NimNode]
  for key, val in tbl:
    fieldInit.add nnkPar.newTree(
      initCodegen[K](key),
      initCodegen[V](val)
    )

  nnkCall.newTree(
    newIdentNode("toTable"),
    nnkBracket.newTree(fieldInit)
  )

macro convtest(): untyped =
  var t = [(1, Test(name: "hello"))].toTable()
  result = initCodegen(t)
  # result = initCodegen(Test(name: "fff"))
  # result = quote do:
  #   let qqq {.inject.} = `result`

  # result = nnkStmtList.newTree(result)
